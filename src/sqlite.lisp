(defpackage #:coalton-sqlite/sqlite
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:const #:coalton-sqlite/constants)
   (#:vec #:coalton-library/vector)
   (#:ffi #:coalton-sqlite/ffi))
  (:export
   #:Database
   #:Statement
   #:SqliteCondition
   #:throw-sqlite
   #:maybe-throw-sqlite
   #:open-database
   #:close-database
   #:database-filename
   #:with-database
   #:prepare-statement
   #:finalize-statement
   #:with-statement
   #:step-statement
   #:column-count
   #:column-type
   #:column-text
   #:column-i64
   #:column-f64
   #:column-blob
   #:column-name
   #:parameter-count
   #:parameter-index
   #:parameter-name
   #:bind-i64
   #:bind-f64
   #:bind-text
   #:bind-blob
   #:bind-null
   ))

(in-package #:coalton-sqlite/sqlite)

(cl:defmacro ignore-errors (cl:&body body)
  `(catch (progn ,@body Unit)
     (_ Unit)))

(coalton-toplevel
  (repr :native cffi:foreign-pointer)
  (define-type Database)

  (repr :native cffi:foreign-pointer)
  (define-type Statement)

  (define-exception SqliteCondition
    (SqliteCondition const:SqliteError)
    (LispCondition String)))

(coalton-toplevel
  (inline)
  (declare throw-sqlite (U8 -> Unit))
  (define (throw-sqlite x)
    (throw (SqliteCondition (the const:SqliteError (const:load x)))))

  (inline)
  (declare maybe-throw-sqlite (U8 -> Unit))
  (define (maybe-throw-sqlite x)
    (unless (zero? x)
      (throw (SqliteCondition (the const:SqliteError (const:load x)))))))

(coalton-toplevel
  (declare open-database (String -> (Optional (List const:SqliteOpenFlag)) -> Database))
  (define (open-database path flags)
    (match flags
      ((None) 
       (lisp Database (path)
         (cffi:with-foreign-object (db 'ffi:p-sqlite3)
           (maybe-throw-sqlite (ffi:sqlite3-open path db))
           (cffi:mem-ref db :pointer))))
      ((Some flags)
       (let flags = (map const:dump flags))
       (lisp Database (path flags)
         (cffi:with-foreign-object (db 'ffi:p-sqlite3)
           (maybe-throw-sqlite
            (ffi:sqlite3-open-v2
             path
             db
             (cl:reduce #'cl:logior flags)
             (cffi:null-pointer)))
           (cffi:mem-ref db :pointer))))))

  (declare close-database (Database -> Unit))
  (define (close-database db)
    (lisp Unit (db)
      (maybe-throw-sqlite (ffi:sqlite3-close db))))

  (declare key-database (Database -> String -> Unit))
  (define (key-database db key)
    (lisp Unit (db key) 
      (maybe-throw-sqlite 
       (ffi:sqlite3-key-v3 db (cffi:null-pointer) key (cl:length key) (cffi:null-pointer)))))

  (declare database-filename (Database -> String))
  (define (database-filename db)
    (lisp String (db)
      (ffi:sqlite3-db-filename)))

  (declare with-database (String -> (Optional (List const:SqliteOpenFlag)) -> (Database -> :t) -> :t))
  (define (with-database path flags func)
    (let db = (open-database path flags))
    (lisp :t (func db)
      (cl:unwind-protect (call-coalton-function func db)
        (call-coalton-function close-database db))))

  (declare prepare-statement (Database -> String -> Statement))
  (define (prepare-statement db sql)
    (lisp Statement (db sql)
      (cffi:with-foreign-object (cstmt 'ffi:p-sqlite3-stmt)
        (cffi:with-foreign-object (ctail '(:pointer :char))
          (cffi:with-foreign-string (csql sql)
            (maybe-throw-sqlite (ffi:sqlite3-prepare db csql -1 cstmt ctail))
            (cl:if (cl:zerop (cffi:mem-ref (cffi:mem-ref ctail :pointer) :uchar))
                   (cffi:mem-ref cstmt :pointer)
                   (throw-sqlite 21)))))))

  (declare finalize-statement (Statement -> Unit))
  (define (finalize-statement stmt)
    (lisp Unit (stmt)
      (maybe-throw-sqlite (ffi:sqlite3-finalize stmt))))

  (declare with-statement (Database -> String -> (Statement -> :t) -> :t))
  (define (with-statement db sql func)
    (let stmt = (prepare-statement db sql))
    (lisp :t (func stmt)
      (cl:unwind-protect (call-coalton-function func stmt)
        (call-coalton-function finalize-statement stmt))))

  (inline)
  (declare step-statement (Statement -> Boolean))
  (define (step-statement stmt)
    (lisp Boolean (stmt)
      (cl:let ((error-code (ffi:sqlite3-step stmt)))
        (cl:case error-code
          (100 True)
          (101 False)
          (cl:otherwise (throw-sqlite error-code) False)))))

  (inline)
  (declare column-count (Statement -> UFix))
  (define (column-count stmt)
    (lisp UFix (stmt)
      (ffi:sqlite3-column-count stmt)))

  (inline)
  (declare column-type (Statement -> UFix -> const:SqliteType))
  (define (column-type stmt index)
    (const:load
     (lisp U8 (stmt index)
       (ffi:sqlite3-column-type stmt index))))

  (inline)
  (declare column-text (Statement -> UFix -> String))
  (define (column-text stmt index)
    (lisp String (stmt index)
      (ffi:sqlite3-column-text stmt index)))

  (inline)
  (declare column-i64 (Statement -> UFix -> I64))
  (define (column-i64 stmt index)
    (lisp I64 (stmt index)
      (ffi:sqlite3-column-int64 stmt index)))

  (inline)
  (declare column-f64 (Statement -> UFix -> F64))
  (define (column-f64 stmt index)
    (lisp F64 (stmt index)
      (ffi:sqlite3-column-double stmt index)))

  (inline)
  (declare column-blob (Statement -> UFix -> (vec:Vector U8)))
  (define (column-blob stmt index)
    (lisp (vec:Vector U8) (stmt index)
      (cffi:foreign-array-to-lisp
       (ffi:sqlite3-column-blob stmt index)
       (cl:list :array :uint8 (ffi:sqlite3-column-bytes stmt index))
       :adjustable cl:t)))

  (inline)
  (declare column-name (Statement -> UFix -> String))
  (define (column-name stmt index)
    (lisp String (stmt index)
      (ffi:sqlite3-column-name stmt index)))

  (inline)
  (declare parameter-count (Statement -> UFix))
  (define (parameter-count stmt)
    (lisp UFix (stmt)
      (ffi:sqlite3-bind-parameter-count stmt)))

  (inline)
  (declare parameter-index (Statement -> String -> UFix))
  (define (parameter-index stmt name)
    (lisp UFix (stmt name)
      (ffi:sqlite3-bind-parameter-index stmt name)))

  (inline)
  (declare parameter-name (Statement -> UFix -> String))
  (define (parameter-name stmt index)
    (lisp String (stmt index)
      (ffi:sqlite3-bind-parameter-name stmt index)))

  (inline)
  (declare bind-i64 (Statement -> UFix -> I64 -> Unit))
  (define (bind-i64 stmt index x)
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-int64 stmt index x))))

  (inline)
  (declare bind-f64 (Statement -> UFix -> F64 -> Unit))
  (define (bind-f64 stmt index x)
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-double stmt index x))))

  (declare bind-text (Statement -> UFix -> String -> Unit))
  (define (bind-text stmt index x)
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-text stmt index x -1 (ffi:destructor-transient)))))

  (inline)
  (declare bind-blob (Statement -> UFix -> (vec:Vector U8) -> Unit))
  (define (bind-blob stmt index x)
    (let len = (vec:length x))
    (lisp Unit (stmt index x len)
      ;; TODO, switch to this once we can use LispArray
      ;; (cffi:with-pointer-to-vector-data (ptr x)
      ;;   (maybe-throw-sqlite (ffi:sqlite3-bind-blob stmt index ptr len (ffi:destructor-transient))))
      (cffi:with-foreign-object (ptr :unsigned-char len)
        (cl:dotimes (i len)
          (cl:setf (cffi:mem-aref ptr :unsigned-char i) (cl:aref x i)))
        (maybe-throw-sqlite 
         (ffi:sqlite3-bind-blob stmt index ptr len (ffi:destructor-transient))))))

  (inline)
  (declare bind-null (Statement -> UFix -> Unit))
  (define (bind-null stmt index)
    (lisp Unit (stmt index)
      (maybe-throw-sqlite (ffi:sqlite3-bind-null stmt index)))))
