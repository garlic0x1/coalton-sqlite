(defpackage #:coalton-sqlite/sqlite
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:ffi #:coalton-sqlite/ffi))
  (:export
   #:SqliteError
   #:ErrOk
   #:ErrError
   #:ErrInternal
   #:ErrPerm
   #:ErrAbort
   #:ErrBusy
   #:ErrLocked
   #:ErrNoMem
   #:ErrReadOnly
   #:ErrInterrupt
   #:ErrIOErr
   #:ErrCorrupt
   #:ErrNotFound
   #:ErrFull
   #:ErrCantOpen
   #:ErrProtocol
   #:ErrEmpty
   #:ErrSchema
   #:ErrTooBig
   #:ErrConstraint
   #:ErrMismatch
   #:ErrMisuse
   #:ErrNoLFS
   #:ErrAuth
   #:ErrFormat
   #:ErrRange
   #:ErrNotADb
   #:ErrRow
   #:ErrDone
   #:SqliteType
   #:SqliteInt
   #:SqliteFloat
   #:SqliteText
   #:SqliteBlob
   #:SqliteNull
   #:Database
   #:Statement
   #:SqliteCondition
   #:throw-sqlite
   #:maybe-throw-sqlite
   #:open-database
   #:close-database
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

(coalton-toplevel
  (define-class (Enum :a :b)
    (dump (:a -> :b))
    (load (:b -> :a))))

(cl:defmacro define-enum (name type cl:&body ctors)
  `(progn
     (derive Eq)
     (define-type ,name
       ,@(cl:mapcar #'cl:first ctors))
     (define-instance (Enum ,name ,type)
       (define (load value)
         (match value
           ,@(cl:mapcar
              (cl:lambda (ctor)
                `(,(cl:eval (cl:second ctor))
                  ,(cl:first ctor)))
              ctors)
           (x (lisp ,name (x) (cl:error "Invalid error code ~A" x)))))
       (define (dump enum)
         (match enum
           ,@(cl:mapcar
              (cl:lambda (ctor)
                `((,(cl:first ctor))
                  ,(cl:eval (cl:second ctor))))
              ctors))))))

(cl:defmacro ignore-errors (cl:&body body)
  `(catch (progn ,@body Unit)
     (_ Unit)))

(coalton-toplevel
  (define-enum SqliteError U8
    (ErrOk         0)
    (ErrError      1)
    (ErrInternal   2)
    (ErrPerm       3)
    (ErrAbort      4)
    (ErrBusy       5)
    (ErrLocked     6)
    (ErrNoMem      7)
    (ErrReadOnly   8)
    (ErrInterrupt  9)
    (ErrIOErr      10)
    (ErrCorrupt    11)
    (ErrNotFound   12)
    (ErrFull       13)
    (ErrCantOpen   14)
    (ErrProtocol   15)
    (ErrEmpty      16)
    (ErrSchema     17)
    (ErrTooBig     18)
    (ErrConstraint 19)
    (ErrMismatch   20)
    (ErrMisuse     21)
    (ErrNoLFS      22)
    (ErrAuth       23)
    (ErrFormat     24)
    (ErrRange      25)
    (ErrNotADb     26)
    (ErrRow        100)
    (ErrDone       101))

  (define-enum SqliteType U8
    (SqliteInt   1)
    (SqliteFloat 2)
    (SqliteText  3)
    (SqliteBlob  4)
    (SqliteNull  5)))

(coalton-toplevel
  (repr :native cffi:foreign-pointer)
  (define-type Database)

  (repr :native cffi:foreign-pointer)
  (define-type Statement)

  (define-exception SqliteCondition
    (SqliteCondition SqliteError)
    (LispCondition String)))

(coalton-toplevel
  (inline)
  (declare throw-sqlite (U8 -> Unit))
  (define (throw-sqlite x)
    (throw (SqliteCondition (the SqliteError (load x)))))

  (inline)
  (declare maybe-throw-sqlite (U8 -> Unit))
  (define (maybe-throw-sqlite x)
    (unless (zero? x)
      (throw (SqliteCondition (the SqliteError (load x)))))))

(coalton-toplevel
  (declare open-database (String -> Database))
  (define (open-database path)
    (lisp Database (path)
      (cffi:with-foreign-object (db 'ffi:p-sqlite3)
        (maybe-throw-sqlite (ffi:sqlite3-open path db))
        (cffi:mem-ref db :pointer))))

  (declare close-database (Database -> Unit))
  (define (close-database db)
    (lisp Unit (db)
      (maybe-throw-sqlite (ffi:sqlite3-close db))))

  (declare key-database (Database -> String -> Unit))
  (define (key-database db key)
    (lisp Unit (db key) 
      (maybe-throw-sqlite 
       (ffi:sqlite3-key-v3 db (cffi:null-pointer) key (cl:length key) (cffi:null-pointer)))))

  (declare with-database (String -> (Database -> :t) -> :t))
  (define (with-database path func)
    (let db = (open-database path))
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
  (declare column-type (Statement -> UFix -> SqliteType))
  (define (column-type stmt index)
    (load
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
    (let size = (coalton-library/string:length x))
    (lisp Unit (stmt index x size)
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
