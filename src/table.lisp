;; This is probably terrible

(defpackage #:coalton-sqlite/table
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:sym #:coalton-library/symbol)
   (#:types #:coalton-library/types)
   (#:sqlite #:coalton-sqlite/sqlite))
  (:export
   #:define-table
   #:bind-table
   #:read-table
   #:insert
   #:select-all
   #:create-table
   ))

(in-package #:coalton-sqlite/table)

(coalton-toplevel
  (define-class (Column :t)
    (bind-column (sqlite:Statement -> UFix -> :t -> Unit))
    (read-column (sqlite:Statement -> UFix -> :t)))

  (define-class (Table :t)
    (table-name (:t -> sym:Symbol))
    (table-size (:t -> UFix))
    (bind-table (sqlite:Statement -> UFix -> :t -> Unit))
    (read-table (sqlite:Statement -> UFix -> :t))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defvar *table-registry* (cl:make-hash-table))

  (cl:defmacro define-column (type binder reader sqlite-type)
    `(progn
       (define-instance (Column ,type)
         (define bind-column ,binder)
         (define read-column ,reader))
       (define-instance (Column (Optional ,type))
         (define (bind-column stmt index value)
           (match value
             ((Some value) (,binder stmt index value))
             ((None) (sqlite:bind-null stmt index))))
         (define (read-column stmt index)
           (match (sqlite:column-type stmt index)
             ((,sqlite-type) (Some (,reader stmt index)))
             ((sqlite:SqliteNull) None)
             (_ (sqlite::throw-sqlite 20) None))))))

  (cl:defmacro define-table (name cl:&body slots)
    (cl:let ((slot-names (cl:mapcar #'cl:first slots)))
      (cl:setf (cl:gethash name *table-registry*) slot-names)
      `(progn
         (derive Eq)
         (define-struct ,name ,@slots)
         (define-instance (Table ,name)
           (define (table-name _struct)
             (lisp sym:Symbol () ',name))
           (define (table-size _struct)
             (lisp UFix () ,(cl:length slots)))
           (define (bind-table stmt index struct)
             (let (,name ,@slot-names) = struct)
             ,@(cl:loop
                  :for i :upfrom 0
                  :for slot-name :in slot-names
                  :collect `(bind-column stmt (+ index ,i) ,slot-name)))
           (define (read-table stmt index)
             (,name
              ,@(cl:loop
                   :for i :upfrom 0
                   :for slot :in slots
                   :collect `(read-column stmt (+ index ,i))))))))))

(coalton-toplevel
  (define-column I64 sqlite:bind-i64 sqlite:column-i64 sqlite:SqliteInt)
  (define-column F64 sqlite:bind-f64 sqlite:column-f64 sqlite:SqliteFloat)
  (define-column String sqlite:bind-text sqlite:column-text sqlite:SqliteText)
  (define-column (vec:Vector U8) sqlite:bind-blob sqlite:column-blob sqlite:SqliteBlob))

(coalton-toplevel
  (define-instance (Column sqlite:SqliteValue)
    (define bind-column sqlite:bind-value)
    (define read-column sqlite:column-value)))

(coalton-toplevel
  (declare insert (Table :t => sqlite:Database -> :t -> Unit))
  (define (insert db table)
    (let name = (table-name table))
    (let size = (table-size table))
    (let sql =
      (lisp String (name size)
        (cl:format cl:nil "INSERT INTO ~A VALUES (~{~A~^, ~})"
                   name
                   (cl:loop :repeat size :collect "?"))))
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-table stmt 1 table)
        (sqlite:step-statement stmt)
        Unit)))

  (declare select-all ((types:RuntimeRepr :t) (Table :t) => sqlite:Database -> (List :t)))
  (define (select-all db)
    (let prox = types:Proxy)
    (let type = (types:runtime-repr (types:proxy-inner prox)))
    (let sql =
      (lisp String (type)
        (cl:format cl:nil "SELECT ~{~A~^, ~} FROM ~A"
                   (cl:gethash type *table-registry*)
                   type)))
    (types:as-proxy-of
     (sqlite:with-statement db sql
       (fn (stmt)
         (rec f ((continue? (sqlite:step-statement stmt)) (acc Nil))
           (if (not continue?)
               acc
               (let ((row (read-table stmt 0)))
                 (f (sqlite:step-statement stmt) (Cons row acc))))) ))
     prox))

  (declare create-table* (sqlite:Database -> sym:Symbol -> Unit))
  (define (create-table* db type)
    (let sql =
      (lisp String (type)
        (cl:format cl:nil "CREATE TABLE IF NOT EXISTS ~A (~{~A~^, ~})"
                   type
                   (cl:gethash type *table-registry*))))
    (coalton-sqlite/query:execute db sql mempty)))

(cl:defmacro create-table (db type)
  `(create-table* ,db (lisp sym:Symbol () ',type)))
