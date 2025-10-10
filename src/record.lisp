;; This is probably terrible

(defpackage #:coalton-sqlite/record
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:sym #:coalton-library/symbol)
   (#:cell #:coalton-library/cell)
   (#:types #:coalton-library/types)
   (#:sqlite #:coalton-sqlite/sqlite)
   (#:value #:coalton-sqlite/value))
  (:export
   #:Record
   #:define-record
   #:bind-record
   #:read-record
   #:insert
   #:select-all
   #:create-table
   ))

(in-package #:coalton-sqlite/record)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defvar *record-registry* (cl:make-hash-table))

  (cl:defmacro define-record (name cl:&body slots)
    (cl:let ((slot-names (cl:mapcar #'cl:first slots)))
      `(cl:progn
         (cl:setf (cl:gethash ',name *record-registry*) ',slot-names)
         (coalton-toplevel 
           (derive Eq Default)
           (define-struct ,name ,@slots)

           (define-instance (Record ,name)
             (inline)
             (define (record-name _struct) (lisp sym:Symbol () ',name))

             (inline)
             (define (record-size _struct) (lisp UFix () ,(cl:length slots)))

             (define (bind-record stmt struct)
               (let (,name ,@slot-names) = struct)
               ,@(cl:loop
                    :for i :upfrom 1
                    :for slot-name :in slot-names
                    :collect `(value:bind-value stmt ,i ,slot-name)))

             (define (read-record stmt)
               (,name
                ,@(cl:loop
                     :for i :upfrom 0
                     :for slot-name :in slot-names
                     :collect `(value:column-value stmt ,i))))))))))

(coalton-toplevel
  (define-class (Record :t)
    (record-name (:t -> sym:Symbol))
    (record-size (:t -> UFix))
    (bind-record (sqlite:Statement -> :t -> Unit))
    (read-record (sqlite:Statement -> :t))))

(coalton-toplevel 
  (declare insert (Record :t => sqlite:Database -> :t -> Unit))
  (define (insert db record)
    (let name = (record-name record))
    (let size = (record-size record))
    (let sql =
      (lisp String (name size)
        (cl:format cl:nil "INSERT INTO ~A VALUES (~{~A~^, ~})"
                   name
                   (cl:loop :repeat size :collect "?"))))
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-record stmt record)
        (sqlite:step-statement stmt)
        Unit)))

  (declare select-all ((types:RuntimeRepr :t) (Record :t) => sqlite:Database -> (List :t)))
  (define (select-all db)
    (let prox = types:Proxy)
    (let type = (types:runtime-repr (types:proxy-inner prox)))
    (let sql =
      (lisp String (type)
        (cl:format cl:nil "SELECT ~{~A~^, ~} FROM ~A"
                   (cl:gethash type *record-registry*)
                   type)))
    (types:as-proxy-of
     (sqlite:with-statement db sql
       (fn (stmt)
         (rec f ((continue? (sqlite:step-statement stmt)) (acc Nil))
           (if (not continue?)
               acc
               (let ((row (read-record stmt)))
                 (f (sqlite:step-statement stmt) (Cons row acc))))) ))
     prox))

  (declare create-table* (sqlite:Database -> sym:Symbol -> Unit))
  (define (create-table* db type)
    (let sql =
      (lisp String (type)
        (cl:format cl:nil "CREATE TABLE IF NOT EXISTS ~A (~{~A~^, ~})"
                   type
                   (cl:gethash type *record-registry*))))
    (coalton-sqlite/query:execute db sql mempty)))

(cl:defmacro create-table (db type)
  `(create-table* ,db (lisp sym:Symbol () ',type)))

(define-record Point
  (x I64)
  (y I64))
