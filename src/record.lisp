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
  (cl:defmacro define-record (name cl:&body slots)
    (cl:let ((slot-names (cl:mapcar #'cl:first slots))
             (slot-types (cl:mapcar #'cl:second slots))
             (slot-meta  (cl:mapcar #'cl:third slots))
             (schema-name (cl:intern (cl:string-upcase (cl:format cl:nil "*~A-schema*" name)))))

      `(progn
         ;; Codegen Record type
         (derive Eq Default)
         (define-struct ,name
           ,@(cl:mapcar (cl:lambda (name type) `(,name ,type))
                        slot-names
                        slot-types))

         ;; Codegen Record schema
         (define ,schema-name
           (RecordSchema
            (lisp sym:Symbol () ',name)
            (make-list
             ,@(cl:mapcar (cl:lambda (name meta)
                            `(Tuple (lisp sym:Symbol () ',name) ,(cl:or meta "")))
                          slot-names
                          slot-meta))))

         ;; Codegen Record methods
         (define-instance (Record ,name)
           (inline)
           (define (record-schema _struct) ,schema-name)

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
                   :collect `(value:column-value stmt ,i)))))))))

(coalton-toplevel
  (define-struct RecordSchema
    (name sym:Symbol)
    (columns (List (Tuple sym:Symbol String))))

  (define-class (Record :t)
    (record-schema (:t -> RecordSchema))
    (bind-record (sqlite:Statement -> :t -> Unit))
    (read-record (sqlite:Statement -> :t))))

(coalton-toplevel
  (declare insert (Record :t => sqlite:Database -> :t -> Unit))
  (define (insert db record)
    (let (RecordSchema name columns) = (record-schema record))
    (let size = (length columns))
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

  (declare select-all ((Record :t) => sqlite:Database -> RecordSchema -> (List :t)))
  (define (select-all db schema)
    (let (RecordSchema name columns) = schema)
    (let column-names = (map fst columns))
    (let sql =
      (lisp String (name column-names)
        (cl:format cl:nil "SELECT ~{~A~^, ~} FROM ~A"
                   column-names
                   name)))
    (sqlite:with-statement db sql
      (fn (stmt)
        (rec f ((continue? (sqlite:step-statement stmt)) (acc Nil))
          (if (not continue?)
              acc
              (let ((row (read-record stmt)))
                (f (sqlite:step-statement stmt) (Cons row acc))))))))

  (declare create-table (sqlite:Database -> RecordSchema -> Unit))
  (define (create-table db schema)
    (let (RecordSchema name columns) = schema)
    (let columns = (map (fn ((Tuple name meta))
                          (lisp String (name meta)
                            (cl:format cl:nil "~A ~A" name meta)))
                        columns))
    (let sql =
      (lisp String (name columns)
        (cl:format cl:nil "CREATE TABLE IF NOT EXISTS ~A (~{~A~^, ~})"
                   name
                   columns)))
    (coalton-sqlite/query:execute db sql mempty)))
