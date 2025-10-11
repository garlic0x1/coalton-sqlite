(defpackage #:coalton-sqlite/query
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:lisparray #:coalton-library/lisparray)
   (#:sqlite #:coalton-sqlite/sqlite)
   (#:value #:coalton-sqlite/value))
  (:export
   #:execute
   #:execute-string
   #:query
   #:query-one
   #:do-rows
   #:with-transaction))

(in-package #:coalton-sqlite/query)

(coalton-toplevel 
  (declare bind-values (sqlite:Statement -> (List value:DynamicValue) -> Unit))
  (define (bind-values stmt values)
    (rec f ((values values) (i 1))
      (match (head values)
        ((None) Unit)
        ((Some head)
         (value:bind-dynamic-value stmt i head)
         (f (unwrap (tail values)) (1+ i))))))

  (declare column-values (sqlite:Statement -> (List value:DynamicValue)))
  (define (column-values stmt)
    (rec f ((i (1- (sqlite:column-count stmt))) (acc Nil))
      (if (zero? i)
          (Cons (value:column-dynamic-value stmt i) acc)
          (f (1- i) (Cons (value:column-dynamic-value stmt i) acc)))))

  (declare execute (sqlite:Database -> String -> (List value:DynamicValue) -> Unit))
  (define (execute db sql params)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (sqlite:step-statement stmt)
        Unit))) 

  (declare execute-string (sqlite:Database -> String -> Unit))
  (define (execute-string db sql)
    (sqlite:with-statement db sql
      (fn (stmt)
        (sqlite:step-statement stmt)
        Unit)))

  (declare query (sqlite:Database -> String -> (List value:DynamicValue) -> (List (List value:DynamicValue))))
  (define (query db sql params)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (rec f ((continue? (sqlite:step-statement stmt)) (acc Nil))
          (if (not continue?)
              acc
              (let ((row (column-values stmt)))
                (f (sqlite:step-statement stmt) (Cons row acc))))))))

  (declare query-one (sqlite:Database -> String -> (List value:DynamicValue) -> (Optional (List value:DynamicValue))))
  (define (query-one db sql params)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (if (not (sqlite:step-statement stmt))
            None
            (Some (column-values stmt))))))

  (declare do-rows (sqlite:Database -> String -> (List value:DynamicValue) -> ((List value:DynamicValue) -> Unit) -> Unit))
  (define (do-rows db sql params func)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (rec f ((continue? (sqlite:step-statement stmt)))
          (func (column-values stmt))
          (when continue?
            (f (sqlite:step-statement stmt)))))))

  (declare with-transaction (sqlite:Database -> (Unit -> :t) -> :t))
  (define (with-transaction db func)
    (execute-string db "BEGIN TRANSACTION")
    (lisp :t (db func)
      (cl:let ((ok? cl:nil)) 
        (cl:unwind-protect
             (cl:prog1 (call-coalton-function func db)
               (cl:setf ok? cl:t))
          (cl:if ok?
                 (call-coalton-function execute-string db "COMMIT TRANSACTION")
                 (call-coalton-function execute-string db "ROLLBACK TRANSACTION")))))))

