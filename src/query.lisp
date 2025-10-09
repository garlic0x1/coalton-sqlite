(defpackage #:coalton-sqlite/query
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:lisparray #:coalton-library/lisparray)
   (#:sqlite #:coalton-sqlite/sqlite))
  (:export
   #:execute
   #:query))

(in-package #:coalton-sqlite/query)

(coalton-toplevel 
  (declare bind-values (sqlite:Statement -> (List sqlite:SqliteValue) -> Unit))
  (define (bind-values stmt values)
    (rec f ((values values) (i 1))
      (match (head values)
        ((None) Unit)
        ((Some head)
         (sqlite:bind-value stmt i head)
         (f (unwrap (tail values)) (1+ i))))))

  (declare column-values (sqlite:Statement -> (List sqlite:SqliteValue)))
  (define (column-values stmt)
    (rec f ((i (1- (sqlite:column-count stmt))) (acc Nil))
      (if (zero? i)
          (Cons (sqlite:column-value stmt i) acc)
          (f (1- i) (Cons (sqlite:column-value stmt i) acc)))))

  (declare execute (sqlite:Database -> String -> (List sqlite:SqliteValue) -> Unit))
  (define (execute db sql params)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (sqlite:step-statement stmt)
        Unit))) 

  (declare query (sqlite:Database -> String -> (List sqlite:SqliteValue) -> (List (List sqlite:SqliteValue))))
  (define (query db sql params)
    (sqlite:with-statement db sql
      (fn (stmt)
        (bind-values stmt params)
        (rec f ((continue? (sqlite:step-statement stmt)) (acc Nil))
          (if (not continue?)
              acc
              (let ((row (column-values stmt)))
                (f (sqlite:step-statement stmt) (Cons row acc)))))))))

