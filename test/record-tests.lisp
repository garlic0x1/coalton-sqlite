(in-package #:coalton-sqlite/test)

(coalton-toplevel 
  (define-record Point
    (x I64)
    (y DynamicValue)))

(define-test test-basic-record-insert ()
  (is (==
       (make-list
        (Point 1 (Text "foo"))
        (Point 1 (Int 2))) 
       (the (List Point) 
            (with-database "" None
              (fn (db)
                (create-table db *point-schema*)
                (insert db (Point 1 (Int 2)))
                (insert db (Point 1 (Text "foo")))
                (select-all db *point-schema*)))))))
