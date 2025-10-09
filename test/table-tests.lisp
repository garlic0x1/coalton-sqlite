(in-package #:coalton-sqlite/test)

(define-table Point
  (x I64)
  (y SqliteValue))

(define-test test-basic-table-insert ()
  (is (==
       (make-list
        (Point 1 (Text "foo"))
        (Point 1 (Int 2))) 
       (the (List Point) 
            (with-database ""
              (fn (db)
                (create-table db Point)
                (insert db (Point 1 (Int 2)))
                (insert db (Point 1 (Text "foo")))
                (select-all db)))))))
