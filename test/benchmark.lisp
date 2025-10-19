(in-package #:coalton-sqlite/test)

(coalton-toplevel  

  (define (benchmark-simple-insert n)
    (with-database "/tmp/coalton-sqlite-benchmark.sqlite3" None
      (fn (db)
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (coalton-library/experimental:dotimes (i n)
          (execute db "insert into myrecord values (?, ?, ?)"
                   (make-list (Text "x") (Text "x") (Int (into i))))))))

  (define (benchmark-stmt-insert n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (coalton-library/experimental:dotimes (i n)
          (with-statement db "insert into myrecord values (?, ?, ?)"
            (fn (stmt)
              (bind-i64 stmt 1 (into i))
              (bind-i64 stmt 2 (into i))
              (bind-i64 stmt 3 (into i))
              (step-statement stmt)
              Unit)))))) 

  (define (benchmark-cached-stmt-insert n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (cache:with-statement-cache db 2
          (fn (cache)
            (coalton-library/experimental:dotimes (i n)
              (cache:with-cached-statement cache "insert into myrecord values (?, ?, ?)"
                (fn (stmt)
                  (bind-i64 stmt 1 (into i))
                  (bind-i64 stmt 2 (into i))
                  (bind-i64 stmt 3 (into i))
                  (step-statement stmt)
                  Unit))))))))

  ) 
