(in-package #:coalton-sqlite/test)

(coalton-toplevel  

  (define-table MyRecord
    (x (coalton-library/cell:Cell String))
    (y String)
    (z I64))

  (monomorphize)
  (declare benchmark-table-insert (Ufix -> Unit))
  (define (benchmark-table-insert n) 
    (with-database "/tmp/coalton-sqlite-benchmark.sqlite3"
      (fn (db)
        (execute db "PRAGMA journal_mode = MEMORY;" mempty)
        (execute db "PRAGMA temp_store = MEMORY;" mempty)
        (execute db "PRAGMA synchronous = NORMAL;" mempty)
        (execute db "PRAGMA locking_mode = EXCLUSIVE;" mempty)
        (create-table db MyRecord)
        (execute db "BEGIN TRANSACTION" mempty)
        (coalton-library/experimental:dotimes (i n)
          (insert db (MyRecord (coalton-library/cell:new "hello world this is the X slot")
                               "idk I just want these strings to have a realistic sizze and not be one byte or anything silly like that, we would never be storing single-byte strings IRL"
                               (into i))))
        (execute db "COMMIT TRANSACTION" mempty)
        Unit)))

  (define (benchmark-simple-insert n)
    (with-database "/tmp/coalton-sqlite-benchmark.sqlite3"
      (fn (db)
        (create-table db MyRecord)
        (coalton-library/experimental:dotimes (i n)
          (execute db "insert into myrecord values (?, ?, ?)"
                   (make-list (Text "x") (Text "x") (Int (into i))))))))

  (define (benchmark-stmt-insert n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3"
      (fn (db)
        (execute db "pragma journal_mode = MEMORY;" mempty)
        (create-table db MyRecord)
        (coalton-library/experimental:dotimes (i n)
          (traceobject "i" i)
          (with-statement db "insert into myrecord values (?, ?, ?)"
            (fn (stmt)
              (traceobject "stmt" stmt)
              (bind-text stmt 1 "s")
              (bind-text stmt 2 "s")
              (bind-i64 stmt 3 (into i))
              (traceobject "stmt" stmt)
              
              (traceobject "step" (step-statement stmt))
              Unit
              ))
          )))))
