(defpackage #:coalton-sqlite/bench
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-testing
   #:coalton-sqlite)
  (:local-nicknames
   (#:cache #:coalton-sqlite/cache)
   (#:ffi #:coalton-sqlite/ffi))
  (:local-nicknames
   (#:result #:coalton-library/result))
  (:export
   #:run-tests))

(in-package #:coalton-sqlite/bench)

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

  (define (benchmark-bind-and-step-stmt n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (let stmt = (prepare-statement db "insert into myrecord values (?, ?, ?)"))
        (coalton-library/experimental:dotimes (i n)
          (reset-statement stmt)
          (bind-i64 stmt 1 (into i))
          (bind-i64 stmt 2 (into i))
          (bind-i64 stmt 3 (into i))
          (step-statement stmt)
          Unit)
        (finalize-statement stmt))))

  (define (benchmark-noalloc-stmts n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (let stmt = (prepare-statement db "insert into myrecord values (?, ?, ?) on conflict do nothing"))
        (coalton-library/experimental:dotimes (_ n)
          (reset-statement stmt)
          Unit)
        (finalize-statement stmt))))

  (define (benchmark-alloc-stmts n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (coalton-library/experimental:dotimes (_ n)
          (with-statement db  "insert into myrecord values (?, ?, ?) on conflict do nothing"
            (fn (_) Unit))
          Unit))))

  (define (benchmark-cache-stmts n)
    (with-database "/tmp/coalton-sqlite-benchmark2.sqlite3" None
      (fn (db)
        ;; (execute-string db "pragma journal_mode = MEMORY;")
        (execute-string db "create table if not exists myrecord (x, y, z)")
        (cache:with-statement-cache db 2
          (fn (cache)
            (coalton-library/experimental:dotimes (_ n)
              (cache:with-cached-statement cache "insert into myrecord values (?, ?, ?) on conflict do nothing"
                (fn (_) Unit))
              Unit)))))))
