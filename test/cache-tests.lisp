(in-package #:coalton-sqlite/test)

(define-test test-cache-basic ()
  (let sql1 = "insert into myrecord values (?, ?, ?)")
  (let sql2 = "insert into myrecord values  (?, ?, ?)")
  (let sql3 = "insert into myrecord values   (?, ?, ?)")
  (let sql4 = "insert into myrecord values    (?, ?, ?)")
  (is (== sql3
          (with-database ":memory:" None
            (fn (db)
              (execute-string db "create table if not exists myrecord (x, y, z)")
              (with-statement-cache db 2
                (fn (cache)
                  ;; Saved in cache
                  (with-cached-statement cache sql1
                    (fn (stmt)
                      (bind-i64 stmt 1 (the I64 1))
                      (bind-i64 stmt 2 (the I64 2))
                      (bind-i64 stmt 3 (the I64 3))
                      (step-statement stmt)))
                  ;; Saved in cache
                  (with-cached-statement cache sql2
                    (fn (stmt)
                      (bind-i64 stmt 1 (the I64 1))
                      (bind-i64 stmt 2 (the I64 2))
                      (bind-i64 stmt 3 (the I64 3))
                      (step-statement stmt)))
                  ;; Saved in cache, clearing sql1
                  (with-cached-statement cache sql3
                    (fn (stmt)
                      (bind-i64 stmt 1 (the I64 1))
                      (bind-i64 stmt 2 (the I64 2))
                      (bind-i64 stmt 3 (the I64 3))
                      (step-statement stmt)))
                  ;; Saved in cache, clearing sql2
                  (with-cached-statement cache sql4
                    (fn (stmt)
                      (bind-i64 stmt 1 (the I64 1))
                      (bind-i64 stmt 2 (the I64 2))
                      (bind-i64 stmt 3 (the I64 3))
                      (step-statement stmt)))
                  (coalton-library/queue:pop-unsafe! (.fifo cache)))))))))
