(in-package #:coalton-sqlite/test)

(define-test test-sqlite-connect ()
  (with-database ":memory:" None
    (fn (_) Unit)))

(define-test test-sqlite-basic-statements ()
  (is (== (Tuple "user_name" "garlic")
          (with-database ":memory:" None
            (fn (db)
              (with-statement db
                "create table users
                  (id integer primary key,
                   user_name text not null,
                   age integer null)"
                step-statement)
              (with-statement db
                "insert into users (user_name, age) values (?, ?);"
                (fn (stmt)
                  (bind-text stmt 1 "garlic")
                  (bind-i64 stmt 2 26)
                  (step-statement stmt)))
              (with-statement db
                "select * from users where age = :age;"
                (fn (stmt)
                  (let ix = (parameter-index stmt ":age"))
                  (bind-i64 stmt ix 26)
                  (step-statement stmt)
                  (Tuple (column-name stmt 1) (column-text stmt 1)))))))))

(define-test test-sqlite-statement-database ()
  (is (match (with-database ":memory:" None
               (fn (db)
                 (with-statement db
                   "create table users
                  (id integer primary key,
                   user_name text not null,
                   age integer null)"
                   (fn (stmt)
                     (Tuple db (statement-database stmt))))))
        ((Tuple x y)
         (lisp Boolean (x y)
           (cffi:pointer-eq x y))))))

(define-test test-sqlite-database-filename ()
  (is (== "" (with-database ":memory:" None database-filename))))

(define-test test-sqlite-last-insert-rowid ()
  (is (== 0
          (with-database ":memory:" None
            last-insert-rowid)))

  (is (== 12
          (with-database ":memory:" None
            (fn (db)
              (with-statement db
                "create table users
                  (id integer primary key,
                   user_name text)"
                step-statement)
              (with-statement db "insert into users values (?, ?)"
                (fn (stmt)
                  (bind-i64 stmt 1 12)
                  (bind-null stmt 2)
                  (step-statement stmt)))
              (last-insert-rowid db)))))

  (is (== 1
          (with-database ":memory:" None
            (fn (db)
              (with-statement db
                "create table users
                  (id primary key,
                   user_name text not null)"
                step-statement)
              (with-statement db "insert into users values (?, ?)"
                (fn (stmt)
                  (bind-i64 stmt 1 12)
                  (bind-text stmt 2 "hi")
                  (step-statement stmt)))
              (last-insert-rowid db))))))
