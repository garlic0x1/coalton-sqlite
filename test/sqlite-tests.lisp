(in-package #:coalton-sqlite/test)

(define-test test-connect ()
  (with-database ":memory:" None
    (fn (_) Unit)))

(define-test test-basic-statements ()
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
