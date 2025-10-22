(in-package #:coalton-sqlite/test)

(define-test test-query-basic ()
  (is (==
       (make-list
        (make-list (Int 2) (Int -2))
        (make-list (Int 0) (Int 0)))
       (with-database ":memory:" None
         (fn (db)
           (execute-string db "create table pnt (x, y);")
           (execute db "insert into pnt values (?, ?)" (make-list (Int 0) (Int 0)))
           (execute db "insert into pnt values (?, ?)" (make-list (Int 2) (Int -2)))
           (query db "select * from pnt" mempty))))))

(define-test test-query-execute-statements ()
  (is (==
       (Text "garlic")
       (with-database ":memory:" None
         (fn (db)
           (with-transaction db
             (fn ()
               (execute-string
                db "create table users
                     (id integer primary key,
                      user_name text not null,
                      age integer null)")
               (execute db "insert into users (user_name, age) values (?, ?);"
                        (make-list (Text "garlic") (Int 26)))
               (with-statement db "select * from users where age = ?;"
                 (fn (stmt)
                   (bind-value stmt 1 (Int 26))
                   (step-statement stmt)
                   (column-value stmt 1))))))))))
