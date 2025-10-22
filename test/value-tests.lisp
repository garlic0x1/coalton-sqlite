(in-package #:coalton-sqlite/test)

(define-test test-value-methods ()
  (is (== (Tuple 1 (Float 1d-19))
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table pnt (x, y)"
                step-statement)
              (with-statement db "insert into pnt values (?, ?)"
                (fn (stmt)
                  (bind-value stmt 1 (the I64 1))
                  (bind-value stmt 2 1d-19)
                  (step-statement stmt)))
              (with-statement db "select * from pnt"
                (fn (stmt)
                  (step-statement stmt)
                  (Tuple
                   (column-value stmt 0)
                   (column-value stmt 1)))))))))

(define-test test-value-bind-macro ()
  (is (== (Tuple 1 "test")
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table pnt (x, y)"
                step-statement)
              (with-statement db "insert into pnt values (?, ?)"
                (fn (stmt)
                  (bind-values stmt (the I64 1) "test")
                  (step-statement stmt)))
              (with-statement db "select * from pnt"
                (fn (stmt)
                  (step-statement stmt)
                  (Tuple
                   (column-value stmt 0)
                   (column-value stmt 1)))))))))

(define-test test-float-array ()
  (let arr =
    (the (lisparray:LispArray F64)
         (lisparray:make 10 (lisp F64 () (cl:coerce 3 'cl:double-float)))))
  (is (== arr
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))

(define-test test-i16-array ()
  (let arr =
    (the (lisparray:LispArray I16)
         (lisparray:make 100 (the I16 -32768))))
  (is (== arr
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))

(define-test test-optional-i16-array ()
  (let arr =
    (the (lisparray:LispArray I16)
         (lisparray:make 100 (the I16 -32768))))
  (is (== (Some arr)
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))

(define-test test-optional-optional-array ()
  (let arr =
    (the (lisparray:LispArray I16)
         (lisparray:make 100 (the I16 -32768))))
  (is (== (Some (Some arr))
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  #+#:TODO-BUG
                  (bind-value stmt 1 (Some (Some arr)))
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))

(define-test test-cell-array ()
  (let arr =
    (coalton-library/cell:new
     (the (lisparray:LispArray I16)
          (lisparray:make 100 (the I16 -32768)))))
  (is (== arr
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))

#+#:TODO-BUG
(define-test test-optional-optional-i64 ()
  (let value = (Some (Some (the I64 1000))))
  (is (== value
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (bind-value stmt 1 value)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))
