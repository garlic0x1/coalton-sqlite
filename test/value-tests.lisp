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

(define-test test-value-f64-array ()
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

(define-test test-value-f32-array ()
  (let arr =
    (the (lisparray:LispArray F32)
         (lisparray:make 10 (lisp F32 () (cl:coerce -23.456 'cl:single-float)))))
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

(define-test test-value-i16-array ()
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

(define-test test-value-optional-i64 ()
  (let value = (the (Optional I64) (Some -32768)))
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
                  (column-value stmt 0)))))))

  (let value = (the (Optional I64) None))
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

(define-test test-value-cell-array ()
  (let arr =
    (coalton-library/cell:new
     (the (lisparray:LispArray F32)
          (lisparray:make 100 (the F32 -32768)))))
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

(define-test test-value-cell-optional-array ()
  (let arr =
    (coalton-library/cell:new
     (the (Optional (lisparray:LispArray F32))
          None)))
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
                  (column-value stmt 0)))))))

  (is (== (lisparray:make 10 (the F32 123.3))
          (with-database ":memory:" None
            (fn (db)
              (with-statement db "create table blobber (x)" step-statement)
              (with-statement db "insert into blobber values (?)"
                (fn (stmt)
                  (coalton-library/cell:write!
                   arr
                   (Some (lisparray:make 10 (the F32 123.3))))
                  (bind-value stmt 1 arr)
                  (step-statement stmt)))
              (with-statement db "select * from blobber"
                (fn (stmt)
                  (step-statement stmt)
                  (column-value stmt 0))))))))
