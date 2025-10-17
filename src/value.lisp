(defpackage #:coalton-sqlite/value
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:cell #:coalton-library/cell)
   (#:ffi #:coalton-sqlite/ffi)
   (#:const #:coalton-sqlite/constants)
   (#:sqlite #:coalton-sqlite/sqlite))
  (:export
   #:DynamicValue
   #:Null
   #:Int
   #:Float
   #:Text
   #:Blob
   #:column-dynamic-value
   #:bind-dynamic-value
   #:SqliteValue
   #:column-value
   #:bind-value
   #:bind-values
   ))

(in-package #:coalton-sqlite/value)

(coalton-toplevel
  ;; TODO: switch Blob to LispArray,
  ;; requires (Eq LispArray) to be implemented.
  (derive Eq Hash)
  (define-type DynamicValue
    Null
    (Int   I64)
    (Float F64)
    (Text  String)
    (Blob  (vec:Vector U8)))

  (define-instance (Default DynamicValue)
    (define (default) Null))

  (inline)
  (declare column-dynamic-value (sqlite:Statement -> UFix -> DynamicValue))
  (define (column-dynamic-value stmt index)
    "Read a dynamic value from column."
    (match (lisp U8 (stmt index) (ffi:sqlite3-column-type stmt index))
      (1 (Int   (sqlite:column-i64 stmt index)))
      (2 (Float (sqlite:column-f64 stmt index)))
      (3 (Text  (sqlite:column-text stmt index)))
      (4 (Blob  (sqlite:column-blob stmt index)))
      (5 Null)
      (_ (sqlite::throw-sqlite 1) Null)))


  (inline)
  (declare bind-dynamic-value (sqlite:Statement -> UFix -> DynamicValue -> Unit))
  (define (bind-dynamic-value stmt index value)
    "Bind a dynamic value to statement."
    (match value
      ((Int x)   (sqlite:bind-i64 stmt index x))
      ((Float x) (sqlite:bind-f64 stmt index x))
      ((Text x)  (sqlite:bind-text stmt index x))
      ((Blob x)  (sqlite:bind-blob stmt index x))
      ((Null)    (sqlite:bind-null stmt index)))))

(coalton-toplevel
  (define-class (SqliteValue :t)
    (column-value
     "Read a value from column."
     (sqlite:Statement -> UFix -> :t))
    (bind-value
     "Bind a value to statement."
     (sqlite:Statement -> UFix -> :t -> Unit))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro define-sqlite-value (type binder reader sqlite-type)
    `(progn
       (define-instance (SqliteValue ,type)
         (inline)
         (define bind-value ,binder)

         (inline)
         (define column-value ,reader))
       (define-instance (SqliteValue (Optional ,type))
         (inline)
         (define (bind-value stmt index value)
           (match value
             ((Some value) (,binder stmt index value))
             ((None) (sqlite:bind-null stmt index))))

         (inline)
         (define (column-value stmt index)
           (match (sqlite:column-type stmt index)
             ((,sqlite-type) (Some (,reader stmt index)))
             ((const:SqliteNull) None)
             (_ (sqlite::throw-sqlite 20) None)))))))

(coalton-toplevel
  (define-sqlite-value I64 sqlite:bind-i64 sqlite:column-i64 const:SqliteInt)
  (define-sqlite-value F64 sqlite:bind-f64 sqlite:column-f64 const:SqliteFloat)
  (define-sqlite-value String sqlite:bind-text sqlite:column-text const:SqliteText)
  (define-sqlite-value (vec:Vector U8) sqlite:bind-blob sqlite:column-blob const:SqliteBlob)

  (define-instance (SqliteValue DynamicValue)
    (inline)
    (define bind-value bind-dynamic-value)

    (inline)
    (define column-value column-dynamic-value))

  (define-instance (SqliteValue :t => SqliteValue (cell:Cell :t))
    (inline)
    (define (bind-value stmt index value)
      (bind-value stmt index (cell:read value)))

    (inline)
    (define (column-value stmt index)
      (cell:new (column-value stmt index)))))

(cl:defmacro bind-values (stmt cl:&rest values)
  (cl:let ((stmt-var (cl:gensym "STMT-")))
    `(let ((,stmt-var ,stmt))
       ,@(cl:loop
            :for i :upfrom 1
            :for value :in values
            :collect `(bind-value ,stmt-var ,i ,value)))))
