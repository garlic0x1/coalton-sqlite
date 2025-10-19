(defpackage #:coalton-sqlite/value
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:array #:coalton-library/lisparray)
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
  (derive Eq Hash)
  (define-type DynamicValue
    "A union type that represents all possible SQLite values."
    Null
    (Int   I64)
    (Float F64)
    (Text  String)
    (Blob  (array:LispArray U8)))

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
    "Objects that can be bound and read from statements."
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
             (_ (sqlite::throw-sqlite 20) None))))))

  (cl:defmacro define-sqlite-blob (type csize ctype cltype)
    `(progn
       (define-instance (SqliteValue ,type)
         (inline)
         (define (bind-value stmt index value)
           (let bytes = (* ,csize (array:length value)))
           (lisp Unit (stmt index value bytes)
             (cffi:with-pointer-to-vector-data (ptr value)
               (sqlite::maybe-throw-sqlite
                (ffi:sqlite3-bind-blob stmt index ptr bytes (ffi:destructor-transient))))))

         (inline)
         (define (column-value stmt index)
           (lisp ,type (stmt index)
             (cffi:foreign-array-to-lisp
              (ffi:sqlite3-column-blob stmt index)
              (cl:list :array ,ctype (cl:/ (ffi:sqlite3-column-bytes stmt index) ,csize))
              :element-type ',cltype))))

      (define-instance (SqliteValue (Optional ,type))
         (inline)
         (define (bind-value stmt index value)
           (match value
             ((Some value) (bind-value stmt index value))
             ((None) (sqlite:bind-null stmt index))))

         (inline)
         (define (column-value stmt index)
           (match (sqlite:column-type stmt index)
             ((const:SqliteBlob) (Some (column-value stmt index)))
             ((const:SqliteNull) None)
             (_ (sqlite::throw-sqlite 20) None)))))))

(coalton-toplevel
  (define-sqlite-value I64 sqlite:bind-i64 sqlite:column-i64 const:SqliteInt)
  (define-sqlite-value F64 sqlite:bind-f64 sqlite:column-f64 const:SqliteFloat)
  (define-sqlite-value String sqlite:bind-text sqlite:column-text const:SqliteText)
  (define-sqlite-value (array:LispArray U8) sqlite:bind-blob sqlite:column-blob const:SqliteBlob)

  (define-sqlite-blob (array:LispArray U16) 16 :uint16 (cl:unsigned-byte 16))
  (define-sqlite-blob (array:LispArray U32) 32 :uint32 (cl:unsigned-byte 32))
  (define-sqlite-blob (array:LispArray U64) 64 :uint64 (cl:unsigned-byte 64))
  (define-sqlite-blob (array:LispArray I8)  8  :int8  (cl:signed-byte 8))
  (define-sqlite-blob (array:LispArray I16) 16 :int16 (cl:signed-byte 16))
  (define-sqlite-blob (array:LispArray I32) 32 :int32 (cl:signed-byte 32))
  (define-sqlite-blob (array:LispArray I64) 64 :int64 (cl:signed-byte 64))
  (define-sqlite-blob (array:LispArray F32) 32 :float cl:float)
  (define-sqlite-blob (array:LispArray F64) 64 :double cl:double-float)

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
