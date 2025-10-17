(defpackage #:coalton-sqlite/sqlite
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:const #:coalton-sqlite/constants)
   (#:vec #:coalton-library/vector)
   (#:ffi #:coalton-sqlite/ffi))
  (:export
   #:Database
   #:Statement
   #:SqliteCondition
   #:open-database
   #:close-database
   #:database-filename
   #:with-database
   #:prepare-statement
   #:finalize-statement
   #:reset-statement
   #:clear-statement
   #:statement-database
   #:with-statement
   #:step-statement
   #:column-count
   #:column-type
   #:column-text
   #:column-i64
   #:column-f64
   #:column-blob
   #:column-name
   #:parameter-count
   #:parameter-index
   #:parameter-name
   #:bind-i64
   #:bind-f64
   #:bind-text
   #:bind-blob
   #:bind-null
   ))

(in-package #:coalton-sqlite/sqlite)

(cl:defmacro ignore-errors (cl:&body body)
  `(catch (progn ,@body Unit)
     (_ Unit)))

(coalton-toplevel
  (repr :native cffi:foreign-pointer)
  (define-type Database
    "Database Connection Handle

Each open SQLite database is represented by a pointer to an instance
of the opaque structure named `sqlite3'.")

  (repr :native cffi:foreign-pointer)
  (define-type Statement
    "Prepared Statement Object.

An instance of this object represents a single SQL statement that has
been compiled into binary form and is ready to be evaluated.

Think of each SQL statement as a separate computer program. The
original SQL text is source code. A prepared statement object is the
compiled object code. All SQL must be converted into a prepared
statement before it can be run.")

  (define-exception SqliteCondition
    "Many of the routines in the SQLite C-language Interface return
numeric result codes indicating either success or failure, and in the
event of a failure, providing some idea of the cause of the
failure."
    (SqliteCondition const:SqliteError)
    (LispCondition String)))

(coalton-toplevel
  (inline)
  (declare throw-sqlite (U8 -> Unit))
  (define (throw-sqlite x)
    (throw (SqliteCondition (the const:SqliteError (const:load x)))))

  (inline)
  (declare maybe-throw-sqlite (U8 -> Unit))
  (define (maybe-throw-sqlite x)
    (unless (zero? x)
      (throw (SqliteCondition (the const:SqliteError (const:load x)))))))

(coalton-toplevel
  (declare open-database (String -> (Optional (List const:SqliteOpenFlag)) -> Database))
  (define (open-database path flags)
    "Open a SQLite database file as specified by `path', with optional
flags.

Resources associated with the database connection handle should be
released by passing it to `close-database' when it is no longer
required."
    (match flags
      ((None) 
       (lisp Database (path)
         (cffi:with-foreign-object (db 'ffi:p-sqlite3)
           (maybe-throw-sqlite (ffi:sqlite3-open path db))
           (cffi:mem-ref db :pointer))))
      ((Some flags)
       (let flags = (map const:dump flags))
       (lisp Database (path flags)
         (cffi:with-foreign-object (db 'ffi:p-sqlite3)
           (maybe-throw-sqlite
            (ffi:sqlite3-open-v2
             path
             db
             (cl:reduce #'cl:logior flags)
             (cffi:null-pointer)))
           (cffi:mem-ref db :pointer))))))

  (declare close-database (Database -> Unit))
  (define (close-database db)
    "The `close-database' function is a destructor for the `Database'
object and deallocates all associated resources. Calls to
`close-database' signal `SqliteError' if the object is not
successfully destroyed.

Ideally, applications should finalize all prepared statements prior to
attempting to close the object. If the database connection is
associated with unfinalized prepared statements, BLOB handlers, and/or
unfinished sqlite3_backup objects then `close-database' will leave the
database connection open and signal `ErrBusy'.

If a `Database' object is destroyed while a transaction is open, the
transaction is automatically rolled back."
    (lisp Unit (db)
      (maybe-throw-sqlite (ffi:sqlite3-close db))))

  (declare key-database (Database -> String -> Unit))
  (define (key-database db key)
    "TODO"
    (lisp Unit (db key) 
      (maybe-throw-sqlite 
       (ffi:sqlite3-key-v3 db (cffi:null-pointer) key (cl:length key) (cffi:null-pointer)))))

  (declare database-filename (Database -> String))
  (define (database-filename db)
    "The `database-filename' function returns a string of the filename
associated with database `db'. If the database is closed or is a
temporary or in-memory database, then this function will return an
empty string."
    (lisp String (db)
      (ffi:sqlite3-db-filename db (cffi:null-pointer))))

  (declare with-database (String -> (Optional (List const:SqliteOpenFlag)) -> (Database -> :t) -> :t))
  (define (with-database path flags func)
    "Safely open a `Database' that will be closed as the stack unwinds."
    (let db = (open-database path flags))
    (lisp :t (func db)
      (cl:unwind-protect (call-coalton-function func db)
        (call-coalton-function close-database db))))

  (declare prepare-statement (Database -> String -> Statement))
  (define (prepare-statement db sql)
    "To execute an SQL statement, it must first be compiled into a
byte-code program using one of these routines. Or, in other words,
these routines are constructors for the prepared statement object."
    (lisp Statement (db sql)
      (cffi:with-foreign-object (cstmt 'ffi:p-sqlite3-stmt)
        (cffi:with-foreign-object (ctail '(:pointer :char))
          (cffi:with-foreign-string (csql sql)
            (maybe-throw-sqlite (ffi:sqlite3-prepare db csql -1 cstmt ctail))
            (cl:if (cl:zerop (cffi:mem-ref (cffi:mem-ref ctail :pointer) :uchar))
                   (cffi:mem-ref cstmt :pointer)
                   (throw-sqlite 21)))))))

  (declare finalize-statement (Statement -> Unit))
  (define (finalize-statement stmt)
    "Destroy A Prepared Statement Object.

The `finalize-statement' function is called to delete a prepared
statement. If the most recent evaluation of the statement encountered
no errors or if the statement is never been evaluated, then
`finalize-statement' returns `Unit'. If the most recent evaluation of
statement `stmt' failed, then `finalize-statement' signals the
appropriate error code or extended error code.

The `finalize-statement' routine can be called at any point during the
life cycle of prepared `stmt': before it is ever evaluated, after one
or more calls to `reset-statement', or after any call to
`step-statement' regardless of whether or not the statement has
completed execution.

The application must finalize every prepared statement in order to
avoid resource leaks. It is a grievous error for the application to
try to use a prepared statement after it has been finalized. Any use
of a prepared statement after it has been finalized can result in
undefined and undesirable behavior such as segfaults and heap
corruption."
    (lisp Unit (stmt)
      (maybe-throw-sqlite (ffi:sqlite3-finalize stmt))))

  (declare reset-statement (Statement -> Unit))
  (define (reset-statement stmt)
    "Reset A Prepared Statement Object.

The `reset-statement' function is called to reset a prepared statement
object back to its initial state, ready to be re-executed. Any SQL
statement variables that had values bound to them using the `bind-*'
API retain their values. `clear-statement' to reset the bindings."
    (lisp Unit (stmt)
      (maybe-throw-sqlite (ffi:sqlite3-reset stmt))))

  (declare clear-statement (Statement -> Unit))
  (define (clear-statement stmt)
    "Reset All Bindings On A Prepared Statement.

Contrary to the intuition of many, `reset-statement' does not reset the
bindings on a prepared statement. Use this routine to reset all host
parameters to NULL."
    (lisp Unit (stmt)
      (maybe-throw-sqlite (ffi:sqlite3-clear-bindings stmt))))

  (declare statement-database (Statement -> Database))
  (define (statement-database stmt)
    "Find The Database Handle Of A Prepared Statement."
    (lisp Database (stmt) 
      (ffi:sqlite3-db-handle stmt)))

  (declare with-statement (Database -> String -> (Statement -> :t) -> :t))
  (define (with-statement db sql func)
    "Safely prepare a `Statement' that will be finalized as the stack unwinds."
    (let stmt = (prepare-statement db sql))
    (lisp :t (func stmt)
      (cl:unwind-protect (call-coalton-function func stmt)
        (call-coalton-function finalize-statement stmt))))

  (inline)
  (declare step-statement (Statement -> Boolean))
  (define (step-statement stmt)
    "Evaluate An SQL Statement.

After a prepared statement has been prepared using
`prepare-statement', this function must be called one or more times to
evaluate the statement."
    (lisp Boolean (stmt)
      (cl:let ((error-code (ffi:sqlite3-step stmt)))
        (cl:case error-code
          (100 True)
          (101 False)
          (cl:otherwise (throw-sqlite error-code) False)))))

  (inline)
  (declare column-count (Statement -> UFix))
  (define (column-count stmt)
    "Number Of Columns In A Result Set

Return the number of columns in the result set returned by the
prepared statement. If this routine returns 0, that means the prepared
statement returns no data (for example an UPDATE). However, just
because this routine returns a positive number does not mean that one
or more rows of data will be returned. A SELECT statement will always
have a positive column count but depending on the WHERE
clause constraints and the table content, it might return no rows."
    (lisp UFix (stmt)
      (ffi:sqlite3-column-count stmt)))

  (inline)
  (declare column-type (Statement -> UFix -> const:SqliteType))
  (define (column-type stmt index)
    "The `column-type' function returns the datatype code for the
initial data type of the result column. The returned value is one of
`SqliteInt' `SqliteFloat', `SqliteText', `SqliteBlob', or
`SqliteNull'. The return value of `column-type' can be used to decide
which of the first six interface should be used to extract the column
value. The value returned by `column-type' is only meaningful if no
automatic type conversions have occurred for the value in
question. After a type conversion, the result of calling `column-type'
is undefined, though harmless. Future versions of SQLite may change
the behavior of `column-type' following a type conversion."
    (const:load
     (lisp U8 (stmt index)
       (ffi:sqlite3-column-type stmt index))))

  (inline)
  (declare column-text (Statement -> UFix -> String))
  (define (column-text stmt index)
    "Read a string from column."
    (lisp String (stmt index)
      (ffi:sqlite3-column-text stmt index)))

  (inline)
  (declare column-i64 (Statement -> UFix -> I64))
  (define (column-i64 stmt index)
    "Read an integer from column."
    (lisp I64 (stmt index)
      (ffi:sqlite3-column-int64 stmt index)))

  (inline)
  (declare column-f64 (Statement -> UFix -> F64))
  (define (column-f64 stmt index)
    "Read a float from column."
    (lisp F64 (stmt index)
      (ffi:sqlite3-column-double stmt index)))

  (inline)
  (declare column-blob (Statement -> UFix -> (vec:Vector U8)))
  (define (column-blob stmt index)
    "Read a blob from column."
    (lisp (vec:Vector U8) (stmt index)
      (cffi:foreign-array-to-lisp
       (ffi:sqlite3-column-blob stmt index)
       (cl:list :array :uint8 (ffi:sqlite3-column-bytes stmt index))
       :adjustable cl:t)))

  (inline)
  (declare column-name (Statement -> UFix -> String))
  (define (column-name stmt index)
    "This function returns the name assigned to a particular column in
the result set of a SELECT statement. The `column-name' interface
returns a string. The first parameter is the prepared statement that
implements the SELECT statement. The second parameter is the column
number. The leftmost column is number 0."
    (lisp String (stmt index)
      (ffi:sqlite3-column-name stmt index)))

  (inline)
  (declare parameter-count (Statement -> UFix))
  (define (parameter-count stmt)
    "This function can be used to find the number of SQL parameters in a
prepared statement. SQL parameters are tokens of the form ?, ?NNN,
:AAA, $AAA, or @AAA that serve as placeholders for values that are
bound to the parameters at a later time.

This function actually returns the index of the largest (rightmost)
parameter. For all forms except ?NNN, this will correspond to the
number of unique parameters. If parameters of the ?NNN form are used,
there may be gaps in the list."
    (lisp UFix (stmt)
      (ffi:sqlite3-bind-parameter-count stmt)))

  (inline)
  (declare parameter-index (Statement -> String -> UFix))
  (define (parameter-index stmt name)
    "Return the index of an SQL parameter given its name. The index value
returned is suitable for use as the second parameter to `bind-*'. A
zero is returned if no matching parameter is found."
    (lisp UFix (stmt name)
      (ffi:sqlite3-bind-parameter-index stmt name)))

  (inline)
  (declare parameter-name (Statement -> UFix -> String))
  (define (parameter-name stmt index)
    "The `parameter-name' function returns the name of the N-th SQL
parameter in the prepared statement P. SQL parameters of the form?NNN
or :AAA or @AAA or $AAA have a name which is the string ?NNN or :AAA
or @AAA or $AAA respectively. In other words, the initial : or $ or @
or ? is included as part of the name. Parameters of the form ? without
a following integer have no name and are referred to as nameless or
anonymous parameters.

The first host parameter has an index of 1, not 0.

If the value N is out of range or if the N-th parameter is nameless,
then NULL is returned."
    (lisp String (stmt index)
      (ffi:sqlite3-bind-parameter-name stmt index)))

  (inline)
  (declare bind-i64 (Statement -> UFix -> I64 -> Unit))
  (define (bind-i64 stmt index x)
    "Bind an integer to statement."
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-int64 stmt index x))))

  (inline)
  (declare bind-f64 (Statement -> UFix -> F64 -> Unit))
  (define (bind-f64 stmt index x)
    "Bind a float to statement."
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-double stmt index x))))

  (declare bind-text (Statement -> UFix -> String -> Unit))
  (define (bind-text stmt index x)
    "Bind a string to statement."
    (lisp Unit (stmt index x)
      (maybe-throw-sqlite (ffi:sqlite3-bind-text stmt index x -1 (ffi:destructor-transient)))))

  (inline)
  (declare bind-blob (Statement -> UFix -> (vec:Vector U8) -> Unit))
  (define (bind-blob stmt index x)
    "Bind a blob to statement."
    (let len = (vec:length x))
    (lisp Unit (stmt index x len)
      ;; TODO, switch to this once we can use LispArray
      ;; (cffi:with-pointer-to-vector-data (ptr x)
      ;;   (maybe-throw-sqlite (ffi:sqlite3-bind-blob stmt index ptr len (ffi:destructor-transient))))
      (cffi:with-foreign-object (ptr :unsigned-char len)
        (cl:dotimes (i len)
          (cl:setf (cffi:mem-aref ptr :unsigned-char i) (cl:aref x i)))
        (maybe-throw-sqlite 
         (ffi:sqlite3-bind-blob stmt index ptr len (ffi:destructor-transient))))))

  (inline)
  (declare bind-null (Statement -> UFix -> Unit))
  (define (bind-null stmt index)
    "Bind NULL to statement."
    (lisp Unit (stmt index)
      (maybe-throw-sqlite (ffi:sqlite3-bind-null stmt index)))))
