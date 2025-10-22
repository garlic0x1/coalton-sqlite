(defpackage #:coalton-sqlite/see
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:ffi #:coalton-sqlite/ffi)
   (#:sqlite #:coalton-sqlite/sqlite))
  (:export
   #:key-database
   #:rekey-database
   #:activate-encryption-extension
   ))

(in-package #:coalton-sqlite/see)

(cffi:defcfun sqlite3-key-v3 ffi:error-code
  (db ffi:p-sqlite3)
  (z-db-name :string)
  (key :string)
  (key-size :int)
  (codec :string))

(cffi:defcfun sqlite3-rekey-v3 ffi:error-code
  (db ffi:p-sqlite3)
  (z-db-name :string)
  (key :string)
  (key-size :int)
  (codec :string))

(cffi:defcfun sqlite3-activate-see :void
  (activation-code :string))

(coalton-toplevel
  (declare key-database (sqlite:Database -> String -> Unit))
  (define (key-database db key)
    "Use the `open-database' function to open an encrypted database
or any database that you want to rekey. Immediately after opening,
specify the key using `key-database'."
    (lisp Unit (db key)
      (sqlite::maybe-throw-sqlite
       (sqlite3-key-v3 db (cffi:null-pointer) key (cl:length key) (cffi:null-pointer)))))

  (declare rekey-database (sqlite:Database -> (Optional String) -> Unit))
  (define (rekey-database db key)
    "You can change the key on a database using the `rekey-database'
function.

A `None' key decrypts the database.

Rekeying requires that every page of the database file be read,
decrypted, reencrypted with the new key, then written out
again. Consequently, rekeying can take a long time on a larger
database."
    (match key
      ;; Case #1: Decrypt Database
      ((None)
       (lisp Unit (db)
         (sqlite::maybe-throw-sqlite
          (sqlite3-rekey-v3 db (cffi:null-pointer) (cffi:null-pointer) 0 (cffi:null-pointer)))))
      ;; Case #2: Rekey Database
      ((Some key)
       (lisp Unit (db key)
         (sqlite::maybe-throw-sqlite
          (sqlite3-rekey-v3 db (cffi:null-pointer) key (cl:length key) (cffi:null-pointer)))))))

  (declare activate-encryption-extension (String -> Unit))
  (define (activate-encryption-extension passphrase)
    "If you deploy the SQLite encryption extension as a DLL or shared
library then you must first activate the library by invoking
`activate-encryption'.

The argument is your product activation key. The activation key is
available as plain-text in the source code so you can clearly see what
it is. The purpose of the activation key is to prevent one of your
customers from extracting the SQLite library and using it separately
from your application. Without knowledge of the activation key, which
only you should know, your users will be unable to access the
encryption features."
    (lisp Unit (passphrase)
      (sqlite3-activate-see passphrase)
      Unit)))
