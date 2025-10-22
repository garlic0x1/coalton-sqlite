(defpackage #:coalton-sqlite/constants
  (:use
   #:coalton
   #:coalton-prelude)
  (:export
   #:load
   #:dump
   #:SqliteError
   #:ErrOk
   #:ErrError
   #:ErrInternal
   #:ErrPerm
   #:ErrAbort
   #:ErrBusy
   #:ErrLocked
   #:ErrNoMem
   #:ErrReadOnly
   #:ErrInterrupt
   #:ErrIOErr
   #:ErrCorrupt
   #:ErrNotFound
   #:ErrFull
   #:ErrCantOpen
   #:ErrProtocol
   #:ErrEmpty
   #:ErrSchema
   #:ErrTooBig
   #:ErrConstraint
   #:ErrMismatch
   #:ErrMisuse
   #:ErrNoLFS
   #:ErrAuth
   #:ErrFormat
   #:ErrRange
   #:ErrNotADb
   #:ErrRow
   #:ErrDone
   #:SqliteType
   #:SqliteInt
   #:SqliteFloat
   #:SqliteText
   #:SqliteBlob
   #:SqliteNull
   #:SqliteOpenFlag
   #:OpenReadOnly
   #:OpenReadWrite
   #:OpenCreate
   #:OpenURI
   #:OpenMemory
   #:OpenNoMutex
   #:OpenFullMutex
   #:OpenSharedCache
   #:OpenPrivateCache
   #:OpenNoFollow
   ))

(in-package #:coalton-sqlite/constants)

(coalton-toplevel
  (define-class (Enum :a :b (:a -> :b))
    (dump (:a -> :b))
    (load (:b -> :a))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro define-enum (name type cl:&body ctors)
    `(progn
       (derive Eq)
       (define-type ,name
         ,@(cl:mapcar #'cl:first ctors))
       (define-instance (Enum ,name ,type)
         (define (load value)
           (match value
             ,@(cl:mapcar
                (cl:lambda (ctor)
                  `(,(cl:eval (cl:second ctor))
                    ,(cl:first ctor)))
                ctors)
             (x (lisp ,name (x) (cl:error "Invalid error code ~A" x)))))
         (define (dump enum)
           (match enum
             ,@(cl:mapcar
                (cl:lambda (ctor)
                  `((,(cl:first ctor))
                    ,(cl:eval (cl:second ctor))))
                ctors)))))))

(coalton-toplevel
  (define-enum SqliteError U8
    (ErrOk         0)
    (ErrError      1)
    (ErrInternal   2)
    (ErrPerm       3)
    (ErrAbort      4)
    (ErrBusy       5)
    (ErrLocked     6)
    (ErrNoMem      7)
    (ErrReadOnly   8)
    (ErrInterrupt  9)
    (ErrIOErr      10)
    (ErrCorrupt    11)
    (ErrNotFound   12)
    (ErrFull       13)
    (ErrCantOpen   14)
    (ErrProtocol   15)
    (ErrEmpty      16)
    (ErrSchema     17)
    (ErrTooBig     18)
    (ErrConstraint 19)
    (ErrMismatch   20)
    (ErrMisuse     21)
    (ErrNoLFS      22)
    (ErrAuth       23)
    (ErrFormat     24)
    (ErrRange      25)
    (ErrNotADb     26)
    (ErrRow        100)
    (ErrDone       101))

  (define-enum SqliteType U8
    (SqliteInt   1)
    (SqliteFloat 2)
    (SqliteText  3)
    (SqliteBlob  4)
    (SqliteNull  5))

  (define-enum SqliteOpenFlag I64
    (OpenReadOnly         #x00000001) ; Ok for sqlite3_open_v2()
    (OpenReadWrite        #x00000002) ; Ok for sqlite3_open_v2()
    (OpenCreate           #x00000004) ; Ok for sqlite3_open_v2()
    ;; (OpenDeleteOnClose    #x00000008) ; VFS only
    ;; (OpenExclusive        #x00000010) ; VFS only
    ;; (OpenAutoProxy        #x00000020) ; VFS only
    (OpenURI              #x00000040) ; Ok for sqlite3_open_v2()
    (OpenMemory           #x00000080) ; Ok for sqlite3_open_v2()
    ;; (OpenMainDB           #x00000100) ; VFS only
    ;; (OpenTempDB           #x00000200) ; VFS only
    ;; (OpenTransientDB      #x00000400) ; VFS only
    ;; (OpenMainJournal      #x00000800) ; VFS only
    ;; (OpenTempJournal      #x00001000) ; VFS only
    ;; (OpenSubJournal       #x00002000) ; VFS only
    ;; (OpenSuperJournal     #x00004000) ; VFS only
    (OpenNoMutex          #x00008000) ; Ok for sqlite3_open_v2()
    (OpenFullMutex        #x00010000) ; Ok for sqlite3_open_v2()
    (OpenSharedCache      #x00020000) ; Ok for sqlite3_open_v2()
    (OpenPrivateCache     #x00040000) ; Ok for sqlite3_open_v2()
    ;; (OpenWAL              #x00080000) ; VFS only
    (OpenNoFollow         #x01000000) ; Ok for sqlite3_open_v2()
    ;; (OpenExResCode        #x02000000) ; Extended result codes
    ))
