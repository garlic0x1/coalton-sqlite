(defpackage #:coalton-sqlite/cache
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
   (#:queue #:coalton-library/queue)
   (#:table #:coalton-library/hashtable)
   (#:iter #:coalton-library/iterator)
   (#:sqlite #:coalton-sqlite/sqlite))
  (:export
   #:StatementCache
   #:with-statement-cache
   #:with-cached-statement))

(in-package #:coalton-sqlite/cache)

(coalton-toplevel
  (define-struct StatementCache
    "A synchronous FIFO replacement cache for SQlite statements.

Finalizing statements that are not already in the cache causes
them to be enqueued into the cache, deferring ~real~ finalization
until more space is needed."
    (db
     "The database associated with the cache."
     sqlite:Database)
    (size
     "Maximum amount of statements to store."
     UFix)
    (fifo
     "FIFO queue of keys, when the cache is full one is dequeued and
finalized to make room."
     (queue:Queue String))
    (cache
     "`HashTable' mapping SQL strings to `Statement' objects."
     (table:HashTable String sqlite:Statement)))

  (declare make-statement-cache (sqlite:Database -> UFix -> StatementCache))
  (define (make-statement-cache db size)
    (StatementCache db size (queue:new) (table:new)))

  (declare finalize-statement-cache (StatementCache -> Unit))
  (define (finalize-statement-cache cache)
    (iter:for-each! sqlite:finalize-statement
                    (table:values (.cache cache))))

  (declare with-statement-cache (sqlite:Database -> UFix -> (StatementCache -> :t) -> :t))
  (define (with-statement-cache db size func)
    "Create a `StatementCache' using `db' holding up to `size' statements.

Usage of a `StatementCache' shall be constrained to a single thread."
    (let cache = (make-statement-cache db size))
    (lisp :t (func cache)
      (cl:unwind-protect (call-coalton-function func cache)
        (call-coalton-function finalize-statement-cache cache))))

  (inline)
  (declare maybe-dequeue-cache (StatementCache -> Unit))
  (define (maybe-dequeue-cache cache)
    (let (StatementCache _ size fifo cache) = cache)
    (when (== size (queue:length fifo))
      (let sql = (queue:pop-unsafe! fifo))
      (sqlite:finalize-statement (unwrap (table:get cache sql)))
      (table:remove! cache sql)))

  (inline)
  (declare enqueue-cache (StatementCache -> String -> sqlite:Statement -> Unit))
  (define (enqueue-cache cache sql stmt)
    (let (StatementCache _ _ fifo cache) = cache)
    (queue:push! sql fifo)
    (table:set! cache sql stmt)
    Unit)

  (inline)
  (declare prepare-statement (StatementCache -> String -> sqlite:Statement))
  (define (prepare-statement cache sql)
    (match (table:get (.cache cache) sql)
      ((Some stmt)
       (sqlite:reset-statement stmt)
       stmt)
      ((None)
       (sqlite:prepare-statement (.db cache) sql))))

  (inline)
  (declare finalize-statement (StatementCache -> String -> sqlite:Statement -> Unit))
  (define (finalize-statement cache sql stmt)
    (maybe-dequeue-cache cache)
    (match (table:get (.cache cache) sql)
      ((Some _stmt) Unit)
      ((None) (enqueue-cache cache sql stmt))))

  (declare with-cached-statement (StatementCache -> String -> (sqlite:Statement -> :t) -> :t))
  (define (with-cached-statement cache sql func)
    "Get a `Statement' from `cache' that will be put back in the cache as
the stack unwinds, possibly finalizing some other `Statement'."
    (let stmt = (prepare-statement cache sql))
    (lisp :t (cache sql stmt func)
      (cl:unwind-protect (call-coalton-function func stmt)
        (finalize-statement cache sql stmt)))))
