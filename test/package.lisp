(defpackage #:coalton-sqlite/test
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-testing
   #:coalton-sqlite)
  (:local-nicknames
   ;; (#:cache #:coalton-sqlite/cache)
   (#:ffi #:coalton-sqlite/ffi))
  (:local-nicknames
   (#:result #:coalton-library/result))
  (:export
   #:run-tests))

(in-package #:coalton-sqlite/test)

(fiasco:define-test-package #:coalton-sqlite/fiasco-test-package)
(coalton-fiasco-init #:coalton-sqlite/fiasco-test-package)

(cl:defun run-tests (cl:&optional interactive)
  (fiasco:run-package-tests
   :packages '(#:coalton-sqlite/fiasco-test-package)
   :interactive interactive))
