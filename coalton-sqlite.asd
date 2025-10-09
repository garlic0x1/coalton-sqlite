(asdf:defsystem "coalton-sqlite"
  :author "garlic0x1"
  :license "MIT"
  :depends-on ("coalton" "cffi")
  :components ((:module "src"
                :components ((:file "ffi")
                             (:file "sqlite")
                             (:file "query")
                             (:file "package"))))
  :in-order-to ((test-op (test-op "coalton-sqlite/test"))))

(asdf:defsystem "coalton-sqlite/test"
  :depends-on ("coalton-sqlite" "coalton/testing")
  :components ((:module "test"
                :components ((:file "package")
                             (:file "sqlite-tests")
                             (:file "query-tests"))))
  :perform (asdf:test-op
            (o s)
            (uiop:symbol-call '#:coalton-sqlite/test '#:run-tests)))
