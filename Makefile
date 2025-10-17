LISP ?= ${shell which sbcl}

clean:
	find . -type f -name '*.fasl' -delete
	find . -type f -name '*~' -delete

docs:
	$(LISP) --load coalton-sqlite.asd \
		--eval "(asdf:load-system :coalton-sqlite)" \
		--eval "(ql:quickload :slynk)" \
		--eval "(ql:quickload :coalton-docgen)" \
		--eval "(with-open-file (s \"REFERENCE.md\" :direction ':output :if-exists ':supersede) (coalton-docgen:render-docs s :coalton-sqlite/sqlite :coalton-sqlite/value :coalton-sqlite/query))" \
		--quit
