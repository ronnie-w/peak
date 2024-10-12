LISP?=sbcl
run: FORCE
	@$(LISP) --load peak.asd \
			 --eval '(ql:quickload :peak)' \
			 --eval '(in-package :peak)' \
			 --eval '(main)'

build: FORCE
	@$(LISP) --load peak.asd \
			   --eval '(ql:quickload :peak)' \
			   --eval '(asdf:make :peak)' \
			   --eval '(quit)'

FORCE:
