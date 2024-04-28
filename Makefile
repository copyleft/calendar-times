docs/timelib.md:
	sbcl --eval '(require :simple-doc)' --load 'timelib.asd' --eval '(require :timelib)' --eval '(simple-doc:generate-markdown-doc (asdf:system-relative-pathname :timelib "docs/timelib.md") (find-package :timelib))' --quit

clean:
	rm docs/timelib.md
