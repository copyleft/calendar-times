all: docs/calendar-times.md README.md

docs/calendar-times.md:
	sbcl --eval '(require :simple-doc)' --load 'calendar-times.asd' --eval '(require :calendar-times)' --eval '(simple-doc:generate-markdown-doc (asdf:system-relative-pathname :calendar-times "docs/calendar-times.md") (find-package :calendar-times) :output-undocumented t)' --quit

README.md:
	sbcl --eval '(require :simple-doc)' --load 'calendar-times.asd' --eval '(require :calendar-times)' --eval '(simple-doc:generate-markdown-doc (asdf:system-relative-pathname :calendar-times "README.md") (find-package :calendar-times) :output-undocumented t)' --quit

test:
	sbcl --eval '(asdf:test-system :calendar-times)' --quit

clean:
	rm docs/calendar-times.md
	rm README.md
