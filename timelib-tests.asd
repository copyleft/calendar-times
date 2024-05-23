(asdf:defsystem timelib-tests
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "A calendar times library on top of local-time"
  :depends-on (:timelib :fiasco)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :fiasco :run-tests 'timelib-tests)))

