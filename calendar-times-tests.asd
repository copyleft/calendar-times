(asdf:defsystem calendar-times-tests
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "A calendar times library on top of local-time"
  :depends-on (:calendar-times :fiasco)
  :components ((:file "tests"))
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :fiasco :run-tests 'calendar-times-tests)))

