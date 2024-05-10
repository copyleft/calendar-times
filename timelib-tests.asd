(asdf:defsystem timelib-tests
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "A calendar times library on top of local-time"
  :depends-on (:timelib :fiasco)
  :components ((:file "tests")))

