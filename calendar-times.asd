(asdf:defsystem calendar-times
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "A calendar times library on top of local-time"
  :depends-on (:local-time :alexandria :uiop :split-sequence :closer-mop)
  :components ((:file "calendar-times"))
  :in-order-to ((asdf:test-op (asdf:test-op :calendar-times-tests))))
