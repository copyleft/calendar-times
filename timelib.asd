(asdf:defsystem timelib
  :author "Mariano Montone <marianomontone@gmail.com>"
  :description "A calendar times library on top of local-time"
  :depends-on (:local-time :alexandria :uiop)
  :components ((:file "timelib")))
