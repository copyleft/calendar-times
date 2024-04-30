;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :timelib-generic
  (:use :cl)
  (:local-nicknames (gcl generic-cl)))

(in-package :timelib-generic)

(defstruct time-measure
  value unit)

(defmethod gcl:add ((timestamp local-time:timestamp) (measure time-measure))
  (local-time:timestamp+ timestamp
                         (time-measure-value measure)
                         (time-measure-unit measure)))

(gcl:+ (local-time:now) (make-time-measure :value 2 :unit :year))

