;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :timelib-generic
  (:use :cl :calendar)
  (:local-nicknames (gcl generic-cl)))

(in-package :timelib-generic)

(defmethod gcl:add ((timestamp local-time:timestamp) (measure calendar:time-measure))
  (local-time:timestamp+ timestamp
                         (time-measure-value measure)
                         (time-measure-unit measure)))

(gcl:+ (local-time:now) (make-time-measure 2 :year))

(defmethod gcl:add ((timestamp timelib:timestamp) (measure calendar:time-measure))
  (timelib:timestamp+ timestamp (time-measure-value measure)
                      (time-measure-unit measure)))

(gcl:+ (timelib::today) (make-time-measure 2 :year))

(defmethod gcl:lessp ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp< a b))

(gcl:< (local-time:now) (gcl:+ (local-time:now) (make-time-measure 2 :minute)))
