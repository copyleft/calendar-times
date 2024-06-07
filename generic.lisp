;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :calendar-times-generic
  (:use :cl :calendar)
  (:local-nicknames (gcl generic-cl)))

(in-package :calendar-times-generic)

(defmethod gcl:add ((caltime local-time:timestamp) (measure calendar:time-measure))
  (local-time:timestamp+ caltime
                         (time-measure-value measure)
                         (time-measure-unit measure)))

(defmethod generic-cl:equalp ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp= a b))

(gcl:+ (local-time:now) (make-time-measure 2 :year))

(defmethod gcl:add ((caltime calendar-times:caltime) (measure calendar:time-measure))
  (calendar-times:caltime+ caltime (time-measure-value measure)
                      (time-measure-unit measure)))

(gcl:+ (calendar-times::today) (make-time-measure 2 :year))

(defmethod gcl:lessp ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp< a b))

(gcl:< (local-time:now) (gcl:+ (local-time:now) (make-time-measure 2 :minute)))

(defmethod generic-cl:subtract ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp-difference a b))

(defmethod generic-cl:subtract ((a calendar-times:caltime) (b calendar-times:caltime))
  (calendar-times::caltime-difference a b))
