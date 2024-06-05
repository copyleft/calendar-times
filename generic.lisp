;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :timelib-generic
  (:use :cl :calendar)
  (:local-nicknames (gcl generic-cl)))

(in-package :timelib-generic)

(defmethod gcl:add ((time-entity local-time:timestamp) (measure calendar:time-measure))
  (local-time:timestamp+ time-entity
                         (time-measure-value measure)
                         (time-measure-unit measure)))

(defmethod generic-cl:equalp ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp= a b))

(gcl:+ (local-time:now) (make-time-measure 2 :year))

(defmethod gcl:add ((time-entity timelib:time-entity) (measure calendar:time-measure))
  (timelib:time-entity+ time-entity (time-measure-value measure)
                      (time-measure-unit measure)))

(gcl:+ (timelib::today) (make-time-measure 2 :year))

(defmethod gcl:lessp ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp< a b))

(gcl:< (local-time:now) (gcl:+ (local-time:now) (make-time-measure 2 :minute)))

(defmethod generic-cl:subtract ((a local-time:timestamp) (b local-time:timestamp))
  (local-time:timestamp-difference a b))

(defmethod generic-cl:subtract ((a timelib:time-entity) (b timelib:time-entity))
  (timelib::time-entity-difference a b))
