;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :calendar
  (:use :cl)
  (:export
   :time-measure
   :time-measure-value
   :time-measure-unit
   :year
   :year-value
   :make-time-measure))

(in-package :calendar)

(defstruct (time-measure
            (:constructor %make-time-measure))
  value unit)

(defun make-time-measure (value unit)
  (%make-time-measure :value value :unit unit))

(defstruct year
  number)

(make-year :number 2020)
