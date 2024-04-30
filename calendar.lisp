;; https://github.com/Cuis-Smalltalk/Calendars/blob/master/Chalten/A%20Point%20Based%20Model%20of%20the%20Gregorian%20Calendar.pdf

(defpackage :calendar
  (:use :cl)
  (:export
   :time-measure
   :time-measure-value
   :time-measure-unit
   :year
   :year-value))

(in-package :calendar)

(defstruct time-measure
  value unit)

(defstruct year
  number)

(make-year :number 2020)
