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

(defstruct (year
            (:constructor %make-year))
  number)

(defun year (number)
  (%make-year :number number))

(year 2020)

(defstruct (month
            (:constructor %make-month))
  number)

(defun month (number-or-name)
  (%make-month :number number-or-name))

(month 1)

(defstruct (day-of-month
            (:constructor %make-day-of-month))
  day month)

(defun day-of-month (day month)
  (%make-day-of-month :day day :month month))

(defstruct (week
            (:constructor %make-week))
  number year)

(defstruct (semester
            (:constructor %make-semester))
  number year)

(defun yesterday ())

(defun tomorrow ())

(defgeneric distance-between (ce1 ce2)
  (:documentation "Distance between two calendar entities."))

(defmethod distance-between ((y1 year) (y2 year)) 
  (make-time-measure (abs (- (year-number y2) (year-number y1))) :year))

(distance-between (year 2005) (year 2006))

(defgeneric days-of (entity))
(defgeneric months-of (entity))
(defgeneric hours-of (entity))
(defgeneric minutes-of (entity))
(defgeneric weeks-of (entity))

(defstruct time-period
  years months weeks days hours minutes seconds)

(make-time-period :years 2 :weeks 30)
