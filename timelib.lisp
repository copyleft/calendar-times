(defpackage :timelib
  (:use :cl))

(in-package :timelib)

(defclass timestamp ()
  ())

(defclass walltime (timestamp)
  ((hour :initarg :hour
         :accessor hour-of
         :type integer)
   (minutes :initarg :minutes
           :accessor minutes-of
           :type integer)
   (seconds :initarg :seconds
           :accessor seconds-of
            :type integer)))

(defclass date (timestamp)
  ((year :initarg :year
         :accessor year-of)
   (month :initarg :month
          :accessor month-of)
   (day :initarg :day
        :accessor day-of)))

(defclass local-datetime (date walltime)
  ())

(defclass zoned-datetime (local-datetime)
  ((timezone :initarg :timezone
             :accessor timezone-of)))

(defun date->local-time (timestamp)
  (local-time:encode-timestamp
   0 0 0 0
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)))

(defun local-datetime->local-time (timestamp &optional (timezone local-time:*default-timezone*) offset)
  (check-type timestamp local-datetime)
  (local-time:encode-timestamp
   0
   (seconds-of timestamp)
   (minutes-of timestamp)
   (hour-of timestamp)
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)))

(defun local-datetime->universal (timestamp &optional timezone offset)
  (local-time:timestamp-to-universal
   (local-datetime->local-time timestamp timezone offset)))

(defun zoned-datetime->local-time (timestamp)
  (check-type timestamp zoned-datetime)
  (local-time:encode-timestamp
   0
   (seconds-of timestamp)
   (minutes-of timestamp)
   (hour-of timestamp)
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)
   :timezone (timezone-of timestamp)))

(defgeneric format-timestamp (destination timestamp &rest args))

(defmethod format-timestamp (destination (timestamp zoned-datetime) &rest args)
  (local-time:format-timestring destination (zoned-datetime->local-time timestamp)
                                :timezone (timezone-of timestamp)))

(defmethod format-timestamp (destination (timestamp date) &rest args)
  (local-time:format-timestring
   destination
   (date->local-time timestamp)
   :format local-time:+iso-8601-date-format+))

(defmethod print-object ((timestamp timestamp) stream)
  (print-unreadable-object (timestamp stream :type t)
    (format-timestamp stream timestamp)))

(make-instance 'zoned-datetime
               :day 1
               :month 1
               :year 2024
               :hour 1
               :minutes 0
               :seconds 0
               :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))

(make-instance 'date
               :day 1
               :month 1
               :year 2024)

(defgeneric timestamp-convert (timestamp class &rest args))

(defmethod timestamp-convert ((timestamp local-datetime) (class (eql 'date)) &rest args)
  (make-instance 'date
                 :day (day-of timestamp)
                 :month (month-of timestamp)
                 :year (year-of timestamp)))

(defgeneric timestamp->local-time (timestamp))
(defmethod timestamp->local-time ((timestamp date))
  (local-time:encode-timestamp
   0 0 0 0
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)))

(defun local-time->timestamp (local-time timestamp-class))
  

(defgeneric timestamp+ (timestamp amount unit))
(defmethod timestamp+ ((timestamp timestamp) amount unit)
  (let ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit)))
    (local-time->timestamp lt)))

(defmethod timestamp+ ((timestamp date) amount unit)
  (let ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit)))
    (make-instance 'date :day (local-time:timestamp-day lt)
                         :month (local-time:timestamp-month lt)
                         :year (local-time:timestamp-year lt))))

(let ((day (make-instance 'date :day 1 :month 1 :year 2024)))
  (timestamp+ day 1 :day))
