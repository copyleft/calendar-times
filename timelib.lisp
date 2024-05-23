(defpackage :timelib
  (:use :cl)
  (:export
   ;; classes
   #:timestamp
   #:walltime
   #:date
   #:datetime
   #:zoned-datetime
   #:zoned-date

   ;; constructors
   #:make-time
   #:make-date
   #:make-datetime
   #:make-zoned-date
   #:make-zoned-datetime

   ;; accessors
   #:seconds-of
   #:minutes-of
   #:hour-of
   #:day-of
   #:month-of
   #:year-of
   #:timezone-of
   #:datetime-time
   #:datetime-date
   #:decode-timestamp

   ;; comparisons
   #:timestamp-equalp
   #:timestamp=
   #:timestamp<
   #:timestamp<=
   #:timestamp>
   #:timestamp>=

   ;; calculations
   #:timestamp+
   #:timestamp-
   #:timestamp-difference
   #:day-of-week

   ;; conversions
   #:timestamp-adjust
   #:timestamps-compose
   #:timestamp-coerce
   #:timestamp->local-time
   #:timestamp->universal-time

   ;; constants
   #:+months-per-year+
   #:+days-per-week+
   #:+hours-per-day+
   #:+minutes-per-day+
   #:+minutes-per-hour+
   #:+seconds-per-day+
   #:+seconds-per-hour+
   #:+seconds-per-minute+

   ;; operations
   #:clone-timestamp

   ;; utilities
   #:time-now
   #:now
   #:today

   ;; formatting
   #:format-timestamp

   ;; parsing
   #:parse-timestring)
  (:documentation "TIMELIB is a calendar time library implemented on top of LOCAL-TIME library.

It features zoned timestamps and calculations."))

(in-package :timelib)

;; ** Constants

(defconstant +months-per-year+ 12)
(defconstant +days-per-week+ 7)
(defconstant +hours-per-day+ 24)
(defconstant +minutes-per-day+ 1440)
(defconstant +minutes-per-hour+ 60)
(defconstant +seconds-per-day+ 86400)
(defconstant +seconds-per-hour+ 3600)
(defconstant +seconds-per-minute+ 60)
(defvar +day-names+ #(:sunday :monday :tuesday :wednesday :thursday :friday :saturday))

;; ** Timestamp classes

(defclass timestamp ()
  ()
  (:documentation "Abstract timestamp class"))

(defclass walltime (timestamp)
  ((hour :reader hour-of
         :type integer)
   (minutes :reader minutes-of
            :type integer)
   (seconds :reader seconds-of
            :type integer))
  (:documentation "Represents a 'wall' time. Like 01:01:22"))

(defclass date (timestamp)
  ((year :reader year-of)
   (month :reader month-of)
   (day :reader day-of))
  (:documentation "A date like 2024-01-01"))

(defclass datetime (date walltime)
  ()
  (:documentation "A datetime like 2024-01-01T00:00:00"))

(defclass zoned-timestamp ()
  ((timezone :reader timezone-of
             :initform local-time:+utc-zone+
             :type (or local-time::timezone integer)
             :documentation "Timezone can be a LOCAL-TIME::TIMEZONE object, or an offset."))
  (:documentation "A timestamp with timezone. Abstract class."))

(defclass zoned-datetime (datetime zoned-timestamp)
  ()
  (:documentation "A datetime with a timezone."))

(defclass zoned-date (date zoned-timestamp)
  ()
  (:documentation "A date with a timezone."))

;; ** Utility

(defun ensure-timezone (timezone-or-string)
  (etypecase timezone-or-string
    (local-time::timezone timezone-or-string)
    (string (or (local-time:find-timezone-by-location-name timezone-or-string)
                (error "Timezone not found: ~s" timezone-or-string)))))

(defun offset->string (offset)
  "Format OFFSET. OFFSET is in seconds."
  (multiple-value-bind (offset-hours offset-secs)
      (truncate offset +seconds-per-hour+)
    (declare (fixnum offset-hours offset-secs))
    (format nil "~c~2,'0d~:[:~;~]~2,'0d"
            (if (minusp offset) #\- #\+)
            (abs offset-hours)
            nil
            (round (abs offset-secs)
                   local-time:+seconds-per-minute+))))

(defun make-gmt-offset-timezone (offset)
  "Create a GMT + OFFSET timezone. OFFSET is in seconds."
  (local-time::%make-simple-timezone
   (format nil "GMT ~a" (offset->string offset))
   (format nil "GMT ~a" (offset->string offset))
   offset))

;; (make-gmt-offset-timezone (* -3600 3))

;; ** Constructors

(defun make-time (seconds minutes hour)
  "Create a time object."
  (unless (local-time::valid-timestamp-p 0 seconds minutes hour 1 1 1970)
    (error "Invalid walltime: ~2,'0d:~2,'0d:~2,'0d" hour minutes seconds))
  (let ((walltime (make-instance 'walltime)))
    (setf (slot-value walltime 'hour) hour
          (slot-value walltime 'minutes) minutes
          (slot-value walltime 'seconds) seconds)
    walltime))

(defun make-date (day month year)
  "Create a date object from DAY, MONTH and YEAR."
  (unless (local-time::valid-timestamp-p 0 0 0 0 day month year)
    (error "Invalid date: ~4,'0d-~2,'0d-~2,'0d" year month day))
  (let ((date (make-instance 'date)))
    (setf (slot-value date 'year) year
          (slot-value date 'month) month
          (slot-value date 'day) day)
    date))

(defun make-datetime (seconds minutes hour day month year)
  "Create a date and time object."
  (unless (local-time::valid-timestamp-p 0 seconds minutes hour day month year)
    (error "Invalid datetime: ~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
           year month day hour minutes seconds))
  (let ((datetime (make-instance 'datetime)))
    (setf (slot-value datetime 'hour) hour
          (slot-value datetime 'minutes) minutes
          (slot-value datetime 'seconds) seconds
          (slot-value datetime 'year) year
          (slot-value datetime 'month) month
          (slot-value datetime 'day) day)
    datetime))

;; (make-datetime 0 0 0 1 1 2024)
;; (make-datetime 0 0 0 30 2 2024)

(defun make-zoned-date (day month year &optional (timezone local-time:*default-timezone*))
  "Create a date with a timezone."
  (unless (local-time::valid-timestamp-p 0 0 0 0 day month year)
    (error "Invalid date: ~4,'0d-~2,'0d-~2,'0d"
           year month day))
  (let ((date (make-instance 'zoned-date)))
    (setf (slot-value date 'year) year
          (slot-value date 'month) month
          (slot-value date 'day) day
          (slot-value date 'timezone)
          (etypecase timezone
            (integer timezone)
            (t (ensure-timezone timezone))))
    date))

(defun make-zoned-datetime (seconds minutes hour day month year &optional (timezone local-time:*default-timezone*))
  "Create a datetime with a timezone."
  (unless (local-time::valid-timestamp-p 0 seconds minutes hour day month year)
    (error "Invalid datetime: ~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
           year month day hour minutes seconds))
  (let ((datetime (make-instance 'zoned-datetime)))
    (setf (slot-value datetime 'hour) hour
          (slot-value datetime 'minutes) minutes
          (slot-value datetime 'seconds) seconds
          (slot-value datetime 'year) year
          (slot-value datetime 'month) month
          (slot-value datetime 'day) day
          (slot-value datetime 'timezone)
          (etypecase timezone
            (integer timezone)
            (t (ensure-timezone timezone))))
    datetime))

;; ** Object accessors

(defun datetime-date (datetime)
  "Returns the DATE of DATETIME"
  (make-date (day-of datetime)
             (month-of datetime)
             (year-of datetime)))

(defun datetime-time (datetime)
  "Returns the WALLTIME of DATETIME."
  (make-time
   (seconds-of datetime)
   (minutes-of datetime)
   (hour-of datetime)))

(defgeneric decode-timestamp (timestamp)
  (:documentation "Decode a TIMESTAMP parts and return them with VALUES.
The order of the list of values is the same as passed to the constructor functions."))

(defmethod decode-timestamp ((time walltime))
  (values (seconds-of time)
          (minutes-of time)
          (hour-of time)))

(defmethod decode-timestamp ((date date))
  (values (day-of date)
          (month-of date)
          (year-of date)))

(defmethod decode-timestamp ((datetime datetime))
  (values (seconds-of datetime)
          (minutes-of datetime)
          (hour-of datetime)
          (day-of datetime)
          (month-of datetime)
          (year-of datetime)))

(defmethod decode-timestamp ((datetime zoned-datetime))
  (values (seconds-of datetime)
          (minutes-of datetime)
          (hour-of datetime)
          (day-of datetime)
          (month-of datetime)
          (year-of datetime)
          (timezone-of datetime)))

;; ** Conversions

(defun timestamp->universal-time (timestamp)
  "Convert TIMESTAMP to UNIVERSAL-TIME."
  (local-time:timestamp-to-universal
   (timestamp->local-time timestamp)))

(defun time->local-time (timestamp)
  "Convert WALLTIME to TIMESTAMP."
  (local-time:encode-timestamp
   0
   (seconds-of timestamp)
   (minutes-of timestamp)
   (hour-of timestamp)
   1 1 1970
   :timezone local-time:+utc-zone+))

(defun date->local-time (timestamp)
  (local-time:encode-timestamp
   0 0 0 0
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)
   :timezone local-time:+utc-zone+))

(defun datetime->local-time (timestamp &optional (timezone local-time:*default-timezone*) offset)
  (check-type timestamp datetime)
  (local-time:encode-timestamp
   0
   (seconds-of timestamp)
   (minutes-of timestamp)
   (hour-of timestamp)
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)
   :timezone timezone
   :offset offset))

(defun zoned-datetime->local-time (timestamp)
  (check-type timestamp zoned-datetime)
  (etypecase (timezone-of timestamp)
    (integer ;; offset
     (local-time:encode-timestamp
      0
      (seconds-of timestamp)
      (minutes-of timestamp)
      (hour-of timestamp)
      (day-of timestamp)
      (month-of timestamp)
      (year-of timestamp)
      :offset (timezone-of timestamp)))
    (local-time::timezone
     (local-time:encode-timestamp
      0
      (seconds-of timestamp)
      (minutes-of timestamp)
      (hour-of timestamp)
      (day-of timestamp)
      (month-of timestamp)
      (year-of timestamp)
      :timezone (timezone-of timestamp)))))

(defun zoned-date->local-time (timestamp)
  (check-type timestamp zoned-date)
  (etypecase (timezone-of timestamp)
    (local-time::timezone
     (local-time:encode-timestamp
      0 0 0 0
      (day-of timestamp)
      (month-of timestamp)
      (year-of timestamp)
      :timezone (timezone-of timestamp)))
    (integer ;; offset
     (local-time:encode-timestamp
      0 0 0 0
      (day-of timestamp)
      (month-of timestamp)
      (year-of timestamp)
      :offset (timezone-of timestamp)))))

(defgeneric timestamp-coerce (timestamp class &rest args)
  (:method (timestamp class &rest args)
    (declare (ignore args))
    (error "Can't coerce ~s to ~s" timestamp class))
  (:documentation "Convert between different classes of time types."))

(defmethod timestamp-coerce ((timestamp datetime) (class (eql 'date)) &rest args)
  (declare (ignore args))
  (make-date (day-of timestamp)
             (month-of timestamp)
             (year-of timestamp)))

(defmethod timestamp-coerce ((timestamp datetime) (class (eql 'zoned-datetime)) &rest args)
  (make-zoned-datetime (seconds-of timestamp)
                       (minutes-of timestamp)
                       (hour-of timestamp)
                       (day-of timestamp)
                       (month-of timestamp)
                       (year-of timestamp)
                       (or (car args) local-time:+utc-zone+)))

(defmethod timestamp-coerce ((timestamp datetime) (class (eql 'time)) &rest args)
  (declare (ignore args))
  (make-time (seconds-of timestamp)
             (minutes-of timestamp)
             (hour-of timestamp)))

(defmethod timestamp-coerce ((timestamp zoned-datetime) (class (eql 'datetime)) &rest args)
  (declare (ignore args))
  (make-datetime (seconds-of timestamp)
                 (minutes-of timestamp)
                 (hour-of timestamp)
                 (day-of timestamp)
                 (month-of timestamp)
                 (year-of timestamp)))

(defgeneric timestamp->local-time (timestamp)
  (:documentation "Generic timestamp to local-time conversion."))

(defmethod timestamp->local-time ((timestamp walltime))
  (time->local-time timestamp))

(defmethod timestamp->local-time ((timestamp date))
  (date->local-time timestamp))

(defmethod timestamp->local-time ((timestamp zoned-date))
  (zoned-date->local-time timestamp))

(defmethod timestamp->local-time ((timestamp zoned-datetime))
  (zoned-datetime->local-time timestamp))

(defun local-time->date (timestamp)
  (make-date (local-time:timestamp-day timestamp)
             (local-time:timestamp-month timestamp)
             (local-time:timestamp-year timestamp)))

(defun local-time->walltime (timestamp)
  (make-time (local-time:timestamp-second timestamp)
             (local-time:timestamp-minute timestamp)
             (local-time:timestamp-hour timestamp)))

(defgeneric local-time->timestamp (local-time timestamp-class))

;; ** Formatting

(defparameter +time-format+
  '((:hour 2) #\: (:min 2) #\: (:sec 2)))

(defparameter +date-format+
  local-time:+iso-8601-date-format+)

(defparameter +datetime-format+
  (append +date-format+ (list #\T) +time-format+))

(defparameter +zoned-date-format+
  (append +date-format+ (list #\space :gmt-offset-or-z)))

(defparameter +zoned-datetime-format+
  (append +date-format+ (list #\T) +time-format+ (list :gmt-offset-hhmm)))

(defgeneric format-timestamp (destination timestamp &optional format &rest args)
  (:documentation "Format TIMESTAMP.
Destination can be T, then timestring is written to *STANDARD-OUTPUT*;
can be NIL, then a string is returned;
or can be a stream."))

(defmethod format-timestamp (destination (timestamp zoned-datetime) &optional (format +zoned-datetime-format+) &rest args)
  (declare (ignore args))
  (uiop:with-output (out destination)
    (local-time:format-timestring
     out (zoned-datetime->local-time timestamp)
     :format format
     :timezone (if (integerp (timezone-of timestamp))
                   (make-gmt-offset-timezone (timezone-of timestamp))
                   (timezone-of timestamp)))
    (unless (integerp (timezone-of timestamp))
      (write-char #\space out)
      (write-string (local-time::timezone-name (timezone-of timestamp))
                    out))))

(defmethod format-timestamp (destination (timestamp date) &optional (format +date-format+) &rest args)
  (declare (ignore args))
  (local-time:format-timestring
   destination
   (date->local-time timestamp)
   :format format
   :timezone local-time:+utc-zone+))

(defmethod format-timestamp (destination (timestamp zoned-date) &optional (format +zoned-date-format+) &rest args)
  (declare (ignore args))
  (local-time:format-timestring
   destination
   (date->local-time timestamp)
   ;;:timezone (timezone-of timestamp)
   :format format))

;; (defparameter +zoned-date-format+ "%F %z")

;; (defmethod format-timestamp (destination (timestamp zoned-date) &rest args)
;;   (cl-strftime:format-time
;;    destination
;;    +zoned-date-format+
;;    (timestamp->universal-time timestamp)
;;    (etypecase (timezone-of timestamp)
;;      (local-time::timezone
;;       (timezone-of timestamp))
;;      (integer (local-time::%make-simple-timezone "offset" "OFFSET" (timezone-of timestamp)))
;;      )))

(defmethod format-timestamp (destination (timestamp walltime) &optional (format +time-format+) &rest args)
  (declare (ignore args))
  (local-time:format-timestring
   destination
   (time->local-time timestamp)
   :format format
   :timezone local-time:+utc-zone+))

(defmethod format-timestamp (destination (timestamp datetime) &optional (format +datetime-format+) &rest args)
  (declare (ignore args))
  (local-time:format-timestring
   destination
   (datetime->local-time timestamp)
   :format format))

(defmethod print-object ((timestamp timestamp) stream)
  (print-unreadable-object (timestamp stream :type t)
    (format-timestamp stream timestamp)))

;; ** Calculations

(defgeneric timestamp+ (timestamp amount unit &rest more))

(defmethod timestamp+ ((timestamp timestamp) amount unit &rest more)
  (let* ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit))
         (new-timestamp (local-time->timestamp lt (class-of timestamp))))
    (if more
        (apply #'timestamp+ new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(defmethod timestamp+ ((timestamp date) amount unit &rest more)
  (let* ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit))
         (new-timestamp
           (make-date (local-time:timestamp-day lt)
                      (local-time:timestamp-month lt)
                      (local-time:timestamp-year lt))))
    (if more
        (apply #'timestamp+ new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(defmethod timestamp+ ((timestamp zoned-datetime) amount unit &rest more)
  (let* ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit
                                    (timezone-of timestamp)))
         (new-timestamp
           (make-zoned-datetime
            (local-time:timestamp-second lt :timezone (timezone-of timestamp))
            (local-time:timestamp-minute lt :timezone (timezone-of timestamp))
            (local-time:timestamp-hour lt :timezone (timezone-of timestamp))
            (local-time:timestamp-day lt :timezone (timezone-of timestamp))
            (local-time:timestamp-month lt :timezone (timezone-of timestamp))
            (local-time:timestamp-year lt :timezone (timezone-of timestamp))
            (timezone-of timestamp))))
    (if more
        (apply #'timestamp+ new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

#+test
(let ((day (make-zoned-datetime 0 0 0 1 1 2024)))
  (timestamp+ day 1 :day 2 :year))

;; Use apply for a period language
#+test
(let ((date (make-zoned-datetime 0 0 0 1 1 2024))
      (period '(1 :year 2 :month)))
  (apply #'timestamp+ date period))

(defgeneric timestamp- (timestamp amount unit &rest more)
  (:documentation "Return a new timestamp from TIMESTAMP reduced in AMOUNT UNITs.
Example:
(timestamp- (now) 2 :day)"))

(defmethod timestamp- ((timestamp timestamp) amount unit &rest more)
  (let* ((lt (local-time:timestamp- (timestamp->local-time timestamp) amount unit))
         (new-timestamp (local-time->timestamp lt (class-of timestamp))))
    (if more
        (apply #'timestamp- new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(defmethod timestamp- ((timestamp date) amount unit &rest more)
  (let* ((lt (local-time:timestamp- (timestamp->local-time timestamp) amount unit))
         (new-timestamp
           (make-date (local-time:timestamp-day lt)
                      (local-time:timestamp-month lt)
                      (local-time:timestamp-year lt))))
    (if more
        (apply #'timestamp- new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(defmethod timestamp- ((timestamp zoned-datetime) amount unit &rest more)
  (let* ((lt (local-time:timestamp- (timestamp->local-time timestamp) amount unit
                                    (timezone-of timestamp)))
         (new-timestamp
           (make-zoned-datetime
            (local-time:timestamp-second lt :timezone (timezone-of timestamp))
            (local-time:timestamp-minute lt :timezone (timezone-of timestamp))
            (local-time:timestamp-hour lt :timezone (timezone-of timestamp))
            (local-time:timestamp-day lt :timezone (timezone-of timestamp))
            (local-time:timestamp-month lt :timezone (timezone-of timestamp))
            (local-time:timestamp-year lt :timezone (timezone-of timestamp))
            (timezone-of timestamp))))
    (if more
        (apply #'timestamp- new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(declaim (ftype (function (timestamp &optional (member :number :name))
                          (or integer keyword))
                day-of-week))
(defun day-of-week (timestamp &optional (format :number))
  "Return day of week of TIMESTAMP.
FORMAT can be either :NUMBER (default) or :NAME."
  (let ((day-of-week (local-time:timestamp-day-of-week (timestamp->local-time timestamp))))
    (case format
      (:number day-of-week)
      (:name (aref +day-names+ day-of-week)))))

#+test
(let ((day (make-zoned-datetime 0 0 0 1 1 2024)))
  (timestamp- day 1 :day 2 :year))

;; Use apply for a period language
#+test
(let ((date (make-zoned-datetime 0 0 0 1 1 2024))
      (period '(1 :year 2 :month)))
  (apply #'timestamp- date period))

;; Naive units conversions. How to improve?
(defgeneric convert-units (value from-unit to-unit))
(defmethod convert-units (value (from-unit (eql :seconds))
                          (to-unit (eql :minutes)))
  (/ value +seconds-per-minute+))
(defmethod convert-units (value (from-unit (eql :minutes)) (to-unit (eql :hours)))
  (/ value +minutes-per-hour+))
(defmethod convert-units (value (from-unit (eql :seconds)) (to-unit (eql :hours)))
  (/ value +seconds-per-hour+))

;; (convert-units 60 :seconds :minutes)
;; (convert-units 7200 :seconds :hours)

(defmethod convert-units (value (from-unit (eql :minutes))
                          (to-unit (eql :seconds)))
  (* value +seconds-per-minute+))

(defmethod convert-units (value (from-unit (eql :hours))
                          (to-unit (eql :minutes)))
  (* value +minutes-per-hour+))

(defmethod convert-units (value (from-unit (eql :hours))
                          (to-unit (eql :seconds)))
  (* value +seconds-per-hour+))

;; (convert-units 2 :hours :minutes)

(defgeneric timestamp-difference (t1 t2 &optional unit)
  (:documentation "Difference between timestamps, in UNITs."))

(defmethod timestamp-difference (t1 t2 &optional unit)
  (let ((seconds (local-time:timestamp-difference
                  (timestamp->local-time t1)
                  (timestamp->local-time t2))))
    (if unit
        (convert-units seconds :seconds unit)
        seconds)))

;; ** Utilities

(defun time-now (&optional timezone)
  "The WALLTIME now."
  (let ((now (local-time:now)))
    (if timezone
        (let ((timestamp-values
                (coerce
                 (multiple-value-list
                  (local-time:decode-timestamp
                   now
                   :timezone (if (integerp timezone)
                                 local-time:+utc-zone+
                                 (ensure-timezone timezone))
                   :offset (when (integerp timezone) timezone)))
                 'vector)))
          (make-time (aref timestamp-values 1)
                     (aref timestamp-values 2)
                     (aref timestamp-values 3)))
        ;; else
        (make-time (local-time:timestamp-second now)
                   (local-time:timestamp-minute now)
                   (local-time:timestamp-hour now)))))

(defun now (&optional timezone)
  "The ZONED-DATETIME now."
  (let ((now (local-time:now)))
    (if timezone
        (let ((timestamp-values
                (coerce
                 (multiple-value-list
                  (local-time:decode-timestamp
                   now
                   :timezone (if (integerp timezone)
                                 local-time:+utc-zone+
                                 (ensure-timezone timezone))
                   :offset (when (integerp timezone) timezone)))
                 'vector)))
          (make-zoned-datetime (aref timestamp-values 1)
                               (aref timestamp-values 2)
                               (aref timestamp-values 3)
                               (aref timestamp-values 4)
                               (aref timestamp-values 5)
                               (aref timestamp-values 6)
                               (if (integerp timezone)
                                   timezone
                                   (ensure-timezone timezone))))
        ;; else
        (make-zoned-datetime
         (local-time:timestamp-second now)
         (local-time:timestamp-minute now)
         (local-time:timestamp-hour now)
         (local-time:timestamp-day now)
         (local-time:timestamp-month now)
         (local-time:timestamp-year now)
         local-time:*default-timezone*))))

(defun today (&optional timezone)
  "Returns DATE today."
  (let ((now (local-time:now)))
    (if timezone
        (let ((timestamp-values
                (coerce
                 (multiple-value-list
                  (local-time:decode-timestamp
                   now
                   :timezone (if (integerp timezone)
                                 local-time:+utc-zone+
                                 (ensure-timezone timezone))
                   :offset (when (integerp timezone) timezone)))
                 'vector)))
          (make-date (aref timestamp-values 4)
                     (aref timestamp-values 5)
                     (aref timestamp-values 6)))
        ;; else
        (make-date (local-time:timestamp-day now)
                   (local-time:timestamp-month now)
                   (local-time:timestamp-year now)))))

;; https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defgeneric clone-timestamp (timestamp &rest args))
(defmethod clone-timestamp ((timestamp timestamp) &rest args)
  (apply #'copy-instance timestamp args))

#+test
(let* ((d1 (make-date 2024 10 10))
       (d2 (clone-timestamp d1 :year 2023)))
  (list d1 d2))

#+test
(let* ((d1 (make-instance 'zoned-datetime :year 2023 :timezone "America/Argentina/Buenos_Aires"))
       (d2 (clone-timestamp d1 :timezone "Europe/Stockholm")))
  (list d1 d2))

(defun timestamp-adjust (timestamp &rest changes)
  (let ((adjusted-timestamp (clone-timestamp timestamp)))
    (flet ((apply-change (change args)
             (ecase change
               (setf
                (setf (slot-value adjusted-timestamp (car args))
                      (cadr args))))))
      (dolist (change changes)
        (destructuring-bind (change-name &rest args) change
          (apply-change change-name args)))
      adjusted-timestamp)))

#+test
(let ((now (now)))
  (timestamp-adjust now
                    '(setf day 22)
                    '(setf hour 00)
                    ))

(defgeneric %timestamps-compose (t1 t2)
  (:method (t1 t2)
    (error "Can't compose ~s with ~s" t1 t2)))

(defmethod %timestamps-compose ((t1 date) (t2 walltime))
  (make-datetime (seconds-of t2)
                 (minutes-of t2)
                 (hour-of t2)
                 (day-of t1)
                 (month-of t1)
                 (year-of t1)))

(defmethod %timestamps-compose ((t1 walltime) (t2 date))
  (%timestamps-compose t2 t1))

(defmethod %timestamps-compose ((t1 datetime) (z local-time::timezone))
  (timestamp-coerce t1 'zoned-datetime z))

(defmethod %timestamps-compose ((t1 datetime) (t2 date))
  (%timestamps-compose t2 (datetime-time t1)))

(defmethod %timestamps-compose ((t1 datetime) (t2 walltime))
  (%timestamps-compose (datetime-date t1) t2))

(defun timestamps-compose (t1 t2 &rest more)
  "Compose timestamps.

For example, a date + a time = datetime; a date-time + timezone = zoned-datetime.."
  (%timestamps-compose t1 t2))

;; (timestamps-compose (today) (time-now))
;; (timestamps-compose (time-now) (today))
;; (timestamps-compose (timestamp-coerce (now) 'datetime) local-time:+utc-zone+)

(defgeneric timestamp-compare (t1 t2))

(defmethod timestamp-compare ((t1 timestamp) (t2 timestamp))
  (local-time::%timestamp-compare
   (timestamp->local-time t1)
   (timestamp->local-time t2)))

(defgeneric timestamp-equalp (t1 t2)
  (:documentation "Compare timestamps for equality.
This is a structural equality comparison. So, two timestamps that represent
the same point in time, but differ in one of its elements (for instance, its timezone), are considered different. Use TIMESTAMP= for equality for timestamps that
represent the same point in time."))

(defmethod timestamp-equalp ((t1 timestamp) (t2 timestamp))
  (equalp t1 t2))

(defmethod timestamp-equalp ((t1 walltime) (t2 walltime))
  (and (= (seconds-of t1) (seconds-of t2))
       (= (minutes-of t1) (minutes-of t2))
       (= (hour-of t1) (hour-of t2))))

(defmethod timestamp-equalp ((t1 date) (t2 date))
  (and (= (day-of t1) (day-of t2))
       (= (month-of t1) (month-of t2))
       (= (year-of t1) (year-of t2))))

(defmethod timestamp-equalp ((t1 datetime) (t2 datetime))
  (and (timestamp-equalp (datetime-date t1)
                         (datetime-date t2))
       (timestamp-equalp (datetime-time t1)
                         (datetime-time t2))))

(defmethod timestamp-equalp ((t1 zoned-datetime) (t2 zoned-datetime))
  (and (timestamp-equalp (datetime-time t1) (datetime-time t2))
       (timestamp-equalp (datetime-date t2) (datetime-date t2))
       (equalp (timezone-of t1) (timezone-of t2))))

(defun timestamp= (t1 t2)
  "Returns T when the timestamps represent the same point in time."
  (local-time:timestamp= (timestamp->local-time t1)
                         (timestamp->local-time t2)))

(defun timestamp> (t1 t2)
  (local-time:timestamp> (timestamp->local-time t1)
                         (timestamp->local-time t2)))

(defun timestamp>= (t1 t2)
  (local-time:timestamp>= (timestamp->local-time t1)
                          (timestamp->local-time t2)))

(defun timestamp< (t1 t2)
  (local-time:timestamp< (timestamp->local-time t1)
                         (timestamp->local-time t2)))

(defun timestamp<= (t1 t2)
  (local-time:timestamp<= (timestamp->local-time t1)
                          (timestamp->local-time t2)))

;; ** Parsing

(defun parse-date (string)
  (destructuring-bind (year month day &rest args)
      (local-time::%split-timestring string
                                     :allow-missing-date-part nil
                                     :allow-missing-time-part t
                                     :allow-missing-timezone-part t)
    (declare (ignore args))
    (make-date day month year)))

;; (parse-date "2014-10-10")
;; (parse-date "2014-10-11")

(defun parse-time (string)
  (destructuring-bind (year month day hour minute second &rest args)
      (local-time::%split-timestring string
                                     :allow-missing-date-part t
                                     :allow-missing-time-part nil
                                     :allow-missing-timezone-part t)
    (declare (ignore year month day args))
    (make-time second minute hour)))

;; (parse-time "03:24:34")

(defun parse-datetime (string)
  (destructuring-bind (year month day hour minute second &rest args)
      (local-time::%split-timestring string
                                     :allow-missing-date-part nil
                                     :allow-missing-time-part nil
                                     :allow-missing-timezone-part t)
    (declare (ignore args))
    (make-datetime second minute hour day month year)))

(defun parse-zoned-datetime (string)
  (error "TODO"))

(defgeneric parse-timestring (timestring class &rest args)
  (:documentation "Parse TIMESTRING and return an instance of CLASS.
CLASS should be the class name of one of the subclasses of TIMESTAMP."))

(defmethod parse-timestring ((timestring string) (class (eql 'date)) &rest args)
  (declare (ignore args))
  (parse-date timestring))

;; (parse-timestring "2014-10-10" 'date)

(defmethod parse-timestring ((timestring string) (class (eql 'walltime)) &rest args)
  (declare (ignore args))
  (parse-time timestring))

(defmethod parse-timestring ((timestring string) (class (eql 'time)) &rest args)
  (declare (ignore args))
  (parse-time timestring))

(defmethod parse-timestring ((timestring string) (class (eql 'datetime)) &rest args)
  (declare (ignore args))
  (parse-datetime timestring))
