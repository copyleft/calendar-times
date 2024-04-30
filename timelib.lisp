(defpackage :timelib
  (:use :cl)
  (:export #:timestamp
           #:walltime
           #:date
           #:datetime
           #:timestamp+
           #:parse-timestring)
  (:documentation "TIMELIB is a calendar time library implemented on top of LOCAL-TIME library.

It features zoned timestamps and calculations."))

(in-package :timelib)

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
             :type local-time::timezone))
  (:documentation "A timestamp with timezone. Abstract class."))

(defclass zoned-datetime (datetime zoned-timestamp)
  ()
  (:documentation "A datetime with a timezone."))

;; (defmethod initialize-instance :around ((timestamp zoned-timestamp) &rest args)
;;   (if (member :timezone args)
;;       (let ((timezone (getf args :timezone)))
;;         (apply #'call-next-method timestamp
;;                (list* :timezone (if (typep timezone 'local-time::timezone)
;;                                     timezone
;;                                     (or (local-time:find-timezone-by-location-name timezone)
;;                                         (error "Invalid timezone: ~s" timezone)))
;;                       (alexandria:remove-from-plist args :timezone))))
;;       (call-next-method)))

;; (defmethod reinitialize-instance :around ((timestamp zoned-timestamp) &rest args)
;;   (if (member :timezone args)
;;       (let ((timezone (getf args :timezone)))
;;         (apply #'call-next-method timestamp
;;                (list* :timezone (if (typep timezone 'local-time::timezone)
;;                                     timezone
;;                                     (or (local-time:find-timezone-by-location-name timezone)
;;                                         (error "Invalid timezone: ~s" timezone)))
;;                       (alexandria:remove-from-plist args :timezone))))
;;       (call-next-method)))


(defclass zoned-date (date zoned-timestamp)
  ())

;; * Constructors

(defun make-walltime (hour minutes seconds)
  (unless (local-time::valid-timestamp-p 0 seconds minutes hour 1 1 1970)
    (error "Invalid walltime: ~2,'0d:~2,'0d:~2,'0d" hour minutes seconds))
  (let ((walltime (make-instance 'walltime)))
    (setf (slot-value walltime 'hour) hour
          (slot-value walltime 'minutes) minutes
          (slot-value walltime 'seconds) seconds)
    walltime))

;;(make-walltime 24 0 0)
;;(make-walltime 1 33 4)

(defun make-date (day month year)
  (unless (local-time::valid-timestamp-p 0 0 0 0 day month year)
    (error "Invalid date: ~4,'0d-~2,'0d-~2,'0d" year month day))
  (let ((date (make-instance 'date)))
    (setf (slot-value date 'year) year
          (slot-value date 'month) month
          (slot-value date 'day) day)
    date))

;; (make-date 1 1 2024)
;; (make-date 30 2 2024)

(defun make-datetime (seconds minutes hour day month year)
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

(defun make-zoned-datetime (seconds minutes hour day month year &optional (timezone local-time:+utc-zone+))
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
          (slot-value datetime 'timezone) (etypecase timezone
                                            (local-time::timezone timezone)
                                            (string (local-time:find-timezone-by-location-name timezone))))
    datetime))

;; (make-zoned-datetime 0 0 0 1 1 2024)
;; (make-zoned-datetime 0 0 0 30 2 2024)
;; (make-zoned-datetime 0 0 0 1 1 2024 "America/Argentina/Buenos_Aires")

;; ** Conversions

(defun walltime->local-time (timestamp)
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

(defun datetime->universal (timestamp &optional timezone offset)
  (local-time:timestamp-to-universal
   (datetime->local-time timestamp timezone offset)))

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

(defun datetime->date (timestamp)
  (make-instance 'date
                 :day (day-of timestamp)
                 :month (month-of timestamp)
                 :year (year-of timestamp)))

(defgeneric timestamp-convert (timestamp class &rest args))

(defmethod timestamp-convert ((timestamp datetime) (class (eql 'date)) &rest args)
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
(defmethod timestamp->local-time ((timestamp zoned-datetime))
  (local-time:encode-timestamp
   0 (seconds-of timestamp)
   (minutes-of timestamp)
   (hour-of timestamp)
   (day-of timestamp)
   (month-of timestamp)
   (year-of timestamp)
   :timezone (timezone-of timestamp)))

(defun local-time->date (timestamp)
  (make-instance 'date
                 :year (local-time:timestamp-year timestamp)
                 :month (local-time:timestamp-month timestamp)
                 :day (local-time:timestamp-day timestamp)))

(defun local-time->walltime (timestamp)
  (make-instance 'walltime
                 :hour (local-time:timestamp-hour timestamp)
                 :minutes (local-time:timestamp-minute timestamp)
                 :seconds (local-time:timestamp-second timestamp)))

(defgeneric local-time->timestamp (local-time timestamp-class))

;; ** Formatting

(defgeneric format-timestamp (destination timestamp &rest args))

(defmethod format-timestamp (destination (timestamp zoned-datetime) &rest args)
  (local-time:format-timestring destination (zoned-datetime->local-time timestamp)
                                :timezone (timezone-of timestamp)))

(defmethod format-timestamp (destination (timestamp date) &rest args)
  (local-time:format-timestring
   destination
   (date->local-time timestamp)
   :format local-time:+iso-8601-date-format+
   :timezone local-time:+utc-zone+))

(defparameter +zoned-date-format+
  ;; 2008-11-18T02:32:00.586931+01:00
  (append local-time:+iso-8601-date-format+ (list #\T) (list :gmt-offset-or-z)))

(defmethod format-timestamp (destination (timestamp zoned-date) &rest args)
  (local-time:format-timestring
   destination
   (date->local-time timestamp)
   :timezone (timezone-of timestamp)
   :format +zoned-date-format+))

(defmethod format-timestamp (destination (timestamp walltime) &rest args)
  (local-time:format-timestring
   destination
   (walltime->local-time timestamp)
   :format local-time:+iso-8601-time-format+
   :timezone local-time:+utc-zone+))

(defparameter +iso-8601-datetime-format+
  ;; 2008-11-18T02:32:00.586931+01:00
  (append local-time::+iso-8601-date-format+ (list #\T) local-time::+iso-8601-time-format+))

(defmethod format-timestamp (destination (timestamp datetime) &rest args)
  (local-time:format-timestring
   destination
   (datetime->local-time timestamp)
   :format +iso-8601-datetime-format+))

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
           (make-instance 'date :day (local-time:timestamp-day lt)
                                :month (local-time:timestamp-month lt)
                                :year (local-time:timestamp-year lt))))
    (if more
        (apply #'timestamp+ new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

(defmethod timestamp+ ((timestamp zoned-datetime) amount unit &rest more)
  (let* ((lt (local-time:timestamp+ (timestamp->local-time timestamp) amount unit
                                    (timezone-of timestamp)))
         (new-timestamp
           (make-instance 'zoned-datetime
                          :seconds (local-time:timestamp-second lt :timezone (timezone-of timestamp))
                          :minutes (local-time:timestamp-minute lt :timezone (timezone-of timestamp))
                          :hour (local-time:timestamp-hour lt :timezone (timezone-of timestamp))
                          :day (local-time:timestamp-day lt :timezone (timezone-of timestamp))
                          :month (local-time:timestamp-month lt :timezone (timezone-of timestamp))
                          :year (local-time:timestamp-year lt :timezone (timezone-of timestamp))
                          :timezone (timezone-of timestamp))))
    (if more
        (apply #'timestamp+ new-timestamp (car more) (cadr more) (cddr more))
        new-timestamp)))

;; (let ((day (make-instance 'zoned-datetime :day 1 :month 1 :year 2024)))
;;   (timestamp+ day 1 :day 2 :year))

;; Use apply for a period language
;; (let ((date (make-instance 'zoned-datetime :day 1 :month 1 :year 2024))
;;       (period '(1 :year 2 :month)))
;;   (apply #'timestamp+ date period))

(defgeneric timestamp-difference (t1 t2))

(defmethod timestamp-difference (t1 t2)
  (local-time:timestamp-difference
   (timestamp->local-time t1)
   (timestamp->local-time t2)))

(defun today ()
  (let ((now (local-time:now)))
    (make-date (local-time:timestamp-day now)
               (local-time:timestamp-month now)
               (local-time:timestamp-year now))))


(defun now ()
  (let ((now (local-time:now)))
    (make-instance 'zoned-datetime
                   :seconds (local-time:timestamp-second now)
                   :minutes (local-time:timestamp-minute now)
                   :hour (local-time:timestamp-hour now)
                   :day (local-time:timestamp-day now)
                   :month (local-time:timestamp-month now)
                   :year (local-time:timestamp-year now)
                   :timezone local-time:*default-timezone*)))

(defun yesterday ())

(defun tomorrow ())

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

(defun adjust-timestamp (timestamp &rest spec))

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

(defgeneric timestamp-compare (t1 t2))

(defmethod timestamp-compare ((t1 timestamp) (t2 timestamp))
  (local-time::%timestamp-compare
   (timestamp->local-time t1)
   (timestamp->local-time t2)))

(defgeneric timestamp= (t1 t2))
(defmethod timestamp= ((t1 timestamp) (t2 timestamp))
  (local-time:timestamp=
   (timestamp->local-time t1)
   (timestamp->local-time t2)))

#+test(timestamp-compare
       (make-instance 'zoned-datetime :year 2023 :timezone "America/Argentina/Buenos_Aires")
       (make-instance 'zoned-datetime :year 2023))

;; ** Parsing

(defun parse-date (string)
  (destructuring-bind (year month day &rest args)
      (local-time::%split-timestring string
                                     :allow-missing-date-part nil
                                     :allow-missing-time-part t
                                     :allow-missing-timezone-part t)
    (make-instance 'date :year year
                         :month month
                         :day day)))

;; (parse-date "2014-10-10")
;; (parse-date "2014-10-11")

(defun parse-walltime (string)
  (local-time->walltime
   (local-time:parse-timestring
    string
    :allow-missing-date-part t
    :allow-missing-time-part nil
    :allow-missing-timezone-part t)))

;; (parse-walltime "03:24:34")

(defun parse-zoned-datetime (string)
  (local-time->zoned-datetime
   (local-time:parse-timestring string :allow-missing-date-part nil
                                       :allow-missing-time-part nil
                                       :allow-missing-timezone-part nil)))

(defgeneric parse-timestring (timestring class &rest args))

(defmethod parse-timestring ((timestring string) (class (eql 'date)) &rest args)
  (parse-date timestring))

;; (parse-timestring "2014-10-10" 'date)

(defmethod parse-timestring ((timestring string) (class (eql 'walltime)) &rest args)
  (parse-walltime timestring))

(defmethod parse-timestring ((timestring string) (class (eql 'time)) &rest args)
  (parse-walltime timestring))

;; (parse-timestring "01:00:22" 'time)

;; (let* ((d1 (make-date 2024 1 10))
;;        (d2 (parse-date (format-timestamp nil d1))))
;;   (list d1 d2 (timestamp-compare d1 d2)))

;; (let* ((d1 (make-date 2024 1 10))
;;        (d2 (parse-date (format-timestamp nil d1))))
;;   (list d1 d2 (timestamp= d1 d2)))

;; (parse-date (format-timestamp nil (make-date 2024 1 10)))
