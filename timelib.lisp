(defpackage :timelib
  (:use :cl))

(in-package :timelib)

(defclass timestamp ()
  ())

(defclass walltime (timestamp)
  ((hour :initarg :hour
         :accessor hour-of
         :type integer
         :initform 0)
   (minutes :initarg :minutes
            :accessor minutes-of
            :type integer
            :initform 0)
   (seconds :initarg :seconds
            :accessor seconds-of
            :type integer
            :initform 0)))

(defclass date (timestamp)
  ((year :initarg :year
         :accessor year-of)
   (month :initarg :month
          :accessor month-of
          :initform 1)
   (day :initarg :day
        :accessor day-of
        :initform 1)))

(defclass local-datetime (date walltime)
  ())

(defclass zoned-timestamp ()
  ((timezone :initarg :timezone
             :accessor timezone-of
             :initform local-time:+utc-zone+
             :type local-time::timezone)))

(defmethod initialize-instance :around ((timestamp zoned-timestamp) &rest args)
  (if (member :timezone args)
      (let ((timezone (getf args :timezone)))
        (apply #'call-next-method timestamp
               (list* :timezone (if (typep timezone 'local-time::timezone)
                                    timezone
                                    (or (local-time:find-timezone-by-location-name timezone)
                                        (error "Invalid timezone: ~s" timezone)))
                      (alexandria:remove-from-plist args :timezone))))
      (call-next-method)))

(defmethod reinitialize-instance :around ((timestamp zoned-timestamp) &rest args)
  (if (member :timezone args)
      (let ((timezone (getf args :timezone)))
        (apply #'call-next-method timestamp
               (list* :timezone (if (typep timezone 'local-time::timezone)
                                    timezone
                                    (or (local-time:find-timezone-by-location-name timezone)
                                        (error "Invalid timezone: ~s" timezone)))
                      (alexandria:remove-from-plist args :timezone))))
      (call-next-method)))

(defclass zoned-datetime (local-datetime zoned-timestamp)
  ())

(defclass zoned-date (date zoned-timestamp)
  ())

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

(defun local-datetime->local-time (timestamp &optional (timezone local-time:*default-timezone*) offset)
  (check-type timestamp local-datetime)
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

(defmethod print-object ((timestamp timestamp) stream)
  (print-unreadable-object (timestamp stream :type t)
    (format-timestamp stream timestamp)))

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

(let ((day (make-instance 'date :day 1 :month 1 :year 2024)))
  (timestamp+ day 1 :day))

(let ((day (make-instance 'date :day 1 :month 1 :year 2024)))
  (timestamp+ day 1 :day 2 :year))

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

(let ((day (make-instance 'zoned-datetime :day 1 :month 1 :year 2024)))
  (timestamp+ day 1 :day 2 :year))

(defgeneric timestamp-difference (t1 t2))

(defmethod timestamp-difference (t1 t2)
  (local-time:timestamp-difference
   (timestamp->local-time t1)
   (timestamp->local-time t2)))

(defun today ()
  )

(defun now ()
  )

(defun yesterday ())

(defun tomorrow ())

(defun make-date (&optional year month day)
  (let ((today (local-time:today)))
    (make-instance 'date
                   :year (or year (local-time:timestamp-year today))
                   :month (or month (local-time:timestamp-month today))
                   :day (or day (local-time:timestamp-day today)))))

(make-date 2025 1 1)

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

(let* ((d1 (make-date 2024 10 10))
       (d2 (clone-timestamp d1 :year 2023)))
  (list d1 d2))

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

(timestamp-compare
 (make-instance 'zoned-datetime :year 2023 :timezone "America/Argentina/Buenos_Aires")
 (make-instance 'zoned-datetime :year 2023))

(defun parse-date (string)
  (destructuring-bind (year month day &rest args)
      (local-time::%split-timestring string
                                     :allow-missing-date-part nil
                                     :allow-missing-time-part t
                                     :allow-missing-timezone-part t)
    (make-instance 'date :year year
                         :month month
                         :day day)))

(parse-date "2014-10-10")
(parse-date "2014-10-11")

(defun parse-walltime (string)
  (local-time->walltime
   (local-time:parse-timestring
    string
    :allow-missing-date-part t
    :allow-missing-time-part nil
    :allow-missing-timezone-part t)))

(parse-walltime "03:24:34")

(defun parse-zoned-datetime (string)
  (local-time->zoned-datetime
   (local-time:parse-timestring string :allow-missing-date-part nil
                                       :allow-missing-time-part nil
                                       :allow-missing-timezone-part nil)))

(defgeneric parse-timestring (timestring class &rest args))

(defmethod parse-timestring ((timestring string) (class (eql 'date)) &rest args)
  (parse-date timestring))

(parse-timestring "2014-10-10" 'date)

(defmethod parse-timestring ((timestring string) (class (eql 'walltime)) &rest args)
  (parse-walltime timestring))

(defmethod parse-timestring ((timestring string) (class (eql 'time)) &rest args)
  (parse-walltime timestring))

(parse-timestring "01:00:22" 'time)

(let* ((d1 (make-date 2024 1 10))
       (d2 (parse-date (format-timestamp nil d1))))
  (list d1 d2 (timestamp-compare d1 d2)))

(let* ((d1 (make-date 2024 1 10))
       (d2 (parse-date (format-timestamp nil d1))))
  (list d1 d2 (timestamp= d1 d2)))

(parse-date (format-timestamp nil (make-date 2024 1 10)))
