(in-package :timelib)

(defclass timestamp ()
  ((internal-timestamp
    :accessor internal-timestamp
    :type local-time:timestamp)))

(defmethod initialize-instance :after ((timestamp timestamp) &key
                                                               seconds
                                                               minutes hour day month year)
  #+nil(setf (internal-timestamp timestamp)
             (local-time:encode-timestamp 0 seconds minutes hour day month year
                                          :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp date) &key day month year)
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-timestamp timestamp)
        (local-time:encode-timestamp 0 0 0 0 day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp walltime) &key seconds minutes hour)
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-timestamp timestamp)
        (local-time:encode-timestamp 0 seconds minutes hour
                                     1 1 1900
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp local-datetime) &key
                                                                    seconds
                                                                    minutes hour day month year)
  (setf (internal-timestamp timestamp)
        (local-time:encode-timestamp 0 seconds minutes hour day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod day-of ((timestamp date))
  (local-time:timestamp-day (internal-timestamp timestamp)
                            :timezone local-time:+utc-zone+))

(defmethod month-of ((timestamp date))
  (local-time:timestamp-month (internal-timestamp timestamp)
                              :timezone local-time:+utc-zone+))

(defmethod year-of ((timestamp date))
  (local-time:timestamp-year (internal-timestamp timestamp)
                             :timezone local-time:+utc-zone+))


(defun parse-date (string)
  (local-time->date
   (local-time:parse-timestring
    string
    :allow-missing-date-part nil
    :allow-missing-time-part t
    :allow-missing-timezone-part t)))

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
