(in-package :timelib)

(defclass timestamp ()
  ((internal-timestamp
    :accessor internal-timestamp
    :type local-time:timestamp)))

(defmethod initialize-instance :after ((timestamp timestamp) &key
                                                               (seconds 0)
                                                               (minutes 0)
                                                               (hour 0)
                                                               (day 1)
                                                               (month 1)
                                                               (year (local-time:timestamp-year (local-time:now))))
  #+nil(setf (internal-timestamp timestamp)
             (local-time:encode-timestamp 0 seconds minutes hour day month year
                                          :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp date) &key (day 1) (month 1)
                                                          (year (local-time:timestamp-year (local-time:now))))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-timestamp timestamp)
        (local-time:encode-timestamp 0 0 0 0 day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp walltime) &key
                                                              (seconds 0)
                                                              (minutes 0)
                                                              (hour 0))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-timestamp timestamp)
        (local-time:encode-timestamp 0 seconds minutes hour
                                     1 1 1900
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((timestamp local-datetime) &key
                                                                    (seconds 0)
                                                                    (minutes 0)
                                                                    (hour 0)
                                                                    (day 1)
                                                                    (month 1)
                                                                    year)
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
