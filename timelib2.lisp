(in-package :calendar-times)

(defclass caltime ()
  ((internal-caltime
    :accessor internal-caltime
    :type local-time:timestamp)))

(defmethod initialize-instance :after ((caltime caltime) &key
                                                               (seconds 0)
                                                               (minutes 0)
                                                               (hour 0)
                                                               (day 1)
                                                               (month 1)
                                                               (year (local-time:timestamp-year (local-time:now))))
  #+nil(setf (internal-caltime caltime)
             (local-time:encode-timestamp 0 seconds minutes hour day month year
                                          :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((caltime date) &key (day 1) (month 1)
                                                          (year (local-time:timestamp-year (local-time:now))))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-caltime caltime)
        (local-time:encode-timestamp 0 0 0 0 day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((caltime walltime) &key
                                                              (seconds 0)
                                                              (minutes 0)
                                                              (hour 0))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-caltime caltime)
        (local-time:encode-timestamp 0 seconds minutes hour
                                     1 1 2000
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((caltime local-datetime) &key
                                                                    (seconds 0)
                                                                    (minutes 0)
                                                                    (hour 0)
                                                                    (day 1)
                                                                    (month 1)
                                                                    year)
  (setf (internal-caltime caltime)
        (local-time:encode-timestamp 0 seconds minutes hour day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod day-of ((caltime date))
  (local-time:timestamp-day (internal-caltime caltime)
                            :timezone local-time:+utc-zone+))

(defmethod month-of ((caltime date))
  (local-time:timestamp-month (internal-caltime caltime)
                              :timezone local-time:+utc-zone+))

(defmethod year-of ((caltime date))
  (local-time:timestamp-year (internal-caltime caltime)
                             :timezone local-time:+utc-zone+))
