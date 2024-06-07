(in-package :calendar-times)

(defclass time-entity ()
  ((internal-time-entity
    :accessor internal-time-entity
    :type local-time:timestamp)))

(defmethod initialize-instance :after ((time-entity time-entity) &key
                                                               (seconds 0)
                                                               (minutes 0)
                                                               (hour 0)
                                                               (day 1)
                                                               (month 1)
                                                               (year (local-time:timestamp-year (local-time:now))))
  #+nil(setf (internal-time-entity time-entity)
             (local-time:encode-timestamp 0 seconds minutes hour day month year
                                          :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((time-entity date) &key (day 1) (month 1)
                                                          (year (local-time:timestamp-year (local-time:now))))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-time-entity time-entity)
        (local-time:encode-timestamp 0 0 0 0 day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((time-entity walltime) &key
                                                              (seconds 0)
                                                              (minutes 0)
                                                              (hour 0))
  #+nil(call-next-method :seconds 0 :minutes 0 :hour 0
                         :day day :month month :year year)
  (setf (internal-time-entity time-entity)
        (local-time:encode-timestamp 0 seconds minutes hour
                                     1 1 2000
                                     :timezone local-time:+utc-zone+))
  )

(defmethod initialize-instance :after ((time-entity local-datetime) &key
                                                                    (seconds 0)
                                                                    (minutes 0)
                                                                    (hour 0)
                                                                    (day 1)
                                                                    (month 1)
                                                                    year)
  (setf (internal-time-entity time-entity)
        (local-time:encode-timestamp 0 seconds minutes hour day month year
                                     :timezone local-time:+utc-zone+))
  )

(defmethod day-of ((time-entity date))
  (local-time:timestamp-day (internal-time-entity time-entity)
                            :timezone local-time:+utc-zone+))

(defmethod month-of ((time-entity date))
  (local-time:timestamp-month (internal-time-entity time-entity)
                              :timezone local-time:+utc-zone+))

(defmethod year-of ((time-entity date))
  (local-time:timestamp-year (internal-time-entity time-entity)
                             :timezone local-time:+utc-zone+))
