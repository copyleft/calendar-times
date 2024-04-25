(fiasco:define-test-package :timelib-tests
  (:use :timelib))

(in-package :timelib-tests)

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
(let ((at-four (make-instance 'timelib::zoned-datetime
                              :seconds 0 :minutes 0
                              :hour 4 :day 30 :month 3 :year 2014
                              :timezone
                              (local-time:find-timezone-by-location-name "Europe/Stockholm")))
      (at-one (make-instance 'timelib::zoned-datetime
                             :seconds 0 :minutes 0
                             :hour 1 :day 30 :month 3 :year 2014
                             :timezone
                             (local-time:find-timezone-by-location-name "Europe/Stockholm"))))
  (is (= (timelib::timestamp-difference at-four at-one) 7200)))

(let ((ts
        (make-instance 'timelib::zoned-datetime
                       :day 1
                       :month 1
                       :year 2024
                       :hour 1
                       :minutes 0
                       :seconds 0
                       :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires")))
      (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
  (is (local-time:timestamp= lt
                             (timelib::timestamp->local-time ts)))
  (is (local-time:timestamp=
       (timelib::timestamp->local-time (timelib::timestamp+ ts 1 :hour))
       (local-time:timestamp+ (timelib::timestamp->local-time ts) 1 :hour)))
  (is (= 2 (timelib::hour-of (timelib::timestamp+ ts 1 :hour)))))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
(let ((ts (make-instance 'timelib::zoned-datetime
                         :day 30
                         :month 3
                         :year 2014
                         :hour 1
                         :minutes 0
                         :seconds 0
                         :timezone
                         (local-time:find-timezone-by-location-name "Europe/Stockholm"))))
  (timelib::timestamp+ ts 60 :minute))

(make-instance 'timelib::zoned-datetime
               :day 1
               :month 1
               :year 2024
               :hour 1
               :minutes 0
               :seconds 0
               :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))

(make-instance 'timelib::date
               :day 1
               :month 1
               :year 2024)

(make-instance 'timelib::walltime
               :hour 1
               :minutes 20
               :seconds 20)

(make-instance 'timelib::zoned-date
               :day 1
               :month 1
               :year 2024
               :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))

