(fiasco:define-test-package :timelib-tests
  (:use :timelib))

(in-package :timelib-tests)

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
(let ((at-four (make-zoned-datetime 0 0 4 30 3 2014 "Europe/Stockholm"))
      (at-one (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
  (is (= (timelib::timestamp-difference at-four at-one) 7200)))

(let ((ts (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
      (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
  (is (local-time:timestamp= lt (timelib::timestamp->local-time ts)))
  (is (local-time:timestamp=
       (timelib::timestamp->local-time (timelib::timestamp+ ts 1 :hour))
       (local-time:timestamp+ (timelib::timestamp->local-time ts) 1 :hour)))
  (is (= 2 (timelib::hour-of (timelib::timestamp+ ts 1 :hour)))))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
(let ((ts (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
  (timelib::timestamp+ ts 60 :minute))

(let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
      (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
  (timelib::timestamp-difference ts1 ts2))

(let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
      (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
  (timelib::timestamp= ts1 ts2))

(let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
      (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
  (timelib::timestamp-difference ts1 ts2 :hours))

(let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
      (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
  (timelib::timestamp= ts1 ts2))
