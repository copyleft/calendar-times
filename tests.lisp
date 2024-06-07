(fiasco:define-test-package :calendar-times-tests
  (:use :calendar-times))

(in-package :calendar-times-tests)

;; (fiasco:all-tests) to run

(eval-when (:load-toplevel)
  (local-time:reread-timezone-repository))

;; test that LOCAL-TIME library timezone calculations are in good shape:
(deftest localtime-timezones-calc-test ()
  ;; https://github.com/dlowe-net/local-time/issues/67
  (let ((cet (local-time:find-timezone-by-location-name "Europe/Stockholm")))
    (let ((t1 (local-time:encode-timestamp 0 0 0 0 30 3 2014 :timezone cet))
          (t2 (local-time:encode-timestamp 0 0 0 4 30 3 2014 :timezone cet))
          (t3 (local-time:encode-timestamp 0 0 0 1 30 3 2014 :timezone cet)))
      (is (local-time:timestamp= t1
                                 (local-time:encode-timestamp 0 0 0 23 29 03 2014 :timezone local-time:+utc-zone+)) "CET is 1 hour ahead of UTC")
      (is
          (local-time:timestamp= t2
                                 (local-time:encode-timestamp 0 0 0 02 30 03 2014 :timezone local-time:+utc-zone+))
          "CET with daylight saving time is 2 hours head of UTC")
      (is (local-time:timestamp= t3
                                 (local-time:encode-timestamp 0 0 0 00 30 03 2014 :timezone local-time:+utc-zone+))
          "daylight saving time transititon starts at 2"))

    ;; https://github.com/dlowe-net/local-time/issues/47
    (let ((t1 (local-time:encode-timestamp 0 0 0 4 30 3 2014 :timezone cet))
          (t2 (local-time:encode-timestamp 0 0 0 1 30 3 2014 :timezone cet)))
      (is (= (local-time:timestamp-difference t1 t2) 7200) "2 hours difference because of DST"))))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
(deftest timezones-calc-test ()
  (let ((at-four (make-zoned-datetime 0 0 4 30 3 2014 "Europe/Stockholm"))
        (at-one (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (= (caltime-difference at-four at-one) 7200))
    ;; observe the difference in daylight saving time (offset)
    (is (string= (format-caltime nil at-four) "2014-03-30T04:00:00+0200 Europe/Stockholm"))
    (is (string= (format-caltime nil at-one) "2014-03-30T01:00:00+0100 Europe/Stockholm")))

  (let ((ts (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
    (is (local-time:timestamp= lt (caltime->local-time ts)))
    (is (local-time:timestamp=
         (caltime->local-time (caltime+ ts 1 :hour))
         (local-time:timestamp+ (caltime->local-time ts) 1 :hour)))
    (is (= 2 (hour-of (caltime+ ts 1 :hour)))))

  ;; https://github.com/dlowe-net/local-time/issues/67
  ;; play with hour between 1 and 2 and observe timezone
  (let ((ts (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (string= (format-caltime nil (caltime+ ts 30 :minute))
                 "2014-03-30T01:30:00+0100 Europe/Stockholm"))
    (is (string= (format-caltime nil (caltime+ ts 60 :minute))
                 "2014-03-30T03:00:00+0200 Europe/Stockholm"))))

(deftest equality-test ()
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (caltime-difference ts1 ts2))))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (caltime= ts1 ts2) "Not equal caltime, but same point in time.")
    (is (not (caltime-equalp ts1 ts2)) "Same point in time, but not equal"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires")))
    (is (caltime= ts1 ts2) "Same point in time")
    (is (caltime-equalp ts1 ts2) "Caltimes are equal")))

(deftest timezones-test ()
  ;; These are caltimes with different timezone but same offset
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (caltime-difference ts1 ts2))) ;; => this is T
    (caltime= ts1 ts2) ;; => what should this be?
    )

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (caltime= ts1 ts2) "Equal caltime. Different timezone name, but same offset"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (= (caltime-difference ts1 ts2 :hours) 4)))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (not (caltime= ts1 ts2)))))

(deftest coercion-tests ()
  (let ((dt (make-datetime 1 2 3 4 5 2024)))
    (is (caltime= (caltime-coerce dt 'date)
                  (make-date 4 5 2024)))))

(deftest validation-tests ()
  (signals error (make-time 0 0 -1))
  (signals error (make-date 30 2 2024))
  (signals error (make-datetime 0 0 0 30 2 2024)))

(deftest formatting-tests ()
  (is (string= (format-caltime nil (make-time 0 0 1))
               "01:00:00"))
  (is (string= (format-caltime nil (make-date 1 1 2024))
               "2024-01-01"))
  (is (string= (format-caltime nil (make-datetime 0 0 0 1 1 2024))
               "2024-01-01T00:00:00"))
  (is (string= (format-caltime nil (make-zoned-datetime 0 0 0 1 1 2024 "Europe/Stockholm"))
               "2024-01-01T00:00:00+0100 Europe/Stockholm"))
  (is (string= (format-caltime nil (make-zoned-datetime 0 0 0 1 1 2024 "America/Argentina/Buenos_Aires"))
               "2024-01-01T00:00:00-0300 America/Argentina/Buenos_Aires")))

(deftest parsing-tests ()
  (let ((time (parse-timestring "01:00:22" 'time)))
    (is (typep time 'walltime))
    (is (caltime= time (make-time 22 00 01))))
  (let ((caltimes (list (make-time 0 1 2)
                        (make-date 1 2 2000)
                        (make-datetime 0 1 2 3 4 2005)
                        (now "America/Argentina/Buenos_Aires")
                        (now "Europe/Stockholm"))))
    (dolist (caltime caltimes)
      (is (caltime-equalp caltime
                          (parse-timestring (format-caltime nil caltime)
                                            (class-name (class-of caltime))))))))

(deftest decoding-tests ()
  (let ((date (today)))
    (is (caltime-equalp (apply #'make-date (multiple-value-list (decode-caltime date)))
                        date)))
  (let ((time (time-now)))
    (is (caltime-equalp (apply #'make-time (multiple-value-list (decode-caltime time)))
                        time)))
  (let ((datetime (caltime-coerce (now) 'datetime)))
    (is (caltime-equalp (apply #'make-datetime (multiple-value-list (decode-caltime datetime)))
                        datetime)))

  (let ((zdatetime (now)))
    (is (caltime-equalp (apply #'make-zoned-datetime (multiple-value-list (decode-caltime zdatetime)))
                        zdatetime))))

(deftest compose-tests ()
  (let ((date (make-date 1 1 2024))
        (time (make-time 0 1 1)))
    (is (caltime-equalp
         (caltimes-compose date time)
         (make-datetime 0 1 1 1 1 2024)))
    (is (caltime-equalp
         (caltimes-compose time date)
         (make-datetime 0 1 1 1 1 2024))))
  (let ((datetime (make-datetime 0 0 0 1 1 2024)))
    (is (typep (caltimes-compose datetime local-time:+utc-zone+)
               'zoned-datetime))))
