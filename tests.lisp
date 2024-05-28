(fiasco:define-test-package :timelib-tests
  (:use :timelib))

(in-package :timelib-tests)

;; (fiasco:all-tests) to run

(eval-when (:load-toplevel)
  (local-time:reread-timezone-repository))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone

(deftest timezones-calc-test ()
  (let ((at-four (make-zoned-datetime 0 0 4 30 3 2014 "Europe/Stockholm"))
        (at-one (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (= (timestamp-difference at-four at-one) 7200))
    ;; observe the difference in daylight saving time (offset)
    (is (string= (format-timestamp nil at-four) "2014-03-30T04:00:00+0200 Europe/Stockholm"))
    (is (string= (format-timestamp nil at-one) "2014-03-30T01:00:00+0100 Europe/Stockholm")))

  (let ((ts (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
    (is (local-time:timestamp= lt (timestamp->local-time ts)))
    (is (local-time:timestamp=
         (timestamp->local-time (timestamp+ ts 1 :hour))
         (local-time:timestamp+ (timestamp->local-time ts) 1 :hour)))
    (is (= 2 (hour-of (timestamp+ ts 1 :hour)))))

  ;; https://github.com/dlowe-net/local-time/issues/67
  ;; play with hour between 1 and 2 and observe timezone
  (let ((ts (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (string= (format-timestamp nil (timestamp+ ts 30 :minute))
                 "2014-03-30T01:30:00+0100 Europe/Stockholm"))
    (is (string= (format-timestamp nil (timestamp+ ts 60 :minute))
                 "2014-03-30T03:00:00+0200 Europe/Stockholm"))))

(deftest equality-test ()
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (timestamp-difference ts1 ts2))))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (timestamp= ts1 ts2) "Not equal timestamp, but same point in time.")
    (is (not (timestamp-equalp ts1 ts2)) "Same point in time, but not equal"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires")))
    (is (timestamp= ts1 ts2) "Same point in time")
    (is (timestamp-equalp ts1 ts2) "Timestamps are equal")))

(deftest timezones-test ()
  ;; These are timestamps with different timezone but same offset
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (timestamp-difference ts1 ts2))) ;; => this is T
    (timestamp= ts1 ts2) ;; => what should this be?
    )

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (timestamp= ts1 ts2) "Equal timestamp. Different timezone name, but same offset"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (= (timestamp-difference ts1 ts2 :hours) 4)))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (not (timestamp= ts1 ts2)))))

(deftest coercion-tests ()
  (let ((dt (make-datetime 1 2 3 4 5 2024)))
    (is (timestamp= (timestamp-coerce dt 'date)
                    (make-date 4 5 2024)))))

(deftest validation-tests ()
  (signals error (make-time 0 0 -1))
  (signals error (make-date 30 2 2024))
  (signals error (make-datetime 0 0 0 30 2 2024)))

(deftest formatting-tests ()
  (is (string= (format-timestamp nil (make-time 0 0 1))
               "01:00:00"))
  (is (string= (format-timestamp nil (make-date 1 1 2024))
               "2024-01-01"))
  (is (string= (format-timestamp nil (make-datetime 0 0 0 1 1 2024))
               "2024-01-01T00:00:00"))
  (is (string= (format-timestamp nil (make-zoned-datetime 0 0 0 1 1 2024 "Europe/Stockholm"))
               "2024-01-01T00:00:00+0100 Europe/Stockholm"))
  (is (string= (format-timestamp nil (make-zoned-datetime 0 0 0 1 1 2024 "America/Argentina/Buenos_Aires"))
               "2024-01-01T00:00:00-0300 America/Argentina/Buenos_Aires")))

(deftest parsing-tests ()
  (let ((time (parse-timestring "01:00:22" 'time)))
    (is (typep time 'walltime))
    (is (timestamp= time (make-time 22 00 01))))
  (let ((timestamps (list (make-time 0 1 2)
                          (make-date 1 2 2000)
                          (make-datetime 0 1 2 3 4 2005)
                          (now "America/Argentina/Buenos_Aires")
                          (now "Europe/Stockholm"))))
    (dolist (timestamp timestamps)
      (is (timestamp= timestamp
                      (parse-timestring (format-timestamp nil timestamp)
                                        (class-name (class-of timestamp))))))))

(deftest decoding-tests ()
  (let ((date (today)))
    (is (timestamp-equalp (apply #'make-date (multiple-value-list (decode-timestamp date)))
                          date)))
  (let ((time (time-now)))
    (is (timestamp-equalp (apply #'make-time (multiple-value-list (decode-timestamp time)))
                          time)))
  (let ((datetime (timestamp-coerce (now) 'datetime)))
    (is (timestamp-equalp (apply #'make-datetime (multiple-value-list (decode-timestamp datetime)))
                          datetime)))

  (let ((zdatetime (now)))
    (is (timestamp-equalp (apply #'make-zoned-datetime (multiple-value-list (decode-timestamp zdatetime)))
                          zdatetime))))
