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
    (is (= (timestamp-difference at-four at-one) 7200)))

  (let ((ts (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
    (is (local-time:timestamp= lt (timestamp->local-time ts)))
    (is (local-time:timestamp=
         (timestamp->local-time (timestamp+ ts 1 :hour))
         (local-time:timestamp+ (timestamp->local-time ts) 1 :hour)))
    (is (= 2 (hour-of (timestamp+ ts 1 :hour))))))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone
#+nil(let ((ts (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
       (timestamp+ ts 60 :minute))

(deftest equality-test ()
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (timestamp-difference ts1 ts2))))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (timestamp= ts1 ts2) "Equal timestamp. Different timezone name, but same offset"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires")))
    (is (timestamp= ts1 ts2) "Equal timestamp")))

(deftest timezones-test ()
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (timestamp-difference ts1 ts2))))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (timestamp= ts1 ts2) "Equal timestamp. Different timezone name, but same offset"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (= (timestamp-difference ts1 ts2 :hours) 4)))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (not (timestamp= ts1 ts2)))))

(deftest conversion-tests ()
  (let ((dt (make-datetime 1 2 3 4 5 2024)))
    (is (timestamp= (timestamp-convert dt 'date)
                    (make-date 4 5 2024)))))

(deftest validation-tests ()
  (signals error (make-walltime 0 0 -1))
  (signals error (make-date 30 2 2024))
  (signals error (make-datetime 0 0 0 30 2 2024)))

(deftest formatting-tests ()
  (is (string= (format-timestamp nil (make-walltime 0 0 1))
                 "01:00:00"))
  (is (string= (format-timestamp nil (make-date 1 1 2024))
               "2024-01-01"))
  (is (string= (format-timestamp nil (make-datetime 0 0 0 1 1 2024))
               "2024-01-01T00:00:00"))
  (is (string= (format-timestamp nil (make-zoned-datetime 0 0 0 1 1 2024 "Europe/Stockholm"))
               "2024-01-01T00:00:00 Europe/Stockholm"))
  (is (string= (format-timestamp nil (make-zoned-datetime 0 0 0 1 1 2024 "America/Argentina/Buenos_Aires"))
               "2024-01-01T00:00:00 America/Argentina/Buenos_Aires")))

(deftest parsing-tests ()
  (let ((time (parse-timestring "01:00:22" 'time)))
    (is (typep time 'walltime))
    (is (timestamp= time (make-walltime 22 00 01))))
  (let ((timestamps (list (make-walltime 0 1 2)
                          (make-date 1 2 2000)
                          (make-datetime 0 1 2 3 4 2005))))
    (dolist (timestamp timestamps)
      (is (timestamp= timestamp
                      (parse-timestring (format-timestamp nil timestamp)
                                        (class-name (class-of timestamp))))))))
