(fiasco:define-test-package :calendar-times-tests
  (:use :calendar-times))

(in-package :calendar-times-tests)

;; (fiasco:all-tests) to run

(eval-when (:load-toplevel)
  (local-time:reread-timezone-repository))

;; https://github.com/dlowe-net/local-time/issues/67
;; play with hour between 1 and 2 and observe timezone

(deftest timezones-calc-test ()
  (let ((at-four (make-zoned-datetime 0 0 4 30 3 2014 "Europe/Stockholm"))
        (at-one (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (= (time-entity-difference at-four at-one) 7200))
    ;; observe the difference in daylight saving time (offset)
    (is (string= (format-time-entity nil at-four) "2014-03-30T04:00:00+0200 Europe/Stockholm"))
    (is (string= (format-time-entity nil at-one) "2014-03-30T01:00:00+0100 Europe/Stockholm")))

  (let ((ts (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (lt (local-time:encode-timestamp 0 0 0 1 1 1 2024 :timezone (local-time:find-timezone-by-location-name "America/Argentina/Buenos_Aires"))))
    (is (local-time:timestamp= lt (time-entity->local-time ts)))
    (is (local-time:timestamp=
         (time-entity->local-time (time-entity+ ts 1 :hour))
         (local-time:timestamp+ (time-entity->local-time ts) 1 :hour)))
    (is (= 2 (hour-of (time-entity+ ts 1 :hour)))))

  ;; https://github.com/dlowe-net/local-time/issues/67
  ;; play with hour between 1 and 2 and observe timezone
  (let ((ts (make-zoned-datetime 0 0 1 30 3 2014 "Europe/Stockholm")))
    (is (string= (format-time-entity nil (time-entity+ ts 30 :minute))
                 "2014-03-30T01:30:00+0100 Europe/Stockholm"))
    (is (string= (format-time-entity nil (time-entity+ ts 60 :minute))
                 "2014-03-30T03:00:00+0200 Europe/Stockholm"))))

(deftest equality-test ()
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (time-entity-difference ts1 ts2))))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (time-entity= ts1 ts2) "Not equal time-entity, but same point in time.")
    (is (not (time-entity-equalp ts1 ts2)) "Same point in time, but not equal"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires")))
    (is (time-entity= ts1 ts2) "Same point in time")
    (is (time-entity-equalp ts1 ts2) "Time-Entities are equal")))

(deftest timezones-test ()
  ;; These are time-entities with different timezone but same offset
  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (zerop (time-entity-difference ts1 ts2))) ;; => this is T
    (time-entity= ts1 ts2) ;; => what should this be?
    )

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "America/Montevideo")))
    (is (time-entity= ts1 ts2) "Equal time-entity. Different timezone name, but same offset"))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (= (time-entity-difference ts1 ts2 :hours) 4)))

  (let ((ts1 (make-zoned-datetime 0 0 1 1 1 2024 "America/Argentina/Buenos_Aires"))
        (ts2 (make-zoned-datetime 0 0 1 1 1 2024 "Europe/Stockholm")))
    (is (not (time-entity= ts1 ts2)))))

(deftest coercion-tests ()
  (let ((dt (make-datetime 1 2 3 4 5 2024)))
    (is (time-entity= (time-entity-coerce dt 'date)
                    (make-date 4 5 2024)))))

(deftest validation-tests ()
  (signals error (make-time 0 0 -1))
  (signals error (make-date 30 2 2024))
  (signals error (make-datetime 0 0 0 30 2 2024)))

(deftest formatting-tests ()
  (is (string= (format-time-entity nil (make-time 0 0 1))
               "01:00:00"))
  (is (string= (format-time-entity nil (make-date 1 1 2024))
               "2024-01-01"))
  (is (string= (format-time-entity nil (make-datetime 0 0 0 1 1 2024))
               "2024-01-01T00:00:00"))
  (is (string= (format-time-entity nil (make-zoned-datetime 0 0 0 1 1 2024 "Europe/Stockholm"))
               "2024-01-01T00:00:00+0100 Europe/Stockholm"))
  (is (string= (format-time-entity nil (make-zoned-datetime 0 0 0 1 1 2024 "America/Argentina/Buenos_Aires"))
               "2024-01-01T00:00:00-0300 America/Argentina/Buenos_Aires")))

(deftest parsing-tests ()
  (let ((time (parse-timestring "01:00:22" 'time)))
    (is (typep time 'walltime))
    (is (time-entity= time (make-time 22 00 01))))
  (let ((time-entities (list (make-time 0 1 2)
                          (make-date 1 2 2000)
                          (make-datetime 0 1 2 3 4 2005)
                          (now "America/Argentina/Buenos_Aires")
                          (now "Europe/Stockholm"))))
    (dolist (time-entity time-entities)
      (is (time-entity-equalp time-entity
                            (parse-timestring (format-time-entity nil time-entity)
                                              (class-name (class-of time-entity))))))))

(deftest decoding-tests ()
  (let ((date (today)))
    (is (time-entity-equalp (apply #'make-date (multiple-value-list (decode-time-entity date)))
                          date)))
  (let ((time (time-now)))
    (is (time-entity-equalp (apply #'make-time (multiple-value-list (decode-time-entity time)))
                          time)))
  (let ((datetime (time-entity-coerce (now) 'datetime)))
    (is (time-entity-equalp (apply #'make-datetime (multiple-value-list (decode-time-entity datetime)))
                          datetime)))

  (let ((zdatetime (now)))
    (is (time-entity-equalp (apply #'make-zoned-datetime (multiple-value-list (decode-time-entity zdatetime)))
                          zdatetime))))
