(require 'ert)
(require 'subr-x) ;for string-join
(require 'activity-log)

(ert-deftest activity-log-starting-dow-test ()
  (let ((activity-log-week-starts-on 'wednesday))
    (should (= (activity-log-starting-dow) 3))))

(ert-deftest activity-log-starting-dow-invalid-dow-test ()
  (let ((activity-log-week-starts-on 'tuuuesday))
    ;; If misspelled or can't find specified week start,
    ;; default to Monday.
    (should (= (activity-log-starting-dow) 1))))

(ert-deftest activity-log-ending-dow-test ()
  (let ((activity-log-week-starts-on 'sunday))
    (should (= (activity-log-ending-dow) 6))))

(ert-deftest activity-log-week-start-test ()
  (let* ((activity-log-week-starts-on 'thursday)
         (start (activity-log-week-start
                 (date-to-time "2016-12-07 00:00:00"))))
    (should (string= "2016-12-01"
                     (format-time-string "%Y-%m-%d" start)))))

(ert-deftest activity-log-week-end-test ()
  (let* ((activity-log-week-starts-on 'wednesday)
         (start (activity-log-week-start
                 (date-to-time "2016-12-07 00:00:00"))))
    (should (string= "2016-12-07"
                     (format-time-string "%Y-%m-%d" start)))))

(ert-deftest activity-log-week-of-test ()
  (let* ((activity-log-week-starts-on 'tuesday)
         (week (activity-log-week-of (date-to-time "2017-01-01 00:00:00"))))
    (should (equal '("2016-12-27" "2016-12-28" "2016-12-29"
                     "2016-12-30" "2016-12-31" "2017-01-01"
                     "2017-01-02")
                   (mapcar (lambda (day)
                             (format-time-string "%Y-%m-%d" day))
                           week)))))

(ert-deftest activity-log-integation-test ()
  (let* ((activity-log-week-starts-on 'saturday)
         (activity-log-exclude-days '(sunday friday))
         (activity-log-day-header "<%Y-%m-%d %a>")
         (activity-log-week-header "<%Y-%m-%d (week %U)>")
         (activity-log-week-header-level "*")
         (day (date-to-time "2017-01-06 00:00:00"))
         (week
          '("* <2016-12-31 (week 52)>"
            "** <2016-12-31 Sat>"
            "** <2017-01-02 Mon>"
            "** <2017-01-03 Tue>"
            "** <2017-01-04 Wed>"
            "** <2017-01-05 Thu>"))
         (week+1
          '("* <2017-01-07 (week 01)>"
            "** <2017-01-07 Sat>"
            "** <2017-01-09 Mon>"
            "** <2017-01-10 Tue>"
            "** <2017-01-11 Wed>"
            "** <2017-01-12 Thu>"
            "")))

    ;; Test activity-log-insert-for-week
    (should (string= (with-temp-buffer
                       (activity-log-insert-for-week day)
                       (buffer-string))
                      (concat (string-join week "\n") "\n")))

    ;; Test activity-log-latest-in-current-buffer
    (should (string= (with-temp-buffer
                       (activity-log-insert-for-week day)
                       (format-time-string
                        "%Y-%m-%d"
                        (activity-log-latest-in-current-buffer)))
                     "2017-01-05"))

    ;; Test activity-log-insert
    (should (string= (with-temp-buffer
                       (activity-log-insert-for-week day)
                       (activity-log-insert 1)
                       (buffer-string))
                     (string-join (append week week+1) "\n")))))
