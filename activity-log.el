;;; activity-log.el --- Provides an activity log for org mode

;; Copyright (C) 2016 by Sriram Thaiyar

;; Author: Sriram Thaiyar <sriram.thaiyar@gmail.com>
;; Version: 0.1
;; Keywords: convenience
;; Homepage: https://github.com/sri/activity-log
;; Package-Requires: ((org "8.2") (emacs "25"))

;; This file is not part of GNU Emacs.



;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.md for details.

;;; Code:

(require 'org)
(require 'time-date)

;;
;; User Customization:

(defvar activity-log-week-starts-on 'monday)
(defvar activity-log-exclude-days '(saturday sunday))
(defvar activity-log-day-header "<%Y-%m-%d %a - day %j>")
(defvar activity-log-week-header "%Y-%m-%d (week %U)")
(defvar activity-log-week-header-level "**")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Internals:
;;

;;
;; Date-Time handling

(defvar activity-log-1day (days-to-time 1))

(defun activity-log-next-day (time)
  (time-add time activity-log-1day))

(defun activity-log-previous-day (time)
  (time-subtract time activity-log-1day))

(defvar activity-log-dow-alist
  '((sunday    . 0)
    (monday    . 1)
    (tuesday   . 2)
    (wednesday . 3)
    (thursday  . 4)
    (friday    . 5)
    (saturday  . 6)))

;; Week start and end

(defun activity-log-starting-dow ()
  (or (cdr (assq activity-log-week-starts-on activity-log-dow-alist))
      (cdr (assq 'monday activity-log-dow-alist))))

(defun activity-log-ending-dow ()
  (let ((start (activity-log-starting-dow)))
    (if (<= start 0)
        (cdar (last activity-log-dow-alist))
      (1- start))))

(defun activity-log-week-start (time)
  (let ((start (format "%s" (activity-log-starting-dow))))
    (while (not (string= (format-time-string "%w" time) start))
      (setq time (activity-log-previous-day time)))
    time))

(defun activity-log-week-end (time)
  (let ((end (format "%s" (activity-log-starting-dow))))
    (while (not (string= (format-time-string "%w" time) end))
      (setq time (activity-log-next-day time)))
    time))

(defun activity-log-next-week (time)
  (let ((end (activity-log-week-end time)))
    (activity-log-next-day end)))

;; Week generation

(defun activity-log-week-of (time)
  (let ((results '())
        (start (activity-log-week-start time)))
    (dotimes (i 7)
      (push start results)
      (setq start (activity-log-next-day start)))
    (nreverse results)))

(defun activity-log-insert-for-week (time)
  (let ((week (activity-log-week-of time))
        (exclude (mapcar (lambda (day)
                           (cdr (assq day activity-log-dow-alist)))
                         activity-log-exclude-days)))
    (insert activity-log-week-header-level
            " "
            (format-time-string activity-log-week-header (car week))
            "\n")
    (dolist (day week)
      (let ((dow (nth 6 (decode-time day))))
        (unless (memq dow exclude)
          (insert (concat activity-log-week-header-level "*")
                  " "
                  (format-time-string activity-log-day-header day)
                  "\n"))))))

;; Date dete

(defun activity-log-latest-in-current-buffer ()
  (save-excursion
    (goto-char (point-max))
    (when (re-search-backward "^[*]+ <\\([^>]+\\)>" nil t)
      ;; Date-to-time needs to have all the components present.
      (date-to-time (format "%s 00:00:00" (match-string 1))))))

(defun activity-log-insert (&optional arg)
  (interactive "p")
  (let* ((latest (activity-log-latest-in-current-buffer))
         (week (if (not latest)
                   (activity-log-week-start (current-time))
                 (activity-log-next-week latest))))
    (save-excursion
      (goto-char (point-max))
      (dotimes (_ (or arg 1))
        (activity-log-insert-for-week week)
        (setq week (activity-log-next-week week))))))

(defun activity-log-goto-today (&optional buffer)
  (interactive)
  (let* ((now (current-time))
         (week-heading (format-time-string activity-log-week-header
                                           (activity-log-week-start now)))
        (day-heading (format-time-string activity-log-day-header
                                         now)))
    (goto-char (point-min))
    (when (search-forward week-heading nil t)
      (outline-show-branches)
      (unless (called-interactively-p 'interactive)
	(sit-for 0.3))
      (recenter 0)
      (when (search-forward day-heading nil t)
        (end-of-line)))))

(provide 'activity-log)

;;; activity-log.el ends here
