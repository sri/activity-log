;;; activity-log.el --- Provides an activity log for org mode

;; Copyright (C) 2017 by Sriram Thaiyar

;; Author: Sriram Thaiyar <sriram.thaiyar@gmail.com>
;; Version: 0.3
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

;; Here is an example of an activity log template:
;;
;; ** 2016-12-12 (week 50)
;; *** <2016-12-12 Mon - day 347>
;; **** Stuff I did on Monday
;; *** <2016-12-13 Tue - day 348>
;; **** Stuff I did on Tuesday
;; *** <2016-12-14 Wed - day 349>
;; *** <2016-12-16 Fri - day 351>
;; *** <2016-12-17 Sat - day 352>
;; *** <2016-12-18 Sun - day 353>
;;
;; This library provides a couple of helper function to generate such
;; a log:
;;
;; 1) `activity-log-insert' inserts a template like the above into the
;;    current buffer. If such a template already exists in the current
;;    buffer, then it'll insert the template for the week after that.
;;
;; 2) `activity-log-goto-today' puts the cursor on header for today.
;;    So something like this is useful to add to your `~/.emacs':
;;
;;      (activity-log-goto-today (find-file "~/Desktop/notes.org"))
;;
;; 3) If you use a ticketing system (like JIRA), then you can use
;;    dynamic links -- text that automatically turn into links. If you
;;    have a ticket called `JS-223' and you provide your company's
;;    JIRA URL like so:
;;
;;      (setq activity-log-ticket-url "https://my-company.example.com/%s")
;;
;;    Then clicking on `JS-223' will take you to
;;    `https://my-company.example.com/JS-233'. See also
;;    `activity-log-dynamic-links-matcher'.

;;; Code:

(require 'org)
(require 'time-date)

;;
;; User Customization:

(defgroup activity-log nil
  "Tools to help with an activity log."
  :link '(url-link "https://github.com/sri/activity-log")
  :group 'convenience)

;; Ticket matching

(defface activity-log-dynamic-link-face
  '((t :foreground "#268bd2" :box 1 :weight bold :inherit unspecified))
  "Face for Activity Log dynamic links.")

;; See `org-activate-plain-links' and `org-set-font-lock-defaults'
;; for an example of how this is done in Org mode.
;; Another way to achieve this is with `goto-address-mode'.

(defcustom activity-log-ticket-url nil
  "This should contain the URL to take the user when
a ticket regex is clicked on.
See `activity-log-dynamic-links-matcher'."
  :group 'activity-log)

(defcustom activity-log-dynamic-links-matcher
  '(("\\([[:alpha:]]\\{2,5\\}[[:digit:]]?-[[:digit:]]+\\)" activity-log-ticket-url))

  "Matcher for dynamic links.
Each element must be a 2-element list of the format:

  (REGEX URL)

REGEX should match whichever word you want to convert to a link.
URL should be the URL to open when the link is clicked. If it
contains a \"%s\", then it will be replaced with the matched
word. If that isn't present, then the URL is visited. If it is a
symbol, then the symbol's value is used as the URL.

Dynamic link can only be clicked on by the mouse. Hitting <Enter>
on them does nothing.

The default regexp here will match a 'ticket' such as:

  JIRA-122
  CH-39399
  TN-292"
  :group 'activity-log)

(defcustom activity-log-week-starts-on 'monday
  "Day of week when the week starts."
  :type 'symbol
  :group 'activity-log)

(defcustom activity-log-exclude-days '(saturday sunday)
  "Days to exclude from the template."
  :type '()
  :group 'activity-log)

(defcustom activity-log-day-header "<%Y-%m-%d %a - day %j>"
  "Format of the day header."
  :group 'activity-log)

(defcustom activity-log-week-header "%Y-%m-%d (week %U)"
  "Format of the week header."
  :group 'activity-log)

(defcustom activity-log-week-header-level "**"
  "Org header level when inserting a week."
  :group 'activity-log)

(defcustom activity-log-top-header "ACTIVITY LOG"
  "Name of the top level header that contains all the activity logs."
  :group 'activity-log)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Internals:
;;

;; Dynamic links:
(defun activity-log-activate-dynamic-links (limit)
  (let ((matchers activity-log-dynamic-links-matcher)
        (result nil)
        (regex)
        (link-template))
    (while (and matchers
                (null result))

      (setq regex (caar matchers)
            link-template (cadar matchers)
            matchers (cdr matchers))

      (when (symbolp link-template)
        (setq link-template (symbol-value link-template)))

      ;; Below is mostly copied from `org-activate-plain-links'.
      (when (and (re-search-forward regex limit t)
                 (not (org-in-src-block-p)))
        (let ((face
               (get-text-property (max (1- (match-beginning 0)) (point-min))
                                  'face))
              (link
               (if (save-match-data (string-match "%s" link-template))
                   (format link-template (org-match-string-no-properties 0))
                 link-template)))
          (unless (if (consp face) (memq 'org-tag face) (eq 'org-tag face))
            (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'mouse-face 'highlight
                                       'face 'activity-log-dynamic-link-face
                                       'activity-log-link `(:uri ,link)
                                       'keymap org-mouse-map))
            (org-rear-nonsticky-at (match-end 0))
            (setq result t)))))
    result))


(add-hook 'org-open-at-point-functions
          (lambda ()
            (let ((link (get-text-property (point) 'activity-log-link)))
              (when (cadr link)
                (browse-url (cadr link))
                t))))

;; Puts our function into the `font-lock-defaults'.
(add-hook 'org-font-lock-set-keywords-hook
          (lambda ()
            (nconc org-font-lock-extra-keywords
                   (list '(activity-log-activate-dynamic-links (0 'org-link t))))))
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
    (when activity-log-week-header
      (insert activity-log-week-header-level
              " "
              (format-time-string activity-log-week-header (car week))
              "\n"))
    (dolist (day week)
      (let ((dow (nth 6 (decode-time day))))
        (unless (memq dow exclude)
          (insert (concat activity-log-week-header-level "*")
                  " "
                  (format-time-string activity-log-day-header day)
                  "\n"))))))

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
    (when (and activity-log-top-header
               (search-forward activity-log-top-header nil t))
      (outline-show-children))
    (when (search-forward week-heading nil t)
      (outline-show-children)
      (unless (called-interactively-p 'interactive)
	(sit-for 0.3))
      (recenter 0)
      (when (search-forward day-heading nil t)
        (outline-show-children)
        (end-of-line)))))

(provide 'activity-log)

;;; activity-log.el ends here
