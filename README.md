## activity-log.el

**activity-log.el is currently tested under GNU Emacs 25.1.1 on OS X.**

## Screenshot

![alt text](https://github.com/sri/activity-log/raw/master/activity-log-1.png "Activity Log Screenshot")

## Usage

Invoke `activity-log-insert` to insert a template for the next week. However, if a valid date (see below in the TODOs for date detection) is detected in the current buffer, it insert the template for the week after that.

With a prefix argument, it inserts the template for the next `N` weeks.

Invoke `activity-log-goto-today` like so to open and place the cursor on the current date:

```
(activity-log-goto-today
  ;; find-file opens the file in a new buffer
  ;; (if one doesn't exist) and switches current
  ;; window to that buffer.
  (find-file "~/Desktop/work-notes/notes.org"))
```

## Features

* Generate

## TODOs

* Move user customization from `defvar` to `defcustom`.
* Better date detection in `activity-log-latest-in-current-buffer`. Currently, any org header (a line that starts with '*') with a timestamp (starts with `<` and ends with `>`) is considered a valid date.
*
