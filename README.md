## activity-log.el

**activity-log.el is currently tested under GNU Emacs 25.1.1 on OS X.**

## Screenshot

![alt text](https://github.com/sri/activity-log/raw/master/activity-log-1.png "Activity Log Screenshot")

## Usage

Invoke `activity-log-insert` to insert a week template for the next week. If a week has already been entered in the current buffer, then it insert the template for the week after that (see "Better date detection" in the TODOs section). With a prefix argument, it inserts the template for the next `N` weeks.

Invoke `activity-log-goto-today` like so to open and place the cursor on the current date (I do this in my ~/.emacs to see what I need to do for that day):

```
(activity-log-goto-today
  ;; find-file opens the file in a new buffer
  ;; (if one doesn't exist) and switches current
  ;; window to that buffer.
  (find-file "~/Desktop/work-notes/notes.org"))
```

### User customizable variables

* `activity-log-week-starts-on` defaults to the symbol `monday`. Select one of: `sunday`, `monday`, `tuesday`, `wednesday`, `thursday`, `friday`, or `saturday`.
* `activity-log-exclude-days` defaults to `'(saturday sunday)`. Add days which shouldn't show up when inserting the week's template into the current buffer.
* `activity-log-day-header` defaults to `"<%Y-%m-%d %a - day %j>"`. This is how the day will show up. See screenshot. **Note: this or the week header should be surrounded by `<` and `>` for date detection to work.**
* `activity-log-week-header` defaults to `"%Y-%m-%d (week %U)"`. This is how the week header will be formatted. See screenshot.
* `activity-log-week-header-level` defaults to `"**"`. This and a space is added before inserting the week. Day header will also add an extra `*`.


## TODOs

* Move user customization from `defvar` to `defcustom`.
* Better date detection in `activity-log-latest-in-current-buffer`. Currently, any org header (a line that starts with '*') with a timestamp (starts with `<` and ends with `>`) is considered a valid date.
*
