# activity-log.el

**Currently tested under GNU Emacs 25.1.1 on OS X.**

I use this to keep track of what I've done, issues that I have solved, and everything else while at work. Originally, this was a part of my `~/.emacs` but it seemed ripe for extraction into its own library.

## So what does it do?
1. It inserts a template for the upcoming week so that I don't have to do it manually.
2. And when Emacs starts up, I open the log file and place the cursor on today's header.

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

* `activity-log-week-starts-on` defaults to the `'monday`. It must be one of: `sunday`, `monday`, `tuesday`, `wednesday`, `thursday`, `friday`, or `saturday`.
* `activity-log-exclude-days` defaults to `'(saturday sunday)`. Days added to this list won't be output when you invoke `activity-log-insert`.
* `activity-log-day-header` defaults to `"<%Y-%m-%d %a - day %j>"`. This is how the day will show up. See screenshot. **Note: this or the week header should be surrounded by `<` and `>` for date detection to work.**
* `activity-log-week-header` defaults to `"%Y-%m-%d (week %U)"`. This is how the week header will be formatted. See screenshot. This can be set to `nil` in which case the week header is not rendered.
* `activity-log-week-header-level` defaults to `"**"`. This and a space is added before inserting the week. Day header will also add an extra `*`.

Here is a way to have days listed in your activity log with only daily headers:

```
(setq activity-log-week-header-level "")
(setq activity-log-week-header nil)
(setq activity-log-day-header "<%Y-%m-%d %a>")
```

gives you toplevel days

```
* <2019-01-22 Tue>
* <2019-01-23 Wed>
* <2019-01-24 Thu>
* <2019-01-25 Fri>
```

## TODOs

* Move user customization from `defvar` to `defcustom`.
* Better date detection in `activity-log-latest-in-current-buffer`. Currently, any org header (a line that starts with '*') with a timestamp (starts with `<` and ends with `>`) is considered a valid date.
