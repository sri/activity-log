emacs ?= /usr/local/bin/emacs

all: test

test: clean
	$(emacs) -Q -batch -L . -l activity-log-test.el -f ert-run-tests-batch-and-exit

# NOT USED
compile:
	$(emacs) -Q -batch -f batch-byte-compile activity-log.el

clean:
	rm -f activity-log.elc

.PHONY:	all test
