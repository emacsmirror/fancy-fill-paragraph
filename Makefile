# SPDX-License-Identifier: GPL-3.0-or-later
# This Makefile is for convenience only; it is not needed for building the package.

EMACS ?= emacs

.PHONY: all test doc clean

all: test

test:
	./tests/fancy-fill-paragraph-tests.sh

doc:
	python3 ./_misc/readme_update.py

clean:
	rm -f *.elc
