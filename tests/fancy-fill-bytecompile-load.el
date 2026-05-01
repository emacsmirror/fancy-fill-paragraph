;;; fancy-fill-bytecompile-load.el --- Byte-compiled load smoke test -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loaded by `fancy-fill-bytecompile-load.py' after byte-compiling the package
;; into a staging directory.  The package source is removed from that directory
;; before this file runs, so `require' must resolve via the .elc.

;;; Code:

(require 'fancy-fill-paragraph)

(defconst fancy-fill-bytecompile-load-input "Hello world. Foo bar.")
(defconst fancy-fill-bytecompile-load-expected "Hello world.\nFoo bar.")

(defun fancy-fill-bytecompile-load-run ()
  "Fill a paragraph and assert the output matches the expected wrap."
  (let ((fancy-fill-paragraph-split-weights
         (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (with-temp-buffer
      (setq fill-column 20)
      (insert fancy-fill-bytecompile-load-input)
      (goto-char (point-min))
      (fancy-fill-paragraph)
      (let ((actual (buffer-string)))
        (cond
         ((equal actual fancy-fill-bytecompile-load-expected)
          (message "PASS: byte-compiled package loads and fills a paragraph"))
         (t
          (message "FAIL: output mismatch\n  expected: %S\n  actual:   %S"
                   fancy-fill-bytecompile-load-expected
                   actual)
          (kill-emacs 1)))))))

(fancy-fill-bytecompile-load-run)

;;; fancy-fill-bytecompile-load.el ends here
