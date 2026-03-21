;;; fancy-fill-paragraph-tests.el --- Testing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See: `fancy-fill-paragraph-tests.sh' for launching this script.

(require 'ert)

;;; Code:

(defvar fancy-fill-paragraph-tests-basedir (concat (file-name-directory load-file-name) "..")
  "Base directory for fancy-fill-paragraph tests.")
(add-to-list 'load-path fancy-fill-paragraph-tests-basedir)
(require 'fancy-fill-paragraph)

;; Suppress "Can't guess python-indent-offset" warnings in test buffers.
(setq python-indent-guess-indent-offset nil)

;; ---------------------------------------------------------------------------
;; Internal Functions/Macros

(defun buffer-reset-text (initial-buffer-text)
  "Initialize buffer with INITIAL-BUFFER-TEXT."
  (erase-buffer)
  ;; Don't move the cursor.
  (save-excursion (insert initial-buffer-text)))

(defmacro with-fancy-fill-paragraph-test (initial-buffer-text &rest body)
  "Run BODY with messages inhibited, setting buffer text to INITIAL-BUFFER-TEXT."
  (declare (indent 1))
  `(with-temp-buffer
     (let ((inhibit-message t))
       (buffer-reset-text ,initial-buffer-text)
       ,@body)))

(defun test-split-items (text)
  "Split TEXT and return the items list."
  (plist-get (fancy-fill-paragraph--paragraph-to-items text) :items))

(defun test-split-and-solve (text fill-col)
  "Split TEXT and solve with FILL-COL, returning result as a string."
  (let* ((split-result (fancy-fill-paragraph--paragraph-to-items text))
         (items (plist-get split-result :items)))
    (mapconcat #'identity
               (fancy-fill-paragraph--solve
                items
                fill-col
                (plist-get split-result :break-weights)
                (plist-get split-result :seps)
                (plist-get split-result :sep-lens)
                fancy-fill-paragraph-blank-space-weight)
               "\n")))


;; ---------------------------------------------------------------------------
;; NOOP Tests (degenerate inputs)

(ert-deftest fill-noop-empty-buffer ()
  "Empty buffer should be unchanged."
  (with-fancy-fill-paragraph-test ""
    (fancy-fill-paragraph)
    (should (equal "" (buffer-string)))))

(ert-deftest fill-noop-single-space ()
  "Single space should be unchanged."
  (with-fancy-fill-paragraph-test " "
    (fancy-fill-paragraph)
    (should (equal " " (buffer-string)))))

(ert-deftest fill-noop-blank-lines ()
  "Blank lines should be unchanged."
  (with-fancy-fill-paragraph-test "\n\n\n"
    (fancy-fill-paragraph)
    (should (equal "\n\n\n" (buffer-string)))))

(ert-deftest fill-noop-c-mode-empty-comment ()
  "Empty C block comment should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (text "/*\n */"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (+ (point-min) 3))
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-c-mode-blank-comment ()
  "C block comment with only blank `*' line should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (text "/*\n *\n */"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (+ (point-min) 3))
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-python-empty-string ()
  "Empty Python triple-quoted string should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (text "\"\"\"\n\"\"\""))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (+ (point-min) 4))
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-python-blank-string ()
  "Python triple-quoted string with only blank line should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (text "\"\"\"\n\n\"\"\""))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (+ (point-min) 4))
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))


;; ---------------------------------------------------------------------------
;; Split Tests

(ert-deftest split-single-sentence ()
  "Single sentence with no split point stays as one item."
  (let ((text-initial "Hello world.")
        (text-expected '("Hello world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-two-sentences ()
  "Two sentences split at period boundary."
  (let ((text-initial "First. Second.")
        (text-expected '("First." "Second."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-three-sentences ()
  "Three sentences split at each period."
  (let ((text-initial "A. B. C.")
        (text-expected '("A." "B." "C."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-no-period ()
  "Text without any period stays as one item."
  (let ((text-initial "No period here")
        (text-expected '("No period here"))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-trailing-period-no-space ()
  "Trailing period without following space is not a split point."
  (let ((text-initial "Hello world.")
        (text-expected '("Hello world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-comma-active ()
  "Commas act as split points when weight is non-zero."
  (let ((text-initial "Hello, world.")
        (text-expected '("Hello," "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-comma-inactive ()
  "Commas are not split points when weight is zero."
  (let ((text-initial "Hello, world.")
        (text-expected '("Hello, world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-mixed-period-comma ()
  "Both period and comma boundaries are split when active."
  (let ((text-initial "Hello, world. Foo, bar.")
        (text-expected '("Hello," "world." "Foo," "bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-period-inactive ()
  "Periods are not split points when weight is zero."
  (let ((text-initial "Hello. world, foo.")
        (text-expected '("Hello. world," "foo."))
        (fancy-fill-paragraph-split-weights (list :period 0 :comma 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-all-inactive ()
  "No splitting when all weights are zero."
  (let ((text-initial "Hello. world, foo.")
        (text-expected '("Hello. world, foo."))
        (fancy-fill-paragraph-split-weights (list :period 0 :comma 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-colon-active ()
  "Colons act as split points when weight is non-zero."
  (let ((text-initial "Note: this is important.")
        (text-expected '("Note:" "this is important."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :colon 40 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-colon-inactive ()
  "Colons are not split points when weight is zero."
  (let ((text-initial "Note: this is important.")
        (text-expected '("Note: this is important."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :colon 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-semicolon-active ()
  "Semicolons act as split points when weight is non-zero."
  (let ((text-initial "First clause; second clause.")
        (text-expected '("First clause;" "second clause."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :semicolon 60 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-semicolon-inactive ()
  "Semicolons are not split points when weight is zero."
  (let ((text-initial "First clause; second clause.")
        (text-expected '("First clause; second clause."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :semicolon 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-question-active ()
  "Question marks act as split points when weight is non-zero."
  (let ((text-initial "Really? Yes.")
        (text-expected '("Really?" "Yes."))
        (fancy-fill-paragraph-split-weights (list :period 100 :question 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-question-inactive ()
  "Question marks are not split points when weight is zero."
  (let ((text-initial "Really? Yes.")
        (text-expected '("Really? Yes."))
        (fancy-fill-paragraph-split-weights (list :period 100 :question 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-exclamation-active ()
  "Exclamation marks act as split points when weight is non-zero."
  (let ((text-initial "Wow! Amazing.")
        (text-expected '("Wow!" "Amazing."))
        (fancy-fill-paragraph-split-weights (list :period 100 :exclamation 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-exclamation-inactive ()
  "Exclamation marks are not split points when weight is zero."
  (let ((text-initial "Wow! Amazing.")
        (text-expected '("Wow! Amazing."))
        (fancy-fill-paragraph-split-weights (list :period 100 :exclamation 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-em-dash-active ()
  "Em dashes act as split points when weight is non-zero."
  (let ((text-initial "Hello\u2014 world.")
        (text-expected '("Hello\u2014" "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :em-dash 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-em-dash-inactive ()
  "Em dashes are not split points when weight is zero."
  (let ((text-initial "Hello\u2014 world.")
        (text-expected '("Hello\u2014 world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :em-dash 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-en-dash-active ()
  "En dashes act as split points when weight is non-zero."
  (let ((text-initial "Hello\u2013 world.")
        (text-expected '("Hello\u2013" "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :en-dash 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-en-dash-inactive ()
  "En dashes are not split points when weight is zero."
  (let ((text-initial "Hello\u2013 world.")
        (text-expected '("Hello\u2013 world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :en-dash 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-hyphen-dash-active ()
  "Hyphen dashes act as split points when weight is non-zero."
  (let ((text-initial "Hello - world.")
        (text-expected '("Hello -" "world."))
        (fancy-fill-paragraph-infix-delimiters '(("-" . 50)))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-hyphen-dash-inactive ()
  "Hyphen dashes are not split points when weight is zero."
  (let ((text-initial "Hello - world.")
        (text-expected '("Hello - world."))
        (fancy-fill-paragraph-infix-delimiters nil)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-hyphen-dash-with-space-active ()
  "Hyphen dash is not subsumed by space delimiter."
  (let ((text-initial "Hello - world.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello -\n"
                 "world."))
        (fancy-fill-paragraph-infix-delimiters '(("-" . 50)))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 10)))))

(ert-deftest split-paren-active ()
  "Closing parentheses act as split points when weight is non-zero."
  (let ((text-initial "Hello (world) foo.")
        (text-expected '("Hello (world)" "foo."))
        (fancy-fill-paragraph-split-weights (list :period 100 :paren 50 :open-paren 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-paren-inactive ()
  "Closing parentheses are not split points when weight is zero."
  (let ((text-initial "Hello (world) foo.")
        (text-expected '("Hello (world) foo."))
        (fancy-fill-paragraph-split-weights (list :period 100 :paren 0 :open-paren 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-bracket-active ()
  "Closing brackets act as split points when weight is non-zero."
  (let ((text-initial "See [1] for details.")
        (text-expected '("See [1]" "for details."))
        (fancy-fill-paragraph-split-weights
         (list :period 100 :bracket 50 :open-bracket 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-bracket-inactive ()
  "Closing brackets are not split points when weight is zero."
  (let ((text-initial "See [1] for details.")
        (text-expected '("See [1] for details."))
        (fancy-fill-paragraph-split-weights
         (list :period 100 :bracket 0 :open-bracket 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-brace-active ()
  "Closing braces act as split points when weight is non-zero."
  (let ((text-initial "Use {foo} here.")
        (text-expected '("Use {foo}" "here."))
        (fancy-fill-paragraph-split-weights (list :period 100 :brace 50 :open-brace 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-brace-inactive ()
  "Closing braces are not split points when weight is zero."
  (let ((text-initial "Use {foo} here.")
        (text-expected '("Use {foo} here."))
        (fancy-fill-paragraph-split-weights (list :period 100 :brace 0 :open-brace 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-ellipsis-active ()
  "Ellipsis acts as a split point when weight is non-zero."
  (let ((text-initial "Hello... world.")
        (text-expected '("Hello..." "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :ellipsis 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-ellipsis-inactive ()
  "Ellipsis is not a split point when weight is zero."
  (let ((text-initial "Hello... world.")
        (text-expected '("Hello... world."))
        (fancy-fill-paragraph-split-weights (list :period 0 :ellipsis 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-ellipsis-priority-over-period ()
  "Ellipsis takes priority over period at the same position."
  (let ((text-initial "Hello... world.")
        (text-expected '("Hello..." "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :ellipsis 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-ellipsis-unicode-active ()
  "Unicode ellipsis acts as a split point when weight is non-zero."
  (let ((text-initial "Hello\u2026 world.")
        (text-expected '("Hello\u2026" "world."))
        (fancy-fill-paragraph-split-weights (list :period 100 :ellipsis 50 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-ellipsis-unicode-inactive ()
  "Unicode ellipsis is not a split point when weight is zero."
  (let ((text-initial "Hello\u2026 world.")
        (text-expected '("Hello\u2026 world."))
        (fancy-fill-paragraph-split-weights (list :period 0 :ellipsis 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))


;; ---------------------------------------------------------------------------
;; Solver Tests

(ert-deftest solve-single-item ()
  "Single item returns as one line."
  (let ((text-initial "Hello.")
        (text-expected "Hello.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-two-items-fit-one-line ()
  "Two short items fit on one line."
  (let ((text-initial "A. B.")
        (text-expected "A. B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-two-items-need-two-lines ()
  "Two items that exceed fill-column are split."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 15)))))

(ert-deftest solve-double-space-separator ()
  "Double space is used between sentences when enabled."
  (let ((text-initial "A. B.")
        (text-expected "A.  B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-single-space-separator ()
  "Single space is used between sentences when double-space is disabled."
  (let ((text-initial "A. B.")
        (text-expected "A. B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-comma-single-space ()
  "Single space is always used after commas regardless of double-space setting."
  (let ((text-initial "Hello, world.")
        (text-expected "Hello, world.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 50 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-prefer-period-break ()
  "Solver prefers breaking at periods over commas."
  (let ((text-initial "Hello, world. Foo, bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello, world.\n"
                 "Foo, bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 30 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-oversized-single-item ()
  "Single item exceeding fill-column is placed on its own line."
  (let ((text-initial "This is a very long sentence that exceeds the fill column.")
        (text-expected "This is a very long sentence that exceeds the fill column.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-optimal-three-way-split ()
  "Solver optimally distributes items across lines."
  (let ((text-initial "Alpha beta. Gamma. Delta epsilon.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha beta. Gamma.\n"
                 "Delta epsilon."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    ;; "Alpha beta." (11) + " " + "Gamma." (6) = 18, fits in 20.
    ;; "Delta epsilon." (14) on next line, fits in 20.
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-double-space-affects-line-length ()
  "Double space after period is accounted for in line length calculation."
  (let ((text-initial "Alpha. Beta.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha.\n"
                 "Beta."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space t))
    ;; "Alpha." (6) + "  " (2) + "Beta." (5) = 13, exceeds fill-col 12.
    ;; Must split onto two lines.
    (should (equal text-expected (test-split-and-solve text-initial 12)))))


;; ---------------------------------------------------------------------------
;; Integration Tests

(ert-deftest fill-no-wrap-needed ()
  "Short paragraph that fits on one line is unchanged."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-simple-wrap ()
  "Long paragraph wraps at sentence boundary."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-multi-line-normalize ()
  "Multi-line input is normalized and refilled."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar. Baz qux."))
        (text-expected
         ;; format-next-line: off
         (concat "Hello world. Foo bar.\n"
                 "Baz qux."))
        (fill-column 25)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-double-space ()
  "Double space after periods when enabled."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected "Hello world.  Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-normalizes-double-space ()
  "Existing double spaces are normalized then re-applied based on preference."
  (let ((text-initial "Hello world.  Foo bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-comma-no-split ()
  "Commas don't cause splits when weight is zero."
  (let ((text-initial "Hello, world. Foo, bar.")
        (text-expected "Hello, world. Foo, bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-comma-split-when-active ()
  "Commas cause splits when weight is non-zero and line exceeds fill-column."
  (let ((text-initial "Hello, world, foo.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello, world,\n"
                 "foo."))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 50 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-collapse-extra-whitespace ()
  "Multiple spaces within text are collapsed."
  (let ((text-initial "Hello   world.   Foo   bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Blank-Space-Weight Tests

(ert-deftest solve-blank-space-weight-zero-no-join ()
  "When blank-space-weight is zero, items are never joined."
  (let ((text-initial "A. B.")
        (text-expected
         ;; format-next-line: off
         (concat "A.\n"
                 "B."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 0))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-blank-space-weight-100-joins-freely ()
  "When blank-space-weight is 100, items join freely when they fit."
  (let ((text-initial "A. B. C.")
        (text-expected "A. B. C.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest fill-blank-space-weight-zero ()
  "Every delimiter boundary produces a line break when weight is zero."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 0))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Space Delimiter Tests

(ert-deftest split-space-active ()
  "Spaces act as split points when weight is non-zero."
  (let ((text-initial "Hello world foo")
        (text-expected '("Hello" "world" "foo"))
        (fancy-fill-paragraph-split-weights (list :period 0 :space 1)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-space-inactive ()
  "Spaces are not split points when weight is zero."
  (let ((text-initial "Hello world foo")
        (text-expected '("Hello world foo"))
        (fancy-fill-paragraph-split-weights (list :period 0 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest solve-space-wraps-long-text ()
  "Space delimiter enables word-level wrapping for long text."
  (let ((text-initial "This is a very long sentence that exceeds the fill column.")
        (text-expected
         ;; format-next-line: off
         (concat "This is a very\n"
                 "long sentence that\n"
                 "exceeds the fill\n"
                 "column."))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-space-prefers-delimiter-break ()
  "Solver prefers delimiter breaks over space breaks."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 15)))))


;; ---------------------------------------------------------------------------
;; Open Bracket Tests

(ert-deftest solve-open-paren-avoids-line-end ()
  "Solver prefers breaking before opening paren."
  (let ((text-initial "Hello (world) end.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello\n"
                 "(world)\n"
                 "end."))
        (fancy-fill-paragraph-split-weights (list :period 100 :paren 25 :open-paren 5 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 10)))))


;; ---------------------------------------------------------------------------
;; Quote Tests

(ert-deftest solve-closing-quote-break ()
  "Solver breaks after closing double quote."
  (let ((text-initial "He said \"hello\" then left.")
        (text-expected
         ;; format-next-line: off
         (concat "He said \"hello\"\n"
                 "then left."))
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 16)))))

(ert-deftest solve-open-quote-avoids-line-end ()
  "Solver prefers breaking before opening double quote."
  (let ((text-initial "He said \"hello\" end.")
        (text-expected
         ;; format-next-line: off
         (concat "He said\n"
                 "\"hello\"\n"
                 "end."))
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 5 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 10)))))

(ert-deftest solve-closing-quote-sentence-end-double-space ()
  "Closing quote uses double space when sentence ends inside quotes."
  (let ((text-initial "She said \"hello.\" He replied.")
        (text-expected "She said \"hello.\"  He replied.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-quote-mid-sentence-no-double-space ()
  "Closing quote mid-sentence uses single space, not double."
  (let ((text-initial "The value \"None\" is returned.")
        (text-expected "The value \"None\" is returned.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-paren-sentence-end-double-space ()
  "Closing paren uses double space when sentence ends inside parens."
  (let ((text-initial "(See above.) The next.")
        (text-expected "(See above.)  The next.")
        (fancy-fill-paragraph-split-weights (list :period 100 :paren 25 :open-paren 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-bracket-sentence-end-double-space ()
  "Closing bracket uses double space when sentence ends inside brackets."
  (let ((text-initial "[See above.] The next.")
        (text-expected "[See above.]  The next.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :bracket 25 :open-bracket 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-brace-sentence-end-double-space ()
  "Closing brace uses double space when sentence ends inside braces."
  (let ((text-initial "{See above.} The next.")
        (text-expected "{See above.}  The next.")
        (fancy-fill-paragraph-split-weights (list :period 100 :brace 25 :open-brace 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-question-mark-quote-double-space ()
  "Question mark before closing quote gets double space."
  (let ((text-initial "She asked \"why?\" He shrugged.")
        (text-expected "She asked \"why?\"  He shrugged.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :question 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-exclamation-quote-double-space ()
  "Exclamation before closing quote gets double space."
  (let ((text-initial "He yelled \"stop!\" She froze.")
        (text-expected "He yelled \"stop!\"  She froze.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :exclamation 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-ellipsis-quote-double-space ()
  "Ellipsis before closing quote gets double space."
  (let ((text-initial "She said \"well\u2026\" He waited.")
        (text-expected "She said \"well\u2026\"  He waited.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :ellipsis 100 :double-quote 25 :open-double-quote 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-comma-after-period-no-double-space ()
  "Comma after abbreviation period does not get double space."
  (let ((text-initial "Use etc., not etcetera.")
        (text-expected "Use etc., not etcetera.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 25 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-semicolon-after-period-no-double-space ()
  "Semicolon after abbreviation period does not get double space."
  (let ((text-initial "Use etc.; not etcetera.")
        (text-expected "Use etc.; not etcetera.")
        (fancy-fill-paragraph-split-weights (list :period 100 :semicolon 25 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-colon-after-period-no-double-space ()
  "Colon after abbreviation period does not get double space."
  (let ((text-initial "See etc.: the list.")
        (text-expected "See etc.: the list.")
        (fancy-fill-paragraph-split-weights (list :period 100 :colon 25 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))


;; ---------------------------------------------------------------------------
;; Region Tests

(ert-deftest fill-region-two-paragraphs ()
  "Two paragraphs in a region are filled separately."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Hello world. Foo bar.\n"
                 "\n"
                 "Baz qux. Alpha beta."))
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar.\n"
                 "\n"
                 "Baz qux.\n"
                 "Alpha beta."))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-region-preserves-blank-lines ()
  "Blank lines between paragraphs are preserved."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Short.\n"
                 "\n"
                 "Also short."))
        (text-expected
         ;; format-next-line: off
         (concat "Short.\n"
                 "\n"
                 "Also short."))
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-region-three-paragraphs ()
  "Three paragraphs in a region are each filled independently."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Alpha beta. Gamma.\n"
                 "\n"
                 "Delta. Epsilon.\n"
                 "\n"
                 "Zeta eta. Theta."))
        (text-expected
         ;; format-next-line: off
         (concat "Alpha beta.\n"
                 "Gamma.\n"
                 "\n"
                 "Delta.\n"
                 "Epsilon.\n"
                 "\n"
                 "Zeta eta.\n"
                 "Theta."))
        (fill-column 12)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-region-normalizes-paragraphs ()
  "Multi-line paragraphs in a region are normalized and refilled."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Hello\n"
                 "world. Foo bar.\n"
                 "\n"
                 "Baz qux.\n"
                 "Alpha beta."))
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar.\n"
                 "\n"
                 "Baz qux.\n"
                 "Alpha beta."))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Prefix Tests

(ert-deftest fill-prefix-whitespace ()
  "Indented paragraph preserves leading whitespace prefix."
  (let ((text-initial
         ;; format-next-line: off
         (concat "    Hello world. Foo bar.\n"
                 "    Baz qux."))
        (text-expected
         ;; format-next-line: off
         (concat "    Hello world.\n"
                 "    Foo bar. Baz qux."))
        (fill-column 22)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-prefix-comment ()
  "Comment prefix is detected and preserved."
  (let ((text-initial
         ;; format-next-line: off
         (concat ";; Hello world. Foo bar.\n"
                 ";; Baz qux."))
        (text-expected
         ;; format-next-line: off
         (concat ";; Hello world.\n"
                 ";; Foo bar. Baz qux."))
        (fill-column 21)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (emacs-lisp-mode)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-prefix-explicit ()
  "Explicit fill-prefix is used when set."
  (let ((text-initial
         ;; format-next-line: off
         (concat "# Hello world. Foo bar.\n"
                 "# Baz qux."))
        (text-expected
         ;; format-next-line: off
         (concat "# Hello world.\n"
                 "# Foo bar. Baz qux."))
        (fill-column 20)
        (fill-prefix "# ")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-prefix-reduces-fill-column ()
  "Prefix length is subtracted from fill-column."
  (let ((text-initial
         ;; format-next-line: off
         (concat "        A. B."))
        (text-expected
         ;; format-next-line: off
         (concat "        A.\n"
                 "        B."))
        (fill-column 12)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    ;; Prefix "        " (8 chars) leaves 4 columns.
    ;; "A." (2) + " " (1) + "B." (2) = 5, exceeds 4. Must split.
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


(ert-deftest fill-prefix-mismatch-later-lines ()
  "Prefix not matching later lines is reduced to minimum common indent."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "   Alpha bravo charlie.\n"
          "   Delta echo foxtrot.\n"
          "Golf hotel india."))
        (text-expected
         ;; format-next-line: off
         (concat
          "   Alpha bravo charlie.\n"
          "   Delta echo foxtrot.\n"
          "Golf hotel india."))
        (fill-column 30)
        (fill-prefix "   ")
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-prefix-dot-point-not-propagated ()
  "Dot-point prefix is not added to every line when dot-point mode is off."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "- This is a sentence with a dot-point prefix"
          " that should not be propagated to every line."))
        (text-expected
         ;; format-next-line: off
         (concat
          "- This is a sentence with a dot-point\n"
          "  prefix that should not be propagated\n"
          "  to every line."))
        (fill-column 40)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dickens-tale-of-two-cities ()
  "Opening of A Tale of Two Cities fills at comma boundaries."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "It was the best of times, it was the worst of times,"
          " it was the age of wisdom, it was the age of foolishness,"
          " it was the epoch of belief, it was the epoch of incredulity,"
          " it was the season of Light, it was the season of Darkness,"
          " it was the spring of hope, it was the winter of despair,"
          " we had everything before us, we had nothing before us,"
          " we were all going direct to Heaven,"
          " we were all going direct the other way."))
        (text-expected
         ;; format-next-line: off
         (concat
          "It was the best of times, it was the worst of times, it was the age\n"
          "of wisdom, it was the age of foolishness, it was the epoch of belief,\n"
          "it was the epoch of incredulity, it was the season of Light,\n"
          "it was the season of Darkness, it was the spring of hope,\n"
          "it was the winter of despair, we had everything before us,\n"
          "we had nothing before us, we were all going direct to Heaven,\n"
          "we were all going direct the other way."))
        (fill-column 70)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Dot-Point Tests

(ert-deftest fill-dot-point-flat ()
  "Three flat dot-points fill independently."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "- Delta echo foxtrot\n"
                 "- Golf hotel india"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie\n"
                 "- Delta echo\n"
                 "  foxtrot\n"
                 "- Golf hotel\n"
                 "  india"))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-mixed-prefix-lengths ()
  "Dot-points with different prefix lengths fill correctly."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie delta\n"
                 ">> Echo foxtrot golf hotel"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "  delta\n"
                 ">> Echo foxtrot golf\n"
                 "   hotel"))
        (fill-column 22)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75)
        (fancy-fill-paragraph-dot-point-prefix (list "- " ">> ")))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-continuation ()
  "Dot-point with continuation text joins and fills as one paragraph."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "  delta echo foxtrot\n"
                 "- Golf hotel"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie delta\n"
                 "  echo foxtrot\n"
                 "- Golf hotel"))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-recursive ()
  "Nested dot-points at multiple indent levels fill independently."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "  - Delta echo foxtrot\n"
                 "  - Golf hotel india\n"
                 "- Juliet kilo lima"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie\n"
                 "  - Delta echo\n"
                 "    foxtrot\n"
                 "  - Golf hotel\n"
                 "    india\n"
                 "- Juliet kilo\n"
                 "  lima"))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-long-lines-wrap ()
  "Long dot-point lines wrap into multiple continuation lines."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie delta echo foxtrot\n"
                 "- Golf hotel india juliet kilo lima"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie delta\n"
                 "  echo foxtrot\n"
                 "- Golf hotel\n"
                 "  india juliet\n"
                 "  kilo lima"))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-short-lines-unwrap ()
  "Short dot-point continuation lines are joined and refilled."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha\n"
                 "  bravo\n"
                 "  charlie\n"
                 "- Delta\n"
                 "  echo"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie\n"
                 "- Delta echo"))
        (fill-column 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-nested-three-levels ()
  "Three nesting levels each fill within their reduced fill-column."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "  - Delta echo foxtrot golf\n"
                 "    - Hotel india juliet kilo\n"
                 "    - Lima mike november oscar\n"
                 "  - Papa quebec romeo\n"
                 "- Sierra tango uniform"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie\n"
                 "  - Delta echo\n"
                 "    foxtrot golf\n"
                 "    - Hotel india\n"
                 "      juliet kilo\n"
                 "    - Lima mike\n"
                 "      november oscar\n"
                 "  - Papa quebec\n"
                 "    romeo\n"
                 "- Sierra tango\n"
                 "  uniform"))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-nested-continuation ()
  "Nested dot-points with continuation text at multiple levels."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo charlie\n"
                 "  delta echo foxtrot\n"
                 "  - Golf hotel india juliet kilo lima\n"
                 "    mike november oscar\n"
                 "  - Papa quebec romeo\n"
                 "- Sierra tango"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  charlie delta echo\n"
                 "  foxtrot\n"
                 "  - Golf hotel india\n"
                 "    juliet kilo lima\n"
                 "    mike november\n"
                 "    oscar\n"
                 "  - Papa quebec\n"
                 "    romeo\n"
                 "- Sierra tango"))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-nested-sibling-groups ()
  "Sibling top-level groups each with nested children."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  - Charlie delta echo foxtrot\n"
                 "  - Golf hotel india juliet\n"
                 "- Kilo lima\n"
                 "  - Mike november oscar papa\n"
                 "  - Quebec romeo sierra tango"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha bravo\n"
                 "  - Charlie delta\n"
                 "    echo foxtrot\n"
                 "  - Golf hotel india\n"
                 "    juliet\n"
                 "- Kilo lima\n"
                 "  - Mike november\n"
                 "    oscar papa\n"
                 "  - Quebec romeo\n"
                 "    sierra tango"))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-nested-short-lines-unwrap ()
  "Deeply nested short continuation lines are joined and refilled."
  (let ((text-initial
         ;; format-next-line: off
         (concat "- Alpha\n"
                 "  - Bravo\n"
                 "    charlie\n"
                 "    delta\n"
                 "    echo\n"
                 "  - Foxtrot\n"
                 "    golf\n"
                 "- Hotel\n"
                 "  india"))
        (text-expected
         ;; format-next-line: off
         (concat "- Alpha\n"
                 "  - Bravo charlie\n"
                 "    delta echo\n"
                 "  - Foxtrot golf\n"
                 "- Hotel india"))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-long-prose-default-weights ()
  "Long dot-points with default weights and rich punctuation."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "- The \"quick brown fox\" (a well-known animal) jumped over the lazy dog;"
          " however, the dog was not amused."
          " According to the experts, \"this behavior is typical.\""
          " See [RFC 2119] for definitions of key words: MUST, SHOULD, and MAY."
          " The fox (having landed safely) ran into the forest"
          " \u2014 disappearing among the trees."
          " Was the dog upset? Absolutely!"
          " But it remained calm, waiting patiently for the fox to return.\n"
          "- Meanwhile, the cat [a domestic shorthair] sat on the mat."
          " \"Why bother?\" it thought;"
          " after all, cats are known for their indifference."
          " The mat (which was old and worn) provided little comfort,"
          " but the cat did not care."
          " Note: this is not unusual."
          " According to the researchers,"
          " cats spend 70% of their lives sleeping."
          " The remaining 30% is devoted to eating, grooming,"
          " and ignoring their owners"
          " \u2014 a well-documented phenomenon (see Johnson et al. 2023)."))
        (text-expected
         ;; format-next-line: off
         (concat
          "- The \"quick brown fox\" (a well-known animal)\n"
          "  jumped over the lazy dog; however, the dog was not amused.\n"
          "  According to the experts, \"this behavior is typical.\"\n"
          "  See [RFC 2119] for definitions of key words: MUST, SHOULD, and MAY.\n"
          "  The fox (having landed safely) ran into the forest \u2014\n"
          "  disappearing among the trees. Was the dog upset? Absolutely!\n"
          "  But it remained calm, waiting patiently for the fox to return.\n"
          "- Meanwhile, the cat [a domestic shorthair] sat on the mat.\n"
          "  \"Why bother?\" it thought;\n"
          "  after all, cats are known for their indifference.\n"
          "  The mat (which was old and worn) provided little comfort,\n"
          "  but the cat did not care. Note: this is not unusual. According\n"
          "  to the researchers, cats spend 70% of their lives sleeping.\n"
          "  The remaining 30% is devoted to eating, grooming, and ignoring their\n"
          "  owners \u2014 a well-documented phenomenon (see Johnson et al. 2023)."))
        (fill-column 70)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-nested-long-prose-default-weights ()
  "Nested long dot-points with default weights and rich punctuation."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "- The \"quick brown fox\" (a well-known animal) jumped over the lazy dog;"
          " however, the dog was not amused."
          " According to the experts, \"this behavior is typical.\""
          " See [RFC 2119] for definitions of key words: MUST, SHOULD, and MAY."
          " The fox (having landed safely) ran into the forest"
          " \u2014 disappearing among the trees."
          " Was the dog upset? Absolutely!"
          " But it remained calm, waiting patiently for the fox to return.\n"
          "- Meanwhile, the cat [a domestic shorthair] sat on the mat.\n"
          "  Several observations were noted:\n"
          "  - \"Why bother?\" it thought;"
          " after all, cats are known for their indifference.\n"
          "    The mat (which was old and worn) provided little comfort,"
          " but the cat did not care.\n"
          "  - According to the researchers,"
          " cats spend 70% of their lives sleeping.\n"
          "    The remaining 30% is devoted to eating, grooming,"
          " and ignoring their owners"
          " \u2014 a well-documented phenomenon (see Johnson et al. 2023)."))
        (text-expected
         ;; format-next-line: off
         (concat
          "- The \"quick brown fox\" (a well-known animal)\n"
          "  jumped over the lazy dog; however, the dog was not amused.\n"
          "  According to the experts, \"this behavior is typical.\"\n"
          "  See [RFC 2119] for definitions of key words: MUST, SHOULD, and MAY.\n"
          "  The fox (having landed safely) ran into the forest \u2014\n"
          "  disappearing among the trees. Was the dog upset? Absolutely!\n"
          "  But it remained calm, waiting patiently for the fox to return.\n"
          "- Meanwhile, the cat [a domestic shorthair] sat on the mat.\n"
          "  Several observations were noted:\n"
          "  - \"Why bother?\" it thought;\n"
          "    after all, cats are known for their indifference.\n"
          "    The mat (which was old and worn) provided little comfort,\n"
          "    but the cat did not care.\n"
          "  - According to the researchers, cats spend 70% of their\n"
          "    lives sleeping. The remaining 30% is devoted to eating,\n"
          "    grooming, and ignoring their owners \u2014\n"
          "    a well-documented phenomenon (see Johnson et al. 2023)."))
        (fill-column 70)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-preamble-line ()
  "Dot-points with a preamble line preserve structure and indent continuations."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "The settings pass includes:\n"
          "- Background-mode assignment (#Global.background),"
          " checked by other subsystems which may be skipped"
          " in background mode.\n"
          "- The animation player may be launched which takes"
          " over argument passing, initializes the sub-systems"
          " it needs which have not yet been started."
          " The animation player will call `exit(..)` too,"
          " so code after this call never runs when it's invoked.\n"
          "- All the `--debug-*` flags."))
        (text-expected
         ;; format-next-line: off
         (concat
          "The settings pass includes:\n"
          "- Background-mode assignment (#Global.background),\n"
          "  checked by other subsystems which may be skipped in background mode.\n"
          "- The animation player may be launched which takes over argument\n"
          "  passing, initializes the sub-systems it needs which have not yet\n"
          "  been started. The animation player will call `exit(..)` too,\n"
          "  so code after this call never runs when it's invoked.\n"
          "- All the `--debug-*` flags."))
        (fill-column 70)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Indent-Split Tests

(ert-deftest fill-indent-split-paragraphs ()
  "Changes in leading indentation split into separate paragraphs."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Alpha bravo.\n"
                 "   Charlie delta echo\n"
                 "   foxtrot. Golf hotel india.\n"
                 "      Juliet kilo.\n"
                 "   Lima mike."))
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo.\n"
                 "   Charlie delta echo foxtrot.\n"
                 "   Golf hotel india.\n"
                 "      Juliet kilo.\n"
                 "   Lima mike."))
        (fill-column 30)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-indent-split-three-line-groups ()
  "Each indentation group spans three lines and is filled independently."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Alpha bravo\n"
                 "charlie delta\n"
                 "echo foxtrot.\n"
                 "   Golf hotel\n"
                 "   india juliet\n"
                 "   kilo lima.\n"
                 "      Mike november\n"
                 "      oscar papa\n"
                 "      quebec romeo.\n"
                 "   Sierra tango\n"
                 "   uniform victor\n"
                 "   whiskey xray."))
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo charlie delta\n"
                 "echo foxtrot.\n"
                 "   Golf hotel india juliet\n"
                 "   kilo lima.\n"
                 "      Mike november oscar\n"
                 "      papa quebec romeo.\n"
                 "   Sierra tango uniform\n"
                 "   victor whiskey xray."))
        (fill-column 26)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-indent-split-mixed-dot-points ()
  "Mixed indentation with dot-points at multiple levels."
  (let
      ((text-initial
        ;; format-next-line: off
        (concat
         "The system architecture has three main layers with distinct responsibilities.\n"
         "   The presentation layer handles user interaction and rendering of visual components."
         " It communicates with the service layer through well defined interfaces.\n"
         "      Configuration is loaded at startup from multiple sources including environment variables.\n"
         "   The service layer coordinates business logic:\n"
         "   - Authentication validates user credentials against the identity provider and issues session tokens.\n"
         "   - Authorization checks role-based permissions using configurable access control policies.\n"
         "      Database connections use a pooling strategy:\n"
         "      - Read replicas handle all SELECT queries to improve overall throughput.\n"
         "      - The write primary processes INSERT and UPDATE operations exclusively.\n"
         "      - Connection limits are enforced per application tier to prevent exhaustion.\n"
         "   Error handling follows a consistent pattern across all layers.\n"
         "      Recoverable errors trigger retry with exponential backoff."
         " Fatal errors are logged and escalated to the monitoring system.\n"
         "   The data layer manages persistence:\n"
         "   - Schema migrations run automatically during each deployment using versioned scripts.\n"
         "   - Query indexes are maintained to optimize the most frequent access patterns.\n"
         "Final integration testing verifies all layers function together correctly."))
       (text-expected
        ;; format-next-line: off
        (concat
         "The system architecture has three main layers with distinct\n"
         "responsibilities.\n"
         "   The presentation layer handles user interaction and\n"
         "   rendering of visual components. It communicates with the\n"
         "   service layer through well defined interfaces.\n"
         "      Configuration is loaded at startup from multiple\n"
         "      sources including environment variables.\n"
         "   The service layer coordinates business logic:\n"
         "   - Authentication validates user credentials against the\n"
         "     identity provider and issues session tokens.\n"
         "   - Authorization checks role-based permissions using\n"
         "     configurable access control policies.\n"
         "      Database connections use a pooling strategy:\n"
         "      - Read replicas handle all SELECT queries to improve\n"
         "        overall throughput.\n"
         "      - The write primary processes INSERT and UPDATE\n"
         "        operations exclusively.\n"
         "      - Connection limits are enforced per application tier\n"
         "        to prevent exhaustion.\n"
         "   Error handling follows a consistent pattern across all\n"
         "   layers.\n"
         "      Recoverable errors trigger retry with exponential\n"
         "      backoff. Fatal errors are logged and escalated to the\n"
         "      monitoring system.\n"
         "   The data layer manages persistence:\n"
         "   - Schema migrations run automatically during each\n"
         "     deployment using versioned scripts.\n"
         "   - Query indexes are maintained to optimize the most\n"
         "     frequent access patterns.\n"
         "Final integration testing verifies all layers function\n"
         "together correctly."))
       (fill-column 60)
       (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-infix-dash-no-false-dot-points ()
  "Wrapping text with infix dashes never creates false dot-point lines."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "Alpha one - bravo two - charlie three - delta four"
          " - echo five - foxtrot six - golf seven - hotel eight.\n"
          "   India one - juliet two - kilo three - lima four"
          " - mike five - november six - oscar seven - papa eight.\n"
          "Quebec one - romeo two - sierra three - tango four"
          " - uniform five - victor six - whiskey seven - xray eight."))
        (fill-column 30)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      ;; Fill with short column.
      (fancy-fill-paragraph)
      (let ((wrapped (buffer-string)))
        ;; Verify no line starts with optional whitespace then "- " (false dot-point).
        (should-not (string-match-p "^[ \t]*- " wrapped))
        ;; Round-trip: fill with very large column should recover original.
        (let ((fill-column 1000))
          (fancy-fill-paragraph)
          (should (equal text-initial (buffer-string))))))))

(ert-deftest fill-indent-split-mixed-dot-points-unwrap ()
  "Wrapped text with mixed indentation and dot-points unwraps to canonical form."
  (let
      ((text-initial
        ;; format-next-line: off
        (concat
         "The system architecture has three main layers with distinct\n"
         "responsibilities.\n"
         "   The presentation layer handles user interaction and\n"
         "   rendering of visual components. It communicates with the\n"
         "   service layer through well defined interfaces.\n"
         "      Configuration is loaded at startup from multiple\n"
         "      sources including environment variables.\n"
         "   The service layer coordinates business logic:\n"
         "   - Authentication validates user credentials against the\n"
         "     identity provider and issues session tokens.\n"
         "   - Authorization checks role-based permissions using\n"
         "     configurable access control policies.\n"
         "      Database connections use a pooling strategy:\n"
         "      - Read replicas handle all SELECT queries to improve\n"
         "        overall throughput.\n"
         "      - The write primary processes INSERT and UPDATE\n"
         "        operations exclusively.\n"
         "      - Connection limits are enforced per application tier\n"
         "        to prevent exhaustion.\n"
         "   Error handling follows a consistent pattern across all\n"
         "   layers.\n"
         "      Recoverable errors trigger retry with exponential\n"
         "      backoff. Fatal errors are logged and escalated to the\n"
         "      monitoring system.\n"
         "   The data layer manages persistence:\n"
         "   - Schema migrations run automatically during each\n"
         "     deployment using versioned scripts.\n"
         "   - Query indexes are maintained to optimize the most\n"
         "     frequent access patterns.\n"
         "Final integration testing verifies all layers function\n"
         "together correctly."))
       (text-expected
        ;; format-next-line: off
        (concat
         "The system architecture has three main layers with distinct responsibilities.\n"
         "   The presentation layer handles user interaction and rendering of visual components."
         " It communicates with the service layer through well defined interfaces.\n"
         "      Configuration is loaded at startup from multiple sources including environment variables.\n"
         "   The service layer coordinates business logic:\n"
         "   - Authentication validates user credentials against the identity provider and issues session tokens.\n"
         "   - Authorization checks role-based permissions using configurable access control policies.\n"
         "      Database connections use a pooling strategy:\n"
         "      - Read replicas handle all SELECT queries to improve overall throughput.\n"
         "      - The write primary processes INSERT and UPDATE operations exclusively.\n"
         "      - Connection limits are enforced per application tier to prevent exhaustion.\n"
         "   Error handling follows a consistent pattern across all layers.\n"
         "      Recoverable errors trigger retry with exponential backoff."
         " Fatal errors are logged and escalated to the monitoring system.\n"
         "   The data layer manages persistence:\n"
         "   - Schema migrations run automatically during each deployment using versioned scripts.\n"
         "   - Query indexes are maintained to optimize the most frequent access patterns.\n"
         "Final integration testing verifies all layers function together correctly."))
       (fill-column 1000)
       (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Fill-Column-Margin / Fill-Column-Target Tests

(ert-deftest fill-column-margin ()
  "Fill-column-margin reduces the effective fill column."
  (let ((text-initial "Alpha bravo. Charlie delta.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo.\n"
                 "Charlie delta."))
        (fill-column 30)
        (fancy-fill-paragraph-fill-column-margin 5)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-column-target ()
  "Fill-column-target biases the solver toward shorter lines."
  (let ((text-initial
         ;; format-next-line: off
         (concat "Alpha bravo charlie. Delta echo foxtrot."
                 " Golf hotel india. Juliet kilo lima."))
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo charlie. Delta echo foxtrot.\n"
                 "Golf hotel india. Juliet kilo lima."))
        (fill-column 60)
        (fancy-fill-paragraph-fill-column-target -20)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Idempotency Test

(ert-deftest fill-idempotent ()
  "Filling an already-filled paragraph produces identical output."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "The system architecture has three main layers.\n"
          "   The presentation layer handles user interaction and visual rendering.\n"
          "   - Authentication validates user credentials against providers.\n"
          "   - Authorization checks role-based access control permissions.\n"
          "Final integration testing verifies all layers function together."))
        (fill-column 55)
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (let ((first-fill (buffer-string)))
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal first-fill (buffer-string)))))))


;; ---------------------------------------------------------------------------
;; False Dot-Point Prevention (No Infix) Test

(ert-deftest fill-no-false-dot-points-without-infix ()
  "False dot-point prevention works even when infix delimiters are disabled."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "Alpha one - bravo two - charlie three - delta four"
          " - echo five - foxtrot six - golf seven - hotel eight."))
        (fill-column 30)
        (fancy-fill-paragraph-infix-delimiters nil)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (let ((wrapped (buffer-string)))
        (should-not (string-match-p "^[ \t]*- " wrapped))
        (let ((fill-column 1000))
          (fancy-fill-paragraph)
          (should (equal text-initial (buffer-string))))))))


;; ---------------------------------------------------------------------------
;; Infix Delimiter Variant Tests

(ert-deftest split-infix-double-dash ()
  "Double-dash infix delimiter splits correctly."
  (let ((text-initial "Hello -- world.")
        (text-expected '("Hello --" "world."))
        (fancy-fill-paragraph-infix-delimiters '(("--" . 50)))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-infix-slash ()
  "Slash infix delimiter splits correctly."
  (let ((text-initial "Hello / world.")
        (text-expected '("Hello /" "world."))
        (fancy-fill-paragraph-infix-delimiters '(("/" . 50)))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))

(ert-deftest split-infix-tilde ()
  "Tilde infix delimiter splits correctly."
  (let ((text-initial "Hello ~ world.")
        (text-expected '("Hello ~" "world."))
        (fancy-fill-paragraph-infix-delimiters '(("~" . 50)))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 0)))
    (should (equal text-expected (test-split-items text-initial)))))


;; ---------------------------------------------------------------------------
;; Fill-Column-Target Branch Tests

(ert-deftest fill-column-target-float ()
  "Float fill-column-target sets target as a fraction of fill-column."
  (let ((text-initial
         ;; format-next-line: off
         (concat "A short one. A much longer sentence here."
                 " Another short. Yet another long sentence here."))
        (text-expected
         ;; format-next-line: off
         (concat "A short one.\n"
                 "A much longer sentence here.\n"
                 "Another short. Yet another long sentence here."))
        (fill-column 50)
        (fancy-fill-paragraph-fill-column-target 0.5)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-column-target-positive ()
  "Positive integer fill-column-target is used directly as the target."
  (let ((text-initial "Red orange. Yellow green. Blue indigo. Violet white.")
        (text-expected
         ;; format-next-line: off
         (concat "Red orange. Yellow green.\n"
                 "Blue indigo. Violet white."))
        (fill-column 55)
        (fancy-fill-paragraph-fill-column-target 25)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Tab Prefix Test

(ert-deftest fill-prefix-tab ()
  "Tab-based fill-prefix uses column width, not character count."
  (let ((text-initial "\tAlpha bravo. Charlie delta.")
        (text-expected
         ;; format-next-line: off
         (concat "\tAlpha bravo.\n"
                 "\tCharlie delta."))
        (fill-prefix "\t")
        (tab-width 4)
        (fill-column 30)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Fill-Column-Margin Clamp Test

(ert-deftest fill-column-margin-clamp ()
  "Fill-column-margin larger than fill-column clamps to effective column 1."
  (let ((text-initial "Alpha bravo. Charlie delta.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo.\n"
                 "Charlie delta."))
        (fill-column 10)
        (fancy-fill-paragraph-fill-column-margin 15)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Empty / Whitespace Input Tests

(ert-deftest fill-empty-buffer ()
  "Filling an empty buffer does not error."
  (with-fancy-fill-paragraph-test ""
    (fancy-fill-paragraph)
    (should (equal "" (buffer-string)))))

(ert-deftest fill-whitespace-only ()
  "Filling whitespace-only text preserves the text."
  (let ((text-initial "   "))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-initial (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Double-Space Separator Width Test

(ert-deftest fill-double-space-forces-wrap ()
  "Double-space separator width causes wrapping when single space would fit."
  (let ((text-initial "Alpha bravo. Charlie delta.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha bravo.\n"
                 "Charlie delta."))
        (fill-column 27)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (fancy-fill-paragraph-sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Custom Dot-Point Prefix Test

(ert-deftest fill-dot-point-custom-prefix ()
  "Custom dot-point prefix is detected and filled correctly."
  (let ((text-initial
         ;; format-next-line: off
         (concat
          "* Alpha bravo charlie delta echo foxtrot.\n"
          "* Golf hotel india juliet."))
        (text-expected
         ;; format-next-line: off
         (concat
          "* Alpha bravo charlie\n"
          "  delta echo foxtrot.\n"
          "* Golf hotel india\n"
          "  juliet."))
        (fill-column 22)
        (fancy-fill-paragraph-dot-point-prefix (list "* "))
        (fancy-fill-paragraph-split-weights (list :period 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Oversized Word Test

(ert-deftest fill-oversized-word ()
  "A word longer than fill-column is placed on its own line."
  (let ((text-initial "Short supercalifragilisticexpialidocious end.")
        (text-expected
         ;; format-next-line: off
         (concat "Short\n"
                 "supercalifragilisticexpialidocious\n"
                 "end."))
        (fill-column 20)
        (fancy-fill-paragraph-split-weights (list :period 0 :space 1))
        (fancy-fill-paragraph-sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Syntax Bounds Tests

(ert-deftest fill-syntax-bounds-c-comments-wrap ()
  "Three separate C-style comments should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta echo foxtrot golf. */\n"
          "/* Hotel india juliet kilo lima mike november. */\n"
          "/* Oscar papa quebec romeo sierra tango uniform. */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo\n"
          " * foxtrot golf. */\n"
          "/* Hotel india juliet kilo lima mike\n"
          " * november. */\n"
          "/* Oscar papa quebec romeo sierra tango\n"
          " * uniform. */")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        ;; Fill each comment paragraph from the top.
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comments-unwrap ()
  "Three separate wrapped C-style comments should each unwrap independently."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot. */\n"
          "/* Golf hotel india juliet\n"
          " * kilo lima. */\n"
          "/* Mike november oscar papa\n"
          " * quebec romeo. */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo foxtrot. */\n"
          "/* Golf hotel india juliet kilo lima. */\n"
          "/* Mike november oscar papa quebec romeo. */")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comments-multiline-wrap ()
  "Three multi-line C-style comments with delimiters on own lines should each wrap."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/*\n"
          " * Alpha bravo charlie delta echo foxtrot golf.\n"
          " */\n"
          "/*\n"
          " * Hotel india juliet kilo lima mike november.\n"
          " */\n"
          "/*\n"
          " * Oscar papa quebec romeo sierra tango uniform.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " * Alpha bravo charlie delta echo\n"
          " * foxtrot golf.\n"
          " */\n"
          "/*\n"
          " * Hotel india juliet kilo lima mike\n"
          " * november.\n"
          " */\n"
          "/*\n"
          " * Oscar papa quebec romeo sierra tango\n"
          " * uniform.\n"
          " */")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comments-multiline-unwrap ()
  "Three multi-line C-style comments with delimiters on own lines should each unwrap."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/*\n"
          " * Alpha bravo charlie\n"
          " * delta echo foxtrot.\n"
          " */\n"
          "/*\n"
          " * Golf hotel india\n"
          " * juliet kilo lima.\n"
          " */\n"
          "/*\n"
          " * Mike november oscar\n"
          " * papa quebec romeo.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " * Alpha bravo charlie delta echo foxtrot.\n"
          " */\n"
          "/*\n"
          " * Golf hotel india juliet kilo lima.\n"
          " */\n"
          "/*\n"
          " * Mike november oscar papa quebec romeo.\n"
          " */")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comments-dot-points-wrap ()
  "Indented C block comment with dot-points should wrap preserving dot-point structure."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "void func() {\n"
          "  /* The settings pass includes:\n"
          "   * - Background-mode assignment (#Global.background), "
          "checked by other subsystems which may be skipped in background mode.\n"
          "   * - The animation player may be launched which takes over argument passing, " ; wrap.
          "initializes the sub-systems it needs which have not yet been started. " ; wrap.
          "The animation player will call `exit(..)` too, " ; wrap.
          "so code after this call never runs when it's invoked.\n"
          "   * - All the `--debug-*` flags.\n"
          "   */\n"
          "  int x = 1;\n"
          "}\n"))
        (text-expected
         (concat
          "void func() {\n"
          "  /* The settings pass includes:\n"
          "   * - Background-mode assignment (#Global.background),\n"
          "   *   checked by other subsystems which may be skipped in background mode.\n"
          "   * - The animation player may be launched which takes over argument passing,\n"
          "   *   initializes the sub-systems it needs which have not yet been started.\n"
          "   *   The animation player will call `exit(..)` too,\n"
          "   *   so code after this call never runs when it's invoked.\n"
          "   * - All the `--debug-*` flags.\n"
          "   */\n"
          "  int x = 1;\n"
          "}\n")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        ;; Position point inside the comment body, not at the start.
        ;; This tests the `syntax-ppss' fallback in `--syntax-regions'.
        (forward-line 3)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comments-dot-points-unwrap ()
  "Indented C block comment with dot-points should unwrap preserving dot-point structure."
  (let ((fill-column 120)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "void func() {\n"
          "  /* The settings pass includes:\n"
          "   * - Background-mode assignment (#Global.background),\n"
          "   *   checked by other subsystems which may be skipped in background mode.\n"
          "   * - The animation player may be launched which takes over argument passing,\n"
          "   *   initializes the sub-systems it needs which have not yet been started.\n"
          "   *   The animation player will call `exit(..)` too,\n"
          "   *   so code after this call never runs when it's invoked.\n"
          "   * - All the `--debug-*` flags.\n"
          "   */\n"
          "  int x = 1;\n"
          "}\n"))
        (text-expected
         (concat
          "void func() {\n"
          "  /* The settings pass includes:\n"
          "   * - Background-mode assignment (#Global.background),\n"
          "   *   checked by other subsystems which may be skipped in background mode.\n"
          "   * - The animation player may be launched which takes over argument passing,\n"
          "   *   initializes the sub-systems it needs which have not yet been started.\n"
          "   *   The animation player will call `exit(..)` too, " ; wrap.
          "so code after this call never runs when it's invoked.\n"
          "   * - All the `--debug-*` flags.\n"
          "   */\n"
          "  int x = 1;\n"
          "}\n")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        ;; Position point inside the comment body, not at the start.
        ;; This tests the `syntax-ppss' fallback in `*--syntax-regions'.
        (forward-line 3)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-xml-comments-wrap ()
  "Three inline XML comments should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "<!-- Alpha bravo charlie delta echo foxtrot golf. -->\n"
          "<!-- Hotel india juliet kilo lima mike november. -->\n"
          "<!-- Oscar papa quebec romeo sierra tango uniform. -->"))
        (text-expected
         (concat
          "<!-- Alpha bravo charlie delta echo\n"
          " !-- foxtrot golf. -->\n"
          "<!-- Hotel india juliet kilo lima mike\n"
          " !-- november. -->\n"
          "<!-- Oscar papa quebec romeo sierra\n"
          " !-- tango uniform. -->")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-xml-comments-unwrap ()
  "Three inline wrapped XML comments should each unwrap independently."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "<!-- Alpha bravo charlie\n"
          " !-- delta echo foxtrot. -->\n"
          "<!-- Golf hotel india\n"
          " !-- juliet kilo lima. -->\n"
          "<!-- Mike november oscar\n"
          " !-- papa quebec romeo. -->"))
        (text-expected
         (concat
          "<!-- Alpha bravo charlie delta echo foxtrot. -->\n"
          "<!-- Golf hotel india juliet kilo lima. -->\n"
          "<!-- Mike november oscar papa quebec romeo. -->")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-xml-comments-multiline-wrap ()
  "Three multi-line XML comments with delimiters on own lines should each wrap."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "<!--\n"
          "Alpha bravo charlie delta echo foxtrot golf.\n"
          "-->\n"
          "<!--\n"
          "Hotel india juliet kilo lima mike november.\n"
          "-->\n"
          "<!--\n"
          "Oscar papa quebec romeo sierra tango uniform.\n"
          "-->"))
        (text-expected
         (concat
          "<!--\n"
          "Alpha bravo charlie delta echo foxtrot\n"
          "golf.\n"
          "-->\n"
          "<!--\n"
          "Hotel india juliet kilo lima mike\n"
          "november.\n"
          "-->\n"
          "<!--\n"
          "Oscar papa quebec romeo sierra tango\n"
          "uniform.\n"
          "-->")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-xml-comments-multiline-unwrap ()
  "Three multi-line XML comments with delimiters on own lines should each unwrap."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "<!--\n"
          "Alpha bravo charlie\n"
          "delta echo foxtrot.\n"
          "-->\n"
          "<!--\n"
          "Golf hotel india\n"
          "juliet kilo lima.\n"
          "-->\n"
          "<!--\n"
          "Mike november oscar\n"
          "papa quebec romeo.\n"
          "-->"))
        (text-expected
         (concat
          "<!--\n"
          "Alpha bravo charlie delta echo foxtrot.\n"
          "-->\n"
          "<!--\n"
          "Golf hotel india juliet kilo lima.\n"
          "-->\n"
          "<!--\n"
          "Mike november oscar papa quebec romeo.\n"
          "-->")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-strings-wrap ()
  "Three triple-single-quoted Python strings should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "'''\n"
          "Alpha bravo charlie delta echo foxtrot golf.\n"
          "'''\n"
          "'''\n"
          "Hotel india juliet kilo lima mike november.\n"
          "'''\n"
          "'''\n"
          "Oscar papa quebec romeo sierra tango uniform.\n"
          "'''"))
        (text-expected
         (concat
          "'''\n"
          "Alpha bravo charlie delta echo foxtrot\n"
          "golf.\n"
          "'''\n"
          "'''\n"
          "Hotel india juliet kilo lima mike\n"
          "november.\n"
          "'''\n"
          "'''\n"
          "Oscar papa quebec romeo sierra tango\n"
          "uniform.\n"
          "'''")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-strings-unwrap ()
  "Three triple-double-quoted Python strings should each unwrap independently."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "\"\"\"\n"
          "Alpha bravo charlie delta\n"
          "echo foxtrot golf.\n"
          "\"\"\"\n"
          "\"\"\"\n"
          "Hotel india juliet kilo\n"
          "lima mike november.\n"
          "\"\"\"\n"
          "\"\"\"\n"
          "Oscar papa quebec romeo\n"
          "sierra tango uniform.\n"
          "\"\"\""))
        (text-expected
         (concat
          "\"\"\"\n"
          "Alpha bravo charlie delta echo foxtrot golf.\n"
          "\"\"\"\n"
          "\"\"\"\n"
          "Hotel india juliet kilo lima mike november.\n"
          "\"\"\"\n"
          "\"\"\"\n"
          "Oscar papa quebec romeo sierra tango uniform.\n"
          "\"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-strings-inline-wrap ()
  "Three inline triple-single-quoted Python strings should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "'''Alpha bravo charlie delta echo foxtrot golf.'''\n"
          "'''Hotel india juliet kilo lima mike november.'''\n"
          "'''Oscar papa quebec romeo sierra tango uniform.'''"))
        (text-expected
         (concat
          "'''Alpha bravo charlie delta echo\n"
          "foxtrot golf.'''\n"
          "'''Hotel india juliet kilo lima mike\n"
          "november.'''\n"
          "'''Oscar papa quebec romeo sierra tango\n"
          "uniform.'''")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-strings-inline-unwrap ()
  "Three inline triple-double-quoted Python strings should each unwrap independently."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "\"\"\"Alpha bravo charlie delta\n"
          "echo foxtrot golf.\"\"\"\n"
          "\"\"\"Hotel india juliet kilo\n"
          "lima mike november.\"\"\"\n"
          "\"\"\"Oscar papa quebec romeo\n"
          "sierra tango uniform.\"\"\""))
        (text-expected
         (concat
          "\"\"\"Alpha bravo charlie delta echo foxtrot golf.\"\"\"\n"
          "\"\"\"Hotel india juliet kilo lima mike november.\"\"\"\n"
          "\"\"\"Oscar papa quebec romeo sierra tango uniform.\"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-strings-multi-paragraph ()
  "Python triple-quoted string with blank line should preserve separate paragraphs."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"\n"
          "    Generic Class,\n"
          "    can be used for any toolbar.\n"
          "\n"
          "    AB.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"\n"
          "    Generic Class, can be used for any toolbar.\n"
          "\n"
          "    AB.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 10))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-wrap ()
  "Two Python comment paragraphs separated by a blank line should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "# Alpha bravo charlie delta echo foxtrot golf.\n"
          "# Hotel india juliet kilo lima mike november.\n"
          "\n"
          "# Oscar papa quebec romeo sierra tango uniform.\n"
          "# Victor whiskey xray yankee zulu alpha bravo."))
        (text-expected
         (concat
          "# Alpha bravo charlie delta echo foxtrot\n"
          "# golf. Hotel india juliet kilo lima\n"
          "# mike november.\n"
          "\n"
          "# Oscar papa quebec romeo sierra tango\n"
          "# uniform. Victor whiskey xray yankee\n"
          "# zulu alpha bravo.")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (goto-char (point-max))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-unwrap ()
  "Two wrapped Python comment paragraphs separated by a blank line should each unwrap independently."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "# Alpha bravo charlie\n"
          "# delta echo foxtrot golf.\n"
          "# Hotel india juliet kilo\n"
          "# lima mike november.\n"
          "\n"
          "# Oscar papa quebec\n"
          "# romeo sierra tango uniform.\n"
          "# Victor whiskey xray\n"
          "# yankee zulu alpha bravo."))
        (text-expected
         (concat
          "# Alpha bravo charlie delta echo foxtrot golf.\n"
          "# Hotel india juliet kilo lima mike november.\n"
          "\n"
          "# Oscar papa quebec romeo sierra tango uniform.\n"
          "# Victor whiskey xray yankee zulu alpha bravo.")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (goto-char (point-max))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-comments-wrap ()
  "C-mode inline block comments should wrap using `comment-continue'."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta echo foxtrot golf. */\n"
          "/* Hotel india juliet kilo lima mike november. */\n"
          "/* Oscar papa quebec romeo sierra tango uniform. */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo\n"
          " * foxtrot golf. */\n"
          "/* Hotel india juliet kilo lima mike\n"
          " * november. */\n"
          "/* Oscar papa quebec romeo sierra tango\n"
          " * uniform. */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-comments-unwrap ()
  "C-mode inline wrapped block comments should unwrap using `comment-continue'."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot. */\n"
          "/* Golf hotel india juliet\n"
          " * kilo lima. */\n"
          "/* Mike november oscar papa\n"
          " * quebec romeo. */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo foxtrot. */\n"
          "/* Golf hotel india juliet kilo lima. */\n"
          "/* Mike november oscar papa quebec romeo. */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-elisp-comments-wrap ()
  "Emacs Lisp semicolon comments should pass through to normal fill."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          ";; Alpha bravo charlie delta echo foxtrot golf.\n"
          ";; Hotel india juliet kilo lima mike november.\n"
          "\n"
          ";; Oscar papa quebec romeo sierra tango uniform.\n"
          ";; Victor whiskey xray yankee zulu alpha bravo."))
        (text-expected
         (concat
          ";; Alpha bravo charlie delta echo\n"
          ";; foxtrot golf. Hotel india juliet kilo\n"
          ";; lima mike november.\n"
          "\n"
          ";; Oscar papa quebec romeo sierra tango\n"
          ";; uniform. Victor whiskey xray yankee\n"
          ";; zulu alpha bravo.")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (goto-char (point-max))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-elisp-comments-unwrap ()
  "Wrapped Emacs Lisp semicolon comments should pass through to normal unwrap."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          ";; Alpha bravo charlie\n"
          ";; delta echo foxtrot golf.\n"
          ";; Hotel india juliet kilo\n"
          ";; lima mike november.\n"
          "\n"
          ";; Oscar papa quebec\n"
          ";; romeo sierra tango uniform.\n"
          ";; Victor whiskey xray\n"
          ";; yankee zulu alpha bravo."))
        (text-expected
         (concat
          ";; Alpha bravo charlie delta echo foxtrot golf.\n"
          ";; Hotel india juliet kilo lima mike november.\n"
          "\n"
          ";; Oscar papa quebec romeo sierra tango uniform.\n"
          ";; Victor whiskey xray yankee zulu alpha bravo.")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (goto-char (point-max))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-javadoc-inline-wrap ()
  "C-mode Javadoc inline comment with `/**' opener should preserve the opener."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/** AAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBBBB"
          " CCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDD"
          " EEEEEEEEEEEEEEEEEEE.\n"
          " */"))
        (text-expected
         (concat
          "/** AAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBBBB\n"
          " * CCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDD\n"
          " * EEEEEEEEEEEEEEEEEEE.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 5))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-star-dot-points ()
  "C-mode block comment with `*' dot-point list should fill each item independently."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/*\n"
          " * Overview of the algorithm:\n"
          " *\n"
          " * * Alpha bravo charlie\n"
          " *   delta echo foxtrot\n"
          " *   golf hotel india.\n"
          " * * Juliet kilo lima mike\n"
          " *   november oscar papa\n"
          " *   quebec romeo sierra.\n"
          " * * Tango uniform victor\n"
          " *   whiskey xray yankee\n"
          " *   zulu.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " * Overview of the algorithm:\n"
          " *\n"
          " * * Alpha bravo charlie delta echo foxtrot golf\n"
          " *   hotel india.\n"
          " * * Juliet kilo lima mike november oscar papa\n"
          " *   quebec romeo sierra.\n"
          " * * Tango uniform victor whiskey xray yankee\n"
          " *   zulu.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 5))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-dash-prefix ()
  "C-mode block comment with alternative `c-block-comment-prefix' set to dash."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "/*\n"
          " - Alpha bravo charlie delta echo foxtrot golf hotel.\n"
          " -\n"
          " - - Juliet kilo lima mike november oscar papa quebec.\n"
          " - - Romeo sierra tango uniform victor whiskey xray.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " - Alpha bravo charlie delta echo\n"
          " - foxtrot golf hotel.\n"
          " -\n"
          " - - Juliet kilo lima mike november\n"
          " -   oscar papa quebec.\n"
          " - - Romeo sierra tango uniform victor\n"
          " -   whiskey xray.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t)
            (c-block-comment-prefix "- "))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 5))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-multi-paragraph ()
  "C-mode block comment with blank `*' lines should preserve separate paragraphs."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (text-initial
         (concat
          "    /* Refresh so the block is recreated with the region visible.\n"
          "     *\n"
          "     * Without this, `block_begin` sets #BLOCK_LOOP before the region is shown,\n"
          "     * causing `template_popup_confirm` to skip attaching its close callback\n"
          "     * (since it assumes #BLOCK_LOOP menus close via `menuretval`).\n"
          "     * With #BLOCK_KEEP_OPEN set this doesn't happen.\n"
          "     *\n"
          "     * An alternative would be to ensure the popup state always matches\n"
          "     * the initial state, however this ends up involving assumptions\n"
          "     * which may not hold in *every* case,\n"
          "     * so triggering a refresh is a reasonable solution.\n"
          "     * A refresh also occurs when moving the cursor between buttons,\n"
          "     * so it's expected\n"
          "     * to be fast. see #155320.\n"
          "     */"))
        (text-expected
         (concat
          "    /* Refresh so the block is recreated with the region visible.\n"
          "     *\n"
          "     * Without this, `block_begin` sets #BLOCK_LOOP before the region is shown,\n"
          "     * causing `template_popup_confirm` to skip attaching its close callback\n"
          "     * (since it assumes #BLOCK_LOOP menus close via `menuretval`).\n"
          "     * With #BLOCK_KEEP_OPEN set this doesn't happen.\n"
          "     *\n"
          "     * An alternative would be to ensure the popup state always matches the\n"
          "     * initial state, however this ends up involving assumptions which may not\n"
          "     * hold in *every* case, so triggering a refresh is a reasonable solution.\n"
          "     * A refresh also occurs when moving the cursor between buttons,\n"
          "     * so it's expected to be fast. see #155320.\n"
          "     */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 6))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-markdown-numbered-list ()
  "Markdown numbered list with multi-digit numbers should fill each item independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-dot-point-prefix (list "- " '(:regexp . "[0-9]+\\. ")))
        (text-initial
         (concat
          "9. Alpha bravo charlie\n"
          "   delta echo foxtrot.\n"
          "10. Golf hotel india\n"
          "    juliet kilo lima.\n"
          "11. Mike november oscar\n"
          "    papa quebec romeo."))
        (text-expected
         (concat
          "9. Alpha bravo charlie delta echo\n"
          "   foxtrot.\n"
          "10. Golf hotel india juliet kilo lima.\n"
          "11. Mike november oscar papa quebec\n"
          "    romeo.")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char (point-min))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-markdown-numbered-list-extreme-width ()
  "Numbered list alternating short and long prefixes should use correct indent per item."
  ;; "1. " = 3 chars (sub-fc 69), "10000000000000000000000000000000000. " = 37 chars (sub-fc 35).
  ;; Alternating prefix widths verify each item computes its own continuation indent.
  (let ((fill-column 72)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-dot-point-prefix (list '(:regexp . "[0-9]+\\. ")))
        (text-initial
         (concat
          "1. Alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo lima mike .\n"
          "10000000000000000000000000000000000. Alpha bravo charlie delta echo foxtrot golf.\n"
          "                                     Kilo lima mike.\n"
          "2. Quebec romeo sierra tango uniform victor whiskey xray yankee zulu alpha bravo.\n"
          "10000000000000000000000000000000001. Foxtrot golf hotel india juliet kilo lima mike.\n"
          "                                     Papa quebec romeo.\n"
          "3. Sierra tango uniform victor whiskey xray yankee zulu alpha bravo charlie delta .\n"
          "10000000000000000000000000000000002. Hotel india juliet kilo lima mike november.\n"
          "                                     Romeo sierra tango."))
        (text-expected
         (concat
          "1. Alpha bravo charlie delta echo foxtrot golf hotel india juliet kilo\n"
          "   lima mike .\n"
          "10000000000000000000000000000000000. Alpha bravo charlie delta echo\n"
          "                                     foxtrot golf. Kilo lima mike.\n"
          "2. Quebec romeo sierra tango uniform victor whiskey xray yankee zulu\n"
          "   alpha bravo.\n"
          "10000000000000000000000000000000001. Foxtrot golf hotel india juliet\n"
          "                                     kilo lima mike. Papa quebec romeo.\n"
          "3. Sierra tango uniform victor whiskey xray yankee zulu alpha bravo\n"
          "   charlie delta .\n"
          "10000000000000000000000000000000002. Hotel india juliet kilo lima mike\n"
          "                                     november. Romeo sierra tango.")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char (point-min))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-numbered-list ()
  "Numbered list inside a C block comment should fill each item independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-dot-point-prefix (list "- " '(:regexp . "[0-9]+\\. ")))
        (text-initial
         (concat
          "/*\n"
          " * Steps:\n"
          " *\n"
          " * 1. Alpha bravo charlie delta echo foxtrot golf.\n"
          " * 2. Hotel india juliet kilo lima mike november.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " * Steps:\n"
          " *\n"
          " * 1. Alpha bravo charlie delta echo\n"
          " *    foxtrot golf.\n"
          " * 2. Hotel india juliet kilo lima mike\n"
          " *    november.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (+ (point-min) 5))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-mixed-literal-and-regexp-dot-points ()
  "Literal `- ' and regexp numbered dot-points in the same text."
  (let ((fill-column 40)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-dot-point-prefix (list "- " '(:regexp . "[0-9]+\\. ")))
        (text-initial
         (concat
          "- Alpha bravo charlie delta echo foxtrot.\n"
          "1. Golf hotel india juliet kilo lima mike."))
        (text-expected
         (concat
          "- Alpha bravo charlie delta echo\n"
          "  foxtrot.\n"
          "1. Golf hotel india juliet kilo lima\n"
          "   mike.")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char (point-min))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-regexp-dot-point-no-match-without-space ()
  "Text like `1.noSpace' should not match the numbered list regexp."
  (let ((fill-column 30)
        (fancy-fill-paragraph-sentence-end-double-space nil)
        (fancy-fill-paragraph-dot-point-prefix (list '(:regexp . "[0-9]+\\. ")))
        (text-initial "1.noSpace should not match and 2.this also not.")
        (text-expected
         ;; format-next-line: off
         (concat
          "1.noSpace should not match and\n"
          "2.this also not.")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char (point-min))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; fancy-fill-paragraph-tests.el ends here
