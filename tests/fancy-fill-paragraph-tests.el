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

(defun should-fill-again-unchanged ()
  "Fill again from point, expecting the buffer and point to stay as they are.
Call after a fill, point having followed its text, so it is still
within the paragraph just filled."
  (let ((text-once (buffer-string))
        (point-once (point)))
    (fancy-fill-paragraph)
    (should (equal text-once (buffer-string)))
    (should (equal point-once (point)))))

(defun should-fill-same-from-every-position (text-initial line-index text-expected)
  "Fill TEXT-INITIAL from every position on LINE-INDEX, expecting TEXT-EXPECTED.
Filling behaves as if the cursor were at the last non-blank character
of its line, so where point sits on it must never change the result.
Positions are stepped rather than columns, so a tab counts once.
A second fill must leave the result as it is.
Call inside a buffer whose major mode is already set."
  (buffer-reset-text text-initial)
  (goto-char (point-min))
  (forward-line line-index)
  (let ((line-beg (point))
        (line-end (pos-eol)))
    (dotimes (offset (1+ (- line-end line-beg)))
      (buffer-reset-text text-initial)
      (goto-char (+ line-beg offset))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string)))
      (should-fill-again-unchanged))))

(defun should-fill-keep-point (text-initial text-expected)
  "Fill TEXT-INITIAL at its `|', expecting TEXT-EXPECTED with `|' at point.
The `|' marks the cursor and is removed before filling, so point must
stay with the text beside it rather than keep its buffer offset.
A second fill must leave the result as it is.
Call inside a buffer whose major mode is already set."
  (buffer-reset-text text-initial)
  (goto-char (point-min))
  (search-forward "|")
  (delete-char -1)
  (fancy-fill-paragraph)
  ;; Check the result before filling again, so a wrong fill reports as such.
  (insert "|")
  (should (equal text-expected (buffer-string)))
  (delete-char -1)
  (should-fill-again-unchanged))

(defun should-fill-selection (text-initial line-index text-expected)
  "Fill TEXT-INITIAL selected from LINE-INDEX to its end, expecting TEXT-EXPECTED.
Filling the same selection again must leave the result as it is.
Call inside a buffer whose major mode is already set."
  (buffer-reset-text text-initial)
  (let ((transient-mark-mode t))
    (dotimes (_ 2)
      (goto-char (point-min))
      (forward-line line-index)
      (set-mark (point))
      (goto-char (point-max))
      (activate-mark)
      (fancy-fill-paragraph)
      (deactivate-mark)
      (should (equal text-expected (buffer-string))))))

(defmacro with-advice (advice &rest body)
  "Execute BODY with ADVICE temporarily enabled.

Each advice is a triplet of (SYMBOL HOW FUNCTION),
see `advice-add' documentation."
  (declare (indent 1))
  (let ((advice-list advice)
        (body-let nil)
        (body-advice-add nil)
        (body-advice-remove nil)
        (item nil))
    (unless (listp advice-list)
      (error "Advice must be a list"))
    (cond
     ((null advice-list)
      (error "Advice must be a list containing at least one item"))
     (t
      (while (setq item (pop advice-list))
        (unless (and (listp item) (eq 3 (length item)))
          (error "Each advice must be a list of 3 items"))
        (let ((fn-sym (gensym))
              (fn-advise (pop item))
              (fn-advice-ty (pop item))
              (fn-body (pop item)))
          ;; Build the calls for each type.
          (push (list fn-sym fn-body) body-let)
          (push (list 'advice-add fn-advise fn-advice-ty fn-sym) body-advice-add)
          (push (list 'advice-remove fn-advise fn-sym) body-advice-remove)))
      (setq body-let (nreverse body-let))
      (setq body-advice-add (nreverse body-advice-add))

      ;; Compose the call.
      `(let ,body-let
         (unwind-protect
             (progn
               ,@body-advice-add
               ,@body)
           ,@body-advice-remove))))))

(defmacro with-forward-line-stall-guard (&rest body)
  "Run BODY and signal when repeated `forward-line' calls stop advancing."
  (declare (indent 0))
  `(let ((forward-line-stall-count 0))
     (with-advice (('forward-line
                    :around
                    (lambda (forward-line-original &optional arg)
                      (let ((before (point))
                            (step (or arg 1)))
                        (prog1 (funcall forward-line-original arg)
                          (cond
                           ((and (> step 0) (= (point) before))
                            (setq forward-line-stall-count (1+ forward-line-stall-count))
                            (when (> forward-line-stall-count 1)
                              (error "forward-line stopped advancing repeatedly")))
                           (t
                            (setq forward-line-stall-count 0))))))))
       ,@body)))

(defun test-split-items (text)
  "Split TEXT and return the items list."
  (plist-get (fancy-fill-paragraph--paragraph-to-items text) :items))

(defun test-split-and-solve (text fill-col &optional first-line-extra)
  "Split TEXT and solve with FILL-COL, returning result as a string.
FIRST-LINE-EXTRA is passed to the solver as is."
  (let* ((split-result (fancy-fill-paragraph--paragraph-to-items text))
         (items (plist-get split-result :items)))
    (mapconcat #'identity
               (fancy-fill-paragraph--solve
                items
                fill-col
                (plist-get split-result :break-weights)
                (plist-get split-result :seps)
                (plist-get split-result :sep-lens)
                fancy-fill-paragraph-blank-space-weight
                first-line-extra)
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

(ert-deftest fill-noop-python-indented-empty-string ()
  "Indented empty Python triple-quoted string should be unchanged.
The delimiter-only opener and closer lines must not be merged; that
would change the string's runtime value."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "x = (\n    \"\"\"\n    \"\"\"\n)\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 14)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-python-indented-blank-string ()
  "Indented Python triple-quoted string with blank line should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "x = (\n    \"\"\"\n\n    \"\"\"\n)\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 14)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-c-mode-indented-empty-comment ()
  "Indented empty C block comment should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "    /*\n     */\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 7)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-noop-c-mode-indented-blank-comment ()
  "Indented C block comment with only a blank `*' line should be unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "    /*\n     *\n     */\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 7)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))


(ert-deftest fill-syntax-bounds-c-comment-trailing-code-no-error ()
  "Code sharing a line after \"*/\" must not signal and must be untouched.
The comment region must end at the closer, not at end-of-line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "/* note */ x = 1;\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-trailing-code-untouched ()
  "Code sharing a line after a string closer must not be reflowed.
The string region must end at the closer, not at end-of-line; the
code's own spacing (here around \"+\") must survive filling from
inside the string."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "x = \"\"\"aaa\"\"\"  +  y\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 10)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-unterminated-string-region-no-scan-error ()
  "Active region in an unterminated string must not signal `scan-error'."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (text-expected "x = \"unterminated\nwords words words\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text "x = \"unterminated words words words\n")
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "unterminated")
        (set-mark (line-end-position))
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comment-trailing-code-region-no-error ()
  "Active region inside \"/* ... */ code;\" must not signal or modify code.
The region path dispatches through the same inline fill as the
point path and crashed the same way."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "/* note */ x = 1;\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 4)
        (set-mark 9)
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comment-trailing-code-multiline-body-fills ()
  "A multi-line block comment with code after \"*/\" still fills its body.
Trailing code only skips the inline fill; the multi-line body fill
excludes the closer line, so it must keep working and the closer
line (with its code) must be untouched."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         (concat "/* aaa bbb.\n" " * ccc ddd eee fff ggg hhh iii jjj.\n" " * kkk */ x = 1;\n"))
        (text-expected
         (concat "/* aaa bbb. ccc ddd eee fff\n" " * ggg hhh iii jjj.\n" " * kkk */ x = 1;\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 20)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-cursor-column-comment-line-elisp ()
  "Any column of a comment line fills the comment, indentation included.
Leading blank-space is outside the syntax region, so probing it
reported code and the preceding code line was reflowed as prose."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "  (code-before)\n" "  ;; aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n" "  ;; kkk lll.\n"))
        (text-expected
         (concat
          "  (code-before)\n"
          "  ;; aaa bbb ccc ddd eee fff ggg hhh iii\n"
          "  ;; jjj. kkk lll.\n")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial 2 text-expected)))))

(ert-deftest fill-syntax-bounds-cursor-column-comment-line-python ()
  "Any column of a Python comment line fills the comment."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat "def f():\n" "    x = 1\n" "    # aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n"))
        (text-expected
         (concat
          "def f():\n"
          "    x = 1\n"
          "    # aaa bbb ccc ddd eee fff ggg hhh\n"
          "    # iii jjj.\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 2 text-expected)))))

(ert-deftest fill-syntax-bounds-cursor-column-two-char-comment-opener ()
  "Any column of a \"//\" comment line fills the comment.
Probing only one character ahead lands on the second \"/\", which
`syntax-ppss' still reports as code, so the comment was missed."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "  code_before();\n"
          "  // aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n"
          "  // kkk lll.\n"))
        (text-expected
         (concat
          "  code_before();\n"
          "  // aaa bbb ccc ddd eee fff ggg hhh iii\n"
          "  // jjj. kkk lll.\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial 2 text-expected)))))

(ert-deftest fill-syntax-bounds-cursor-column-block-comment-opener ()
  "Any column of a \"/* ... */\" line fills the comment."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial (concat "code_before();\n" "/* aaa bbb ccc ddd eee fff ggg hhh. */\n"))
        (text-expected
         (concat "code_before();\n" "/* aaa bbb ccc ddd eee fff ggg\n" " * hhh. */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-cursor-column-trailing-comment ()
  "Any column of a line ending in a comment fills that comment.
The cursor is taken to be at the last non-blank character, which
is comment text even when the line starts with code."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat "x = 1  # aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n" "       # kkk lll.\n"))
        (text-expected
         (concat "x = 1  # aaa bbb ccc ddd eee fff ggg hhh\n" "       # iii jjj. kkk lll.\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-code-line-beside-comment-untouched ()
  "A code line in a paragraph holding a comment must never be reflowed.
Falling back to a plain fill merged the code into the comment text."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 50)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "int main(void) {\n"
          "  int x = 1;\n"
          "  // A fairly long comment line that will need wrapping.\n"
          "  return x;\n"
          "}\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        ;; Lines 0, 1, 3 and 4 are code, they are left alone.
        (dolist (line-index (list 0 1 3 4))
          (should-fill-same-from-every-position text-initial line-index text-initial))))))

(ert-deftest fill-plain-code-paragraph-still-fills ()
  "A paragraph with no comment or string still falls back to a plain fill.
The comment guard must not disable filling for code-only text."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial "aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk\n")
        (text-expected "aaa bbb ccc ddd eee fff ggg\nhhh iii jjj kkk\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-quoted-prose-outside-prog-mode-still-fills ()
  "Quoted prose outside a programming mode still fills.
Double quotes are string delimiters in the standard syntax table
and `org-mode' also sets `comment-start', so the guard against
filling code beside a string must not reach prose modes.
`conf-mode' is not a prose mode, see
`fill-syntax-bounds-conf-mode-keys-beside-strings-untouched'."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial "aaa \"bbb ccc\" ddd eee fff ggg hhh iii jjj\n")
        (text-expected "aaa \"bbb ccc\"\nddd eee fff ggg hhh iii jjj\n"))
    (dolist (mode (list #'text-mode #'org-mode #'fundamental-mode))
      (with-temp-buffer
        (funcall mode)
        (let ((inhibit-message t))
          (buffer-reset-text text-initial)
          (goto-char (point-min))
          (fancy-fill-paragraph)
          (should (equal text-expected (buffer-string))))))))

(ert-deftest fill-quoted-prose-across-lines-outside-prog-mode-fills-as-prose ()
  "Quoted prose across lines outside a programming mode fills as prose.
With the cursor in the quote, it was filled as a string with code after
its closer, so only the opener line wrapped, while from the closer line
the paragraph was filled.  `text-mode' gives a double quote punctuation
syntax, it's included for contrast."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (mode (list #'text-mode #'org-mode))
      (with-temp-buffer
        (funcall mode)
        (let ((inhibit-message t))
          (dolist (case
                   (list
                    (list
                     "He said \"aaa bbb ccc ddd eee fff\nggg\" then left the room.\n"
                     "He said \"aaa bbb ccc ddd eee\nfff ggg\" then left the room.\n")
                    (list
                     "He said \"aaa bbb ccc ddd eee fff\nggg hhh\" iii.\n"
                     "He said \"aaa bbb ccc ddd eee\nfff ggg hhh\" iii.\n")))
            (dolist (line-index (list 0 1))
              (should-fill-same-from-every-position (nth 0 case) line-index (nth 1 case)))
            (should-fill-selection (nth 0 case) 0 (nth 1 case))))))))

(define-derived-mode
 fancy-fill-paragraph-test-data-mode text-mode "Data"
 "A data mode derived from `text-mode', a double quote opening a string.
A stand-in for modes such as `yaml-ts-mode', whose grammar may be missing."
 (modify-syntax-entry ?\" "\""))

(defun fancy-fill-paragraph-test-quintuple-syntax-propertize (start end)
  "Give string fence syntax to one quote of each five-quote delimiter in START..END.
The opener's last quote and the closer's first, the others reading as
empty strings, as `python-mode' marks a triple quote."
  (goto-char start)
  (while (re-search-forward "\"\\{5\\}" end t)
    (let ((run-beg (match-beginning 0))
          (run-end (match-end 0)))
      (cond
       ((nth 3 (save-excursion (syntax-ppss run-beg)))
        (put-text-property run-beg (1+ run-beg) 'syntax-table (string-to-syntax "|")))
       (t
        (put-text-property (1- run-end) run-end 'syntax-table (string-to-syntax "|")))))))

(define-derived-mode
 fancy-fill-paragraph-test-quintuple-mode prog-mode "Quintuple"
 "A mode whose long strings are delimited by five quotes.
Marked as `python-mode' marks a triple quote, with more quotes besides
the fence."
 (setq-local syntax-propertize-function #'fancy-fill-paragraph-test-quintuple-syntax-propertize))

(ert-deftest fill-syntax-bounds-string-delimiter-any-length ()
  "A repeated delimiter of any length fills as a triple quote does.
Its length is read from the runs of its character at each end, the
quotes beside the one with string syntax reading as empty strings, as
`python-mode' marks a triple quote."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (fancy-fill-paragraph-test-quintuple-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position
         "x = \"\"\"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\"\"\"\"\n"
         0
         "x = \"\"\"\"\"aaa bbb ccc ddd eee\nfff ggg hhh\"\"\"\"\"\n")
        (let ((text-initial
               "x = \"\"\"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.\n\"\"\"\"\"\n")
              (text-expected
               "x = \"\"\"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii\njjj kkk.\n\"\"\"\"\"\n"))
          ;; From the closer line, its quotes are the string's.
          (dolist (line-index (list 2 3))
            (should-fill-same-from-every-position text-initial line-index text-expected)))))))

(ert-deftest fill-syntax-bounds-data-mode-keys-beside-strings-untouched ()
  "Keys beside a quoted value are never merged in a data mode.
Only programming and configuration modes took the text beside a string
for code, so a mode derived from `text-mode' (`yaml-ts-mode' for e.g.)
or `bibtex-mode' filled the whole paragraph as prose, merging its keys.
A string fills alone, as in a programming mode."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'fancy-fill-paragraph-test-data-mode
               20
               1
               "a: 1\nkey: \"aaa bbb ccc ddd eee fff ggg\n  hhh iii\"\n"
               "a: 1\nkey: \"aaa bbb ccc\n  ddd eee fff ggg\n  hhh iii\"\n")
              (list
               #'fancy-fill-paragraph-test-data-mode
               20
               0
               "a: 1\nkey: \"aaa bbb ccc ddd eee fff ggg\n  hhh iii\"\n"
               "a: 1\nkey: \"aaa bbb ccc ddd eee fff ggg\n  hhh iii\"\n")
              (list
               #'bibtex-mode 30 1
               (concat
                "@article{x,\n  title = \"aaa bbb ccc ddd eee fff ggg\n    hhh iii\",\n"
                "  year = 2020\n}\n")
               (concat
                "@article{x,\n  title = \"aaa bbb ccc ddd eee\n    fff ggg hhh iii\",\n"
                "  year = 2020\n}\n"))))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t)
              (fill-column (nth 1 case)))
          (should-fill-same-from-every-position (nth 3 case) (nth 2 case) (nth 4 case)))))))

(ert-deftest fill-syntax-bounds-prose-beside-comment-fills ()
  "A line holding a line comment is left as it is when prose around it fills.
The prose was filled with the comment as one paragraph, so in
`latex-mode' text joined after the comment was commented out.  The
lines around it fill on their own."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (fill-column 30))
    (with-temp-buffer
      (latex-mode)
      (let ((inhibit-message t))
        (dotimes (line-index 2)
          (should-fill-same-from-every-position
           "Aaa bbb ccc. % note\nDdd eee fff.\n" line-index "Aaa bbb ccc. % note\nDdd eee fff.\n"))
        (let ((text-initial
               ;; format-next-line: off
               (concat "Aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n"
                       "Kkk. % note\n"
                       "Lll mmm nnn ooo ppp qqq rrr sss ttt uuu.\n"
                       "Vvv www.\n"))
              (text-expected
               ;; format-next-line: off
               (concat "Aaa bbb ccc ddd eee fff ggg\n"
                       "hhh iii jjj.\n"
                       "Kkk. % note\n"
                       "Lll mmm nnn ooo ppp qqq rrr\n"
                       "sss ttt uuu. Vvv www.\n")))
          (should-fill-same-from-every-position text-initial 2 text-expected)
          (should-fill-same-from-every-position text-initial 0 text-expected)
          (should-fill-selection text-initial 0 text-expected))))))

(ert-deftest fill-syntax-bounds-prose-without-line-comment-unchanged ()
  "Prose without a line comment fills as one paragraph.
A preservation case for `fill-syntax-bounds-prose-beside-comment-fills':
an escaped `%' isn't a comment, nor is a `%' in `text-mode', prose in a
programming mode still fills, code beside a comment is still left alone,
and without syntax bounds the comment is joined as before."
  (let ((sentence-end-double-space nil)
        (fill-column 30))
    (dolist (case
             (list
              (list t #'latex-mode "Aaa bbb\nccc ddd.\n" "Aaa bbb ccc ddd.\n")
              (list t #'latex-mode "Aaa \\% bbb\nccc ddd.\n" "Aaa \\% bbb ccc ddd.\n")
              (list
               t #'text-mode "Aaa bbb. % note\nDdd eee fff.\n" "Aaa bbb. % note Ddd eee fff.\n")
              (list t #'c-mode "aaa bbb\nccc ddd\n" "aaa bbb ccc ddd\n")
              (list t #'c-mode "aaa bbb // note\nccc ddd\n" "aaa bbb // note\nccc ddd\n")
              (list t #'emacs-lisp-mode "(foo) ; note\n(bar)\n" "(foo) ; note\n(bar)\n")
              (list nil #'latex-mode "Aaa bbb. % note\nDdd eee.\n" "Aaa bbb. % note Ddd eee.\n")))
      (let ((fancy-fill-paragraph-syntax-bounds (nth 0 case)))
        (with-temp-buffer
          (funcall (nth 1 case))
          (let ((inhibit-message t))
            (dotimes (line-index 2)
              (should-fill-same-from-every-position (nth 2 case) line-index (nth 3 case)))))))))

(ert-deftest fill-syntax-bounds-conf-mode-keys-beside-strings-untouched ()
  "Keys and section headers beside a quoted value are never merged.
`conf-mode' was taken for prose, so a paragraph of them was filled into
one line, from any line but the string's.  Text beside a string is code
there, as in a programming mode.  The last case is a preservation case,
a comment still fills."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (mode (list #'conf-unix-mode #'conf-toml-mode #'conf-javaprop-mode))
      (with-temp-buffer
        (funcall mode)
        (let ((inhibit-message t))
          (dolist (text
                   (list
                    "[package]\nname = \"foo\"\nversion = \"1.0\"\n"
                    "[section]\nkey = \"aaa bbb ccc ddd eee fff ggg hhh iii\"\nother = 1\n"))
            (dotimes (line-index 3)
              (should-fill-same-from-every-position text line-index text))
            (should-fill-selection text 0 text))
          (should-fill-same-from-every-position
           "# aaa bbb ccc ddd eee fff ggg hhh iii jjj\nkey = \"x\"\n"
           0
           "# aaa bbb ccc ddd eee fff ggg\n# hhh iii jjj\nkey = \"x\"\n"))))))

(ert-deftest fill-syntax-bounds-conf-mode-keys-without-strings-untouched ()
  "Keys and section headers without a comment or string are never merged.
Only those beside a comment or string were left alone, others were
filled as prose into one line.  The last cases are preservation cases,
a comment still fills, by the cursor and by a selection, and a code
paragraph in a programming mode still falls back to a prose fill."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (mode (list #'conf-unix-mode #'conf-toml-mode #'conf-javaprop-mode))
      (with-temp-buffer
        (funcall mode)
        (let ((inhibit-message t)
              (text "[sec]\nkey = 1\nother = 2\n"))
          (dotimes (line-index 3)
            (should-fill-same-from-every-position text line-index text))
          (should-fill-selection text 0 text)
          ;; Preservation.
          (let ((text-initial "# aaa bbb ccc ddd eee fff ggg hhh iii jjj\n\n[sec]\nkey = 1\n")
                (text-expected "# aaa bbb ccc ddd eee fff ggg\n# hhh iii jjj\n\n[sec]\nkey = 1\n"))
            (should-fill-same-from-every-position text-initial 0 text-expected)
            (should-fill-selection text-initial 0 text-expected)))))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position
         "int x = 1;\nint y = 2;\n" 0 "int x = 1; int y = 2;\n")))))

(ert-deftest fill-syntax-bounds-python-line-comment-leading-code-untouched ()
  "Code sharing a line before a Python comment must not be reflowed."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "x = 1  # aaa\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 10)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comment-leading-code-untouched ()
  "Code before an aligned trailing C++ line comment must not be reflowed."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (sentence-end-double-space nil)
        (text-initial "int x;  // aaa\n        // bbb\n")
        (text-expected "int x;  // aaa bbb\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 27)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-line-comment-adjacent-leading-code-untouched ()
  "Adjacent trailing line comments must not delete intervening code."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text "x = 1  # aaa\ny = 2  # bbb\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char 22)
        (fancy-fill-paragraph)
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comment-tab-indentation-preserves-content ()
  "Tab-indented block comments must not strip body content while filling."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text-initial "  \t/* aaa bbb ccc ddd\n  \t * eee fff ggg hhh\n  \t */\n")
        (text-expected "  \t/* aaa bbb ccc ddd eee fff ggg hhh\n  \t */\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 8)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-comment-body-tab-indentation-preserves-content ()
  "Tab-indented block-comment body lines must not strip content while filling."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text-initial "  \t/*\n  \t * aaa bbb ccc ddd\n  \t * eee fff ggg hhh\n  \t */\n")
        (text-expected "  \t/*\n  \t * aaa bbb ccc ddd eee fff ggg hhh\n  \t */\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 11)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-tab-indentation-preserves-content ()
  "Tab-indented multi-line strings must not strip body content while filling."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (text-initial "  \t\"\"\"Aaa bbb\n  \tCcc ddd eee\n  \t\"\"\"\n")
        (text-expected "  \t\"\"\"Aaa bbb Ccc ddd eee\n  \t\"\"\"\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 8)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-unterminated-comment-still-fills ()
  "An unterminated comment is the normal state while typing one.
The guard against filling code beside a comment must not suppress the
fill when the range holds no code to merge into it.  It's filled as a
block comment, lines it wraps onto taking the continuation prefix."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo ppp\n")
        (text-expected
         (concat "/* aaa bbb ccc ddd eee fff ggg hhh iii\n" " * jjj kkk lll mmm nnn ooo ppp\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-apostrophe-in-code-still-fills ()
  "An apostrophe opens a string that never closes, it is not a string.
Treating it as one stopped code paragraphs holding an apostrophe from
filling at all."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "echo it doesn't matter aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll\n")
        (text-expected
         (concat "echo it doesn't matter aaa bbb ccc ddd\n" "eee fff ggg hhh iii jjj kkk lll\n")))
    (with-temp-buffer
      (sh-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-comment-at-eob-without-newline-fills ()
  "The last comment of a file without a trailing newline must fill.
The comment has no closing newline, which must not read as code beside
it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk")
        (text-expected (concat "// aaa bbb ccc ddd eee fff ggg hhh iii\n" "// jjj kkk")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-code-beside-comment-at-eob-untouched ()
  "Code beside a comment running to end-of-buffer must not be reflowed.
Same shape as a comment filling the whole paragraph, only the code
before it separates the two cases."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text "int x = 1;\n// aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk."))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (point-min))
        (should-not (fancy-fill-paragraph))
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-code-beside-string-untouched ()
  "Code beside a terminated string must not be reflowed.
Filling as prose splits the literal across a newline."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text "int x = 1;\nchar *s = \"aaa bbb ccc ddd eee fff ggg hhh\";\n"))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text)
        (goto-char (point-min))
        (should-not (fancy-fill-paragraph))
        (should (equal text (buffer-string)))))))

(ert-deftest fill-syntax-bounds-code-before-block-comment-prefix-excludes-code ()
  "Code before a block comment must not become the continuation prefix.
`fancy-fill-paragraph--extract-body-indent' takes everything up to the
opener column, so leading code is otherwise repeated on every wrapped
line.  Blank-space of the same width keeps the `*' aligned under the
opener, which is also the width the fill is measured against."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "foo(); /* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll */\n")
        (text-expected
         (concat "foo(); /* aaa bbb ccc ddd eee fff ggg\n" "        * hhh iii jjj kkk lll */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-code-before-block-comment-bare-opener ()
  "A two-line comment after code, its opener line holding no text, fills its text.
Filling it inserted a stray ` *' line above the code and joined the text
onto the opener.  It fills as a longer comment of the same shape does.
With text after the opener, the comment still fills."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   20
                   "x = 1; /*\n   aaa bbb ccc ddd eee fff ggg hhh */\n"
                   "x = 1; /*\n   aaa bbb ccc ddd\n   eee fff ggg hhh\n   */\n")
                  (list
                   70
                   "x = 1; /*\n   aaa bbb ccc ddd eee fff ggg hhh */\n"
                   "x = 1; /*\n   aaa bbb ccc ddd eee fff ggg hhh */\n")
                  ;; The longer layout, for contrast.
                  (list
                   20
                   "x = 1; /*\n   aaa bbb ccc ddd eee\n   fff ggg hhh */\n"
                   "x = 1; /*\n   aaa bbb ccc ddd\n   eee fff ggg hhh\n   */\n")))
          ;; A selection leaves a comment after code alone, so only the cursor.
          (let ((fill-column (nth 0 case)))
            (dolist (line-index (list 0 1))
              (should-fill-same-from-every-position (nth 1 case) line-index (nth 2 case)))))
        (let ((fill-column 24))
          (dolist (line-index (list 0 1))
            (should-fill-same-from-every-position
             "x = 1; /* aaa\n   bbb ccc ddd eee fff ggg hhh */\n" line-index
             (concat
              "x = 1; /* aaa bbb ccc\n" "        * ddd eee fff\n" "        * ggg hhh */\n"))))))))

(ert-deftest fill-syntax-bounds-two-line-block-comment-bare-opener-fills ()
  "A two-line comment, its opener line holding no text, fills its text.
It was left unfilled, the closer line's text not joining the opener.
It fills as a longer comment of the same shape does, from the cursor or
a selection, as does a comment that never closes.  A closer line
without text to fill, or text that already fits, is left as it is."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (fill-column 20))
    (dolist (case
             (list
              (list
               #'c-mode
               "/*\n * aaa bbb ccc ddd eee fff ggg hhh */\n"
               "/*\n * aaa bbb ccc ddd\n * eee fff ggg hhh\n */\n")
              ;; The longer layout, for contrast.
              (list
               #'c-mode
               "/*\n * aaa bbb ccc ddd eee\n * fff ggg hhh */\n"
               "/*\n * aaa bbb ccc ddd\n * eee fff ggg hhh\n */\n")
              (list
               #'c-mode
               "  /*\n     aaa bbb ccc ddd eee fff ggg hhh */\n"
               "  /*\n     aaa bbb ccc ddd\n     eee fff ggg hhh\n     */\n")
              (list
               #'html-mode
               "<!--\naaa bbb ccc ddd eee fff ggg hhh -->\n"
               "<!--\naaa bbb ccc ddd eee\nfff ggg hhh -->\n")
              ;; Preservation: nothing to fill, or already filled.
              (list #'c-mode "/*\n */\n" "/*\n */\n")
              (list #'c-mode "/*\n * */\n" "/*\n * */\n")
              (list #'c-mode "/*\n x */\n" "/*\n x */\n")
              (list #'c-mode "/*\n * aaa bbb */\n" "/*\n * aaa bbb */\n")
              ;; A comment that never closes.
              (list
               #'c-mode
               "/*\n * aaa bbb ccc ddd eee fff ggg hhh"
               "/*\n * aaa bbb ccc ddd\n * eee fff ggg hhh")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (dolist (line-index (list 0 1))
            (should-fill-same-from-every-position (nth 1 case) line-index (nth 2 case)))
          (should-fill-selection (nth 1 case) 0 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-code-before-block-comment-fill-twice-keeps-text ()
  "Filling twice must not delete comment text or duplicate the code.
Merging the code into the comment on the first fill is a separate
defect, limited to modes not derived from `prog-mode'.  That first fill
takes the prose path, so the result still shifts once when the comment
filler takes over, only the text and the settled result are asserted."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 44)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "<p>AAA_ONE</p>\n"
          "<!-- aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo. -->\n"
          "<p>ZZZ_TWO</p>\n")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (let ((text-twice (buffer-string)))
          ;; No comment text may be dropped.
          (dolist (word
                   (list
                    "aaa"
                    "bbb"
                    "ccc"
                    "ddd"
                    "eee"
                    "fff"
                    "ggg"
                    "hhh"
                    "iii"
                    "jjj"
                    "kkk"
                    "lll"
                    "mmm"
                    "nnn"
                    "ooo."))
            (should (string-match-p (regexp-quote word) text-twice)))
          ;; The code must not be repeated onto a wrapped line.
          (should (equal 1 (how-many (regexp-quote "<p>AAA_ONE</p>") (point-min) (point-max))))
          (goto-char (point-min))
          (fancy-fill-paragraph)
          (should (equal text-twice (buffer-string))))))))

(ert-deftest fill-syntax-bounds-block-comment-continuation-prefix-preserved ()
  "A block comment on its own line keeps its indent and `*' continuation.
Rejecting a non-blank body indent must not disturb the ordinary case."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "  /* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn */\n")
        (text-expected
         (concat "  /* aaa bbb ccc ddd eee fff ggg hhh iii\n" "   * jjj kkk lll mmm nnn */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-block-comment-rule-line-keeps-text ()
  "A body line holding no letters must not be taken for a continuation prefix.
Cutting the prefix width from a rule line deletes it, only a line that
carries the prefix may be cut."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "int x = 1; /* aaa bbb ccc ddd eee fff ggg hhh iii jjj\n" "-------------------- */\n"))
        (text-expected
         (concat
          "int x = 1; /* aaa bbb ccc ddd eee\n"
          "            * fff ggg hhh iii jjj\n"
          "            * -------------------- */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        ;; Every position, the loss only showed with the cursor inside the
        ;; comment.  A cursor on the code never reproduced it.
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-block-comment-rule-line-survives-refill ()
  "Repeated fills must not shorten a rule line, from any cursor position.
Nor may they change the layout, the first fill settles it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "int x = 1; /* aaa bbb ccc ddd eee fff ggg hhh iii jjj\n" "-------------------- */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        ;; Filling from inside the comment is what lost the text, so step
        ;; every position rather than resting on `point-min'.
        (dotimes (offset (length text-initial))
          (buffer-reset-text text-initial)
          (goto-char (min (+ (point-min) offset) (point-max)))
          (fancy-fill-paragraph)
          (let ((text-once (buffer-string)))
            (dotimes (_ 2)
              (goto-char (min (+ (point-min) offset) (point-max)))
              (fancy-fill-paragraph)
              (should (equal text-once (buffer-string)))))
          (should (equal 20 (length (replace-regexp-in-string "[^-]" "" (buffer-string))))))))))

(ert-deftest fill-syntax-bounds-block-comment-body-rule-line-keeps-text ()
  "A rule line in a block comment body keeps every character.
The opener line carries text, so it is filled together with the body
and the body lines are stripped against the synthesized prefix.  A line
not carrying that prefix holds body text where the prefix should be,
cutting the prefix width from it deletes the text."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 25)
        (sentence-end-double-space nil)
        (text-initial (concat "/* aaa bbb ccc ddd eee\n" "--------------------\n" " */\n"))
        (text-expected (concat "/* aaa bbb ccc ddd eee\n" " * --------------------\n" " */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-region-code-beside-comment-untouched ()
  "A selection holding code beside a comment never merges the code into it.
Each line fills as the cursor on it fills, code beside a comment is
refused and the comment's own line fills the comment."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-selection
         "int x = 1;\n// aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n"
         0
         "int x = 1;\n// aaa bbb ccc ddd eee fff ggg hhh iii\n// jjj kkk.\n")))))

(ert-deftest fill-syntax-bounds-region-refuses-only-the-mixed-paragraph ()
  "Refusal is per line, the rest of the selection still fills.
The code line beside a comment is refused, the comment's line fills it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "// aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll.\n"
          "\n"
          "int x = 1;\n"
          "// mmm nnn ooo ppp qqq rrr sss ttt uuu vvv.\n"))
        (text-expected
         (concat
          "// aaa bbb ccc ddd eee fff ggg hhh iii\n"
          "// jjj kkk lll.\n"
          "\n"
          "int x = 1;\n"
          "// mmm nnn ooo ppp qqq rrr sss ttt uuu\n"
          "// vvv.\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (set-mark (point))
        (goto-char (point-max))
        (activate-mark)
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-region-whole-block-comment ()
  "Selecting a whole block comment fills it as a comment.
A selection ending past the comment was filled as prose, dropping the
continuation prefix from wrapped lines."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/** aaa bbb ccc ddd eee fff ggg\n"
                 " *\n"
                 " * hhh iii jjj kkk lll mmm nnn */\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/** aaa bbb ccc ddd\n"
                 " * eee fff ggg\n"
                 " *\n"
                 " * hhh iii jjj kkk\n"
                 " * lll mmm nnn */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-whole-docstring ()
  "Selecting a whole docstring fills it as a string, once.
Its paragraphs are separated by a blank line, so the selection holds
two paragraphs of the same string, which must be filled by one job."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    Iii jjj kkk lll mmm nnn ooo ppp.\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    Iii jjj kkk lll mmm\n"
                 "    nnn ooo ppp.\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-region-line-comments-two-indents ()
  "Selecting line comments at two indents fills each block as comments.
Blocks at different indents were left to the prose fill, which dropped
the delimiter from wrapped lines."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd eee fff\n"
                 "  # ggg hhh iii jjj kkk lll\n"))
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# eee fff\n"
                 "  # ggg hhh iii jjj\n"
                 "  # kkk lll\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-comment-paragraph-beside-refused ()
  "A comment paragraph fills as a comment, code beside a comment is refused.
The two are separate paragraphs of one selection, the comment after
the code fills as the cursor on its line fills it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* aaa bbb ccc ddd eee fff ggg hhh iii */\n"
                 "\n"
                 "int x = 1;\n"
                 "// jjj kkk lll mmm nnn ooo ppp qqq.\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/* aaa bbb ccc ddd eee\n"
                 " * fff ggg hhh iii */\n"
                 "\n"
                 "int x = 1;\n"
                 "// jjj kkk lll mmm nnn\n"
                 "// ooo ppp qqq.\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-string-closer-in-prose-paragraph ()
  "A string ending in a prose paragraph is not filled twice.
The string's job runs to the end of its closer's line, which the prose
paragraph also held.  The prose fill ran first, so the string's fill
used positions it had moved and never finished.  Outside programming
modes every paragraph fills as prose, the string's closer line with the
rest of its paragraph."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "\"Aaa bbb\n"
                 "\n"
                 "ccc\" ddd     eee\n"
                 "fff ggg hhh iii jjj kkk lll mmm nnn.\n"
                 "\n"
                 "ooo ppp qqq rrr sss ttt uuu vvv www.\n"))
        (text-expected
         ;; format-next-line: off
         (concat "\"Aaa bbb\n"
                 "\n"
                 "ccc\" ddd eee fff ggg\n"
                 "hhh iii jjj kkk lll\n"
                 "mmm nnn.\n"
                 "\n"
                 "ooo ppp qqq rrr sss\n"
                 "ttt uuu vvv www.\n")))
    (with-temp-buffer
      (org-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-comment-closer-in-prose-paragraph ()
  "A comment ending in a prose paragraph is not filled twice.
A selection fills each paragraph as the cursor does, the comment's job
leaves its closer line alone, the text after the closer is beside it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "<!-- aaa\n"
                 "\n"
                 "bbb --> ccc\n"
                 "ddd eee fff ggg hhh iii jjj kkk lll mmm.\n"))
        (text-expected
         ;; format-next-line: off
         (concat "<!-- aaa\n"
                 "\n"
                 "bbb --> ccc\n"
                 "ddd eee fff ggg hhh iii jjj kkk lll mmm.\n")))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-stray-quote-many-prose-paragraphs ()
  "Every paragraph inside a string opened by a stray quote fills as prose."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "I bought a 12\" screen.\n"
                 "\n"
                 "Next paragraph without any quotes at all but it is long enough to wrap.\n"
                 "\n"
                 "Another paragraph without any quotes at all but it is long enough to wrap.\n"
                 "\n"
                 "Then she said \"hi\" to me, and then left.\n"))
        (text-expected
         ;; format-next-line: off
         (concat "I bought a 12\" screen.\n"
                 "\n"
                 "Next paragraph without any\n"
                 "quotes at all but it is long\n"
                 "enough to wrap.\n"
                 "\n"
                 "Another paragraph without any\n"
                 "quotes at all but it is long\n"
                 "enough to wrap.\n"
                 "\n"
                 "Then she said \"hi\" to me,\n"
                 "and then left.\n")))
    (with-temp-buffer
      (org-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-string-opened-in-refused-paragraph ()
  "A selected string opened beside code fills as the cursor fills it.
The next paragraph of the string was taken for prose, as if a stray
quote had opened it, moving the closer onto its text."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "import os\n"
                 "\n"
                 "x = \"\"\"Summary\n"
                 "more\n"
                 "\n"
                 "second paragraph\n"
                 "\"\"\"\n"))
        (text-expected-cursor
         ;; format-next-line: off
         (concat "import os\n"
                 "\n"
                 "x = \"\"\"Summary more\n"
                 "\n"
                 "second paragraph\n"
                 "\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        ;; The cursor path fills the paragraph at point as a string.
        (should-fill-same-from-every-position text-initial 3 text-expected-cursor)
        (should-fill-selection text-initial 0 text-expected-cursor)))))

(ert-deftest fill-syntax-bounds-region-stray-quote-prose-paragraphs ()
  "A paragraph inside a string opened by a stray quote fills as prose.
Outside programming modes a stray quote opens a string running into the
paragraphs after it.  The paragraph holding the quote filled as prose,
the one after it was skipped as if the string's job covered it.
A paragraph holding a whole quote fills as prose too."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "I bought a 12\" screen.\n"
                 "\n"
                 "Next paragraph without any quotes at all but it is long enough to wrap.\n"
                 "\n"
                 "Then she said \"hi\" to me, and then left.\n"
                 "\n"
                 "\"A quote that is long enough to need wrapping here.\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "I bought a 12\" screen.\n"
                 "\n"
                 "Next paragraph without any\n"
                 "quotes at all but it is long\n"
                 "enough to wrap.\n"
                 "\n"
                 "Then she said \"hi\" to me,\n"
                 "and then left.\n"
                 "\n"
                 "\"A quote that is long enough\n"
                 "to need wrapping here.\"\n")))
    (with-temp-buffer
      (org-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-line-comment-at-buffer-end-no-newline ()
  "A line comment ending the buffer without a newline fills as a comment.
`forward-comment' fails on it, so it was taken for a block comment that
never closes."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'python-mode
               "x = 1\n# aaa bbb ccc ddd eee fff ggg hhh iii jjj"
               "x = 1\n# aaa bbb ccc ddd\n# eee fff ggg hhh\n# iii jjj")
              (list
               #'c-mode
               "int x;\n// aaa bbb ccc ddd eee fff ggg hhh iii jjj"
               "int x;\n// aaa bbb ccc ddd\n// eee fff ggg hhh\n// iii jjj")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) 1 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-region-unclosed-block-comment ()
  "Selecting a block comment that never closes fills it as a block comment.
It had no job of its own, so it was left alone, while the cursor filled
it as prose, losing the continuation prefix.  It's filled to the end of
its paragraph, after a closed comment in the same paragraph or another."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-comment-closed "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj */\n")
        (text-comment-unclosed
         ;; format-next-line: off
         (concat "/* A comment with quite a lot of words in it so that it wraps\n"
                 " * and more words here too"))
        (text-expected-closed
         ;; format-next-line: off
         (concat "/* aaa bbb ccc ddd eee fff ggg\n"
                 " * hhh iii jjj */\n"))
        (text-expected-unclosed
         ;; format-next-line: off
         (concat "/* A comment with quite a lot\n"
                 " * of words in it so that it\n"
                 " * wraps and more words here\n"
                 " * too")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-comment-unclosed 0 text-expected-unclosed)
        (should-fill-selection
         (concat text-comment-closed "\n" text-comment-unclosed)
         0
         (concat text-expected-closed "\n" text-expected-unclosed))
        (should-fill-selection
         (concat text-comment-closed text-comment-unclosed)
         0
         (concat text-expected-closed text-expected-unclosed))
        (dotimes (line-index 2)
          (should-fill-same-from-every-position
           text-comment-unclosed line-index text-expected-unclosed))))))

(ert-deftest fill-syntax-bounds-region-unclosed-after-comment-beside-code ()
  "A selected unclosed comment after one opened beside code fills on its own.
The first comment fills as the cursor fills it, its closer staying on
its line, the unclosed comment filled as a block comment."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "int x; /* aaa bbb\n"
                 "\n"
                 "ccc ddd eee fff ggg hhh iii jjj kkk\n"
                 "lll */\n"
                 "/* mmm nnn ooo ppp qqq rrr sss ttt"))
        (text-expected
         ;; format-next-line: off
         (concat "int x; /* aaa bbb\n"
                 "\n"
                 "ccc ddd eee fff ggg hhh iii\n"
                 "jjj kkk\n"
                 "lll */\n"
                 "/* mmm nnn ooo ppp qqq rrr sss\n"
                 " * ttt")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-region-fills-each-line-as-the-cursor ()
  "A selection fills the paragraphs it touches as the cursor fills each line.
Paragraph bounds were read once before filling, a comment's fill moving
the text after it, so later bounds fell in code, duplicating it.  Code
lines beside a doc-string were filled as prose, a paragraph whose first
line ends in a comment was filled with the comment, and an empty
selection filled nothing."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (js-mode)
      (let ((inhibit-message t)
            (fill-column 70)
            (code "function add(a,\n             b) {\n  return a +\n    b;\n}\n"))
        (buffer-reset-text
         (concat
          "/**\n"
          (mapconcat (lambda (i) (format " * w%d" i)) (number-sequence 1 20)
                     "\n")
          "\n * @param a first\n * @param b second\n */\n\n" code))
        (transient-mark-mode 1)
        (goto-char (point-min))
        (set-mark (point))
        (forward-line 24)
        (activate-mark)
        (fancy-fill-paragraph)
        (should (string-suffix-p (concat "*/\n\n" code) (buffer-string)))))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t)
            (fill-column 70)
            (text "def f():\n    \"\"\"Aaa bbb.\"\"\"\n    x = 1\n    y = 2\n"))
        (should-fill-selection text 2 text)))
    (with-temp-buffer
      (latex-mode)
      (let ((inhibit-message t)
            (fill-column 30))
        (should-fill-selection
         "Aaa bbb ccc % note\nddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo ppp\n"
         0
         "Aaa bbb ccc % note\nddd eee fff ggg hhh iii jjj\nkkk lll mmm nnn ooo ppp\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t)
            (fill-column 30))
        (buffer-reset-text "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll */\n")
        (transient-mark-mode 1)
        (goto-char 10)
        (set-mark (point))
        (activate-mark)
        (should (fancy-fill-paragraph))
        (should
         (equal "/* aaa bbb ccc ddd eee fff ggg\n * hhh iii jjj kkk lll */\n" (buffer-string)))))))

(ert-deftest fill-syntax-bounds-region-paragraph-shrinking-at-end-of-buffer ()
  "A selected paragraph ending the buffer fills although its fill shrinks it.
Its bounds were read before the fill and scanned after it, the end then
past the end of the buffer, an error.  The paragraph before another is a
preservation case."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (mode (list #'text-mode #'org-mode #'emacs-lisp-mode))
      (with-temp-buffer
        (funcall mode)
        (let ((inhibit-message t))
          (should-fill-selection "aaa    bbb\nccc    ddd\n" 0 "aaa bbb ccc ddd\n")
          (should-fill-selection
           "aaa    bbb\nccc    ddd\n\neee\n" 0 "aaa bbb ccc ddd\n\neee\n"))))))

(ert-deftest fill-syntax-bounds-region-edge-on-comment-separator-line ()
  "A selection starting or ending on a comment line between paragraphs fills it.
A line separating paragraphs gave a neighboring paragraph's bounds, so a
selection widened to that paragraph, filling it and skipping the comment
the cursor on the line fills.  Other lines separating paragraphs fill
the paragraph beside them, as the cursor on them does."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist
        (case
         (list
          (list
           #'latex-mode
           2
           2
           "Aaa bbb ccc\nddd eee.\n% Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           "Aaa bbb ccc\nddd eee.\n% Mmm nnn ooo ppp qqq rrr sss\n% ttt uuu vvv www.\n")
          (list
           #'latex-mode
           0
           0
           "% Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\nAaa bbb ccc\nddd eee.\n"
           "% Mmm nnn ooo ppp qqq rrr sss\n% ttt uuu vvv www.\nAaa bbb ccc\nddd eee.\n")
          (list
           #'latex-mode
           0
           2
           "% Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n\nMmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           (concat
            "% Aaa bbb ccc ddd eee fff ggg\n% hhh iii jjj kkk.\n\n"
            "Mmm nnn ooo ppp qqq rrr sss\nttt uuu vvv www.\n"))
          (list
           #'c-mode
           1
           2
           "/*\n * Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n *\n * Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n */\n"
           (concat
            "/*\n * Aaa bbb ccc ddd eee fff ggg\n * hhh iii jjj kkk.\n *\n"
            " * Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n */\n"))
          (list
           #'c-mode
           1
           2
           "int x;\n/// Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n///\n/// Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           (concat
            "int x;\n/// Aaa bbb ccc ddd eee fff\n/// ggg hhh iii jjj kkk.\n///\n"
            "/// Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"))
          (list
           #'latex-mode
           1
           1
           "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n%\nMmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n%\nMmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n")
          ;; Unchanged: a heading or a blank line fills the paragraph beside it.
          (list
           #'latex-mode
           0
           0
           "\\section{Foo}\nAaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n"
           "\\section{Foo}\nAaa bbb ccc ddd eee fff ggg\nhhh iii jjj kkk.\n")
          (list
           #'org-mode
           0
           1
           "* Heading\nAaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n"
           "* Heading\nAaa bbb ccc ddd eee fff ggg\nhhh iii jjj kkk.\n")
          (list
           #'latex-mode
           1
           1
           "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n\n\nMmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n\n\nMmm nnn ooo ppp qqq rrr sss\nttt uuu vvv www.\n")
          ;; Unchanged: a comment line within the selection, prose at its edges.
          (list
           #'latex-mode 0 2
           (concat
            "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk.\n"
            "% Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
            "Ooo ppp qqq rrr sss ttt uuu vvv www xxx.\n")
           (concat
            "Aaa bbb ccc ddd eee fff ggg\nhhh iii jjj kkk.\n"
            "% Mmm nnn ooo ppp qqq rrr sss\n% ttt uuu vvv www.\n"
            "Ooo ppp qqq rrr sss ttt uuu\nvvv www xxx.\n"))
          (list
           #'latex-mode
           1
           1
           "Aaa bbb ccc\nddd eee.\n% Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n"
           "Aaa bbb ccc ddd eee.\n% Mmm nnn ooo ppp qqq rrr sss ttt uuu vvv www.\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t)
              (transient-mark-mode t))
          (buffer-reset-text (nth 3 case))
          (goto-char (point-min))
          (forward-line (nth 1 case))
          (set-mark (point))
          (goto-char (point-min))
          (forward-line (nth 2 case))
          (goto-char (pos-eol))
          (activate-mark)
          (fancy-fill-paragraph)
          (should (equal (nth 4 case) (buffer-string))))))))

(ert-deftest fill-syntax-bounds-region-restored-around-its-text ()
  "The selection is restored around the text it held after filling.
Its end was adjusted by the change in buffer size, although a string's
paragraph past the selection fills too, so the end fell short."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t)
            (transient-mark-mode t))
        (buffer-reset-text
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc.\n"
                 "    ddd\n"
                 "    eee\n"
                 "    fff\n"
                 "    ggg\n"
                 "    \"\"\"\n"))
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 2)
        (activate-mark)
        (fancy-fill-paragraph)
        (should
         (equal
          "    \"\"\"Aaa bbb ccc.\n    ddd"
          (string-trim-right
           (buffer-substring-no-properties (region-beginning) (region-end)))))))))

(ert-deftest fill-syntax-bounds-region-within-line-comment-paragraph ()
  "A selection starting or ending within a block of line comments fills all of it.
The block was searched for within the selected lines only, so only they
were filled, as a block of their own, as the cursor on a line doesn't."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat ";; aaa bbb ccc ddd eee fff\n"
                 ";; ggg hhh\n"
                 ";; iii jjj kkk lll\n"
                 ";; mmm nnn\n"))
        (text-expected
         ;; format-next-line: off
         (concat ";; aaa bbb ccc ddd\n"
                 ";; eee fff ggg hhh\n"
                 ";; iii jjj kkk lll\n"
                 ";; mmm nnn\n")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t)
            (transient-mark-mode t))
        ;; Starting within it.
        (should-fill-selection text-initial 1 text-expected)
        ;; Starting and ending within it.
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (forward-line 1)
        (set-mark (point))
        (forward-line 2)
        (activate-mark)
        (fancy-fill-paragraph)
        (deactivate-mark)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-region-beside-code-spanning-paragraphs ()
  "A selected comment or string opened beside code fills as the cursor does.
It spans a blank line into a paragraph without code, each paragraph
filling on its own, the code on the opener line kept."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'c-mode
               (concat
                "int x; /* aaa bbb ccc ddd eee fff ggg hhh iii\n"
                "\n"
                "   jjj kkk lll mmm nnn ooo ppp qqq rrr */\n")
               (concat
                "int x; /* aaa bbb ccc ddd eee\n"
                "        * fff ggg hhh iii\n"
                "\n"
                "   jjj kkk lll mmm nnn ooo ppp\n"
                "   qqq rrr */\n"))
              (list
               #'python-mode
               (concat
                "x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh iii\n"
                "\n"
                "jjj kkk lll mmm nnn ooo ppp qqq rrr\n"
                "\"\"\"\n")
               (concat
                "x = \"\"\"aaa bbb ccc ddd eee fff\n"
                "ggg hhh iii\n"
                "\n"
                "jjj kkk lll mmm nnn ooo ppp\n"
                "qqq rrr\n"
                "\"\"\"\n"))))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-selection (nth 1 case) 0 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-body-paragraph-bounds-past-buffer-end ()
  "Body paragraph bounds ending past the buffer stop at its end.
Out of date bounds must not hang the scan."
  (with-temp-buffer
    (insert "aaa\nbbb")
    (should (equal '((1 . 8)) (fancy-fill-paragraph--body-paragraph-bounds 1 100 0 1 100)))))

(ert-deftest fill-lines-first-line-extra-width-first-line-only ()
  "The first line's extra width narrows the first line only.
Whichever section holds the first line takes it: prose before a list,
the first list item or the first indentation group."
  (let ((dp-list (fancy-fill-paragraph--compile-dot-point-patterns))
        (sentence-end-double-space nil))
    (should
     (equal
      '("aaa bbb" "ccc ddd" "- eee fff" "  ggg hhh" "- iii jjj" "  kkk lll")
      (fancy-fill-paragraph--fill-lines
       '("aaa bbb ccc ddd" "- eee fff ggg hhh" "- iii jjj kkk lll") 11 dp-list 4)))
    (should
     (equal
      '("- eee" "  fff ggg" "  hhh" "- iii jjj" "  kkk lll")
      (fancy-fill-paragraph--fill-lines '("- eee fff ggg hhh" "- iii jjj kkk lll") 11 dp-list 4)))
    (should
     (equal
      '("aaa bbb" "ccc ddd" "  eee fff" "  ggg hhh")
      (fancy-fill-paragraph--fill-lines '("aaa bbb ccc ddd" "  eee fff ggg hhh") 11 dp-list 4)))))

;; ---------------------------------------------------------------------------
;; Split Tests

(ert-deftest split-delimiter-match-point-uses-all-patterns ()
  "Delimiter matching returns the earliest match across all patterns."
  (let ((match-point (fancy-fill-paragraph--make-delimiter-match-point 'close '(" z" " y" " x"))))
    (should (= 2 (funcall match-point "a x b y c z" 0)))))

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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-two-items-fit-one-line ()
  "Two short items fit on one line."
  (let ((text-initial "A. B.")
        (text-expected "A. B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-two-items-need-two-lines ()
  "Two items that exceed fill-column are split."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 15)))))

(ert-deftest solve-double-space-separator ()
  "Double space is used between sentences when enabled."
  (let ((text-initial "A. B.")
        (text-expected "A.  B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-single-space-separator ()
  "Single space is used between sentences when double-space is disabled."
  (let ((text-initial "A. B.")
        (text-expected "A. B.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-comma-single-space ()
  "Single space is always used after commas regardless of double-space setting."
  (let ((text-initial "Hello, world.")
        (text-expected "Hello, world.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 50 :space 0))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-prefer-period-break ()
  "Solver prefers breaking at periods over commas."
  (let ((text-initial "Hello, world. Foo, bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello, world.\n"
                 "Foo, bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 30 :space 0))
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-oversized-single-item ()
  "Single item exceeding fill-column is placed on its own line."
  (let ((text-initial "This is a very long sentence that exceeds the fill column.")
        (text-expected "This is a very long sentence that exceeds the fill column.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-optimal-three-way-split ()
  "Solver optimally distributes items across lines."
  (let ((text-initial "Alpha beta. Gamma. Delta epsilon.")
        (text-expected
         ;; format-next-line: off
         (concat "Alpha beta. Gamma.\n"
                 "Delta epsilon."))
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil)
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
        (sentence-end-double-space t))
    ;; "Alpha." (6) + "  " (2) + "Beta." (5) = 13, exceeds fill-col 12.
    ;; Must split onto two lines.
    (should (equal text-expected (test-split-and-solve text-initial 12)))))

(ert-deftest solve-first-line-extra-slack ()
  "The first line's slack is measured with its extra width."
  (let ((text-initial "a b c")
        (text-expected
         ;; format-next-line: off
         (concat "a b\n"
                 "c"))
        (sentence-end-double-space nil))
    ;; "a b" (3) + extra (3) = 6, filling fill-col 6.
    ;; Measured without the extra, breaking after "a" looks better.
    (should (equal text-expected (test-split-and-solve text-initial 6 3)))))

(ert-deftest solve-first-line-extra-single-item ()
  "A single item on the first line is measured with its extra width."
  (let ((text-initial "a b ccc dddd")
        (text-expected
         ;; format-next-line: off
         (concat "a\n"
                 "b ccc\n"
                 "dddd"))
        (sentence-end-double-space nil))
    ;; "a" (1) + extra (4) = 5, as wide as "b ccc" (5).
    ;; Measured without the extra, "a" alone looks too short to keep.
    (should (equal text-expected (test-split-and-solve text-initial 7 4)))))


;; ---------------------------------------------------------------------------
;; Integration Tests

(ert-deftest fill-no-wrap-needed ()
  "Short paragraph that fits on one line is unchanged."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-double-space-not-after-dotted-word ()
  "A word holding a period (a file name for e.g.) isn't a sentence end.
Any character after a period was taken for closing punctuation, so the
space after `foo.c' was doubled.  A closing quote or bracket after the
period still ends a sentence, as does a closer after it in a comment."
  (let ((fill-column 80)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test "See foo.c and bar.h. (Here.) Then \"this.\" Done.\n"
      (fancy-fill-paragraph)
      (should (equal "See foo.c and bar.h.  (Here.)  Then \"this.\"  Done.\n" (buffer-string)))))
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space t))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position
         "/* Aaa bbb\n * see foo.c */\n" 0 "/* Aaa bbb see foo.c */\n")
        (should-fill-same-from-every-position
         "/* Aaa bbb\n * see foo. */\n" 0 "/* Aaa bbb see foo.  */\n")))))

(ert-deftest fill-double-space-after-closer-unchanged ()
  "Each closing quote, guillemet or bracket after punctuation ends a sentence.
A preservation case for `fill-double-space-not-after-dotted-word', a
dotted abbreviation and a period before a comma keep a single space."
  (let ((fill-column 200)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test
        (concat
         "Aaa.) Bbb.] Ccc.} Ddd.\" Eee.' Fff.\u201D Ggg.\u2019 Hhh?) Iii!\" Jjj\u2026) "
         "\u00ABLll.\u00BB \u2039Mmm.\u203A "
         "Kkk. The U.S. and e.g., then etc., done.\n")
      (fancy-fill-paragraph)
      (should
       (equal
        (concat
         "Aaa.)  Bbb.]  Ccc.}  Ddd.\"  Eee.'  Fff.\u201D  Ggg.\u2019  Hhh?)  Iii!\"  Jjj\u2026)  "
         "\u00ABLll.\u00BB  \u2039Mmm.\u203A  "
         "Kkk.  The U.S. and e.g., then etc., done.\n")
        (buffer-string))))))

(ert-deftest fill-abbreviation-single-space-preserved-with-double-space ()
  "Single space after an abbreviation period should be preserved."
  (let ((text-initial "alpha beta e.g. gamma delta epsilon zeta eta theta\n")
        (text-expected "alpha beta e.g. gamma delta epsilon zeta eta theta\n")
        (fill-column 60)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-abbreviation-line-break-uses-single-space ()
  "Line breaks after abbreviations refill with a single space."
  (let ((text-initial "alpha beta e.g.\ngamma delta epsilon zeta eta theta\n")
        (text-expected "alpha beta e.g. gamma delta epsilon zeta eta theta\n")
        (fill-column 60)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-abbreviation-double-space-collapsed ()
  "A double space after a dotted abbreviation is collapsed to one.
It can't be told apart from an abbreviation within a sentence, a
known limitation, see `fancy-fill-paragraph--maybe-double-space'.
The double space after an ordinary sentence end is kept."
  (let ((text-initial "He moved to the U.S.  Then he left.  Alpha beta gamma.\n")
        (text-expected "He moved to the U.S. Then he left.  Alpha beta gamma.\n")
        (fill-column 70)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-sentence-double-space-preserved-with-double-space ()
  "Double space after a sentence period should be preserved."
  (let ((text-initial "alpha beta.  Gamma delta epsilon zeta eta theta\n")
        (text-expected "alpha beta.  Gamma delta epsilon zeta eta theta\n")
        (fill-column 60)
        (sentence-end-double-space t))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-normalizes-double-space ()
  "Existing double spaces are normalized then re-applied based on preference."
  (let ((text-initial "Hello world.  Foo bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-comma-no-split ()
  "Commas don't cause splits when weight is zero."
  (let ((text-initial "Hello, world. Foo, bar.")
        (text-expected "Hello, world. Foo, bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-collapse-extra-whitespace ()
  "Multiple spaces within text are collapsed."
  (let ((text-initial "Hello   world.   Foo   bar.")
        (text-expected "Hello world. Foo bar.")
        (fill-column 80)
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest line-matches-prefix-p-accepts-a-line-without-the-trailing-space ()
  "A line holding the prefix without its trailing space still matches.
Trailing blank-space is trimmed from a line holding nothing else, so
the padding is what keeps a blank body line from being treated as text."
  (with-fancy-fill-paragraph-test (concat " *\n" " * aaa\n" " *bbb\n")
    (goto-char (point-min))
    (should (fancy-fill-paragraph--line-matches-prefix-p " * "))
    (forward-line 1)
    (should (fancy-fill-paragraph--line-matches-prefix-p " * "))
    (forward-line 1)
    (should-not (fancy-fill-paragraph--line-matches-prefix-p " * "))))

(ert-deftest range-has-non-blank-p-empty-and-reversed-ranges ()
  "An empty or reversed range holds nothing, whatever lies between.
The code-beside-comment scan relies on this, an opener preceding the
scan position reverses the range it checks for code."
  (with-fancy-fill-paragraph-test "aaa   bbb\n"
    (should (fancy-fill-paragraph--range-has-non-blank-p (point-min) (point-max)))
    ;; Blank-space only.
    (should-not (fancy-fill-paragraph--range-has-non-blank-p 4 7))
    ;; Empty.
    (should-not (fancy-fill-paragraph--range-has-non-blank-p 5 5))
    ;; Reversed, every character of the buffer lies between the two.
    (should-not (fancy-fill-paragraph--range-has-non-blank-p 9 2))))

(ert-deftest strip-region-lines-end-past-point-max-does-not-stall ()
  "A stale END past `point-max' must not make line stripping loop forever."
  (with-fancy-fill-paragraph-test "aaa  bbb\n"
    (with-forward-line-stall-guard
      (should (equal '("aaa  bbb") (fancy-fill-paragraph--strip-region-lines 1 12 0))))))

(ert-deftest strip-region-lines-keeps-a-line-without-the-prefix ()
  "A line that does not carry the prefix must keep every character.
Only its leading blank-space is removed, cutting the prefix width from
body text deletes it."
  (with-fancy-fill-paragraph-test " * aaa\n--------\n"
    (should
     (equal
      '("aaa" "--------")
      (fancy-fill-paragraph--strip-region-lines (point-min) (point-max) 3 " * ")))))

(ert-deftest strip-region-lines-without-a-prefix-cuts-by-column ()
  "With no prefix to check, every line is cut at the given column."
  (with-fancy-fill-paragraph-test " * aaa\n--------\n"
    (should
     (equal
      '("aaa" "-----") (fancy-fill-paragraph--strip-region-lines (point-min) (point-max) 3)))))

(ert-deftest syntax-region-at-closed-regions ()
  "A closed comment or string reports its kind and the position after its closer."
  (with-temp-buffer
    (c-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "/* closed */ tail")
      (syntax-propertize (point-max))
      (should (equal '(comment . 13) (fancy-fill-paragraph--syntax-region-at 1)))
      (buffer-reset-text "\"a string\"; x")
      (syntax-propertize (point-max))
      (should (equal '(string . 11) (fancy-fill-paragraph--syntax-region-at 1)))
      (buffer-reset-text "// line\nx;")
      (syntax-propertize (point-max))
      (should (equal '(comment . 9) (fancy-fill-paragraph--syntax-region-at 1)))
      ;; The end of the buffer closes a line comment without a newline.
      (buffer-reset-text "x; // line")
      (syntax-propertize (point-max))
      (should (equal '(comment . 11) (fancy-fill-paragraph--syntax-region-at 4))))))

(ert-deftest syntax-region-at-unclosed-regions ()
  "A region that never closes reports its kind with no resume position.
An apostrophe in shell text opens a string that runs to the end of the
buffer, a quoted word beside it still closes.  The kind is what tells
an unclosed comment from an unclosed string."
  (with-temp-buffer
    (c-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "/* never closed")
      (syntax-propertize (point-max))
      (should (equal '(comment) (fancy-fill-paragraph--syntax-region-at 1)))))
  (with-temp-buffer
    (sh-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "echo don't")
      (syntax-propertize (point-max))
      (should (equal '(string) (fancy-fill-paragraph--syntax-region-at 9)))
      (buffer-reset-text "echo 'closed' x")
      (syntax-propertize (point-max))
      (should (equal '(string . 14) (fancy-fill-paragraph--syntax-region-at 6))))))

(ert-deftest string-end-includes-triple-quote-closer ()
  "A triple-quoted string ends after all three closing quotes.
`python-mode' ends the string at the first closing quote, the two after
it read as an empty string.  Both such pairs belong to the string, while
a real empty string stays apart, after a closed string, before code or
glued before a string opened with one quote."
  (with-temp-buffer
    (python-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "\"\"\"aaa\"\"\" x")
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (search-forward "aaa")
      (let* ((text-beg (match-beginning 0))
             ;; The string opens at its last opening quote.
             (opener (1- text-beg))
             (closer-beg (match-end 0))
             (string-end (+ closer-beg 3)))
        (should (equal opener (fancy-fill-paragraph--comment-or-string-start-at (1+ text-beg))))
        (should (equal string-end (fancy-fill-paragraph--string-end opener)))
        (should (equal (cons 'string string-end) (fancy-fill-paragraph--syntax-region-at opener)))
        (should (equal opener (fancy-fill-paragraph--string-opener-run-start (point-min))))
        (should (equal opener (fancy-fill-paragraph--string-closer-run-owner (1+ closer-beg))))
        ;; A cursor on the closing quotes is in the string they close.
        (should
         (equal opener (fancy-fill-paragraph--comment-or-string-start-at (+ closer-beg 2)))))))
  (with-temp-buffer
    (c-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "\"aaa\"\"\"; x")
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (search-forward "aaa")
      ;; The empty string after the closing quote is a string of its own.
      (let ((empty-beg (1+ (match-end 0))))
        (should (equal empty-beg (fancy-fill-paragraph--string-end (point-min))))
        (should-not (fancy-fill-paragraph--string-closer-run-owner empty-beg))
        (should-not (fancy-fill-paragraph--string-opener-run-start (point-min))))))
  (with-temp-buffer
    (python-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "x = \"\" + y")
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (search-forward "\"")
      ;; The empty string before code opens no string after it.
      (let ((empty-beg (match-beginning 0)))
        (should (equal (+ empty-beg 2) (fancy-fill-paragraph--string-end empty-beg)))
        (should-not (fancy-fill-paragraph--string-opener-run-start empty-beg)))))
  (with-temp-buffer
    (python-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "x = \"\"\"\"\"\"")
      (syntax-propertize (point-max))
      ;; An empty triple-quoted string, its opener and closer are one run of
      ;; quotes, the closer's quotes are still its own.
      (let ((opener 7))
        (should (equal 11 (fancy-fill-paragraph--string-end opener)))
        (dolist (pos (list 8 9 10))
          (should (equal opener (fancy-fill-paragraph--comment-or-string-start-at pos)))))))
  (with-temp-buffer
    (sh-mode)
    (let ((inhibit-message t))
      (buffer-reset-text "echo \"\"\"aaa\" x")
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (search-forward "aaa")
      ;; Runs of three and one quote, the shorter is the delimiter, the
      ;; empty string before it is apart.
      (let ((opener (1- (match-beginning 0))))
        (should (equal opener (fancy-fill-paragraph--comment-or-string-start-at (1+ opener))))
        (should (equal (1+ (match-end 0)) (fancy-fill-paragraph--string-end opener)))
        (should-not (plist-get (fancy-fill-paragraph--string-opener-info opener) :repeated))
        (should-not (fancy-fill-paragraph--string-opener-run-start (- opener 2)))))))

(ert-deftest string-closer-run-owner-not-a-comment ()
  "The quotes opening a triple-quoted string below a comment aren't its closer.
The comment was taken to own them, scanning for its end skips it and
ends past the quotes.  Scanning the selection's regions then resumed at
the quotes, the comment's end, looping forever."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# Aaa.\n"
                 "\"\"\"Bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Iii.\n"
                 "\"\"\"\n"
                 "import os\n"))
        (text-expected
         ;; format-next-line: off
         (concat "# Aaa.\n"
                 "\"\"\"Bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Iii.\n"
                 "\"\"\"\n"
                 "import os\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (should-not (fancy-fill-paragraph--string-closer-run-owner 8))
        (should-fill-selection text-initial 0 text-expected)))))

(ert-deftest string-opener-info-splits-delimiter-from-text ()
  "The opener prefix ends after one run of the opening delimiter.
A quote directly after a backtick opener is text, although a quote is
a string delimiter too.  String prefix letters and a triple quote stay
in the prefix, the triple quote is repeated."
  (with-temp-buffer
    (js-mode)
    (let ((inhibit-message t))
      (buffer-reset-text
       ;; format-next-line: off
       (concat "`\"aaa\" bbb\n"
               "ccc\n"
               "`\n"))
      (syntax-propertize (point-max))
      (should
       (equal
        '(:prefix "`" :text "\"aaa\" bbb" :repeated nil)
        (fancy-fill-paragraph--string-opener-info (point-min))))))
  (with-temp-buffer
    (python-mode)
    (let ((inhibit-message t))
      (buffer-reset-text
       ;; format-next-line: off
       (concat "    r\"\"\"Alpha bravo\n"
               "    charlie\n"
               "    \"\"\"\n"))
      (syntax-propertize (point-max))
      (should
       (equal
        '(:prefix "    r\"\"\"" :text "Alpha bravo" :repeated t)
        (fancy-fill-paragraph--string-opener-info
         (fancy-fill-paragraph--comment-or-string-start-at
          (progn
            (goto-char (point-min))
            (search-forward "Alpha")))))))))

(ert-deftest syntax-fill-jobs-in-range-one-job-per-docstring ()
  "A docstring whose closer shares a line with text is one job.
The quotes after the closing fence once made a second job, filling the
closer line on its own before the docstring was filled over it."
  (with-temp-buffer
    (python-mode)
    (let ((inhibit-message t)
          (fancy-fill-paragraph-syntax-bounds t))
      (buffer-reset-text
       ;; format-next-line: off
       (concat "def f():\n"
               "    \"\"\"aaa bbb\n"
               "    ccc ddd.\"\"\"\n"))
      (syntax-propertize (point-max))
      (goto-char (point-min))
      (forward-line 1)
      (let* ((docstring-end (pos-eol 2))
             (jobs
              (fancy-fill-paragraph--syntax-fill-jobs-in-range (pos-bol) docstring-end (pos-eol))))
        (should (equal 1 (length jobs)))
        ;; Scanning resumes past the closing quotes, which end the line.
        (should
         (equal docstring-end (fancy-fill-paragraph--syntax-fill-job-resume (car jobs))))))))

(ert-deftest fill-syntax-bounds-option-off-code-beside-comment-fills ()
  "With `fancy-fill-paragraph-syntax-bounds' nil, code beside a comment fills.
The refusal is part of the syntax handling, turning it off restores the
plain prose fill even where that merges the code into the comment."
  (let ((fancy-fill-paragraph-syntax-bounds nil)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial (concat "int x = 1;\n" "// aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n"))
        (text-expected (concat "int x = 1; // aaa bbb ccc ddd eee fff\n" "ggg hhh iii jjj.\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest extract-body-indent-tab-straddling-opener-column ()
  "A tab reaching past OPENER-COL is kept whole, there is nothing to pad.
Padding by the difference would be negative, which is an error rather
than a wrong result."
  (with-fancy-fill-paragraph-test "\tfoo bar\n"
    (setq tab-width 8)
    (should (equal "\t" (fancy-fill-paragraph--extract-body-indent (point-min) 4))))
  (with-fancy-fill-paragraph-test "  foo bar\n"
    ;; Blank-space short of OPENER-COL is padded out to it.
    (should (equal "    " (fancy-fill-paragraph--extract-body-indent (point-min) 4)))))

(ert-deftest fill-syntax-bounds-block-comment-tab-body-under-space-opener ()
  "A tab-indented body line under a space-indented opener fills.
The opener column falls inside the tab, so the indent taken from the
body line already reaches past it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 25)
        (sentence-end-double-space nil)
        (text-initial (concat "  x(); /* aaa bbb ccc ddd eee\n" "\tttt uuu vvv www\n" " */\n"))
        (text-expected
         (concat "  x(); /* aaa bbb ccc\n" "\t * ddd eee ttt\n" "\t * uuu vvv www\n" " */\n")))
    (with-temp-buffer
      (c-mode)
      (setq tab-width 8)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-option-off-cursor-path-plain-fill ()
  "With `fancy-fill-paragraph-syntax-bounds' nil the cursor path fills as prose.
No continuation prefix is synthesized, the comment is treated as text."
  (let ((fancy-fill-paragraph-syntax-bounds nil)
        (fill-column 25)
        (sentence-end-double-space nil)
        (text-initial "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk */\n")
        (text-expected (concat "/* aaa bbb ccc ddd eee\n" "fff ggg hhh iii jjj kkk\n" "*/\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-option-off-selection-path-plain-fill ()
  "With `fancy-fill-paragraph-syntax-bounds' nil a selection fills as prose.
The selection path is gated by the option just as the cursor path is.
Both ends sit inside the comment, a selection the path only recognizes
as one syntax region while the option is on."
  (let ((fancy-fill-paragraph-syntax-bounds nil)
        (fill-column 25)
        (sentence-end-double-space nil)
        (text-initial "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk */\n")
        (text-expected (concat "/* aaa bbb ccc ddd eee\n" "fff ggg hhh iii jjj kkk\n" "*/\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (push-mark 44 t t)
        (should (fancy-fill-paragraph))
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-region-end-past-point-max-does-not-stall ()
  "A stale END past `point-max' must not make filling loop forever."
  (let ((fill-column 70)
        ;; Avoid unrelated prefix detection so this exercises the TODO #11 path.
        (fill-prefix ""))
    (with-fancy-fill-paragraph-test "aaa  bbb\n"
      (with-forward-line-stall-guard
        (fancy-fill-paragraph--fill-region 1 12))
      (should (equal "aaa bbb" (buffer-string))))))

(ert-deftest fill-region-end-past-point-max-with-prefix-does-not-stall ()
  "A stale END past `point-max' must be clamped before prefix validation."
  (let ((fill-column 70)
        (fill-prefix "> "))
    (with-fancy-fill-paragraph-test "> aaa  bbb\n"
      (with-forward-line-stall-guard
        (fancy-fill-paragraph--fill-region 1 13))
      (should (equal "> aaa bbb" (buffer-string))))))


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
        (sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 0))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-blank-space-weight-100-joins-freely ()
  "When blank-space-weight is 100, items join freely when they fit."
  (let ((text-initial "A. B. C.")
        (text-expected "A. B. C.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 0 :space 0))
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 20)))))

(ert-deftest solve-space-prefers-delimiter-break ()
  "Solver prefers delimiter breaks over space breaks."
  (let ((text-initial "Hello world. Foo bar.")
        (text-expected
         ;; format-next-line: off
         (concat "Hello world.\n"
                 "Foo bar."))
        (fancy-fill-paragraph-split-weights (list :period 100 :space 1))
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (should (equal text-expected (test-split-and-solve text-initial 10)))))

(ert-deftest solve-closing-quote-sentence-end-double-space ()
  "Closing quote uses double space when sentence ends inside quotes."
  (let ((text-initial "She said \"hello.\" He replied.")
        (text-expected "She said \"hello.\"  He replied.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-quote-mid-sentence-no-double-space ()
  "Closing quote mid-sentence uses single space, not double."
  (let ((text-initial "The value \"None\" is returned.")
        (text-expected "The value \"None\" is returned.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :double-quote 25 :open-double-quote 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-paren-sentence-end-double-space ()
  "Closing paren uses double space when sentence ends inside parens."
  (let ((text-initial "(See above.) The next.")
        (text-expected "(See above.)  The next.")
        (fancy-fill-paragraph-split-weights (list :period 100 :paren 25 :open-paren 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-bracket-sentence-end-double-space ()
  "Closing bracket uses double space when sentence ends inside brackets."
  (let ((text-initial "[See above.] The next.")
        (text-expected "[See above.]  The next.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :bracket 25 :open-bracket 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-closing-brace-sentence-end-double-space ()
  "Closing brace uses double space when sentence ends inside braces."
  (let ((text-initial "{See above.} The next.")
        (text-expected "{See above.}  The next.")
        (fancy-fill-paragraph-split-weights (list :period 100 :brace 25 :open-brace 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-question-mark-quote-double-space ()
  "Question mark before closing quote gets double space."
  (let ((text-initial "She asked \"why?\" He shrugged.")
        (text-expected "She asked \"why?\"  He shrugged.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :question 100 :double-quote 25 :open-double-quote 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-exclamation-quote-double-space ()
  "Exclamation before closing quote gets double space."
  (let ((text-initial "He yelled \"stop!\" She froze.")
        (text-expected "He yelled \"stop!\"  She froze.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :exclamation 100 :double-quote 25 :open-double-quote 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-ellipsis-quote-double-space ()
  "Ellipsis before closing quote gets double space."
  (let ((text-initial "She said \"well\u2026\" He waited.")
        (text-expected "She said \"well\u2026\"  He waited.")
        (fancy-fill-paragraph-split-weights
         (list :period 100 :ellipsis 100 :double-quote 25 :open-double-quote 0 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-unicode-ellipsis-sentence-end-double-space ()
  "Unicode ellipsis gets double space when it ends a sentence."
  (let ((text-initial "She paused\u2026 He waited.")
        (text-expected "She paused\u2026  He waited.")
        (fancy-fill-paragraph-split-weights (list :period 100 :ellipsis 100 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-comma-after-period-no-double-space ()
  "Comma after abbreviation period does not get double space."
  (let ((text-initial "Use etc., not etcetera.")
        (text-expected "Use etc., not etcetera.")
        (fancy-fill-paragraph-split-weights (list :period 100 :comma 25 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-semicolon-after-period-no-double-space ()
  "Semicolon after abbreviation period does not get double space."
  (let ((text-initial "Use etc.; not etcetera.")
        (text-expected "Use etc.; not etcetera.")
        (fancy-fill-paragraph-split-weights (list :period 100 :semicolon 25 :space 1))
        (sentence-end-double-space t))
    (should (equal text-expected (test-split-and-solve text-initial 80)))))

(ert-deftest solve-colon-after-period-no-double-space ()
  "Colon after abbreviation period does not get double space."
  (let ((text-initial "See etc.: the list.")
        (text-expected "See etc.: the list.")
        (fancy-fill-paragraph-split-weights (list :period 100 :colon 25 :space 1))
        (sentence-end-double-space t))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-region-same-size-reflow-returns-changed ()
  "Region fill should report changes even when buffer size is unchanged."
  (let ((text-initial "aaa bbb\nccc ddd\n")
        (text-expected "aaa bbb ccc ddd\n")
        (fill-column 20))
    (with-fancy-fill-paragraph-test text-initial
      (transient-mark-mode 1)
      (goto-char (point-max))
      (push-mark (point-min) t t)
      (let ((changed (fancy-fill-paragraph)))
        (should (equal text-expected (buffer-string)))
        (should changed)))))


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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-prefix-mismatch-preserves-non-whitespace-prefix ()
  "Fallback prefix validation must not delete non-whitespace prefix chars."
  (let ((fill-column 40)
        (sentence-end-double-space nil)
        (text-initial "; a\n; b\n  c\n"))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (= 2 (how-many ";" (point-min) (point-max)))))))

(ert-deftest fill-prefix-mismatch-preserves-many-non-whitespace-prefixes ()
  "Fallback validation must preserve many prefix markers before mismatches."
  (let ((fill-column 36)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat
          ";; alpha beta gamma delta.\n"
          ";; epsilon zeta eta theta.\n"
          ";; iota kappa lambda mu.\n"
          "   nu xi omicron pi rho.\n"
          "  sigma tau upsilon phi chi psi omega.\n")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 4)
      (fancy-fill-paragraph)
      (should (= 6 (how-many ";" (point-min) (point-max)))))))

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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 75))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-dot-point-continuation-less-indented ()
  "Dot-point continuation indented less than the item text keeps its text.
The item's indentation was removed from each continuation line without
checking it was blank, deleting the first letters of the line."
  (let ((text-initial
         ;; format-next-line: off
         (concat " - Alpha bravo\n"
                 "  charlie delta"))
        (text-expected
         ;; format-next-line: off
         (concat " - Alpha bravo charlie\n"
                 "   delta"))
        (fill-column 24)
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
       (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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

(ert-deftest fill-false-dot-point-on-fitting-line-stays-joined ()
  "False dot-point prevention must not split text that fits on one line."
  (let* ((text-initial "alpha beta * gamma\n")
         (text-expected text-initial)
         (fill-column 40)
         (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-numbered-false-dot-point-on-fitting-line-stays-joined ()
  "Regexp dot-point prevention must not split text that fits on one line."
  (let* ((text-initial "alpha beta 2. gamma\n")
         (text-expected text-initial)
         (fill-column 40)
         (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char 2)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

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
       (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
        (fancy-fill-paragraph-blank-space-weight 100))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-column-target-float-clamps-to-effective-fill-column ()
  "Float fill-column-target above 1.0 clamps to the effective fill column."
  (let ((break-weights [75 75 75])
        (seps [" " " " " "])
        (sep-lens [1 1 1])
        (fancy-fill-paragraph-fill-column-target 2.0))
    (should
     (equal
      '("xxx" "xxx" "xxx" "xxx")
      (fancy-fill-paragraph--solve
       '("xxx" "xxx" "xxx" "xxx") 10 break-weights seps sep-lens 50)))))

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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space t))
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
        (sentence-end-double-space nil))
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
        (sentence-end-double-space nil))
    (with-fancy-fill-paragraph-test text-initial
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))


;; ---------------------------------------------------------------------------
;; Point Placement Tests

(ert-deftest fill-syntax-bounds-point-follows-text-line-comment ()
  "Point stays in its word when a line comment wraps.
Each wrap inserts a prefix before point, keeping the buffer offset
left point lines away from the word it was in."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "# aaa bbb ccc ddd eee fff ggg hhh iii jjj kk|k lll\n")
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# eee fff ggg hhh\n"
                 "# iii jjj kk|k lll\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-follows-text-block-comment ()
  "Point stays in its word when a block comment wraps."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kk|k */\n")
        (text-expected
         ;; format-next-line: off
         (concat "/* aaa bbb ccc ddd\n"
                 " * eee fff ggg hhh\n"
                 " * iii jjj kk|k */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-follows-text-string ()
  "Point stays in its word when a docstring wraps."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa bbb ccc ddd eee fff ggg hhh iii jjj kk|k.\"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh iii\n"
                 "    jjj kk|k.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-follows-text-when-lines-join ()
  "Point stays in its word when comment lines join.
Joining removes prefixes before point, the inverse of a wrap."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# aaa\n"
                 "# bbb\n"
                 "# ccc\n"
                 "# dd|d\n"))
        (text-expected "# aaa bbb ccc dd|d\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-follows-text-in-later-job ()
  "Point in the second of two comment blocks stays in its word.
The blocks fill last to first, so the first block changes length after
the second is filled, moving the text point is in."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd eee fff\n"
                 "  # ggg hhh iii jjj kkk| lll\n"))
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# eee fff\n"
                 "  # ggg hhh iii jjj\n"
                 "  # kkk| lll\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-before-inserted-line-break ()
  "Point before a space that becomes a line break ends the first line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "# aaa bbb ccc ddd| eee fff ggg\n")
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd|\n"
                 "# eee fff ggg\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-after-inserted-line-break ()
  "Point after a space that becomes a line break starts the next line.
Point lands after the continuation prefix, before the word it preceded."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "# aaa bbb ccc ddd |eee fff ggg\n")
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# |eee fff ggg\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-at-eob-follows-text ()
  "Point at the end of a buffer without a trailing newline stays there."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "# aaa bbb ccc ddd eee fff ggg|")
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# eee fff ggg|")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-before-changes-untouched ()
  "Point in the first of two comment blocks keeps its place.
Only text after point changes, so point must not move."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# aaa bbb| ccc ddd eee fff\n"
                 "  # ggg hhh iii jjj kkk lll\n"))
        (text-expected
         ;; format-next-line: off
         (concat "# aaa bbb| ccc ddd\n"
                 "# eee fff\n"
                 "  # ggg hhh iii jjj\n"
                 "  # kkk lll\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))

(ert-deftest fill-syntax-bounds-point-unchanged-comment-untouched ()
  "Point in a comment that is already filled keeps its place."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "# aaa bbb ccc ddd\n"
                 "# ee|e fff ggg\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-initial)))))

(ert-deftest fill-point-follows-text-prose ()
  "Point stays in its word when plain text wraps.
Plain text takes a separate path from comments and strings, both must
keep point with its text."
  (let ((fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "aaa bbb ccc ddd eee fff ggg hhh iii jjj kk|k lll\n")
        (text-expected
         ;; format-next-line: off
         (concat "aaa bbb ccc ddd eee\n"
                 "fff ggg hhh iii jjj\n"
                 "kk|k lll\n")))
    (with-temp-buffer
      (let ((inhibit-message t))
        (should-fill-keep-point text-initial text-expected)))))


;; ---------------------------------------------------------------------------
;; Comment and String Layout Tests
;;
;; Line widths, openers and closers of comments and strings.  Each fill
;; goes through a helper that fills again, which must change nothing.

(ert-deftest fill-syntax-bounds-c-mode-inline-comment-refill-unchanged ()
  "An inline block comment wrapped past two lines stays as filled.
The filled comment is no longer inline, the refill once left its closer
line out of the paragraph and broke the rest again."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 28)
        (sentence-end-double-space nil)
        (text-initial
         "  /* aaa bbbb cccccccc, dddd eeeee ffffff ggg hhh iiiii j, kk llll mmmmmmm nn */\n")
        (text-expected
         ;; format-next-line: off
         (concat "  /* aaa bbbb cccccccc,\n"
                 "   * dddd eeeee ffffff\n"
                 "   * ggg hhh iiiii j,\n"
                 "   * kk llll mmmmmmm nn */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-c-mode-inline-comment-beside-code-refill-unchanged ()
  "An inline block comment beside code wrapped past two lines stays as filled."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial "int x = 1; /* aaaaaaaa bb ccccc d eee, ffffff. */\n")
        (text-expected
         ;; format-next-line: off
         (concat "int x = 1; /* aaaaaaaa\n"
                 "            * bb ccccc\n"
                 "            * d eee,\n"
                 "            * ffffff. */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-html-inline-comment-refill-unchanged ()
  "An inline XML comment wrapped past two lines stays as filled.
Only that is checked, not the continuation prefix XML comments take."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 28)
        (sentence-end-double-space nil)
        (text-initial "<!-- aaa bb cc ddddd eeeeee ffff ggg, hhh ii jjj -->\n"))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        ;; Past two lines, the second fill takes the multi-line path.
        (should (< 2 (count-lines (point-min) (point-max))))
        (should-fill-again-unchanged)))))

(ert-deftest fill-syntax-bounds-python-module-docstring-refill-unchanged ()
  "A one-line module docstring wrapped past two lines stays as filled."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial "\"\"\"Alpha bravo charlie delta echo foxtrot golf.\"\"\"\n")
        (text-expected
         ;; format-next-line: off
         (concat "\"\"\"Alpha bravo\n"
                 "charlie delta echo\n"
                 "foxtrot golf.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)))))

(ert-deftest fill-syntax-bounds-python-inline-docstring-refill-unchanged ()
  "A one-line docstring wrapped past two lines stays as filled."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa. bbbbbbbb. cccc\"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa.\n"
                 "    bbbbbbbb.\n"
                 "    cccc\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-closer-text-joins-last-paragraph ()
  "Text on a closer line fills with the paragraph it ends.
Only the paragraph at point fills.  With point in the first paragraph,
the closer line is left as it is."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* Aaa bbb ccc ddd eee fff ggg.\n"
                 " *\n"
                 " * Hhh iii jjj kkk lll mmm nnn. */\n"))
        (text-expected-last
         ;; format-next-line: off
         (concat "/* Aaa bbb ccc ddd eee fff ggg.\n"
                 " *\n"
                 " * Hhh iii jjj kkk lll\n"
                 " * mmm nnn. */\n"))
        (text-expected-first
         ;; format-next-line: off
         (concat "/* Aaa bbb ccc ddd eee\n"
                 " * fff ggg.\n"
                 " *\n"
                 " * Hhh iii jjj kkk lll mmm nnn. */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 2 text-expected-last)
        (should-fill-same-from-every-position text-initial 0 text-expected-first)))))

(ert-deftest fill-syntax-bounds-python-docstring-closer-alone-stays ()
  "Closing quotes on a line of their own are not joined to the text.
Filling may shrink the docstring to two lines, after which it fills as
inline text.  The closer must stay on its own line in both cases, blank
space after it included."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Alpha bravo\n"
                 "    charlie.\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Alpha bravo charlie.\n"
                 "    \"\"\"\n"))
        ;; The same docstring with blank space after the closer.
        (text-initial-spaced
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Alpha bravo\n"
                 "    charlie.\n"
                 "    \"\"\" \t\n"))
        (text-expected-spaced
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Alpha bravo charlie.\n"
                 "    \"\"\" \t\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        ;; Already two lines, as the first fill leaves it.
        (should-fill-same-from-every-position text-expected 1 text-expected)
        (should-fill-same-from-every-position text-initial-spaced 1 text-expected-spaced)
        (should-fill-same-from-every-position text-expected-spaced 1 text-expected-spaced)))))

(ert-deftest fill-syntax-bounds-js-template-closer-alone-stays ()
  "A backtick closing a template string on a line of its own stays there.
Only quote characters were taken for a closer, so the backtick was read
as text and joined to the last line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "const s = `\n"
                 "bbb ccc ddd eee fff ggg hhh iii jjj kkk\n"
                 "lll\n"
                 "`\n"))
        (text-expected
         ;; format-next-line: off
         (concat "const s = `\n"
                 "bbb ccc ddd eee fff ggg hhh\n"
                 "iii jjj kkk lll\n"
                 "`\n"))
        ;; The same string with blank space after the closer.
        (text-initial-spaced
         ;; format-next-line: off
         (concat "const s = `\n"
                 "bbb ccc ddd eee fff ggg hhh iii jjj kkk\n"
                 "lll\n"
                 "`  \n"))
        (text-expected-spaced
         ;; format-next-line: off
         (concat "const s = `\n"
                 "bbb ccc ddd eee fff ggg hhh\n"
                 "iii jjj kkk lll\n"
                 "`  \n")))
    (with-temp-buffer
      (js-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial-spaced 1 text-expected-spaced)))))

(ert-deftest fill-syntax-bounds-js-template-opener-text-fills ()
  "Text after a backtick opening a template string fills with the body.
Only quote characters were taken for an opener, so the opener line was
left out of the paragraph, however long."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "const s = `aaa bbb ccc\n"
                 "ddd eee fff ggg hhh iii jjj kkk\n"
                 "lll\n"
                 "`;\n"))
        (text-expected
         ;; format-next-line: off
         (concat "const s = `aaa bbb ccc ddd eee\n"
                 "fff ggg hhh iii jjj kkk lll\n"
                 "`;\n"))
        ;; No code before the opener, its line longer than `fill-column'.
        (text-initial-bare
         ;; format-next-line: off
         (concat "`aaa bbb ccc ddd eee fff ggg hhh\n"
                 "iii jjj kkk lll mmm nnn ooo ppp\n"
                 "`\n"))
        (text-expected-bare
         ;; format-next-line: off
         (concat "`aaa bbb ccc ddd eee fff ggg\n"
                 "hhh iii jjj kkk lll mmm nnn\n"
                 "ooo ppp\n"
                 "`\n")))
    (with-temp-buffer
      (js-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial-bare 1 text-expected-bare)))))

(ert-deftest fill-syntax-bounds-python-docstring-summary-keeps-indent ()
  "A summary line wrapped above a blank line keeps the body indentation.
The indentation was taken from the line after the opener, blank in this
common layout, so wrapped summary lines lost theirs."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    Iii jjj.\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    Iii jjj.\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-python-docstring-summary-above-indented-example ()
  "A summary line wrapped above an indented example keeps the body indentation.
The indentation was taken from the first non-blank line after the
opener, an example indented deeper than the doc-string."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "        >>> example()\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "        >>> example()\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-python-string-closer-shallower-than-text ()
  "A string's wrapped first line keeps its text's indentation.
The closer line is less indented than the text, it was taken for the
indentation of the lines continuing the opener line.  A summary line on
its own has no text of its own to read, so it takes the closer's
indentation, with code after the closer too."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    parser.add(help=\"\"\"Aaa bbb ccc ddd eee fff.\n"
                 "        Ggg hhh iii jjj kkk.\n"
                 "    \"\"\")\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    parser.add(help=\"\"\"Aaa bbb ccc ddd\n"
                 "        eee fff. Ggg hhh iii jjj kkk.\n"
                 "    \"\"\")\n"))
        (text-initial-summary
         ;; format-next-line: off
         (concat "def f():\n"
                 "    parser.add(help=\"\"\"Aaa bbb ccc ddd eee fff.\n"
                 "\n"
                 "        Ggg hhh iii jjj kkk.\n"
                 "    \"\"\")\n"))
        (text-expected-summary
         ;; format-next-line: off
         (concat "def f():\n"
                 "    parser.add(help=\"\"\"Aaa bbb ccc ddd\n"
                 "    eee fff.\n"
                 "\n"
                 "        Ggg hhh iii jjj kkk.\n"
                 "    \"\"\")\n"))
        (text-initial-module
         ;; format-next-line: off
         (concat "x = \"\"\"Aaa bbb ccc ddd eee fff ggg hhh iii jjj.\n"
                 "    Kkk lll mmm.\n"
                 "\"\"\"\n"))
        (text-expected-module
         ;; format-next-line: off
         (concat "x = \"\"\"Aaa bbb ccc ddd eee fff ggg hhh\n"
                 "    iii jjj. Kkk lll mmm.\n"
                 "\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial-module 0 text-expected-module)
        (should-fill-same-from-every-position text-initial-summary 1 text-expected-summary)))))

(ert-deftest fill-syntax-bounds-elisp-docstring-summary-keeps-column-zero ()
  "A doc-string summary wrapped above body lines at column zero keeps it.
The closer line is indented deeper than the text, the least
indentation is used."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Aaa bbb ccc ddd eee fff ggg hhh iii.\n"
                 "Jjj kkk.\n"
                 "  \"\n"
                 "  nil)\n"))
        (text-expected
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Aaa bbb ccc ddd eee fff ggg\n"
                 "hhh iii. Jjj kkk.\n"
                 "  \"\n"
                 "  nil)\n")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-python-docstring-empty-line-summary-wraps ()
  "A summary wraps above an empty line, the only line before the closer line.
The empty line and the body on the closer line are kept as they are.
The string was taken for a two-line one and filled as a single
paragraph, dropping the empty line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii.\"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii.\"\"\"\n"))
        ;; The same docstring quoted with apostrophes.
        (text-initial-single
         ;; format-next-line: off
         (concat "def f():\n"
                 "    '''Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii.'''\n"))
        (text-expected-single
         ;; format-next-line: off
         (concat "def f():\n"
                 "    '''Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii.'''\n"))
        ;; The same docstring in a method, indented further.
        (text-initial-method
         ;; format-next-line: off
         (concat "class C:\n"
                 "    def f(self):\n"
                 "        \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "        Body iii.\"\"\"\n"))
        (text-expected-method
         ;; format-next-line: off
         (concat "class C:\n"
                 "    def f(self):\n"
                 "        \"\"\"Summary aaa bbb ccc ddd\n"
                 "        eee fff ggg hhh.\n"
                 "\n"
                 "        Body iii.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        ;; Nothing else needs filling.
        (should-fill-same-from-every-position text-initial 2 text-initial)
        (should-fill-same-from-every-position text-initial 3 text-initial)
        (should-fill-same-from-every-position text-initial-single 1 text-expected-single)
        (let ((fill-column 34))
          (should-fill-same-from-every-position text-initial-method 2 text-expected-method))))))

(ert-deftest fill-syntax-bounds-python-docstring-empty-line-body-wraps ()
  "A body on the closer line wraps below an empty line, which is kept.
The string was taken for a two-line one, the body beside its closer
left unfilled."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary.\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary.\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm\n"
                 "    nnn ooo ppp qqq.\"\"\"\n"))
        ;; No text beside the opener.
        (text-initial-bare-opener
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\"\"\n"))
        (text-expected-bare-opener
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm\n"
                 "    nnn ooo ppp qqq.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 3 text-expected)
        (should-fill-same-from-every-position text-initial 1 text-initial)
        (should-fill-same-from-every-position
         text-initial-bare-opener 3 text-expected-bare-opener)))))

(ert-deftest fill-syntax-bounds-python-module-docstring-empty-line ()
  "Each paragraph of a module docstring fills alone, the empty line kept.
Filling either merged both paragraphs, dropping the empty line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "\"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\"\"\n"))
        (text-expected-summary
         ;; format-next-line: off
         (concat "\"\"\"Summary aaa bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\"\"\n"))
        (text-expected-body
         ;; format-next-line: off
         (concat "\"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected-summary)
        (should-fill-same-from-every-position text-initial 1 text-initial)
        (should-fill-same-from-every-position text-initial 2 text-expected-body)))))

(ert-deftest fill-syntax-bounds-python-docstring-empty-line-closer-alone ()
  "A summary wraps above an empty line followed by a closer on its own line.
The string was taken for a two-line one, filling dropped the empty line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial 2 text-initial)
        (should-fill-same-from-every-position text-initial 3 text-initial)))))

(ert-deftest fill-syntax-bounds-elisp-docstring-empty-line ()
  "Each paragraph of a docstring fills alone, the empty line kept.
This is the usual layout of a short Emacs Lisp docstring.  The string
was taken for a two-line one, filling dropped the empty line or left
the body unfilled.  A closing parenthesis after the closer isn't code,
the body fills as it does without one."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial-summary
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii.\"\n"
                 "  nil)\n"))
        (text-expected-summary
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary aaa bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Body iii.\"\n"
                 "  nil)\n"))
        (text-initial-body
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\n"
                 "  nil)\n"))
        (text-expected-body
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.\"\n"
                 "  nil)\n"))
        (text-initial-defvar
         ;; format-next-line: off
         (concat "(defvar v nil\n"
                 "  \"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\")\n"))
        (text-expected-defvar
         ;; format-next-line: off
         (concat "(defvar v nil\n"
                 "  \"Summary aaa bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\")\n"))
        (text-expected-defvar-body
         ;; format-next-line: off
         (concat "(defvar v nil\n"
                 "  \"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.\")\n")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial-summary 1 text-expected-summary)
        (should-fill-same-from-every-position text-initial-summary 2 text-initial-summary)
        (should-fill-same-from-every-position text-initial-summary 3 text-initial-summary)
        (should-fill-same-from-every-position text-initial-body 3 text-expected-body)
        (should-fill-same-from-every-position text-initial-defvar 1 text-expected-defvar)
        (should-fill-same-from-every-position text-initial-defvar 3 text-expected-defvar-body)))))

(ert-deftest fill-syntax-bounds-string-escaped-newline-unchanged ()
  "A string continued with an escaped newline is left as it is.
Its lines can't be broken or joined without changing the string, a raw
newline is an error in C and Python, a joined escape is another
character.
A newline after an escaped backslash isn't escaped, nor is a newline
after another escape, both fill as before."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30))
    (dolist (case
             (list
              (list
               #'c-mode
               "s = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\";\n"
               "s = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\";\n")
              (list
               #'c-mode
               "s = \"aaa bbb ccc ddd eee fff ggg \\\nhhh \\\niii\";\n"
               "s = \"aaa bbb ccc ddd eee fff ggg \\\nhhh \\\niii\";\n")
              (list
               #'python-mode
               "x = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\" + y\n"
               "x = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\" + y\n")
              (list
               #'python-mode
               "x = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\"\n"
               "x = \"aaa bbb ccc ddd eee fff ggg \\\nhhh\"\n")
              ;; Preservation: no escaped newline.
              (list
               #'python-mode
               "x = \"\"\"aaa bbb ccc ddd eee fff ggg \\\\\nhhh\"\"\"\n"
               "x = \"\"\"aaa bbb ccc ddd eee fff\nggg \\\\ hhh\"\"\"\n")
              (list
               #'python-mode
               "x = \"\"\"aaa bbb\\n ccc ddd eee fff ggg\nhhh\"\"\"\n"
               "x = \"\"\"aaa bbb\\n ccc ddd eee\nfff ggg hhh\"\"\"\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-string-escaped-newline-other-paragraphs-fill ()
  "Paragraphs of a string without an escaped newline fill as before.
An escaped newline anywhere left the whole string as it was, an Emacs
Lisp doc-string opening with `\"\\' or a Python doc-string with an
example continued across lines for e.g.
The paragraph holding the escaped newline is still left as it is."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40))
    (dolist (case
             (let ((elisp-text
                    (concat
                     "(defun foo ()\n"
                     "  \"\\\n"
                     "Summary line.\n"
                     "\n"
                     "Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll mmm nnn ooo ppp.\"\n"
                     "  nil)\n"))
                   (python-text
                    (concat
                     "def f():\n"
                     "    \"\"\"Summary.\n"
                     "\n"
                     "    Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll.\n"
                     "\n"
                     "    Example: x = 1 + \\\n"
                     "    2\n"
                     "    \"\"\"\n")))
               (list
                (list
                 #'emacs-lisp-mode elisp-text 4
                 (concat
                  "(defun foo ()\n"
                  "  \"\\\n"
                  "Summary line.\n"
                  "\n"
                  "Aaa bbb ccc ddd eee fff ggg hhh iii jjj\n"
                  "kkk lll mmm nnn ooo ppp.\"\n"
                  "  nil)\n"))
                (list
                 #'python-mode python-text 3
                 (concat
                  "def f():\n"
                  "    \"\"\"Summary.\n"
                  "\n"
                  "    Aaa bbb ccc ddd eee fff ggg hhh iii\n"
                  "    jjj kkk lll.\n"
                  "\n"
                  "    Example: x = 1 + \\\n"
                  "    2\n"
                  "    \"\"\"\n"))
                ;; Preservation: the paragraph holding the escaped newline.
                (list #'emacs-lisp-mode elisp-text 2 elisp-text)
                (list #'python-mode python-text 5 python-text)
                (list #'python-mode python-text 6 python-text))))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) (nth 2 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-string-one-line-unchanged ()
  "A string on one line is left as it is, unless its delimiter is repeated.
It was filled as prose, breaking it across lines, an error in Python
and a changed value in shell.  An Emacs Lisp docstring lost its
indentation.  A repeated delimiter (Python's triple quotes for e.g.)
opens a string that may span lines, filled as before.  An empty string
glued before the text isn't one, though its quotes run on."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'python-mode
               0
               "x = \"aaa bbb ccc ddd eee fff ggg hhh\"\n"
               "x = \"aaa bbb ccc ddd eee fff ggg hhh\"\n")
              (list
               #'python-mode
               0
               "x = 'aaa bbb ccc ddd eee fff ggg hhh'\n"
               "x = 'aaa bbb ccc ddd eee fff ggg hhh'\n")
              (list
               #'sh-mode
               0
               "echo \"aaa bbb ccc ddd eee fff ggg hhh\"\n"
               "echo \"aaa bbb ccc ddd eee fff ggg hhh\"\n")
              (list
               #'emacs-lisp-mode
               1
               "(defun f ()\n  \"Aaa bbb ccc ddd eee fff ggg hhh.\"\n  nil)\n"
               "(defun f ()\n  \"Aaa bbb ccc ddd eee fff ggg hhh.\"\n  nil)\n")
              ;; An empty string beside the text isn't a repeated delimiter.
              (list
               #'python-mode
               0
               "x = \"\" + \"aaa bbb ccc ddd eee fff ggg hhh\"\n"
               "x = \"\" + \"aaa bbb ccc ddd eee fff ggg hhh\"\n")
              ;; Nor is one glued to it.
              (list
               #'sh-mode
               0
               "printf \"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\n"
               "printf \"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\n")
              (list
               #'c-mode
               0
               "x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh\";\n"
               "x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh\";\n")
              ;; Preservation: a repeated delimiter.
              (list
               #'python-mode
               0
               "x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\"\"\n"
               "x = \"\"\"aaa bbb ccc ddd eee fff\nggg hhh\"\"\"\n")
              (list
               #'python-mode
               0
               "x = '''aaa bbb ccc ddd eee fff ggg hhh'''\n"
               "x = '''aaa bbb ccc ddd eee fff\nggg hhh'''\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-string-caret-delimiter-one-line-unchanged ()
  "A string delimited by `^' on one line is left as it is, as one by `!' is.
The delimiter was given to `skip-chars-forward' as written, a `^'
negating the set, so the run of delimiters took the whole line and the
string was taken for a repeated delimiter, its line wrapped."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20))
    (with-temp-buffer
      (perl-mode)
      (let ((inhibit-message t))
        (dolist (text
                 (list
                  "my $x = q^aaa bbb ccc ddd eee fff ggg^;\n"
                  "my $x = q!aaa bbb ccc ddd eee fff ggg!;\n"))
          (should-fill-same-from-every-position text 0 text))))))

(ert-deftest fill-syntax-bounds-string-empty-line-other-modes ()
  "An empty line as the only line before the closer line is kept in other modes.
Covers a C string and JavaScript template strings, with and without code
around them.  A semicolon glued to the closer isn't code, the body
fills."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial-c
         ;; format-next-line: off
         (concat "char *s = \"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii.\";\n"))
        (text-expected-c
         ;; format-next-line: off
         (concat "char *s = \"Summary aaa bbb ccc\n"
                 "ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii.\";\n"))
        (text-initial-js
         ;; format-next-line: off
         (concat "const s = `Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.`;\n"))
        (text-expected-js
         ;; format-next-line: off
         (concat "const s = `Summary aaa bbb ccc\n"
                 "ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.`;\n"))
        (text-expected-js-body
         ;; format-next-line: off
         (concat "const s = `Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.`;\n"))
        ;; No code around the string, so the body fills too.
        (text-initial-js-bare
         ;; format-next-line: off
         (concat "`Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.`\n"))
        (text-expected-js-bare-summary
         ;; format-next-line: off
         (concat "`Summary aaa bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.`\n"))
        (text-expected-js-bare-body
         ;; format-next-line: off
         (concat "`Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.`\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial-c 0 text-expected-c)
        ;; Code after the closer, the body stays as it is.
        (should-fill-same-from-every-position text-initial-c 2 text-initial-c)))
    (with-temp-buffer
      (js-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial-js 0 text-expected-js)
        (should-fill-same-from-every-position text-initial-js 2 text-expected-js-body)
        (should-fill-same-from-every-position text-initial-js-bare 0 text-expected-js-bare-summary)
        (should-fill-same-from-every-position text-initial-js-bare 1 text-initial-js-bare)
        (should-fill-same-from-every-position
         text-initial-js-bare 2 text-expected-js-bare-body)))))

(ert-deftest fill-syntax-bounds-string-one-line-code-before-opener-untouched ()
  "Code before a string on one line is left as it is, the string's text wraps.
The line was filled as prose, reflowing the code with the text, an
error in Python.  A tab before the opener was measured as one column, so
the text wrapped short.  Later lines continue at the line's indentation,
the last cases are preservation cases."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               0
               "result_value = prefix_string + \"\"\"aaa bbb\"\"\"\n"
               "result_value = prefix_string + \"\"\"aaa\nbbb\"\"\"\n")
              (list
               0
               "value = compute(1, 2) + \"\"\"aaa bbb ccc\"\"\"\n"
               "value = compute(1, 2) + \"\"\"aaa\nbbb ccc\"\"\"\n")
              (list
               0
               "x =\t\"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\"\"\n"
               "x =\t\"\"\"aaa bbb ccc ddd eee\nfff ggg hhh\"\"\"\n")
              ;; Preservation.
              (list
               0
               "    x = - \"\"\"aaa bbb ccc ddd eee fff ggg hhh\"\"\"\n"
               "    x = - \"\"\"aaa bbb ccc ddd\n    eee fff ggg hhh\"\"\"\n")
              (list
               1
               "def f():\n    \"\"\"Aaa bbb ccc ddd eee fff ggg hhh.\"\"\"\n    pass\n"
               "def f():\n    \"\"\"Aaa bbb ccc ddd eee fff\n    ggg hhh.\"\"\"\n    pass\n")))
      (with-temp-buffer
        (python-mode)
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) (nth 0 case) (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-two-line-string-continues-at-second-line-indent ()
  "A string on two lines, text on both, wraps at its second line's indentation.
The opener line was grouped apart by its indentation, so its wrapped
text took the opener's indentation and the second line was not joined.
Two-line strings whose lines share an indentation fill as before."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 40)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'emacs-lisp-mode
               (concat
                "(defun f ()\n"
                "  \"Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll\n"
                "mmm.\"\n"
                "  nil)\n")
               (concat
                "(defun f ()\n"
                "  \"Aaa bbb ccc ddd eee fff ggg hhh iii\n"
                "jjj kkk lll mmm.\"\n"
                "  nil)\n"))
              (list
               #'python-mode
               (concat
                "def f():\n"
                "    x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh iii\n"
                "    jjj kkk.\"\"\"\n")
               (concat
                "def f():\n"
                "    x = \"\"\"aaa bbb ccc ddd eee fff ggg\n"
                "    hhh iii jjj kkk.\"\"\"\n"))
              (list
               #'org-mode
               (concat "\"Aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll\n" "mmm.\"\n")
               (concat "\"Aaa bbb ccc ddd eee fff ggg hhh iii jjj\n" "kkk lll mmm.\"\n"))))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (buffer-reset-text (nth 1 case))
          (goto-char (point-min))
          (search-forward "aa" nil nil)
          (should (fancy-fill-paragraph))
          (should (equal (nth 2 case) (buffer-string))))))))

(ert-deftest fill-syntax-bounds-python-docstring-opener-dot-point-continuation ()
  "A dot-point on the opener line keeps the line continuing it as its own.
A string on two lines, the second hangs past the marker, its
indentation was taken for the continuation, so the fill hung it again.
The last cases are preservation cases, a dot-point or text at the
opener line's indentation is not taken to hang."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist
        (case
         (list
          (list
           "    \"\"\"- aaa bbb ccc ddd eee fff\n      ggg hhh iii jjjjjj \"\"\"\n"
           "    \"\"\"- aaa bbb ccc ddd eee\n      fff ggg hhh iii jjjjjj\n    \"\"\"\n")
          (list
           "    \"\"\"1. aaa bbb ccc ddd eee fff\n       ggg hhh iii jjjjjj \"\"\"\n"
           "    \"\"\"1. aaa bbb ccc ddd eee\n       fff ggg hhh iii jjjjjj\n    \"\"\"\n")
          ;; Preservation.
          (list
           "    \"\"\"- aaa bbb ccc ddd eee fff\n      ggg hhh iii jjjjjj\n    - kkk\n    \"\"\"\n"
           "    \"\"\"- aaa bbb ccc ddd eee\n      fff ggg hhh iii jjjjjj\n    - kkk\n    \"\"\"\n")
          (list
           "    \"\"\"- aaa bbb ccc ddd eee fff\n    ggg hhh iii jjjjjj\n    \"\"\"\n"
           "    \"\"\"- aaa bbb ccc ddd eee\n      fff\n    ggg hhh iii jjjjjj\n    \"\"\"\n")))
      (with-temp-buffer
        (python-mode)
        (let ((inhibit-message t))
          (dotimes (line-index 2)
            (should-fill-same-from-every-position (nth 0 case) line-index (nth 1 case))))))))

(ert-deftest fill-syntax-bounds-two-line-region-fills-as-longer ()
  "A string on two lines fills as a longer one of the same shape.
Each case is followed by its longer form for contrast.  A string's
closer alone on its line took the prose fill, continuing at the opener
line's indentation, after code it broke the line before the opener.  It
now continues at the closer's indentation, as with a line between them."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              ;; A string's closer alone.
              (list
               #'python-mode
               '(1 2)
               nil
               "def f():\n    \"\"\"aaa bbb ccc ddd eee fff\n\"\"\"\n"
               "def f():\n    \"\"\"aaa bbb ccc\nddd eee fff\n\"\"\"\n")
              (list
               #'python-mode
               '(1)
               nil
               "def f():\n    \"\"\"aaa bbb ccc ddd eee fff\n\n\"\"\"\n"
               "def f():\n    \"\"\"aaa bbb ccc\nddd eee fff\n\n\"\"\"\n")
              (list
               #'python-mode
               '(0 1)
               nil
               "x = 1 + \"\"\"aaa bbb ccc ddd eee\n    \"\"\"\n"
               "x = 1 + \"\"\"aaa bbb\n    ccc ddd eee\n    \"\"\"\n")
              (list
               #'python-mode
               '(0)
               nil
               "x = 1 + \"\"\"aaa bbb ccc ddd eee\n\n    \"\"\"\n"
               "x = 1 + \"\"\"aaa bbb\n    ccc ddd eee\n\n    \"\"\"\n")
              (list
               #'emacs-lisp-mode
               '(1 2)
               nil
               "(defun f ()\n  \"aaa bbb ccc ddd eee fff ggg hhh\n\"\n  nil)\n"
               "(defun f ()\n  \"aaa bbb ccc ddd\neee fff ggg hhh\n\"\n  nil)\n")
              (list
               #'emacs-lisp-mode
               '(1)
               nil
               "(defun f ()\n  \"aaa bbb ccc ddd eee fff ggg hhh\n\n\"\n  nil)\n"
               "(defun f ()\n  \"aaa bbb ccc ddd\neee fff ggg hhh\n\n\"\n  nil)\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (dolist (line-index (nth 1 case))
            (should-fill-same-from-every-position (nth 3 case) line-index (nth 4 case)))
          (when (nth 2 case)
            (should-fill-selection (nth 3 case) 0 (nth 4 case))))))))

(ert-deftest fill-syntax-bounds-buffer-local-case-fold-search-carried ()
  "A buffer-local `case-fold-search' applies to dot-points in a comment.
The comment is filled in another buffer, which used the default value,
so a prefix matched without case there, unlike in the buffer."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-dot-point-prefix (list "o "))
        (sentence-end-double-space nil)
        (fill-column 20))
    (with-temp-buffer
      (c-mode)
      (setq-local case-fold-search nil)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position
         "/* aaa bbb\n * O ccc ddd eee fff */\n" 0 "/* aaa bbb O ccc ddd\n * eee fff */\n")))))

(ert-deftest fill-syntax-bounds-buffer-local-fill-settings ()
  "Comments and strings fill with the buffer's own settings.
Their text was filled in a temporary buffer, seeing the default value
of `sentence-end-double-space', not the buffer's, while a closer after
the text and a string filled in place saw the buffer's.  Each case sets
the buffer's value against the default."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30))
    (dolist (case
             (list
              (list
               #'python-mode
               t
               nil
               "x = \"\"\"Aaa bbb. Ccc ddd. Eee fff ggg hhh iii.\"\"\"\n"
               "x = \"\"\"Aaa bbb. Ccc ddd.\nEee fff ggg hhh iii.\"\"\"\n")
              (list
               #'python-mode
               t
               nil
               "x = \"\"\"Aaa bbb. Ccc ddd. Eee fff ggg hhh iii\njjj kkk lll.\"\"\"\n"
               "x = \"\"\"Aaa bbb. Ccc ddd.\nEee fff ggg hhh iii jjj kkk\nlll.\"\"\"\n")
              (list
               #'c-mode
               nil
               t
               "/* Aaa bbb. Ccc ddd. Eee fff ggg hhh iii jjj kkk. */\n"
               "/* Aaa bbb.  Ccc ddd.  Eee fff\n * ggg hhh iii jjj kkk.  */\n")
              (list
               #'c-mode
               t
               nil
               "/* Aaa bbb. Ccc ddd. Eee fff ggg hhh iii jjj kkk. */\n"
               "/* Aaa bbb. Ccc ddd. Eee fff\n * ggg hhh iii jjj kkk. */\n")))
      (let ((sentence-end-double-space (nth 1 case)))
        (with-temp-buffer
          (funcall (nth 0 case))
          (setq-local sentence-end-double-space (nth 2 case))
          (let ((inhibit-message t))
            (should-fill-same-from-every-position (nth 3 case) 0 (nth 4 case))))))))

(ert-deftest fill-syntax-bounds-buffer-local-option-carried ()
  "This package's options local to the buffer apply to comments and strings.
They're found by name, so an option isn't missed for not being listed.
Missed, the text was filled with the default margin, while the closer
after it used the buffer's."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-fill-column-margin 0)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'c-mode
               "/* aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll */\n"
               "/* aaa bbb ccc ddd eee\n * fff ggg hhh iii jjj\n * kkk lll */\n")
              (list
               #'python-mode
               "x = \"\"\"aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk lll\"\"\"\n"
               "x = \"\"\"aaa bbb ccc ddd\neee fff ggg hhh iii jjj\nkkk lll\"\"\"\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (setq-local fancy-fill-paragraph-fill-column-margin 6)
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-default-fill-settings-unchanged ()
  "Comments and strings in a buffer without local fill settings use the defaults.
Carried over to the buffer the text is filled in, a value bound around
the fill applies as before."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30))
    (dolist (case
             (list
              (list
               #'python-mode
               t
               "x = \"\"\"Aaa bbb. Ccc ddd. Eee fff ggg hhh iii.\"\"\"\n"
               "x = \"\"\"Aaa bbb.  Ccc ddd.\nEee fff ggg hhh iii.\"\"\"\n")
              (list
               #'python-mode
               nil
               "x = \"\"\"Aaa bbb. Ccc ddd. Eee fff ggg hhh iii.\"\"\"\n"
               "x = \"\"\"Aaa bbb. Ccc ddd.\nEee fff ggg hhh iii.\"\"\"\n")
              (list
               #'c-mode
               t
               "/* Aaa bbb. Ccc ddd. Eee fff ggg hhh iii jjj kkk. */\n"
               "/* Aaa bbb.  Ccc ddd.  Eee fff\n * ggg hhh iii jjj kkk.  */\n")
              (list
               #'c-mode
               nil
               "/* Aaa bbb. Ccc ddd. Eee fff ggg hhh iii jjj kkk. */\n"
               "/* Aaa bbb. Ccc ddd. Eee fff\n * ggg hhh iii jjj kkk. */\n")))
      (let ((sentence-end-double-space (nth 1 case)))
        (with-temp-buffer
          (funcall (nth 0 case))
          (let ((inhibit-message t))
            (should-fill-same-from-every-position (nth 2 case) 0 (nth 3 case))))))))

(ert-deftest fill-syntax-bounds-closer-before-closing-delimiters-fills ()
  "A closer followed by closing delimiters fills as one followed by nothing.
Any text after the closer was taken for code, so a closer line ending
an expression (`\")' ending an Emacs Lisp doc-string, `\"\"\")' or `\"\"\",'
in Python, `);' or `*/);' for e.g.) was never filled.  Glued to the
closer, the delimiters are read from the syntax table and move with it.
With the cursor on such a line, its last character is past the closer,
so the string before it is found too.  The last cases are preservation
cases, code after the closer is left alone, as is a Lisp string on one
line."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'emacs-lisp-mode
               3
               "(defvar foo nil\n  \"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.\")\n"
               "(defvar foo nil\n  \"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii\njjj kkk.\")\n")
              (list
               #'emacs-lisp-mode
               2
               "(defvar foo nil\n  \"Aaa bbb.\nccc ddd eee fff ggg hhh iii jjj kkk.\")\n"
               "(defvar foo nil\n  \"Aaa bbb. ccc ddd eee fff\nggg hhh iii jjj kkk.\")\n")
              (list
               #'python-mode
               2
               "x = f(\"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.\"\"\")\n"
               "x = f(\"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii\njjj kkk.\"\"\")\n")
              (list
               #'python-mode
               3
               "x = [\n    \"\"\"Aaa bbb.\n\n    ccc ddd eee fff ggg hhh iii jjj kkk.\"\"\",\n]\n"
               (concat
                "x = [\n    \"\"\"Aaa bbb.\n\n    ccc ddd eee fff ggg hhh\n"
                "    iii jjj kkk.\"\"\",\n]\n"))
              (list
               #'python-mode
               0
               "x = f(\"\"\"aaa bbb ccc ddd eee fff ggg hhh iii\"\"\")\n"
               "x = f(\"\"\"aaa bbb ccc ddd eee\nfff ggg hhh iii\"\"\")\n")
              (list
               #'js-mode
               2
               "foo(`Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.`);\n"
               "foo(`Aaa bbb.\n\nccc ddd eee fff ggg hhh iii\njjj kkk.`);\n")
              (list
               #'c-mode
               0
               "foo(/* aaa bbb\n       ccc ddd eee fff ggg hhh iii */);\n"
               "foo(/* aaa bbb ccc ddd eee fff\n     * ggg hhh iii */);\n")
              ;; A closer that doesn't fit wraps alone, its delimiters with it,
              ;; not after the continuation prefix as a word (` * */);').
              (list
               #'c-mode
               1
               "foo(a, /* aaa bbb ccc ddd\n       eee fff ggg hhh iii jjj */);\n"
               "foo(a, /* aaa bbb ccc ddd eee\n        * fff ggg hhh iii jjj\n        */);\n")
              ;; Preservation.
              (list
               #'python-mode
               2
               "x = \"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.\"\"\" + y\n"
               "x = \"\"\"Aaa bbb.\n\nccc ddd eee fff ggg hhh iii jjj kkk.\"\"\" + y\n")
              (list
               #'emacs-lisp-mode
               0
               "(message \"aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk\")\n"
               "(message \"aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk\")\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-region-docstring-empty-line ()
  "Selecting a whole docstring fills each paragraph, the empty line kept.
The selection filled both paragraphs as one, dropping the empty line,
and a second fill changed the result again."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial-python
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\"\"\n"))
        (text-expected-python
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "    Body iii jjj kkk lll mmm\n"
                 "    nnn ooo ppp qqq.\"\"\"\n"))
        (text-initial-elisp
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo ppp qqq.\"\n"
                 "  nil)\n"))
        (text-expected-elisp
         ;; format-next-line: off
         (concat "(defun f ()\n"
                 "  \"Summary aaa bbb ccc ddd eee\n"
                 "fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn\n"
                 "ooo ppp qqq.\"\n"
                 "  nil)\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial-python 1 text-expected-python)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (should-fill-selection text-initial-elisp 1 text-expected-elisp)))))

(ert-deftest fill-syntax-bounds-block-comment-empty-line-body-tab-indent ()
  "A tab indented block comment starting its body with an empty line keeps tabs.
The indentation was read from the empty line, which holds none, so the
continuation lines were indented with spaces."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (indent-tabs-mode t)
        (tab-width 8)
        (text-initial
         ;; format-next-line: off
         (concat "void f() {\n"
                 "\t/* Summary aaa bbb ccc ddd eee fff ggg.\n"
                 "\n"
                 "\t */\n"
                 "}\n"))
        (text-expected
         ;; format-next-line: off
         (concat "void f() {\n"
                 "\t/* Summary aaa bbb ccc\n"
                 "\t * ddd eee fff ggg.\n"
                 "\n"
                 "\t */\n"
                 "}\n"))
        (text-initial-longer
         ;; format-next-line: off
         (concat "void f() {\n"
                 "\t/* Summary aaa bbb ccc ddd eee fff ggg.\n"
                 "\n"
                 "\t   Body.\n"
                 "\t */\n"
                 "}\n"))
        (text-expected-longer
         ;; format-next-line: off
         (concat "void f() {\n"
                 "\t/* Summary aaa bbb ccc\n"
                 "\t * ddd eee fff ggg.\n"
                 "\n"
                 "\t   Body.\n"
                 "\t */\n"
                 "}\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)
        (should-fill-same-from-every-position text-initial-longer 1 text-expected-longer)))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-empty-line-body ()
  "A block comment with a lone empty body line fills as a longer one does.
The empty line is kept, as it is when more body lines follow.  Filled
as a two-line comment, the empty line became a ` *' separator and the
closer line took ` * ' in place of its own prefix."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "   Body iii. */\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee\n"
                 " * fff ggg hhh.\n"
                 "\n"
                 "   Body iii. */\n"))
        ;; The same comment with the closer on a line of its own.
        (text-initial-longer
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "   Body iii.\n"
                 " */\n"))
        (text-expected-longer
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee\n"
                 " * fff ggg hhh.\n"
                 "\n"
                 "   Body iii.\n"
                 " */\n"))
        (text-initial-javadoc
         ;; format-next-line: off
         (concat "/** Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 " * Body iii. */\n"))
        (text-expected-javadoc
         ;; format-next-line: off
         (concat "/** Summary aaa bbb ccc ddd\n"
                 " * eee fff ggg hhh.\n"
                 "\n"
                 " * Body iii. */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (should-fill-same-from-every-position text-initial 2 text-initial)
        (should-fill-same-from-every-position text-initial-longer 0 text-expected-longer)
        (should-fill-same-from-every-position text-initial-javadoc 0 text-expected-javadoc)))))

(ert-deftest fill-syntax-bounds-block-comment-empty-line-body-code-after-closer ()
  "A block comment with a lone empty body line and code after its closer.
Its text fills as a longer comment's does, the closer line and the code
after it are left as they are."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "   Body iii jjj kkk lll mmm nnn ooo. */ int x = 1;\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/* Summary aaa bbb ccc ddd eee\n"
                 " * fff ggg hhh.\n"
                 "\n"
                 "   Body iii jjj kkk lll mmm nnn ooo. */ int x = 1;\n"))
        (text-initial-html
         ;; format-next-line: off
         (concat "<!-- Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo. --> <p>x</p>\n"))
        (text-expected-html
         ;; format-next-line: off
         (concat "<!-- Summary aaa bbb ccc ddd\n"
                 " !-- eee fff ggg hhh.\n"
                 "\n"
                 "Body iii jjj kkk lll mmm nnn ooo. --> <p>x</p>\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (should-fill-same-from-every-position text-initial 2 text-initial)))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial-html 0 text-expected-html)
        (should-fill-same-from-every-position text-initial-html 2 text-initial-html)))))

(ert-deftest fill-syntax-bounds-block-comment-unprefixed-closer-line-joins ()
  "A closer line with text joins a body laid out without the ` * ' prefix.
Only a closer line starting with the continuation prefix joined, so in
a GNU style comment the text beside the closer was never filled.  Once
an empty line counted as a body, a three-line comment of this shape was
not filled either.  A closer alone on its line stays there."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'c-mode
               (concat "/* Aaa bbb.\n" "\n" "   Ccc ddd eee fff ggg hhh iii. */\n")
               (concat "/* Aaa bbb.\n" "\n" "   Ccc ddd eee fff\n" "   ggg hhh iii. */\n"))
              (list
               #'c-mode
               (concat
                "/* Aaa bbb.\n" "\n" "   Ccc ddd eee fff\n" "   ggg hhh iii jjj kkk lll. */\n")
               (concat
                "/* Aaa bbb.\n"
                "\n"
                "   Ccc ddd eee fff\n"
                "   ggg hhh iii jjj\n"
                "   kkk lll. */\n"))
              (list
               #'c-mode
               (concat "/* Aaa bbb.\n" "\n" "   Ccc ddd eee fff ggg hhh iii.\n" "   */\n")
               (concat "/* Aaa bbb.\n" "\n" "   Ccc ddd eee fff\n" "   ggg hhh iii.\n" "   */\n"))
              ;; Closers ending in a close parenthesis rather than
              ;; punctuation.
              (list
               #'pascal-mode
               (concat "(* Aaa bbb.\n" "\n" "   Ccc ddd eee fff ggg hhh iii. *)\n")
               (concat "(* Aaa bbb.\n" "\n" "   Ccc ddd eee fff\n" "   ggg hhh iii. *)\n"))
              (list
               #'pascal-mode
               (concat "(* Aaa bbb.\n" "\n" "   Ccc ddd eee fff ggg hhh iii.\n" "   *)\n")
               (concat "(* Aaa bbb.\n" "\n" "   Ccc ddd eee fff\n" "   ggg hhh iii.\n" "   *)\n"))
              (list
               #'pascal-mode
               (concat "{ Aaa bbb.\n" "\n" "  Ccc ddd eee fff ggg hhh iii.\n" "  }\n")
               (concat "{ Aaa bbb.\n" "\n" "  Ccc ddd eee fff\n" "  ggg hhh iii.\n" "  }\n"))
              (list
               #'html-mode
               (concat "<!-- Aaa bbb.\n" "\n" "     Ccc ddd eee fff ggg hhh. -->\n")
               (concat "<!-- Aaa bbb.\n" "\n" "     Ccc ddd eee fff\n" "     ggg hhh. -->\n"))))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 1 case) 2 (nth 2 case))
          (should-fill-selection (nth 1 case) 0 (nth 2 case)))))))

(ert-deftest fill-syntax-bounds-block-comment-unprefixed-closer-line-indent ()
  "A closer line with text joins an unprefixed body however it is indented.
Shallower or deeper than the body's text, it takes the continuation
prefix the body's lines are given."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               (concat "/* Aaa bbb.\n" "   Ccc ddd eee fff ggg hhh iii.\n" "   Jjj kkk. */\n")
               (concat
                "/* Aaa bbb.\n" " * Ccc ddd eee fff\n" " * ggg hhh iii.\n" " * Jjj kkk. */\n"))
              (list
               (concat "/* Aaa bbb.\n" "   Ccc ddd eee fff ggg hhh iii.\n" "Jjj kkk. */\n")
               (concat
                "/* Aaa bbb.\n" " * Ccc ddd eee fff\n" " * ggg hhh iii.\n" " * Jjj kkk. */\n"))
              (list
               (concat "/* Aaa bbb.\n" "   Ccc ddd eee fff ggg hhh iii.\n" "       Jjj kkk. */\n")
               (concat
                "/* Aaa bbb.\n" " * Ccc ddd eee fff\n" " * ggg hhh iii.\n" " * Jjj kkk. */\n"))))
      (with-temp-buffer
        (c-mode)
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 0 case) 0 (nth 1 case))
          (should-fill-selection (nth 0 case) 0 (nth 1 case)))))))

(ert-deftest fill-syntax-bounds-block-comment-empty-separator-closer-line ()
  "An empty separator line leaves the last paragraph as a ` *' one does.
The whole body was checked for the ` * ' prefix, which an empty line
lacks, so the closer line joined the last paragraph and its prefixed
lines were no longer joined.  Only the paragraph the closer joins is
relevant.  The closer line is indented past the prefix, so it joins too,
as it does a comment spanning two lines."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 70)
        (sentence-end-double-space nil))
    (dolist (separator (list "" " *"))
      (with-temp-buffer
        (c-mode)
        (let ((inhibit-message t)
              (text-initial
               ;; format-next-line: off
               (concat "/*\n"
                       " * aaa\n"
                       separator "\n"
                       " * bbb bbb\n"
                       " * ccc\n"
                       "   ddd */\n"))
              (text-expected
               ;; format-next-line: off
               (concat "/*\n"
                       " * aaa\n"
                       separator "\n"
                       " * bbb bbb ccc ddd */\n")))
          (should-fill-same-from-every-position text-initial 3 text-expected)
          (should-fill-selection text-initial 0 text-expected))))))

(ert-deftest fill-syntax-bounds-python-docstring-empty-line-neighbours-unchanged ()
  "A two-line docstring fills as one paragraph, two empty lines are kept.
So is an empty line holding blank-space.  These sit beside a lone empty
line, whose handling must not change them."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial-two-line
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "    Body iii.\"\"\"\n"))
        (text-expected-two-line
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc\n"
                 "    ddd eee fff ggg hhh.\n"
                 "    Body iii.\"\"\"\n"))
        (text-initial-two-empty
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "\n"
                 "\n"
                 "    Body iii.\"\"\"\n"))
        (text-expected-two-empty
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "\n"
                 "\n"
                 "    Body iii.\"\"\"\n"))
        (text-initial-spaced
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "    \n"
                 "    Body iii.\"\"\"\n"))
        (text-expected-spaced
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Summary aaa bbb ccc ddd\n"
                 "    eee fff ggg hhh.\n"
                 "    \n"
                 "    Body iii.\"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial-two-line 1 text-expected-two-line)
        (should-fill-same-from-every-position text-initial-two-line 2 text-expected-two-line)
        (should-fill-same-from-every-position text-initial-two-empty 1 text-expected-two-empty)
        (should-fill-same-from-every-position text-initial-spaced 1 text-expected-spaced)))))

(ert-deftest fill-syntax-bounds-python-docstring-cursor-on-closer-line ()
  "A cursor on the closing quotes fills the docstring they close.
The quotes after the closing fence read as an empty string, which was
taken for the region at point instead."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 24)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"\n"
                 "    Alpha bravo charlie delta echo foxtrot golf.\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"\n"
                 "    Alpha bravo charlie\n"
                 "    delta echo foxtrot\n"
                 "    golf.\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 3 text-expected)))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-cursor-on-closer-line ()
  "A cursor on a ` */' closer line fills the paragraph above it."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* aaa\n"
                 " * bbb ccc ddd eee fff ggg hhh\n"
                 " */\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/* aaa bbb ccc ddd\n"
                 " * eee fff ggg hhh\n"
                 " */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 2 text-expected)))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-closer-without-prefix-kept ()
  "A closer line not starting with the continuation prefix is kept as it is.
Filled as a body line, its prefix width would be stripped, cutting into
a closer such as `  */' and leaving the comment unclosed.  Text before
the closer joins the body, it loses only its blank-space."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (closer '("  */" "   */" "    */" " **/"))
          (dolist (opener '("/*" "/**"))
            (should-fill-same-from-every-position
             ;; format-next-line: off
             (concat opener "\n"
                     " * bbb ccc ddd eee fff ggg hhh\n"
                     closer "\n")
             1
             ;; format-next-line: off
             (concat opener "\n"
                     " * bbb ccc ddd eee\n"
                     " * fff ggg hhh\n"
                     closer "\n")))
          ;; Opener text takes another path, which must keep the line too.
          (should-fill-same-from-every-position
           ;; format-next-line: off
           (concat "/* aaa\n"
                   " * bbb ccc ddd eee fff ggg hhh\n"
                   closer "\n")
           1
           ;; format-next-line: off
           (concat "/* aaa bbb ccc ddd\n"
                   " * eee fff ggg hhh\n"
                   closer "\n")))
        (dolist (opener '("/*" "/**"))
          (should-fill-same-from-every-position
           (concat opener "\n * bbb ccc ddd eee fff ggg hhh\n  more */\n")
           1
           (concat opener "\n * bbb ccc ddd eee\n * fff ggg hhh more\n */\n")))
        (should-fill-same-from-every-position
         "/* aaa\n * bbb ccc ddd eee fff ggg hhh\n  more */\n"
         1
         "/* aaa bbb ccc ddd\n * eee fff ggg hhh\n * more */\n")))))

(ert-deftest fill-syntax-bounds-block-comment-region-closing-elsewhere-errors ()
  "A comment region its comment doesn't close in is an error, not filled.
Only the region's end was checked to be within the comment, so a region
ending before the comment's closer line was accepted.  Its last line is
taken for the closer line, filling it could join the code after the
region as comment text."
  (with-temp-buffer
    (c-mode)
    (let ((inhibit-message t))
      (insert "/* aaa\n * bbb */\n")
      (syntax-propertize (point-max))
      (should-error
       (fancy-fill-paragraph--fill-block-comment-region
        (point-min)
        (save-excursion
          (goto-char (point-min))
          (pos-eol))
        (point-min))))))

(ert-deftest fill-syntax-bounds-block-comment-closer-line-without-prefix-joins ()
  "A closer line without the prefix joins a body with the prefix.
It was kept, while a comment spanning two lines joined it, so a fill
leaving two lines had a second fill join it.  Stripped of its
blank-space alone, it loses no text, at any indentation."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   40
                   0
                   "/* Aaa bbb\n * Ccc ddd.\n   Eee fff. */\n"
                   "/* Aaa bbb Ccc ddd. Eee fff. */\n")
                  (list
                   40
                   2
                   "/* Aaa.\n\n * Bbb ccc.\n   Ddd eee. */\n"
                   "/* Aaa.\n\n * Bbb ccc. Ddd eee. */\n")
                  (list
                   40
                   2
                   "/* Aaa.\n\n * Bbb ccc.\n\tDdd eee. */\n"
                   "/* Aaa.\n\n * Bbb ccc. Ddd eee. */\n")
                  (list
                   20
                   2
                   "/* Aaa.\n\n * Bbb ccc ddd\n   eee fff ggg. */\n"
                   "/* Aaa.\n\n * Bbb ccc ddd eee\n * fff ggg. */\n")
                  (list 40 2 "/* Aaa.\n\n * Bbb ccc.\n  x */\n" "/* Aaa.\n\n * Bbb ccc. x */\n")))
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case))
            (should-fill-selection (nth 2 case) 0 (nth 3 case))))))))

(ert-deftest fill-syntax-bounds-block-comment-closer-line-fills-stable ()
  "A closer line is joined or kept as a second fill would, its text kept.
Kept or joined by the layout of the body before the fill, which the
fill then changed, a second fill could decide otherwise.  Lines without
the prefix were stripped of its width, a deeper one wasn't joined but
taken for an indented paragraph, others filled as prose, without the
prefix a second fill gave them."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list 40 1 "/*\n * bbb ccc\n      ddd */\n" "/*\n * bbb ccc ddd */\n")
                  (list
                   30
                   1
                   "/*\n * aaa bbb ccc ddd eee fff ggg\n   hhh iii\n   jjj */\n"
                   "/*\n * aaa bbb ccc ddd eee fff ggg\n * hhh iii jjj */\n")
                  (list
                   40
                   0
                   "/* aaa bbb ccc ddd eee fff\n   ggg hhh\n * iii */\n"
                   "/* aaa bbb ccc ddd eee fff ggg hhh iii\n */\n")
                  (list
                   40
                   0
                   "/* aaa bbb ccc\n * ddd eee fff\n  ggg.  */\n"
                   "/* aaa bbb ccc ddd eee fff ggg. */\n")))
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case))
            (should-fill-selection (nth 2 case) 0 (nth 3 case))))))))

(ert-deftest fill-syntax-bounds-block-comment-paragraph-starting-unprefixed-stable ()
  "A paragraph with the prefix past its first line fills with the prefix.
Its first line had to carry it, so a paragraph starting without it was
filled as prose, losing the first line's indentation, which a second
fill took for a paragraph separator."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   40
                   "/*\n    aaa\n * bbb ccc\n * ddd eee fff ggg\n */\n"
                   "/*\n * aaa bbb ccc ddd eee fff ggg\n */\n")
                  (list
                   30
                   "\t/*\n\t   aaa.\n\t * bbb ccc ddd eee fff ggg hhh.\n\t  iii jjj */\n"
                   "\t/*\n\t * aaa. bbb ccc ddd\n\t * eee fff ggg hhh.\n\t * iii jjj */\n")))
          (let ((fill-column (nth 0 case)))
            (dotimes (line-index (length (split-string (nth 1 case) "\n" t)))
              (should-fill-same-from-every-position (nth 1 case) line-index (nth 2 case)))))))))

(ert-deftest fill-syntax-bounds-block-comment-banner-line-separates ()
  "A line of the prefix's mark alone separates paragraphs, it's left as it is.
A banner line (` ****' for e.g.) was taken for text in a paragraph with
the prefix, so it was given the prefix (` * ****').  The last cases are
preservation cases, text after marks is still text, and a string has no
mark to repeat."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  ;; Found in practice (`png.h').
                  (list
                   79 (list 1 2)
                   (concat
                    "/*****************************************************************\n"
                    " * Section 5: SIMPLIFIED API\n"
                    " *****************************************************************\n"
                    " *\n"
                    " * Please read the documentation.\n"
                    " */\n")
                   (concat
                    "/*****************************************************************\n"
                    " * Section 5: SIMPLIFIED API\n"
                    " *****************************************************************\n"
                    " *\n"
                    " * Please read the documentation.\n"
                    " */\n"))
                  ;; The paragraphs on either side fill on their own.
                  (list
                   20
                   (list 1)
                   "/*\n * aaa bbb ccc ddd eee\n ******\n * fff\n * ggg */\n"
                   "/*\n * aaa bbb ccc ddd\n * eee\n ******\n * fff\n * ggg */\n")
                  (list
                   20
                   (list 3 4)
                   "/*\n * aaa bbb ccc ddd eee\n ******\n * fff\n * ggg */\n"
                   "/*\n * aaa bbb ccc ddd eee\n ******\n * fff ggg */\n")
                  ;; Preservation.
                  (list
                   40
                   (list 1)
                   "/*\n * aaa bbb\n **bold** ccc\n */\n"
                   "/*\n * aaa bbb **bold** ccc\n */\n")))
          (let ((fill-column (nth 0 case)))
            (dolist (line-index (nth 1 case))
              (should-fill-same-from-every-position (nth 2 case) line-index (nth 3 case)))))
        ;; A selection fills the paragraphs on either side, the banner kept.
        (let ((fill-column 20))
          (should-fill-selection
           "/*\n * aaa bbb ccc ddd eee\n ******\n * fff\n * ggg */\n"
           0
           "/*\n * aaa bbb ccc ddd\n * eee\n ******\n * fff ggg */\n"))))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t)
            (fill-column 30))
        (should-fill-same-from-every-position
         "def f():\n    \"\"\"Aaa.\n\n    bbb ccc\n    ****\n    ddd\n    \"\"\"\n"
         3
         "def f():\n    \"\"\"Aaa.\n\n    bbb ccc **** ddd\n    \"\"\"\n")))))

(ert-deftest fill-syntax-bounds-block-comment-misaligned-prefix-stripped ()
  "A line whose prefix is a column off in a paragraph with the prefix loses it.
Only its blank-space was stripped, its mark taken for text, then for a
dot-point (` * * fff').  The last cases are preservation cases, a mark
glued to text is text, and without a line carrying the prefix a mark is
a dot-point."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (fill-column 40))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   "/* Aaa.\n *\n * bbb ccc ddd eee\n  * fff ggg hhh\n */\n"
                   "/* Aaa.\n *\n * bbb ccc ddd eee fff ggg hhh\n */\n")
                  (list
                   "/* Aaa.\n *\n * bbb ccc ddd eee\n* fff ggg hhh\n */\n"
                   "/* Aaa.\n *\n * bbb ccc ddd eee fff ggg hhh\n */\n")
                  ;; Preservation.
                  (list
                   "/* Aaa.\n *\n * bbb ccc ddd eee\n   *emph* fff\n */\n"
                   "/* Aaa.\n *\n * bbb ccc ddd eee *emph* fff\n */\n")))
          (dotimes (line-index 2)
            (should-fill-same-from-every-position (nth 0 case) (+ 2 line-index) (nth 1 case))))
        (should-fill-same-from-every-position
         "/* Some intro text:\n   * first item\n   * second item.  */\n"
         0
         "/* Some intro text:\n * * first item\n * * second item. */\n")))))

(ert-deftest fill-syntax-bounds-block-comment-wrapped-closer-alone ()
  "A closer wrapped onto a line of its own is laid out as a closer alone.
Joined to the last paragraph, the closer was filled as a word, so when
it didn't fit it took the continuation prefix (` * */').  A closer
already on a line of its own is kept as ` */', the fill matches it, as
it does a closer line written as ` * */'.  A prefix not starting the
closer is blanked, `pascal-mode' wrapped `|*)' after its `|' prefix.
The last C case was found in practice, the comma weight breaking the
first line early so the closer was all that didn't fit."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   16
                   nil
                   "/* aaa bbb ccc\n * ddd eee fff */\n"
                   "/* aaa bbb ccc\n * ddd eee fff\n */\n")
                  (list
                   16
                   nil
                   "/* aaa bbb ccc ddd eee fff */\n"
                   "/* aaa bbb ccc\n * ddd eee fff\n */\n")
                  ;; Already alone, for contrast.
                  (list
                   16
                   nil
                   "/* aaa bbb ccc\n * ddd eee fff\n */\n"
                   "/* aaa bbb ccc\n * ddd eee fff\n */\n")
                  ;; Written with the prefix.
                  (list
                   16
                   nil
                   "/* aaa bbb ccc\n * ddd eee fff\n * */\n"
                   "/* aaa bbb ccc\n * ddd eee fff\n */\n")
                  ;; Tabs indenting the prefix are kept.
                  (list
                   22
                   nil
                   "\t/* aaa bbb ccc\n\t * ddd eee fff */\n"
                   "\t/* aaa bbb ccc\n\t * ddd eee fff\n\t */\n")
                  (list
                   99 (list :comma 80)
                   (concat
                    "/* When building as a Python module, don't use special argument handling\n"
                    " * so the module loading logic can control the `argv` & `argc`. */\n")
                   (concat
                    "/* When building as a Python module,\n"
                    " * don't use special argument handling so the module loading logic"
                    " can control the `argv` & `argc`.\n"
                    " */\n"))))
          (let ((fill-column (nth 0 case))
                (fancy-fill-paragraph-split-weights (nth 1 case)))
            (should-fill-same-from-every-position (nth 2 case) 0 (nth 3 case))
            (should-fill-selection (nth 2 case) 0 (nth 3 case))))))
    (with-temp-buffer
      (pascal-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list 20 "(* aaa bbb ccc ddd *)\n" "(* aaa bbb ccc ddd\n *)\n")
                  (list 20 "{ aaa bbb ccc ddd ee }\n" "{ aaa bbb ccc ddd ee\n }\n")
                  (list 22 "  (* aaa bbb ccc ddd *)\n" "  (* aaa bbb ccc ddd\n   *)\n")))
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case))
            (should-fill-selection (nth 1 case) 0 (nth 2 case))))))))

(ert-deftest fill-syntax-bounds-block-comment-unprefixed-body-tab-indent-kept ()
  "A comment body without the prefix keeps its indentation as written.
It was filled as prose, its indentation rebuilt from spaces, so a tab
and three spaces became four, moving the text from column 11 to 4.  On
two lines the comment wasn't filled.  Each result also fills unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (fill-column 38))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list "\t/**\n\t   d hhh ff. i.e. */\n" "\t/**\n\t   d hhh ff. i.e. */\n")
                  (list
                   "\t/**\n\t   aaa bbb\n\t   ccc ddd */\n" "\t/**\n\t   aaa bbb ccc ddd */\n")
                  (list
                   "\t/**\n\t   aaa bbb ccc ddd eee fff ggg hhh iii */\n"
                   "\t/**\n\t   aaa bbb ccc ddd eee fff ggg\n\t   hhh iii */\n")))
          (should-fill-same-from-every-position (nth 0 case) 1 (nth 1 case))
          (should-fill-selection (nth 0 case) 0 (nth 1 case)))))))

(ert-deftest fill-syntax-bounds-block-comment-closer-kept-on-last-line ()
  "A closer that fits stays on the last line, however the text breaks.
Filled as a word, the closer could be broken before to suit the solver,
after a sentence weighted to split for e.g., although it fit.
A closer glued to text is part of a word, filled as before.
The last case was found in practice."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   30
                   (list :period 95)
                   0
                   "/* Aaa bbb ccc. Ddd eee fff. */\n"
                   "/* Aaa bbb ccc.\n * Ddd eee fff. */\n")
                  (list
                   30
                   (list :period 95)
                   0
                   "/* Aaa bbb ccc. Ddd eee fff.\n * Ggg hhh. */\n"
                   "/* Aaa bbb ccc.\n * Ddd eee fff.\n * Ggg hhh. */\n")
                  ;; Preservation: glued to text.
                  (list
                   30
                   (list :period 95)
                   0
                   "/* Aaa bbb ccc. Ddd eee fff.*/\n"
                   "/* Aaa bbb ccc.\n * Ddd eee fff.*/\n")
                  (list
                   99 (list :period 95 :comma 80) 0.9
                   (concat
                    "  /* Use the v2 Level Zero adapter of the SYCL unified runtime."
                    " As a fix for #159584, the v1 Level\n"
                    "   * Zero adapter is not included. While the Cycles oneAPI device"
                    " sets this as well, we also need\n"
                    "   * the environment variable for the use of Open Image Denoise"
                    " in the compositor. */\n")
                   (concat
                    "  /* Use the v2 Level Zero adapter of the SYCL unified runtime.\n"
                    "   * As a fix for #159584, the v1 Level Zero adapter is not included.\n"
                    "   * While the Cycles oneAPI device sets this as well,\n"
                    "   * we also need the environment variable for the use of Open Image Denoise"
                    " in the compositor. */\n"))))
          (let ((fill-column (nth 0 case))
                (fancy-fill-paragraph-split-weights (nth 1 case))
                (fancy-fill-paragraph-fill-column-target (nth 2 case)))
            (should-fill-same-from-every-position (nth 3 case) 0 (nth 4 case))
            (should-fill-selection (nth 3 case) 0 (nth 4 case))))))))

(ert-deftest fill-syntax-bounds-closer-after-abbreviation ()
  "A closer after a dotted abbreviation keeps a single space before it.
A double space typed before the closer was kept, it's collapsed as
within the text.  The last case is a preservation case, a sentence end
is still doubled."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space t))
    (dolist (case
             (list
              (list #'c-mode 0 "/* Use U.S. */\n" "/* Use U.S. */\n")
              (list #'c-mode 0 "/* Aaa bbb\n * use U.S. */\n" "/* Aaa bbb use U.S. */\n")
              ;; A later paragraph, filled without a prefix.
              (list
               #'c-mode 2 "/* Aaa.\n\n   Bbb\n   use U.S. */\n" "/* Aaa.\n\n   Bbb use U.S. */\n")
              (list
               #'python-mode
               3
               "def f():\n    \"\"\"Aaa.\n\n    Bbb\n    use U.S. \"\"\"\n"
               "def f():\n    \"\"\"Aaa.\n\n    Bbb use U.S. \"\"\"\n")
              (list #'c-mode 0 "/* Use U.S.  */\n" "/* Use U.S. */\n")
              ;; Preservation.
              (list #'c-mode 0 "/* End. */\n" "/* End.  */\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t)
              (fill-column 70))
          (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-closer-alone-takes-prefix ()
  "A closer on a line of its own takes the continuation prefix.
A dot-point's hanging indent isn't followed, after one the closer is
under the dash, in the opener's paragraph or a later one.  A C closer
drops the prefix's mark."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (fill-column 30))
    (dolist (case
             (list
              (list
               #'python-mode
               2
               "    \"\"\"Aaa.\n\n    - aaa bbb ccc ddd eee fff ggg hhh iii jjjjjjjjj \"\"\"\n"
               (concat
                "    \"\"\"Aaa.\n\n    - aaa bbb ccc ddd eee fff\n"
                "      ggg hhh iii jjjjjjjjj\n    \"\"\"\n"))
              (list
               #'c-mode
               0
               "/* - aaa bbb ccc ddd eee fff ggg hhh iii jjjjjjjjjjj */\n"
               "/* - aaa bbb ccc ddd eee fff\n *   ggg hhh iii jjjjjjjjjjj\n */\n")
              ;; A string on one line, its opener's line filled as a longer string's is.
              (list
               #'python-mode
               0
               "    \"\"\"- aaa bbb ccc ddd eee fff ggg hhh iii jjjjjj \"\"\"\n"
               "    \"\"\"- aaa bbb ccc ddd eee\n      fff ggg hhh iii jjjjjj\n    \"\"\"\n")
              (list
               #'python-mode
               0
               "    \"\"\"Aaa bbb ccc ddd eee fff ggg hhh iii jjjjjjjjjjjjjj \"\"\"\n"
               "    \"\"\"Aaa bbb ccc ddd eee fff\n    ggg hhh iii jjjjjjjjjjjjjj\n    \"\"\"\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t))
          (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-closer-within-fill-column-margin ()
  "A closer follows the text when it fits within the margin, as the text does.
With the closer held out of the fill, it was fit within `fill-column',
so it ran into `fancy-fill-paragraph-fill-column-margin' where the text
wraps before it.  The last case is a preservation case, the closer fits."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fancy-fill-paragraph-fill-column-margin 4)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'c-mode 0 "/* Aaa bbb ccc ddd eee fff */\n" "/* Aaa bbb ccc ddd eee fff\n */\n")
              ;; A later paragraph, filled without a prefix.
              (list
               #'python-mode
               3
               "def f():\n    \"\"\"Aaa.\n\n    Bbb ccc ddd eee fff gg \"\"\"\n"
               "def f():\n    \"\"\"Aaa.\n\n    Bbb ccc ddd eee fff gg\n    \"\"\"\n")
              ;; Preservation.
              (list #'c-mode 0 "/* Aaa bbb ccc ddd eee */\n" "/* Aaa bbb ccc ddd eee */\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t)
              (fill-column 30))
          (should-fill-same-from-every-position (nth 2 case) (nth 1 case) (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-html-comment-closer-held-out ()
  "An HTML comment's closer is held out of the fill as a C one is.
Only its `>' has comment syntax, so `-->' was filled as a word, wrapped
after the continuation prefix (` !-- -->').  A closer alone is laid out
with the prefix blanked before it, its characters aren't taken for text."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (html-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  (list
                   20
                   "<!-- aaa bbb ccc ddd eee fff ggg hhh -->\n"
                   "<!-- aaa bbb ccc ddd\n !-- eee fff ggg hhh\n   -->\n")
                  (list
                   22
                   "  <!-- aaa bbb ccc ddd eee fff ggg hhh -->\n"
                   "  <!-- aaa bbb ccc ddd\n   !-- eee fff ggg hhh\n     -->\n")
                  ;; The closer follows the text when it fits.
                  (list
                   24
                   "<!-- aaa bbb ccc ddd eee fff -->\n"
                   "<!-- aaa bbb ccc ddd eee\n !-- fff -->\n")))
          ;; In a prose mode a selection fills as prose, so only the cursor.
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case))))))))

(ert-deftest fill-syntax-bounds-two-line-block-comment-closer-line-as-longer ()
  "A two-line comment keeps or joins its closer line as a longer one does.
A closer line was kept when no wider than the continuation prefix, text
or not.  So a closer alone but wider (`  */') joined the text.  Narrow
text (`  x */') is still kept, the longer layout takes such a line for a
paragraph separator.  Text glued to the closer is read as the closer
alone, kept in both layouts.  Text beside the closer still joins, GNU
style or prefixed."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  ;; A closer alone stays, as in the longer layout (last case).
                  (list
                   20
                   "/* aaa bbb ccc ddd eee fff\n  */\n"
                   "/* aaa bbb ccc ddd\n * eee fff\n  */\n")
                  (list 70 "/* aaa bbb\n  */\n" "/* aaa bbb\n  */\n")
                  (list 70 "/* aaa bbb\n **/\n" "/* aaa bbb\n **/\n")
                  (list 70 "/* aaa bbb\n */\n" "/* aaa bbb\n */\n")
                  (list 70 "/* aaa bbb\n * ccc\n  */\n" "/* aaa bbb ccc\n  */\n")
                  ;; Narrow text is kept, as in the longer layout (last case).
                  (list 70 "    /* aaa\n  x */\n" "    /* aaa\n  x */\n")
                  (list 70 "    /* aaa\n       bbb\n  x */\n" "    /* aaa bbb\n  x */\n")
                  ;; Glued text is kept, as in the longer layout (last case).
                  (list 70 "/* aaa\n   bbb.*/\n" "/* aaa\n   bbb.*/\n")
                  (list 70 "/* aaa\n   bbb\n   ccc.*/\n" "/* aaa bbb\n   ccc.*/\n")
                  ;; Text beside the closer joins.
                  (list 70 "/* aaa bbb\n   ccc */\n" "/* aaa bbb ccc */\n")
                  (list 70 "/* aaa\n * bbb */\n" "/* aaa bbb */\n")
                  (list
                   20
                   "/* aaa bbb ccc ddd eee fff\n * ggg */\n"
                   "/* aaa bbb ccc ddd\n * eee fff ggg */\n")))
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case))
            (should-fill-selection (nth 1 case) 0 (nth 2 case))))
        ;; Code before the opener widens the prefix, text must be wider to join.
        ;; A selection leaves a comment after code alone, so only the cursor.
        (let ((fill-column 70))
          (should-fill-same-from-every-position
           "x = 1; /* aaa\n   bbb */\n" 0 "x = 1; /* aaa\n   bbb */\n")
          (should-fill-same-from-every-position
           "x = 1; /* aaa\n   bbb ccc */\n" 0 "x = 1; /* aaa bbb ccc */\n"))))
    ;; A closer alone, wider than the `|' prefix, was joined.
    (with-temp-buffer
      (pascal-mode)
      (let ((inhibit-message t)
            (fill-column 70))
        (should-fill-same-from-every-position "(* aaa bbb\n *)\n" 0 "(* aaa bbb\n *)\n")))))

(ert-deftest fill-syntax-bounds-two-line-block-comment-prefix-from-closer-line ()
  "A two-line comment reads its continuation prefix from the closer line.
It was read from the opener line, whose indentation is its leading
blank-space padded with spaces.  So a closer line indented otherwise,
with tabs after code for e.g., didn't match and its ` *' was joined as
text.  Measured on the opener line, tabs or wide characters in the code
gave the prefix the wrong width, cutting into the closer line's text.
Each case is followed by its longer form for contrast."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  ;; Tabs after code (Linux kernel style for e.g.).
                  (list 70 "\tfoo();\t/* aaa bbb\n\t\t * ccc */\n" "\tfoo();\t/* aaa bbb ccc */\n")
                  (list
                   70
                   "\tfoo();\t/* aaa bbb\n\t\t * ccc\n\t\t * ddd */\n"
                   "\tfoo();\t/* aaa bbb ccc ddd */\n")
                  (list
                   36
                   "\tfoo();\t/* aaa bbb ccc ddd eee\n\t\t * fff */\n"
                   "\tfoo();\t/* aaa bbb ccc ddd\n\t\t * eee fff */\n")
                  (list
                   36
                   "\tfoo();\t/* aaa bbb ccc ddd eee\n\t\t * fff\n\t\t * ggg */\n"
                   "\tfoo();\t/* aaa bbb ccc ddd\n\t\t * eee fff ggg */\n")
                  ;; A tab before the opener, spaces before the closer line's prefix.
                  (list 70 "\t/* aaa bbb\n         * ccc */\n" "\t/* aaa bbb ccc */\n")
                  (list
                   70
                   "\t/* aaa bbb\n         * ccc\n         * ddd */\n"
                   "\t/* aaa bbb ccc ddd */\n")
                  ;; Wide characters or a tab in the code before the opener.
                  (list
                   70
                   "s = \"\u65e5\u672c\u8a9e\"; /* aaa bbb\n               * ccc */\n"
                   "s = \"\u65e5\u672c\u8a9e\"; /* aaa bbb ccc */\n")
                  (list
                   70
                   (concat
                    "s = \"\u65e5\u672c\u8a9e\"; /* aaa bbb\n"
                    "               * ccc\n"
                    "               * ddd */\n")
                   "s = \"\u65e5\u672c\u8a9e\"; /* aaa bbb ccc ddd */\n")
                  (list 70 "foo();\t/* aaa bbb\n         * ccc */\n" "foo();\t/* aaa bbb ccc */\n")
                  (list
                   70
                   "foo();\t/* aaa bbb\n         * ccc\n         * ddd */\n"
                   "foo();\t/* aaa bbb ccc ddd */\n")))
          ;; A selection leaves a comment after code alone, so only the cursor.
          (let ((fill-column (nth 0 case)))
            (dolist (line-index (list 0 1))
              (should-fill-same-from-every-position (nth 1 case) line-index (nth 2 case)))))))))

(ert-deftest fill-syntax-bounds-two-line-block-comment-prefix-spaces-unchanged ()
  "A comment indented with spaces fills as before its prefix was measured alone.
Measured over the opener line, or read from it, spaces gave the same
prefix as the closer line's.  A closer line without the prefix, alone
or no wider than it, is kept as before."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (dolist (case
             (list
              (list
               #'c-mode
               70
               "  x = 1; /* aaa bbb\n          * ccc */\n"
               "  x = 1; /* aaa bbb ccc */\n")
              (list
               #'c-mode
               30
               "  x = 1; /* aaa bbb ccc ddd eee fff\n          * ggg */\n"
               "  x = 1; /* aaa bbb ccc ddd\n          * eee fff ggg */\n")
              (list
               #'c-mode
               30
               "  x = 1; /* aaa bbb ccc ddd eee fff ggg hhh */\n"
               "  x = 1; /* aaa bbb ccc ddd\n          * eee fff ggg hhh */\n")
              (list #'c-mode 70 "x = 1; /* aaa\n   bbb */\n" "x = 1; /* aaa\n   bbb */\n")
              (list
               #'c-mode
               20
               "/* aaa bbb ccc ddd eee fff ggg\n */\n"
               "/* aaa bbb ccc ddd\n * eee fff ggg\n */\n")
              (list
               #'c-mode
               20
               "/* aaa bbb ccc ddd eee fff ggg\n x */\n"
               "/* aaa bbb ccc ddd\n * eee fff ggg x */\n")
              (list
               #'js-mode
               24
               "    /** aaa bbb ccc ddd eee fff\n     * ggg */\n"
               "    /** aaa bbb ccc ddd\n     * eee fff ggg */\n")))
      (with-temp-buffer
        (funcall (nth 0 case))
        (let ((inhibit-message t)
              (fill-column (nth 1 case)))
          (should-fill-same-from-every-position (nth 2 case) 0 (nth 3 case)))))))

(ert-deftest fill-syntax-bounds-one-line-block-comment-prefix-width ()
  "A comment on one line wraps as wide as its continuation lines allow.
The prefix width was measured on the opener line, counting the code
before the opener.  So wide characters or tabs there wrapped the text
short, filling it again moved it.  Each result also fills unchanged."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (dolist (case
                 (list
                  ;; Wide characters in the code before the opener.
                  (list
                   40
                   (concat
                    "s = \"\u65e5\u672c\u8a9e\u65e5\u672c\u8a9e\";"
                    " /* aa bb cc dd ee ff gg hh ii jj */\n")
                   (concat
                    "s = \"\u65e5\u672c\u8a9e\u65e5\u672c\u8a9e\"; /* aa bb cc dd ee ff\n"
                    "                     * gg hh ii jj */\n"))
                  ;; Tabs in the code before the opener.
                  (list
                   37
                   "\tfoo();\t/* aaa bbb ccc ddd eee fff ggg hhh */\n"
                   "\tfoo();\t/* aaa bbb ccc ddd\n\t         * eee fff ggg hhh */\n")
                  ;; Spaces before a tab indenting the opener line, the prefix keeps both.
                  (list
                   36
                   "  \tfoo(); /* aaa bbb ccc ddd eee fff ggg hhh */\n"
                   "  \tfoo(); /* aaa bbb ccc ddd\n  \t        * eee fff ggg hhh */\n")))
          ;; A selection leaves a comment after code alone, so only the cursor.
          (let ((fill-column (nth 0 case)))
            (should-fill-same-from-every-position (nth 1 case) 0 (nth 2 case))
            (should-fill-same-from-every-position (nth 2 case) 0 (nth 2 case))))))))

(ert-deftest fill-syntax-bounds-prefix-width-tab-width-zero ()
  "A prefix holding a tab is measured with a `tab-width' of zero as eight.
Emacs lays out a tab with a `tab-width' of zero as eight columns."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t)
            (fill-column 34))
        (setq tab-width 0)
        (should-fill-same-from-every-position
         "\tfoo();\t/* aaa bbb ccc ddd eee fff ggg hhh iii jjj\n\t\t * kkk */\n"
         0
         "\tfoo();\t/* aaa bbb ccc ddd\n\t\t * eee fff ggg hhh\n\t\t * iii jjj kkk */\n")))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-bare-separator-width ()
  "A bare ` *' separator line does not widen the paragraph above it.
The prefix width was measured on that line, one column short of ` * '.
The paragraph wraps past two lines, the first line alone may not show
it, its width being taken from the opener."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "/* aaa bbb ccc dddddd eee fff ggg hhh iii jjj\n"
                 " *\n"
                 " * Details here. */\n"))
        (text-expected
         ;; format-next-line: off
         (concat "/* aaa bbb ccc\n"
                 " * dddddd eee fff\n"
                 " * ggg hhh iii jjj\n"
                 " *\n"
                 " * Details here. */\n"))
        ;; The same comment with the separator's trailing space kept.
        (text-initial-spaced
         ;; format-next-line: off
         (concat "/* aaa bbb ccc dddddd eee fff ggg hhh iii jjj\n"
                 " * \n"
                 " * Details here. */\n"))
        (text-expected-spaced
         ;; format-next-line: off
         (concat "/* aaa bbb ccc\n"
                 " * dddddd eee fff\n"
                 " * ggg hhh iii jjj\n"
                 " * \n"
                 " * Details here. */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (should-fill-same-from-every-position text-initial-spaced 0 text-expected-spaced)))))

(ert-deftest fill-syntax-bounds-c-mode-javadoc-opener-width ()
  "The first line after a `/** ' opener is one column narrower.
The opener is wider than the ` * ' continuation, which alone set the
width, so the first line ran past `fill-column'."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial-multiline
         ;; format-next-line: off
         (concat "/** aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk\n"
                 " * lll\n"
                 " */\n"))
        (text-expected-multiline
         ;; format-next-line: off
         (concat "/** aaa bbb ccc\n"
                 " * ddd eee fff ggg\n"
                 " * hhh iii jjj kkk\n"
                 " * lll\n"
                 " */\n"))
        (text-initial-inline "/** aaa bbb ccc ddd eee fff ggg hhh iii jjj kkk */\n")
        (text-expected-inline
         ;; format-next-line: off
         (concat "/** aaa bbb ccc ddd\n"
                 " * eee fff ggg hhh iii\n"
                 " * jjj kkk */\n")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (let ((fill-column 18))
          (should-fill-same-from-every-position text-initial-multiline 0 text-expected-multiline))
        (let ((fill-column 22))
          (should-fill-same-from-every-position text-initial-inline 0 text-expected-inline))))))

(ert-deftest fill-syntax-bounds-python-docstring-opener-width ()
  "The first line after the opening quotes is narrower by their width.
The continuation prefix alone set the width, so the first line ran past
`fill-column'."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa bbb ccc ddd eee fff ggg hhh iii\n"
                 "    jjj\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"aaa bbb ccc\n"
                 "    ddd eee fff ggg\n"
                 "    hhh iii jjj\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-python-docstring-opener-width-dot-points ()
  "Dot points after the opener text are not narrowed by the opener.
Each dot point fills on its own, only the first line of the first takes
the opener's width."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Intro aaa bbb ccc ddd eee fff ggg.\n"
                 "    - item one aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "    - item two aaa bbb ccc ddd eee fff ggg hhh.\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "def f():\n"
                 "    \"\"\"Intro aaa bbb ccc ddd\n"
                 "    eee fff ggg.\n"
                 "    - item one aaa bbb ccc ddd\n"
                 "      eee fff ggg hhh.\n"
                 "    - item two aaa bbb ccc ddd\n"
                 "      eee fff ggg hhh.\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))

(ert-deftest fill-syntax-bounds-python-docstring-narrow-opener-width ()
  "An opener narrower than the continuation prefix leaves the width as it is.
The first line holds as much text as the others, it is not widened to
end at `fill-column'."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 20)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "\"\"\"aaaa bbb ccc ddd eee fff ggg hhh iii\n"
                 "    jjj\n"
                 "    \"\"\"\n"))
        (text-expected
         ;; format-next-line: off
         (concat "\"\"\"aaaa bbb ccc ddd\n"
                 "    eee fff ggg hhh\n"
                 "    iii jjj\n"
                 "    \"\"\"\n"))
        ;; Balancing the lines also takes the first line to be as wide.
        (text-initial-balance
         ;; format-next-line: off
         (concat "\"\"\"a bbbbbbb ccc d eeeeeee ff g hhhhh\n"
                 "    ii\n"
                 "    \"\"\"\n"))
        (text-expected-balance
         ;; format-next-line: off
         (concat "\"\"\"a bbbbbbb ccc\n"
                 "    d eeeeeee ff g\n"
                 "    hhhhh ii\n"
                 "    \"\"\"\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 0 text-expected)
        (let ((fill-column 21))
          (should-fill-same-from-every-position text-initial-balance 0 text-expected-balance))))))

(ert-deftest fill-syntax-bounds-python-string-opener-width-beside-code ()
  "Code before a string opener narrows the first line, code after the closer is kept.
The closer line holds code, so it stays out of the paragraph."
  (let ((fancy-fill-paragraph-syntax-bounds t)
        (fill-column 30)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat "x = \"\"\"aaa bbb\n"
                 "ccc ddd eee fff ggg hhh iii jjj\n"
                 "\"\"\" + y\n"))
        (text-expected
         ;; format-next-line: off
         (concat "x = \"\"\"aaa bbb ccc ddd eee fff\n"
                 "ggg hhh iii jjj\n"
                 "\"\"\" + y\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (should-fill-same-from-every-position text-initial 1 text-expected)))))


;; ---------------------------------------------------------------------------
;; Syntax Bounds Tests

(ert-deftest fill-syntax-bounds-c-comments-wrap ()
  "Three separate C-style comments should each wrap independently."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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

(ert-deftest fill-syntax-bounds-python-string-three-line-unwrap ()
  "Three short lines in a Python triple-quoted string should unwrap."
  (let ((fill-column 52)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial (concat "\"\"\"\n" "AAA.\n" "BBB.\n" "CCC.\n" "\n" "\n" "\"\"\""))
        (text-expected (concat "\"\"\"\n" "AAA. BBB. CCC.\n" "\n" "\n" "\"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (forward-line 1)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-three-line-wrap ()
  "Three long Python comment lines should wrap within fill-column."
  (let ((fill-column 52)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "# Alpha bravo charlie delta echo foxtrot golf hotel india.\n"
          "# Juliet kilo lima mike november oscar papa quebec.\n"
          "# Romeo sierra tango uniform victor whiskey xray."))
        (text-expected
         (concat
          "# Alpha bravo charlie delta echo\n"
          "# foxtrot golf hotel india.\n"
          "# Juliet kilo lima mike november oscar papa quebec.\n"
          "# Romeo sierra tango uniform victor whiskey xray.")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (forward-line 1)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-three-line-unwrap ()
  "Three short Python comment lines should unwrap within fill-column."
  (let ((fill-column 52)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial (concat "# AAA.\n" "# BBB.\n" "# CCC."))
        (text-expected "# AAA. BBB. CCC."))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (forward-line 1)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-wrap ()
  "A long C++ \"//\" comment should wrap keeping the comment leader."
  (let ((fill-column 20)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "  // aaa bbb ccc ddd eee fff ggg hhh\n")
        (text-expected (concat "  // aaa bbb ccc ddd\n" "  // eee fff ggg hhh\n")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 6)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-unwrap ()
  "Two short C++ \"//\" comment lines should join within fill-column."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n// ccc ddd\n")
        (text-expected "// aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-exclude-block-comment ()
  "A block comment line must not be absorbed into an adjacent \"//\" block."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n/* block */\n")
        (text-expected "// aaa bbb\n/* block */\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-exclude-unclosed-block-at-eob ()
  "An unclosed block comment on the last line must not join a \"//\" block."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa\n/* bbb")
        (text-expected "// aaa\n/* bbb"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-unwrap-no-trailing-newline ()
  "C++ \"//\" lines should join when the buffer lacks a trailing newline."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n// ccc ddd")
        (text-expected "// aaa bbb ccc ddd"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-doc-leader-wrap ()
  "A doc-comment \"///\" line should wrap keeping the full delimiter run."
  (let ((fill-column 20)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "  /// aaa bbb ccc ddd eee fff ggg hhh\n")
        (text-expected (concat "  /// aaa bbb ccc\n" "  /// ddd eee fff\n" "  /// ggg hhh\n")))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 6)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-sql-line-comments-unwrap ()
  "Two short SQL \"--\" comment lines should join within fill-column."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "-- aaa bbb\n-- ccc ddd\n")
        (text-expected "-- aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (sql-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-pascal-line-comments-unwrap ()
  "Pascal \"//\" lines (comment style c) should join within fill-column."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n// ccc ddd\n")
        (text-expected "// aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (pascal-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-pascal-line-comments-exclude-block-comment ()
  "A Pascal \"{ }\" comment line must not be absorbed into a \"//\" block.
The \"{\" opener is comment-start class (11), but its style a does
not match the style c newline ender, so it is a block comment."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n{ block }\n")
        (text-expected "// aaa bbb\n{ block }\n"))
    (with-temp-buffer
      (pascal-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-exclude-tab-misaligned ()
  "A tab-indented comment at a different visual column must not join.
`move-to-column' overshoots when the target column falls inside a
tab; the comment found at the overshot column belongs to another
block."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "    # aaa bbb\n\t# ccc ddd\n")
        (text-expected "    # aaa bbb\n\t# ccc ddd\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 7)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-exclude-tab-misaligned ()
  "A tab-indented \"//\" comment at a different visual column must not join."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "    // aaa bbb\n\t// ccc ddd\n")
        (text-expected "    // aaa bbb\n\t// ccc ddd\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 8)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-elisp-comments-exclude-mismatched-run ()
  "A \";;\" line must not be absorbed into a \";;;\" block.
The continuation prefix is stripped by column, so absorbing a
shorter delimiter run deletes content characters (here the first
\"c\" of \"ccc\")."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial ";;; aaa bbb\n;; ccc ddd\n")
        (text-expected ";;; aaa bbb\n;; ccc ddd\n"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-exclude-mismatched-run ()
  "A \"///\" line must not be absorbed into a \"//\" block."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "// aaa bbb\n/// ccc ddd\n")
        (text-expected "// aaa bbb\n/// ccc ddd\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-doc-leader-unwrap ()
  "Two \"///\" lines with equal delimiter runs should still join."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "/// aaa bbb\n/// ccc ddd\n")
        (text-expected "/// aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-uneven-gap-no-deletion ()
  "A narrower delimiter-to-content gap on a later line must lose no text.
The continuation prefix derives from the first line; stripping its
width from a line with a narrower gap used to delete content."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "#  aaa bbb\n# ccc ddd\n")
        (text-expected "#  aaa bbb\n# ccc ddd\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-line-comments-uneven-gap-no-deletion ()
  "A narrower gap after \"//\" on a later line must lose no text."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "//  aaa bbb\n// ccc ddd\n")
        (text-expected "//  aaa bbb\n// ccc ddd\n"))
    (with-temp-buffer
      (c++-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-tab-gap-unwrap ()
  "A tab after the delimiter must not distort the stripped width.
`string-width' counts a tab as `tab-width' columns regardless of its
start column; the strip width must come from buffer columns.  The
mixed-gap lines join and all content characters survive."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "#\taaa bbb\n# ccc ddd\n")
        (text-expected "# aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-wide-paragraph-keeps-gap ()
  "A wide-gap paragraph must keep its prefix beside a narrow one.
The continuation prefix is derived per paragraph; a narrower
delimiter-to-content gap in another paragraph of the same block must
not re-indent this one."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "#   wide aaa\n#   bbb ccc\n#\n# narrow ggg\n")
        (text-expected "#   wide aaa bbb ccc\n#\n# narrow ggg\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-wide-paragraph-keeps-gap-wrap ()
  "A wide-gap paragraph must keep its prefix on wrapped lines.
Same preservation as the join case: wrapping must carry the
paragraph's own \"#   \" prefix onto continuation lines, not the
block-minimum prefix from the narrow sibling paragraph."
  (let ((fill-column 16)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "#   wide aaa bbb ccc ddd eee fff\n#\n# narrow ggg\n")
        (text-expected
         (concat "#   wide aaa bbb\n" "#   ccc ddd eee\n" "#   fff\n" "#\n" "# narrow ggg\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 5)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-narrow-paragraph-beside-wide ()
  "Filling the narrow paragraph must not disturb the wide sibling.
The narrow paragraph joins with its own \"# \" prefix; the wide
paragraph's lines and gap are untouched."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "#   wide aaa bbb\n#\n# narrow ggg hhh\n# iii jjj\n")
        (text-expected "#   wide aaa bbb\n#\n# narrow ggg hhh iii jjj\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 25)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-ignore-hash-in-string ()
  "Line-comment block expansion must ignore a same-column hash inside a string."
  (let ((fill-column 45)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "s = \"\"\"abc\n"
          "# not a comment line that should stay in the string\"\"\"\n"
          "# real comment alpha beta gamma delta\n"
          "# epsilon zeta eta theta iota\n"))
        (text-expected
         (concat
          "s = \"\"\"abc\n"
          "# not a comment line that should stay in the string\"\"\"\n"
          "# real comment alpha beta gamma delta epsilon\n"
          "# zeta eta theta iota\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "real comment")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comments-tab-aligned-unwrap ()
  "Tab and space indented comments at the same visual column should join."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "\t# aaa bbb\n        # ccc ddd\n")
        (text-expected "\t# aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-style-c-first-char-line-comments-unwrap ()
  "Comment style c carried only on the first opener character is honored.
Emacs takes style b from the style-determining character alone but
style c from either character of a two-character opener."
  (let ((fill-column 70)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial "$% aaa bbb\n$% ccc ddd\n")
        (text-expected "$% aaa bbb ccc ddd\n"))
    (with-temp-buffer
      (let ((tbl (make-syntax-table)))
        (modify-syntax-entry ?$ ". 1c" tbl)
        (modify-syntax-entry ?% ". 2" tbl)
        (modify-syntax-entry ?\n "> c" tbl)
        (set-syntax-table tbl))
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char 4)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-comments-wrap ()
  "C-mode inline block comments should wrap using `comment-continue'."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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

(ert-deftest fill-syntax-bounds-elisp-comments-three-line-wrap ()
  "Three long Emacs Lisp comment lines should wrap within fill-column."
  (let ((fill-column 52)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          ";; Alpha bravo charlie delta echo foxtrot golf hotel india.\n"
          ";; Juliet kilo lima mike november oscar papa quebec.\n"
          ";; Romeo sierra tango uniform victor whiskey xray."))
        (text-expected
         (concat
          ";; Alpha bravo charlie delta echo\n"
          ";; foxtrot golf hotel india.\n"
          ";; Juliet kilo lima mike november oscar papa quebec.\n"
          ";; Romeo sierra tango uniform victor whiskey xray.")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-elisp-comments-three-line-unwrap ()
  "Three short Emacs Lisp comment lines should unwrap within fill-column."
  (let ((fill-column 52)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat ";; Alpha bravo charlie\n" ";; delta echo foxtrot.\n" ";; Golf hotel india."))
        (text-expected
         (concat ";; Alpha bravo charlie delta echo foxtrot.\n" ";; Golf hotel india.")))
    (with-temp-buffer
      (emacs-lisp-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-cursor-first-paragraph ()
  "With cursor in the first paragraph of a two-paragraph block comment, only first fills."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " * Hotel india juliet.\n"
          " *\n"
          " * Kilo lima mike november\n"
          " * oscar papa quebec.\n"
          " * Romeo sierra tango.\n"
          " */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo\n"
          " * foxtrot golf. Hotel india juliet.\n"
          " *\n"
          " * Kilo lima mike november\n"
          " * oscar papa quebec.\n"
          " * Romeo sierra tango.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "echo")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-cursor-second-paragraph ()
  "With cursor in the second paragraph of a two-paragraph block comment, only the second fills."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " * Hotel india juliet.\n"
          " *\n"
          " * Kilo lima mike november\n"
          " * oscar papa quebec.\n"
          " * Romeo sierra tango.\n"
          " */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " * Hotel india juliet.\n"
          " *\n"
          " * Kilo lima mike november oscar papa\n"
          " * quebec. Romeo sierra tango.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "oscar")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-dot-points-noop ()
  "C-mode block comment with dot-points already at fill-column should be unchanged."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "  /* Alpha bravo.\n"
          "   *\n"
          "   * Charlie delta echo:\n"
          "   * - Foxtrot golf hotel india juliet kilo,\n"
          "   *   lima mike november oscar papa quebec romeo sierra.\n"
          "   * - Tango uniform victor whiskey xray yankee zulu alpha, bravo charlie delta\n"
          "   *   echo foxtrot golf hotel india. Juliet kilo lima mike november oscar,\n"
          "   *   papa quebec romeo sierra tango uniform.\n"
          "   * - Victor whiskey xray.\n"
          "   */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "Charlie")
        (fancy-fill-paragraph)
        (should (equal text-initial (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-tab-indent-noop ()
  "C-mode block comment with tab indent and dot-points should be unchanged."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "\t/* Alpha bravo.\n"
          "\t *\n"
          "\t * Charlie delta echo:\n"
          "\t * - Foxtrot golf hotel india juliet kilo,\n"
          "\t *   lima mike november oscar papa quebec romeo sierra.\n"
          "\t * - Tango uniform victor whiskey xray yankee zulu alpha,\n"
          "\t *   bravo charlie delta echo foxtrot golf hotel india.\n"
          "\t * - Victor whiskey xray.\n"
          "\t */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (setq tab-width 4)
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "Charlie")
        (fancy-fill-paragraph)
        (should (equal text-initial (buffer-string)))))))


(ert-deftest fill-syntax-bounds-c-mode-block-comment-tab-indent-noop-prefix ()
  "C-mode block comment with tab indent and dot-points should be unchanged."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "X\t/* Alpha bravo.\n"
          "\t *\n"
          "\t * Charlie delta echo:\n"
          "\t * - Foxtrot golf hotel india juliet kilo,\n"
          "\t *   lima mike november oscar papa quebec romeo sierra.\n"
          "\t * - Tango uniform victor whiskey xray yankee zulu alpha,\n"
          "\t *   bravo charlie delta echo foxtrot golf hotel india.\n"
          "\t * - Victor whiskey xray.\n"
          "\t */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (setq tab-width 4)
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "Charlie")
        (fancy-fill-paragraph)
        (should (equal text-initial (buffer-string)))))))


(ert-deftest fill-syntax-bounds-c-mode-javadoc-inline-wrap ()
  "C-mode Javadoc inline comment with `/**' opener should preserve the opener."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
          " * * Alpha bravo charlie\n"
          " *   delta echo foxtrot\n"
          " *   golf hotel india.\n"
          " * * Juliet kilo lima mike\n"
          " *   november oscar papa\n"
          " *   quebec romeo sierra.\n"
          " * * Tango uniform victor\n"
          " *   whiskey xray yankee\n"
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
  "C-mode block comment with alternative `c-block-comment-prefix' set to dash, only first paragraph fills."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
          " - - Juliet kilo lima mike november oscar papa quebec.\n"
          " - - Romeo sierra tango uniform victor whiskey xray.\n"
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
  "C-mode block comment with blank `*' lines, cursor in first paragraph fills only that paragraph."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
          "     * An alternative would be to ensure the popup state always matches\n"
          "     * the initial state, however this ends up involving assumptions\n"
          "     * which may not hold in *every* case,\n"
          "     * so triggering a refresh is a reasonable solution.\n"
          "     * A refresh also occurs when moving the cursor between buttons,\n"
          "     * so it's expected\n"
          "     * to be fast. see #155320.\n"
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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
  "Numbered list inside a C block comment, cursor in heading paragraph leaves list unchanged."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
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
          " * 1. Alpha bravo charlie delta echo foxtrot golf.\n"
          " * 2. Hotel india juliet kilo lima mike november.\n"
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
        (sentence-end-double-space nil)
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
        (sentence-end-double-space nil)
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

(ert-deftest fill-syntax-bounds-c-mode-block-comment-isolate-first-paragraph ()
  "Cursor in first paragraph should not fill the second paragraph."
  (let ((fill-column 60)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie\n"
          " * delta echo foxtrot.\n"
          " * Golf hotel india.\n"
          " *\n"
          " * Kilo lima mike\n"
          " * november oscar papa.\n"
          " * Quebec romeo sierra.\n"
          " */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta echo foxtrot. Golf hotel india.\n"
          " *\n"
          " * Kilo lima mike\n"
          " * november oscar papa.\n"
          " * Quebec romeo sierra.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "echo")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-isolate-second-paragraph ()
  "Cursor in second paragraph should not fill the first paragraph."
  (let ((fill-column 60)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie\n"
          " * delta echo foxtrot.\n"
          " * Golf hotel india.\n"
          " *\n"
          " * Kilo lima mike\n"
          " * november oscar papa.\n"
          " * Quebec romeo sierra.\n"
          " */"))
        ;; NOTE: the second paragraph fits on one line at fill-column 60.
        (text-expected
         (concat
          "/* Alpha bravo charlie\n"
          " * delta echo foxtrot.\n"
          " * Golf hotel india.\n"
          " *\n"
          " * Kilo lima mike november oscar papa. Quebec romeo sierra.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (goto-char (point-min))
        (search-forward "oscar")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-markdown-star-bold-dot-points ()
  "Markdown `* **Bold:**' dot-point list should fill each item independently."
  (let
      ((fill-column 120)
       (sentence-end-double-space nil)
       (fancy-fill-paragraph-dot-point-prefix (list "- " "* " '(:regexp . "[0-9]+\\. ")))
       (text-initial
        (concat
         "* **Permissions:** The \"Allow edits from maintainers\" checkbox must be enabled on the PR.\n"
         "* **Access:** You must have Write or Admin access to the destination (upstream) repository.\n"
         "* **Authentication:** Use your own Gitea credentials; the system validates your maintainer status to allow the push to the fork."))
       (text-expected
        (concat
         "* **Permissions:** The \"Allow edits from maintainers\" checkbox must be enabled on the PR.\n"
         "* **Access:** You must have Write or Admin access to the destination (upstream) repository.\n"
         "* **Authentication:** Use your own Gitea credentials;\n"
         "  the system validates your maintainer status to allow the push to the fork.")))
    (with-fancy-fill-paragraph-test text-initial
      (goto-char (point-max))
      (beginning-of-line)
      (fancy-fill-paragraph)
      (should (equal text-expected (buffer-string))))))

(ert-deftest fill-syntax-bounds-python-string-cursor-first-paragraph ()
  "Filling first paragraph of a Python triple-quoted string should not touch the second."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"\n"
          "    Alpha bravo charlie delta\n"
          "    echo foxtrot golf.\n"
          "    Hotel india juliet.\n"
          "\n"
          "    Kilo lima mike november\n"
          "    oscar papa quebec.\n"
          "    Romeo sierra tango.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"\n"
          "    Alpha bravo charlie delta echo\n"
          "    foxtrot golf. Hotel india juliet.\n"
          "\n"
          "    Kilo lima mike november\n"
          "    oscar papa quebec.\n"
          "    Romeo sierra tango.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "Alpha")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-cursor-second-paragraph ()
  "Filling second paragraph of a Python triple-quoted string should not touch the first."
  (let ((fill-column 40)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"\n"
          "    Alpha bravo charlie delta\n"
          "    echo foxtrot golf.\n"
          "    Hotel india juliet.\n"
          "\n"
          "    Kilo lima mike november\n"
          "    oscar papa quebec.\n"
          "    Romeo sierra tango.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"\n"
          "    Alpha bravo charlie delta\n"
          "    echo foxtrot golf.\n"
          "    Hotel india juliet.\n"
          "\n"
          "    Kilo lima mike november oscar papa\n"
          "    quebec. Romeo sierra tango.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "oscar")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-region-middle-paragraphs ()
  "Active region spanning middle 3 of 5 paragraphs should fill only those 3."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/*\n"
          " * Aaa bbb ccc\n"
          " * ddd eee fff.\n"
          " *\n"
          " * Ggg hhh iii\n"
          " * jjj kkk lll.\n"
          " *\n"
          " * Mmm nnn ooo\n"
          " * ppp qqq rrr.\n"
          " *\n"
          " * Sss ttt uuu\n"
          " * vvv www xxx.\n"
          " *\n"
          " * Yyy zzz aaa\n"
          " * bbb ccc ddd.\n"
          " */"))
        (text-expected
         (concat
          "/*\n"
          " * Aaa bbb ccc\n"
          " * ddd eee fff.\n"
          " *\n"
          " * Ggg hhh iii jjj kkk lll.\n"
          " *\n"
          " * Mmm nnn ooo ppp qqq rrr.\n"
          " *\n"
          " * Sss ttt uuu vvv www xxx.\n"
          " *\n"
          " * Yyy zzz aaa\n"
          " * bbb ccc ddd.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        ;; Select paragraphs 2-4 (skip first and last).
        (goto-char (point-min))
        (search-forward "Ggg")
        (set-mark (point))
        (search-forward "xxx.")
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-region-middle-paragraphs ()
  "Active region spanning middle 3 of 5 paragraphs should fill only those 3."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"\n"
          "    Aaa bbb ccc\n"
          "    ddd eee fff.\n"
          "\n"
          "    Ggg hhh iii\n"
          "    jjj kkk lll.\n"
          "\n"
          "    Mmm nnn ooo\n"
          "    ppp qqq rrr.\n"
          "\n"
          "    Sss ttt uuu\n"
          "    vvv www xxx.\n"
          "\n"
          "    Yyy zzz aaa\n"
          "    bbb ccc ddd.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"\n"
          "    Aaa bbb ccc\n"
          "    ddd eee fff.\n"
          "\n"
          "    Ggg hhh iii jjj kkk lll.\n"
          "\n"
          "    Mmm nnn ooo ppp qqq rrr.\n"
          "\n"
          "    Sss ttt uuu vvv www xxx.\n"
          "\n"
          "    Yyy zzz aaa\n"
          "    bbb ccc ddd.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        ;; Select paragraphs 2-4 (skip first and last).
        (goto-char (point-min))
        (search-forward "Ggg")
        (set-mark (point))
        (search-forward "xxx.")
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-cursor-in-class-body ()
  "Filling a Python comment block inside a class body should not spill into code."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "class _Global:\n"
          "    # While not expected, failure to parse a single signature should not\n"
          "    # cause all stub generation to fail.\n"
          "    # These should be treated as bugs and fixed.\n"
          "    parse_errors: int = 0\n"))
        (text-expected
         (concat
          "class _Global:\n"
          "    # While not expected, failure to parse a single signature\n"
          "    # should not cause all stub generation to fail.\n"
          "    # These should be treated as bugs and fixed.\n"
          "    parse_errors: int = 0\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "should not")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-single-long-line-wraps ()
  "A single long Python comment line should wrap without spilling into code."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "class Foo:\n"
          "    # While not expected, failure to parse a single signature"
          " should not cause all stub generation to fail,"
          " these should be treated as bugs and fixed.\n"
          "    parse_errors: int = 0\n"))
        (text-expected
         (concat
          "class Foo:\n"
          "    # While not expected, failure to parse a single signature\n"
          "    # should not cause all stub generation to fail,\n"
          "    # these should be treated as bugs and fixed.\n"
          "    parse_errors: int = 0\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "failure")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-five-lines-cursor-last ()
  "Filling five Python comment lines with cursor on the last should refill the block."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india\n"
          "    # juliet kilo lima mike november oscar papa quebec romeo\n"
          "    # sierra tango uniform victor whiskey xray yankee zulu\n"
          "    # alpha bravo charlie delta echo foxtrot golf hotel.\n"
          "    # India juliet kilo lima.\n"
          "    parse_errors: int = 0\n"))
        (text-expected
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india juliet\n"
          "    # kilo lima mike november oscar papa quebec romeo sierra tango\n"
          "    # uniform victor whiskey xray yankee zulu alpha bravo charlie delta\n"
          "    # echo foxtrot golf hotel. India juliet kilo lima.\n"
          "    parse_errors: int = 0\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "India juliet")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-five-lines-cursor-first ()
  "Filling five Python comment lines with cursor on the first should refill the block."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india\n"
          "    # juliet kilo lima mike november oscar papa quebec romeo\n"
          "    # sierra tango uniform victor whiskey xray yankee zulu\n"
          "    # alpha bravo charlie delta echo foxtrot golf hotel.\n"
          "    # India juliet kilo lima.\n"
          "    parse_errors: int = 0\n"))
        (text-expected
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india juliet\n"
          "    # kilo lima mike november oscar papa quebec romeo sierra tango\n"
          "    # uniform victor whiskey xray yankee zulu alpha bravo charlie delta\n"
          "    # echo foxtrot golf hotel. India juliet kilo lima.\n"
          "    parse_errors: int = 0\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "Alpha")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-five-lines-cursor-at-hash ()
  "Filling with cursor on the `#' character itself should still fill correctly."
  (let ((fill-column 72)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india\n"
          "    # juliet kilo lima mike november oscar papa quebec romeo\n"
          "    # sierra tango uniform victor whiskey xray yankee zulu\n"
          "    # alpha bravo charlie delta echo foxtrot golf hotel.\n"
          "    # India juliet kilo lima.\n"
          "    parse_errors: int = 0\n"))
        (text-expected
         (concat
          "class Foo:\n"
          "    # Alpha bravo charlie delta echo foxtrot golf hotel india juliet\n"
          "    # kilo lima mike november oscar papa quebec romeo sierra tango\n"
          "    # uniform victor whiskey xray yankee zulu alpha bravo charlie delta\n"
          "    # echo foxtrot golf hotel. India juliet kilo lima.\n"
          "    parse_errors: int = 0\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        ;; Position cursor directly on the # of the third comment line.
        (forward-line 3)
        (back-to-indentation)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-opener-text-region-middle ()
  "Active region in a middle paragraph should not refill the opener paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " *\n"
          " * Kilo lima mike\n"
          " * november oscar\n"
          " * papa quebec.\n"
          " *\n"
          " * Romeo sierra tango.\n"
          " */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " *\n"
          " * Kilo lima mike november oscar papa quebec.\n"
          " *\n"
          " * Romeo sierra tango.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "Kilo")
        (set-mark (point))
        (search-forward "quebec.")
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-c-mode-block-comment-opener-text-cursor-middle ()
  "Cursor in a middle paragraph should not refill the opener paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " *\n"
          " * Kilo lima mike\n"
          " * november oscar\n"
          " * papa quebec.\n"
          " *\n"
          " * Romeo sierra tango.\n"
          " */"))
        (text-expected
         (concat
          "/* Alpha bravo charlie delta\n"
          " * echo foxtrot golf.\n"
          " *\n"
          " * Kilo lima mike november oscar papa quebec.\n"
          " *\n"
          " * Romeo sierra tango.\n"
          " */")))
    (with-temp-buffer
      (c-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "november")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-opener-text-cursor-first-paragraph ()
  "Filling first paragraph of a triple-quoted string with opener text should merge it."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"Alpha bravo\n"
          "    charlie delta echo\n"
          "    foxtrot golf hotel.\n"
          "    India juliet kilo.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"Alpha bravo charlie delta echo foxtrot golf\n"
          "    hotel. India juliet kilo.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "charlie")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-string-opener-text-cursor-second-paragraph ()
  "Cursor in second paragraph should not refill the opener paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    \"\"\"Alpha bravo charlie\n"
          "    delta echo foxtrot.\n"
          "\n"
          "    Golf hotel india\n"
          "    juliet kilo lima.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    \"\"\"Alpha bravo charlie\n"
          "    delta echo foxtrot.\n"
          "\n"
          "    Golf hotel india juliet kilo lima.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "hotel")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-r-string-opener-text ()
  "Prefixed r-string with opener text should merge opener into first paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    r\"\"\"Alpha bravo\n"
          "    charlie delta echo\n"
          "    foxtrot golf hotel.\n"
          "    India juliet kilo.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    r\"\"\"Alpha bravo charlie delta echo foxtrot\n"
          "    golf hotel. India juliet kilo.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "charlie")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-f-string-opener-text-cursor-second-paragraph ()
  "Cursor in second paragraph of f-string should not refill opener paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "    f\"\"\"Alpha bravo charlie\n"
          "    delta echo foxtrot.\n"
          "\n"
          "    Golf hotel india\n"
          "    juliet kilo lima.\n"
          "    \"\"\""))
        (text-expected
         (concat
          "    f\"\"\"Alpha bravo charlie\n"
          "    delta echo foxtrot.\n"
          "\n"
          "    Golf hotel india juliet kilo lima.\n"
          "    \"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "hotel")
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-dot-point-cursor-last-continuation ()
  "Cursor at end of a continuation line should derive the prefix from the block, not the cursor line.
The continuation lines have `#   ' (deeper post-`#' indent) than the
dot-point lines `# * '.  Filling must use `# ' as the cont-prefix
regardless of which line the cursor is on."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat
          "    # Assumptions:\n"
          "    # * We don't return the absolute path name of the directory content, just\n"
          "    #   the name relative to `path`\n"
          "    # * We can return directory contents in any order;"
          " we choose to sort directory contents\n"
          "    #   to make the tests deterministic\n"
          "    def ls(self): pass\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "    # Assumptions:\n"
          "    # * We don't return the absolute path name of the directory content,\n"
          "    #   just the name relative to `path`\n"
          "    # * We can return directory contents in any order;\n"
          "    #   we choose to sort directory contents to make the tests deterministic\n"
          "    def ls(self): pass\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (forward-line 4)
        (end-of-line)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-comment-dot-point-cursor-middle-continuation ()
  "Cursor at end of an inner continuation line should derive the prefix from the block."
  (let ((fill-column 80)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         ;; format-next-line: off
         (concat
          "    # Assumptions:\n"
          "    # * We don't return the absolute path name of the directory content, just\n"
          "    #   the name relative to `path`\n"
          "    # * We can return directory contents in any order;"
          " we choose to sort directory contents\n"
          "    #   to make the tests deterministic\n"
          "    def ls(self): pass\n"))
        (text-expected
         ;; format-next-line: off
         (concat
          "    # Assumptions:\n"
          "    # * We don't return the absolute path name of the directory content,\n"
          "    #   just the name relative to `path`\n"
          "    # * We can return directory contents in any order;\n"
          "    #   we choose to sort directory contents to make the tests deterministic\n"
          "    def ls(self): pass\n")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (forward-line 2)
        (end-of-line)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

(ert-deftest fill-syntax-bounds-python-r-string-opener-text-region-second-paragraph ()
  "Active region in second paragraph of r-string should not refill opener paragraph."
  (let ((fill-column 50)
        (fancy-fill-paragraph-syntax-bounds t)
        (sentence-end-double-space nil)
        (text-initial
         (concat
          "r\"\"\"Alpha bravo charlie\n"
          "delta echo foxtrot.\n"
          "\n"
          "Golf hotel india\n"
          "juliet kilo lima.\n"
          "\"\"\""))
        (text-expected
         (concat
          "r\"\"\"Alpha bravo charlie\n"
          "delta echo foxtrot.\n"
          "\n"
          "Golf hotel india juliet kilo lima.\n"
          "\"\"\"")))
    (with-temp-buffer
      (python-mode)
      (let ((inhibit-message t))
        (buffer-reset-text text-initial)
        (syntax-propertize (point-max))
        (goto-char (point-min))
        (search-forward "Golf")
        (set-mark (point))
        (search-forward "lima.")
        (activate-mark)
        (fancy-fill-paragraph)
        (should (equal text-expected (buffer-string)))))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; fancy-fill-paragraph-tests.el ends here
