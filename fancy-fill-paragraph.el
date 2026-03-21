;;; fancy-fill-paragraph.el --- Fancy paragraph fill -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-fancy-fill-paragraph
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Provide a fill-paragraph command with context aware formatting.
;;

;;; Usage:

;;
;; Write the following code to your .emacs file:
;;
;;   (require 'fancy-fill-paragraph)
;;
;; Or with `use-package':
;;
;;   (use-package fancy-fill-paragraph)
;;

;;; Code:

;; For `replace-region-contents', can be dropped when Emacs 30.1 support is removed.
(require 'subr-x)


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup fancy-fill-paragraph nil
  "Fancy paragraph fill."
  :group 'convenience)

(defcustom fancy-fill-paragraph-sentence-end-double-space t
  "When non-nil use two spaces after a period when joining sentences on a line."
  :type 'boolean)

(defcustom fancy-fill-paragraph-split-weights nil
  "Overrides for splitting weights at punctuation boundaries as a plist.
Each value is an integer from 0 to 100 where 0 means never split
at this boundary and 100 means always prefer splitting here.
Intermediate values bias the solver towards splitting at that boundary."
  :type '(plist :key-type symbol :value-type natnum))

(defcustom fancy-fill-paragraph-fill-column-margin 0
  "Number of columns to subtract from `fill-column' when filling.
The effective fill column is at least 1."
  :type 'natnum)

(defcustom fancy-fill-paragraph-fill-column-target 0
  "Target column for the solver, allowing lines to extend up to `fill-column'.
When zero, the target equals `fill-column'.
When a float (0.1 to 1.0), a fraction of the effective fill column.
When a negative integer, subtracted from the effective fill column.
When a positive integer, used directly, clamped to the effective fill column.
The solver minimizes raggedness around this target while still allowing
lines up to `fill-column' before applying overflow penalties."
  :type
  '(choice (float :tag "Fraction of fill-column (0.1 .. 1.0)")
           (integer :tag "Zero (disabled), negative (offset), or positive (literal)")))

(defcustom fancy-fill-paragraph-blank-space-weight 75
  "Weight for joining items across delimiter boundaries.
An integer from 0 to 100 where higher values allow the solver to
more readily join items onto the same line across delimiters.
When zero, items will never be joined across delimiters,
each delimiter boundary will always produce a line break."
  :type 'natnum)

(defcustom fancy-fill-paragraph-infix-delimiters '(("--" . 40) ("-" . 40) ("/" . 10) ("~" . 10))
  "Alist of spaced infix delimiters and their split weights.
Each entry is (STRING . WEIGHT) where STRING is matched with
spaces on both sides.  For example \"-\" matches \" - \" in the text.
WEIGHT is an integer from 0 to 100 (see `fancy-fill-paragraph-split-weights')."
  :type '(alist :key-type string :value-type natnum))

(defcustom fancy-fill-paragraph-dot-point-prefix (list "- " "* ")
  "List of dot-point prefix strings to detect.
Each string is matched after optional leading blank-space.
Set to nil to disable dot-point detection entirely."
  :type '(repeat string))

(defcustom fancy-fill-paragraph-syntax-bounds t
  "When non-nil, constrain paragraphs to syntax boundaries.
In programming modes this treats each comment or string as a separate
paragraph, preventing `fancy-fill-paragraph' from merging text across
distinct comments or strings."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Private Helpers

(defun fancy-fill-paragraph--join-string-default (_pos)
  "Return a single space for joining items.
Argument _POS is ignored."
  (declare (important-return-value t))
  " ")

(defun fancy-fill-paragraph--join-string-sentence-end (_pos)
  "Return the join string for sentence-ending punctuation.
Uses double space when variable
`fancy-fill-paragraph-sentence-end-double-space' is non-nil.
Argument _POS is ignored."
  (declare (important-return-value t))
  (cond
   (fancy-fill-paragraph-sentence-end-double-space
    "  ")
   (t
    " ")))

(defsubst fancy-fill-paragraph--maybe-double-space (sep text split-pos)
  "Upgrade SEP to double-space when TEXT at SPLIT-POS follows a sentence end.
When `fancy-fill-paragraph-sentence-end-double-space' is active and the
delimiter at SPLIT-POS follows sentence-ending punctuation (e.g. `.\\'',
`.)'), return double-space.  Exclude continuation punctuation (`,;:')
where the period is typically an abbreviation (e.g. `etc.,')."
  (declare (important-return-value t))
  (cond
   ((and fancy-fill-paragraph-sentence-end-double-space
         (>= split-pos 2)
         (not (memq (aref text (1- split-pos)) '(?, ?\; ?:)))
         (memq (aref text (- split-pos 2)) '(?. ?? ?! ?\u2026)))
    "  ")
   (t
    sep)))

(defsubst fancy-fill-paragraph--delimiter-weight (key props)
  "Return the effective weight for delimiter KEY with properties PROPS.
Uses the override from `fancy-fill-paragraph-split-weights' when present,
otherwise the default :weight from PROPS."
  (declare (important-return-value t))
  (cond
   ((plist-member fancy-fill-paragraph-split-weights key)
    (plist-get fancy-fill-paragraph-split-weights key))
   (t
    (or (plist-get props :weight) 0))))

(defsubst fancy-fill-paragraph--leading-indent (str)
  "Return the number of leading whitespace characters in STR."
  (declare (important-return-value t))
  (string-match "\\`[ \t]*" str)
  (match-end 0))

(defsubst fancy-fill-paragraph--earliest-match (p1 p2)
  "Return the earliest of two optional match positions P1 and P2.
Either or both may be nil."
  (declare (important-return-value t))
  (cond
   ((and p1 p2)
    (min p1 p2))
   (t
    (or p1 p2))))

(defsubst fancy-fill-paragraph--prefix-column-width (pos prefix)
  "Return the column width of PREFIX at buffer position POS."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (goto-char (min (+ pos (length prefix)) (pos-eol)))
    (current-column)))

(defsubst fancy-fill-paragraph--last-bol (end)
  "Return the beginning-of-line position for buffer position END."
  (declare (important-return-value t))
  (save-excursion
    (goto-char end)
    (pos-bol)))

(defsubst fancy-fill-paragraph--string-blank-or-empty-p (str)
  "Return non-nil if STR is empty or contains only whitespace and newlines."
  (declare (important-return-value t))
  (string-match-p "\\`[ \t\n]*\\'" str))

(defsubst fancy-fill-paragraph--empty-syntax-region-p (beg end)
  "Return non-nil if the syntax region BEG..END has no fillable content.
True when the delimiters are on separate lines and the opener line
contains only the delimiter (no text after it)."
  (declare (important-return-value t))
  (and (< beg
          (save-excursion
            (goto-char end)
            (pos-bol)))
       (save-excursion
         (goto-char beg)
         (skip-chars-forward "^ \t\n" (pos-eol))
         (skip-chars-forward " \t" (pos-eol))
         (eolp))))

(defsubst fancy-fill-paragraph--strip-indent (line n)
  "Strip N leading characters from LINE.
Falls back to `string-trim-left' when LINE is shorter than N."
  (declare (important-return-value t))
  (cond
   ((>= (length line) n)
    (substring line n))
   (t
    (string-trim-left line))))


;; ---------------------------------------------------------------------------
;; Private Helpers (Syntax)

(defsubst fancy-fill-paragraph--ppss-start (ppss)
  "Return the start position of the innermost string or comment from PPSS.
Extracts element 8 of the `syntax-ppss' result, which holds the
character position of the start of the enclosing string or comment,
or nil when outside both."
  (declare (important-return-value t))
  (nth 8 ppss))

(defsubst fancy-fill-paragraph--syntax-string-delimiter-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR is a string delimiter.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       ;; Class 7: string-quote (e.g. \" in C).
       ;; Class 15: string-fence (used by `python-mode' for triple-quoted strings).
       (memq (syntax-class syn-descriptor) '(7 15))))

(defsubst fancy-fill-paragraph--syntax-comment-end-class-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR indicates a `comment-end' character.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       ;; Class 12: comment-end (e.g. newline in Lisp).
       ;; Bit 19: second character of a two-character comment ender (e.g. / in */).
       (or (= (syntax-class syn-descriptor) 12)
           (not (zerop (logand (car syn-descriptor) (ash 1 19)))))))


;; ---------------------------------------------------------------------------
;; Private Macros

(defmacro fancy-fill-paragraph--with-result-list (tail &rest body)
  "Execute BODY with TAIL bound to a sentinel list tail, return the result list.
Use `fancy-fill-paragraph--append-tail' within BODY to append items."
  (declare (indent 1))
  (let ((head (make-symbol "head")))
    `(let* ((,head (cons nil nil))
            (,tail ,head))
       ,@body
       (cdr ,head))))

(defmacro fancy-fill-paragraph--append-tail (tail items)
  "Append ITEMS to the sentinel list at TAIL, updating TAIL to the new end.
TAIL must be a place (symbol) holding the current tail cons cell."
  (declare (indent 1))
  (let ((val (make-symbol "val")))
    `(let ((,val ,items))
       (when ,val
         (setcdr ,tail ,val)
         (setq ,tail (last ,val))))))

(defmacro fancy-fill-paragraph--do-table (key props &rest body)
  "Execute BODY for each delimiter table entry, binding KEY and PROPS.
KEY is bound to the entry keyword, PROPS to its property list."
  (declare (indent 2))
  (let ((table (make-symbol "table")))
    `(let ((,table fancy-fill-paragraph--delimiter-table))
       (while ,table
         (let ((,key (car ,table))
               (,props (cadr ,table)))
           ,@body)
         (setq ,table (cddr ,table))))))

(defmacro fancy-fill-paragraph--delim (type join weight &rest patterns)
  "Generate a delimiter entry plist with match-point lambda.
TYPE is `close' (split after delimiter) or `open' (split at leading space).
JOIN is symbol `sentence-end' or `default'.
WEIGHT is the split weight (0-100).
PATTERNS are one or two search strings."
  (let* ((join-fn
          (cond
           ((eq join 'sentence-end)
            '#'fancy-fill-paragraph--join-string-sentence-end)
           (t
            '#'fancy-fill-paragraph--join-string-default)))
         (search-form
          (lambda (pat)
            (cond
             ((eq type 'close)
              (let ((off (1- (length pat))))
                `(let ((p (string-search ,pat text pos)))
                   (when p
                     (+ p ,off)))))
             (t
              `(string-search ,pat text pos)))))
         (match-body
          (cond
           ((= (length patterns) 1)
            (funcall search-form (car patterns)))
           (t
            `(fancy-fill-paragraph--earliest-match
              ,(funcall search-form (car patterns)) ,(funcall search-form (cadr patterns)))))))
    `(list :weight ,weight :match-point (lambda (text pos) ,match-body) :join-string ,join-fn)))


;; ---------------------------------------------------------------------------
;; Private Variables

(defconst fancy-fill-paragraph--weight-scale 100
  "Maximum value for break and join weights (0 to this value).")

;; NOTE: this can't use `most-positive-fixnum' because an oversized single item
;; is the only valid placement, and cost[i] must remain finite so subsequent
;; items can build on it (infinite cost would make the rest of the DP unreachable).
(defconst fancy-fill-paragraph--overflow-penalty (* 10 fancy-fill-paragraph--weight-scale)
  "Penalty multiplier for lines that exceed `fill-column'.")

(defconst fancy-fill-paragraph--prevent-dot-point-penalty
  (- fancy-fill-paragraph--overflow-penalty)
  "Break weight used to prevent false dot-point lines.
Applied to boundaries where breaking would place an item resembling
a dot-point prefix at the start of a line.")

(defconst fancy-fill-paragraph--delimiter-table
  (list
   :ellipsis (fancy-fill-paragraph--delim close sentence-end 60 "... " "\u2026 ")
   :period (fancy-fill-paragraph--delim close sentence-end 60 ". ")
   :comma (fancy-fill-paragraph--delim close default 40 ", ")
   :colon (fancy-fill-paragraph--delim close default 50 ": ")
   :semicolon (fancy-fill-paragraph--delim close default 50 "; ")
   :question (fancy-fill-paragraph--delim close sentence-end 60 "? ")
   :exclamation (fancy-fill-paragraph--delim close sentence-end 60 "! ")
   :em-dash (fancy-fill-paragraph--delim close default 40 "\u2014 ")
   :en-dash (fancy-fill-paragraph--delim close default 40 "\u2013 ")
   :paren (fancy-fill-paragraph--delim close default 40 ") ")
   :bracket (fancy-fill-paragraph--delim close default 40 "] ")
   :brace (fancy-fill-paragraph--delim close default 40 "} ")
   :double-quote (fancy-fill-paragraph--delim close default 40 "\" " "\u201D ")
   :single-quote (fancy-fill-paragraph--delim close default 40 "' " "\u2019 ")
   :open-paren (fancy-fill-paragraph--delim open default 10 " (")
   :open-bracket (fancy-fill-paragraph--delim open default 10 " [")
   :open-brace (fancy-fill-paragraph--delim open default 10 " {")
   :open-double-quote (fancy-fill-paragraph--delim open default 10 " \"" " \u201C")
   :open-single-quote (fancy-fill-paragraph--delim open default 10 " '" " \u2018")
   :space
   (list
    :weight 1
    :match-point
    (lambda (text pos)
      (let ((p (string-search " " text pos))
            (text-len (length text)))
        ;; Skip spaces that start " X " infix patterns, otherwise :space
        ;; matches at the leading space, stealing the match from the infix
        ;; delimiter and producing the wrong split position.
        (when fancy-fill-paragraph-infix-delimiters
          (let ((skip-end nil))
            (while (and p
                        (progn
                          (setq skip-end nil)
                          (dolist (entry fancy-fill-paragraph-infix-delimiters)
                            (when (and (null skip-end)
                                       (> (cdr entry) 0)
                                       (let ((end (+ p 1 (length (car entry)))))
                                         (and (<= (1+ end) text-len)
                                              (eq (aref text end) ?\s)
                                              (eq
                                               (compare-strings
                                                text (1+ p) end (car entry) 0 nil)
                                               t))))
                              (setq skip-end (+ p 1 (length (car entry))))))
                          skip-end))
              (setq p (string-search " " text (1+ skip-end))))))
        p))
    :join-string #'fancy-fill-paragraph--join-string-default))
  "Internal table of delimiter definitions.
An ordered plist where each value is a plist with:
- :weight - Default split weight (0-100), overridden by
  `fancy-fill-paragraph-split-weights'.
- :match-point - Function taking (TEXT POS), searches forward from POS
  and returns the split index (position after the delimiter character) or nil.
- :join-string - Function taking (POS), returns the separator string
  to use when joining items at this boundary on the same line.
Order matters: when multiple delimiters match at the same position,
the first entry in the table takes priority.")

;; Append available keys from the delimiter table to the doc-string
;; so `C-h v' and readme generation stay in sync with the table.
(put
 'fancy-fill-paragraph-split-weights 'variable-documentation
 (concat
  (get 'fancy-fill-paragraph-split-weights 'variable-documentation)
  "\n\nAvailable keys (with default weights):\n\n"
  (let ((entries nil))
    (fancy-fill-paragraph--do-table key props
      (push (format "- %s %d" key (plist-get props :weight)) entries))
    (mapconcat #'identity (nreverse entries) "\n"))))


;; ---------------------------------------------------------------------------
;; Private Functions

(defun fancy-fill-paragraph--fill-paragraphs-in-string
    (text local-fill-column &optional prefix-stripped)
  "Fill each paragraph in TEXT independently within LOCAL-FILL-COLUMN.
Paragraphs are separated by blank lines.  Returns the filled text
with paragraph separators preserved.
When PREFIX-STRIPPED is non-nil, set `fill-prefix' to the empty string
so `fill-context-prefix' detection is suppressed (the caller already
removed the prefix from TEXT)."
  (declare (important-return-value t))
  ;; Nothing to fill when text is empty or whitespace-only.
  (cond
   ((fancy-fill-paragraph--string-blank-or-empty-p text)
    text)
   (t
    (with-temp-buffer
      (insert text)
      (let ((fill-column local-fill-column)
            (fill-prefix
             (when prefix-stripped
               "")))
        (let ((bounds nil))
          (goto-char (point-min))
          (while (progn
                   (skip-chars-forward " \t\n" (point-max))
                   (< (point) (point-max)))
            (let ((para-beg (pos-bol)))
              (forward-paragraph 1)
              (let ((resume (point)))
                (skip-chars-backward " \t\n" para-beg)
                (push (cons para-beg (pos-eol)) bounds)
                (goto-char resume))))
          (dolist (para-bounds bounds)
            (fancy-fill-paragraph--fill-region (car para-bounds) (cdr para-bounds)))))
      (buffer-string)))))

(defun fancy-fill-paragraph--fill-and-reprefix
    (lines local-fill-column cont-prefix &optional first-prefix)
  "Fill LINES within LOCAL-FILL-COLUMN and re-add CONT-PREFIX to each output line.
FIRST-PREFIX, when non-nil, is used instead of CONT-PREFIX on the
first output line (e.g. for a comment opener like \"/** \")."
  (declare (important-return-value t))
  (let ((stripped-text (mapconcat #'identity lines "\n")))
    ;; Return nil when stripped text is empty or whitespace-only.
    (cond
     ((fancy-fill-paragraph--string-blank-or-empty-p stripped-text)
      nil)
     (t
      (let* ((filled-text
              (fancy-fill-paragraph--fill-paragraphs-in-string stripped-text local-fill-column t))
             (prefix-trimmed (string-trim-right cont-prefix))
             (filled-lines (split-string filled-text "\n"))
             (is-first t))
        (mapconcat (lambda (line)
                     (cond
                      ((string-empty-p line)
                       prefix-trimmed)
                      (is-first
                       (setq is-first nil)
                       (concat (or first-prefix cont-prefix) line))
                      (t
                       (concat cont-prefix line))))
                   filled-lines
                   "\n"))))))

(defun fancy-fill-paragraph--paragraph-bounds ()
  "Return (BEG . END) of the current paragraph."
  (declare (important-return-value t))
  (save-excursion
    (let (beg
          end)
      (forward-paragraph 1)
      (skip-chars-backward " \t\n")
      (setq end (pos-eol))
      (backward-paragraph 1)
      (skip-chars-forward " \t\n")
      (setq beg (pos-bol))
      (cons beg end))))

(defun fancy-fill-paragraph--syntax-body-bounds (beg end)
  "Return body bounds between separate-line delimiters, or nil.
BEG and END are the full region bounds (from `pos-bol' of the opener
to `pos-eol' of the closer).  When the first and last lines contain
only delimiters, return (BODY-BEG . BODY-END) for the lines between
them.  Otherwise return nil, indicating inline delimiters."
  (declare (important-return-value t))
  (save-excursion
    ;; End of the last body line (line before the closer).
    (goto-char end)
    (let ((body-end (pos-eol 0)))
      (goto-char beg)
      (when (and (zerop (forward-line 1)) (> body-end (point)))
        (cons (point) body-end)))))

(defun fancy-fill-paragraph--syntax-comment-end-p ()
  "Return non-nil if point follows a block comment closer.
Newlines are excluded because they terminate line comments,
not block comments."
  (declare (important-return-value t))
  (and (not (eq (char-before) ?\n))
       (fancy-fill-paragraph--syntax-comment-end-class-p (syntax-after (1- (point))))))

(defun fancy-fill-paragraph--syntax-regions (beg end pos)
  "Return list of (BEG END FILL-FN) for each syntax region in BEG..END.
POS is the cursor position, used to detect when point is inside a
comment or string whose opener precedes BEG.
Each entry spans from `pos-bol' of the opener to `pos-eol' of the closer.
Block comments include the opener position as a fourth element.
Line-style comments are skipped so they fall through to normal paragraph
filling.  Returns nil when no syntax boundaries are found."
  (declare (important-return-value t))
  (save-excursion
    ;; Ensure syntax properties are set for the entire region.
    ;; Modes like `python-mode' rely on `syntax-propertize-function'
    ;; to mark triple-quoted strings.
    (syntax-propertize end)
    (let ((regions nil))
      (goto-char beg)
      ;; If BEG or POS is inside a comment or string, retreat to
      ;; its opener.  This handles the case where paragraph bounds
      ;; extend beyond the syntax region (e.g. code surrounds the
      ;; comment).
      (let ((syn-start
             (or (fancy-fill-paragraph--ppss-start (save-excursion (syntax-ppss)))
                 (fancy-fill-paragraph--ppss-start (save-excursion (syntax-ppss pos))))))
        (when syn-start
          (goto-char syn-start)))
      (while (< (point) end)
        ;; Skip whitespace between regions.
        (skip-chars-forward " \t\n" end)
        (when (< (point) end)
          (let ((region-line-beg (pos-bol))
                (opener-pos (point)))
            (cond
             ;; Try advancing over a comment.
             ((forward-comment 1)
              (when (fancy-fill-paragraph--syntax-comment-end-p)
                (push (list
                       region-line-beg
                       (pos-eol)
                       #'fancy-fill-paragraph--fill-block-comment-region
                       opener-pos)
                      regions)))
             ;; Not a comment; check for a string delimiter.
             ((fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after (point)))
              (condition-case nil
                  (progn
                    (forward-sexp 1)
                    (push (list
                           region-line-beg (pos-eol) #'fancy-fill-paragraph--fill-string-region)
                          regions))
                (scan-error
                 (goto-char end))))
             (t
              ;; Neither comment nor string, stop scanning.
              (goto-char end))))))
      (nreverse regions))))

(defun fancy-fill-paragraph--fill-block-comment-region (beg end comment-start-pos)
  "Fill a block comment in the region BEG..END.
COMMENT-START-POS is the buffer position of the comment opener.
When the opener and closer occupy their own lines, fills only the body
lines between them.  When they share lines with body text (inline),
temporarily transforms the opener so all lines share the continuation
prefix, fills, then restores the opener."
  (save-excursion
    (comment-normalize-vars)
    (let* ((opener-col
            (progn
              (goto-char comment-start-pos)
              (current-column)))
           ;; Build the continuation prefix.
           ;; Modes with `c-block-comment-prefix': use it (e.g. "* ").
           ;; Other modes: use `comment-continue' when set by
           ;; `comment-normalize-vars' (e.g. " * " for c-mode,
           ;; "" for nxml-mode), otherwise derive from the opener text.
           (cont-prefix
            (concat
             (make-string opener-col ?\s)
             (cond
              ((and (boundp 'c-block-comment-prefix)
                    (stringp c-block-comment-prefix)
                    (not (string-empty-p c-block-comment-prefix)))
               (concat " " c-block-comment-prefix))
              ((stringp comment-continue)
               comment-continue)
              (t
               ;; Derive from the opener text: skip punctuation (".")
               ;; and symbol ("_") characters after the comment-start
               ;; character (e.g. "*" after "/" in "/*").
               (concat
                " "
                (buffer-substring-no-properties
                 (1+ comment-start-pos)
                 (progn
                   (goto-char (1+ comment-start-pos))
                   (skip-syntax-forward "._")
                   (point)))
                " "))))))
      (let ((body (fancy-fill-paragraph--syntax-body-bounds beg end)))
        (cond
         (body
          ;; Delimiters on own lines.
          ;; Check if all body lines match the continuation prefix,
          ;; accepting blank prefix lines (e.g. " *" for prefix " * ").
          ;; When they do, fill in a temporary buffer where blank comment
          ;; lines become real blank lines, so `forward-paragraph'
          ;; naturally separates sub-paragraphs.
          ;; When they don't (e.g. XML comments with no continuation
          ;; prefix), fall back to standard single-paragraph fill.
          (let* ((body-beg (car body))
                 (body-end (cdr body))
                 (prefix-matches
                  (save-excursion
                    (goto-char body-beg)
                    (let ((all-match t))
                      (while (and all-match (<= (point) body-end))
                        (let ((line (buffer-substring-no-properties (point) (pos-eol))))
                          (unless (string-prefix-p cont-prefix (concat line " "))
                            (setq all-match nil)))
                        (forward-line 1))
                      all-match))))
            (cond
             (prefix-matches
              (let* ((prefix-col
                      (save-excursion
                        (goto-char body-beg)
                        (goto-char (min (+ body-beg (length cont-prefix)) (pos-eol)))
                        (current-column)))
                     ;; Collect body lines, stripping the continuation prefix.
                     ;; Blank comment lines (shorter than the prefix) become "".
                     (lines
                      (save-excursion
                        (goto-char body-beg)
                        (let ((result nil))
                          (while (<= (point) body-end)
                            (move-to-column prefix-col)
                            (push (buffer-substring-no-properties (point) (pos-eol)) result)
                            (forward-line 1))
                          (nreverse result))))
                     (stripped-text (mapconcat #'identity lines "\n"))
                     (local-fill-column (max 1 (- fill-column prefix-col)))
                     (filled-text
                      (fancy-fill-paragraph--fill-paragraphs-in-string
                       stripped-text local-fill-column
                       t))
                     ;; Re-add continuation prefix to each line.
                     (prefix-trimmed (string-trim-right cont-prefix))
                     (result
                      (mapconcat (lambda (line)
                                   (cond
                                    ((string-empty-p line)
                                     prefix-trimmed)
                                    (t
                                     (concat cont-prefix line))))
                                 (split-string filled-text "\n")
                                 "\n")))
                (save-excursion (replace-region-contents body-beg body-end (lambda () result)))))
             (t
              ;; Prefix doesn't match body lines (e.g. XML comments),
              ;; fall back to standard fill.
              (let ((fill-prefix cont-prefix))
                (fancy-fill-paragraph--fill-region body-beg body-end))))))
         ;; Empty comment (e.g. "/*\n */") - nothing to fill.
         ((fancy-fill-paragraph--empty-syntax-region-p beg end))
         (t
          ;; Inline delimiters - fill without mutating the buffer.
          ;; Capture the opener text (e.g. "/* " or "/** ") and
          ;; use it as the first-line prefix when re-assembling.
          ;; Verify we are at a comment opener before proceeding.
          (unless (eq
                   comment-start-pos
                   (fancy-fill-paragraph--ppss-start (save-excursion (syntax-ppss (1- end)))))
            (error "Expected comment at %d" comment-start-pos))
          (let* ((prefix-col
                  (max (fancy-fill-paragraph--prefix-column-width beg cont-prefix)
                       (length cont-prefix)))
                 ;; The opener may be wider than cont-prefix (e.g. "/** "
                 ;; vs " * ").  Compute opener-col by skipping past the
                 ;; non-space opener characters and any trailing space(s).
                 (opener-col
                  (save-excursion
                    (goto-char comment-start-pos)
                    (skip-chars-forward "^ \t\n" (pos-eol))
                    (skip-chars-forward " \t" (pos-eol))
                    (current-column)))
                 (opener-text
                  (save-excursion
                    (goto-char beg)
                    (move-to-column opener-col)
                    (buffer-substring-no-properties beg (point))))
                 (last-bol (fancy-fill-paragraph--last-bol end))
                 ;; When the last line is no wider than prefix-col it
                 ;; holds only a closer (e.g. " */") and must be
                 ;; preserved verbatim since stripping loses its text.
                 (last-line-verbatim
                  (save-excursion
                    (goto-char last-bol)
                    (and (> last-bol beg)
                         (<= (- (pos-eol) last-bol) prefix-col)
                         (buffer-substring-no-properties last-bol (pos-eol)))))
                 ;; Collect content lines, stripping the prefix.
                 ;; First line at opener-col, rest at prefix-col.
                 ;; Exclude the closer line when preserved verbatim.
                 (content-end-bol
                  (cond
                   (last-line-verbatim
                    last-bol)
                   (t
                    (1+ last-bol))))
                 (lines
                  (save-excursion
                    (goto-char beg)
                    (move-to-column opener-col)
                    (let ((result (list (buffer-substring-no-properties (point) (pos-eol)))))
                      (forward-line 1)
                      (while (< (point) content-end-bol)
                        (move-to-column prefix-col)
                        (push (buffer-substring-no-properties (point) (pos-eol)) result)
                        (forward-line 1))
                      (nreverse result))))
                 (local-fill-column (max 1 (- fill-column prefix-col)))
                 (filled-result
                  (fancy-fill-paragraph--fill-and-reprefix lines local-fill-column cont-prefix
                                                           opener-text))
                 ;; Append the verbatim closer line when present.
                 (result
                  (cond
                   (last-line-verbatim
                    (concat filled-result "\n" last-line-verbatim))
                   (t
                    filled-result))))
            (save-excursion (replace-region-contents beg end (lambda () result))))))))))

(defun fancy-fill-paragraph--fill-string-region (beg end)
  "Fill a string in the region BEG..END.
When the delimiters occupy their own lines, fills only the body text
between them.  When the delimiters share lines with body text (inline
delimiters), fills the entire region, treating the delimiters as part
of the text."
  (let ((body (fancy-fill-paragraph--syntax-body-bounds beg end)))
    (cond
     (body
      ;; Delimiters on own lines - fill each paragraph independently.
      (let* ((body-beg (car body))
             (body-end (cdr body))
             (body-text (buffer-substring-no-properties body-beg body-end))
             (filled-text (fancy-fill-paragraph--fill-paragraphs-in-string body-text fill-column)))
        (save-excursion (replace-region-contents body-beg body-end (lambda () filled-text)))))
     ;; Empty string (e.g. """\n""") - nothing to fill.
     ((fancy-fill-paragraph--empty-syntax-region-p beg end))
     (t
      ;; Inline delimiters - fill the entire region.
      (fancy-fill-paragraph--fill-region beg end)))))


(defun fancy-fill-paragraph--paragraph-to-items (text)
  "Split TEXT into items at weighted punctuation boundaries.
Uses `fancy-fill-paragraph--delimiter-table' for delimiter definitions
and `fancy-fill-paragraph-split-weights' for active delimiters.
Returns a plist with keys:
- :items - list of strings.
- :break-weights - vector of break weights per item boundary.
- :seps - vector of separator strings per item boundary.
- :sep-lens - vector of separator lengths per item boundary."
  (declare (important-return-value t))
  (let ((items nil)
        (boundary-info nil)
        (pos 0)
        (len (length text))
        (d-count 0))

    ;; Count active delimiters.
    (fancy-fill-paragraph--do-table key props
      (when (> (fancy-fill-paragraph--delimiter-weight key props) 0)
        (setq d-count (1+ d-count))))

    ;; Count active infix delimiters.
    (dolist (entry fancy-fill-paragraph-infix-delimiters)
      (when (> (cdr entry) 0)
        (setq d-count (1+ d-count))))

    (let ((d-match-fns (make-vector d-count nil))
          (d-weights (make-vector d-count 0))
          (d-join-fns (make-vector d-count nil))
          (d-cached (make-vector d-count nil)))

      ;; Populate delimiter vectors.
      (let ((d 0))
        (fancy-fill-paragraph--do-table key props
          (let ((weight (fancy-fill-paragraph--delimiter-weight key props)))
            (when (> weight 0)
              (aset d-match-fns d (plist-get props :match-point))
              (aset d-weights d weight)
              (aset d-join-fns d (plist-get props :join-string))
              (setq d (1+ d)))))

        ;; Append infix delimiters (" X " patterns).
        (dolist (entry fancy-fill-paragraph-infix-delimiters)
          (let ((weight (cdr entry)))
            (when (> weight 0)
              (let ((pattern (concat " " (car entry) " "))
                    (offset (1+ (length (car entry)))))
                (aset
                 d-match-fns d
                 (lambda (text pos)
                   (let ((p (string-search pattern text pos)))
                     (when p
                       (+ p offset)))))
                (aset d-weights d weight)
                (aset d-join-fns d #'fancy-fill-paragraph--join-string-default)
                (setq d (1+ d)))))))

      ;; Scan for split points with cached match positions.
      (while (< pos len)
        (let ((best-split nil)
              (best-index nil)
              (d 0))
          (while (< d d-count)
            (let ((cached-pos (aref d-cached d)))
              ;; Refresh stale or uninitialized cache ('no-match avoids re-searching).
              (when (or (null cached-pos) (and (integerp cached-pos) (<= cached-pos pos)))
                (setq cached-pos (or (funcall (aref d-match-fns d) text pos) 'no-match))
                (aset d-cached d cached-pos))
              ;; Earliest (leftmost) match wins; table order breaks ties.
              (when (and (integerp cached-pos) (or (null best-split) (< cached-pos best-split)))
                (setq best-split cached-pos)
                (setq best-index d)))
            (setq d (1+ d)))
          (cond
           (best-split
            (push (substring text pos best-split) items)
            (push (cons best-index best-split) boundary-info)
            ;; Skip the space after the delimiter.
            (setq pos (1+ best-split)))
           (t
            (push (substring text pos) items)
            (setq pos len)))))

      (setq items (nreverse items))
      (setq boundary-info (nreverse boundary-info))

      ;; Build boundary metadata vectors.
      (let* ((n (length items))
             (break-weights (make-vector n 0))
             (seps (make-vector n " "))
             (sep-lens (make-vector n 1))
             (boundaries-remaining boundary-info)
             (i 0))
        (while boundaries-remaining
          (let* ((boundary (car boundaries-remaining))
                 (d-index (car boundary))
                 (split-pos (cdr boundary))
                 (sep
                  (fancy-fill-paragraph--maybe-double-space
                   (funcall (aref d-join-fns d-index) split-pos) text split-pos)))
            (aset break-weights i (aref d-weights d-index))
            (aset seps i sep)
            (aset sep-lens i (string-width sep)))
          (setq boundaries-remaining (cdr boundaries-remaining))
          (setq i (1+ i)))

        (list :items items :break-weights break-weights :seps seps :sep-lens sep-lens)))))

(defun fancy-fill-paragraph--solve
    (items local-fill-column break-weights seps sep-lens blank-space-weight)
  "Find optimal line breaks for ITEMS within LOCAL-FILL-COLUMN.
ITEMS is a list of strings to arrange into lines.
BREAK-WEIGHTS is a vector of break preference weights per item boundary.
SEPS is a vector of separator strings per item boundary.
SEP-LENS is a vector of separator lengths per item boundary.
BLANK-SPACE-WEIGHT controls joining items across delimiters (0-100).
Returns a list of strings, one per output line.
Uses dynamic programming to minimize raggedness."
  (declare (important-return-value t))
  (let* ((n (length items))
         (items-vec (make-vector n nil))
         (lens (make-vector n 0))
         (cost (make-vector (1+ n) most-positive-fixnum))
         (from (make-vector (1+ n) 0))
         (blank-space-positive (> blank-space-weight 0))
         ;; Soft target: slack is measured from target-column,
         ;; overflow from local-fill-column.
         ;; When zero (default), target-column equals local-fill-column
         ;; for zero inner-loop overhead.
         (target-column
          (let ((target fancy-fill-paragraph-fill-column-target))
            (cond
             ((floatp target)
              (max 1 (round (* target local-fill-column))))
             ((< target 0)
              (max 1 (+ local-fill-column target)))
             ((> target 0)
              (min target local-fill-column))
             (t
              local-fill-column))))
         ;; Pre-computed factor for join penalty: higher blank-space-weight
         ;; lowers this value, making the solver more willing to join items.
         ;; Scaled by target-column to be commensurate with squared-slack costs.
         (join-cost-factor
          (* (- fancy-fill-paragraph--weight-scale blank-space-weight) target-column)))

    ;; Pre-compute item vector and lengths.
    (let ((items-remaining items)
          (i 0))
      (while items-remaining
        (aset items-vec i (car items-remaining))
        (aset lens i (string-width (car items-remaining)))
        (setq items-remaining (cdr items-remaining))
        (setq i (1+ i))))

    ;; DP: cost[i] = minimum cost to arrange items[0..i-1] into lines.
    ;; from[i] = start index of the last line (items[from[i]..i-1]).
    (aset cost 0 0)
    (let ((i 1))
      (while (<= i n)
        ;; Try putting items[j..i-1] on one line.
        ;; Start with just the last item and extend backwards.
        (let* ((i-1 (1- i))
               (is-last (= i n))
               ;; Penalty for breaking after item i-1. Scaled by target-column so
               ;; break preferences are commensurate with squared-slack costs.
               (break-penalty
                (cond
                 (is-last
                  0)
                 (t
                  (* (- fancy-fill-paragraph--weight-scale (aref break-weights i-1))
                     target-column))))
               (line-len (aref lens i-1))
               (best-cost-i (aref cost i)))
          ;; Single-item case: items[i-1] alone on a line.
          ;; No joins, so join-penalty is zero.
          (let ((prev-cost (aref cost i-1)))
            (when (< prev-cost best-cost-i)
              (let* ((line-cost
                      (cond
                       (is-last
                        0)
                       ((<= line-len local-fill-column)
                        (let ((slack (- target-column line-len)))
                          (+ (* slack slack) break-penalty)))
                       (t
                        (let ((overflow (- line-len local-fill-column)))
                          (+ (* overflow overflow fancy-fill-paragraph--overflow-penalty)
                             break-penalty)))))
                     (total (+ prev-cost line-cost)))
                (when (< total best-cost-i)
                  (aset cost i total)
                  (aset from i i-1)
                  (setq best-cost-i total)))))
          ;; Multi-item candidates: extend line backwards.
          ;; Only when joining across delimiters is allowed.
          (when blank-space-positive
            (let ((j (1- i-1)))
              (when (>= j 0)
                ;; Pre-extend to a 2-item line before the loop (single-item
                ;; was handled above), so the while body always has j < i-1.
                (setq line-len (+ (aref lens j) (aref sep-lens j) line-len))
                (let ((join-weight-sum (aref break-weights j)))
                  (while (and (>= j 0) (<= line-len local-fill-column))
                    (let ((prev-cost (aref cost j)))
                      (when (< prev-cost best-cost-i)
                        (let* ((join-penalty
                                (/ (* join-weight-sum join-cost-factor)
                                   fancy-fill-paragraph--weight-scale))
                               (line-cost
                                (cond
                                 ;; Last line: only join penalty.
                                 (is-last
                                  join-penalty)
                                 ;; Fits: squared slack + break penalty + join penalty.
                                 (t
                                  (let ((slack (- target-column line-len)))
                                    (+ (* slack slack) break-penalty join-penalty)))))
                               (total (+ prev-cost line-cost)))
                          (when (< total best-cost-i)
                            (aset cost i total)
                            (aset from i j)
                            (setq best-cost-i total)))))
                    ;; Extend to include one more item.
                    (setq j (1- j))
                    (when (>= j 0)
                      (setq line-len (+ (aref lens j) (aref sep-lens j) line-len))
                      (setq join-weight-sum (+ join-weight-sum (aref break-weights j))))))))))
        (setq i (1+ i))))

    ;; Reconstruct lines by walking `from' backwards from n to 0.
    ;; Each from[index] gives the start of the line ending at index.
    ;; Pushing produces forward order without needing nreverse.
    (let ((lines nil)
          (index n))
      (while (> index 0)
        (let* ((start (aref from index))
               (parts nil)
               (k start))
          (while (< k index)
            (when (> k start)
              (push (aref seps (1- k)) parts))
            (push (aref items-vec k) parts)
            (setq k (1+ k)))
          (push (apply #'concat (nreverse parts)) lines))
        (setq index (aref from index)))
      lines)))


(defun fancy-fill-paragraph--dot-point-rx-list ()
  "Build a list of (REGEX . DP-PREFIX-STRING) for each configured prefix.
The regex matches the prefix after optional leading blank-space."
  (declare (important-return-value t))
  (mapcar
   (lambda (dp) (cons (concat "\\`\\([ \t]*\\)" (regexp-quote dp)) dp))
   fancy-fill-paragraph-dot-point-prefix))

(defun fancy-fill-paragraph--line-dot-point-match (line rx-list)
  "Check if LINE matches a dot-point prefix using pre-built RX-LIST.
RX-LIST is from `fancy-fill-paragraph--dot-point-rx-list'.
Returns (INDENT-WIDTH . DP-PREFIX-STRING) or nil.
INDENT-WIDTH is the number of leading blank-space characters."
  (declare (important-return-value t))
  (let ((result nil)
        (entries-remaining rx-list))
    (while (and entries-remaining (null result))
      (let ((rx-entry (car entries-remaining)))
        (when (string-match (car rx-entry) line)
          ;; Use match positions to compute indent width, avoiding `match-string' allocation.
          (setq result (cons (- (match-end 1) (match-beginning 1)) (cdr rx-entry)))))
      (setq entries-remaining (cdr entries-remaining)))
    result))

(defun fancy-fill-paragraph--fill-lines-plain (lines local-fill-column)
  "Fill LINES as a single paragraph within LOCAL-FILL-COLUMN.
LINES is a list of strings.  Returns a list of result line strings.
Joins lines, normalizes blank-space, splits at delimiters, and solves."
  (declare (important-return-value t))
  (let* ((text (mapconcat #'identity lines " "))
         (text (replace-regexp-in-string "  +" " " text))
         (text (string-trim text)))
    (cond
     ((not (string-empty-p text))
      (let* ((split-result (fancy-fill-paragraph--paragraph-to-items text))
             (items (plist-get split-result :items))
             (break-weights (plist-get split-result :break-weights)))
        ;; Penalize breaks that would create false dot-point lines.
        ;; When an item would look like a dot-point prefix if placed at the
        ;; beginning of a line, set a very high cost on that break.
        (when fancy-fill-paragraph-dot-point-prefix
          (let ((items-tail (cdr items))
                (i 0))
            (while items-tail
              (let ((item-with-space (concat (car items-tail) " ")))
                (dolist (dp fancy-fill-paragraph-dot-point-prefix)
                  (when (string-prefix-p dp item-with-space)
                    (aset break-weights i fancy-fill-paragraph--prevent-dot-point-penalty))))
              (setq items-tail (cdr items-tail))
              (setq i (1+ i)))))
        (fancy-fill-paragraph--solve
         items
         local-fill-column
         break-weights
         (plist-get split-result :seps)
         (plist-get split-result :sep-lens)
         fancy-fill-paragraph-blank-space-weight)))
     (t
      nil))))

(defun fancy-fill-paragraph--fill-lines-indent-split (lines local-fill-column rx-list)
  "Fill LINES, splitting into sub-paragraphs at indentation changes.
LOCAL-FILL-COLUMN is the target line width.
Groups consecutive lines with the same leading whitespace and fills
each group independently via `fancy-fill-paragraph--fill-lines'.
Falls through to plain fill when all lines share the same indentation.
RX-LIST is passed through for dot-point detection in recursive calls."
  (declare (important-return-value t))
  (let* ((n (length lines))
         (indents (make-vector n 0))
         (has-change nil)
         (lines-remaining lines)
         (i 0))
    ;; Compute indent for each line.
    (while lines-remaining
      (aset indents i (fancy-fill-paragraph--leading-indent (car lines-remaining)))
      (setq lines-remaining (cdr lines-remaining))
      (setq i (1+ i)))
    ;; Check if indentation varies.
    (when (> n 1)
      (let ((first-indent (aref indents 0))
            (j 1))
        (while (and (< j n) (not has-change))
          (when (/= (aref indents j) first-indent)
            (setq has-change t))
          (setq j (1+ j)))))
    (cond
     ((not has-change)
      (fancy-fill-paragraph--fill-lines-plain lines local-fill-column))
     (t
      ;; Group consecutive lines by indent level.
      (let ((groups nil)
            (current-indent (aref indents 0))
            (current-group nil))
        (setq i 0)
        (setq lines-remaining lines)
        (while lines-remaining
          (let ((indent (aref indents i)))
            (cond
             ((/= indent current-indent)
              ;; Indent changed: finalize current group, start new one.
              (push (cons current-indent (nreverse current-group)) groups)
              (setq current-group (list (car lines-remaining)))
              (setq current-indent indent))
             (t
              (push (car lines-remaining) current-group))))
          (setq lines-remaining (cdr lines-remaining))
          (setq i (1+ i)))
        (when current-group
          (push (cons current-indent (nreverse current-group)) groups))
        (setq groups (nreverse groups))
        ;; Fill each group independently.
        (fancy-fill-paragraph--with-result-list result-tail
          (dolist (group groups)
            (let* ((indent (car group))
                   (group-lines (cdr group))
                   (indent-str (make-string indent ?\s))
                   (sub-fill-column (max 1 (- local-fill-column indent)))
                   ;; Strip indent from each line.
                   (stripped
                    (mapcar
                     (lambda (line) (fancy-fill-paragraph--strip-indent line indent)) group-lines))
                   ;; Recursively fill (may trigger dot-point or further indent-split).
                   (filled (fancy-fill-paragraph--fill-lines stripped sub-fill-column rx-list)))
              ;; Re-add indent to each result line.
              (fancy-fill-paragraph--append-tail
                  result-tail
                (mapcar (lambda (line) (concat indent-str line)) filled))))))))))

(defun fancy-fill-paragraph--fill-lines (lines local-fill-column rx-list)
  "Fill LINES within LOCAL-FILL-COLUMN, handling dot-points recursively.
LINES is a list of strings.  Returns a list of result line strings.
RX-LIST is a pre-built regex list from
`fancy-fill-paragraph--dot-point-rx-list'.
When RX-LIST is non-nil, detects dot-point prefixes and fills each
item as a separate sub-paragraph.  Falls through to plain fill when
no dot-points are found."
  (declare (important-return-value t))
  (cond
   ;; Dot-point mode disabled (rx-list is nil), skip dot-point scan.
   ((null rx-list)
    (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column nil))
   (t
    (let* ((n (length lines))
           (matches (make-vector n nil))
           (has-dp nil)
           (min-indent most-positive-fixnum)
           (lines-remaining lines)
           (i 0))
      (while lines-remaining
        (let ((dp-match
               (fancy-fill-paragraph--line-dot-point-match (car lines-remaining) rx-list)))
          (aset matches i dp-match)
          (when dp-match
            (setq has-dp t)
            (when (< (car dp-match) min-indent)
              (setq min-indent (car dp-match)))))
        (setq lines-remaining (cdr lines-remaining))
        (setq i (1+ i)))
      (cond
       ;; No dot-points found, try indent-split.
       ((not has-dp)
        (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column rx-list))
       (t
        ;; Group lines into preamble and dot-point groups using cached matches.
        ;; Each group is (DP-PREFIX FIRST-LINE CONT-LINE ...) where
        ;; DP-PREFIX is the prefix string and the rest are plain line strings.
        (let ((groups nil)
              (current-group nil)
              (preamble nil)
              (found-first nil)
              (trailing nil)
              (lines-remaining lines))
          (setq i 0)
          (while (and lines-remaining (null trailing))
            (let ((line (car lines-remaining))
                  (dp-match (aref matches i)))
              (cond
               ;; Line is a dot-point at the minimum indent level.
               ;; Deeper dot-points become continuations, handled by recursion.
               ((and dp-match (= (car dp-match) min-indent))
                (when current-group
                  (push (nreverse current-group) groups))
                ;; Start group as (FIRST-LINE DP-PREFIX); after push+nreverse
                ;; it becomes (DP-PREFIX FIRST-LINE CONT-LINES...).
                (setq current-group (list line (cdr dp-match)))
                (setq found-first t))
               ;; Before first dot-point, accumulate preamble.
               ((not found-first)
                (push line preamble))
               ;; After first dot-point: continuation only if indent exceeds dp indent.
               (t
                (cond
                 ((> (fancy-fill-paragraph--leading-indent line) min-indent)
                  (push line current-group))
                 ;; Non-continuation: end dot-point section, process rest separately.
                 (t
                  (when current-group
                    (push (nreverse current-group) groups))
                  (setq current-group nil)
                  (setq trailing lines-remaining))))))
            (unless trailing
              (setq lines-remaining (cdr lines-remaining))
              (setq i (1+ i))))
          (when current-group
            (push (nreverse current-group) groups))
          (setq groups (nreverse groups))
          (setq preamble (nreverse preamble))

          ;; Indent string is shared (all groups share min-indent).
          (let ((indent-str (make-string min-indent ?\s)))
            (fancy-fill-paragraph--with-result-list result-tail

              ;; Fill preamble lines (may contain indent changes).
              (when preamble
                (fancy-fill-paragraph--append-tail
                    result-tail
                  (fancy-fill-paragraph--fill-lines preamble local-fill-column rx-list)))

              ;; Fill each dot-point group.
              ;; Group structure after nreverse: (DP-PREFIX FIRST-LINE CONT-LINES...).
              (dolist (group groups)
                (let* ((dp-prefix (car group))
                       (dp-prefix-len (length dp-prefix))
                       (indent-plus-dp (+ min-indent dp-prefix-len))
                       (cont-indent-str (make-string indent-plus-dp ?\s))
                       (sub-fill-column (max 1 (- local-fill-column indent-plus-dp)))
                       (body-lines nil)
                       (lines-remaining (cddr group))
                       (is-first t))
                  ;; Build body lines by stripping indent + dp-prefix.
                  ;; First line: strip indent + dp-prefix.
                  (push (substring (cadr group) indent-plus-dp) body-lines)
                  (while lines-remaining
                    (push (fancy-fill-paragraph--strip-indent (car lines-remaining) indent-plus-dp)
                          body-lines)
                    (setq lines-remaining (cdr lines-remaining)))
                  (setq body-lines (nreverse body-lines))

                  ;; Recursively fill the body.
                  (let ((filled
                         (fancy-fill-paragraph--fill-lines body-lines sub-fill-column rx-list))
                        (prefixed nil))
                    ;; Re-add indent + prefix.
                    (dolist (line filled)
                      (cond
                       (is-first
                        (setq is-first nil)
                        (push (concat indent-str dp-prefix line) prefixed))
                       (t
                        (push (concat cont-indent-str line) prefixed))))
                    (fancy-fill-paragraph--append-tail
                        result-tail
                      (nreverse prefixed)))))
              ;; Fill trailing (may contain more dot-points and indent changes).
              (when trailing
                (fancy-fill-paragraph--append-tail
                    result-tail
                  (fancy-fill-paragraph--fill-lines trailing local-fill-column rx-list))))))))))))

(defun fancy-fill-paragraph--fill-region (beg end)
  "Fill the paragraph in the region from BEG to END.
Detects the fill prefix, strips it, normalizes blank-space,
splits at delimiter boundaries, solves for optimal line breaks,
and re-adds the prefix to each output line."
  (let* ((prefix (or fill-prefix (fill-context-prefix beg end) ""))
         ;; Use `current-column' rather than `length' to handle tabs in prefix.
         (prefix-column-width
          (save-excursion
            (goto-char beg)
            (goto-char (min (+ beg (length prefix)) (pos-eol)))
            (current-column)))
         ;; Build regex list for prefix checking (always) and dot-point filling.
         (rx-list (fancy-fill-paragraph--dot-point-rx-list))
         (first-line
          (save-excursion
            (goto-char beg)
            (buffer-substring-no-properties (point) (pos-eol))))
         ;; Check if the fill prefix consumed a dot-point prefix.
         ;; If so, override with just the blank-space indent.  This prevents
         ;; `fill-context-prefix' from treating "- " as a fill prefix
         ;; and ensures continuation lines are indented correctly.
         (prefix
          (cond
           ((and rx-list (not (string-empty-p prefix)))
            (let ((m (fancy-fill-paragraph--line-dot-point-match first-line rx-list)))
              (cond
               ((and m (> (length prefix) (car m)))
                (setq prefix-column-width (car m))
                (make-string (car m) ?\s))
               (t
                prefix))))
           (t
            prefix)))
         ;; Ensure prefix matches all lines.
         ;; `fill-context-prefix' examines only the first two lines,
         ;; so later lines may not share the detected prefix.
         (prefix
          (cond
           ((not (string-empty-p prefix))
            (save-excursion
              (goto-char beg)
              (let ((all-match t)
                    (min-indent most-positive-fixnum))
                ;; Early exit: once a mismatch is found and min-indent
                ;; reaches 0, the result is "" regardless of remaining lines.
                (while (and (< (point) end) (or all-match (> min-indent 0)))
                  (let ((line (buffer-substring-no-properties (point) (pos-eol))))
                    (when (and all-match (not (string-prefix-p prefix line)))
                      (setq all-match nil))
                    (unless all-match
                      (setq min-indent
                            (min min-indent (fancy-fill-paragraph--leading-indent line)))))
                  (forward-line 1))
                (cond
                 (all-match
                  prefix)
                 (t
                  (setq prefix-column-width min-indent)
                  (make-string min-indent ?\s))))))
           (t
            prefix)))
         ;; Strip the prefix from each line by column position, returning a list.
         (lines
          (save-excursion
            (let ((result nil))
              (goto-char beg)
              (while (< (point) end)
                (move-to-column prefix-column-width)
                (push (buffer-substring-no-properties (point) (pos-eol)) result)
                (forward-line 1))
              (nreverse result))))
         ;; Fill using dot-point aware function.
         ;; Always pass rx-list so dot-points are automatically detected;
         ;; `--fill-lines' falls through to plain fill when none are found.
         (filled
          (fancy-fill-paragraph--fill-lines
           lines
           (max 1
                (- fill-column fancy-fill-paragraph-fill-column-margin prefix-column-width))
           rx-list)))
    (when filled
      (let ((result (mapconcat (lambda (line) (concat prefix line)) filled "\n")))
        (save-excursion
          ;; Use `replace-region-contents' to minimize undo differences
          ;; compared with delete-region + insert.
          ;; NOTE: when emacs 30.x is dropped, `result' can be passed in directly.
          (replace-region-contents beg end (lambda () result)))))))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun fancy-fill-paragraph ()
  "Fill the current paragraph with context aware formatting.
With an active region, fill each paragraph in the region separately.
Breaks lines preferring sentence boundaries."
  (interactive "*")
  (cond
   ((region-active-p)
    (let ((beg (region-beginning))
          (end (region-end))
          ;; Snapshot buffer size to compute adjusted end position after filling.
          (buf-size (buffer-size))
          (point-at-beg (< (point) (mark))))
      (save-excursion
        (let ((bounds nil))
          (goto-char beg)
          (while (progn
                   (skip-chars-forward " \t\n" end)
                   (< (point) end))
            (let ((para-beg (pos-bol)))
              (forward-paragraph 1)
              (when (> (point) end)
                (goto-char end))
              (let ((resume (point)))
                (skip-chars-backward " \t\n" para-beg)
                (push (cons para-beg (pos-eol)) bounds)
                (goto-char resume))))
          ;; Fill in reverse order so positions remain valid.
          (dolist (para-bounds bounds)
            (fancy-fill-paragraph--fill-region (car para-bounds) (cdr para-bounds)))))
      ;; Restore mark/point in original direction, deactivate mark.
      (let ((end-adjusted (+ end (- (buffer-size) buf-size))))
        (cond
         (point-at-beg
          (set-mark end-adjusted)
          (goto-char beg))
         (t
          (set-mark beg)
          (goto-char end-adjusted))))
      (setq deactivate-mark t)))
   (t
    (let* ((bounds (fancy-fill-paragraph--paragraph-bounds))
           (beg (car bounds))
           (end (cdr bounds))
           (sub-regions
            (when fancy-fill-paragraph-syntax-bounds
              (fancy-fill-paragraph--syntax-regions beg end (point)))))
      (cond
       (sub-regions
        ;; Fill each region independently, in reverse order so
        ;; earlier positions remain valid after later fills.
        ;; Save point as an integer since `replace-region-contents'
        ;; can displace `save-excursion' markers inside fill functions.
        (let ((saved-point (point)))
          (dolist (region (reverse sub-regions))
            (apply (nth 2 region) (nth 0 region) (nth 1 region) (nthcdr 3 region)))
          (goto-char saved-point)))
       (t
        (fancy-fill-paragraph--fill-region beg end)))))))

(provide 'fancy-fill-paragraph)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; fancy-fill-paragraph.el ends here
