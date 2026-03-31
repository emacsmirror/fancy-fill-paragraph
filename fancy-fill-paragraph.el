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

(defcustom fancy-fill-paragraph-dot-point-prefix (list "- " "* " '(:regexp . "[0-9]+\\. "))
  "List of dot-point prefix patterns to detect.
Each entry is one of:

- A string: matched literally after optional leading blank-space.
  For example \"- \" detects lines like \"  - item text\".

- A cons (:regexp . PATTERN): PATTERN is a regexp whose entire match
  is the prefix.  For example (:regexp . \"[0-9]+\\\\. \") detects
  numbered lists like \"1. \", \"10. \", \"123. \".

Set to nil to disable dot-point detection entirely."
  :type
  '(repeat (choice (string :tag "Literal")
                   (cons :tag "Regexp" (const :regexp) string))))

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
`sentence-end-double-space' is non-nil.
Argument _POS is ignored."
  (declare (important-return-value t))
  (cond
   (sentence-end-double-space
    "  ")
   (t
    " ")))

(defsubst fancy-fill-paragraph--maybe-double-space (sep text split-pos)
  "Upgrade SEP to double-space when TEXT at SPLIT-POS follows a sentence end.
When `sentence-end-double-space' is active and the
delimiter at SPLIT-POS follows sentence-ending punctuation (e.g. `.\\'',
`.)'), return double-space.  Exclude continuation punctuation (`,;:')
where the period is typically an abbreviation (e.g. `etc.,')."
  (declare (important-return-value t))
  (cond
   ((and sentence-end-double-space
         (>= split-pos 2)
         (not (memq (aref text (1- split-pos)) '(?, ?\; ?:)))
         (memq (aref text (- split-pos 2)) '(?. ?? ?! ?\u2026)))
    "  ")
   (t
    sep)))

(defsubst fancy-fill-paragraph--delimiter-entry-weight (entry)
  "Return the effective weight for delimiter ENTRY.
Uses the override from `fancy-fill-paragraph-split-weights' when present,
otherwise the default :weight from ENTRY."
  (declare (important-return-value t))
  (let ((key (plist-get entry :key)))
    (cond
     ((plist-member fancy-fill-paragraph-split-weights key)
      (plist-get fancy-fill-paragraph-split-weights key))
     (t
      (or (plist-get entry :weight) 0)))))

(defsubst fancy-fill-paragraph--leading-indent (str)
  "Return the number of leading blank-space characters in STR."
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

(defun fancy-fill-paragraph--extract-body-indent (pos opener-col)
  "Extract indentation characters from the line at POS up to OPENER-COL.
Preserves the actual blank-space (tabs or spaces) from the buffer
rather than synthesizing spaces from the column width."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (move-to-column opener-col)
    (buffer-substring-no-properties (pos-bol) (point))))

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
  "Return non-nil if STR is empty or contains only blank-space and newlines."
  (declare (important-return-value t))
  (string-match-p "\\`[ \t\n]*\\'" str))

(defsubst fancy-fill-paragraph--empty-syntax-region-p (beg end)
  "Return non-nil if the syntax region BEG..END has no fillable content.
True when the delimiters are on separate lines and the opener line
contains only the delimiter (no text after it)."
  (declare (important-return-value t))
  (and (< beg (fancy-fill-paragraph--last-bol end))
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

(defun fancy-fill-paragraph--collect-paragraph-bounds (beg end &optional clamp-end)
  "Return paragraph bounds in BEG..END in reverse order.
Each element is a cons cell (PARA-BEG . PARA-END).
When CLAMP-END is non-nil, clamp paragraph scanning to END before
recording the final bound.  Reverse ordering keeps later bounds first,
so callers can fill them without invalidating earlier positions."
  (declare (important-return-value t))
  (save-excursion
    (goto-char beg)
    (let ((bounds nil))
      (while (progn
               (skip-chars-forward " \t\n" end)
               (< (point) end))
        (let ((para-beg (pos-bol)))
          (forward-paragraph 1)
          (when (and clamp-end (> (point) end))
            (goto-char end))
          (let ((resume (point)))
            (skip-chars-backward " \t\n" para-beg)
            (push (cons para-beg (pos-eol)) bounds)
            (goto-char resume))))
      bounds)))

(defun fancy-fill-paragraph--fill-paragraph-bounds (bounds)
  "Fill each paragraph in BOUNDS.
BOUNDS is a reverse-ordered list of (BEG . END) pairs."
  (dolist (para-bounds bounds)
    (fancy-fill-paragraph--fill-region (car para-bounds) (cdr para-bounds))))


;; ---------------------------------------------------------------------------
;; Private Helpers (Syntax)

(defsubst fancy-fill-paragraph--ppss-start (ppss)
  "Return the start position of the innermost string or comment from PPSS.
Extracts element 8 of the `syntax-ppss' result, which holds the
character position of the start of the enclosing string or comment,
or nil when outside both."
  (declare (important-return-value t))
  (nth 8 ppss))

(defun fancy-fill-paragraph--comment-or-string-start-at (pos)
  "Return the comment or string start position for POS, or nil.
Probes `syntax-ppss' at POS and, when that yields nothing, at
POS+1 so that a cursor directly on a comment-start character
\(e.g. `#') or string delimiter is still detected."
  (declare (important-return-value t))
  (save-excursion
    (or (fancy-fill-paragraph--ppss-start (syntax-ppss pos))
        (cond
         ((< pos (point-max))
          (let ((beg (fancy-fill-paragraph--ppss-start (syntax-ppss (1+ pos)))))
            (and beg (<= beg pos) beg)))
         (t
          nil)))))

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


(defun fancy-fill-paragraph--delimiter-join-function (join)
  "Return the join function for JOIN."
  (declare (important-return-value t))
  (cond
   ((eq join 'sentence-end)
    #'fancy-fill-paragraph--join-string-sentence-end)
   (t
    #'fancy-fill-paragraph--join-string-default)))

(defun fancy-fill-paragraph--make-delimiter-match-point (type patterns)
  "Return a delimiter match-point function for TYPE and PATTERNS."
  (declare (important-return-value t))
  (lambda (text pos)
    (let ((matches
           (mapcar
            (lambda (pattern)
              (let ((match (string-search pattern text pos)))
                (when match
                  (cond
                   ((eq type 'close)
                    (+ match (1- (length pattern))))
                   (t
                    match)))))
            patterns)))
      (cond
       ((= (length matches) 1)
        (car matches))
       (t
        (fancy-fill-paragraph--earliest-match (car matches) (cadr matches)))))))

(defun fancy-fill-paragraph--make-delimiter (key type join weight &rest patterns)
  "Return one delimiter entry.
KEY is the user-facing key, TYPE is `close' or `open', JOIN selects the
join-string function, WEIGHT is the default split weight, and PATTERNS
are the string search patterns."
  (declare (important-return-value t))
  (list
   :key key
   :weight weight
   :match-point (fancy-fill-paragraph--make-delimiter-match-point type patterns)
   :join-string (fancy-fill-paragraph--delimiter-join-function join)))

(defun fancy-fill-paragraph--space-delimiter-match-point (text pos)
  "Return the next plain-space delimiter match in TEXT from POS.
Skips spaces that start active infix-delimiter patterns."
  (declare (important-return-value t))
  (let ((p (string-search " " text pos))
        (text-len (length text)))
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
                                           (compare-strings text (1+ p) end (car entry) 0 nil)
                                           t))))
                          (setq skip-end (+ p 1 (length (car entry))))))
                      skip-end))
          (setq p (string-search " " text (1+ skip-end))))))
    p))

(defun fancy-fill-paragraph--make-infix-delimiter (entry)
  "Return an active delimiter entry for infix ENTRY, or nil.
ENTRY is one element of `fancy-fill-paragraph-infix-delimiters'."
  (declare (important-return-value t))
  (let ((weight (cdr entry)))
    (when (> weight 0)
      (let ((pattern (concat " " (car entry) " "))
            (offset (1+ (length (car entry)))))
        (list
         :weight weight
         :match-point
         (lambda (text pos)
           (let ((p (string-search pattern text pos)))
             (when p
               (+ p offset))))
         :join-string #'fancy-fill-paragraph--join-string-default)))))

;; Forward declaration; defined as `defconst' below after its dependencies.
(defvar fancy-fill-paragraph--delimiter-table)

(defun fancy-fill-paragraph--active-delimiters ()
  "Return a vector of active delimiter entries in matching priority order."
  (declare (important-return-value t))
  (vconcat
   (delq
    nil
    (append
     (mapcar
      (lambda (entry)
        (let ((weight (fancy-fill-paragraph--delimiter-entry-weight entry)))
          (when (> weight 0)
            (list
             :key (plist-get entry :key)
             :weight weight
             :match-point (plist-get entry :match-point)
             :join-string (plist-get entry :join-string)))))
      fancy-fill-paragraph--delimiter-table)
     (mapcar
      #'fancy-fill-paragraph--make-infix-delimiter fancy-fill-paragraph-infix-delimiters)))))


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
  (append
   (mapcar
    (lambda (spec) (apply #'fancy-fill-paragraph--make-delimiter spec))
    '((:ellipsis close sentence-end 60 "... " "\u2026 ")
      (:period close sentence-end 60 ". ")
      (:comma close default 40 ", ")
      (:colon close default 50 ": ")
      (:semicolon close default 50 "; ")
      (:question close sentence-end 60 "? ")
      (:exclamation close sentence-end 60 "! ")
      (:em-dash close default 40 "\u2014 ")
      (:en-dash close default 40 "\u2013 ")
      (:paren close default 40 ") ")
      (:bracket close default 40 "] ")
      (:brace close default 40 "} ")
      (:double-quote close default 40 "\" " "\u201D ")
      (:single-quote close default 40 "' " "\u2019 ")
      (:open-paren open default 10 " (")
      (:open-bracket open default 10 " [")
      (:open-brace open default 10 " {")
      (:open-double-quote open default 10 " \"" " \u201C")
      (:open-single-quote open default 10 " '" " \u2018")))
   (list
    (list
     :key
     :space
     :weight 1
     :match-point #'fancy-fill-paragraph--space-delimiter-match-point
     :join-string #'fancy-fill-paragraph--join-string-default)))
  "Internal table of delimiter definitions.
An ordered list of plists where each entry contains:
- :key - User-facing delimiter key.
- :weight - Default split weight (0-100), overridden by
  `fancy-fill-paragraph-split-weights'.
- :match-point - Function taking (TEXT POS), searches forward from POS
  and returns the split index (position after the delimiter character) or nil.
- :join-string - Function taking (POS), returns the separator string
  to use when joining items at this boundary on the same line.
Order matters: when multiple delimiters match at the same position,
the first entry in the table takes priority.")

(defun fancy-fill-paragraph--split-weights-docstring ()
  "Return the full docstring for `fancy-fill-paragraph-split-weights'."
  (declare (important-return-value t))
  (concat
   (get 'fancy-fill-paragraph-split-weights 'variable-documentation)
   "\n\nAvailable keys (with default weights):\n\n"
   (let ((entries nil))
     (dolist (entry fancy-fill-paragraph--delimiter-table)
       (push (format "- %s %d" (plist-get entry :key) (plist-get entry :weight)) entries))
     (mapconcat #'identity (nreverse entries) "\n"))))

;; Append available keys from the delimiter table to the doc-string
;; so `C-h v' and readme generation stay in sync with the table.
(put
 'fancy-fill-paragraph-split-weights
 'variable-documentation
 (fancy-fill-paragraph--split-weights-docstring))


;; ---------------------------------------------------------------------------
;; Private Functions

(defun fancy-fill-paragraph--fill-paragraphs-in-string
    (text local-fill-column &optional suppress-prefix)
  "Fill each paragraph in TEXT independently within LOCAL-FILL-COLUMN.
Paragraphs are separated by blank lines.  Returns the filled text
with paragraph separators preserved.
When SUPPRESS-PREFIX is non-nil, set `fill-prefix' to the empty string
so `fill-context-prefix' detection is suppressed."
  (declare (important-return-value t))
  ;; Nothing to fill when text is empty or blank-space-only.
  (cond
   ((fancy-fill-paragraph--string-blank-or-empty-p text)
    text)
   (t
    (with-temp-buffer
      (insert text)
      (let ((fill-column local-fill-column)
            (fill-prefix (and suppress-prefix "")))
        (fancy-fill-paragraph--fill-paragraph-bounds
         (fancy-fill-paragraph--collect-paragraph-bounds (point-min) (point-max))))
      (buffer-string)))))

(defun fancy-fill-paragraph--fill-and-reprefix
    (lines local-fill-column cont-prefix &optional first-prefix)
  "Fill LINES within LOCAL-FILL-COLUMN and re-add CONT-PREFIX to each output line.
FIRST-PREFIX, when non-nil, is used instead of CONT-PREFIX on the
first output line (e.g. for a comment opener like \"/** \")."
  (declare (important-return-value t))
  (let ((stripped-text (mapconcat #'identity lines "\n")))
    ;; Return nil when stripped text is empty or blank-space-only.
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

(defun fancy-fill-paragraph--comment-or-string-body-bounds (beg end)
  "Return inner body bounds between delimiter lines, or nil.
BEG and END are the full region bounds (from `pos-bol' of the opener
to `pos-eol' of the closer).  Returns (BODY-BEG . BODY-END) spanning
from the first line after the opener to the last line before the closer,
or nil when there are no lines between them (single-line region).
Note: this returns the raw inner body regardless of whether the opener
line carries text.  Callers that need to include opener-line text in
paragraph scanning should widen the returned bounds themselves."
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

(defun fancy-fill-paragraph--line-comment-indent-col (comment-pos)
  "Return the column of the comment character at COMMENT-POS."
  (declare (important-return-value t))
  (save-excursion
    (goto-char comment-pos)
    (current-column)))

(defun fancy-fill-paragraph--line-comment-at-col-p (indent-col)
  "Return non-nil when the current line has a line comment at INDENT-COL."
  (declare (important-return-value t))
  (save-excursion
    (move-to-column indent-col)
    (let ((syn (syntax-after (point))))
      ;; Class 11 = comment-start.
      (and syn (= (syntax-class syn) 11)))))

(defun fancy-fill-paragraph--line-comment-bounds (opener-pos beg end)
  "Return (BLOCK-BEG . BLOCK-END) for consecutive line comments around OPENER-POS.
OPENER-POS is the comment-start of the line comment at point.
BEG and END bound the search.  Only lines whose comment character
starts at the same column as OPENER-POS are included."
  (declare (important-return-value t))
  (save-excursion
    (goto-char opener-pos)
    (let ((indent-col (fancy-fill-paragraph--line-comment-indent-col opener-pos))
          (block-beg (pos-bol))
          (block-end (pos-eol)))
      ;; Scan backward.
      (while (and (zerop (forward-line -1))
                  (>= (point) beg)
                  (fancy-fill-paragraph--line-comment-at-col-p indent-col))
        (setq block-beg (pos-bol)))
      ;; Scan forward.
      (goto-char opener-pos)
      (while (and (zerop (forward-line 1))
                  (<= (point) end)
                  (fancy-fill-paragraph--line-comment-at-col-p indent-col))
        (setq block-end (pos-eol)))
      (cons block-beg block-end))))

(defun fancy-fill-paragraph--line-comment-prefix (opener-pos)
  "Return the continuation prefix for the line comment at OPENER-POS.
The prefix includes leading indentation, comment delimiter characters,
and one trailing space when present."
  (declare (important-return-value t))
  (save-excursion
    (goto-char opener-pos)
    ;; Skip comment delimiter characters (e.g. \"#\", \";;\").
    ;; Syntax class `<' (comment-start) matches only the delimiter,
    ;; unlike a character-class skip which would consume content too.
    (skip-syntax-forward "<")
    ;; Include trailing blank-space after the delimiter.
    (skip-chars-forward " \t" (pos-eol))
    (buffer-substring-no-properties (pos-bol) (point))))

(defun fancy-fill-paragraph--fill-line-comment-region
    (beg end opener-pos &optional sel-beg sel-end)
  "Fill a line-comment block in the region BEG..END.
OPENER-POS is the buffer position of a comment character in the block.
When SEL-BEG and SEL-END are non-nil, fill only paragraphs
overlapping that selection.  Otherwise fill the paragraph at point."
  (let ((pos (point)))
    (save-excursion
      (let* ((cont-prefix (fancy-fill-paragraph--line-comment-prefix opener-pos))
             (prefix-col (string-width cont-prefix)))
        (fancy-fill-paragraph--fill-body-paragraphs
         beg end prefix-col (or sel-beg pos) (or sel-end pos)
         (lambda (para-beg para-end)
           (fancy-fill-paragraph--fill-prefixed-region para-beg para-end cont-prefix)))))))

(defun fancy-fill-paragraph--comment-or-string-regions (beg end pos)
  "Return list of (BEG END FILL-FN) for each syntax region in BEG..END.
POS is the cursor position, used to detect when point is inside a
comment or string whose opener precedes BEG.
Each entry spans from `pos-bol' of the opener to `pos-eol' of the closer.
Block comments and line-comment blocks include the opener position
as a fourth element.
Returns nil when no syntax boundaries are found."
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
             (or (fancy-fill-paragraph--comment-or-string-start-at (point))
                 (fancy-fill-paragraph--comment-or-string-start-at pos))))
        (when syn-start
          (goto-char syn-start)))
      (while (< (point) end)
        ;; Skip blank-space between regions.
        (skip-chars-forward " \t\n" end)
        (when (< (point) end)
          (let ((region-line-beg (pos-bol))
                (opener-pos (point)))
            (cond
             ;; Try advancing over a comment.
             ((forward-comment 1)
              (cond
               ;; Block comment.
               ((fancy-fill-paragraph--syntax-comment-end-p)
                (push (list
                       region-line-beg
                       (pos-eol)
                       #'fancy-fill-paragraph--fill-block-comment-region
                       opener-pos)
                      regions))
               ;; Line comment - find the full block and skip past it.
               (t
                (let ((bounds (fancy-fill-paragraph--line-comment-bounds opener-pos beg end)))
                  (push (list
                         (car bounds)
                         (cdr bounds)
                         #'fancy-fill-paragraph--fill-line-comment-region
                         opener-pos)
                        regions)
                  (goto-char (1+ (cdr bounds)))))))
             ;; Not a comment; check for a string delimiter.
             ((fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after (point)))
              (condition-case nil
                  (progn
                    (forward-sexp 1)
                    (push (list
                           region-line-beg
                           (pos-eol)
                           #'fancy-fill-paragraph--fill-string-region
                           opener-pos)
                          regions))
                (scan-error
                 (goto-char end))))
             (t
              ;; Neither comment nor string, stop scanning.
              (goto-char end))))))
      (nreverse regions))))

(defun fancy-fill-paragraph--block-comment-continuation-suffix (comment-start-pos)
  "Return the synthesized continuation suffix for a block comment.
COMMENT-START-POS is the buffer position of the comment opener."
  (declare (important-return-value t))
  (cond
   ((and (boundp 'c-block-comment-prefix)
         (stringp c-block-comment-prefix)
         (not (string-empty-p c-block-comment-prefix)))
    (concat " " c-block-comment-prefix))
   ((stringp comment-continue)
    comment-continue)
   (t
    ;; Derive from the opener text: skip punctuation (".") and
    ;; symbol ("_") characters after the comment-start character
    ;; (e.g. "*" after "/" in "/*").
    (concat
     " "
     (buffer-substring-no-properties
      (1+ comment-start-pos)
      (save-excursion
        (goto-char (1+ comment-start-pos))
        (skip-syntax-forward "._")
        (point)))
     " "))))

(defun fancy-fill-paragraph--block-comment-continuation-prefix
    (beg body opener-col comment-start-pos)
  "Return the full continuation prefix for a block comment.
BEG is the region start, BODY is the body bounds or nil, OPENER-COL is
the opener column, and COMMENT-START-POS is the comment opener position."
  (declare (important-return-value t))
  (concat
   (fancy-fill-paragraph--extract-body-indent
    (cond
     (body
      (car body))
     (t
      beg))
    opener-col)
   (fancy-fill-paragraph--block-comment-continuation-suffix comment-start-pos)))

(defun fancy-fill-paragraph--block-comment-body-prefix-matches-p (body-beg body-end cont-prefix)
  "Return non-nil when each line in BODY-BEG..BODY-END matches CONT-PREFIX.
Blank prefix lines are accepted when they contain only CONT-PREFIX
without its trailing space."
  (declare (important-return-value t))
  (save-excursion
    (goto-char body-beg)
    (let ((all-match t))
      (while (and all-match (<= (point) body-end))
        (let ((line (buffer-substring-no-properties (point) (pos-eol))))
          (unless (string-prefix-p cont-prefix (concat line " "))
            (setq all-match nil)))
        (forward-line 1))
      all-match)))

(defun fancy-fill-paragraph--body-paragraph-bounds (body-beg body-end prefix-col sel-beg sel-end)
  "Return paragraph bounds in BODY-BEG..BODY-END overlapping SEL-BEG..SEL-END.
Paragraphs are separated by lines blank after PREFIX-COL columns.
SEL-BEG and SEL-END are clamped to the body range so callers may pass
a cursor position outside the body (e.g. on a delimiter line).
Returns a list of (PARA-BEG . PARA-END) in reverse order so callers
can fill later paragraphs first without invalidating earlier positions."
  (declare (important-return-value t))
  (save-excursion
    ;; Clamp selection to body so positions on delimiters still match.
    (setq sel-beg (min (max sel-beg body-beg) body-end))
    (setq sel-end (max (min sel-end body-end) sel-beg))
    (goto-char body-beg)
    (let ((result nil)
          (para-beg nil)
          (para-end nil)
          (done nil))
      (while (and (not done) (<= (point) body-end))
        (let ((separator-p
               (save-excursion
                 (move-to-column prefix-col)
                 (looking-at-p "[ \t]*$"))))
          (cond
           (separator-p
            (when para-beg
              ;; End of previous line (pos-eol with 0 = line above).
              (setq para-end (pos-eol 0))))
           (t
            (unless para-beg
              (setq para-beg (point))))))
        (cond
         ((>= (pos-eol) body-end)
          ;; Last line: close any open paragraph and exit.
          (when (and para-beg (not para-end))
            (setq para-end (min (pos-eol) body-end)))
          (setq done t))
         (t
          (forward-line 1)))
        ;; Flush completed paragraph if it overlaps the selection.
        (when (and para-end (<= para-beg sel-end) (>= para-end sel-beg))
          (push (cons para-beg para-end) result))
        (when para-end
          (setq para-beg nil)
          (setq para-end nil)))
      result)))

(defun fancy-fill-paragraph--fill-prefixed-paragraph
    (lines replace-beg replace-end cont-prefix &optional first-prefix)
  "Fill LINES with CONT-PREFIX and replace REPLACE-BEG..REPLACE-END.
FIRST-PREFIX, when non-nil, is used on the first output line instead
of CONT-PREFIX (e.g. for a comment opener like \"/* \").
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (string-width cont-prefix))
         (result
          (fancy-fill-paragraph--fill-and-reprefix
           lines (max 1 (- fill-column prefix-col)) cont-prefix
           first-prefix)))
    (when result
      (fancy-fill-paragraph--replace-region replace-beg replace-end result))))

(defun fancy-fill-paragraph--fill-syntax-region (beg end inner-body multiline-fn inline-fn)
  "Dispatch filling of a syntax region based on its structure.
BEG and END are the full region bounds.  INNER-BODY is the raw body
bounds from `--comment-or-string-body-bounds', or nil.
When INNER-BODY is non-nil, call MULTILINE-FN with no arguments.
When the region is empty (delimiter-only), return nil.
Otherwise call INLINE-FN with no arguments."
  (declare (important-return-value t))
  (cond
   (inner-body
    (funcall multiline-fn))
   ((fancy-fill-paragraph--empty-syntax-region-p beg end)
    nil)
   (t
    (funcall inline-fn))))

(defun fancy-fill-paragraph--fill-body-paragraphs
    (body-beg body-end prefix-col sel-beg sel-end para-fill-fn)
  "Fill paragraphs in BODY-BEG..BODY-END, calling PARA-FILL-FN for each.
PREFIX-COL is the prefix width for paragraph-boundary detection.
SEL-BEG and SEL-END restrict filling to overlapping paragraphs.
PARA-FILL-FN is called as (PARA-FILL-FN PARA-BEG PARA-END) and should
return non-nil when the buffer was modified.
Returns non-nil when any paragraph changed."
  (declare (important-return-value t))
  (let ((changed nil))
    (dolist (para
             (fancy-fill-paragraph--body-paragraph-bounds
              body-beg body-end prefix-col sel-beg sel-end))
      (when (funcall para-fill-fn (car para) (cdr para))
        (setq changed t)))
    changed))

(defun fancy-fill-paragraph--fill-prefixed-region
    (beg end cont-prefix &optional first-prefix first-line-text replace-beg)
  "Strip lines from BEG..END, fill with CONT-PREFIX, and replace the region.
Lines are stripped of CONT-PREFIX width columns.  When FIRST-LINE-TEXT
is non-nil it is prepended as the first line (e.g. opener-line body text).
FIRST-PREFIX, when non-nil, is used on the first output line.
REPLACE-BEG, when non-nil, overrides BEG as the replacement start
\(e.g. to include the opener line in the replaced span).
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (string-width cont-prefix))
         (lines
          (fancy-fill-paragraph--strip-region-lines beg (min (1+ end) (point-max)) prefix-col))
         (lines
          (cond
           (first-line-text
            (cons first-line-text lines))
           (t
            lines))))
    (fancy-fill-paragraph--fill-prefixed-paragraph lines (or replace-beg beg) end cont-prefix
                                                   first-prefix)))

(defun fancy-fill-paragraph--string-continuation-prefix (inner-body)
  "Return the continuation prefix for a multi-line string with opener text.
INNER-BODY is the raw body bounds (first line after opener to last line
before closer).  Returns the indentation of the first body line as a
blank-space string."
  (declare (important-return-value t))
  (save-excursion
    (goto-char (car inner-body))
    (skip-chars-forward " \t" (pos-eol))
    (buffer-substring-no-properties (pos-bol) (point))))

(defun fancy-fill-paragraph--fill-multiline-syntax-body
    (beg inner-body cont-prefix opener-prefix opener-text para-fill-fn sel-beg sel-end)
  "Fill paragraphs in a multi-line syntax region, merging opener text.
BEG is `pos-bol' of the opener line.  INNER-BODY is (BODY-BEG . BODY-END)
for the lines between opener and closer.  CONT-PREFIX is the continuation
prefix for body lines.  OPENER-PREFIX is the first-line prefix when the
opener carries text (e.g. \"/* \"), or nil.  OPENER-TEXT is the body text
on the opener line (e.g. \"Only flush\"), or nil.
PARA-FILL-FN is called as (PARA-FILL-FN PARA-BEG PARA-END) for each
non-opener paragraph.  SEL-BEG and SEL-END restrict filling to
overlapping paragraphs (callers should default to point when not
region-restricted).
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (string-width cont-prefix))
         ;; Widen body to include the opener line when it carries text,
         ;; so paragraph detection sees it as part of the first paragraph.
         (scan-body
          (cond
           (opener-text
            (cons beg (cdr inner-body)))
           (t
            inner-body))))
    (fancy-fill-paragraph--fill-body-paragraphs
     (car scan-body) (cdr scan-body) prefix-col sel-beg sel-end
     (lambda (para-beg para-end)
       (cond
        ;; First paragraph includes the opener line: merge opener
        ;; text with body lines using the opener prefix for line 1.
        ((and opener-text (= para-beg beg))
         (fancy-fill-paragraph--fill-prefixed-region (car inner-body) para-end cont-prefix
                                                     opener-prefix
                                                     opener-text
                                                     beg))
        (t
         (funcall para-fill-fn para-beg para-end)))))))

(defun fancy-fill-paragraph--fill-block-comment-body (body-beg body-end cont-prefix)
  "Fill a separate-line block comment body from BODY-BEG to BODY-END."
  (cond
   ((fancy-fill-paragraph--block-comment-body-prefix-matches-p body-beg body-end cont-prefix)
    (fancy-fill-paragraph--fill-prefixed-region body-beg body-end cont-prefix))
   (t
    ;; Prefix doesn't match body lines (e.g. XML comments),
    ;; fall back to standard fill.
    (let ((fill-prefix cont-prefix))
      (fancy-fill-paragraph--fill-region body-beg body-end)))))

(defun fancy-fill-paragraph--opener-prefix-and-text ()
  "Return a plist with :prefix and :text for the opener line at point.
Point should be at the position where body content would begin.
:prefix is the buffer text from `pos-bol' to point.
:text is the remaining text on the line, or nil when point is at eol."
  (declare (important-return-value t))
  (list
   :prefix (buffer-substring-no-properties (pos-bol) (point))
   :text
   (cond
    ((eolp)
     nil)
    (t
     (buffer-substring-no-properties (point) (pos-eol))))))

(defun fancy-fill-paragraph--block-comment-opener-info (comment-start-pos)
  "Return a plist describing the block comment opener at COMMENT-START-POS.
The plist contains:
- :content-col  Column where content begins (after delimiter and blank-space).
- :prefix       The opener prefix string (from `pos-bol' to :content-col).
- :text         Text after the delimiter on the opener line, or nil."
  (declare (important-return-value t))
  (save-excursion
    (goto-char comment-start-pos)
    (skip-chars-forward "^ \t\n" (pos-eol))
    (skip-chars-forward " \t" (pos-eol))
    (nconc (list :content-col (current-column)) (fancy-fill-paragraph--opener-prefix-and-text))))

(defun fancy-fill-paragraph--block-comment-inline-closer (beg end prefix-col)
  "Return the verbatim closer line for an inline block comment, or nil."
  (declare (important-return-value t))
  (let ((last-bol (fancy-fill-paragraph--last-bol end)))
    (save-excursion
      (goto-char last-bol)
      (and (> last-bol beg)
           (<= (- (pos-eol) last-bol) prefix-col)
           (buffer-substring-no-properties last-bol (pos-eol))))))

(defun fancy-fill-paragraph--block-comment-inline-lines
    (beg end opener-col prefix-col last-line-verbatim)
  "Return inline block comment content lines stripped for filling.
BEG and END are the full region bounds.  OPENER-COL is used for the
first line, PREFIX-COL for continuation lines, and LAST-LINE-VERBATIM,
when non-nil, is excluded from the returned content."
  (declare (important-return-value t))
  (let* ((last-bol (fancy-fill-paragraph--last-bol end))
         (content-end-bol
          (cond
           (last-line-verbatim
            last-bol)
           (t
            (1+ last-bol)))))
    (save-excursion
      (goto-char beg)
      (move-to-column opener-col)
      (let ((result (list (buffer-substring-no-properties (point) (pos-eol)))))
        (forward-line 1)
        (while (< (point) content-end-bol)
          (move-to-column prefix-col)
          (push (buffer-substring-no-properties (point) (pos-eol)) result)
          (forward-line 1))
        (nreverse result)))))

(defun fancy-fill-paragraph--fill-block-comment-inline
    (beg end comment-start-pos cont-prefix opener-col)
  "Fill an inline block comment in BEG..END using CONT-PREFIX.
OPENER-COL is the column where content begins after the opener delimiter."
  (unless (eq
           comment-start-pos
           (fancy-fill-paragraph--ppss-start (save-excursion (syntax-ppss (1- end)))))
    (error "Expected comment at %d" comment-start-pos))
  (let* ((prefix-col
          (max (fancy-fill-paragraph--prefix-column-width beg cont-prefix) (length cont-prefix)))
         (opener-text
          (save-excursion
            (goto-char beg)
            (move-to-column opener-col)
            (buffer-substring-no-properties beg (point))))
         (last-line-verbatim
          (fancy-fill-paragraph--block-comment-inline-closer beg end prefix-col))
         (filled-result
          (fancy-fill-paragraph--fill-and-reprefix
           (fancy-fill-paragraph--block-comment-inline-lines
            beg end opener-col prefix-col last-line-verbatim)
           (max 1 (- fill-column prefix-col)) cont-prefix
           opener-text))
         (result
          (cond
           ((null filled-result)
            nil)
           (last-line-verbatim
            (concat filled-result "\n" last-line-verbatim))
           (t
            filled-result))))
    (when result
      (fancy-fill-paragraph--replace-region beg end result))))

(defun fancy-fill-paragraph--fill-block-comment-region
    (beg end comment-start-pos &optional sel-beg sel-end)
  "Fill a block comment in the region BEG..END.
COMMENT-START-POS is the buffer position of the comment opener.
When SEL-BEG and SEL-END are non-nil, fill only body paragraphs
overlapping that selection.  Otherwise fill the paragraph at point.
When the opener and closer occupy their own lines, fills only the body
lines between them.  When they share lines with body text (inline),
temporarily transforms the opener so all lines share the continuation
prefix, fills, then restores the opener."
  (let ((pos (point)))
    (save-excursion
      (comment-normalize-vars)
      (let* ((opener-col
              (progn
                (goto-char comment-start-pos)
                (current-column)))
             (opener-info (fancy-fill-paragraph--block-comment-opener-info comment-start-pos))
             (inner-body (fancy-fill-paragraph--comment-or-string-body-bounds beg end))
             (cont-prefix
              (fancy-fill-paragraph--block-comment-continuation-prefix
               beg inner-body opener-col comment-start-pos)))
        (fancy-fill-paragraph--fill-syntax-region
         beg end inner-body
         (lambda ()
           (fancy-fill-paragraph--fill-multiline-syntax-body
            beg
            inner-body
            cont-prefix
            (plist-get opener-info :prefix)
            (plist-get opener-info :text)
            (lambda
              (para-beg para-end)
              (fancy-fill-paragraph--fill-block-comment-body para-beg para-end cont-prefix))
            (or sel-beg pos)
            (or sel-end pos)))
         (lambda ()
           (fancy-fill-paragraph--fill-block-comment-inline
            beg end comment-start-pos cont-prefix (plist-get opener-info :content-col))))))))

(defun fancy-fill-paragraph--string-opener-info (opener-pos)
  "Return a plist describing the string opener at OPENER-POS.
OPENER-POS is the syntax-derived position where the string sexp begins
\(e.g. the `r' in `r\"\"\"' or the first quote in `\"\"\"').
Normal callers provide a valid string opener; the fallback path
\(no quote found on the line) is a defensive guard only.
The plist contains:
- :prefix  Exact buffer text from `pos-bol' to the start of opener-line
           body content (indentation + any prefix chars + delimiter +
           trailing space).
- :text    Opener-line body content after the prefix, or nil when the
           opener line contains only the delimiter."
  (declare (important-return-value t))
  (save-excursion
    (goto-char opener-pos)
    ;; Skip to the first quote character, passing any string prefix
    ;; letters (e.g. r, f, b, u, rb, fr in r""").
    (skip-chars-forward "^'\"\n" (pos-eol))
    (cond
     ((or (eolp) (eq (char-after) ?\n))
      ;; Defensive fallback: no quote found on this line.
      (list :prefix (buffer-substring-no-properties (pos-bol) (pos-eol)) :text nil))
     (t
      ;; Skip the quote characters (e.g. \"\"\", ''').
      (skip-chars-forward (string (char-after)) (pos-eol))
      (skip-chars-forward " \t" (pos-eol))
      (fancy-fill-paragraph--opener-prefix-and-text)))))

(defun fancy-fill-paragraph--fill-string-region (beg end opener-pos &optional sel-beg sel-end)
  "Fill a string in the region BEG..END.
OPENER-POS is the buffer position where the string sexp begins.
When SEL-BEG and SEL-END are non-nil, fill only body paragraphs
overlapping that selection.  Otherwise fill the paragraph at point.
When the delimiters occupy their own lines, fills only the body text
between them.  When the delimiters share lines with body text (inline
delimiters), fills the entire region, treating the delimiters as part
of the text."
  (let* ((inner-body (fancy-fill-paragraph--comment-or-string-body-bounds beg end))
         (opener-info
          (when inner-body
            (fancy-fill-paragraph--string-opener-info opener-pos)))
         (cont-prefix
          (when (and inner-body (plist-get opener-info :text))
            (fancy-fill-paragraph--string-continuation-prefix inner-body))))
    (fancy-fill-paragraph--fill-syntax-region
     beg end inner-body
     (lambda ()
       (fancy-fill-paragraph--fill-multiline-syntax-body
        beg
        inner-body
        (or cont-prefix "")
        (plist-get opener-info :prefix)
        (plist-get opener-info :text)
        #'fancy-fill-paragraph--fill-region
        (or sel-beg (point))
        (or sel-end (point))))
     (lambda () (fancy-fill-paragraph--fill-region beg end)))))

(defun fancy-fill-paragraph--replace-region (beg end result)
  "Replace the region BEG..END with RESULT when it differs.
Contracts the region from both ends to minimize the replacement span.
Return non-nil when the buffer was modified, nil when unchanged."
  (let ((len (length result))
        (rpos 0))
    ;; Contract from the beginning.
    (while (and (< beg end) (< rpos len) (eq (char-after beg) (aref result rpos)))
      (setq beg (1+ beg))
      (setq rpos (1+ rpos)))
    ;; Fully matched: no change needed.
    (cond
     ((and (= beg end) (= rpos len))
      nil)
     (t
      ;; Contract from the end.
      (let ((rend len))
        (while (and (< beg end) (< rpos rend) (eq (char-before end) (aref result (1- rend))))
          (setq end (1- end))
          (setq rend (1- rend)))
        (save-excursion
          ;; Use `replace-region-contents' to minimize undo differences
          ;; compared with delete-region + insert.
          ;; NOTE: when emacs 30.x is dropped, pass the string directly.
          (replace-region-contents beg end (lambda () (substring result rpos rend)))))
      t))))

(defun fancy-fill-paragraph--detect-fill-prefix (beg end)
  "Return (PREFIX PREFIX-COLUMN-WIDTH) detected for BEG..END."
  (declare (important-return-value t))
  (let ((prefix (or fill-prefix (fill-context-prefix beg end) "")))
    (list prefix (fancy-fill-paragraph--prefix-column-width beg prefix))))

(defun fancy-fill-paragraph--dot-point-safe-prefix (first-line prefix prefix-column-width dp-list)
  "Return a dot-point-safe (PREFIX PREFIX-COLUMN-WIDTH).
When PREFIX consumes a dot-point marker in FIRST-LINE, fall back to
plain indentation so continuation lines align correctly."
  (declare (important-return-value t))
  (cond
   ((and dp-list (not (string-empty-p prefix)))
    (let ((m (fancy-fill-paragraph--line-dot-point-match first-line dp-list)))
      (cond
       ((and m (> (length prefix) (car m)))
        (list (make-string (car m) ?\s) (car m)))
       (t
        (list prefix prefix-column-width)))))
   (t
    (list prefix prefix-column-width))))

(defun fancy-fill-paragraph--validated-fill-prefix (beg end prefix prefix-column-width)
  "Return a validated (PREFIX PREFIX-COLUMN-WIDTH) for BEG..END.
If PREFIX does not match every line, fall back to the minimum line indent."
  (declare (important-return-value t))
  (cond
   ((string-empty-p prefix)
    (list prefix prefix-column-width))
   (t
    (save-excursion
      (goto-char beg)
      (let ((all-match t)
            (min-indent most-positive-fixnum))
        ;; Early exit: once a mismatch is found and min-indent reaches 0,
        ;; the result is "" regardless of remaining lines.
        (while (and (< (point) end) (or all-match (> min-indent 0)))
          (let ((line (buffer-substring-no-properties (point) (pos-eol))))
            (when (and all-match (not (string-prefix-p prefix line)))
              (setq all-match nil))
            (unless all-match
              (setq min-indent (min min-indent (fancy-fill-paragraph--leading-indent line)))))
          (forward-line 1))
        (cond
         (all-match
          (list prefix prefix-column-width))
         (t
          (list (make-string min-indent ?\s) min-indent))))))))

(defun fancy-fill-paragraph--analyze-fill-prefix (beg end dp-list)
  "Return the effective (PREFIX PREFIX-COLUMN-WIDTH) for BEG..END."
  (declare (important-return-value t))
  (let ((first-line
         (save-excursion
           (goto-char beg)
           (buffer-substring-no-properties (point) (pos-eol)))))
    (pcase-let* ((`(,prefix ,prefix-column-width)
                  (fancy-fill-paragraph--detect-fill-prefix beg end))
                 (`(,prefix ,prefix-column-width)
                  (fancy-fill-paragraph--dot-point-safe-prefix
                   first-line prefix prefix-column-width dp-list)))
      (fancy-fill-paragraph--validated-fill-prefix beg end prefix prefix-column-width))))

(defun fancy-fill-paragraph--strip-region-lines (beg end prefix-column-width)
  "Return region lines from BEG..END with PREFIX-COLUMN-WIDTH stripped."
  (declare (important-return-value t))
  (save-excursion
    (let ((result nil))
      (goto-char beg)
      (while (< (point) end)
        (move-to-column prefix-column-width)
        (push (buffer-substring-no-properties (point) (pos-eol)) result)
        (forward-line 1))
      (nreverse result))))


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
  (let* ((items nil)
         (boundary-info nil)
         (pos 0)
         (len (length text))
         (delimiters (fancy-fill-paragraph--active-delimiters))
         (d-count (length delimiters)))

    (let ((d-cached (make-vector d-count nil)))

      ;; Scan for split points with cached match positions.
      (while (< pos len)
        (let ((best-split nil)
              (best-index nil)
              (d 0))
          (while (< d d-count)
            (let* ((delimiter (aref delimiters d))
                   (cached-pos (aref d-cached d)))
              ;; Refresh stale or uninitialized cache ('no-match avoids re-searching).
              (when (or (null cached-pos) (and (integerp cached-pos) (<= cached-pos pos)))
                (setq cached-pos
                      (or (funcall (plist-get delimiter :match-point) text pos) 'no-match))
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
                 (delimiter (aref delimiters d-index))
                 (split-pos (cdr boundary))
                 (sep
                  (fancy-fill-paragraph--maybe-double-space
                   (funcall (plist-get delimiter :join-string) split-pos) text split-pos)))
            (aset break-weights i (plist-get delimiter :weight))
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


(defun fancy-fill-paragraph--compile-dot-point-patterns ()
  "Build compiled matchers for dot-point prefix handling.
Each result entry is a plist with line-matching and item-prefix checks
so line detection and false-dot-point prevention share one definition."
  (declare (important-return-value t))
  (mapcar
   (lambda (dp)
     (cond
      ((stringp dp)
       (list :line-type 'literal :line-value dp :item-type 'literal :item-value dp))
      (t
       (list
        :line-type 'regexp
        :line-value (concat "\\`\\([ \t]*\\)\\(" (cdr dp) "\\)")
        :item-type 'regexp
        :item-value (concat "\\`" (cdr dp))))))
   fancy-fill-paragraph-dot-point-prefix))

(defun fancy-fill-paragraph--line-dot-point-match (line dot-points)
  "Check if LINE matches a dot-point prefix using compiled DOT-POINTS.
DOT-POINTS is from `fancy-fill-paragraph--compile-dot-point-patterns'.
Returns (INDENT-WIDTH . DP-PREFIX-STRING) or nil.
INDENT-WIDTH is the number of leading blank-space characters."
  (declare (important-return-value t))
  (let ((result nil)
        (entries-remaining dot-points))
    (while (and entries-remaining (null result))
      (let ((entry (car entries-remaining)))
        (cond
         ((eq (plist-get entry :line-type) 'literal)
          ;; Literal: check leading blank-space then prefix.
          (let* ((prefix (plist-get entry :line-value))
                 (indent (fancy-fill-paragraph--leading-indent line)))
            (when (eq t (compare-strings prefix 0 nil line indent (+ indent (length prefix))))
              (setq result (cons indent prefix)))))
         (t
          ;; Regexp: match and extract prefix from group 2.
          (when (string-match (plist-get entry :line-value) line)
            (setq result (cons (- (match-end 1) (match-beginning 1)) (match-string 2 line)))))))
      (setq entries-remaining (cdr entries-remaining)))
    result))

(defun fancy-fill-paragraph--dot-point-item-prefix-p (item dot-points)
  "Return non-nil when ITEM would look like a dot-point start.
DOT-POINTS is from `fancy-fill-paragraph--compile-dot-point-patterns'."
  (declare (important-return-value t))
  (let ((item-with-space (concat item " "))
        (result nil)
        (entries-remaining dot-points))
    (while (and entries-remaining (null result))
      (let ((entry (car entries-remaining)))
        (setq result
              (cond
               ((eq (plist-get entry :item-type) 'literal)
                (string-prefix-p (plist-get entry :item-value) item-with-space))
               (t
                (string-match-p (plist-get entry :item-value) item-with-space)))))
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
        (let ((dot-points (fancy-fill-paragraph--compile-dot-point-patterns)))
          (when dot-points
            (let ((items-tail (cdr items))
                  (i 0))
              (while items-tail
                (when (fancy-fill-paragraph--dot-point-item-prefix-p (car items-tail) dot-points)
                  (aset break-weights i fancy-fill-paragraph--prevent-dot-point-penalty))
                (setq items-tail (cdr items-tail))
                (setq i (1+ i))))))
        (fancy-fill-paragraph--solve
         items
         local-fill-column
         break-weights
         (plist-get split-result :seps)
         (plist-get split-result :sep-lens)
         fancy-fill-paragraph-blank-space-weight)))
     (t
      nil))))

(defun fancy-fill-paragraph--group-lines-by-indent (lines)
  "Return LINES grouped by consecutive indentation.
Each result entry is (INDENT . GROUP-LINES)."
  (declare (important-return-value t))
  (let ((groups nil)
        (current-indent nil)
        (current-group nil))
    (dolist (line lines)
      (let ((indent (fancy-fill-paragraph--leading-indent line)))
        (cond
         ((null current-group)
          (setq current-indent indent)
          (setq current-group (list line)))
         ((/= indent current-indent)
          (push (cons current-indent (nreverse current-group)) groups)
          (setq current-indent indent)
          (setq current-group (list line)))
         (t
          (push line current-group)))))
    (when current-group
      (push (cons current-indent (nreverse current-group)) groups))
    (nreverse groups)))

(defun fancy-fill-paragraph--finalize-dot-point-group (group)
  "Return GROUP with its collected lines restored to forward order."
  (declare (important-return-value t))
  (cons (car group) (nreverse (cdr group))))

(defun fancy-fill-paragraph--partition-dot-point-lines (lines dp-list)
  "Return dot-point sections for LINES using DP-LIST, or nil.
The result is a plist with keys :min-indent, :preamble, :groups, and
:trailing.  Each group is (DP-PREFIX . RAW-LINES)."
  (declare (important-return-value t))
  (let ((annotated nil)
        (has-dp nil)
        (min-indent most-positive-fixnum))
    (dolist (line lines)
      (let ((dp-match (fancy-fill-paragraph--line-dot-point-match line dp-list)))
        (push (cons line dp-match) annotated)
        (when dp-match
          (setq has-dp t)
          (when (< (car dp-match) min-indent)
            (setq min-indent (car dp-match))))))
    (when has-dp
      (setq annotated (nreverse annotated))
      (let ((groups nil)
            (current-group nil)
            (preamble nil)
            (found-first nil)
            (trailing nil)
            (rest annotated))
        (while (and rest (null trailing))
          (let* ((entry (car rest))
                 (line (car entry))
                 (dp-match (cdr entry)))
            (cond
             ((and dp-match (= (car dp-match) min-indent))
              (when current-group
                (push (fancy-fill-paragraph--finalize-dot-point-group current-group) groups))
              (setq current-group (cons (cdr dp-match) (list line)))
              (setq found-first t))
             ((not found-first)
              (push line preamble))
             ((> (fancy-fill-paragraph--leading-indent line) min-indent)
              (setcdr current-group (cons line (cdr current-group))))
             (t
              (when current-group
                (push (fancy-fill-paragraph--finalize-dot-point-group current-group) groups))
              (setq current-group nil)
              (setq trailing (mapcar #'car rest)))))
          (unless trailing
            (setq rest (cdr rest))))
        (when current-group
          (push (fancy-fill-paragraph--finalize-dot-point-group current-group) groups))
        (list
         :min-indent min-indent
         :preamble (nreverse preamble)
         :groups (nreverse groups)
         :trailing trailing)))))

(defun fancy-fill-paragraph--fill-dot-point-group (group min-indent local-fill-column dp-list)
  "Return filled output lines for one dot-point GROUP.
GROUP is (DP-PREFIX . RAW-LINES), MIN-INDENT is the shared list indent,
LOCAL-FILL-COLUMN is the available width, and DP-LIST is passed through
for recursive dot-point detection."
  (declare (important-return-value t))
  (let* ((dp-prefix (car group))
         (group-lines (cdr group))
         (dp-prefix-len (length dp-prefix))
         (indent-plus-dp (+ min-indent dp-prefix-len))
         (indent-str (make-string min-indent ?\s))
         (cont-indent-str (make-string indent-plus-dp ?\s))
         (sub-fill-column (max 1 (- local-fill-column indent-plus-dp)))
         (body-lines
          (cons
           (substring (car group-lines) indent-plus-dp)
           (mapcar
            (lambda (line)
              (fancy-fill-paragraph--strip-indent line indent-plus-dp))
            (cdr group-lines))))
         (filled (fancy-fill-paragraph--fill-lines body-lines sub-fill-column dp-list))
         (result nil)
         (is-first t))
    (dolist (line filled)
      (push (cond
             (is-first
              (setq is-first nil)
              (concat indent-str dp-prefix line))
             (t
              (concat cont-indent-str line)))
            result))
    (nreverse result)))

(defun fancy-fill-paragraph--fill-lines-indent-split (lines local-fill-column dp-list)
  "Fill LINES, splitting into sub-paragraphs at indentation changes.
LOCAL-FILL-COLUMN is the target line width.
Groups consecutive lines with the same leading blank-space and fills
each group independently via `fancy-fill-paragraph--fill-lines'.
Falls through to plain fill when all lines share the same indentation.
DP-LIST is passed through for dot-point detection in recursive calls."
  (declare (important-return-value t))
  (let ((groups (fancy-fill-paragraph--group-lines-by-indent lines)))
    (cond
     ((null (cdr groups))
      (fancy-fill-paragraph--fill-lines-plain lines local-fill-column))
     (t
      (let ((result nil))
        (dolist (group groups)
          (let* ((indent (car group))
                 (group-lines (cdr group))
                 (indent-str (make-string indent ?\s))
                 (sub-fill-column (max 1 (- local-fill-column indent)))
                 (filled
                  (fancy-fill-paragraph--fill-lines
                   (mapcar
                    (lambda (line) (fancy-fill-paragraph--strip-indent line indent)) group-lines)
                   sub-fill-column dp-list)))
            (setq result (nconc result (mapcar (lambda (line) (concat indent-str line)) filled)))))
        result)))))

(defun fancy-fill-paragraph--fill-lines (lines local-fill-column dp-list)
  "Fill LINES within LOCAL-FILL-COLUMN, handling dot-points recursively.
LINES is a list of strings.  Returns a list of result line strings.
DP-LIST is a pre-built pattern list from
`fancy-fill-paragraph--compile-dot-point-patterns'.
When DP-LIST is non-nil, detects dot-point prefixes and fills each
item as a separate sub-paragraph.  Falls through to plain fill when
no dot-points are found."
  (declare (important-return-value t))
  (cond
   ;; Dot-point mode disabled (dp-list is nil), skip dot-point scan.
   ((null dp-list)
    (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column nil))
   (t
    (let ((sections (fancy-fill-paragraph--partition-dot-point-lines lines dp-list)))
      (cond
       ((null sections)
        (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column dp-list))
       (t
        (let ((result nil)
              (min-indent (plist-get sections :min-indent))
              (preamble (plist-get sections :preamble))
              (groups (plist-get sections :groups))
              (trailing (plist-get sections :trailing)))
          (when preamble
            (setq result
                  (nconc
                   result (fancy-fill-paragraph--fill-lines preamble local-fill-column dp-list))))
          (dolist (group groups)
            (setq result
                  (nconc
                   result
                   (fancy-fill-paragraph--fill-dot-point-group
                    group min-indent local-fill-column dp-list))))
          (when trailing
            (setq result
                  (nconc
                   result (fancy-fill-paragraph--fill-lines trailing local-fill-column dp-list))))
          result)))))))

(defun fancy-fill-paragraph--fill-region (beg end)
  "Fill the paragraph in the region from BEG to END.
Detects the fill prefix, strips it, normalizes blank-space,
splits at delimiter boundaries, solves for optimal line breaks,
and re-adds the prefix to each output line."
  (let* ((dp-list (fancy-fill-paragraph--compile-dot-point-patterns))
         (prefix-info (fancy-fill-paragraph--analyze-fill-prefix beg end dp-list))
         (prefix (car prefix-info))
         (prefix-column-width (cadr prefix-info))
         (lines (fancy-fill-paragraph--strip-region-lines beg end prefix-column-width))
         ;; Fill using dot-point aware function.
         ;; Always pass dp-list so dot-points are automatically detected;
         ;; `--fill-lines' falls through to plain fill when none are found.
         (filled
          (fancy-fill-paragraph--fill-lines
           lines
           (max 1
                (- fill-column fancy-fill-paragraph-fill-column-margin prefix-column-width))
           dp-list)))
    (when filled
      (fancy-fill-paragraph--replace-region
       beg end (mapconcat (lambda (line) (concat prefix line)) filled "\n")))))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun fancy-fill-paragraph--by-region-impl ()
  "Fill paragraphs in the active region.
When the region is entirely inside a single comment or string,
uses syntax-aware filling.  Returns non-nil when the buffer was modified."
  (let ((beg (region-beginning))
        (end (region-end))
        ;; Snapshot buffer size to compute adjusted end position after filling.
        (buf-size (buffer-size))
        (point-at-beg (< (point) (mark)))
        (changed nil))
    (save-excursion
      ;; When the selection is entirely inside a single comment or string,
      ;; use syntax-aware filling so prefixes (e.g. ` * ') are handled.
      (syntax-propertize end)
      (let* ((beg-syn (fancy-fill-paragraph--comment-or-string-start-at beg))
             (end-syn (fancy-fill-paragraph--comment-or-string-start-at end))
             (syn-start
              (and fancy-fill-paragraph-syntax-bounds
                   beg-syn
                   end-syn
                   (= beg-syn end-syn)
                   beg-syn)))
        (cond
         (syn-start
          (goto-char syn-start)
          (let ((region-beg (pos-bol)))
            (cond
             ;; Block comment: advance past closer via forward-comment.
             ;; Check `--syntax-comment-end-p' to exclude line comments
             ;; (whose closer is a newline, not a block-comment ender).
             ((and (forward-comment 1) (fancy-fill-paragraph--syntax-comment-end-p))
              (setq changed
                    (fancy-fill-paragraph--fill-block-comment-region region-beg (pos-eol) syn-start
                                                                     beg
                                                                     end)))
             ;; Line comment: find the full block and fill the selection.
             ((progn
                (goto-char syn-start)
                (and (forward-comment 1) (not (fancy-fill-paragraph--syntax-comment-end-p))))
              (let ((bounds
                     (fancy-fill-paragraph--line-comment-bounds
                      syn-start (point-min) (point-max))))
                (setq changed
                      (fancy-fill-paragraph--fill-line-comment-region
                       (car bounds) (cdr bounds) syn-start
                       beg end))))
             ;; String: advance past closer via forward-sexp.
             ((progn
                (goto-char syn-start)
                (fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after (point))))
              (forward-sexp 1)
              (setq changed
                    (fancy-fill-paragraph--fill-string-region region-beg (pos-eol) syn-start
                                                              beg
                                                              end))))))
         ;; Selection spans multiple line comments at the same indent:
         ;; beg-syn and end-syn differ but both are line comments.
         ((and fancy-fill-paragraph-syntax-bounds
               beg-syn end-syn
               (save-excursion
                 (goto-char beg-syn)
                 (and (forward-comment 1) (not (fancy-fill-paragraph--syntax-comment-end-p))))
               (= (fancy-fill-paragraph--line-comment-indent-col beg-syn)
                  (fancy-fill-paragraph--line-comment-indent-col end-syn)))
          (let ((bounds
                 (fancy-fill-paragraph--line-comment-bounds beg-syn (point-min) (point-max))))
            (setq changed
                  (fancy-fill-paragraph--fill-line-comment-region (car bounds) (cdr bounds) beg-syn
                                                                  beg
                                                                  end))))
         (t
          (fancy-fill-paragraph--fill-paragraph-bounds
           (fancy-fill-paragraph--collect-paragraph-bounds beg end t))
          (setq changed (/= buf-size (buffer-size)))))))
    ;; Restore mark/point in original direction, deactivate mark.
    (let ((end-adjusted (+ end (- (buffer-size) buf-size))))
      (cond
       (point-at-beg
        (set-mark end-adjusted)
        (goto-char beg))
       (t
        (set-mark beg)
        (goto-char end-adjusted))))
    (setq deactivate-mark t)
    changed))

(defun fancy-fill-paragraph--by-point-impl ()
  "Fill the paragraph at point.
Uses syntax-aware filling when inside a comment or string.
Returns non-nil when the buffer was modified."
  (let* ((bounds (fancy-fill-paragraph--paragraph-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (sub-regions
          (when fancy-fill-paragraph-syntax-bounds
            (fancy-fill-paragraph--comment-or-string-regions beg end (point)))))
    (cond
     (sub-regions
      ;; Fill each region independently, in reverse order so
      ;; earlier positions remain valid after later fills.
      ;; Save point as an integer since `replace-region-contents'
      ;; can displace `save-excursion' markers inside fill functions.
      (let ((saved-point (point))
            (changed nil))
        (dolist (region (reverse sub-regions))
          (pcase-let ((`(,rbeg ,rend ,fill-fn . ,extra) region))
            (when (apply fill-fn rbeg rend extra)
              (setq changed t))))
        (goto-char saved-point)
        changed))
     (t
      (fancy-fill-paragraph--fill-region beg end)))))

;;;###autoload
(defun fancy-fill-paragraph ()
  "Fill the current paragraph with context aware formatting.
With an active region, fill each paragraph in the region separately.
Breaks lines preferring sentence boundaries.
Return non-nil when the buffer was modified."
  (interactive "*")
  (let ((changed
         (cond
          ((region-active-p)
           (fancy-fill-paragraph--by-region-impl))
          (t
           (fancy-fill-paragraph--by-point-impl)))))
    (unless changed
      (let ((message-log-max nil))
        (message "Fancy-fill-paragraph: no changes")))
    changed))

(provide 'fancy-fill-paragraph)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-format-quoted: nil
;; End:
;;; fancy-fill-paragraph.el ends here
