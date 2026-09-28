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
When a float, a fraction of the effective fill column, clamped to that column.
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
distinct comments or strings.
Filling a paragraph that holds code beside a comment or string leaves
it alone rather than reflowing it as prose, which would merge the code
into the comment text.  This reports no changes, the same as an
already filled paragraph."
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
Double spacing is applied by `fancy-fill-paragraph--maybe-double-space'.
Argument _POS is ignored."
  (declare (important-return-value t))
  " ")

(defsubst fancy-fill-paragraph--source-space-width (source-space-widths split-pos)
  "Return original space-run width at SPLIT-POS from SOURCE-SPACE-WIDTHS."
  (declare (important-return-value t))
  (cond
   ((and source-space-widths (< split-pos (length source-space-widths)))
    (aref source-space-widths split-pos))
   (t
    0)))

(defsubst fancy-fill-paragraph--sentence-end-boundary-p (text split-pos)
  "Return non-nil when TEXT at SPLIT-POS follows sentence punctuation."
  (declare (important-return-value t))
  (and (> split-pos 0)
       (or (memq (aref text (1- split-pos)) '(?. ?? ?! ?\u2026))
           (and (>= split-pos 2)
                (not (memq (aref text (1- split-pos)) '(?, ?\; ?:)))
                (memq (aref text (- split-pos 2)) '(?. ?? ?! ?\u2026))))))

(defsubst fancy-fill-paragraph--maybe-double-space
    (sep text split-pos &optional source-space-widths)
  "Upgrade SEP to double-space when TEXT at SPLIT-POS follows a sentence end.
When `sentence-end-double-space' is active and the
delimiter at SPLIT-POS follows sentence-ending punctuation (e.g. `.\\'',
`.)'), return double-space.  Exclude continuation punctuation (`,;:')
where the period is typically an abbreviation (e.g. `etc.,').  When
SOURCE-SPACE-WIDTHS is non-nil, a dotted abbreviation followed by an
original single space keeps a single space."
  (declare (important-return-value t))
  (cond
   ((and sentence-end-double-space
         (fancy-fill-paragraph--sentence-end-boundary-p text split-pos)
         (not
          (and (= (fancy-fill-paragraph--source-space-width source-space-widths split-pos) 1)
               (fancy-fill-paragraph--dotted-abbreviation-boundary-p text split-pos))))
    "  ")
   (t
    sep)))

(defsubst fancy-fill-paragraph--dotted-abbreviation-boundary-p (text split-pos)
  "Return non-nil when TEXT before SPLIT-POS ends with a dotted abbreviation."
  (declare (important-return-value t))
  (and (> split-pos 1)
       (eq (aref text (1- split-pos)) ?.)
       (let ((start split-pos))
         (while (and (> start 0) (not (memq (aref text (1- start)) '(?\s ?\t ?\n))))
           (setq start (1- start)))
         (string-match-p
          "\\`\\(?:[[:alpha:]]\\.\\)\\{2,\\}\\'" (substring text start split-pos)))))

(defun fancy-fill-paragraph--normalize-spaces-with-widths (source)
  "Return (TEXT . WIDTHS) for SOURCE with space runs collapsed."
  (declare (important-return-value t))
  (let ((parts nil)
        (widths nil)
        (pos 0)
        (len (length source)))
    (while (< pos len)
      (cond
       ((eq (aref source pos) ?\s)
        (let ((start pos))
          (while (and (< pos len) (eq (aref source pos) ?\s))
            (setq pos (1+ pos)))
          (push " " parts)
          (push (- pos start) widths)))
       (t
        (push (char-to-string (aref source pos)) parts)
        (push 0 widths)
        (setq pos (1+ pos)))))
    (cons (apply #'concat (nreverse parts)) (vconcat (nreverse widths)))))

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
rather than synthesizing spaces from the column width.  Code sharing
the line is not indentation, only its leading blank-space is kept,
padded with spaces to reach OPENER-COL.  A tab straddling that column
is kept whole, so the result can be wider."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (goto-char (pos-bol))
    (skip-chars-forward " \t" (pos-eol))
    ;; Never past the indentation, code sharing the line is not indent.
    (move-to-column (min opener-col (current-column)))
    (concat
     (buffer-substring-no-properties (pos-bol) (point))
     ;; A tab straddling OPENER-COL leaves point past it, nothing to pad.
     (make-string (max 0 (- opener-col (current-column))) ?\s))))

(defsubst fancy-fill-paragraph--prefix-column-width (pos prefix)
  "Return the column width of PREFIX at buffer position POS.
A line ending within PREFIX, such as a bare ` *' separator without the
trailing space of ` * ', is measured as if it held all of PREFIX."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (goto-char (min (+ pos (length prefix)) (pos-eol)))
    ;; Measure what the line holds, tabs included, then add the rest.
    (+ (current-column) (string-width (substring prefix (- (point) pos))))))

(defsubst fancy-fill-paragraph--last-bol (end)
  "Return the beginning-of-line position for buffer position END."
  (declare (important-return-value t))
  (save-excursion
    (goto-char end)
    (pos-bol)))

(defsubst fancy-fill-paragraph--closer-line-bol (beg end)
  "Return the beginning of the closer line of the region BEG..END, or nil.
BEG and END are the full region bounds.  A closer on the opener line is
never on a line of its own, so nil is returned for it."
  (declare (important-return-value t))
  (let ((last-bol (fancy-fill-paragraph--last-bol end)))
    (and (> last-bol beg) last-bol)))

(defsubst fancy-fill-paragraph--range-has-non-blank-p (beg end)
  "Return non-nil when BEG..END holds a character that is not blank-space.
Newlines count as blank-space.  An empty or reversed range holds
nothing."
  (declare (important-return-value t))
  ;; `skip-chars-forward' does not move when END precedes point, so an
  ;; empty or reversed range falls out of the comparison.
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \t\n" end)
    (< (point) end)))

(defsubst fancy-fill-paragraph--line-matches-prefix-p (cont-prefix)
  "Return non-nil when point up to end of line begins with CONT-PREFIX.
Trailing blank-space is trimmed from a line holding nothing else, so
the text is padded with a space before comparing, accepting a line
that carries CONT-PREFIX without its trailing space."
  (declare (important-return-value t))
  (string-prefix-p cont-prefix (concat (buffer-substring-no-properties (point) (pos-eol)) " ")))

(defsubst fancy-fill-paragraph--move-past-line-prefix (prefix-column-width cont-prefix)
  "Move point past the prefix on the line at point.
CONT-PREFIX, when non-nil, is the prefix the line is expected to carry.
A line without it holds body text where the prefix should be, cutting
PREFIX-COLUMN-WIDTH columns from that deletes the text, so only its
leading blank-space is skipped.
Pass it only for a synthesized prefix.  The comparison is literal, so a
prefix read from the buffer rejects lines that reach the same column
with different blank-space, tabs against spaces for instance."
  (cond
   ((or (null cont-prefix) (fancy-fill-paragraph--line-matches-prefix-p cont-prefix))
    (move-to-column prefix-column-width))
   (t
    (skip-chars-forward " \t" (pos-eol)))))

(defsubst fancy-fill-paragraph--string-blank-or-empty-p (str)
  "Return non-nil if STR is empty or contains only blank-space and newlines."
  (declare (important-return-value t))
  (string-match-p "\\`[ \t\n]*\\'" str))

(defsubst fancy-fill-paragraph--empty-syntax-region-p (beg end)
  "Return non-nil if the syntax region BEG..END has no fillable content.
True when the delimiters are on separate lines and the opener line
contains only the delimiter (no text after it), with or without
leading indentation."
  (declare (important-return-value t))
  (and (< beg (fancy-fill-paragraph--last-bol end))
       (save-excursion
         (goto-char beg)
         ;; Leading indentation first: BEG is a line beginning, so an
         ;; indented opener otherwise leaves the non-blank skip below
         ;; consuming nothing and the delimiter misread as content.
         (skip-chars-forward " \t" (pos-eol))
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
BOUNDS is a reverse-ordered list of (BEG . END) pairs.
Return non-nil when any paragraph was modified."
  (let ((changed nil))
    (dolist (para-bounds bounds)
      (when (fancy-fill-paragraph--fill-region (car para-bounds) (cdr para-bounds))
        (setq changed t)))
    changed))


;; ---------------------------------------------------------------------------
;; Private Helpers (Syntax)

(defun fancy-fill-paragraph--line-last-non-blank (pos)
  "Return the position of the last non-blank character on POS's line.
Returns POS when the line is blank, there is no such character and POS
already sits on the line, so it probes the same syntax region."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (let ((line-beg (pos-bol)))
      (goto-char (pos-eol))
      (skip-chars-backward " \t" line-beg)
      (cond
       ((< line-beg (point))
        (1- (point)))
       (t
        pos)))))

(defun fancy-fill-paragraph--comment-or-string-start-at (pos)
  "Return the comment or string start position for POS, or nil.
Probes `syntax-ppss' at POS and, when that yields nothing, at
POS+1 so that a cursor directly on a comment-start character
\(e.g. `#') or string delimiter is still detected.
The quotes ending a triple-quoted string belong to that string, see
`fancy-fill-paragraph--string-end'."
  (declare (important-return-value t))
  (save-excursion
    (let ((start
           (or (ppss-comment-or-string-start (syntax-ppss pos))
               (cond
                ((< pos (point-max))
                 (let ((beg (ppss-comment-or-string-start (syntax-ppss (1+ pos)))))
                   (and beg (<= beg pos) beg)))
                (t
                 nil)))))
      (or (and start (fancy-fill-paragraph--string-fence-closer-owner start)) start))))

(defsubst fancy-fill-paragraph--syntax-string-delimiter-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR is a string delimiter.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       ;; Class 7: string-quote (e.g. \" in C).
       ;; Class 15: string-fence (used by `python-mode' for triple-quoted strings).
       (memq (syntax-class syn-descriptor) '(7 15))))

(defconst fancy-fill-paragraph--string-delimiter-syntax "\"|"
  "The string delimiter syntax classes, for `skip-syntax-forward'.
String quote and string fence, as for
`fancy-fill-paragraph--syntax-string-delimiter-p', so a backtick counts
as much as a double quote.")

(defsubst fancy-fill-paragraph--syntax-string-fence-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR is a string fence (class 15).
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (eq 15 (syntax-class syn-descriptor)))

(defun fancy-fill-paragraph--sexp-end (pos)
  "Return the end of the sexp at POS, or nil when it never closes."
  (declare (important-return-value t))
  (condition-case nil
      (scan-sexps pos 1)
    (scan-error
     nil)))

(defun fancy-fill-paragraph--empty-string-at-p (pos)
  "Return non-nil when POS opens an empty string, two delimiters in a row."
  (declare (important-return-value t))
  (and (fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after pos))
       (eq (+ pos 2) (fancy-fill-paragraph--sexp-end pos))))

(defun fancy-fill-paragraph--string-end (opener-pos)
  "Return the position after the closer of the string opening at OPENER-POS.
Returns nil when the string never closes.

NOTE: `python-mode' gives string fence syntax to the first quote of a
closing triple quote, ending the string there.  The two quotes after it
then read as an empty string of their own, taken for a separate region
to fill or for code following the closer.  Include them in the closer."
  (declare (important-return-value t))
  (let ((end (fancy-fill-paragraph--sexp-end opener-pos)))
    (cond
     ((and end
           (fancy-fill-paragraph--syntax-string-fence-p (syntax-after (1- end)))
           (fancy-fill-paragraph--empty-string-at-p end))
      (+ end 2))
     (t
      end))))

(defun fancy-fill-paragraph--string-fence-closer-owner (pos)
  "Return the start of the string whose closer holds the empty string at POS.
Returns nil when POS does not open the two quotes after the fence
closing a triple-quoted string, see `fancy-fill-paragraph--string-end'."
  (declare (important-return-value t))
  (let* ((ppss (and (> pos (point-min)) (syntax-ppss (1- pos))))
         ;; Only a string owns them.  A comment ending just before POS (a
         ;; shebang line above a module doc-string for e.g.) is skipped by
         ;; the scan for its end, which then ends past the quotes too.
         (owner (and (nth 3 ppss) (nth 8 ppss))))
    (and owner (eq (+ pos 2) (fancy-fill-paragraph--string-end owner)) owner)))

(defun fancy-fill-paragraph--string-fence-opener-start (pos)
  "Return the start of the string whose opener holds the empty string at POS.
Returns nil when POS does not open the two quotes before the fence
opening a triple-quoted string.  Like the closer, `python-mode' gives
fence syntax to one quote of the opener, its last, so the two before
it read as an empty string of their own."
  (declare (important-return-value t))
  (and (fancy-fill-paragraph--syntax-string-fence-p (syntax-after (+ pos 2)))
       (fancy-fill-paragraph--empty-string-at-p pos)
       (+ pos 2)))

(defsubst fancy-fill-paragraph--syntax-comment-end-class-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR indicates a `comment-end' character.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       ;; Class 12: comment-end (e.g. newline in Lisp).
       ;; Bit 19: second character of a two-character comment ender (e.g. / in */).
       (or (= (syntax-class syn-descriptor) 12)
           (not (zerop (logand (car syn-descriptor) (ash 1 19)))))))

(defsubst fancy-fill-paragraph--syntax-comment-start-2char-p (syn-first syn-next)
  "Return non-nil if SYN-FIRST and SYN-NEXT open a two-character comment.
Either may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-first
       syn-next
       ;; Bit 16: first character of a two-character comment starter
       ;; (e.g. / in //).
       (not (zerop (logand (car syn-first) (ash 1 16))))
       ;; Bit 17: second character of a two-character comment starter.
       (not (zerop (logand (car syn-next) (ash 1 17))))))

(defsubst fancy-fill-paragraph--syntax-comment-style (syn-main &optional syn-other)
  "Return the comment style bits for a comment delimiter.
SYN-MAIN is the syntax of the style-determining character: the only
character of a one-character delimiter, the second character of a
two-character starter or the first of a two-character ender.
SYN-OTHER is the syntax of the remaining character of a two-character
sequence, when present.  Style b (bit 21) is carried only by SYN-MAIN
while style c (bit 23) is carried by either character, matching
SYNTAX_FLAGS_COMMENT_STYLE in the Emacs sources.  Style a sets
neither bit.  SYN-MAIN may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-main
       (logior
        (logand (car syn-main) (ash 1 21))
        (logand
         (logior
          (car syn-main)
          (cond
           (syn-other
            (car syn-other))
           (t
            0)))
         (ash 1 23)))))


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
    (let ((earliest nil))
      (dolist (pattern patterns earliest)
        (let ((match (string-search pattern text pos)))
          (when match
            (let ((point
                   (cond
                    ((eq type 'close)
                     (+ match (1- (length pattern))))
                    (t
                     match))))
              (setq earliest (fancy-fill-paragraph--earliest-match earliest point)))))))))

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

(defun fancy-fill-paragraph--syntax-fill-job-create (&rest plist)
  "Return an internal syntax fill job from PLIST.
PLIST must contain these keys:
- :beg is the region start passed to the fill function.
- :end is the region end passed to the fill function.
- :fill-fn is the function that performs the fill.
- :args are extra arguments appended after :beg and :end.
- :resume is where syntax scanning continues after this job.

The :args value may be nil; all other values must be non-nil."
  (declare (important-return-value t))

  (unless (and plist (listp plist))
    (error "Syntax fill job values must be a property list"))

  (let* ((unset (make-symbol "unset"))
         (kw-beg unset)
         (kw-end unset)
         (kw-fill-fn unset)
         ;; `:args' may be nil, so use a sentinel to detect omission.
         (kw-args unset)
         (kw-resume unset)
         ;; Iteration variables.
         (keyw nil)
         (val nil))

    (while (keywordp (setq keyw (car plist)))
      (setq plist (cdr plist))
      (setq val (pop plist))
      (pcase keyw
        (:beg (setq kw-beg val))
        (:end (setq kw-end val))
        (:fill-fn (setq kw-fill-fn val))
        (:args (setq kw-args val))
        (:resume (setq kw-resume val))
        (_ (error "Unexpected syntax fill job key: %S" keyw))))

    (when plist
      (error "Unexpected trailing syntax fill job values: %S" plist))

    ;; Ensure required keywords.
    (unless (and (not (eq kw-beg unset)) (integer-or-marker-p kw-beg))
      (error "Syntax fill job :beg must be a buffer position"))
    (unless (and (not (eq kw-end unset)) (integer-or-marker-p kw-end))
      (error "Syntax fill job :end must be a buffer position"))
    (unless (and (not (eq kw-fill-fn unset)) (functionp kw-fill-fn))
      (error "Syntax fill job :fill-fn must be a function"))
    (unless (and (not (eq kw-args unset)) (listp kw-args))
      (error "Syntax fill job :args must be a list"))
    (unless (and (not (eq kw-resume unset)) (integer-or-marker-p kw-resume))
      (error "Syntax fill job :resume must be a buffer position"))

    `[
      ;; 0: beg.
      ,kw-beg
      ;; 1: end.
      ,kw-end
      ;; 2: fill-fn.
      ,kw-fill-fn
      ;; 3: args.
      ,kw-args
      ;; 4: resume.
      ,kw-resume]))

(defsubst fancy-fill-paragraph--syntax-fill-job-beg (job)
  "Return the beginning position for syntax fill JOB."
  (declare (important-return-value t))
  (aref job 0))

(defsubst fancy-fill-paragraph--syntax-fill-job-end (job)
  "Return the end position for syntax fill JOB."
  (declare (important-return-value t))
  (aref job 1))

(defsubst fancy-fill-paragraph--syntax-fill-job-fill-fn (job)
  "Return the fill function for syntax fill JOB."
  (declare (important-return-value t))
  (aref job 2))

(defsubst fancy-fill-paragraph--syntax-fill-job-args (job)
  "Return the extra fill arguments for syntax fill JOB."
  (declare (important-return-value t))
  (aref job 3))

(defsubst fancy-fill-paragraph--syntax-fill-job-resume (job)
  "Return the scan resume position for syntax fill JOB."
  (declare (important-return-value t))
  (aref job 4))

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
      (:paren close default 10 ") ")
      (:bracket close default 10 "] ")
      (:brace close default 10 "} ")
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
  ;; Use `documentation-property' rather than `get' so the byte-compiler's
  ;; lazy (file . offset) docstring reference is resolved to a real string.
  (concat
   (documentation-property 'fancy-fill-paragraph-split-weights 'variable-documentation t)
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
    (text local-fill-column &optional first-line-extra-width)
  "Fill each paragraph in TEXT independently within LOCAL-FILL-COLUMN.
Paragraphs are separated by blank lines.  Returns the filled text
with paragraph separators preserved.
`fill-prefix' is set to the empty string, the caller has already
stripped the prefix, so `fill-context-prefix' detection is suppressed.
FIRST-LINE-EXTRA-WIDTH, when non-nil, is how many columns the first
line takes beyond the others, when it carries an opener prefix of
another width (e.g. `/** ' beside ` * ').  Only the opening paragraph
holds the first line."
  (declare (important-return-value t))
  ;; Nothing to fill when text is empty or blank-space-only.
  (cond
   ((fancy-fill-paragraph--string-blank-or-empty-p text)
    text)
   (t
    (with-temp-buffer
      (insert text)
      (let* ((fill-column local-fill-column)
             (fill-prefix "")
             ;; Reverse ordered, so the opening paragraph is last.
             (bounds (fancy-fill-paragraph--collect-paragraph-bounds (point-min) (point-max))))
        (fancy-fill-paragraph--fill-paragraph-bounds (butlast bounds))
        ;; The text is not blank, so there is an opening paragraph.
        (let ((para (car (last bounds))))
          (fancy-fill-paragraph--fill-region (car para) (cdr para) first-line-extra-width)))
      (buffer-string)))))

(defun fancy-fill-paragraph--fill-and-reprefix
    (lines local-fill-column cont-prefix &optional first-prefix first-line-extra-width)
  "Fill LINES within LOCAL-FILL-COLUMN and re-add CONT-PREFIX to each output line.
FIRST-PREFIX, when non-nil, is used instead of CONT-PREFIX on the
first output line (e.g. for a comment opener like \"/** \").
FIRST-LINE-EXTRA-WIDTH, when non-nil, is how many columns FIRST-PREFIX
takes beyond CONT-PREFIX, narrowing the first line by as much.  It is
never negative."
  (declare (important-return-value t))
  (let ((stripped-text (mapconcat #'identity lines "\n")))
    ;; Return nil when stripped text is empty or blank-space-only.
    (cond
     ((fancy-fill-paragraph--string-blank-or-empty-p stripped-text)
      nil)
     (t
      (let* ((filled-text
              (fancy-fill-paragraph--fill-paragraphs-in-string stripped-text local-fill-column
                                                               first-line-extra-width))
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

(defun fancy-fill-paragraph--code-after-closer-p (closer-end)
  "Return non-nil when non-blank text follows CLOSER-END on its line.
CLOSER-END is the position just after a comment or string closing
delimiter.  Trailing code on the closer line must never be reflowed
as comment or string text, so callers skip inline filling for such
regions."
  (declare (important-return-value t))
  (save-excursion
    (goto-char closer-end)
    (skip-chars-forward " \t" (pos-eol))
    (not (eolp))))

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

(defun fancy-fill-paragraph--line-comment-opener-at-point-p ()
  "Return non-nil when point is at a line comment opener.
Match single-character openers by syntax class and two-character
openers by syntax flags, then require a comment style that a newline
terminates (see `fancy-fill-paragraph--syntax-comment-style' for how
the style is derived).  The style of a block comment opener such as
/* never matches the newline comment-ender style, so block openers
are excluded even when unterminated."
  (declare (important-return-value t))
  (let* ((syn (syntax-after (point)))
         (style
          (and syn
               (cond
                ;; Class 11 = comment-start.
                ((= (syntax-class syn) 11)
                 (fancy-fill-paragraph--syntax-comment-style syn))
                ((fancy-fill-paragraph--syntax-comment-start-2char-p
                  syn (syntax-after (1+ (point))))
                 (fancy-fill-paragraph--syntax-comment-style (syntax-after (1+ (point))) syn))
                (t
                 nil)))))
    (and style
         ;; Prefer the syntax of the line's own newline so character
         ;; and text property syntax both apply; fall back to the
         ;; syntax table when the buffer ends without one.
         (let ((nl-syn (or (syntax-after (pos-eol)) (aref (syntax-table) ?\n))))
           (and nl-syn
                ;; Class 12: comment-end.
                (= (syntax-class nl-syn) 12)
                (= (fancy-fill-paragraph--syntax-comment-style nl-syn) style))))))

(defun fancy-fill-paragraph--line-comment-delimiter-end (pos)
  "Return the end position of the line comment delimiter run at POS.
POS must be at a line comment opener: a class 11 character or the
first character of a two-character opener.  The run spans repeated
delimiter characters so leaders such as \";;;\" or \"///\" stay
whole.  Return nil when POS is not at a delimiter."
  (declare (important-return-value t))
  (save-excursion
    (goto-char pos)
    (cond
     ;; Syntax class `<' (comment-start) matches only the delimiter,
     ;; unlike a character-class skip which would consume content too.
     ((not (zerop (skip-syntax-forward "<")))
      (point))
     ;; Two-character openers are punctuation class carrying comment
     ;; flags instead of class `<'.
     ((fancy-fill-paragraph--syntax-comment-start-2char-p
       (syntax-after (point)) (syntax-after (1+ (point))))
      (forward-char 2)
      ;; Consume any further starter-flagged characters so that
      ;; doc-comment leaders keep their full delimiter run, as the
      ;; class `<' skip above does for repeated single-character
      ;; delimiters.
      (while (let ((syn (syntax-after (point))))
               (and syn (not (zerop (logand (car syn) (logior (ash 1 16) (ash 1 17)))))))
        (forward-char 1))
      (point))
     (t
      nil))))

(defun fancy-fill-paragraph--line-comment-pos-at-col (indent-col delim-str)
  "Return line comment opener at INDENT-COL, or nil.
DELIM-STR is the delimiter run of the block opener; a comment whose
run differs belongs to another block (\";;\" must not absorb \";;;\"
nor \"#\" absorb \"##\"), since the continuation prefix is stripped
by column and would delete or distort a mismatched delimiter."
  (declare (important-return-value t))
  (save-excursion
    ;; `move-to-column' may overshoot when INDENT-COL falls inside a
    ;; tab or the line is short; a comment at the resulting column is
    ;; at a different indentation and must not join the block.
    (and (= (move-to-column indent-col) indent-col)
         (fancy-fill-paragraph--line-comment-opener-at-point-p)
         (let ((comment-pos (point))
               (delim-end (fancy-fill-paragraph--line-comment-delimiter-end (point))))
           (and delim-end
                (equal delim-str (buffer-substring-no-properties comment-pos delim-end))
                (let ((ppss (syntax-ppss delim-end)))
                  (and (ppss-comment-depth ppss)
                       (= (ppss-comment-or-string-start ppss) comment-pos)))
                comment-pos)))))

(defun fancy-fill-paragraph--line-comment-at-col-p (indent-col delim-str)
  "Return non-nil when the current line has a line comment at INDENT-COL.
DELIM-STR is the expected delimiter run."
  (declare (important-return-value t))
  (not (null (fancy-fill-paragraph--line-comment-pos-at-col indent-col delim-str))))

(defun fancy-fill-paragraph--line-comment-fill-beg (comment-pos)
  "Return the fill start for the line comment at COMMENT-POS.
When non-blank code precedes the comment opener, start at COMMENT-POS;
otherwise include the line's indentation by starting at BOL."
  (declare (important-return-value t))
  (save-excursion
    (goto-char comment-pos)
    (let ((line-beg (pos-bol)))
      (goto-char line-beg)
      (skip-chars-forward " \t" comment-pos)
      (cond
       ((< (point) comment-pos)
        comment-pos)
       (t
        line-beg)))))

(defun fancy-fill-paragraph--line-comment-fill-beg-at-col (indent-col delim-str)
  "Return fill start for a matching line comment at INDENT-COL, or nil.
DELIM-STR is the expected delimiter run."
  (declare (important-return-value t))
  (let ((comment-pos (fancy-fill-paragraph--line-comment-pos-at-col indent-col delim-str)))
    (and comment-pos (fancy-fill-paragraph--line-comment-fill-beg comment-pos))))

(defun fancy-fill-paragraph--line-comment-bounds (opener-pos beg end)
  "Return (BLOCK-BEG . BLOCK-END) for consecutive line comments around OPENER-POS.
OPENER-POS is the comment-start of the line comment at point.
BEG and END bound the search.  Only lines whose comment delimiter run
starts at the same column as OPENER-POS and matches its delimiter run
are included."
  (declare (important-return-value t))
  (save-excursion
    (goto-char opener-pos)
    (let* ((indent-col (fancy-fill-paragraph--line-comment-indent-col opener-pos))
           (delim-str
            (let ((delim-end (fancy-fill-paragraph--line-comment-delimiter-end opener-pos)))
              (and delim-end (buffer-substring-no-properties opener-pos delim-end))))
           (block-beg (fancy-fill-paragraph--line-comment-fill-beg opener-pos))
           (opener-full-line (= block-beg (pos-bol)))
           (block-end (pos-eol)))
      ;; Scan backward.
      (when opener-full-line
        (let ((done nil))
          (while (and (not done) (zerop (forward-line -1)) (>= (point) beg))
            (let ((line-beg (pos-bol))
                  (fill-beg
                   (fancy-fill-paragraph--line-comment-fill-beg-at-col indent-col delim-str)))
              (cond
               ((null fill-beg)
                (setq done t))
               (t
                (setq block-beg fill-beg)
                (unless (= fill-beg line-beg)
                  (setq done t))))))))
      ;; Scan forward.
      (goto-char opener-pos)
      (let ((done nil))
        (while (and (not done) (zerop (forward-line 1)) (<= (point) end))
          (let ((line-beg (pos-bol))
                (fill-beg
                 (fancy-fill-paragraph--line-comment-fill-beg-at-col indent-col delim-str)))
            (cond
             ((null fill-beg)
              (setq done t))
             ((= fill-beg line-beg)
              (setq block-end (pos-eol)))
             (t
              (setq done t)))
            (goto-char (pos-eol)))))
      (cons block-beg block-end))))

(defun fancy-fill-paragraph--line-comment-prefix (beg end)
  "Return prefix info for line comment block BEG..END.
BEG may be at the line beginning, anywhere in the leading blank-space,
or at the comment-start delimiter itself.
The returned plist has these keys:
- :prefix holds the first line's indentation and delimiter run followed
  by blank-space whose width is the narrowest delimiter-to-content gap.
- :prefix-col is the buffer column at which :prefix ends.
- :first-prefix is non-nil for trailing comments that share a line with
  code; replacing from the comment opener then leaves the code intact
  while continuation lines stay aligned to the opener column."
  (declare (important-return-value t))
  (save-excursion
    (goto-char beg)
    ;; Skip leading blank-space (no-op when BEG is at the delimiter).
    (skip-chars-forward " \t" (pos-eol))
    (let* ((comment-pos (point))
           (trailing-comment
            (= (fancy-fill-paragraph--line-comment-fill-beg comment-pos) comment-pos))
           (delim-end
            (or (fancy-fill-paragraph--line-comment-delimiter-end comment-pos) comment-pos))
           (delim-str (buffer-substring-no-properties comment-pos delim-end))
           (indent-col (current-column))
           (head
            (cond
             (trailing-comment
              (concat (make-string indent-col ?\s) delim-str))
             (t
              (buffer-substring-no-properties (pos-bol) delim-end))))
           (run-end-col
            (progn
              (goto-char delim-end)
              (current-column)))
           (gap nil))
      ;; Find the narrowest delimiter-to-content gap across the block.
      ;; Block lines share the delimiter run and its column.
      (goto-char beg)
      (forward-line 0)
      (while (< (point) end)
        (move-to-column run-end-col)
        (skip-chars-forward " \t" (pos-eol))
        (cond
         ;; Delimiter-only line: no content to protect.
         ((eolp)
          nil)
         (t
          (let ((line-gap (- (current-column) run-end-col)))
            (cond
             ((or (null gap) (< line-gap gap))
              (setq gap line-gap))))))
        (cond
         ((zerop (forward-line 1))
          nil)
         (t
          (goto-char (point-max)))))
      (let ((gap (or gap 0)))
        (list
         :prefix (concat head (make-string gap ?\s))
         :prefix-col (+ run-end-col gap)
         :first-prefix (and trailing-comment (concat delim-str (make-string gap ?\s))))))))

(defun fancy-fill-paragraph--fill-line-comment-region (beg end &optional sel-beg sel-end)
  "Fill a line-comment block in the region BEG..END.
Paragraph boundaries are detected with the block-wide prefix column,
then the continuation prefix is derived per paragraph: a cursor on a
continuation line (e.g. with deeper post-`#' indent) still produces
the canonical prefix shared by the paragraph, and a paragraph whose
delimiter-to-content gap is wider than another paragraph's keeps its
own prefix instead of being re-indented to the block minimum.
When SEL-BEG and SEL-END are non-nil, fill only paragraphs
overlapping that selection.  Otherwise fill the paragraph at point."
  (let ((pos (point)))
    (save-excursion
      (let ((prefix-col
             (plist-get (fancy-fill-paragraph--line-comment-prefix beg end) :prefix-col)))
        (fancy-fill-paragraph--fill-body-paragraphs
         beg end prefix-col (or sel-beg pos) (or sel-end pos)
         (lambda (para-beg para-end)
           (let* ((prefix-info (fancy-fill-paragraph--line-comment-prefix para-beg para-end))
                  (prefix (plist-get prefix-info :prefix))
                  (first-prefix (plist-get prefix-info :first-prefix))
                  (prefix-col (plist-get prefix-info :prefix-col)))
             (fancy-fill-paragraph--fill-prefixed-region para-beg para-end prefix
                                                         first-prefix
                                                         nil
                                                         nil
                                                         prefix-col))))))))

(defun fancy-fill-paragraph--line-comment-syntax-start-p (syn-start)
  "Return non-nil when SYN-START begins a line comment."
  (declare (important-return-value t))
  (save-excursion
    (goto-char syn-start)
    (and (forward-comment 1) (not (fancy-fill-paragraph--syntax-comment-end-p)))))

(defun fancy-fill-paragraph--syntax-fill-job-at (syn-start &optional sel-bounds search-bounds)
  "Return a syntax fill job for the region starting at SYN-START.
When SEL-BOUNDS is non-nil, include its bounds so only overlapping
paragraphs are filled by the job runner.
SEARCH-BOUNDS limits line-comment block expansion."
  (declare (important-return-value t))
  ;; The quotes before a triple quote's fence open the string after them.
  ;; NOTE: mapped here rather than in
  ;; `fancy-fill-paragraph--comment-or-string-start-at' (as the closer's
  ;; quotes are), which returns a start at or before its position.  This
  ;; start follows it, the region scan would take the quotes for code.
  (setq syn-start (or (fancy-fill-paragraph--string-fence-opener-start syn-start) syn-start))
  (save-excursion
    (goto-char syn-start)
    (let ((region-beg (pos-bol))
          (sel-beg (car sel-bounds))
          (sel-end (cdr sel-bounds)))
      (cond
       ;; Block comment: advance past closer via forward-comment.
       ;; Check `--syntax-comment-end-p' to exclude line comments
       ;; (whose closer is a newline, not a block-comment ender).
       ((and (forward-comment 1) (fancy-fill-paragraph--syntax-comment-end-p))
        (fancy-fill-paragraph--syntax-fill-job-create
         :beg region-beg
         :end (pos-eol)
         :fill-fn #'fancy-fill-paragraph--fill-block-comment-region
         :args (list syn-start sel-beg sel-end)
         :resume (point)))
       ;; Line comment: find the full block and skip past it.
       ((progn
          (goto-char syn-start)
          (fancy-fill-paragraph--line-comment-syntax-start-p syn-start))
        (let ((bounds
               (fancy-fill-paragraph--line-comment-bounds
                syn-start
                (or (car search-bounds) (point-min))
                (or (cdr search-bounds) (point-max)))))
          (fancy-fill-paragraph--syntax-fill-job-create
           :beg (car bounds)
           :end (cdr bounds)
           :fill-fn #'fancy-fill-paragraph--fill-line-comment-region
           :args (list sel-beg sel-end)
           :resume (1+ (cdr bounds)))))
       ;; String: advance past closer, an unclosed string has no job.
       ((progn
          (goto-char syn-start)
          (fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after (point))))
        (let ((string-end (fancy-fill-paragraph--string-end syn-start)))
          (when string-end
            (goto-char string-end)
            (fancy-fill-paragraph--syntax-fill-job-create
             :beg region-beg
             :end (pos-eol)
             :fill-fn #'fancy-fill-paragraph--fill-string-region
             :args (list syn-start sel-beg sel-end)
             :resume (point)))))
       (t
        nil)))))

(defun fancy-fill-paragraph--syntax-fill-job-run (job)
  "Run syntax fill JOB and return non-nil when it modified the buffer."
  (declare (important-return-value t))
  (apply (fancy-fill-paragraph--syntax-fill-job-fill-fn job)
         (fancy-fill-paragraph--syntax-fill-job-beg job)
         (fancy-fill-paragraph--syntax-fill-job-end job)
         (fancy-fill-paragraph--syntax-fill-job-args job)))

(defun fancy-fill-paragraph--syntax-fill-jobs-run (jobs &optional reverse)
  "Run syntax fill JOBS.
When REVERSE is non-nil, fill later jobs first so earlier buffer
positions remain valid."
  (declare (important-return-value t))
  (let ((changed nil)
        (jobs
         (cond
          (reverse
           (reverse jobs))
          (t
           jobs))))
    (dolist (job jobs)
      (when (fancy-fill-paragraph--syntax-fill-job-run job)
        (setq changed t)))
    changed))

(defun fancy-fill-paragraph--syntax-fill-jobs-in-range (beg end pos)
  "Return syntax fill jobs for comment or string regions in BEG..END.
POS is the cursor position, used to detect when point is inside a
comment or string whose opener precedes BEG.
Returns nil when no syntax boundaries are found, and when
`fancy-fill-paragraph-syntax-bounds' is nil."
  (declare (important-return-value t))
  (cond
   (fancy-fill-paragraph-syntax-bounds
    (save-excursion
      ;; Ensure syntax properties are set for the entire region.
      ;; Modes like `python-mode' rely on `syntax-propertize-function'
      ;; to mark triple-quoted strings.
      (syntax-propertize end)
      (let ((jobs nil))
        (goto-char beg)
        ;; If BEG or POS is inside a comment or string, retreat to
        ;; its opener.  This handles the case where paragraph bounds
        ;; extend beyond the syntax region (e.g. code surrounds the
        ;; comment).  POS is probed at the last non-blank character of
        ;; its line, so the column never changes the result.
        (let ((syn-start
               (or (fancy-fill-paragraph--comment-or-string-start-at beg)
                   (fancy-fill-paragraph--comment-or-string-start-at
                    (fancy-fill-paragraph--line-last-non-blank pos)))))
          (when syn-start
            (goto-char syn-start)))
        (while (< (point) end)
          ;; Skip blank-space between regions.
          (skip-chars-forward " \t\n" end)
          (when (< (point) end)
            (let ((job (fancy-fill-paragraph--syntax-fill-job-at (point) nil (cons beg end))))
              (cond
               (job
                (push job jobs)
                (goto-char (fancy-fill-paragraph--syntax-fill-job-resume job)))
               (t
                ;; Neither comment nor string, stop scanning.
                (goto-char end))))))
        (nreverse jobs))))
   (t
    nil)))

(defun fancy-fill-paragraph--syntax-region-at (syn-start)
  "Return the region opening at SYN-START as a (KIND . RESUME) cons.
KIND is `comment' or `string'.  RESUME is where to resume scanning,
past the closer and any blank-space following it, so it can sit past
the region.  RESUME is nil when the region never closes, an apostrophe
in shell text opens a string that runs to the end of the buffer.
Both parts are reported because an unclosed comment and an unclosed
string are told apart by KIND alone."
  (declare (important-return-value t))
  (save-excursion
    (goto-char syn-start)
    (cond
     ;; Moves past the closer, blank-space following it is skipped too.
     ;; A line comment is closed by its newline.
     ((forward-comment 1)
      (cons 'comment (point)))
     ((fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after syn-start))
      ;; `fancy-fill-paragraph--string-end' reports the position without
      ;; moving point, which `forward-comment' above has already disturbed.
      (cons 'string (fancy-fill-paragraph--string-end syn-start)))
     (t
      ;; `forward-comment' fails on a comment that is never closed.
      (cons 'comment nil)))))

(defun fancy-fill-paragraph--range-code-beside-comment-or-string-p (beg end)
  "Return non-nil when BEG..END holds code beside a comment or string.
Filling such a range as prose merges the code into the comment or
string text.  Code alone or a comment alone is safe to fill, so both
must be present.  A string that never closes is not counted, an
apostrophe in shell text opens one.  A comment counts either way, code
beside one still being typed merges just the same.
An empty or reversed range holds nothing."
  (declare (important-return-value t))
  (save-excursion
    (syntax-propertize end)
    (let ((has-code nil)
          (has-syn nil))
      (goto-char beg)
      ;; An empty or reversed range never enters the loop.
      (while (and (< (point) end) (not (and has-code has-syn)))
        (let* ((from (point))
               (syn-start
                (or
                 ;; Already within a region, its opener precedes FROM.
                 (fancy-fill-paragraph--comment-or-string-start-at from)
                 ;; Otherwise scan ahead, stopping once one is entered.
                 (ppss-comment-or-string-start
                  (parse-partial-sexp from end nil nil nil 'syntax-table)))))
          ;; Text before the opener is code, the opener itself is not.
          ;; The range reverses when the opener precedes FROM, which
          ;; means FROM is inside the region and holds no code.
          (unless has-code
            (setq has-code (fancy-fill-paragraph--range-has-non-blank-p from (or syn-start end))))
          (cond
           (syn-start
            (let* ((region (fancy-fill-paragraph--syntax-region-at syn-start))
                   (resume (cdr region)))
              ;; A comment counts whether or not it is closed, one still
              ;; being typed has code beside it just the same.  A string
              ;; only counts when it closes, an apostrophe in shell text
              ;; opens one that never does.
              (when (or (eq (car region) 'comment) resume)
                (setq has-syn t))
              ;; An unclosed region runs to the end of the buffer.  A
              ;; closed one always resumes past FROM, which is inside it,
              ;; so the scan cannot retreat.
              (goto-char (or resume end))))
           (t
            (goto-char end)))))
      (and has-code has-syn))))

(defun fancy-fill-paragraph--fill-region-refuse-p (beg end)
  "Return non-nil when a prose fill of BEG..END must be refused.
Such a fill merges code sharing the range into the comment or string
text.  The check is limited to programming modes, `org-mode' and
`conf-mode' set `comment-start' yet give a double quote string syntax,
so quoted prose would otherwise stop filling.  Returns nil both where
the range is safe and where the check does not apply."
  (declare (important-return-value t))
  (and fancy-fill-paragraph-syntax-bounds
       (derived-mode-p 'prog-mode)
       (fancy-fill-paragraph--range-code-beside-comment-or-string-p beg end)))

(defun fancy-fill-paragraph--drop-refused-paragraphs (bounds)
  "Return BOUNDS without the paragraphs a prose fill is refused for.
See `fancy-fill-paragraph--fill-region-refuse-p' for what is refused.
Ordering is preserved, so callers still fill later paragraphs first."
  (declare (important-return-value t))
  (let ((result nil))
    (dolist (para-bounds bounds)
      (unless (fancy-fill-paragraph--fill-region-refuse-p (car para-bounds) (cdr para-bounds))
        (push para-bounds result)))
    (nreverse result)))

(defun fancy-fill-paragraph--syntax-fill-jobs-for-selection (beg end)
  "Return syntax fill jobs for active selection BEG..END, or nil.
Only selections contained by one syntax region, or spanning line
comments at the same indentation, are handled here.
Returns nil when `fancy-fill-paragraph-syntax-bounds' is nil."
  (declare (important-return-value t))
  (cond
   (fancy-fill-paragraph-syntax-bounds
    (save-excursion
      (syntax-propertize end)
      (let* ((beg-syn (fancy-fill-paragraph--comment-or-string-start-at beg))
             (end-syn (fancy-fill-paragraph--comment-or-string-start-at end))
             (syn-start (and beg-syn end-syn (= beg-syn end-syn) beg-syn)))
        (cond
         (syn-start
          (let ((job (fancy-fill-paragraph--syntax-fill-job-at syn-start (cons beg end))))
            (and job (list job))))
         ;; Selection spans multiple line comments at the same indent:
         ;; beg-syn and end-syn differ but both are line comments.
         ((and beg-syn
               end-syn
               (fancy-fill-paragraph--line-comment-syntax-start-p beg-syn)
               (fancy-fill-paragraph--line-comment-syntax-start-p end-syn)
               (= (fancy-fill-paragraph--line-comment-indent-col beg-syn)
                  (fancy-fill-paragraph--line-comment-indent-col end-syn)))
          (let ((job (fancy-fill-paragraph--syntax-fill-job-at beg-syn (cons beg end))))
            (and job (list job))))
         (t
          nil)))))
   (t
    nil)))

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
        (unless (fancy-fill-paragraph--line-matches-prefix-p cont-prefix)
          (setq all-match nil))
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
    (lines replace-beg replace-end cont-prefix &optional first-prefix prefix-col)
  "Fill LINES with CONT-PREFIX and replace REPLACE-BEG..REPLACE-END.
FIRST-PREFIX, when non-nil, is used on the first output line instead
of CONT-PREFIX (e.g. for a comment opener like \"/* \").
PREFIX-COL, when non-nil, is the buffer-measured width of CONT-PREFIX.
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (or prefix-col (string-width cont-prefix)))
         ;; FIRST-PREFIX starts at REPLACE-BEG, measure its end column there,
         ;; so code before a trailing comment's opener is counted too.
         ;; A narrower FIRST-PREFIX leaves the first line as wide as the others.
         (first-line-extra-width
          (and first-prefix
               (max 0
                    (- (fancy-fill-paragraph--prefix-column-width
                        replace-beg first-prefix)
                       prefix-col))))
         (result
          (fancy-fill-paragraph--fill-and-reprefix
           lines (max 1 (- fill-column prefix-col)) cont-prefix
           first-prefix first-line-extra-width)))
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
    (beg end cont-prefix &optional first-prefix first-line-text replace-beg prefix-col)
  "Strip lines from BEG..END, fill with CONT-PREFIX, and replace the region.
Lines are stripped of CONT-PREFIX width columns.  When FIRST-LINE-TEXT
is non-nil it is prepended as the first line (e.g. opener-line body text).
FIRST-PREFIX, when non-nil, is used on the first output line.
REPLACE-BEG, when non-nil, overrides BEG as the replacement start
\(e.g. to include the opener line in the replaced span).
PREFIX-COL, when non-nil, overrides the buffer-measured width of
CONT-PREFIX at BEG (e.g. line comments may strip fewer columns than
the literal prefix string occupies).
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (or prefix-col (fancy-fill-paragraph--prefix-column-width beg cont-prefix)))
         ;; FIRST-LINE-TEXT supplies the opener line, so every line in the
         ;; range is a continuation carrying the synthesized CONT-PREFIX.
         ;; Without it the range opens on the delimiter line, and the prefix
         ;; was read from the buffer, so neither may be matched literally.
         (cont-prefix-literal (and first-line-text cont-prefix))
         (lines
          (fancy-fill-paragraph--strip-region-lines beg (min (1+ end) (point-max)) prefix-col
                                                    cont-prefix-literal))
         (lines
          (cond
           (first-line-text
            (cons first-line-text lines))
           (t
            lines))))
    (fancy-fill-paragraph--fill-prefixed-paragraph lines (or replace-beg beg) end cont-prefix
                                                   first-prefix
                                                   prefix-col)))

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
    (beg
     inner-body
     cont-prefix
     opener-prefix
     opener-text
     para-fill-fn
     sel-beg
     sel-end
     &optional
     closer-line-end)
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
CLOSER-LINE-END, when non-nil, is the end of a closer line carrying
text, which then joins the last paragraph.
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ((prefix-col (fancy-fill-paragraph--prefix-column-width (car inner-body) cont-prefix))
         ;; Widen body to include the opener line when it carries text,
         ;; so paragraph detection sees it as part of the first paragraph.
         ;; The closer line is taken in likewise, as the inline fill does,
         ;; otherwise filling a comment once it wraps past two lines drops
         ;; its last line from the paragraph and breaks the rest again.
         (scan-beg
          (cond
           (opener-text
            beg)
           (t
            (car inner-body))))
         (scan-end (or closer-line-end (cdr inner-body))))
    (fancy-fill-paragraph--fill-body-paragraphs
     scan-beg scan-end prefix-col sel-beg sel-end
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
  "Return the start of the closer line of the block comment in BEG..END, or nil.
It is returned when the line is no wider than PREFIX-COL, the width of
the continuation prefix (e.g. ` */' beside ` * '), so it is kept as it
is rather than filled."
  (declare (important-return-value t))
  (let ((closer-bol (fancy-fill-paragraph--closer-line-bol beg end)))
    (and closer-bol
         (save-excursion
           (goto-char closer-bol)
           (<= (- (pos-eol) closer-bol) prefix-col))
         closer-bol)))

(defun fancy-fill-paragraph--block-comment-prefixed-closer-p (beg end cont-prefix)
  "Return non-nil when the closer line of the comment in BEG..END has CONT-PREFIX.
Such a line (e.g. ` * text */') is laid out as the body lines are."
  (declare (important-return-value t))
  (let ((closer-bol (fancy-fill-paragraph--closer-line-bol beg end)))
    (and closer-bol
         (string-prefix-p cont-prefix (buffer-substring-no-properties closer-bol end)))))

(defun fancy-fill-paragraph--block-comment-inline-lines
    (beg end opener-col prefix-col cont-prefix verbatim-closer-bol)
  "Return inline block comment content lines stripped for filling.
BEG and END are the full region bounds.  OPENER-COL is used for the
first line, PREFIX-COL for continuation lines carrying CONT-PREFIX.
VERBATIM-CLOSER-BOL, when non-nil, starts the closer line, which is
excluded from the returned content."
  (declare (important-return-value t))
  (let ((content-end-bol (or verbatim-closer-bol (1+ (fancy-fill-paragraph--last-bol end)))))
    (save-excursion
      (goto-char beg)
      (move-to-column opener-col)
      (let ((result (list (buffer-substring-no-properties (point) (pos-eol)))))
        (forward-line 1)
        (while (< (point) content-end-bol)
          (fancy-fill-paragraph--move-past-line-prefix prefix-col cont-prefix)
          (push (buffer-substring-no-properties (point) (pos-eol)) result)
          (forward-line 1))
        (nreverse result)))))

(defun fancy-fill-paragraph--fill-block-comment-inline
    (beg end comment-start-pos cont-prefix opener-col)
  "Fill an inline block comment in BEG..END using CONT-PREFIX.
OPENER-COL is the column where content begins after the opener delimiter."
  (unless (eq
           comment-start-pos
           (ppss-comment-or-string-start (save-excursion (syntax-ppss (1- end)))))
    (error "Expected comment at %d" comment-start-pos))
  (let* ((prefix-col (fancy-fill-paragraph--prefix-column-width beg cont-prefix))
         (opener-text
          (save-excursion
            (goto-char beg)
            (move-to-column opener-col)
            (buffer-substring-no-properties beg (point))))
         (verbatim-closer-bol
          (fancy-fill-paragraph--block-comment-inline-closer beg end prefix-col)))
    ;; A verbatim closer line is left in place, the fill ends before it.
    (fancy-fill-paragraph--fill-prefixed-paragraph
     (fancy-fill-paragraph--block-comment-inline-lines
      beg end opener-col prefix-col cont-prefix verbatim-closer-bol)
     beg
     (cond
      (verbatim-closer-bol
       (1- verbatim-closer-bol))
      (t
       end))
     cont-prefix
     opener-text
     prefix-col)))

(defun fancy-fill-paragraph--fill-block-comment-region
    (beg end comment-start-pos &optional sel-beg sel-end)
  "Fill a block comment in the region BEG..END.
COMMENT-START-POS is the buffer position of the comment opener.
When SEL-BEG and SEL-END are non-nil, fill only body paragraphs
overlapping that selection.  Otherwise fill the paragraph at point.
When lines lie between the opener and closer lines, fills body
paragraphs, text on the opener line joining the first.  Otherwise the
comment is inline and fills as one paragraph, the opener line keeping
its opener as prefix and later lines taking the continuation prefix.
Either way a closer line no wider than the continuation prefix, such
as ` */', stays as it is, and code after the closer is never reflowed.
Other text on the closer line joins the last paragraph, for body
paragraphs only when the line starts with the continuation prefix
\(e.g. ` * text */')."
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
               beg inner-body opener-col comment-start-pos))
             (closer-end
              (save-excursion
                (goto-char comment-start-pos)
                (forward-comment 1)
                (point)))
             ;; Code after the closer shares the region; reflowing it
             ;; as comment text would corrupt it.
             (closer-blocked (fancy-fill-paragraph--code-after-closer-p closer-end)))
        (fancy-fill-paragraph--fill-syntax-region
         beg end inner-body
         (lambda ()
           (fancy-fill-paragraph--fill-multiline-syntax-body
            beg
            inner-body
            cont-prefix
            (plist-get opener-info :prefix)
            (plist-get opener-info :text)
            (lambda (para-beg para-end)
              (fancy-fill-paragraph--fill-block-comment-body para-beg para-end cont-prefix))
            (or sel-beg pos)
            (or sel-end pos)
            ;; Only a closer line laid out as the body is joins the last paragraph.
            ;; NOTE: the body is filled by stripping the prefix width from each
            ;; line, a closer line of another shape (e.g. `  */' or ` **/') would
            ;; be cut into, dropping text or leaving the comment unclosed.
            ;; So such a line is kept as it is.
            (and (not closer-blocked)
                 (fancy-fill-paragraph--block-comment-prefixed-closer-p beg end cont-prefix)
                 end)))
         (lambda ()
           (cond
            (closer-blocked
             nil)
            (t
             (fancy-fill-paragraph--fill-block-comment-inline
              beg end comment-start-pos cont-prefix (plist-get opener-info :content-col))))))))))

(defun fancy-fill-paragraph--string-opener-info (opener-pos)
  "Return a plist describing the string opener at OPENER-POS.
OPENER-POS is the syntax-derived position where the string sexp begins
\(e.g. the `r' in `r\"\"\"' or the first quote in `\"\"\"').
Normal callers provide a valid string opener; the fallback path
\(no delimiter found on the line) is a defensive guard only.
The plist contains:
- :prefix  Exact buffer text from `pos-bol' to the start of opener-line
           body content (indentation + any prefix chars + delimiter +
           trailing space).
- :text    Opener-line body content after the prefix, or nil when the
           opener line contains only the delimiter."
  (declare (important-return-value t))
  (save-excursion
    (goto-char opener-pos)
    ;; Skip to the first delimiter, passing any string prefix letters
    ;; (e.g. r, f, b, u, rb, fr in r""").
    (skip-syntax-forward (concat "^" fancy-fill-paragraph--string-delimiter-syntax) (pos-eol))
    (cond
     ((or (eolp) (eq (char-after) ?\n))
      ;; Defensive fallback: no delimiter found on this line.
      (list :prefix (buffer-substring-no-properties (pos-bol) (pos-eol)) :text nil))
     (t
      ;; Skip the delimiter characters (e.g. \"\"\", ''').
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
of the text.  Either way a closer on a line of its own stays there,
while text on the closer line joins the last paragraph."
  (let* ((inner-body (fancy-fill-paragraph--comment-or-string-body-bounds beg end))
         (opener-info
          (when inner-body
            (fancy-fill-paragraph--string-opener-info opener-pos)))
         (cont-prefix
          (when (and inner-body (plist-get opener-info :text))
            (fancy-fill-paragraph--string-continuation-prefix inner-body)))
         (closer-end (fancy-fill-paragraph--string-end opener-pos))
         ;; Code after the closer shares the region; reflowing it as
         ;; string text would corrupt it.  A string that never closes
         ;; has no closer to locate, treat it the same way.
         (closer-blocked
          (or (null closer-end) (fancy-fill-paragraph--code-after-closer-p closer-end)))
         ;; A closer on its own line stays there in both fills below.
         (closer-alone-bol (fancy-fill-paragraph--string-closer-alone-bol beg end)))
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
        (or sel-end (point))
        (and (not closer-blocked) (not closer-alone-bol) end)))
     (lambda ()
       (cond
        (closer-blocked
         nil)
        (closer-alone-bol
         (fancy-fill-paragraph--fill-region beg (1- closer-alone-bol)))
        (t
         (fancy-fill-paragraph--fill-region beg end)))))))

(defun fancy-fill-paragraph--string-closer-alone-bol (beg end)
  "Return the start of the closer line of the string in BEG..END, or nil.
It is returned when the string ends on a line of its own, holding only
blank-space and the closing delimiter."
  (declare (important-return-value t))
  (let ((closer-bol (fancy-fill-paragraph--closer-line-bol beg end)))
    (and closer-bol
         (save-excursion
           (goto-char closer-bol)
           (skip-chars-forward " \t" end)
           (skip-syntax-forward fancy-fill-paragraph--string-delimiter-syntax end)
           (skip-chars-forward " \t" end)
           (= (point) end))
         closer-bol)))

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
        ;; Early exit: after every visited line has contributed to
        ;; `min-indent', a mismatch plus zero indent fixes the result at "".
        (while (and (< (point) end) (or all-match (> min-indent 0)))
          (let ((line (buffer-substring-no-properties (point) (pos-eol))))
            (setq min-indent (min min-indent (fancy-fill-paragraph--leading-indent line)))
            (when (and all-match (not (string-prefix-p prefix line)))
              (setq all-match nil)))
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

(defun fancy-fill-paragraph--strip-region-lines (beg end prefix-column-width &optional cont-prefix)
  "Return region lines from BEG..END with PREFIX-COLUMN-WIDTH stripped.
CONT-PREFIX, when non-nil, is the prefix the lines are expected to
carry, see `fancy-fill-paragraph--move-past-line-prefix' for what it
does and when it may be passed."
  (declare (important-return-value t))
  (setq end (min end (point-max)))
  (save-excursion
    (let ((result nil))
      (goto-char beg)
      (while (< (point) end)
        (fancy-fill-paragraph--move-past-line-prefix prefix-column-width cont-prefix)
        (push (buffer-substring-no-properties (point) (pos-eol)) result)
        (forward-line 1))
      (nreverse result))))


(defun fancy-fill-paragraph--paragraph-to-items (text &optional source-space-widths)
  "Split TEXT into items at weighted punctuation boundaries.
Uses `fancy-fill-paragraph--delimiter-table' for delimiter definitions
and `fancy-fill-paragraph-split-weights' for active delimiters.
SOURCE-SPACE-WIDTHS maps TEXT positions to original space-run widths.
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
                  (fancy-fill-paragraph--maybe-double-space (funcall (plist-get
                                                                      delimiter
                                                                      :join-string)
                                                                     split-pos)
                                                            text split-pos
                                                            source-space-widths)))
            (aset break-weights i (plist-get delimiter :weight))
            (aset seps i sep)
            (aset sep-lens i (string-width sep)))
          (setq boundaries-remaining (cdr boundaries-remaining))
          (setq i (1+ i)))

        (list :items items :break-weights break-weights :seps seps :sep-lens sep-lens)))))

(defun fancy-fill-paragraph--solve
    (items
     local-fill-column
     break-weights
     seps
     sep-lens
     blank-space-weight
     &optional
     first-line-extra-width)
  "Find optimal line breaks for ITEMS within LOCAL-FILL-COLUMN.
ITEMS is a list of strings to arrange into lines.
BREAK-WEIGHTS is a vector of break preference weights per item boundary.
SEPS is a vector of separator strings per item boundary.
SEP-LENS is a vector of separator lengths per item boundary.
BLANK-SPACE-WEIGHT controls joining items across delimiters (0-100).
FIRST-LINE-EXTRA-WIDTH, when non-nil, is a non-negative width added to
the first line, which begins after a prefix wider than the others.
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
              (min local-fill-column (max 1 (round (* target local-fill-column)))))
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
    ;; Every line starting at item 0 is the first line, so widen the item.
    (when (and first-line-extra-width (> n 0))
      (aset lens 0 (+ (aref lens 0) first-line-extra-width)))

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
                (let ((join-weight-sum (max 0 (aref break-weights j))))
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
                      (setq join-weight-sum
                            (+ join-weight-sum (max 0 (aref break-weights j)))))))))))
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

(defun fancy-fill-paragraph--fill-lines-plain
    (lines local-fill-column &optional first-line-extra-width)
  "Fill LINES as a single paragraph within LOCAL-FILL-COLUMN.
LINES is a list of strings.  Returns a list of result line strings.
Joins lines, normalizes blank-space, splits at delimiters, and solves.
FIRST-LINE-EXTRA-WIDTH is passed to `fancy-fill-paragraph--solve'."
  (declare (important-return-value t))
  (let* ((source-text (string-trim (mapconcat #'identity lines " ")))
         (normalized (fancy-fill-paragraph--normalize-spaces-with-widths source-text))
         (text (car normalized))
         (source-space-widths (cdr normalized)))
    (cond
     ((not (string-empty-p text))
      (let* ((split-result (fancy-fill-paragraph--paragraph-to-items text source-space-widths))
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
        (fancy-fill-paragraph--solve items
                                     local-fill-column
                                     break-weights
                                     (plist-get split-result :seps)
                                     (plist-get split-result :sep-lens)
                                     fancy-fill-paragraph-blank-space-weight
                                     first-line-extra-width)))
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

(defun fancy-fill-paragraph--fill-dot-point-group
    (group min-indent local-fill-column dp-list &optional first-line-extra-width)
  "Return filled output lines for one dot-point GROUP.
GROUP is (DP-PREFIX . RAW-LINES), MIN-INDENT is the shared list indent,
LOCAL-FILL-COLUMN is the available width, and DP-LIST is passed through
for recursive dot-point detection.
FIRST-LINE-EXTRA-WIDTH, when non-nil, is taken by the item's first line."
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
         (filled
          (fancy-fill-paragraph--fill-lines body-lines sub-fill-column dp-list
                                            first-line-extra-width))
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

(defun fancy-fill-paragraph--fill-lines-indent-split
    (lines local-fill-column dp-list &optional first-line-extra-width)
  "Fill LINES, splitting into sub-paragraphs at indentation changes.
LOCAL-FILL-COLUMN is the target line width.
Groups consecutive lines with the same leading blank-space and fills
each group independently via `fancy-fill-paragraph--fill-lines'.
Falls through to plain fill when all lines share the same indentation.
DP-LIST is passed through for dot-point detection in recursive calls.
FIRST-LINE-EXTRA-WIDTH, when non-nil, is taken by the first group."
  (declare (important-return-value t))
  (let ((groups (fancy-fill-paragraph--group-lines-by-indent lines)))
    (cond
     ((null (cdr groups))
      (fancy-fill-paragraph--fill-lines-plain lines local-fill-column first-line-extra-width))
     (t
      (let ((result nil)
            (extra first-line-extra-width))
        (dolist (group groups)
          (let* ((indent (car group))
                 (group-lines (cdr group))
                 (indent-str (make-string indent ?\s))
                 (sub-fill-column (max 1 (- local-fill-column indent)))
                 (filled
                  (fancy-fill-paragraph--fill-lines (mapcar
                                                     (lambda (line)
                                                       (fancy-fill-paragraph--strip-indent
                                                        line indent))
                                                     group-lines)
                                                    sub-fill-column dp-list
                                                    extra)))
            (setq extra nil)
            (setq result (nconc result (mapcar (lambda (line) (concat indent-str line)) filled)))))
        result)))))

(defun fancy-fill-paragraph--fill-lines
    (lines local-fill-column dp-list &optional first-line-extra-width)
  "Fill LINES within LOCAL-FILL-COLUMN, handling dot-points recursively.
LINES is a list of strings.  Returns a list of result line strings.
DP-LIST is a pre-built pattern list from
`fancy-fill-paragraph--compile-dot-point-patterns'.
When DP-LIST is non-nil, detects dot-point prefixes and fills each
item as a separate sub-paragraph.  Falls through to plain fill when
no dot-points are found.
FIRST-LINE-EXTRA-WIDTH, when non-nil, is how many columns the first
line takes beyond the others, it narrows that line only."
  (declare (important-return-value t))
  (cond
   ;; Dot-point mode disabled (dp-list is nil), skip dot-point scan.
   ((null dp-list)
    (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column nil
                                                   first-line-extra-width))
   (t
    (let ((sections (fancy-fill-paragraph--partition-dot-point-lines lines dp-list)))
      (cond
       ((null sections)
        (fancy-fill-paragraph--fill-lines-indent-split lines local-fill-column dp-list
                                                       first-line-extra-width))
       (t
        (let ((result nil)
              ;; Taken by whichever section is filled first.
              (extra first-line-extra-width)
              (min-indent (plist-get sections :min-indent))
              (preamble (plist-get sections :preamble))
              (groups (plist-get sections :groups))
              (trailing (plist-get sections :trailing)))
          (when preamble
            (setq result
                  (nconc
                   result
                   (fancy-fill-paragraph--fill-lines preamble local-fill-column dp-list extra)))
            (setq extra nil))
          (dolist (group groups)
            (setq result
                  (nconc
                   result
                   (fancy-fill-paragraph--fill-dot-point-group
                    group min-indent local-fill-column dp-list
                    extra)))
            (setq extra nil))
          (when trailing
            (setq result
                  (nconc
                   result
                   (fancy-fill-paragraph--fill-lines trailing local-fill-column dp-list extra))))
          result)))))))

(defun fancy-fill-paragraph--fill-region (beg end &optional first-line-extra-width)
  "Fill the paragraph in the region from BEG to END.
Detects the fill prefix, strips it, normalizes blank-space,
splits at delimiter boundaries, solves for optimal line breaks,
and re-adds the prefix to each output line.
FIRST-LINE-EXTRA-WIDTH, when non-nil, narrows the first line, see
`fancy-fill-paragraph--fill-lines'."
  (setq end (min end (point-max)))
  (let* ((dp-list (fancy-fill-paragraph--compile-dot-point-patterns))
         (prefix-info (fancy-fill-paragraph--analyze-fill-prefix beg end dp-list))
         (prefix (car prefix-info))
         (prefix-column-width (cadr prefix-info))
         (lines (fancy-fill-paragraph--strip-region-lines beg end prefix-column-width))
         ;; Fill using dot-point aware function.
         ;; Always pass dp-list so dot-points are automatically detected;
         ;; `--fill-lines' falls through to plain fill when none are found.
         (filled
          (fancy-fill-paragraph--fill-lines lines
                                            (max 1
                                                 (- fill-column
                                                    fancy-fill-paragraph-fill-column-margin
                                                    prefix-column-width))
                                            dp-list
                                            first-line-extra-width)))
    (when filled
      (fancy-fill-paragraph--replace-region
       beg end (mapconcat (lambda (line) (concat prefix line)) filled "\n")))))

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
      (let ((jobs (fancy-fill-paragraph--syntax-fill-jobs-for-selection beg end)))
        (cond
         (jobs
          (setq changed (fancy-fill-paragraph--syntax-fill-jobs-run jobs)))
         (t
          ;; Paragraphs holding code beside a comment are left alone, the
          ;; same refusal the cursor path makes.
          (setq changed
                (fancy-fill-paragraph--fill-paragraph-bounds
                 (fancy-fill-paragraph--drop-refused-paragraphs
                  (fancy-fill-paragraph--collect-paragraph-bounds beg end t))))))))
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
         (jobs (fancy-fill-paragraph--syntax-fill-jobs-in-range beg end (point))))
    (cond
     (jobs
      ;; Fill each region independently, in reverse order so
      ;; earlier positions remain valid after later fills.
      ;; Track point with a marker so it follows its text as the fill
      ;; inserts and removes line breaks before it, restored even when
      ;; a fill fails.
      (let ((saved-point (point-marker)))
        (unwind-protect
            (fancy-fill-paragraph--syntax-fill-jobs-run jobs t)
          (goto-char saved-point)
          (set-marker saved-point nil))))
     ;; Point is on code beside a comment or string, leave the buffer
     ;; alone rather than merge the code into it.
     ;;
     ;; NOTE: this walks BEG..END a second time, with its own notion of
     ;; where comments and strings begin and end.  Reusing the scan
     ;; `fancy-fill-paragraph--syntax-fill-jobs-in-range' already made
     ;; would seem better, however that scan returns nil for several
     ;; unrelated reasons and would have to report which one.  The cost
     ;; is one extra pass over a paragraph, so leave as-is.  Noting it
     ;; as a potential improvement.
     ((fancy-fill-paragraph--fill-region-refuse-p beg end)
      nil)
     (t
      (fancy-fill-paragraph--fill-region beg end)))))


;; ---------------------------------------------------------------------------
;; Public Functions

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
