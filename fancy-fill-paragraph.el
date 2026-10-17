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
Each comment, and each string where text beside it is code, is filled
as a separate paragraph, preventing `fancy-fill-paragraph' from merging
text across distinct comments or strings.  Text beside a string is code
in programming and configuration modes, and in other modes where a
double quote opens a string, other than prose modes such as `org-mode'.
Filling a paragraph that holds code beside a comment or string leaves
it alone rather than reflowing it as prose, which would merge the code
into the comment text.  In configuration modes a paragraph of code
alone (keys and section headers) is left alone too.  This reports no
changes, the same as an already filled paragraph.
A line holding a line comment is left as it is when the prose around
it is filled, a block comment (`<!-- -->' in `html-mode' for e.g.) is
filled with the prose, remaining a comment.  Comments are found from
the mode's syntax table, so a comment the syntax table doesn't mark
\(`..' in `rst-mode' for e.g.) is filled as prose."
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

(defsubst fancy-fill-paragraph--sentence-end-boundary-p (text split-pos)
  "Return non-nil when TEXT at SPLIT-POS follows sentence punctuation.
The punctuation may be followed by a closing quote, guillemet or
bracket (`.)' for e.g.), as in `sentence-end-base', any other character
after it makes it part of a word (`foo.c' for e.g.)."
  (declare (important-return-value t))
  (and (> split-pos 0)
       (or (memq (aref text (1- split-pos)) '(?. ?? ?! ?\u2026))
           (and (>= split-pos 2)
                (memq
                 (aref text (1- split-pos)) '(?\) ?\] ?\} ?\" ?' ?\u201D ?\u2019 ?\u00BB ?\u203A))
                (memq (aref text (- split-pos 2)) '(?. ?? ?! ?\u2026))))))

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

(defsubst fancy-fill-paragraph--maybe-double-space (sep text split-pos)
  "Upgrade SEP to double-space when TEXT at SPLIT-POS follows a sentence end.
When `sentence-end-double-space' is active and the
delimiter at SPLIT-POS follows sentence-ending punctuation (e.g. `.\\'',
`.)'), return double-space.  Exclude continuation punctuation (`,;:')
where the period is typically an abbreviation (e.g. `etc.,'), and a
dotted abbreviation (e.g. `U.S.'), which keeps a single space.
NOTE: a double space typed after a dotted abbreviation, ending a
sentence, is collapsed too.  Telling it apart took the width of every
space run in the source threaded through the fill, which isn't worth it
for such a rare case, so accept the limitation."
  (declare (important-return-value t))
  (cond
   ((and sentence-end-double-space
         (fancy-fill-paragraph--sentence-end-boundary-p text split-pos)
         (not (fancy-fill-paragraph--dotted-abbreviation-boundary-p text split-pos)))
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

(defsubst fancy-fill-paragraph--leading-indent-string (str)
  "Return the leading blank-space of STR."
  (declare (important-return-value t))
  (substring str 0 (fancy-fill-paragraph--leading-indent str)))

(defun fancy-fill-paragraph--string-column-width (str &optional col)
  "Return the column STR ends at when laid out from COL, zero by default.
Unlike `string-width', a tab advances to the next tab stop of the
current buffer's `tab-width', so spaces before a tab are not counted on
top of it."
  (declare (important-return-value t))
  (let ((col (or col 0))
        ;; NOTE: matches Emacs, laying out a tab as eight columns when
        ;; `tab-width' is out of range (see `sanitize_tab_width' in `buffer.h').
        (tab-cols
         (cond
          ((and (natnump tab-width) (<= 1 tab-width 1000))
           tab-width)
          (t
           8))))
    (dotimes (i (length str))
      (let ((ch (aref str i)))
        (setq col
              (cond
               ((eq ch ?\t)
                (* tab-cols (1+ (/ col tab-cols))))
               (t
                (+ col (char-width ch)))))))
    col))

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

(defsubst fancy-fill-paragraph--move-past-line-prefix
    (prefix-column-width cont-prefix &optional strip-mark)
  "Move point past the prefix on the line at point.
CONT-PREFIX, when non-nil, is the prefix the line is expected to carry.
A line without it holds body text where the prefix should be, cutting
PREFIX-COLUMN-WIDTH columns from that deletes the text, so only its
leading blank-space is skipped.  With STRIP-MARK, the prefix's mark is
skipped after it too, when followed by the prefix's blank-space or
nothing (` * ' a column off for e.g.), otherwise the mark would be taken
for text.
Pass it only for a synthesized prefix.  The comparison is literal, so a
prefix read from the buffer rejects lines that reach the same column
with different blank-space, tabs against spaces for instance."
  (cond
   ((or (null cont-prefix) (fancy-fill-paragraph--line-matches-prefix-p cont-prefix))
    (move-to-column prefix-column-width))
   (t
    (skip-chars-forward " \t" (pos-eol))
    (let* ((mark (string-trim cont-prefix))
           (mark-tail
            (substring cont-prefix
                       (+ (fancy-fill-paragraph--leading-indent cont-prefix) (length mark)))))
      (when (and strip-mark
                 (not (string-empty-p mark))
                 (looking-at
                  (concat (regexp-quote mark) "\\(?:" (regexp-quote mark-tail) "\\|$\\)")))
        (goto-char (match-end 0)))))))

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
  "Strip up to N leading blank characters from LINE.
A line indented less than N keeps its text."
  (declare (important-return-value t))
  (substring line (min n (fancy-fill-paragraph--leading-indent line))))

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
The quotes ending a repeated delimiter belong to its string, see
`fancy-fill-paragraph--string-delimiter-length'."
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
      (or (and start (fancy-fill-paragraph--string-closer-run-owner start)) start))))

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

(defsubst fancy-fill-paragraph--char-skip-set (char)
  "Return a `skip-chars-forward' set matching CHAR alone.
CHAR is quoted, a `^' would negate the set and a `-' make a range."
  (declare (important-return-value t))
  (concat "\\" (string char)))

(defun fancy-fill-paragraph--sexp-end (pos)
  "Return the end of the sexp at POS, or nil when it never closes."
  (declare (important-return-value t))
  (condition-case nil
      (scan-sexps pos 1)
    (scan-error
     nil)))

(defun fancy-fill-paragraph--string-end (opener-pos)
  "Return the position after the closer of the string opening at OPENER-POS.
Returns nil when the string never closes.  The closer is as long as the
delimiter, see `fancy-fill-paragraph--string-delimiter-length'."
  (declare (important-return-value t))
  (let ((end (fancy-fill-paragraph--sexp-end opener-pos)))
    (and end (+ (1- end) (fancy-fill-paragraph--string-delimiter-length opener-pos end)))))

(defun fancy-fill-paragraph--string-delimiter-length (opener-pos end)
  "Return the length of the delimiter of the string opening at OPENER-POS.
END is the end of the string's sexp, nil when it never closes.  The
delimiter is the run of its character ending at OPENER-POS, as long as
the run starting at the closing delimiter, the shorter of the two when
they differ.  A string that never closes, or closes with another
character, has a delimiter of one.
NOTE: syntax finds where a string is, the text how long its delimiter
is.  A mode may give string syntax to one character of a longer
delimiter, the rest reading as empty strings of their own
\(`python-mode' gives it to one quote of each triple quote for e.g.).
The shorter run is taken so an empty string glued to a string opened
with one quote isn't part of its delimiter (shell `\"\"\"aaa\"' for e.g.).
A limitation: equal runs are taken for a repeated delimiter in any
mode, although shell `\"\"\"aaa\"\"\"' is three strings, so its line may
wrap.  Telling them apart takes the language's rules, so accept it."
  (declare (important-return-value t))
  (let ((chars (fancy-fill-paragraph--char-skip-set (char-after opener-pos))))
    (cond
     ((and end (eq (char-before end) (char-after opener-pos)))
      (save-excursion
        (min (progn
               (goto-char (1+ opener-pos))
               (- (point)
                  (progn
                    (skip-chars-backward chars (pos-bol))
                    (point))))
             (progn
               (goto-char (1- end))
               (skip-chars-forward chars (pos-eol))
               (- (point) (1- end))))))
     (t
      1))))

(defun fancy-fill-paragraph--string-closer-run-owner (pos)
  "Return the start of the string whose closer holds the string at POS.
Returns nil when POS does not follow the start of a closing delimiter
in the same run, see `fancy-fill-paragraph--string-delimiter-length'.
The run before POS is read from its start, the first string open before
one of its characters and closing past POS owning it.  An empty string's
opener and closer are one run (`\"\"\"\"\"\"' for e.g.), so the run
may start before the string."
  (declare (important-return-value t))
  (let ((q
         (save-excursion
           (goto-char pos)
           (skip-chars-backward (fancy-fill-paragraph--char-skip-set (char-after pos)) (pos-bol))
           (point)))
        (owner nil))
    (while (and (null owner) (< q pos))
      (let* ((ppss (save-excursion (syntax-ppss q)))
             ;; Only a string owns them.  A comment ending just before them (a
             ;; shebang line above a module doc-string for e.g.) is skipped by
             ;; the scan for its end, which then ends past the quotes too.
             (start (and (nth 3 ppss) (nth 8 ppss)))
             (start-end (and start (fancy-fill-paragraph--string-end start))))
        (when (and start-end (< pos start-end))
          (setq owner start)))
      (setq q (1+ q)))
    owner))

(defun fancy-fill-paragraph--string-opener-run-start (pos)
  "Return the start of the string whose opener holds the string at POS.
Returns nil when POS does not open a run whose last character starts a
string with a delimiter reaching back to POS, see
`fancy-fill-paragraph--string-delimiter-length'."
  (declare (important-return-value t))
  (let ((last
         (save-excursion
           (goto-char pos)
           (skip-chars-forward (fancy-fill-paragraph--char-skip-set (char-after pos)) (pos-eol))
           (1- (point)))))
    (and (< pos last)
         (let ((ppss (save-excursion (syntax-ppss (1+ last)))))
           (and (nth 3 ppss) (eq (nth 8 ppss) last)))
         (let ((end (fancy-fill-paragraph--sexp-end last)))
           (and end
                (<= (- (1+ last) (fancy-fill-paragraph--string-delimiter-length last end)) pos)))
         last)))

(defsubst fancy-fill-paragraph--syntax-comment-end-class-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR indicates a `comment-end' character.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       ;; Class 12: comment-end (e.g. newline in Lisp).
       ;; Bit 19: second character of a two-character comment ender (e.g. / in */).
       (or (= (syntax-class syn-descriptor) 12)
           (not (zerop (logand (car syn-descriptor) (ash 1 19)))))))

(defsubst fancy-fill-paragraph--syntax-comment-closer-char-p (syn-descriptor)
  "Return non-nil if SYN-DESCRIPTOR indicates a character of a comment closer.
A `comment-end' character, see
`fancy-fill-paragraph--syntax-comment-end-class-p', a comment fence, or
the first character of a two-character comment ender.
SYN-DESCRIPTOR may be nil, in which case nil is returned."
  (declare (important-return-value t))
  (and syn-descriptor
       (or (fancy-fill-paragraph--syntax-comment-end-class-p syn-descriptor)
           ;; Class 14: comment-fence.
           (= (syntax-class syn-descriptor) 14)
           ;; Bit 18: first character of a two-character comment ender (e.g. * in */).
           (not (zerop (logand (car syn-descriptor) (ash 1 18)))))))

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

(defconst fancy-fill-paragraph--fill-variables-builtin
  '(sentence-end-double-space tab-width case-fold-search)
  "Variables outside this package read while filling.
See `fancy-fill-paragraph--fill-local-variables'.")

(defun fancy-fill-paragraph--fill-local-variables ()
  "Return the current buffer's fill settings as (VARIABLE . VALUE) pairs.
These are `fancy-fill-paragraph--fill-variables-builtin', and each of
this package's options local to the buffer.
NOTE: options are read from the package's custom group rather than
listed, so one added later can't be missed, using its default value in
the buffer text is filled in, see
`fancy-fill-paragraph--fill-paragraphs-in-string'.  Reading the
buffer's local variables for them scanned every one of them, for each
paragraph filled."
  (declare (important-return-value t))
  (let ((result
         (mapcar
          (lambda (var) (cons var (symbol-value var)))
          fancy-fill-paragraph--fill-variables-builtin)))
    (dolist (member (get 'fancy-fill-paragraph 'custom-group))
      (when (and (eq (cadr member) 'custom-variable) (local-variable-p (car member)))
        (push (cons (car member) (symbol-value (car member))) result)))
    result))

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
    (let ((bindings (fancy-fill-paragraph--fill-local-variables)))
      (with-temp-buffer
        ;; NOTE: a temporary buffer sees only the default values, the filled
        ;; buffer's own (`sentence-end-double-space' set by a mode hook for
        ;; e.g.) are carried over.  Otherwise text filled here would follow
        ;; other settings than text filled in its buffer, or a closer after it.
        (dolist (binding bindings)
          (set (make-local-variable (car binding)) (cdr binding)))
        (insert text)
        (let* ((fill-column local-fill-column)
               (fill-prefix "")
               ;; Reverse ordered, so the opening paragraph is last.
               (bounds (fancy-fill-paragraph--collect-paragraph-bounds (point-min) (point-max))))
          (fancy-fill-paragraph--fill-paragraph-bounds (butlast bounds))
          ;; The text is not blank, so there is an opening paragraph.
          (let ((para (car (last bounds))))
            (fancy-fill-paragraph--fill-region (car para) (cdr para) first-line-extra-width)))
        (buffer-string))))))

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

(defun fancy-fill-paragraph--line-paragraph-bounds ()
  "Return (BEG . END) of the paragraph holding the line at point.
A comment line separating paragraphs is its own paragraph (a LaTeX
comment or a bare \" *\" for e.g.), see `paragraph-separate'.
NOTE: other lines separating paragraphs are left to
`fancy-fill-paragraph--paragraph-bounds', which gives a neighboring
paragraph, as the cursor on them fills it too (a heading or a blank line
for e.g.).  On a comment line the cursor fills the comment instead."
  (declare (important-return-value t))
  (cond
   ((and fancy-fill-paragraph-syntax-bounds
         (save-excursion
           (goto-char (pos-bol))
           (and (not (looking-at-p "[ \t]*$")) (looking-at-p paragraph-separate)))
         (fancy-fill-paragraph--comment-or-string-start-at
          (fancy-fill-paragraph--line-last-non-blank (point))))
    (cons (pos-bol) (pos-eol)))
   (t
    (fancy-fill-paragraph--paragraph-bounds))))

(defun fancy-fill-paragraph--comment-or-string-body-bounds (beg end)
  "Return inner body bounds between delimiter lines, or nil.
BEG and END are the full region bounds (from `pos-bol' of the opener
to `pos-eol' of the closer).  Returns (BODY-BEG . BODY-END) spanning
from the first line after the opener to the last line before the closer,
or nil when there are no lines between them (the closer on the opener
line or the next).  A lone empty line between them counts, BODY-BEG then
equals BODY-END.
Note: this returns the raw inner body regardless of whether the opener
line carries text.  Callers that need to include opener-line text in
paragraph scanning should widen the returned bounds themselves."
  (declare (important-return-value t))
  (save-excursion
    ;; End of the last body line (line before the closer).
    (goto-char end)
    (let ((body-end (pos-eol 0)))
      (goto-char beg)
      ;; An empty line ends where it starts, so equal bounds count too.
      (when (and (zerop (forward-line 1)) (>= body-end (point)))
        (cons (point) body-end)))))

(defun fancy-fill-paragraph--code-after-closer-p (closer-end)
  "Return non-nil when code follows CLOSER-END on its line.
CLOSER-END is the position just after a comment or string closing
delimiter.  Trailing code on the closer line must never be reflowed
as comment or string text, so callers skip inline filling for such
regions.
NOTE: closing delimiters and punctuation glued to the closer, read from
the syntax table (`\")' ending a Lisp doc-string, `\"\"\"),' or `*/);'
for e.g.), aren't counted.  They end the expression holding the string
or comment and are filled with the closer as one word, never apart from
it, so no code is merged into the text."
  (declare (important-return-value t))
  (save-excursion
    (goto-char closer-end)
    (skip-syntax-forward ")." (pos-eol))
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
    (cond
     ((forward-comment 1)
      (not (fancy-fill-paragraph--syntax-comment-end-p)))
     ;; A line comment ending the buffer without a newline never closes,
     ;; `forward-comment' fails on it as it does on an unclosed block comment.
     ((eobp)
      (goto-char syn-start)
      (fancy-fill-paragraph--line-comment-opener-at-point-p))
     (t
      nil))))

(defun fancy-fill-paragraph--syntax-fill-job-at (syn-start &optional sel-bounds search-bounds)
  "Return a syntax fill job for the region starting at SYN-START.
When SEL-BOUNDS is non-nil, include its bounds so only overlapping
paragraphs are filled by the job runner.
SEARCH-BOUNDS limits line-comment block expansion."
  (declare (important-return-value t))
  ;; The quotes before a repeated delimiter's last open the string after them.
  ;; NOTE: mapped here rather than in
  ;; `fancy-fill-paragraph--comment-or-string-start-at' (as the closer's
  ;; quotes are), which returns a start at or before its position.  This
  ;; start follows it, the region scan would take the quotes for code.
  (setq syn-start (or (fancy-fill-paragraph--string-opener-run-start syn-start) syn-start))
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
       ;; NOTE: in a prose mode a string is quoted prose (a double quote has
       ;; string syntax in `org-mode' for e.g.), with no code around it, so
       ;; it's filled with its paragraph.
       ((progn
          (goto-char syn-start)
          (and (fancy-fill-paragraph--syntax-string-delimiter-p (syntax-after (point)))
               (fancy-fill-paragraph--text-beside-regions-is-code-p)))
        (let ((string-end (fancy-fill-paragraph--string-end syn-start)))
          (when string-end
            (goto-char string-end)
            (fancy-fill-paragraph--syntax-fill-job-create
             :beg region-beg
             :end (pos-eol)
             :fill-fn #'fancy-fill-paragraph--fill-string-region
             :args (list syn-start sel-beg sel-end)
             :resume (point)))))
       ;; Block comment that never closes (one being typed for e.g.), it runs
       ;; to the end of the paragraph searched.
       ((and (eq syn-start (fancy-fill-paragraph--comment-or-string-start-at (1+ syn-start)))
             (equal (fancy-fill-paragraph--syntax-region-at syn-start) '(comment)))
        (let ((end (or (cdr search-bounds) (point-max))))
          (fancy-fill-paragraph--syntax-fill-job-create
           :beg region-beg
           :end end
           :fill-fn #'fancy-fill-paragraph--fill-block-comment-region
           :args (list syn-start sel-beg sel-end)
           :resume end)))
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

(defun fancy-fill-paragraph--syntax-fill-jobs-in-range (beg end pos &optional sel-bounds)
  "Return syntax fill jobs for comment or string regions in BEG..END.
POS is the cursor position, used to detect when point is inside a
comment or string whose opener precedes BEG.
SEL-BOUNDS, when non-nil, restricts the jobs to the selection, see
`fancy-fill-paragraph--syntax-fill-job-at'.
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
                   (let ((last (fancy-fill-paragraph--line-last-non-blank pos)))
                     (or (fancy-fill-paragraph--comment-or-string-start-at last)
                         ;; NOTE: closing delimiters after a closer aren't
                         ;; code, see `fancy-fill-paragraph--code-after-closer-p',
                         ;; so the line is probed before them too (`\")' ending a
                         ;; Lisp doc-string for e.g.).
                         (fancy-fill-paragraph--comment-or-string-start-at
                          (save-excursion
                            (goto-char (1+ last))
                            (skip-syntax-backward ")." (pos-bol))
                            (max (pos-bol) (1- (point))))))))))
          (when syn-start
            (goto-char syn-start)))
        (while (< (point) end)
          ;; Skip blank-space between regions.
          (skip-chars-forward " \t\n" end)
          (when (< (point) end)
            (let ((job
                   (fancy-fill-paragraph--syntax-fill-job-at (point) sel-bounds (cons beg end))))
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
in shell text opens a string that runs to the end of the buffer.  The
end of the buffer closes a line comment, as a newline would.
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
      ;; `forward-comment' fails on a comment that is never closed, and
      ;; on a line comment ending the buffer without a newline.
      (cons
       'comment
       (and (fancy-fill-paragraph--line-comment-syntax-start-p syn-start) (point-max)))))))

(defun fancy-fill-paragraph--range-syntax-regions (beg end)
  "Return whether BEG..END holds comments or strings, and code beside them.
The result is (HAS-REGION . HAS-CODE).  HAS-REGION is non-nil when a
comment or string lies in the range, BEG inside one counting too.
HAS-CODE is non-nil when non-blank text lies outside them.
A string that never closes is not counted, an apostrophe in shell text
opens one.  A comment counts whether or not it closes.  An empty or
reversed range holds nothing.  The scan stops once both are found."
  (declare (important-return-value t))
  (save-excursion
    (syntax-propertize end)
    (let ((has-code nil)
          (has-region nil))
      (goto-char beg)
      ;; An empty or reversed range never enters the loop.
      (while (and (< (point) end) (not (and has-region has-code)))
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
                (setq has-region t))
              ;; An unclosed region runs to the end of the buffer.  A
              ;; closed one always resumes past FROM, which is inside it,
              ;; so the scan cannot retreat.
              (goto-char (or resume end))))
           (t
            (goto-char end)))))
      (cons has-region has-code))))

(defun fancy-fill-paragraph--text-beside-regions-is-code-p ()
  "Return non-nil when text beside comments and strings is code.
It's code in programming and configuration modes, and in other modes
where a double quote opens a string, unless the mode's text is prose
(`org-mode' for e.g.), so quoted prose would otherwise be taken for
code.  Where a double quote is punctuation (`latex-mode' for e.g.) the
text beside a comment is prose, taken for code a paragraph with a
trailing comment would never fill.
NOTE: prose modes are listed rather than data modes, a data mode can't
be told from prose by its parents (`yaml-ts-mode' derives from
`text-mode' for e.g.), however its double quote opens a string.  Taken
for prose, keys beside a quoted value are merged into one line, taken
for code, prose beside a quote or comment is left as it is.  So an
unlisted mode with strings is taken for code, the lesser harm."
  (declare (important-return-value t))
  (or (derived-mode-p 'prog-mode 'conf-mode)
      (and (eq (char-syntax ?\") ?\")
           (not (derived-mode-p 'org-mode 'sgml-mode 'nroff-mode 'fundamental-mode)))))

(defun fancy-fill-paragraph--text-outside-regions-is-code-p ()
  "Return non-nil when all text outside comments and strings is code.
This is limited to configuration modes, where such text is keys and
section headers, never prose.
NOTE: a programming mode may hold prose outside its comments (a buffer
of notes in `c-mode' for e.g.), filling a paragraph without a comment
or string there falls back to a prose fill, so it isn't included."
  (declare (important-return-value t))
  (derived-mode-p 'conf-mode))

(defun fancy-fill-paragraph--fill-region-refuse-p (beg end)
  "Return non-nil when a prose fill of BEG..END must be refused.
Code beside a comment or string would merge into its text, see
`fancy-fill-paragraph--text-beside-regions-is-code-p'.  Code alone is
refused too where no text outside them is prose, see
`fancy-fill-paragraph--text-outside-regions-is-code-p'.
Returns nil both where the range is safe and where the check does not
apply."
  (declare (important-return-value t))
  (and fancy-fill-paragraph-syntax-bounds
       (let ((regions (fancy-fill-paragraph--range-syntax-regions beg end)))
         (and (cdr regions)
              (or (and (car regions) (fancy-fill-paragraph--text-beside-regions-is-code-p))
                  (fancy-fill-paragraph--text-outside-regions-is-code-p))
              t))))

(defun fancy-fill-paragraph--fill-prose-region (beg end)
  "Fill BEG..END as prose, a line holding a line comment left as it is.
NOTE: a line comment ends with its line, so text joined after it would
be commented out, and text it wrapped onto the next line uncommented
\(prose beside a `%' comment in `latex-mode' for e.g.).  The lines
between such lines are filled on their own, so a short line before or
after one isn't joined to it.  A block comment ends with its closer,
so it's filled with the prose, remaining a comment.  Wrapping the text
before the comment would take the comment as one word, which the fill
can't yet do, so leave as-is.
Return non-nil when the buffer was modified."
  (let ((runs nil)
        (run-beg nil)
        (done nil)
        (changed nil))
    (save-excursion
      (goto-char beg)
      (while (not done)
        (cond
         ((and fancy-fill-paragraph-syntax-bounds
               (let ((ppss (save-excursion (syntax-ppss (pos-eol)))))
                 (and (nth 4 ppss)
                      (<= (pos-bol) (nth 8 ppss))
                      (fancy-fill-paragraph--line-comment-syntax-start-p (nth 8 ppss)))))
          (when run-beg
            (push (cons run-beg (1- (pos-bol))) runs)
            (setq run-beg nil)))
         ((null run-beg)
          (setq run-beg (max beg (pos-bol)))))
        (setq done (or (>= (pos-eol) end) (not (zerop (forward-line 1))))))
      (when run-beg
        (push (cons run-beg end) runs)))
    ;; Pushed first to last, so they fill last to first and earlier
    ;; positions remain valid.
    (dolist (run runs)
      (when (fancy-fill-paragraph--fill-region (car run) (cdr run))
        (setq changed t)))
    changed))

(defun fancy-fill-paragraph--fill-selection-paragraphs (beg end)
  "Fill the paragraphs the selection BEG..END touches, as the cursor would.
The selection is taken to cover whole paragraphs, each line filled as
with the cursor on it, see `fancy-fill-paragraph--by-point-impl', last
to first, each found by a marker.  A comment or string fills the body
paragraphs within those paragraphs at once, its other lines then left,
as is a paragraph without one after filling it.
NOTE: filling by paragraph rather than by line would be quicker, however
a paragraph's lines may fill differently (code before a doc-string for
e.g.).  Restricting the fill to the selection took rules of its own,
filling unlike the cursor, so whole paragraphs are filled.
Return non-nil when the buffer was modified."
  (let ((lines nil)
        (sel-beg (make-marker))
        (sel-end (make-marker))
        (changed nil))
    (save-excursion
      ;; Widen the selection to the paragraphs it touches.
      (goto-char beg)
      (skip-chars-forward " \t\n" end)
      (set-marker sel-beg (car (fancy-fill-paragraph--line-paragraph-bounds)))
      (goto-char end)
      (skip-chars-backward " \t\n" beg)
      (set-marker sel-end (max sel-beg (cdr (fancy-fill-paragraph--line-paragraph-bounds))))
      (goto-char sel-beg)
      (while (and (not (eobp)) (<= (point) sel-end))
        (unless (looking-at-p "[ \t]*$")
          (push (point-marker) lines))
        (forward-line 1))
      ;; Pushed first to last, so they fill last to first.
      (unwind-protect
          (let ((skip-beg nil))
            (dolist (line lines)
              (unless (and skip-beg (<= skip-beg line))
                (goto-char line)
                (let* ((pos (point))
                       (bounds (fancy-fill-paragraph--paragraph-bounds))
                       ;; Its fill may move the paragraph's end, past the end
                       ;; of the buffer for e.g., a marker follows it.
                       (para-end (copy-marker (cdr bounds)))
                       (result
                        (fancy-fill-paragraph--fill-at-point
                         (cons (marker-position sel-beg) (marker-position sel-end))))
                       (own-beg nil))
                  (when (car result)
                    (setq changed t))
                  ;; The job holding the line, read before its fill.
                  (dolist (job (cdr result))
                    (when (<= (fancy-fill-paragraph--syntax-fill-job-beg job)
                              pos
                              (fancy-fill-paragraph--syntax-fill-job-end job))
                      (setq own-beg (fancy-fill-paragraph--syntax-fill-job-beg job))))
                  (setq skip-beg
                        (cond
                         ;; Its comment or string is filled for the whole selection.
                         (own-beg
                          own-beg)
                         ;; It fills the same from each of its lines.
                         ((not
                           (and fancy-fill-paragraph-syntax-bounds
                                (car
                                 (fancy-fill-paragraph--range-syntax-regions
                                  (car bounds) para-end))))
                          (car bounds))
                         (t
                          nil)))
                  (set-marker para-end nil)))))
        (dolist (line lines)
          (set-marker line nil))
        (set-marker sel-beg nil)
        (set-marker sel-end nil)))
    changed))

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
    (beg body closer-bol opener-col comment-start-pos)
  "Return the full continuation prefix for a block comment.
BEG is the region start, BODY is the body bounds, the closer line's for
a comment on two lines, or nil on one line.  CLOSER-BOL starts the closer
line or nil, OPENER-COL is the opener column, and COMMENT-START-POS is
the comment opener position.
The indentation is read from the first non-blank body line, the closer
line when the body lines are all blank, the opener line for a comment
on one line.
NOTE: the opener line's indentation is only its leading blank-space,
padded with spaces to OPENER-COL.  With code before the opener, a line
indented with tabs isn't laid out that way, so its prefix wouldn't match.
So any later line is preferred to it."
  (declare (important-return-value t))
  (concat
   (fancy-fill-paragraph--extract-body-indent
    (cond
     ;; A body implies a closer line, checking both is paranoid.
     ((and body closer-bol)
      (save-excursion
        (goto-char (car body))
        ;; Blank lines hold no indentation to read.
        (while (and (< (point) closer-bol) (looking-at-p "[ \t]*$"))
          (forward-line 1))
        (point)))
     (t
      beg))
    opener-col)
   (fancy-fill-paragraph--block-comment-continuation-suffix comment-start-pos)))

(defun fancy-fill-paragraph--block-comment-paragraph-prefixed-p (para-beg para-end cont-prefix)
  "Return non-nil when a line of the paragraph PARA-BEG..PARA-END has CONT-PREFIX.
Other lines lose only their blank-space when the prefix is stripped.
NOTE: a line with the prefix anywhere counts, the first line too, filled
as prose such a paragraph lost the first line's indentation, a second
fill taking it for a paragraph separator."
  (declare (important-return-value t))
  (save-excursion
    (goto-char para-beg)
    (let ((prefixed nil))
      ;; The last line may end the buffer, `forward-line' then stays on it.
      (while (and (not prefixed) (<= (point) para-end) (not (eobp)))
        (setq prefixed (fancy-fill-paragraph--line-matches-prefix-p cont-prefix))
        (forward-line 1))
      prefixed)))

(defsubst fancy-fill-paragraph--mark-run-regexp (mark)
  "Return a regexp matching a line of MARK repeated, nothing else, or nil.
MARK is a prefix's non-blank text (`*' of ` * ' for e.g.), such a line
being a banner (` ****' for e.g.).  An empty MARK has no regexp."
  (declare (important-return-value t))
  (and (not (string-empty-p mark))
       (concat "[ \t]*\\(?:" (regexp-quote mark) "\\)\\{2,\\}[ \t]*$")))

(defun fancy-fill-paragraph--body-paragraph-bounds
    (body-beg body-end prefix-col sel-beg sel-end &optional cont-prefix)
  "Return paragraph bounds in BODY-BEG..BODY-END overlapping SEL-BEG..SEL-END.
Paragraphs are separated by lines blank after PREFIX-COL columns, and
by lines of CONT-PREFIX's mark repeated, when non-nil, see
`fancy-fill-paragraph--mark-run-regexp'.
SEL-BEG and SEL-END are clamped to the body range so callers may pass
a cursor position outside the body (e.g. on a delimiter line).
Returns a list of (PARA-BEG . PARA-END) in reverse order so callers
can fill later paragraphs first without invalidating earlier positions.
NOTE: a banner line has no text to fill, taken for text it was given
the prefix (` * ****' for e.g.), so it's left as it is."
  (declare (important-return-value t))
  (save-excursion
    ;; A BODY-END past the buffer never reaches the last line's end, so the
    ;; scan would never stop.  Unlikely, it means a caller's bounds are out
    ;; of date, but avoid hanging (paranoid).
    (setq body-end (min body-end (point-max)))
    ;; Clamp selection to body so positions on delimiters still match.
    (setq sel-beg (min (max sel-beg body-beg) body-end))
    (setq sel-end (max (min sel-end body-end) sel-beg))
    (goto-char body-beg)
    (let ((result nil)
          (para-beg nil)
          (para-end nil)
          (done nil)
          (mark-run-re (fancy-fill-paragraph--mark-run-regexp (string-trim (or cont-prefix "")))))
      (while (and (not done) (<= (point) body-end))
        (let ((separator-p
               (or (save-excursion
                     (move-to-column prefix-col)
                     (looking-at-p "[ \t]*$"))
                   (and mark-run-re
                        (save-excursion
                          (goto-char (pos-bol))
                          (looking-at-p mark-run-re))))))
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

(defun fancy-fill-paragraph--closer-delimiter-beg (closer-end limit)
  "Return the start of the closer ending at CLOSER-END, not before LIMIT.
The closer's characters are read from the syntax table, so no comment
syntax is assumed: string delimiters, comment enders and fences, or the
characters of a two character comment ender (`*/' for e.g.).
NOTE: a mode may give comment syntax only to the closer's last character
\(the `>' of `-->' in `html-mode' for e.g.), so `comment-end' is matched
too, the longer of the two taken."
  (declare (important-return-value t))
  (save-excursion
    (goto-char closer-end)
    (while (and (> (point) limit)
                (let ((syn (syntax-after (1- (point)))))
                  (or (fancy-fill-paragraph--syntax-string-delimiter-p syn)
                      (fancy-fill-paragraph--syntax-comment-closer-char-p syn))))
      (backward-char))
    (let* ((end-str (string-trim (or comment-end "")))
           (end-beg (- closer-end (length end-str))))
      (when (and (not (string-empty-p end-str))
                 (< end-beg (point))
                 (<= limit end-beg)
                 (string-equal end-str (buffer-substring-no-properties end-beg closer-end)))
        (goto-char end-beg)))
    (point)))

(defun fancy-fill-paragraph--closer-word (closer-kind closer-end limit)
  "Return the closer ending at CLOSER-END when it's a word of its own, or nil.
CLOSER-KIND is how the closer line is filled, see
`fancy-fill-paragraph--closer-line-kind', only a closer joining the
last paragraph is returned.  LIMIT bounds the closer's start.  Closing
delimiters glued to the closer move with it as one word, see
`fancy-fill-paragraph--code-after-closer-p'.  Glued to text, the closer
is part of a word, filled as one.
NOTE: only a space separates it, as in the text, where a tab is part of
a word."
  (declare (important-return-value t))
  (and (eq closer-kind 'joins)
       closer-end
       (save-excursion
         (let ((closer-beg (fancy-fill-paragraph--closer-delimiter-beg closer-end limit)))
           (goto-char closer-end)
           (skip-syntax-forward ")." (pos-eol))
           (and (< closer-beg closer-end)
                (eq (char-before closer-beg) ?\s)
                (buffer-substring-no-properties closer-beg (point)))))))

(defun fancy-fill-paragraph--closer-alone-prefix (cont-prefix closer)
  "Return the prefix of a line holding CLOSER alone, after CONT-PREFIX lines.
When CLOSER starts with the last non-blank character of CONT-PREFIX,
CLOSER takes that character's place, lining up with the prefix (` */'
beside ` * ' for e.g.), otherwise it follows where the prefix ends.
Either way the prefix before CLOSER is blanked, its characters could be
taken for text (` *)' beside `|' in `pascal-mode', `   -->' beside
` !-- ' in `html-mode' for e.g.), keeping its indentation.
NOTE: no comment syntax is checked, only the prefix and the closer
sharing a character, so any comment style laid out this way is handled.
A prefix of blank-space alone (strings for e.g.) is returned as it is."
  (declare (important-return-value t))
  (let ((prefix-trim (string-trim-right cont-prefix)))
    (cond
     ((string-empty-p prefix-trim)
      cont-prefix)
     (t
      (let* ((prefix-before
              (cond
               ((eq (aref closer 0) (aref prefix-trim (1- (length prefix-trim))))
                (substring prefix-trim 0 -1))
               (t
                prefix-trim)))
             (indent (fancy-fill-paragraph--leading-indent-string prefix-before)))
        (concat
         indent
         (make-string
          (- (fancy-fill-paragraph--string-column-width prefix-before)
             (fancy-fill-paragraph--string-column-width indent))
          ?\s)))))))

(defun fancy-fill-paragraph--lines-split-closer (lines closer)
  "Return (LINES CLOSER) with CLOSER split off the last of LINES.
CLOSER is left out of the fill, then put back with
`fancy-fill-paragraph--append-closer'.  A last line holding only CLOSER
is dropped.  A nil CLOSER, or one not ending the last line, returns
\(LINES nil).
NOTE: filled as a word, the closer could be broken before to suit the
solver (after a sentence for e.g.) although it fit, then wrapped with
the continuation prefix (` * */').
NOTE: the closer was read from the buffer and the lines are its stripped
lines, so they agree.  Checked as removing it otherwise would drop text
\(paranoid)."
  (declare (important-return-value t))
  (let ((last-line (and closer lines (string-trim-right (car (last lines))))))
    (cond
     ((and last-line (string-suffix-p closer last-line))
      (let* ((text (substring last-line 0 (- (length last-line) (length closer))))
             (text-trim (string-trim-right text " +")))
        (cond
         ((fancy-fill-paragraph--string-blank-or-empty-p text)
          (list (butlast lines) closer))
         (t
          (list (append (butlast lines) (list text-trim)) closer)))))
     (t
      (list lines nil)))))

(defun fancy-fill-paragraph--append-closer (filled closer cont-prefix)
  "Return FILLED with CLOSER after its text, FILLED being prefixed lines or nil.
CLOSER follows the last line when it fits within `fill-column' less
`fancy-fill-paragraph-fill-column-margin', as the text does, otherwise
it's on a line of its own after CONT-PREFIX, a dot-point's hanging
indent isn't followed.  A nil CLOSER returns FILLED.
FILLED's lines start at column zero, as a fill replaces whole lines.
NOTE: without FILLED text nil is returned, there's nothing to fill.
Laying out CLOSER alone would replace a first line with it, deleting
the opener and any code before it (`int x; /* */' for e.g.)."
  (declare (important-return-value t))
  (cond
   ((or (null closer) (null filled))
    filled)
   (t
    (let* ((last-line
            (let ((last-line-beg (length filled)))
              ;; NOTE: searched back from the end, a regexp anchored to the end
              ;; would be tried from every position of FILLED.
              (while (and (> last-line-beg 0) (not (eq (aref filled (1- last-line-beg)) ?\n)))
                (setq last-line-beg (1- last-line-beg)))
              (substring filled last-line-beg)))
           (sep (fancy-fill-paragraph--maybe-double-space " " last-line (length last-line))))
      (cond
       ((<= (+ (fancy-fill-paragraph--string-column-width last-line) (length sep)
               (string-width closer))
            (- fill-column fancy-fill-paragraph-fill-column-margin))
        (concat filled sep closer))
       (t
        (concat
         filled "\n" (fancy-fill-paragraph--closer-alone-prefix cont-prefix closer) closer)))))))

(defun fancy-fill-paragraph--fill-prefixed-paragraph
    (lines replace-beg replace-end cont-prefix &optional first-prefix prefix-col closer)
  "Fill LINES with CONT-PREFIX and replace REPLACE-BEG..REPLACE-END.
FIRST-PREFIX, when non-nil, is used on the first output line instead
of CONT-PREFIX (e.g. for a comment opener like \"/* \").
PREFIX-COL, when non-nil, is the buffer-measured width of CONT-PREFIX.
CLOSER, when non-nil, is a comment or string closer ending LINES, held
out of the fill, see `fancy-fill-paragraph--lines-split-closer'.
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (pcase-let* ((`(,lines ,closer) (fancy-fill-paragraph--lines-split-closer lines closer))
               (prefix-col (or prefix-col (string-width cont-prefix)))
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
                 first-prefix first-line-extra-width))
               (result (fancy-fill-paragraph--append-closer result closer cont-prefix)))
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
    (body-beg body-end prefix-col sel-beg sel-end para-fill-fn &optional cont-prefix)
  "Fill paragraphs in BODY-BEG..BODY-END, calling PARA-FILL-FN for each.
PREFIX-COL is the prefix width for paragraph-boundary detection.
CONT-PREFIX, when non-nil, is the prefix whose mark separates
paragraphs too, see `fancy-fill-paragraph--body-paragraph-bounds'.
SEL-BEG and SEL-END restrict filling to overlapping paragraphs.
PARA-FILL-FN is called as (PARA-FILL-FN PARA-BEG PARA-END) and should
return non-nil when the buffer was modified.
Returns non-nil when any paragraph changed."
  (declare (important-return-value t))
  (let ((changed nil))
    (dolist (para
             (fancy-fill-paragraph--body-paragraph-bounds
              body-beg body-end prefix-col sel-beg sel-end
              cont-prefix))
      (when (funcall para-fill-fn (car para) (cdr para))
        (setq changed t)))
    changed))

(defun fancy-fill-paragraph--fill-prefixed-region
    (beg end cont-prefix &optional first-prefix first-line-text replace-beg prefix-col closer)
  "Strip lines from BEG..END, fill with CONT-PREFIX, and replace the region.
Lines are stripped of CONT-PREFIX width columns.  When FIRST-LINE-TEXT
is non-nil it is prepended as the first line (e.g. opener-line body text).
FIRST-PREFIX, when non-nil, is used on the first output line.
REPLACE-BEG, when non-nil, overrides BEG as the replacement start
\(e.g. to include the opener line in the replaced span).
PREFIX-COL, when non-nil, overrides the buffer-measured width of
CONT-PREFIX at BEG (e.g. line comments may strip fewer columns than
the literal prefix string occupies).
CLOSER is passed to `fancy-fill-paragraph--fill-prefixed-paragraph'.
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
                                                   prefix-col
                                                   closer)))

(defun fancy-fill-paragraph--string-continuation-prefix
    (body-beg closer-bol closer-kind &optional opener-hang opener-col)
  "Return the continuation prefix for a multi-line string with opener text.
BODY-BEG starts the first line after the opener line, CLOSER-BOL starts
the closer line, the same line when there are no lines between them.
Returns a blank-space string, the least indentation of the lines
continuing the opener line's paragraph.  OPENER-HANG is the width of a
dot-point marker starting the opener line's text, or nil, OPENER-COL
the opener line's indentation.  The lines continuing such a dot-point
hang past its marker, so its width is taken off their indentation,
unless that's less than OPENER-COL, the lines then being under the
marker.  When the opener line is a paragraph of its own (a summary line
for e.g.), the closer line's indentation is used when CLOSER-KIND is
`alone', the closer alone on its line (see
`fancy-fill-paragraph--closer-line-kind'), otherwise the least
indentation of the non-blank lines after it, the closer line when there
are none.  When CLOSER-KIND is `joins', the closer line's text is part
of its paragraph, so its indentation counts too.

NOTE: a closer alone on its line is preferred to the text, which could
be an indented example alone (a Python doc-test for e.g.), deeper than
the doc-string.  Otherwise the closer may be less indented than the text
\(`\"\"\")' after an argument for e.g.), so the text is used."
  (declare (important-return-value t))
  (save-excursion
    (goto-char body-beg)
    (let ((in-first-para t)
          (para-indent nil)
          (body-col nil)
          (body-indent nil)
          (closer-indent nil))
      (while (progn
               (skip-chars-forward " \t" (pos-eol))
               (cond
                ((= (pos-bol) closer-bol)
                 (setq closer-indent (buffer-substring-no-properties (pos-bol) (point)))
                 (when (and (eq closer-kind 'joins)
                            in-first-para
                            (or (null body-col) (< (current-column) body-col)))
                   (setq para-indent closer-indent)))
                ((eolp)
                 (setq in-first-para nil))
                ((or (null body-col) (< (current-column) body-col))
                 (setq body-col (current-column))
                 (setq body-indent (buffer-substring-no-properties (pos-bol) (point)))
                 ;; Only lines of the first paragraph were seen, so the
                 ;; least indentation of the body is that of the paragraph.
                 (when in-first-para
                   (setq para-indent body-indent))))
               (and (< (pos-bol) closer-bol) (zerop (forward-line 1)))))
      (cond
       ((and para-indent
             opener-hang (string-suffix-p (make-string opener-hang ?\s) para-indent)
             (<= (+ opener-col opener-hang)
                 (fancy-fill-paragraph--string-column-width para-indent)))
        (substring para-indent 0 (- opener-hang)))
       (para-indent
        para-indent)
       ((and (eq closer-kind 'alone) closer-indent)
        closer-indent)
       (t
        (or body-indent closer-indent ""))))))

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
     closer-line-end
     closer
     para-keep-fn)
  "Fill paragraphs in a multi-line syntax region, merging opener text.
BEG is `pos-bol' of the opener line.  INNER-BODY is (BODY-BEG . BODY-END)
for the lines between opener and closer.  CONT-PREFIX is the continuation
prefix for body lines.  OPENER-PREFIX is the first-line prefix when the
opener carries text (e.g. \"/* \"), or nil.  OPENER-TEXT is the body text
on the opener line (e.g. \"Only flush\"), or nil.
PARA-FILL-FN is called as (PARA-FILL-FN PARA-BEG PARA-END CLOSER) for
each non-opener paragraph, CLOSER being the closer for the paragraph
ending with it, otherwise nil.  SEL-BEG and SEL-END restrict filling to
overlapping paragraphs (callers should default to point when not
region-restricted).
CLOSER-LINE-END, when non-nil, is the end of a closer line carrying
text, which then joins the last paragraph.  CLOSER, when non-nil, is
the closer as a word of its own, held out of the last paragraph's fill,
see `fancy-fill-paragraph--closer-word'.
PARA-KEEP-FN, when non-nil, is called as (PARA-KEEP-FN PARA-BEG PARA-END)
for each paragraph, which is left as it is when it returns non-nil.
Returns non-nil when the buffer was modified."
  (declare (important-return-value t))
  (let* ( ;; NOTE: the prefix starts its line, as filled lines do, so it's
         ;; measured alone.  Measured on the first body line, a line not
         ;; starting with the prefix (blank-space or text indented with a
         ;; tab for e.g.) gave another width.
         (prefix-col (fancy-fill-paragraph--string-column-width cont-prefix))
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
     scan-beg
     scan-end
     prefix-col
     sel-beg
     sel-end
     (lambda (para-beg para-end)
       (let ((para-closer
              (and closer-line-end (= para-end scan-end) closer)))
         (cond
          ((and para-keep-fn (funcall para-keep-fn para-beg para-end))
           nil)
          ;; First paragraph includes the opener line: merge opener
          ;; text with body lines using the opener prefix for line 1.
          ((and opener-text (= para-beg beg))
           (fancy-fill-paragraph--fill-prefixed-region
            (car inner-body) para-end cont-prefix
            opener-prefix opener-text beg prefix-col para-closer))
          (t
           (funcall para-fill-fn para-beg para-end para-closer)))))
     cont-prefix)))

(defun fancy-fill-paragraph--fill-block-comment-body
    (body-beg body-end cont-prefix &optional closer)
  "Fill a separate-line block comment body from BODY-BEG to BODY-END.
CLOSER, when non-nil, is the comment closer ending the body, held out of
the fill, see `fancy-fill-paragraph--fill-prefixed-paragraph'.
A paragraph with a line starting with CONT-PREFIX is filled with it,
otherwise as prose."
  (cond
   ((fancy-fill-paragraph--block-comment-paragraph-prefixed-p body-beg body-end cont-prefix)
    (let ((prefix-col (fancy-fill-paragraph--string-column-width cont-prefix)))
      ;; NOTE: the prefix is matched literally, so a line without it loses
      ;; only its blank-space, as in the opener's paragraph.  Stripped of
      ;; the prefix's width, a line indented past it kept the rest, taken
      ;; for a deeper indented paragraph.
      (fancy-fill-paragraph--fill-prefixed-paragraph (fancy-fill-paragraph--strip-region-lines
                                                      body-beg
                                                      (min (1+ body-end) (point-max)) prefix-col
                                                      cont-prefix)
                                                     body-beg body-end cont-prefix
                                                     nil prefix-col closer)))
   (t
    ;; Prefix doesn't match body lines (e.g. XML comments),
    ;; fall back to standard fill.
    (fancy-fill-paragraph--fill-region body-beg body-end nil closer))))

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
    ;; Measured in columns, as PREFIX-COL is, counted in characters a line
    ;; indented with tabs was taken to be narrower.
    (and closer-bol
         (save-excursion
           (goto-char closer-bol)
           (end-of-line)
           (<= (current-column) prefix-col))
         closer-bol)))

(defun fancy-fill-paragraph--closer-line-kind (closer-bol closer-end cont-prefix)
  "Return how the closer line of a comment or string is filled.
CLOSER-BOL starts the closer line, nil when the closer is on the opener
line, see `fancy-fill-paragraph--closer-line-bol'.  CLOSER-END follows
the closer, nil for a string that never closes.  CONT-PREFIX is a block
comment's continuation prefix, nil for a string.
The result is one of:
- `blocked' Code follows the closer, the closer line is never filled.
- `alone'   Only the closer is on its line, the line stays as it is.
            For a comment, text glued to the closer is kept with it
            (`bbb.*/' for e.g.), the line's only non-blank run.
- `joins'   Text before the closer, joining the last paragraph.  A
            closer on the opener line joins too."
  (declare (important-return-value t))
  (cond
   ((or (null closer-end) (fancy-fill-paragraph--code-after-closer-p closer-end))
    'blocked)
   ((null closer-bol)
    'joins)
   ((save-excursion
      (goto-char closer-bol)
      (skip-chars-forward " \t" closer-end)
      (cond
       ;; A comment closer's characters have no common syntax to skip
       ;; (`*)' in `pascal-mode' ends with a close parenthesis for e.g.),
       ;; so the closer is taken to be the line's only non-blank run.
       (cont-prefix
        (skip-chars-forward "^ \t" closer-end))
       (t
        (skip-syntax-forward fancy-fill-paragraph--string-delimiter-syntax closer-end)))
      (= (point) closer-end))
    'alone)
   ;; NOTE: a paragraph is filled with the prefix stripped, lines without
   ;; it losing only their blank-space, or as prose, its least indentation
   ;; stripped, so text on the closer line joins either way.  A mark
   ;; without the prefix (` *ggg' for e.g.) is joined as text, telling a
   ;; misplaced mark from text isn't worth the rules it takes.
   (t
    'joins)))

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
    (beg end comment-start-pos cont-prefix opener-col closer-kind &optional closer)
  "Fill an inline block comment in BEG..END using CONT-PREFIX.
OPENER-COL is the column where content begins after the opener delimiter.
CLOSER-KIND is how the closer line is filled, see
`fancy-fill-paragraph--closer-line-kind', a closer alone on its line
stays as it is.  CLOSER, when non-nil, is the comment's closer as a word
of its own, held out of the fill, see
`fancy-fill-paragraph--lines-split-closer'."
  ;; Closing delimiters glued to the closer may follow it on its line, see
  ;; `fancy-fill-paragraph--code-after-closer-p'.  A comment that never
  ;; closes runs to END.
  (unless (let ((closer-end
                 (save-excursion
                   (goto-char comment-start-pos)
                   (and (forward-comment 1) (point)))))
            (or (null closer-end)
                (and (<= closer-end end) (< (fancy-fill-paragraph--last-bol end) closer-end))))
    (error "Expected comment at %d to close on line ending at %d" comment-start-pos end))
  (let* ( ;; NOTE: the prefix starts its line, as filled lines do, so it's
         ;; measured alone.  Measured over the opener line, tabs or wide
         ;; characters in the code before the opener gave it another width,
         ;; cutting into the text of the lines after it.
         (prefix-col (fancy-fill-paragraph--string-column-width cont-prefix))
         (opener-text
          (save-excursion
            (goto-char beg)
            (move-to-column opener-col)
            (buffer-substring-no-properties beg (point))))
         ;; NOTE: a closer alone on its line may be wider than the prefix
         ;; (` *)' beside `|' in `pascal-mode' for e.g.), filled as text it
         ;; would take the prefix, so it's kept too, as is a closer line
         ;; with code after it, the code is never reflowed.
         (verbatim-closer-bol
          (or (fancy-fill-paragraph--block-comment-inline-closer beg end prefix-col)
              (and (memq closer-kind '(alone blocked))
                   (fancy-fill-paragraph--closer-line-bol beg end)))))
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
     prefix-col
     ;; A verbatim closer line holds the closer, the lines before it don't.
     (and (null verbatim-closer-bol) closer))))

(defun fancy-fill-paragraph--fill-block-comment-region
    (beg end comment-start-pos &optional sel-beg sel-end)
  "Fill a block comment in the region BEG..END.
COMMENT-START-POS is the buffer position of the comment opener.
When SEL-BEG and SEL-END are non-nil, fill only body paragraphs
overlapping that selection.  Otherwise fill the paragraph at point.
When lines lie between the opener and closer lines, fills body
paragraphs, text on the opener line joining the first, as is the closer
line's text when the opener line holds only the opener.  Otherwise the
comment is inline and fills as one paragraph, the opener line keeping
its opener as prefix and later lines taking the continuation prefix.
Either way a closer alone on its line (` */'), or a closer line no
wider than the continuation prefix (` x */' beside ` * ' for e.g.),
stays as it is, and code after the closer is never reflowed.  A closer
joining the last paragraph is held out of the fill, put back on the
last line when it fits, otherwise on a line of its own.
Other text on the closer line joins the last paragraph, with the
continuation prefix (` * text */') or without it (`   text */')."
  (let ((pos (point)))
    (save-excursion
      (comment-normalize-vars)
      (let* ((opener-col
              (progn
                (goto-char comment-start-pos)
                (current-column)))
             (opener-info (fancy-fill-paragraph--block-comment-opener-info comment-start-pos))
             (inner-body (fancy-fill-paragraph--comment-or-string-body-bounds beg end))
             (closer-bol (fancy-fill-paragraph--closer-line-bol beg end))
             ;; NOTE: on two lines, the prefix is read from the closer line,
             ;; as a longer comment reads it from its first body line.  The
             ;; opener line's indentation is only its leading blank-space,
             ;; padded with spaces to the opener, so a closer line indented
             ;; with tabs after code (Linux kernel style for e.g.) didn't
             ;; match it, its ` *' joined as text.
             (closer-line-body (and (null inner-body) closer-bol (cons closer-bol end)))
             (cont-prefix
              (fancy-fill-paragraph--block-comment-continuation-prefix
               beg (or inner-body closer-line-body) closer-bol opener-col comment-start-pos))
             (closer-end
              (save-excursion
                (goto-char comment-start-pos)
                (forward-comment 1)
                (point)))
             (closer-kind
              (fancy-fill-paragraph--closer-line-kind closer-bol closer-end cont-prefix))
             ;; NOTE: on two lines, an opener line holding only the opener
             ;; leaves the closer line's text as the body, filled as the
             ;; body of a longer comment of the same shape.  Filled inline,
             ;; the opener line's code was taken for the first line's text,
             ;; the closer line's text was joined onto the opener.
             ;; Only a closer line joining the body has text to fill.
             (inner-body
              (or inner-body
                  (and (null (plist-get opener-info :text))
                       (eq closer-kind 'joins)
                       closer-line-body)))
             ;; A comment that never closes has no closer to hold out.
             (closer
              (fancy-fill-paragraph--closer-word
               closer-kind
               (save-excursion
                 (goto-char comment-start-pos)
                 (and (forward-comment 1) closer-end))
               beg)))
        (fancy-fill-paragraph--fill-syntax-region
         beg end inner-body
         (lambda ()
           (fancy-fill-paragraph--fill-multiline-syntax-body
            beg
            inner-body
            cont-prefix
            (plist-get opener-info :prefix)
            (plist-get opener-info :text)
            (lambda (para-beg para-end para-closer)
              (fancy-fill-paragraph--fill-block-comment-body para-beg para-end cont-prefix
                                                             para-closer))
            (or sel-beg pos)
            (or sel-end pos)
            (and (eq closer-kind 'joins) end)
            closer))
         (lambda ()
           (cond
            ;; Code after a closer on the opener line leaves no line to fill.
            ((and (eq closer-kind 'blocked) (null closer-bol))
             nil)
            (t
             (fancy-fill-paragraph--fill-block-comment-inline beg
                                                              end
                                                              comment-start-pos
                                                              cont-prefix
                                                              (plist-get opener-info :content-col)
                                                              closer-kind
                                                              closer)))))))))

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
           opener line contains only the delimiter.
- :repeated Non-nil when the delimiter is repeated, for e.g. Python's
           triple quotes, opening a string that may span lines, see
           `fancy-fill-paragraph--string-delimiter-length'."
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
      (let ((repeated
             (> (fancy-fill-paragraph--string-delimiter-length
                 (point)
                 (fancy-fill-paragraph--sexp-end (point)))
                1)))
        ;; The delimiter ends where its string starts (e.g. \"\"\", ''').
        (forward-char 1)
        (skip-chars-forward " \t" (pos-eol))
        (append (fancy-fill-paragraph--opener-prefix-and-text) (list :repeated repeated)))))))

(defun fancy-fill-paragraph--string-escaped-newline-p (beg end)
  "Return non-nil when a newline in the string BEG..END is escaped.
An escape is read from the syntax table, so a newline after an escaped
escape (`\\\\' for e.g.) isn't counted.
NOTE: only a newline after a character with escape or character quote
syntax can be escaped, so only those newlines are checked with
`syntax-ppss', rather than every newline of the string."
  (declare (important-return-value t))
  (save-excursion
    (goto-char beg)
    (let ((found nil))
      (while (and (null found) (re-search-forward "\\(?:\\s\\\\|\\s/\\)\n" end t))
        ;; NOTE: `syntax-ppss' may move point (in `python-mode' for e.g.),
        ;; finding the same newline again, so point is kept.
        (setq found (save-excursion (nth 5 (syntax-ppss (1- (point)))))))
      found)))

(defun fancy-fill-paragraph--fill-string-region (beg end opener-pos &optional sel-beg sel-end)
  "Fill a string in the region BEG..END.
OPENER-POS is the buffer position where the string sexp begins.
When SEL-BEG and SEL-END are non-nil, fill only body paragraphs
overlapping that selection.  Otherwise fill the paragraph at point.
When the delimiters occupy their own lines, fills only the body text
between them.  When the delimiters share lines with body text (inline
delimiters), fills the entire region, treating the delimiters as part
of the text.  Either way a closer on a line of its own stays there,
while text on the closer line joins the last paragraph.  A string on
one line is only filled when its delimiter is repeated, otherwise it's
left as it is, as is a paragraph continued with an escaped newline.
Without a line between the opener and closer lines, code before the
opener is kept on the first line, later lines continuing at the second
line's indentation, an opener line holding only the opener leaving the
closer line's text as the body."
  (let* ((inner-body (fancy-fill-paragraph--comment-or-string-body-bounds beg end))
         (opener-info (fancy-fill-paragraph--string-opener-info opener-pos))
         (closer-end (fancy-fill-paragraph--string-end opener-pos))
         (closer-bol (fancy-fill-paragraph--closer-line-bol beg end))
         (closer-kind (fancy-fill-paragraph--closer-line-kind closer-bol closer-end nil))
         (closer (fancy-fill-paragraph--closer-word closer-kind closer-end beg))
         ;; NOTE: on two lines, an opener line holding only the opener
         ;; leaves the closer line's text as the body, filled as the body
         ;; of a longer string of the same shape, as for a block comment.
         ;; Filled as prose, the closer line's text was left as it was or
         ;; joined onto the opener, the code before the opener broken.
         ;; Only a closer line joining the body has text to fill.
         (inner-body
          (or inner-body
              (and (null (plist-get opener-info :text))
                   (eq closer-kind 'joins)
                   closer-bol
                   (cons closer-bol end))))
         (cont-prefix
          (when (and inner-body (plist-get opener-info :text))
            (let ((dp-match
                   (fancy-fill-paragraph--line-dot-point-match
                    (plist-get opener-info :text)
                    (fancy-fill-paragraph--compile-dot-point-patterns))))
              (fancy-fill-paragraph--string-continuation-prefix
               (car inner-body) closer-bol closer-kind
               (and dp-match (string-width (cdr dp-match)))
               (fancy-fill-paragraph--string-column-width
                (fancy-fill-paragraph--leading-indent-string (plist-get opener-info :prefix))))))))
    (fancy-fill-paragraph--fill-syntax-region
     beg end inner-body
     (lambda ()
       (fancy-fill-paragraph--fill-multiline-syntax-body
        beg
        inner-body
        (or cont-prefix "")
        (plist-get opener-info :prefix)
        (plist-get opener-info :text)
        (lambda (para-beg para-end para-closer)
          (fancy-fill-paragraph--fill-region para-beg para-end nil para-closer))
        (or sel-beg (point))
        (or sel-end (point))
        (and (eq closer-kind 'joins) end)
        closer
        ;; NOTE: a paragraph continued with an escaped newline can't have
        ;; its lines broken or joined without changing the string, a raw
        ;; newline is an error in C and Python and a joined escape is
        ;; another character.  Other paragraphs are filled.  The newline
        ;; ending it is checked too, as the line after it may be left out
        ;; of the fill (a closer line with code after it for e.g.).
        (lambda (para-beg para-end)
          (fancy-fill-paragraph--string-escaped-newline-p
           para-beg (min (1+ para-end) (point-max))))))
     (lambda ()
       (cond
        ;; NOTE: only a repeated delimiter shows a string on one line may
        ;; span lines, breaking one opened with a single delimiter is an
        ;; error in C and Python and changes its value in shell.  Lisp
        ;; allows it, however the delimiter can't tell Lisp from C, so
        ;; leave it as it is.
        ((and (null closer-bol) (not (plist-get opener-info :repeated)))
         nil)
        ;; Code after the closer keeps the closer line as it is, only the
        ;; opener line's text is filled, so this needs a closer line, a
        ;; closer and text on the opener line.
        ((and (eq closer-kind 'blocked)
              (or (null closer-bol) (null closer-end) (null (plist-get opener-info :text))))
         nil)
        ;; Keep an escaped newline, as for the body's paragraphs above.
        ((fancy-fill-paragraph--string-escaped-newline-p beg (min (1+ end) (point-max)))
         nil)
        ;; NOTE: code before the opener is kept with it as the first line's
        ;; prefix, filled as prose it was reflowed with the text, an error in
        ;; Python.  Later lines continue at the second line's indentation,
        ;; the closer's when it's alone there, on one line at the opener
        ;; line's, the string has no other line to take it from.
        ((plist-get opener-info :text)
         (let* ((opener-prefix (plist-get opener-info :prefix))
                (opener-indent (fancy-fill-paragraph--leading-indent-string opener-prefix))
                (indent
                 (cond
                  (closer-bol
                   (save-excursion
                     (goto-char closer-bol)
                     (buffer-substring-no-properties
                      (point)
                      (progn
                        (skip-chars-forward " \t" (pos-eol))
                        (point)))))
                  (t
                   opener-indent)))
                ;; The second line of a dot-point hangs past its marker, the
                ;; fill hangs it again, so the marker's width is taken off,
                ;; unless the line is under the marker.
                (opener-hang
                 (let ((dp-match
                        (fancy-fill-paragraph--line-dot-point-match
                         (plist-get opener-info :text)
                         (fancy-fill-paragraph--compile-dot-point-patterns))))
                   (and dp-match (string-width (cdr dp-match)))))
                (indent
                 (cond
                  ((and closer-bol
                        (eq closer-kind 'joins)
                        opener-hang
                        (string-suffix-p (make-string opener-hang ?\s) indent)
                        (<= (+ (fancy-fill-paragraph--string-column-width opener-indent)
                               opener-hang)
                            (fancy-fill-paragraph--string-column-width indent)))
                   (substring indent 0 (- opener-hang)))
                  (t
                   indent)))
                ;; The second line is stripped of the indentation alone, a
                ;; dot-point's hang past it is kept for the fill.
                (lines
                 (cons
                  (plist-get opener-info :text)
                  (and closer-bol
                       (eq closer-kind 'joins)
                       (list
                        (save-excursion
                          (goto-char closer-bol)
                          (move-to-column (fancy-fill-paragraph--string-column-width indent))
                          (buffer-substring-no-properties (point) (pos-eol))))))))
           (fancy-fill-paragraph--fill-prefixed-paragraph
            lines beg
            (cond
             ((memq closer-kind '(alone blocked))
              (1- closer-bol))
             (t
              end))
            indent
            opener-prefix
            (fancy-fill-paragraph--string-column-width indent)
            closer)))
        ((eq closer-kind 'alone)
         (fancy-fill-paragraph--fill-region beg (1- closer-bol)))
        (t
         (fancy-fill-paragraph--fill-region beg end nil closer)))))))

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
does and when it may be passed.  A line's misplaced mark is stripped
only when another line carries the prefix, without one a mark is a
dot-point (`* item' in a comment laid out without the prefix for e.g.)."
  (declare (important-return-value t))
  (setq end (min end (point-max)))
  (save-excursion
    (let ((result nil)
          (strip-mark
           (and cont-prefix
                (save-excursion
                  (goto-char beg)
                  (let ((found nil))
                    (while (and (not found) (< (point) end))
                      (setq found (fancy-fill-paragraph--line-matches-prefix-p cont-prefix))
                      (forward-line 1))
                    found)))))
      (goto-char beg)
      (while (< (point) end)
        (fancy-fill-paragraph--move-past-line-prefix prefix-column-width cont-prefix strip-mark)
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
INDENT-WIDTH is the number of leading blank-space characters.
An empty match is no dot-point, a pattern that can match nothing (an
empty string or a user's `[0-9]*' for e.g.) would otherwise take every
line for one."
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
            (when (and (< 0 (length prefix))
                       (eq
                        t (compare-strings prefix 0 nil line indent (+ indent (length prefix)))))
              (setq result (cons indent prefix)))))
         (t
          ;; Regexp: match and extract prefix from group 2.
          (when (and (string-match (plist-get entry :line-value) line)
                     (< (match-beginning 2) (match-end 2)))
            (setq result (cons (- (match-end 1) (match-beginning 1)) (match-string 2 line)))))))
      (setq entries-remaining (cdr entries-remaining)))
    result))

(defun fancy-fill-paragraph--dot-point-item-prefix-p (item dot-points)
  "Return non-nil when ITEM would look like a dot-point start.
DOT-POINTS is from `fancy-fill-paragraph--compile-dot-point-patterns'.
An empty match is no dot-point, see
`fancy-fill-paragraph--line-dot-point-match'."
  (declare (important-return-value t))
  (let ((item-with-space (concat item " "))
        (result nil)
        (entries-remaining dot-points))
    (while (and entries-remaining (null result))
      (let ((entry (car entries-remaining)))
        (setq result
              (cond
               ((eq (plist-get entry :item-type) 'literal)
                (let ((prefix (plist-get entry :item-value)))
                  (and (< 0 (length prefix)) (string-prefix-p prefix item-with-space))))
               (t
                (and (string-match (plist-get entry :item-value) item-with-space)
                     (< 0 (match-end 0)))))))
      (setq entries-remaining (cdr entries-remaining)))
    result))

(defun fancy-fill-paragraph--fill-lines-plain
    (lines local-fill-column &optional first-line-extra-width)
  "Fill LINES as a single paragraph within LOCAL-FILL-COLUMN.
LINES is a list of strings.  Returns a list of result line strings.
Joins lines, normalizes blank-space, splits at delimiters, and solves.
FIRST-LINE-EXTRA-WIDTH is passed to `fancy-fill-paragraph--solve'."
  (declare (important-return-value t))
  (let* ((text (replace-regexp-in-string " +" " " (string-trim (mapconcat #'identity lines " ")))))
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

(defun fancy-fill-paragraph--fill-region (beg end &optional first-line-extra-width closer)
  "Fill the paragraph in the region from BEG to END.
Detects the fill prefix, strips it, normalizes blank-space,
splits at delimiter boundaries, solves for optimal line breaks,
and re-adds the prefix to each output line.
FIRST-LINE-EXTRA-WIDTH, when non-nil, narrows the first line, see
`fancy-fill-paragraph--fill-lines'.
CLOSER, when non-nil, is a comment or string closer ending the region,
held out of the fill, see `fancy-fill-paragraph--lines-split-closer'."
  (setq end (min end (point-max)))
  (pcase-let* ((dp-list (fancy-fill-paragraph--compile-dot-point-patterns))
               (prefix-info (fancy-fill-paragraph--analyze-fill-prefix beg end dp-list))
               (prefix (car prefix-info))
               (prefix-column-width (cadr prefix-info))
               (`(,lines ,closer)
                (fancy-fill-paragraph--lines-split-closer
                 (fancy-fill-paragraph--strip-region-lines beg end prefix-column-width) closer))
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
    (let ((result
           (fancy-fill-paragraph--append-closer
            (and filled
                 (mapconcat (lambda (line) (concat prefix line)) filled "\n"))
            closer prefix)))
      (when result
        (fancy-fill-paragraph--replace-region beg end result)))))

(defun fancy-fill-paragraph--by-region-impl ()
  "Fill paragraphs in the active region.
Each paragraph fills on its own, see
`fancy-fill-paragraph--fill-selection-paragraphs'.
Returns non-nil when the buffer was modified."
  (let ((beg (region-beginning))
        (end (region-end))
        ;; NOTE: markers follow their text, comments and strings filling
        ;; whole paragraphs, text outside the selection included.  Adjusting
        ;; the end by the change in buffer size took it for the only change.
        (beg-marker (copy-marker (region-beginning)))
        (end-marker (copy-marker (region-end)))
        (point-at-beg (< (point) (mark)))
        (changed nil))
    (unwind-protect
        (setq changed (fancy-fill-paragraph--fill-selection-paragraphs beg end))
      ;; Restore mark/point in original direction, deactivate mark.
      (cond
       (point-at-beg
        (set-mark end-marker)
        (goto-char beg-marker))
       (t
        (set-mark beg-marker)
        (goto-char end-marker)))
      (set-marker beg-marker nil)
      (set-marker end-marker nil)
      (setq deactivate-mark t))
    changed))

(defun fancy-fill-paragraph--fill-at-point (&optional sel-bounds)
  "Fill the paragraph at point, returning (CHANGED . JOBS).
Uses syntax-aware filling when inside a comment or string.  SEL-BOUNDS,
when non-nil, are the bounds of a selection, a comment or string then
filling the body paragraphs within them rather than the one at point.
CHANGED is non-nil when the buffer was modified.  JOBS are the comment
and string fill jobs run, nil when the paragraph filled as prose or was
left alone, see `fancy-fill-paragraph--syntax-fill-jobs-in-range'."
  (let* ((bounds (fancy-fill-paragraph--paragraph-bounds))
         (beg (car bounds))
         (end (cdr bounds))
         (jobs (fancy-fill-paragraph--syntax-fill-jobs-in-range beg end (point) sel-bounds)))
    (cons
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
      ;; alone rather than merge the code into it.  Code alone is left
      ;; alone too in a configuration mode, where it's never prose.
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
       (fancy-fill-paragraph--fill-prose-region beg end)))
     jobs)))

(defun fancy-fill-paragraph--by-point-impl ()
  "Fill the paragraph at point.
Returns non-nil when the buffer was modified, see
`fancy-fill-paragraph--fill-at-point'."
  (car (fancy-fill-paragraph--fill-at-point)))


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
          ;; NOTE: intentionally `region-active-p', not `use-region-p', so an
          ;; empty active region still takes the region path.  Leave as-is.
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
