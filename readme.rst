##########################
Emacs Fancy Fill Paragraph
##########################

A drop-in replacement for ``fill-paragraph`` that produces cleaner, more readable results.

Instead of greedily filling each line, this package finds the best set of line breaks
for the whole paragraph — preferring splits at sentence boundaries and punctuation.

The following features are supported:

- Prefers line breaks at sentence and clause boundaries.
- Handles dot-point lists, including nested lists, with proper indentation.
- Respects changes in leading indentation, filling each indent level independently.
- Works with fill-prefix and region filling.
- Supports ``sentence-end-double-space``.
- Fully configurable split weights for all punctuation types.

Available on MELPA `codeberg <https://melpa.org/#/fancy-fill-paragraph>`__.


Example
=======

Given this text at ``fill-column`` 64, before:

.. code-block:: text

   The function reads configuration from three sources: command-line
   flags, environment variables, and the config file (in JSON or YAML
   format). Values are merged in that order, with later sources
   taking priority. Missing keys fall back to compiled-in defaults.

After:

.. code-block:: text

   The function reads configuration from three sources:
   command-line flags, environment variables, and the config file
   (in JSON or YAML format).  Values are merged in that order,
   with later sources taking priority.
   Missing keys fall back to compiled-in defaults.

Lines break at sentence boundaries, producing balanced line lengths
and easier-to-read text.

Dot-point lists with nested items are also handled. Before:

.. code-block:: text

   The system has three stages:
   - The parser reads input and validates syntax against the schema definition.
     - Errors include line numbers and suggested corrections.
   - The compiler transforms the tree into optimized output, applying constant folding and dead code elimination.
   - The runtime executes output in a sandboxed environment, providing memory safety.

After:

.. code-block:: text

   The system has three stages:
   - The parser reads input and validates syntax against the schema
     definition.
     - Errors include line numbers and suggested corrections.
   - The compiler transforms the tree into optimized output,
     applying constant folding and dead code elimination.
   - The runtime executes output in a sandboxed environment,
     providing memory safety.


Usage
=====

Call ``fancy-fill-paragraph`` in place of ``fill-paragraph``.
With an active region, each paragraph in the region is filled separately.


Customization
-------------

.. BEGIN VARIABLES
``fancy-fill-paragraph-split-weights``: ``nil``
   Overrides for splitting weights at punctuation boundaries as a plist.
   Each value is an integer from 0 to 100 where 0 means never split
   at this boundary and 100 means always prefer splitting here.
   Intermediate values bias the solver towards splitting at that boundary.

   Available keys (with default weights):

   - ``:ellipsis`` 60
   - ``:period`` 60
   - ``:comma`` 40
   - ``:colon`` 50
   - ``:semicolon`` 50
   - ``:question`` 60
   - ``:exclamation`` 60
   - ``:em-dash`` 40
   - ``:en-dash`` 40
   - ``:paren`` 40
   - ``:bracket`` 40
   - ``:brace`` 40
   - ``:double-quote`` 40
   - ``:single-quote`` 40
   - ``:open-paren`` 10
   - ``:open-bracket`` 10
   - ``:open-brace`` 10
   - ``:open-double-quote`` 10
   - ``:open-single-quote`` 10
   - ``:space`` 1

``fancy-fill-paragraph-fill-column-margin``: ``0``
   Number of columns to subtract from ``fill-column`` when filling.
   The effective fill column is at least 1.

``fancy-fill-paragraph-fill-column-target``: ``0``
   Target column for the solver, allowing lines to extend up to ``fill-column``.
   When zero, the target equals ``fill-column``.
   When a float (0.1 to 1.0), a fraction of the effective fill column.
   When a negative integer, subtracted from the effective fill column.
   When a positive integer, used directly, clamped to the effective fill column.
   The solver minimizes raggedness around this target while still allowing
   lines up to ``fill-column`` before applying overflow penalties.

``fancy-fill-paragraph-blank-space-weight``: ``75``
   Weight for joining items across delimiter boundaries.
   An integer from 0 to 100 where higher values allow the solver to
   more readily join items onto the same line across delimiters.
   When zero, items will never be joined across delimiters,
   each delimiter boundary will always produce a line break.

``fancy-fill-paragraph-infix-delimiters``: ``(("--" . 40) ("-" . 40) ("/" . 10) ("~" . 10))``
   Alist of spaced infix delimiters and their split weights.
   Each entry is (STRING . WEIGHT) where STRING is matched with
   spaces on both sides.  For example "-" matches " - " in the text.
   WEIGHT is an integer from 0 to 100 (see ``fancy-fill-paragraph-split-weights``).

``fancy-fill-paragraph-dot-point-prefix``: ``("- " "* " (``:regexp`` . "[0-9]+\\. "))``
   List of dot-point prefix patterns to detect.
   Each entry is one of:

   - A string: matched literally after optional leading blank-space.
     For example "- " detects lines like "  - item text".

   - A cons (``:regexp`` . PATTERN): PATTERN is a regexp whose entire match
     is the prefix.  For example (``:regexp`` . "[0-9]+\\. ") detects
     numbered lists like "1. ", "10. ", "123. ".

   Set to nil to disable dot-point detection entirely.

``fancy-fill-paragraph-syntax-bounds``: ``t``
   When non-nil, constrain paragraphs to syntax boundaries.
   In programming modes this treats each comment or string as a separate
   paragraph, preventing ``fancy-fill-paragraph`` from merging text across
   distinct comments or strings.

.. END VARIABLES


Installation
============

.. code-block:: elisp

   (use-package fancy-fill-paragraph
     :bind ("M-q" . fancy-fill-paragraph))

Without ``use-package``:

.. code-block:: elisp

   (require 'fancy-fill-paragraph)
   (global-set-key (kbd "M-q") #'fancy-fill-paragraph)
