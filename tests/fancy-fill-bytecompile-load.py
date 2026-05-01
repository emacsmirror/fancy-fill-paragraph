#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
"""Smoke test: byte-compile the package and use it from the .elc.

The main ert suite removes the .elc and exercises the source, so failures that
only manifest after byte-compilation (e.g. lazy docstring references stored as
``(file . offset)`` cons cells) slip past it. This script orchestrates two
``emacs -Q --batch`` runs: the first byte-compiles the package into a staging
directory, the second loads ``fancy-fill-bytecompile-load.el`` which performs
the actual assertion.
"""

import os
import shutil
import subprocess
import sys
import tempfile

EMACS = os.environ.get("EMACS", "emacs")
TESTS_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(TESTS_DIR)
SOURCE_EL = os.path.join(ROOT, "fancy-fill-paragraph.el")
TEST_EL = os.path.join(TESTS_DIR, "fancy-fill-bytecompile-load.el")


def main() -> int:
    for path in (SOURCE_EL, TEST_EL):
        if not os.path.isfile(path):
            print("FAIL: file not found: {:s}".format(path), file=sys.stderr)
            return 1

    with tempfile.TemporaryDirectory(prefix="ffp-bc-") as tmp:
        staged_el = os.path.join(tmp, "fancy-fill-paragraph.el")
        staged_elc = os.path.join(tmp, "fancy-fill-paragraph.elc")
        shutil.copy2(SOURCE_EL, staged_el)

        compile_res = subprocess.run(
            [EMACS, "-Q", "--batch",
             "--eval", '(unless (byte-compile-file "{:s}") (kill-emacs 1))'.format(staged_el)],
            capture_output=True, text=True, check=False,
        )
        if compile_res.returncode != 0 or not os.path.isfile(staged_elc):
            print("FAIL: byte-compile failed", file=sys.stderr)
            sys.stderr.write(compile_res.stdout)
            sys.stderr.write(compile_res.stderr)
            return 1

        # Remove the source so `require` must use the `.elc`.
        os.remove(staged_el)

        load_res = subprocess.run(
            [EMACS, "-Q", "--batch", "-L", tmp, "-l", TEST_EL],
            capture_output=True, text=True, check=False,
        )
        sys.stdout.write(load_res.stdout)
        sys.stderr.write(load_res.stderr)
        return load_res.returncode


if __name__ == "__main__":
    sys.exit(main())
