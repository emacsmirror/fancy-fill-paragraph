#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
SCRIPT_PATH="$(dirname "${BASH_SOURCE[0]}")"

EMACS="${EMACS:-emacs}" exec python3 "$SCRIPT_PATH/fancy-fill-bytecompile-load.py"
