#!/usr/bin/env python3
"""
Expand existing notebooks to 5000+ lines.

Thin CLI wrapper — all logic lives in motorhandpro.notebookgen.
Run directly:
    python expand_notebooks_to_5000_lines.py
Or via the package CLI:
    python -m motorhandpro.notebookgen expand --target-lines 5000
"""
import sys

from motorhandpro.notebookgen.__main__ import main

if __name__ == "__main__":
    sys.exit(main(["expand", "--target-lines", "5000"]))
