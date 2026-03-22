#!/usr/bin/env python3
"""
Build massive 5000+ line Jupyter notebooks for MotorHandPro.

Thin CLI wrapper — all logic lives in motorhandpro.notebookgen.
Run directly:
    python build_massive_notebooks.py
Or via the package CLI:
    python -m motorhandpro.notebookgen expand --target-lines 5000
"""
import sys

from motorhandpro.notebookgen.__main__ import main

if __name__ == "__main__":
    sys.exit(main(["expand", "--target-lines", "5000"]))
