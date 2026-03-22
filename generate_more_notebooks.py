#!/usr/bin/env python3
"""
Generate additional Jupyter notebooks for MotorHandPro.

Thin CLI wrapper — all logic lives in motorhandpro.notebookgen.
Run directly:
    python generate_more_notebooks.py
Or via the package CLI:
    python -m motorhandpro.notebookgen generate
"""
import sys

from motorhandpro.notebookgen.__main__ import main

if __name__ == "__main__":
    sys.exit(main(["generate"]))
