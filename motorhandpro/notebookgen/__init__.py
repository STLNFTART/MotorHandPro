"""
motorhandpro.notebookgen - Shared utilities for Jupyter notebook generation.

Public API
----------
count_lines(notebook)           Count total source lines in a notebook dict.
create_cell(cell_type, source)  Build a single notebook cell dict.
create_notebook(title, ...)     Build a complete notebook dict.
colab_badge_cell(path)          Return a Colab badge markdown cell.
env_setup_cell()                Return a standard environment-setup code cell.
expand_notebook(notebook, ...)  Expand a notebook until it reaches a target
                                line count by repeatedly calling a cell factory.
"""

from motorhandpro.notebookgen._core import (
    count_lines,
    create_cell,
    create_notebook,
    colab_badge_cell,
    env_setup_cell,
    expand_notebook,
)

__all__ = [
    "count_lines",
    "create_cell",
    "create_notebook",
    "colab_badge_cell",
    "env_setup_cell",
    "expand_notebook",
]
