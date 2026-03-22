"""
motorhandpro.notebookgen._core
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Core helpers shared by all notebook-generation scripts.
"""
from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional

# ---------------------------------------------------------------------------
# Notebook-dict building helpers
# ---------------------------------------------------------------------------

_NOTEBOOK_METADATA: Dict[str, Any] = {
    "kernelspec": {
        "display_name": "Python 3",
        "language": "python",
        "name": "python3",
    },
    "language_info": {
        "codemirror_mode": {"name": "ipython", "version": 3},
        "file_extension": ".py",
        "mimetype": "text/x-python",
        "name": "python",
        "nbconvert_exporter": "python",
        "pygments_lexer": "ipython3",
        "version": "3.8.0",
    },
    "colab": {
        "provenance": [],
        "include_colab_link": True,
        "toc_visible": True,
    },
}


def count_lines(notebook: Dict[str, Any]) -> int:
    """Return the total number of source lines across all cells."""
    return sum(len(cell.get("source", [])) for cell in notebook.get("cells", []))


def create_cell(
    cell_type: str,
    source: List[str],
    metadata: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Build a single Jupyter notebook cell dict.

    Parameters
    ----------
    cell_type:
        ``"markdown"`` or ``"code"``.
    source:
        List of source-line strings (each ending with ``'\\n'`` except the
        last, following the nbformat convention).
    metadata:
        Optional cell-level metadata dict.

    Returns
    -------
    dict
        A valid nbformat 4 cell dict.
    """
    cell: Dict[str, Any] = {
        "cell_type": cell_type,
        "metadata": metadata or {},
        "source": source,
    }
    if cell_type == "code":
        cell["execution_count"] = None
        cell["outputs"] = []
    return cell


def colab_badge_cell(colab_path: str) -> Dict[str, Any]:
    """Return a markdown cell with an 'Open in Colab' badge."""
    url = (
        f"https://colab.research.google.com/github/STLNFTART/MotorHandPro"
        f"/blob/main/{colab_path}"
    )
    badge = (
        '<a href="{url}" target="_parent">'
        '<img src="https://colab.research.google.com/assets/colab-badge.svg"'
        ' alt="Open In Colab"/>'
        "</a>"
    ).format(url=url)
    return create_cell(
        "markdown",
        [badge],
        metadata={"colab_type": "text", "id": "view-in-github"},
    )


def env_setup_cell() -> Dict[str, Any]:
    """Return a standard environment-setup code cell.

    Installs the package when running in Colab so that notebooks work
    without ``sys.path`` hacking.
    """
    source = [
        "import sys\n",
        "if 'google.colab' in sys.modules:\n",
        "    # Install the package so that imports work without path hacks.\n",
        "    !pip install -q git+https://github.com/STLNFTART/MotorHandPro.git\n",
        "import numpy as np\n",
        "import matplotlib.pyplot as plt\n",
        "import pandas as pd\n",
    ]
    return create_cell("code", source)


def create_notebook(
    title: str,
    colab_path: str,
    description: str,
    extra_cells: Optional[List[Dict[str, Any]]] = None,
    metadata_overrides: Optional[Dict[str, Any]] = None,
) -> Dict[str, Any]:
    """Build a complete notebook dict.

    Parameters
    ----------
    title:
        Human-readable notebook title (used as the first heading).
    colab_path:
        Repo-relative path to this notebook (e.g. ``"notebooks/foo.ipynb"``).
        Used to generate the Colab badge URL.
    description:
        Markdown description placed after the title heading.
    extra_cells:
        Additional cell dicts appended after the standard header cells.
    metadata_overrides:
        Values merged into the top-level notebook metadata.

    Returns
    -------
    dict
        A valid nbformat 4 notebook dict ready for ``json.dump``.
    """
    cells: List[Dict[str, Any]] = [
        colab_badge_cell(colab_path),
        create_cell("markdown", [f"# {title}\n\n{description}"]),
        env_setup_cell(),
    ]
    if extra_cells:
        cells.extend(extra_cells)

    meta = dict(_NOTEBOOK_METADATA)
    if metadata_overrides:
        meta.update(metadata_overrides)

    return {
        "cells": cells,
        "metadata": meta,
        "nbformat": 4,
        "nbformat_minor": 0,
    }


def expand_notebook(
    notebook: Dict[str, Any],
    cell_factory: Callable[[int], List[Dict[str, Any]]],
    target_lines: int = 5000,
    max_iterations: int = 200,
) -> Dict[str, Any]:
    """Expand *notebook* in-place until it has at least *target_lines* source lines.

    Parameters
    ----------
    notebook:
        Notebook dict (modified in-place).
    cell_factory:
        Callable that receives the current iteration number (1-based) and
        returns a list of new cell dicts to append.
    target_lines:
        Minimum total source-line count to reach.
    max_iterations:
        Hard cap on the number of ``cell_factory`` calls so the function
        always terminates.

    Returns
    -------
    dict
        The same (mutated) notebook dict, for convenience.
    """
    iteration = 1
    while count_lines(notebook) < target_lines and iteration <= max_iterations:
        new_cells = cell_factory(iteration)
        notebook["cells"].extend(new_cells)
        iteration += 1
    return notebook


# ---------------------------------------------------------------------------
# Persistence helpers
# ---------------------------------------------------------------------------

def save_notebook(notebook: Dict[str, Any], filepath: Path) -> None:
    """Write *notebook* as JSON to *filepath*, creating parent dirs as needed."""
    filepath = Path(filepath)
    filepath.parent.mkdir(parents=True, exist_ok=True)
    with filepath.open("w", encoding="utf-8") as fh:
        json.dump(notebook, fh, indent=2)
