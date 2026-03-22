"""
Smoke tests for motorhandpro.notebookgen.
These are intentionally lightweight so they run in CI without optional
dependencies (numpy, matplotlib, etc.).
"""
import json
import sys
from pathlib import Path

# Ensure the repo root is on the path when running tests directly.
_REPO_ROOT = Path(__file__).parent.parent.resolve()
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

import pytest

from motorhandpro.notebookgen import (
    count_lines,
    create_cell,
    create_notebook,
    colab_badge_cell,
    env_setup_cell,
    expand_notebook,
)


# ---------------------------------------------------------------------------
# create_cell
# ---------------------------------------------------------------------------

class TestCreateCell:
    def test_markdown_cell_structure(self):
        cell = create_cell("markdown", ["# Hello\n", "World"])
        assert cell["cell_type"] == "markdown"
        assert cell["source"] == ["# Hello\n", "World"]
        assert "execution_count" not in cell
        assert "outputs" not in cell

    def test_code_cell_structure(self):
        cell = create_cell("code", ["print('hi')\n"])
        assert cell["cell_type"] == "code"
        assert cell["execution_count"] is None
        assert cell["outputs"] == []

    def test_default_metadata_is_empty_dict(self):
        cell = create_cell("markdown", [])
        assert cell["metadata"] == {}

    def test_custom_metadata(self):
        cell = create_cell("markdown", [], metadata={"id": "abc"})
        assert cell["metadata"] == {"id": "abc"}


# ---------------------------------------------------------------------------
# count_lines
# ---------------------------------------------------------------------------

class TestCountLines:
    def test_empty_notebook(self):
        nb = {"cells": []}
        assert count_lines(nb) == 0

    def test_single_cell(self):
        nb = {"cells": [create_cell("code", ["a\n", "b\n", "c"])]}
        assert count_lines(nb) == 3

    def test_multiple_cells(self):
        nb = {
            "cells": [
                create_cell("markdown", ["# H\n"]),
                create_cell("code", ["x = 1\n", "y = 2\n"]),
            ]
        }
        assert count_lines(nb) == 3

    def test_missing_cells_key(self):
        assert count_lines({}) == 0


# ---------------------------------------------------------------------------
# colab_badge_cell / env_setup_cell
# ---------------------------------------------------------------------------

class TestSpecialCells:
    def test_colab_badge_is_markdown(self):
        cell = colab_badge_cell("notebooks/foo.ipynb")
        assert cell["cell_type"] == "markdown"
        assert "colab.research.google.com" in "".join(cell["source"])
        assert "notebooks/foo.ipynb" in "".join(cell["source"])

    def test_env_setup_is_code(self):
        cell = env_setup_cell()
        assert cell["cell_type"] == "code"
        source = "".join(cell["source"])
        # Should install the package, not do sys.path manipulation
        assert "pip install" in source
        assert "STLNFTART/MotorHandPro" in source


# ---------------------------------------------------------------------------
# create_notebook
# ---------------------------------------------------------------------------

class TestCreateNotebook:
    def _make(self, extra_cells=None):
        return create_notebook(
            "Test Notebook",
            "notebooks/test.ipynb",
            "A test notebook.",
            extra_cells=extra_cells,
        )

    def test_nbformat(self):
        nb = self._make()
        assert nb["nbformat"] == 4

    def test_has_three_header_cells(self):
        nb = self._make()
        # colab badge + title + env setup
        assert len(nb["cells"]) == 3

    def test_extra_cells_appended(self):
        extra = [create_cell("markdown", ["## Extra\n"])]
        nb = self._make(extra_cells=extra)
        assert len(nb["cells"]) == 4

    def test_title_in_second_cell(self):
        nb = self._make()
        source = "".join(nb["cells"][1]["source"])
        assert "Test Notebook" in source

    def test_notebook_is_json_serialisable(self):
        nb = self._make()
        dumped = json.dumps(nb)
        loaded = json.loads(dumped)
        assert loaded["nbformat"] == 4


# ---------------------------------------------------------------------------
# expand_notebook
# ---------------------------------------------------------------------------

class TestExpandNotebook:
    def _tiny_factory(self, iteration):
        """Returns 10 lines per call."""
        return [create_cell("code", [f"x = {i}\n" for i in range(10)])]

    def test_stops_when_target_reached(self):
        nb = create_notebook("T", "notebooks/t.ipynb", "desc")
        initial = count_lines(nb)
        expand_notebook(nb, self._tiny_factory, target_lines=initial + 25)
        assert count_lines(nb) >= initial + 25

    def test_does_not_expand_if_already_at_target(self):
        nb = create_notebook("T", "notebooks/t.ipynb", "desc")
        initial = count_lines(nb)
        expand_notebook(nb, self._tiny_factory, target_lines=0)
        assert count_lines(nb) == initial  # no new cells

    def test_max_iterations_respected(self):
        nb = create_notebook("T", "notebooks/t.ipynb", "desc")
        calls = []

        def counting_factory(n):
            calls.append(n)
            return [create_cell("code", ["x = 1\n"])]

        # target_lines is huge but max_iterations is 3
        expand_notebook(nb, counting_factory, target_lines=999999, max_iterations=3)
        assert len(calls) == 3


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

class TestCLI:
    def test_generate_command(self, tmp_path):
        from motorhandpro.notebookgen.__main__ import main
        ret = main(["generate", "--output-dir", str(tmp_path)])
        assert ret == 0
        nb_files = list(tmp_path.rglob("*.ipynb"))
        assert len(nb_files) > 0

    def test_expand_command(self, tmp_path):
        from motorhandpro.notebookgen.__main__ import main
        # First generate notebooks into tmp_path
        main(["generate", "--output-dir", str(tmp_path)])
        # Now expand them
        ret = main([
            "expand",
            "--notebooks-dir", str(tmp_path),
            "--target-lines", "20",
        ])
        assert ret == 0
        for nb_file in tmp_path.rglob("*.ipynb"):
            with nb_file.open() as fh:
                nb = json.load(fh)
            assert count_lines(nb) >= 20

    def test_expand_no_notebooks(self, tmp_path):
        from motorhandpro.notebookgen.__main__ import main
        ret = main(["expand", "--notebooks-dir", str(tmp_path)])
        assert ret == 1
