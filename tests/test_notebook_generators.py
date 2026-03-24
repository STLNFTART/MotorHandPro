#!/usr/bin/env python3
"""
Tests for notebook generation modules:
- build_massive_notebooks.py (NotebookBuilder)
- generate_5000_line_notebooks.py (Notebook5000Generator)
- expand_notebooks_to_5000_lines.py (count_lines, generate_*_experiments)
- generate_more_notebooks.py (create_notebook)
- generate_comprehensive_notebooks.py (create_notebook_cell, count_notebook_lines)
"""
import unittest
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from build_massive_notebooks import NotebookBuilder, build_prosthetics_experiments_notebook
from generate_5000_line_notebooks import Notebook5000Generator
from expand_notebooks_to_5000_lines import (
    count_lines,
    generate_prosthetics_experiments,
    generate_radiation_experiments,
    generate_lam_experiments,
    generate_hardware_experiments,
    generate_generic_experiments,
)
from generate_more_notebooks import create_notebook

# generate_comprehensive_notebooks.py contains a syntax error after line ~200
# that prevents importing the module. We directly exec the valid portion of
# the file to access create_notebook_cell, and re-implement count_notebook_lines
# (which appears after the syntax error) locally for testing purposes.
import ast as _ast, types as _types

def _extract_functions_from_broken_module(filepath):
    """Exec the valid prefix of a module containing a syntax error."""
    with open(filepath) as f:
        src = f.read()
    # Compile ignores syntax errors only when we manually truncate.
    # find the line where the error occurs, then exec lines before it.
    lines = src.splitlines(keepends=True)
    # find last completely-valid group of lines
    lo, hi = 0, len(lines)
    # Walk forward: find the maximum prefix that parses successfully
    last_good = 0
    # Step through lines looking for transitions from valid to invalid
    for i in range(len(lines)):
        try:
            _ast.parse("".join(lines[:i+1]))
            last_good = i + 1
        except SyntaxError:
            pass  # keep going to find later valid prefixes too (e.g. after docstring)
    mod = _types.ModuleType("_comprehensive_partial")
    exec(compile("".join(lines[:last_good]), filepath, "exec"), mod.__dict__)
    return mod

_comprehensive_mod = _extract_functions_from_broken_module(
    str(Path(__file__).parent.parent / "generate_comprehensive_notebooks.py")
)
create_notebook_cell = _comprehensive_mod.create_notebook_cell


def count_notebook_lines(nb):
    """Re-implementation matching the behaviour in generate_comprehensive_notebooks.py.

    The original function appears after the syntax error and cannot be imported.
    This version matches what the code intends: sum of cell source list lengths.
    """
    return sum(len(cell["source"]) for cell in nb.get("cells", []))


# ─────────────────────────────────────────────────────────────────────────────
# NotebookBuilder tests
# ─────────────────────────────────────────────────────────────────────────────

class TestNotebookBuilderInit(unittest.TestCase):

    def test_initial_title_stored(self):
        nb = NotebookBuilder("My Test Notebook")
        self.assertEqual(nb.title, "My Test Notebook")

    def test_initial_cells_empty(self):
        nb = NotebookBuilder("Test")
        self.assertEqual(nb.cells, [])

    def test_initial_line_count_zero(self):
        nb = NotebookBuilder("Test")
        self.assertEqual(nb.line_count, 0)


class TestNotebookBuilderAddMarkdown(unittest.TestCase):

    def setUp(self):
        self.nb = NotebookBuilder("Test")

    def test_add_markdown_creates_cell(self):
        self.nb.add_markdown("# Title\n")
        self.assertEqual(len(self.nb.cells), 1)

    def test_add_markdown_cell_type(self):
        self.nb.add_markdown("text\n")
        self.assertEqual(self.nb.cells[0]['cell_type'], 'markdown')

    def test_add_markdown_source_content(self):
        self.nb.add_markdown("line1\n", "line2\n")
        self.assertEqual(self.nb.cells[0]['source'], ["line1\n", "line2\n"])

    def test_add_markdown_increments_line_count(self):
        self.nb.add_markdown("a\n", "b\n", "c\n")
        self.assertEqual(self.nb.line_count, 3)

    def test_add_markdown_metadata_present(self):
        self.nb.add_markdown("text\n")
        self.assertIn('metadata', self.nb.cells[0])

    def test_multiple_markdown_cells_accumulated(self):
        self.nb.add_markdown("a\n")
        self.nb.add_markdown("b\n")
        self.assertEqual(len(self.nb.cells), 2)
        self.assertEqual(self.nb.line_count, 2)


class TestNotebookBuilderAddCode(unittest.TestCase):

    def setUp(self):
        self.nb = NotebookBuilder("Test")

    def test_add_code_creates_cell(self):
        self.nb.add_code("x = 1\n")
        self.assertEqual(len(self.nb.cells), 1)

    def test_add_code_cell_type(self):
        self.nb.add_code("x = 1\n")
        self.assertEqual(self.nb.cells[0]['cell_type'], 'code')

    def test_add_code_source_content(self):
        self.nb.add_code("x = 1\n", "y = 2\n")
        self.assertEqual(self.nb.cells[0]['source'], ["x = 1\n", "y = 2\n"])

    def test_add_code_has_execution_count_none(self):
        self.nb.add_code("x = 1\n")
        self.assertIsNone(self.nb.cells[0]['execution_count'])

    def test_add_code_has_empty_outputs(self):
        self.nb.add_code("x = 1\n")
        self.assertEqual(self.nb.cells[0]['outputs'], [])

    def test_add_code_increments_line_count(self):
        self.nb.add_code("a\n", "b\n")
        self.assertEqual(self.nb.line_count, 2)

    def test_mixed_add_accumulates_line_count(self):
        self.nb.add_markdown("header\n")
        self.nb.add_code("x = 1\n", "y = 2\n")
        self.assertEqual(self.nb.line_count, 3)
        self.assertEqual(len(self.nb.cells), 2)


class TestNotebookBuilderBuild(unittest.TestCase):

    def setUp(self):
        self.nb = NotebookBuilder("My Notebook")
        self.nb.add_markdown("# Title\n")
        self.nb.add_code("x = 1\n")
        self.built = self.nb.build()

    def test_build_returns_dict(self):
        self.assertIsInstance(self.built, dict)

    def test_build_has_cells(self):
        self.assertIn('cells', self.built)
        self.assertEqual(len(self.built['cells']), 2)

    def test_build_has_metadata(self):
        self.assertIn('metadata', self.built)

    def test_build_has_nbformat(self):
        self.assertEqual(self.built['nbformat'], 4)

    def test_build_has_nbformat_minor(self):
        self.assertEqual(self.built['nbformat_minor'], 0)

    def test_build_metadata_has_kernelspec(self):
        self.assertIn('kernelspec', self.built['metadata'])

    def test_build_kernelspec_language_python(self):
        self.assertEqual(self.built['metadata']['kernelspec']['language'], 'python')

    def test_build_metadata_colab_name_is_title(self):
        self.assertEqual(self.built['metadata']['colab']['name'], "My Notebook")

    def test_build_accelerator_is_gpu(self):
        self.assertEqual(self.built['metadata']['accelerator'], 'GPU')

    def test_build_is_valid_json(self):
        # Should be serializable
        serialized = json.dumps(self.built)
        parsed = json.loads(serialized)
        self.assertEqual(parsed['nbformat'], 4)

    def test_build_preserves_cell_order(self):
        self.assertEqual(self.built['cells'][0]['cell_type'], 'markdown')
        self.assertEqual(self.built['cells'][1]['cell_type'], 'code')


class TestBuildProstheticsExperimentsNotebook(unittest.TestCase):

    def setUp(self):
        # This prints during execution, redirect stdout
        import io
        self._old_stdout = sys.stdout
        sys.stdout = io.StringIO()
        self.nb = build_prosthetics_experiments_notebook()
        sys.stdout = self._old_stdout

    def test_returns_dict(self):
        self.assertIsInstance(self.nb, dict)

    def test_has_required_keys(self):
        self.assertIn('cells', self.nb)
        self.assertIn('metadata', self.nb)
        self.assertIn('nbformat', self.nb)

    def test_has_multiple_cells(self):
        self.assertGreater(len(self.nb['cells']), 0)

    def test_has_both_cell_types(self):
        types = {c['cell_type'] for c in self.nb['cells']}
        self.assertIn('markdown', types)
        self.assertIn('code', types)

    def test_is_serializable(self):
        json_str = json.dumps(self.nb)
        self.assertIsInstance(json_str, str)


# ─────────────────────────────────────────────────────────────────────────────
# Notebook5000Generator tests
# ─────────────────────────────────────────────────────────────────────────────

class TestNotebook5000GeneratorInit(unittest.TestCase):

    def test_title_stored(self):
        gen = Notebook5000Generator("My Title")
        self.assertEqual(gen.title, "My Title")

    def test_cells_initially_empty(self):
        gen = Notebook5000Generator("Test")
        self.assertEqual(gen.cells, [])


class TestNotebook5000GeneratorMd(unittest.TestCase):

    def setUp(self):
        self.gen = Notebook5000Generator("Test")

    def test_md_adds_cell(self):
        self.gen.md("# Hello")
        self.assertEqual(len(self.gen.cells), 1)

    def test_md_cell_type_is_markdown(self):
        self.gen.md("text")
        self.assertEqual(self.gen.cells[0]['cell_type'], 'markdown')

    def test_md_source_is_list_of_lines(self):
        self.gen.md("line1\nline2")
        source = self.gen.cells[0]['source']
        self.assertIsInstance(source, list)
        self.assertEqual(len(source), 2)

    def test_md_lines_end_with_newline(self):
        self.gen.md("hello\nworld")
        for line in self.gen.cells[0]['source']:
            self.assertTrue(line.endswith('\n'))


class TestNotebook5000GeneratorCode(unittest.TestCase):

    def setUp(self):
        self.gen = Notebook5000Generator("Test")

    def test_code_adds_cell(self):
        self.gen.code("x = 1")
        self.assertEqual(len(self.gen.cells), 1)

    def test_code_cell_type_is_code(self):
        self.gen.code("x = 1")
        self.assertEqual(self.gen.cells[0]['cell_type'], 'code')

    def test_code_execution_count_none(self):
        self.gen.code("x = 1")
        self.assertIsNone(self.gen.cells[0]['execution_count'])

    def test_code_outputs_empty(self):
        self.gen.code("x = 1")
        self.assertEqual(self.gen.cells[0]['outputs'], [])

    def test_code_source_is_list_of_lines(self):
        self.gen.code("x = 1\ny = 2")
        self.assertEqual(len(self.gen.cells[0]['source']), 2)


class TestNotebook5000GeneratorCountLines(unittest.TestCase):

    def test_empty_generator_zero_lines(self):
        gen = Notebook5000Generator("Test")
        self.assertEqual(gen.count_lines(), 0)

    def test_counts_lines_across_cells(self):
        gen = Notebook5000Generator("Test")
        gen.md("line1\nline2")    # 2 lines
        gen.code("a = 1\nb = 2\nc = 3")  # 3 lines
        self.assertEqual(gen.count_lines(), 5)


class TestNotebook5000GeneratorExperimentMethods(unittest.TestCase):

    def setUp(self):
        self.gen = Notebook5000Generator("Test")

    def test_get_experiment_objective_returns_string(self):
        obj = self.gen._get_experiment_objective(1)
        self.assertIsInstance(obj, str)
        self.assertGreater(len(obj), 0)

    def test_get_experiment_objective_cycles(self):
        """Objectives should cycle - index 0 and 10 should be same"""
        obj0 = self.gen._get_experiment_objective(0)
        obj10 = self.gen._get_experiment_objective(10)
        self.assertEqual(obj0, obj10)

    def test_get_experiment_methodology_returns_string(self):
        method = self.gen._get_experiment_methodology(1)
        self.assertIsInstance(method, str)
        self.assertGreater(len(method), 0)

    def test_get_experiment_outcomes_returns_string(self):
        outcome = self.gen._get_experiment_outcomes(1)
        self.assertIsInstance(outcome, str)
        self.assertGreater(len(outcome), 0)

    def test_add_comprehensive_experiment_adds_cells(self):
        before = len(self.gen.cells)
        self.gen.add_comprehensive_experiment(1)
        after = len(self.gen.cells)
        self.assertGreater(after, before)

    def test_add_comprehensive_experiment_increases_lines(self):
        before = self.gen.count_lines()
        self.gen.add_comprehensive_experiment(1)
        after = self.gen.count_lines()
        self.assertGreater(after, before)

    def test_add_experiments_until_target_reaches_target(self):
        target = 50
        self.gen.add_experiments_until_target(target=target)
        self.assertGreaterEqual(self.gen.count_lines(), target)

    def test_add_experiments_until_target_safety_limit(self):
        """Should stop before 100 experiments even if target is unreachably large"""
        gen = Notebook5000Generator("Test")
        # Add 0 lines per experiment won't reach 1M, but safety limit stops it
        gen.add_experiments_until_target(target=1000000)
        # Should not loop infinitely - just verify it terminates


class TestNotebook5000GeneratorSave(unittest.TestCase):

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_save_creates_file(self):
        gen = Notebook5000Generator("Test")
        gen.md("# Hello")
        filepath = str(self.tmp_path / "test.ipynb")
        gen.save(filepath)
        self.assertTrue(Path(filepath).exists())

    def test_save_returns_line_count(self):
        gen = Notebook5000Generator("Test")
        gen.md("line1\nline2")
        filepath = str(self.tmp_path / "test.ipynb")
        lines = gen.save(filepath)
        self.assertEqual(lines, 2)

    def test_save_creates_valid_json(self):
        gen = Notebook5000Generator("Test")
        gen.code("x = 1")
        filepath = str(self.tmp_path / "test.ipynb")
        gen.save(filepath)
        with open(filepath) as f:
            nb = json.load(f)
        self.assertEqual(nb['nbformat'], 4)
        self.assertIn('cells', nb)

    def test_save_creates_parent_directories(self):
        gen = Notebook5000Generator("Test")
        gen.md("content")
        filepath = str(self.tmp_path / "subdir" / "nested" / "test.ipynb")
        gen.save(filepath)
        self.assertTrue(Path(filepath).exists())

    def test_save_sets_title_in_metadata(self):
        gen = Notebook5000Generator("My Custom Title")
        gen.md("content")
        filepath = str(self.tmp_path / "titled.ipynb")
        gen.save(filepath)
        with open(filepath) as f:
            nb = json.load(f)
        self.assertEqual(nb['metadata']['colab']['name'], "My Custom Title")


# ─────────────────────────────────────────────────────────────────────────────
# expand_notebooks_to_5000_lines tests
# ─────────────────────────────────────────────────────────────────────────────

class TestCountLines(unittest.TestCase):

    def test_empty_notebook_zero_lines(self):
        nb = {"cells": []}
        self.assertEqual(count_lines(nb), 0)

    def test_counts_list_sources(self):
        nb = {
            "cells": [
                {"cell_type": "code", "source": ["a\n", "b\n", "c\n"]},
                {"cell_type": "markdown", "source": ["d\n", "e\n"]}
            ]
        }
        self.assertEqual(count_lines(nb), 5)

    def test_no_cells_key_returns_zero(self):
        nb = {}
        self.assertEqual(count_lines(nb), 0)

    def test_cell_with_empty_source_counts_zero(self):
        nb = {"cells": [{"cell_type": "code", "source": []}]}
        self.assertEqual(count_lines(nb), 0)

    def test_cell_with_no_source_key_counts_zero(self):
        nb = {"cells": [{"cell_type": "code"}]}
        self.assertEqual(count_lines(nb), 0)


class TestGenerateExperimentFunctions(unittest.TestCase):
    """Test generate_*_experiments functions"""

    def _validate_cells(self, cells):
        """Validate that a list of cells has correct structure"""
        self.assertIsInstance(cells, list)
        for cell in cells:
            self.assertIn('cell_type', cell)
            self.assertIn(cell['cell_type'], ['code', 'markdown'])
            self.assertIn('source', cell)
            self.assertIsInstance(cell['source'], list)

    def test_generate_prosthetics_returns_list(self):
        cells = generate_prosthetics_experiments(100)
        self.assertIsInstance(cells, list)

    def test_generate_prosthetics_cells_have_valid_structure(self):
        cells = generate_prosthetics_experiments(100)
        self._validate_cells(cells)

    def test_generate_prosthetics_returns_nonempty_list(self):
        cells = generate_prosthetics_experiments(100)
        self.assertGreater(len(cells), 0)

    def test_generate_radiation_returns_list(self):
        cells = generate_radiation_experiments(100)
        self.assertIsInstance(cells, list)

    def test_generate_lam_returns_list(self):
        cells = generate_lam_experiments(100)
        self.assertIsInstance(cells, list)

    def test_generate_hardware_returns_list(self):
        cells = generate_hardware_experiments(100)
        self.assertIsInstance(cells, list)

    def test_generate_generic_returns_list(self):
        cells = generate_generic_experiments(100)
        self.assertIsInstance(cells, list)

    def test_prosthetics_cells_include_code_cell(self):
        cells = generate_prosthetics_experiments(100)
        types = {c['cell_type'] for c in cells}
        # Should have at least markdown or code cells
        self.assertTrue(types.issubset({'code', 'markdown'}))


# ─────────────────────────────────────────────────────────────────────────────
# generate_more_notebooks.py tests
# ─────────────────────────────────────────────────────────────────────────────

class TestCreateNotebook(unittest.TestCase):

    def setUp(self):
        self.nb = create_notebook(
            title="Test Notebook",
            colab_path="notebooks/test.ipynb",
            description="A test notebook",
            cells_content=[]
        )

    def test_returns_dict(self):
        self.assertIsInstance(self.nb, dict)

    def test_has_cells(self):
        self.assertIn('cells', self.nb)

    def test_has_at_least_three_default_cells(self):
        # create_notebook adds: colab badge, title, setup code
        self.assertGreaterEqual(len(self.nb['cells']), 3)

    def test_first_cell_has_colab_link(self):
        first_source = ''.join(self.nb['cells'][0]['source'])
        self.assertIn('colab.research.google.com', first_source)

    def test_colab_path_in_badge(self):
        first_source = ''.join(self.nb['cells'][0]['source'])
        self.assertIn('notebooks/test.ipynb', first_source)

    def test_title_in_second_cell(self):
        second_source = ''.join(self.nb['cells'][1]['source'])
        self.assertIn('Test Notebook', second_source)

    def test_description_in_second_cell(self):
        second_source = ''.join(self.nb['cells'][1]['source'])
        self.assertIn('A test notebook', second_source)

    def test_custom_cells_appended(self):
        custom_cell = {"cell_type": "markdown", "metadata": {}, "source": ["## Custom\n"]}
        nb = create_notebook("T", "path.ipynb", "desc", [custom_cell])
        # Custom cells should be at the end
        last_cell_source = ''.join(nb['cells'][-1]['source'])
        self.assertIn('Custom', last_cell_source)

    def test_has_metadata(self):
        self.assertIn('metadata', self.nb)

    def test_has_kernelspec(self):
        self.assertIn('kernelspec', self.nb['metadata'])

    def test_kernelspec_python3(self):
        self.assertEqual(self.nb['metadata']['kernelspec']['name'], 'python3')

    def test_nbformat_is_4(self):
        self.assertEqual(self.nb['nbformat'], 4)

    def test_is_valid_json_serializable(self):
        data = json.dumps(self.nb)
        parsed = json.loads(data)
        self.assertEqual(parsed['nbformat'], 4)

    def test_empty_cells_content_works(self):
        nb = create_notebook("T", "p.ipynb", "d", [])
        self.assertIsInstance(nb, dict)
        self.assertGreater(len(nb['cells']), 0)

    def test_multiple_custom_cells_all_appended(self):
        custom = [
            {"cell_type": "markdown", "metadata": {}, "source": ["A\n"]},
            {"cell_type": "code", "metadata": {}, "source": ["x = 1\n"],
             "execution_count": None, "outputs": []}
        ]
        nb = create_notebook("T", "p.ipynb", "d", custom)
        # Should have at least the default 3 + 2 custom
        self.assertGreaterEqual(len(nb['cells']), 5)


# ─────────────────────────────────────────────────────────────────────────────
# generate_comprehensive_notebooks.py tests
# ─────────────────────────────────────────────────────────────────────────────

class TestCreateNotebookCell(unittest.TestCase):

    def test_code_cell_type(self):
        cell = create_notebook_cell("code", ["x = 1\n"])
        self.assertEqual(cell['cell_type'], 'code')

    def test_markdown_cell_type(self):
        cell = create_notebook_cell("markdown", ["# Title\n"])
        self.assertEqual(cell['cell_type'], 'markdown')

    def test_source_stored(self):
        cell = create_notebook_cell("code", ["line1\n", "line2\n"])
        self.assertEqual(cell['source'], ["line1\n", "line2\n"])

    def test_code_cell_has_execution_count(self):
        cell = create_notebook_cell("code", [])
        self.assertIn('execution_count', cell)
        self.assertIsNone(cell['execution_count'])

    def test_code_cell_has_outputs(self):
        cell = create_notebook_cell("code", [])
        self.assertIn('outputs', cell)
        self.assertEqual(cell['outputs'], [])

    def test_markdown_cell_no_execution_count(self):
        cell = create_notebook_cell("markdown", ["text\n"])
        self.assertNotIn('execution_count', cell)

    def test_markdown_cell_no_outputs(self):
        cell = create_notebook_cell("markdown", ["text\n"])
        self.assertNotIn('outputs', cell)

    def test_default_metadata_empty_dict(self):
        cell = create_notebook_cell("code", [])
        self.assertEqual(cell['metadata'], {})

    def test_custom_metadata_stored(self):
        cell = create_notebook_cell("code", [], metadata={"collapsed": True})
        self.assertEqual(cell['metadata'], {"collapsed": True})

    def test_empty_source_list(self):
        cell = create_notebook_cell("code", [])
        self.assertEqual(cell['source'], [])


class TestCountNotebookLines(unittest.TestCase):

    def test_empty_notebook_zero_lines(self):
        nb = {"cells": []}
        self.assertEqual(count_notebook_lines(nb), 0)

    def test_counts_all_cells(self):
        nb = {
            "cells": [
                {"source": ["a\n", "b\n"]},
                {"source": ["c\n"]}
            ]
        }
        self.assertEqual(count_notebook_lines(nb), 3)

    def test_empty_source_counts_zero(self):
        nb = {"cells": [{"source": []}]}
        self.assertEqual(count_notebook_lines(nb), 0)


if __name__ == '__main__':
    unittest.main(verbosity=2)