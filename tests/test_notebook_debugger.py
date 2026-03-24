#!/usr/bin/env python3
"""
Tests for debug_all_notebooks.py - NotebookDebugger class
"""
import unittest
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from debug_all_notebooks import NotebookDebugger


def make_valid_notebook(cells=None):
    """Create a minimal valid Jupyter notebook dict"""
    if cells is None:
        cells = []
    return {
        "cells": cells,
        "metadata": {},
        "nbformat": 4,
        "nbformat_minor": 0
    }


def make_code_cell(source, execution_count=None):
    return {
        "cell_type": "code",
        "source": source,
        "metadata": {},
        "outputs": [],
        "execution_count": execution_count
    }


def make_markdown_cell(source):
    return {
        "cell_type": "markdown",
        "source": source,
        "metadata": {}
    }


class TestNotebookDebuggerInit(unittest.TestCase):
    """Test NotebookDebugger initialization"""

    def setUp(self):
        self.debugger = NotebookDebugger()

    def test_initial_errors_empty(self):
        self.assertEqual(self.debugger.errors, [])

    def test_initial_warnings_empty(self):
        self.assertEqual(self.debugger.warnings, [])

    def test_initial_notebooks_checked_zero(self):
        self.assertEqual(self.debugger.notebooks_checked, 0)

    def test_initial_notebooks_with_errors_zero(self):
        self.assertEqual(self.debugger.notebooks_with_errors, 0)


class TestCheckJsonSyntax(unittest.TestCase):
    """Test check_json_syntax method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _write_file(self, name, content):
        p = self.tmp_path / name
        p.write_text(content, encoding='utf-8')
        return p

    def test_valid_json_returns_true(self):
        nb = make_valid_notebook()
        p = self._write_file("valid.ipynb", json.dumps(nb))
        self.assertTrue(self.debugger.check_json_syntax(p))
        self.assertEqual(len(self.debugger.errors), 0)

    def test_invalid_json_returns_false(self):
        p = self._write_file("invalid.ipynb", "{not valid json }")
        self.assertFalse(self.debugger.check_json_syntax(p))
        self.assertEqual(len(self.debugger.errors), 1)
        self.assertEqual(self.debugger.errors[0]['type'], 'JSON Syntax Error')

    def test_invalid_json_records_notebook_path(self):
        p = self._write_file("bad.ipynb", "{{broken")
        self.debugger.check_json_syntax(p)
        self.assertEqual(self.debugger.errors[0]['notebook'], str(p))

    def test_nonexistent_file_returns_false(self):
        p = self.tmp_path / "nonexistent.ipynb"
        result = self.debugger.check_json_syntax(p)
        self.assertFalse(result)
        self.assertEqual(len(self.debugger.errors), 1)
        self.assertEqual(self.debugger.errors[0]['type'], 'File Read Error')

    def test_empty_json_object_is_valid(self):
        p = self._write_file("empty.ipynb", "{}")
        self.assertTrue(self.debugger.check_json_syntax(p))

    def test_truncated_json_returns_false(self):
        p = self._write_file("truncated.ipynb", '{"cells": [')
        self.assertFalse(self.debugger.check_json_syntax(p))


class TestCheckNotebookStructure(unittest.TestCase):
    """Test check_notebook_structure method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmp_path = Path(tempfile.mkdtemp())

    def _nb_path(self):
        return self.tmp_path / "test.ipynb"

    def test_valid_structure_returns_true(self):
        nb = make_valid_notebook()
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertTrue(result)

    def test_missing_cells_field_returns_false(self):
        nb = {"metadata": {}, "nbformat": 4}
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)
        self.assertTrue(any('cells' in e['error'] for e in self.debugger.errors))

    def test_missing_metadata_field_returns_false(self):
        nb = {"cells": [], "nbformat": 4}
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)
        self.assertTrue(any('metadata' in e['error'] for e in self.debugger.errors))

    def test_missing_nbformat_field_returns_false(self):
        nb = {"cells": [], "metadata": {}}
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)
        self.assertTrue(any('nbformat' in e['error'] for e in self.debugger.errors))

    def test_cells_not_a_list_returns_false(self):
        nb = {"cells": "not a list", "metadata": {}, "nbformat": 4}
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)

    def test_cell_missing_cell_type_returns_false(self):
        nb = make_valid_notebook([{"source": ["code"]}])
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)

    def test_cell_missing_source_returns_false(self):
        nb = make_valid_notebook([{"cell_type": "code"}])
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)

    def test_cell_not_dict_returns_false(self):
        nb = make_valid_notebook(["not a dict"])
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertFalse(result)

    def test_valid_cells_list_returns_true(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"]),
            make_markdown_cell(["# Title\n"])
        ])
        result = self.debugger.check_notebook_structure(self._nb_path(), nb)
        self.assertTrue(result)

    def test_multiple_missing_fields_records_all_errors(self):
        nb = {}
        self.debugger.check_notebook_structure(self._nb_path(), nb)
        error_msgs = [e['error'] for e in self.debugger.errors]
        self.assertTrue(any('cells' in m for m in error_msgs))
        self.assertTrue(any('metadata' in m for m in error_msgs))
        self.assertTrue(any('nbformat' in m for m in error_msgs))


class TestCheckPythonSyntax(unittest.TestCase):
    """Test check_python_syntax method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmp_path = Path(tempfile.mkdtemp())

    def _nb_path(self):
        return self.tmp_path / "test.ipynb"

    def test_valid_python_no_errors(self):
        nb = make_valid_notebook([make_code_cell(["x = 1\n", "y = x + 2\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_invalid_python_syntax_returns_error(self):
        nb = make_valid_notebook([make_code_cell(["def f(\n", "    pass\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertGreater(len(errors), 0)
        self.assertEqual(errors[0]['type'], 'Python Syntax Error')

    def test_magic_commands_are_skipped(self):
        nb = make_valid_notebook([make_code_cell(["%matplotlib inline\n", "x = 1\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_shell_commands_are_skipped(self):
        nb = make_valid_notebook([make_code_cell(["!pip install numpy\n", "import numpy\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_empty_cells_are_skipped(self):
        nb = make_valid_notebook([make_code_cell([])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_whitespace_only_cells_are_skipped(self):
        nb = make_valid_notebook([make_code_cell(["   \n", "\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_markdown_cells_are_skipped(self):
        nb = make_valid_notebook([make_markdown_cell(["# This is not Python\n", "def broken(\n"])])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_source_as_string_is_handled(self):
        cell = {"cell_type": "code", "source": "x = 1\n", "metadata": {}, "outputs": [], "execution_count": None}
        nb = make_valid_notebook([cell])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])

    def test_syntax_error_includes_cell_index(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"]),
            make_code_cell(["def bad syntax here\n"])
        ])
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertGreater(len(errors), 0)
        self.assertIn('cell_index', errors[0])

    def test_no_cells_returns_empty(self):
        nb = {"metadata": {}, "nbformat": 4}
        errors = self.debugger.check_python_syntax(self._nb_path(), nb)
        self.assertEqual(errors, [])


class TestCheckImports(unittest.TestCase):
    """Test check_imports method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmp_path = Path(tempfile.mkdtemp())

    def _nb_path(self):
        return self.tmp_path / "test.ipynb"

    def test_returns_list(self):
        nb = make_valid_notebook()
        result = self.debugger.check_imports(self._nb_path(), nb)
        self.assertIsInstance(result, list)

    def test_no_cells_returns_empty(self):
        nb = {"metadata": {}}
        result = self.debugger.check_imports(self._nb_path(), nb)
        self.assertEqual(result, [])

    def test_valid_imports_returns_empty(self):
        nb = make_valid_notebook([make_code_cell(["import numpy as np\n", "from pathlib import Path\n"])])
        result = self.debugger.check_imports(self._nb_path(), nb)
        self.assertEqual(result, [])


class TestCheckCellExecutionCounts(unittest.TestCase):
    """Test check_cell_execution_counts method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmp_path = Path(tempfile.mkdtemp())

    def _nb_path(self):
        return self.tmp_path / "test.ipynb"

    def test_sequential_counts_no_warnings(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"], execution_count=1),
            make_code_cell(["y = 2\n"], execution_count=2),
            make_code_cell(["z = 3\n"], execution_count=3),
        ])
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertEqual(warnings, [])

    def test_out_of_order_execution_generates_warning(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"], execution_count=1),
            make_code_cell(["y = 2\n"], execution_count=5),
            make_code_cell(["z = 3\n"], execution_count=3),
        ])
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertGreater(len(warnings), 0)
        self.assertEqual(warnings[0]['type'], 'Execution Order Warning')

    def test_none_execution_counts_skipped(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"], execution_count=None),
            make_code_cell(["y = 2\n"], execution_count=None),
        ])
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertEqual(warnings, [])

    def test_single_cell_no_warnings(self):
        nb = make_valid_notebook([make_code_cell(["x = 1\n"], execution_count=1)])
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertEqual(warnings, [])

    def test_no_cells_returns_empty(self):
        nb = {"metadata": {}}
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertEqual(warnings, [])

    def test_markdown_cells_not_counted(self):
        nb = make_valid_notebook([
            make_markdown_cell(["# Header\n"]),
            make_code_cell(["x = 1\n"], execution_count=1),
        ])
        warnings = self.debugger.check_cell_execution_counts(self._nb_path(), nb)
        self.assertEqual(warnings, [])


class TestDebugNotebook(unittest.TestCase):
    """Test debug_notebook method (integration)"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _write_notebook(self, name, nb_dict):
        p = self.tmp_path / name
        p.write_text(json.dumps(nb_dict), encoding='utf-8')
        return p

    def test_valid_notebook_result_structure(self):
        nb = make_valid_notebook([make_code_cell(["x = 1\n"])])
        p = self._write_notebook("valid.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        self.assertIn('path', result)
        self.assertIn('valid_json', result)
        self.assertIn('valid_structure', result)
        self.assertIn('syntax_errors', result)
        self.assertIn('import_warnings', result)
        self.assertIn('execution_warnings', result)
        self.assertIn('line_count', result)
        self.assertIn('cell_count', result)

    def test_valid_notebook_passes_all_checks(self):
        nb = make_valid_notebook([make_code_cell(["x = 1\n", "print(x)\n"])])
        p = self._write_notebook("valid.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        self.assertTrue(result['valid_json'])
        self.assertTrue(result['valid_structure'])
        self.assertEqual(result['syntax_errors'], [])

    def test_invalid_json_stops_early(self):
        p = self.tmp_path / "bad.ipynb"
        p.write_text("{not json}", encoding='utf-8')
        result = self.debugger.debug_notebook(p)
        self.assertFalse(result['valid_json'])
        self.assertFalse(result['valid_structure'])

    def test_line_count_reflects_source_lines(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n", "y = 2\n", "z = 3\n"]),
            make_markdown_cell(["# Title\n", "Some text\n"])
        ])
        p = self._write_notebook("lines.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        self.assertEqual(result['line_count'], 5)

    def test_cell_count_is_correct(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"]),
            make_markdown_cell(["# Title\n"]),
            make_code_cell(["y = 2\n"])
        ])
        p = self._write_notebook("cells.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        self.assertEqual(result['cell_count'], 3)

    def test_syntax_error_notebook_detected(self):
        nb = make_valid_notebook([make_code_cell(["def bad(\n"])])
        p = self._write_notebook("syntax.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        self.assertGreater(len(result['syntax_errors']), 0)

    def test_source_as_string_counted_correctly(self):
        cell = {
            "cell_type": "code",
            "source": "x = 1\ny = 2\nz = 3",
            "metadata": {},
            "outputs": [],
            "execution_count": None
        }
        nb = make_valid_notebook([cell])
        p = self._write_notebook("string_source.ipynb", nb)
        result = self.debugger.debug_notebook(p)
        # String source counts split by '\n'
        self.assertGreater(result['line_count'], 0)


class TestDebugAllNotebooks(unittest.TestCase):
    """Test debug_all_notebooks method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _write_notebook(self, rel_path, nb_dict):
        p = self.tmp_path / rel_path
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(nb_dict), encoding='utf-8')
        return p

    def test_empty_directory_returns_zero_notebooks(self):
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(result['total_notebooks'], 0)
        self.assertEqual(result['notebooks_with_errors'], 0)

    def test_finds_notebooks_recursively(self):
        nb = make_valid_notebook([make_code_cell(["x = 1\n"])])
        self._write_notebook("nb1.ipynb", nb)
        self._write_notebook("subdir/nb2.ipynb", nb)
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(result['total_notebooks'], 2)

    def test_checkpoints_are_excluded(self):
        nb = make_valid_notebook()
        self._write_notebook("good.ipynb", nb)
        self._write_notebook(".ipynb_checkpoints/good-checkpoint.ipynb", nb)
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(result['total_notebooks'], 1)

    def test_result_structure_keys(self):
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertIn('total_notebooks', result)
        self.assertIn('notebooks_with_errors', result)
        self.assertIn('total_errors', result)
        self.assertIn('total_warnings', result)
        self.assertIn('results', result)

    def test_error_notebook_counted(self):
        nb = make_valid_notebook()
        bad_path = self.tmp_path / "bad.ipynb"
        bad_path.write_text("{not valid json}", encoding='utf-8')
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(result['notebooks_with_errors'], 1)

    def test_syntax_errors_summed(self):
        nb_bad = make_valid_notebook([make_code_cell(["def bad(\n"])])
        self._write_notebook("bad_syntax.ipynb", nb_bad)
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertGreater(result['total_errors'], 0)


class TestSaveReport(unittest.TestCase):
    """Test save_report method"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_save_report_creates_file(self):
        debug_results = {
            'total_notebooks': 2,
            'notebooks_with_errors': 0,
            'total_errors': 0,
            'total_warnings': 1,
            'results': []
        }
        output_file = self.tmp_path / "report.json"
        self.debugger.save_report(debug_results, output_file)
        self.assertTrue(output_file.exists())

    def test_save_report_valid_json(self):
        debug_results = {
            'total_notebooks': 1,
            'notebooks_with_errors': 0,
            'total_errors': 0,
            'total_warnings': 0,
            'results': [{'path': 'test.ipynb', 'valid_json': True, 'syntax_errors': []}]
        }
        output_file = self.tmp_path / "report.json"
        self.debugger.save_report(debug_results, output_file)

        with open(output_file) as f:
            data = json.load(f)

        self.assertIn('summary', data)
        self.assertIn('errors', data)
        self.assertIn('warnings', data)
        self.assertIn('results', data)

    def test_save_report_summary_matches(self):
        debug_results = {
            'total_notebooks': 5,
            'notebooks_with_errors': 2,
            'total_errors': 3,
            'total_warnings': 1,
            'results': []
        }
        output_file = self.tmp_path / "report.json"
        self.debugger.save_report(debug_results, output_file)

        with open(output_file) as f:
            data = json.load(f)

        self.assertEqual(data['summary']['total_notebooks'], 5)
        self.assertEqual(data['summary']['notebooks_with_errors'], 2)
        self.assertEqual(data['summary']['total_errors'], 3)
        self.assertEqual(data['summary']['total_warnings'], 1)


class TestNotebookDebuggerEdgeCases(unittest.TestCase):
    """Edge case and regression tests for NotebookDebugger"""

    def setUp(self):
        self.debugger = NotebookDebugger()
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_multiple_syntax_errors_all_reported(self):
        nb = make_valid_notebook([
            make_code_cell(["def bad1(\n"]),
            make_code_cell(["def bad2(\n"]),
        ])
        p = self.tmp_path / "multi_err.ipynb"
        p.write_text(json.dumps(nb))
        result = self.debugger.debug_notebook(p)
        # Both cells have syntax errors
        self.assertGreaterEqual(len(result['syntax_errors']), 2)

    def test_cell_with_only_magic_no_syntax_error(self):
        nb = make_valid_notebook([make_code_cell(["%timeit x = 1\n"])])
        p = self.tmp_path / "magic.ipynb"
        p.write_text(json.dumps(nb))
        result = self.debugger.debug_notebook(p)
        self.assertEqual(result['syntax_errors'], [])

    def test_notebooks_checked_counter_increments(self):
        nb = make_valid_notebook()
        self._write_notebook("a.ipynb", nb)
        self._write_notebook("b.ipynb", nb)
        self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(self.debugger.notebooks_checked, 2)

    def _write_notebook(self, name, nb_dict):
        p = self.tmp_path / name
        p.write_text(json.dumps(nb_dict))
        return p

    def test_out_of_order_warning_includes_notebook_path(self):
        nb = make_valid_notebook([
            make_code_cell(["x = 1\n"], execution_count=3),
            make_code_cell(["y = 2\n"], execution_count=1),
        ])
        p = self.tmp_path / "order.ipynb"
        p.write_text(json.dumps(nb))
        result = self.debugger.debug_notebook(p)
        self.assertGreater(len(result['execution_warnings']), 0)
        self.assertIn('notebook', result['execution_warnings'][0])

    def test_deeply_nested_notebooks_found(self):
        nb = make_valid_notebook()
        p = self.tmp_path / "a" / "b" / "c" / "deep.ipynb"
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(json.dumps(nb))
        result = self.debugger.debug_all_notebooks(self.tmp_path)
        self.assertEqual(result['total_notebooks'], 1)


if __name__ == '__main__':
    unittest.main(verbosity=2)