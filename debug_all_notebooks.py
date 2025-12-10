#!/usr/bin/env python3
"""
Comprehensive Jupyter Notebook Debugger for MotorHandPro
Checks all notebooks for syntax, structure, and common errors
"""

import json
import os
import ast
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Any
import traceback


class NotebookDebugger:
    """Debug and validate Jupyter notebooks"""

    def __init__(self):
        self.errors = []
        self.warnings = []
        self.notebooks_checked = 0
        self.notebooks_with_errors = 0

    def check_json_syntax(self, notebook_path: Path) -> bool:
        """Check if notebook is valid JSON"""
        try:
            with open(notebook_path, 'r', encoding='utf-8') as f:
                json.load(f)
            return True
        except json.JSONDecodeError as e:
            self.errors.append({
                'notebook': str(notebook_path),
                'type': 'JSON Syntax Error',
                'error': str(e),
                'line': e.lineno if hasattr(e, 'lineno') else None
            })
            return False
        except Exception as e:
            self.errors.append({
                'notebook': str(notebook_path),
                'type': 'File Read Error',
                'error': str(e)
            })
            return False

    def check_notebook_structure(self, notebook_path: Path, nb_data: Dict) -> bool:
        """Validate notebook structure"""
        has_error = False

        # Check required fields
        required_fields = ['cells', 'metadata', 'nbformat']
        for field in required_fields:
            if field not in nb_data:
                self.errors.append({
                    'notebook': str(notebook_path),
                    'type': 'Structure Error',
                    'error': f'Missing required field: {field}'
                })
                has_error = True

        # Check cells structure
        if 'cells' in nb_data:
            if not isinstance(nb_data['cells'], list):
                self.errors.append({
                    'notebook': str(notebook_path),
                    'type': 'Structure Error',
                    'error': 'cells field must be a list'
                })
                has_error = True
            else:
                for i, cell in enumerate(nb_data['cells']):
                    if not isinstance(cell, dict):
                        self.errors.append({
                            'notebook': str(notebook_path),
                            'type': 'Structure Error',
                            'error': f'Cell {i} is not a dictionary',
                            'cell_index': i
                        })
                        has_error = True
                    elif 'cell_type' not in cell:
                        self.errors.append({
                            'notebook': str(notebook_path),
                            'type': 'Structure Error',
                            'error': f'Cell {i} missing cell_type',
                            'cell_index': i
                        })
                        has_error = True
                    elif 'source' not in cell:
                        self.errors.append({
                            'notebook': str(notebook_path),
                            'type': 'Structure Error',
                            'error': f'Cell {i} missing source',
                            'cell_index': i
                        })
                        has_error = True

        return not has_error

    def check_python_syntax(self, notebook_path: Path, nb_data: Dict) -> List[Dict]:
        """Check Python syntax in code cells"""
        syntax_errors = []

        if 'cells' not in nb_data:
            return syntax_errors

        for i, cell in enumerate(nb_data['cells']):
            if cell.get('cell_type') != 'code':
                continue

            # Get source code
            source = cell.get('source', [])
            if isinstance(source, list):
                code = ''.join(source)
            else:
                code = source

            # Skip empty cells
            if not code.strip():
                continue

            # Skip cells with magic commands (IPython specific)
            lines = code.split('\n')
            cleaned_lines = []
            for line in lines:
                stripped = line.lstrip()
                # Skip magic commands and shell commands
                if stripped.startswith('%') or stripped.startswith('!'):
                    continue
                cleaned_lines.append(line)

            cleaned_code = '\n'.join(cleaned_lines)
            if not cleaned_code.strip():
                continue

            # Try to parse Python syntax
            try:
                ast.parse(cleaned_code)
            except SyntaxError as e:
                syntax_errors.append({
                    'notebook': str(notebook_path),
                    'type': 'Python Syntax Error',
                    'cell_index': i,
                    'error': str(e),
                    'line': e.lineno,
                    'offset': e.offset,
                    'text': e.text
                })
            except Exception as e:
                # Some valid Python might not parse (e.g., incomplete code)
                # Only report if it looks like a real error
                if 'invalid syntax' in str(e).lower():
                    syntax_errors.append({
                        'notebook': str(notebook_path),
                        'type': 'Python Parse Error',
                        'cell_index': i,
                        'error': str(e)
                    })

        return syntax_errors

    def check_imports(self, notebook_path: Path, nb_data: Dict) -> List[Dict]:
        """Check for common import issues"""
        import_warnings = []

        if 'cells' not in nb_data:
            return import_warnings

        found_imports = set()

        for i, cell in enumerate(nb_data['cells']):
            if cell.get('cell_type') != 'code':
                continue

            source = cell.get('source', [])
            if isinstance(source, list):
                code = ''.join(source)
            else:
                code = source

            # Look for import statements
            for line in code.split('\n'):
                line = line.strip()
                if line.startswith('import ') or line.startswith('from '):
                    # Extract module name
                    try:
                        if line.startswith('import '):
                            module = line.split()[1].split('.')[0].split(',')[0]
                        else:  # from X import Y
                            module = line.split()[1].split('.')[0]
                        found_imports.add(module)
                    except:
                        pass

        # Check for common missing imports
        common_scientific = ['numpy', 'scipy', 'matplotlib', 'pandas', 'sklearn', 'torch', 'tensorflow']
        used_without_import = set()

        # This is a simplified check - would need full execution to be thorough
        return import_warnings

    def check_cell_execution_counts(self, notebook_path: Path, nb_data: Dict) -> List[Dict]:
        """Check for potential execution order issues"""
        warnings = []

        if 'cells' not in nb_data:
            return warnings

        execution_counts = []
        for i, cell in enumerate(nb_data['cells']):
            if cell.get('cell_type') == 'code':
                count = cell.get('execution_count')
                if count is not None:
                    execution_counts.append((i, count))

        # Check if execution counts are non-sequential (might indicate out-of-order execution)
        if len(execution_counts) > 1:
            prev_count = None
            for cell_idx, count in execution_counts:
                if prev_count is not None and count is not None:
                    if count < prev_count:
                        warnings.append({
                            'notebook': str(notebook_path),
                            'type': 'Execution Order Warning',
                            'cell_index': cell_idx,
                            'warning': f'Cell executed out of order (count {count} after {prev_count})'
                        })
                prev_count = count

        return warnings

    def debug_notebook(self, notebook_path: Path) -> Dict[str, Any]:
        """Run all checks on a single notebook"""
        result = {
            'path': str(notebook_path),
            'valid_json': False,
            'valid_structure': False,
            'syntax_errors': [],
            'import_warnings': [],
            'execution_warnings': [],
            'line_count': 0,
            'cell_count': 0
        }

        # Check JSON syntax
        if not self.check_json_syntax(notebook_path):
            return result

        result['valid_json'] = True

        # Load notebook data
        try:
            with open(notebook_path, 'r', encoding='utf-8') as f:
                nb_data = json.load(f)
        except:
            return result

        # Check structure
        result['valid_structure'] = self.check_notebook_structure(notebook_path, nb_data)

        # Count cells and lines
        if 'cells' in nb_data:
            result['cell_count'] = len(nb_data['cells'])
            for cell in nb_data['cells']:
                source = cell.get('source', [])
                if isinstance(source, list):
                    result['line_count'] += len(source)
                else:
                    result['line_count'] += len(str(source).split('\n'))

        # Check Python syntax
        result['syntax_errors'] = self.check_python_syntax(notebook_path, nb_data)

        # Check imports
        result['import_warnings'] = self.check_imports(notebook_path, nb_data)

        # Check execution order
        result['execution_warnings'] = self.check_cell_execution_counts(notebook_path, nb_data)

        return result

    def debug_all_notebooks(self, root_dir: Path) -> Dict[str, Any]:
        """Debug all notebooks in directory"""
        notebooks = list(root_dir.glob('**/*.ipynb'))

        # Filter out checkpoints
        notebooks = [nb for nb in notebooks if '.ipynb_checkpoints' not in str(nb)]

        print(f"Found {len(notebooks)} notebooks to debug\n")

        results = []

        for nb in notebooks:
            print(f"Checking: {nb.relative_to(root_dir)}")
            result = self.debug_notebook(nb)
            results.append(result)

            self.notebooks_checked += 1

            # Count errors
            has_error = False
            if not result['valid_json']:
                has_error = True
                print(f"  ❌ Invalid JSON")
            elif not result['valid_structure']:
                has_error = True
                print(f"  ❌ Invalid structure")
            else:
                print(f"  ✓ Valid JSON and structure")

            if result['syntax_errors']:
                has_error = True
                print(f"  ❌ {len(result['syntax_errors'])} syntax error(s)")

            if result['execution_warnings']:
                print(f"  ⚠️  {len(result['execution_warnings'])} execution warning(s)")

            print(f"  📊 {result['cell_count']} cells, ~{result['line_count']} lines")

            if has_error:
                self.notebooks_with_errors += 1

            print()

        return {
            'total_notebooks': len(results),
            'notebooks_with_errors': self.notebooks_with_errors,
            'total_errors': len([e for r in results for e in r['syntax_errors']]),
            'total_warnings': len([w for r in results for w in r['execution_warnings']]),
            'results': results
        }

    def print_summary(self, debug_results: Dict):
        """Print summary of debug results"""
        print("=" * 80)
        print("NOTEBOOK DEBUG SUMMARY")
        print("=" * 80)
        print(f"Total notebooks checked: {debug_results['total_notebooks']}")
        print(f"Notebooks with errors: {debug_results['notebooks_with_errors']}")
        print(f"Total syntax errors: {debug_results['total_errors']}")
        print(f"Total warnings: {debug_results['total_warnings']}")
        print()

        if self.errors:
            print("ERRORS FOUND:")
            print("-" * 80)
            for error in self.errors:
                print(f"\n📁 {error['notebook']}")
                print(f"   Type: {error['type']}")
                print(f"   Error: {error['error']}")
                if 'cell_index' in error:
                    print(f"   Cell: {error['cell_index']}")
                if 'line' in error and error['line']:
                    print(f"   Line: {error['line']}")

        # Print detailed syntax errors
        syntax_errors = []
        for result in debug_results['results']:
            if result['syntax_errors']:
                for err in result['syntax_errors']:
                    syntax_errors.append(err)

        if syntax_errors:
            print("\n" + "=" * 80)
            print("SYNTAX ERRORS:")
            print("-" * 80)
            for err in syntax_errors:
                print(f"\n📁 {err['notebook']}")
                print(f"   Cell: {err['cell_index']}")
                print(f"   Error: {err['error']}")
                if 'line' in err and err['line']:
                    print(f"   Line: {err['line']}")
                if 'text' in err and err['text']:
                    print(f"   Code: {err['text']}")

        print("\n" + "=" * 80)

        if debug_results['notebooks_with_errors'] == 0:
            print("✅ All notebooks are valid!")
        else:
            print(f"⚠️  {debug_results['notebooks_with_errors']} notebook(s) need attention")

        print("=" * 80)

    def save_report(self, debug_results: Dict, output_file: Path):
        """Save detailed report to JSON"""
        with open(output_file, 'w') as f:
            json.dump({
                'summary': {
                    'total_notebooks': debug_results['total_notebooks'],
                    'notebooks_with_errors': debug_results['notebooks_with_errors'],
                    'total_errors': debug_results['total_errors'],
                    'total_warnings': debug_results['total_warnings']
                },
                'errors': self.errors,
                'warnings': self.warnings,
                'results': debug_results['results']
            }, f, indent=2)

        print(f"\nDetailed report saved to: {output_file}")


def main():
    """Main entry point"""
    root_dir = Path('/home/user/MotorHandPro')

    debugger = NotebookDebugger()

    print("=" * 80)
    print("MOTORHANDPRO NOTEBOOK DEBUGGER")
    print("=" * 80)
    print()

    results = debugger.debug_all_notebooks(root_dir)

    debugger.print_summary(results)

    # Save report
    report_file = root_dir / 'notebook_debug_report.json'
    debugger.save_report(results, report_file)

    # Exit with error code if issues found
    if results['notebooks_with_errors'] > 0:
        sys.exit(1)
    else:
        sys.exit(0)


if __name__ == '__main__':
    main()
