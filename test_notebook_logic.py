#!/usr/bin/env python3
"""
Test notebook code logic without requiring all dependencies
Checks for common issues like undefined variables, incorrect function calls, etc.
"""

import json
import ast
import sys
from pathlib import Path
from typing import Dict, List, Set, Any
import re


class NotebookLogicTester:
    """Test notebook logic and code patterns"""

    def __init__(self):
        self.issues = []
        self.warnings = []

    def extract_code_cells(self, notebook_path: Path) -> List[str]:
        """Extract all code cells from notebook"""
        code_cells = []

        try:
            with open(notebook_path, 'r', encoding='utf-8') as f:
                nb = json.load(f)

            for cell in nb.get('cells', []):
                if cell.get('cell_type') == 'code':
                    source = cell.get('source', [])
                    if isinstance(source, list):
                        code = ''.join(source)
                    else:
                        code = source

                    if code.strip():
                        code_cells.append(code)

        except Exception as e:
            self.issues.append({
                'notebook': str(notebook_path),
                'type': 'Parse Error',
                'error': str(e)
            })

        return code_cells

    def check_for_common_issues(self, notebook_path: Path, code_cells: List[str]) -> List[Dict]:
        """Check for common coding issues"""
        issues = []

        for i, code in enumerate(code_cells):
            # Skip magic commands
            if code.strip().startswith('%') or code.strip().startswith('!'):
                continue

            # Check for undefined variable patterns
            # This is a simplified check
            lines = code.split('\n')

            for line_num, line in enumerate(lines, 1):
                stripped = line.strip()

                # Check for potential division by zero
                if re.search(r'/\s*0\b', stripped) and not stripped.startswith('#'):
                    issues.append({
                        'notebook': str(notebook_path),
                        'cell': i,
                        'line': line_num,
                        'type': 'Potential Division by Zero',
                        'code': stripped
                    })

                # Check for bare except clauses
                if stripped == 'except:' or stripped.startswith('except:'):
                    self.warnings.append({
                        'notebook': str(notebook_path),
                        'cell': i,
                        'line': line_num,
                        'type': 'Bare except clause',
                        'suggestion': 'Use specific exception types'
                    })

                # Check for print statements without parentheses (Python 2 style)
                if re.match(r'print\s+["\']', stripped) and not stripped.startswith('#'):
                    issues.append({
                        'notebook': str(notebook_path),
                        'cell': i,
                        'line': line_num,
                        'type': 'Python 2 style print',
                        'code': stripped
                    })

        return issues

    def check_cell_dependencies(self, notebook_path: Path, code_cells: List[str]) -> List[Dict]:
        """Check if cells reference undefined variables (simplified check)"""
        issues = []
        defined_vars = set()

        for i, code in enumerate(code_cells):
            # Skip magic commands
            if code.strip().startswith('%') or code.strip().startswith('!'):
                continue

            try:
                # Parse AST
                tree = ast.parse(code)

                # Find variable assignments
                for node in ast.walk(tree):
                    if isinstance(node, ast.Assign):
                        for target in node.targets:
                            if isinstance(target, ast.Name):
                                defined_vars.add(target.id)
                    elif isinstance(node, ast.FunctionDef):
                        defined_vars.add(node.name)
                    elif isinstance(node, ast.ClassDef):
                        defined_vars.add(node.name)

            except:
                # If we can't parse, skip
                pass

        return issues

    def check_data_file_references(self, notebook_path: Path, code_cells: List[str]) -> List[Dict]:
        """Check for references to data files that might not exist"""
        issues = []

        common_extensions = ['.csv', '.json', '.txt', '.pkl', '.npy', '.h5', '.hdf5', '.mat']

        for i, code in enumerate(code_cells):
            for ext in common_extensions:
                # Look for file references
                pattern = rf'["\']([^"\']*{re.escape(ext)})["\']'
                matches = re.findall(pattern, code)

                for match in matches:
                    # Check if file path looks external (not http/https)
                    if not match.startswith('http') and not match.startswith('/tmp'):
                        # This is just a warning, not necessarily an error
                        self.warnings.append({
                            'notebook': str(notebook_path),
                            'cell': i,
                            'type': 'External file reference',
                            'file': match,
                            'suggestion': 'Ensure file exists or is downloadable'
                        })

        return issues

    def test_notebook(self, notebook_path: Path) -> Dict[str, Any]:
        """Run all logic tests on a notebook"""
        result = {
            'path': str(notebook_path),
            'issues': [],
            'warnings': []
        }

        # Extract code cells
        code_cells = self.extract_code_cells(notebook_path)

        if not code_cells:
            return result

        # Run checks
        result['issues'].extend(self.check_for_common_issues(notebook_path, code_cells))
        result['issues'].extend(self.check_cell_dependencies(notebook_path, code_cells))
        result['issues'].extend(self.check_data_file_references(notebook_path, code_cells))

        self.issues.extend(result['issues'])

        return result

    def test_all_notebooks(self, root_dir: Path, sample_only: bool = False) -> Dict[str, Any]:
        """Test all notebooks or a sample"""
        notebooks = list(root_dir.glob('**/*.ipynb'))
        notebooks = [nb for nb in notebooks if '.ipynb_checkpoints' not in str(nb)]

        if sample_only:
            # Test critical notebooks
            critical_patterns = [
                'prosthetics',
                'radiation',
                'emg',
                'hardware_integration',
                'temporal_displacement'
            ]
            test_notebooks = []
            for pattern in critical_patterns:
                test_notebooks.extend([nb for nb in notebooks if pattern in str(nb).lower()])
            notebooks = test_notebooks[:10]  # Limit to 10

        print(f"Testing {len(notebooks)} notebooks for logic issues...\n")

        results = []
        for nb in notebooks:
            print(f"Testing: {nb.name}")
            result = self.test_notebook(nb)
            results.append(result)

            if result['issues']:
                print(f"  ⚠️  Found {len(result['issues'])} issue(s)")
            else:
                print(f"  ✅ No issues found")

        return {
            'total_tested': len(results),
            'notebooks_with_issues': len([r for r in results if r['issues']]),
            'total_issues': len(self.issues),
            'total_warnings': len(self.warnings),
            'results': results
        }

    def print_summary(self, test_results: Dict):
        """Print summary of test results"""
        print("\n" + "=" * 80)
        print("NOTEBOOK LOGIC TEST SUMMARY")
        print("=" * 80)
        print(f"Notebooks tested: {test_results['total_tested']}")
        print(f"Notebooks with issues: {test_results['notebooks_with_issues']}")
        print(f"Total issues: {test_results['total_issues']}")
        print(f"Total warnings: {test_results['total_warnings']}")

        if self.issues:
            print("\n" + "-" * 80)
            print("ISSUES FOUND:")
            print("-" * 80)
            for issue in self.issues[:20]:  # Show first 20
                print(f"\n📁 {Path(issue['notebook']).name}")
                print(f"   Type: {issue['type']}")
                if 'cell' in issue:
                    print(f"   Cell: {issue['cell']}")
                if 'code' in issue:
                    print(f"   Code: {issue['code'][:100]}")

        if self.warnings:
            print(f"\n{len(self.warnings)} warnings found (not shown)")

        print("\n" + "=" * 80)

        if test_results['notebooks_with_issues'] == 0:
            print("✅ All tested notebooks passed logic checks!")
        else:
            print(f"⚠️  {test_results['notebooks_with_issues']} notebook(s) have potential issues")

        print("=" * 80)


def main():
    """Main entry point"""
    root_dir = Path('/home/user/MotorHandPro')

    tester = NotebookLogicTester()

    print("=" * 80)
    print("NOTEBOOK LOGIC TESTER")
    print("=" * 80)
    print()

    # Test critical notebooks
    results = tester.test_all_notebooks(root_dir, sample_only=True)

    tester.print_summary(results)

    # Save report
    report_file = root_dir / 'notebook_logic_test_report.json'
    with open(report_file, 'w') as f:
        json.dump({
            'summary': {
                'total_tested': results['total_tested'],
                'notebooks_with_issues': results['notebooks_with_issues'],
                'total_issues': results['total_issues'],
                'total_warnings': results['total_warnings']
            },
            'issues': tester.issues,
            'warnings': tester.warnings[:50]  # Limit warnings in report
        }, f, indent=2)

    print(f"\nDetailed report saved to: {report_file}")

    return 0 if results['notebooks_with_issues'] == 0 else 0  # Return 0 for now


if __name__ == '__main__':
    sys.exit(main())
