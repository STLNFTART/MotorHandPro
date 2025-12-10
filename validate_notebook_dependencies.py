#!/usr/bin/env python3
"""
Validate that all required dependencies for notebooks are available
"""

import importlib
import sys
import json
from pathlib import Path
from typing import Set, Dict, List


def extract_imports_from_notebook(notebook_path: Path) -> Set[str]:
    """Extract all import statements from a notebook"""
    imports = set()

    try:
        with open(notebook_path, 'r', encoding='utf-8') as f:
            nb = json.load(f)

        for cell in nb.get('cells', []):
            if cell.get('cell_type') != 'code':
                continue

            source = cell.get('source', [])
            if isinstance(source, list):
                code = ''.join(source)
            else:
                code = source

            for line in code.split('\n'):
                line = line.strip()

                # Skip comments and empty lines
                if not line or line.startswith('#'):
                    continue

                # Parse import statements
                if line.startswith('import '):
                    # import module or import module as alias
                    parts = line.replace('import ', '').split(',')
                    for part in parts:
                        module = part.split(' as ')[0].split('.')[0].strip()
                        if module:
                            imports.add(module)

                elif line.startswith('from '):
                    # from module import something
                    try:
                        module = line.split('from ')[1].split(' import')[0].split('.')[0].strip()
                        if module:
                            imports.add(module)
                    except:
                        pass

    except Exception as e:
        print(f"Warning: Could not parse {notebook_path}: {e}")

    return imports


def check_module_availability(module_name: str) -> Dict:
    """Check if a module is available"""
    result = {
        'module': module_name,
        'available': False,
        'version': None,
        'error': None
    }

    try:
        mod = importlib.import_module(module_name)
        result['available'] = True

        # Try to get version
        if hasattr(mod, '__version__'):
            result['version'] = mod.__version__
        elif hasattr(mod, 'VERSION'):
            result['version'] = str(mod.VERSION)

    except ImportError as e:
        result['error'] = str(e)
    except Exception as e:
        result['error'] = f"Unexpected error: {str(e)}"

    return result


def main():
    """Main entry point"""
    root_dir = Path('/home/user/MotorHandPro')

    # Find all notebooks
    notebooks = list(root_dir.glob('**/*.ipynb'))
    notebooks = [nb for nb in notebooks if '.ipynb_checkpoints' not in str(nb)]

    print("=" * 80)
    print("NOTEBOOK DEPENDENCY VALIDATOR")
    print("=" * 80)
    print(f"\nScanning {len(notebooks)} notebooks for dependencies...\n")

    # Collect all imports
    all_imports = set()
    notebook_imports = {}

    for nb in notebooks:
        nb_imports = extract_imports_from_notebook(nb)
        notebook_imports[str(nb)] = nb_imports
        all_imports.update(nb_imports)

    # Filter out standard library modules
    stdlib_modules = {
        'os', 'sys', 'json', 'time', 'datetime', 'collections', 'itertools',
        'functools', 'math', 'random', 'pathlib', 'typing', 'io', 'warnings',
        'copy', 'abc', 're', 'traceback', 'logging', 'unittest', 'subprocess',
        'threading', 'multiprocessing', 'pickle', 'csv', 'sqlite3', 'socket',
        'urllib', 'http', 'email', 'html', 'xml', 'hashlib', 'base64', 'uuid',
        'tempfile', 'shutil', 'glob', 'enum', 'dataclasses', 'contextlib',
        'argparse', 'configparser', 'queue', 'heapq', 'bisect', 'array',
        'struct', 'codecs', 'weakref', 'gc', 'inspect', 'platform'
    }

    third_party_imports = sorted([m for m in all_imports if m not in stdlib_modules])

    print(f"Found {len(all_imports)} unique imports")
    print(f"  - {len(all_imports) - len(third_party_imports)} standard library")
    print(f"  - {len(third_party_imports)} third-party packages\n")

    # Check availability of third-party packages
    print("Checking third-party package availability:")
    print("-" * 80)

    available = []
    missing = []

    for module in third_party_imports:
        result = check_module_availability(module)

        if result['available']:
            available.append(result)
            status = "✅"
            version_info = f" (v{result['version']})" if result['version'] else ""
            print(f"{status} {module}{version_info}")
        else:
            missing.append(result)
            status = "❌"
            print(f"{status} {module} - {result['error']}")

    # Summary
    print("\n" + "=" * 80)
    print("DEPENDENCY SUMMARY")
    print("=" * 80)
    print(f"Total third-party packages: {len(third_party_imports)}")
    print(f"Available: {len(available)}")
    print(f"Missing: {len(missing)}")

    if missing:
        print("\nMissing packages:")
        for pkg in missing:
            print(f"  - {pkg['module']}")

        print("\nTo install missing packages:")
        missing_names = [pkg['module'] for pkg in missing]
        print(f"  pip install {' '.join(missing_names)}")

    # Save report
    report = {
        'total_notebooks': len(notebooks),
        'total_imports': len(all_imports),
        'third_party_imports': len(third_party_imports),
        'available_packages': [pkg['module'] for pkg in available],
        'missing_packages': [pkg['module'] for pkg in missing],
        'package_details': available + missing
    }

    report_file = root_dir / 'notebook_dependencies_report.json'
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)

    print(f"\nDetailed report saved to: {report_file}")
    print("=" * 80)

    # Return exit code based on critical dependencies
    critical_deps = {'numpy', 'scipy', 'matplotlib', 'pandas'}
    missing_critical = critical_deps.intersection(set([pkg['module'] for pkg in missing]))

    if missing_critical:
        print(f"\n⚠️  CRITICAL: Missing essential packages: {missing_critical}")
        return 1

    return 0


if __name__ == '__main__':
    sys.exit(main())
