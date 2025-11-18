#!/usr/bin/env python3
"""
PrimalLang CLI - Command-line interface for the PrimalLang compiler
"""
import sys
import argparse
from pathlib import Path
from typing import Optional

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from primallang.compiler import compile_to_python, parse
from primallang.runtime import PrimalConstants


class PrimalLangCLI:
    """Command-line interface for PrimalLang"""

    def __init__(self):
        self.parser = argparse.ArgumentParser(
            description='PrimalLang Compiler - Transpiler with Donte & Lightfoot Constants',
            formatter_class=argparse.RawDescriptionHelpFormatter,
            epilog="""
Examples:
  primallang run program.pml           # Compile and execute
  primallang compile program.pml       # Compile to Python
  primallang check program.pml         # Check syntax only
  primallang repl                      # Interactive REPL
  primallang constants                 # Show Primal Constants

Patent Pending: U.S. Provisional Patent Application No. 63/842,846
© 2025 Donte Lightfoot - The Phoney Express LLC
            """
        )

        self.parser.add_argument('command', choices=['run', 'compile', 'check', 'repl', 'constants'],
                                help='Command to execute')
        self.parser.add_argument('file', nargs='?', help='PrimalLang source file')
        self.parser.add_argument('-o', '--output', help='Output file for compiled code')
        self.parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')

    def run(self, args):
        """Execute CLI command"""
        if args.command == 'constants':
            self.show_constants()
        elif args.command == 'repl':
            self.repl()
        else:
            if not args.file:
                print(f"Error: {args.command} command requires a file argument")
                return 1

            if not Path(args.file).exists():
                print(f"Error: File not found: {args.file}")
                return 1

            if args.command == 'run':
                return self.run_file(args.file, args.verbose)
            elif args.command == 'compile':
                return self.compile_file(args.file, args.output, args.verbose)
            elif args.command == 'check':
                return self.check_file(args.file, args.verbose)

        return 0

    def show_constants(self):
        """Display Primal Constants"""
        print("=" * 70)
        print("PRIMAL CONSTANTS - Donte & Lightfoot Framework")
        print("=" * 70)
        print()

        constants = PrimalConstants.get_all()

        # Core constants
        print("Core Constants:")
        print(f"  D (Donte Constant)      = {constants['D']:.10f}")
        print(f"  λ (Lightfoot Constant)  = {constants['LAMBDA']:.10f}")
        print(f"  I3                      = {constants['I3']:.10f}")
        print(f"  S (Scaling Ratio)       = {constants['S']:.10f}")
        print()

        # Derived constants
        print("Derived Constants:")
        print(f"  τ (Time Constant)       = {constants['TAU']:.10f} seconds")
        print(f"  Xc (Cutoff Threshold)   = {constants['Xc']:.10f}")
        print(f"  F'(D) (Lipschitz)       = {constants['F_PRIME_D']:.12f}")
        print()

        # Golden ratio constants
        print("Golden Ratio Constants:")
        print(f"  φ (Phi)                 = {constants['PHI']:.15f}")
        print(f"  Golden Points           = {constants['GOLDEN_POINTS']}")
        print()

        print("=" * 70)
        print("Patent Pending: U.S. Provisional Patent Application No. 63/842,846")
        print("© 2025 Donte Lightfoot - The Phoney Express LLC")
        print("=" * 70)

    def run_file(self, filepath: str, verbose: bool = False) -> int:
        """Compile and execute a PrimalLang file"""
        try:
            # Read source
            with open(filepath, 'r') as f:
                source = f.read()

            if verbose:
                print(f"Compiling {filepath}...")

            # Compile to Python
            python_code = compile_to_python(source)

            if verbose:
                print("Generated Python code:")
                print("-" * 70)
                print(python_code)
                print("-" * 70)
                print("\nExecuting...")
                print("=" * 70)

            # Execute
            exec(python_code, {'__name__': '__main__'})

            if verbose:
                print("=" * 70)
                print("Execution completed successfully")

            return 0

        except SyntaxError as e:
            print(f"Syntax Error: {e}")
            return 1
        except Exception as e:
            print(f"Runtime Error: {e}")
            import traceback
            if verbose:
                traceback.print_exc()
            return 1

    def compile_file(self, filepath: str, output: Optional[str], verbose: bool = False) -> int:
        """Compile a PrimalLang file to Python"""
        try:
            # Read source
            with open(filepath, 'r') as f:
                source = f.read()

            if verbose:
                print(f"Compiling {filepath}...")

            # Compile to Python
            python_code = compile_to_python(source)

            # Determine output file
            if output:
                output_file = output
            else:
                output_file = Path(filepath).with_suffix('.py')

            # Write output
            with open(output_file, 'w') as f:
                f.write(python_code)

            print(f"Compiled {filepath} -> {output_file}")

            if verbose:
                print("\nGenerated code:")
                print("-" * 70)
                print(python_code)
                print("-" * 70)

            return 0

        except SyntaxError as e:
            print(f"Syntax Error: {e}")
            return 1
        except Exception as e:
            print(f"Error: {e}")
            if verbose:
                import traceback
                traceback.print_exc()
            return 1

    def check_file(self, filepath: str, verbose: bool = False) -> int:
        """Check syntax of a PrimalLang file"""
        try:
            # Read source
            with open(filepath, 'r') as f:
                source = f.read()

            if verbose:
                print(f"Checking {filepath}...")

            # Parse (will raise SyntaxError if invalid)
            ast = parse(source)

            print(f"✓ {filepath} - Syntax OK")

            if verbose:
                print(f"\nAST Summary:")
                print(f"  Statements: {len(ast.statements)}")
                print(f"  Lines: {len(source.splitlines())}")

            return 0

        except SyntaxError as e:
            print(f"✗ Syntax Error: {e}")
            return 1
        except Exception as e:
            print(f"✗ Error: {e}")
            if verbose:
                import traceback
                traceback.print_exc()
            return 1

    def repl(self):
        """Interactive REPL"""
        print("=" * 70)
        print("PrimalLang REPL - Interactive Mode")
        print("With Donte & Lightfoot Constants")
        print("Type 'exit' or Ctrl+D to quit, 'constants' to show constants")
        print("=" * 70)
        print()

        # REPL environment
        env = {}

        # Import runtime primitives
        exec("from primallang.runtime import *", env)
        exec("_constants = PrimalConstants.get_all()", env)
        exec("globals().update(_constants)", env)

        while True:
            try:
                # Read input
                line = input(">>> ")

                if line.strip().lower() == 'exit':
                    break
                elif line.strip().lower() == 'constants':
                    self.show_constants()
                    continue
                elif not line.strip():
                    continue

                # Try to compile and execute
                try:
                    python_code = compile_to_python(line)
                    exec(python_code, env)
                except SyntaxError as e:
                    # Try as expression
                    try:
                        result = eval(line, env)
                        if result is not None:
                            print(result)
                    except:
                        print(f"Syntax Error: {e}")
                except Exception as e:
                    print(f"Error: {e}")

            except EOFError:
                print("\nExiting REPL...")
                break
            except KeyboardInterrupt:
                print("\n(Use 'exit' or Ctrl+D to quit)")
                continue

        print("\nGoodbye!")


def main():
    """Main entry point"""
    cli = PrimalLangCLI()
    args = cli.parser.parse_args()
    return cli.run(args)


if __name__ == '__main__':
    sys.exit(main())
