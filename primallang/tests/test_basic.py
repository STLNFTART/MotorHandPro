#!/usr/bin/env python3
"""
Basic tests for PrimalLang compiler and runtime
"""
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from primallang import compile_to_python, parse, PrimalConstants, Vector384, Matrix384


def test_constants():
    """Test that Primal Constants are accessible"""
    print("Testing Primal Constants...")

    assert PrimalConstants.D == 149.9992314000
    assert PrimalConstants.LAMBDA == 0.16905
    assert abs(PrimalConstants.TAU - 5.92) < 0.1
    assert PrimalConstants.PHI > 1.6 and PrimalConstants.PHI < 1.7

    print("  ✓ Constants OK")


def test_vector384():
    """Test Vector384 operations"""
    print("Testing Vector384...")

    # Create vectors
    v1 = Vector384.fill(0.5)
    v2 = Vector384.fill(0.3)

    # Operations
    v3 = v1 + v2
    assert abs(v3[0] - 0.8) < 1e-10

    # Dot product
    dot_product = v1.dot(v2)
    expected = 0.5 * 0.3 * 384
    assert abs(dot_product - expected) < 1e-6

    # Norm
    norm = v1.norm()
    expected_norm = 0.5 * (384 ** 0.5)
    assert abs(norm - expected_norm) < 1e-6

    print("  ✓ Vector384 OK")


def test_matrix384():
    """Test Matrix384 operations"""
    print("Testing Matrix384...")

    # Create identity matrix
    m = Matrix384.identity()
    assert m.data[0, 0] == 1.0
    assert m.data[0, 1] == 0.0

    # Matrix-vector multiplication
    v = Vector384.ones()
    result = m @ v
    assert abs(result[0] - 1.0) < 1e-10

    print("  ✓ Matrix384 OK")


def test_lexer():
    """Test lexer tokenization"""
    print("Testing Lexer...")

    from primallang.compiler import lex

    source = """
    let x = 5
    constant PI = 3.14159
    """

    tokens = lex(source)
    assert len(tokens) > 0
    assert tokens[-1].type.name == 'EOF'

    print("  ✓ Lexer OK")


def test_parser():
    """Test parser AST generation"""
    print("Testing Parser...")

    source = """
    let x = 5 + 3
    define add(a, b) {
      return a + b
    }
    """

    ast = parse(source)
    assert len(ast.statements) == 2

    print("  ✓ Parser OK")


def test_codegen():
    """Test code generation"""
    print("Testing Code Generation...")

    source = """
    let x = D + LAMBDA
    print(x)
    """

    python_code = compile_to_python(source)
    assert "PrimalConstants" in python_code
    assert "print" in python_code

    print("  ✓ Code Generation OK")


def test_simple_program():
    """Test complete program execution"""
    print("Testing Simple Program...")

    source = """
    constant VALUE = 10
    let result = VALUE * 2
    """

    python_code = compile_to_python(source)

    # Execute generated code
    env = {}
    exec(python_code, env)

    assert 'VALUE' in env
    assert env['VALUE'] == 10
    assert env['result'] == 20

    print("  ✓ Simple Program OK")


def test_control_law():
    """Test control system computation"""
    print("Testing Control Law...")

    source = """
    define exponential_decay(initial, t) {
      return initial * exp(-LAMBDA * t)
    }

    let psi_0 = 1.0
    let t = 5.0
    let psi = exponential_decay(psi_0, t)
    """

    python_code = compile_to_python(source)
    env = {}
    exec(python_code, env)

    # Verify exponential decay
    import math
    expected = 1.0 * math.exp(-PrimalConstants.LAMBDA * 5.0)
    assert abs(env['psi'] - expected) < 1e-6

    print("  ✓ Control Law OK")


def test_vector_operations():
    """Test vector operations in PrimalLang"""
    print("Testing Vector Operations...")

    source = """
    let v1 = fill(0.5)
    let v2 = fill(0.3)
    let d = dot(v1, v2)
    let n = norm(v1)
    """

    python_code = compile_to_python(source)
    env = {}
    exec(python_code, env)

    # Verify results
    expected_dot = 0.5 * 0.3 * 384
    expected_norm = 0.5 * (384 ** 0.5)

    assert abs(env['d'] - expected_dot) < 1e-6
    assert abs(env['n'] - expected_norm) < 1e-6

    print("  ✓ Vector Operations OK")


def test_meta_constraint():
    """Test meta-constraint verification"""
    print("Testing Meta-Constraints...")

    # This should pass
    source_pass = """
    let x = 0.5
    meta => (x < 1.0 implies true)
    """

    python_code = compile_to_python(source_pass)
    env = {}
    exec(python_code, env)  # Should not raise

    print("  ✓ Meta-Constraints OK")


def run_all_tests():
    """Run all tests"""
    print("=" * 70)
    print("PrimalLang Test Suite")
    print("=" * 70)
    print()

    tests = [
        test_constants,
        test_vector384,
        test_matrix384,
        test_lexer,
        test_parser,
        test_codegen,
        test_simple_program,
        test_control_law,
        test_vector_operations,
        test_meta_constraint,
    ]

    passed = 0
    failed = 0

    for test_func in tests:
        try:
            test_func()
            passed += 1
        except Exception as e:
            print(f"  ✗ FAILED: {e}")
            failed += 1
            import traceback
            traceback.print_exc()

    print()
    print("=" * 70)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 70)

    return failed == 0


if __name__ == '__main__':
    success = run_all_tests()
    sys.exit(0 if success else 1)
