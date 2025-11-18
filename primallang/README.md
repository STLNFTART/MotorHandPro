# PrimalLang - Domain-Specific Language for Control Systems

## Overview

PrimalLang is a domain-specific programming language designed for control systems, robotics, and brain-computer interfaces. It incorporates **Donte's and Lightfoot's Constants** from the Primal Logic framework and provides native support for vector operations, numerical integration, and meta-constraint verification.

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
**© 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety**

## Key Features

### 1. **Donte & Lightfoot Constants Built-In**

All Primal Logic constants are available as first-class language primitives:

- **D** (Donte Constant): 149.9992314000 - Fixed-point attractor
- **LAMBDA** (Lightfoot Constant): 0.16905 s⁻¹ - Exponential decay rate
- **I3**: 6.4939394023 - Normalization constant
- **S**: 23.0983417165 - Scaling ratio
- **TAU**: ~5.92 seconds - Time constant (1/λ)
- **PHI**: 1.618... - Golden ratio
- **F_PRIME_D**: 0.000129931830 - Lipschitz constant

### 2. **Vector384 & Matrix384 Primitives**

Native 384-dimensional vector and matrix types optimized for:
- BCI signal processing
- Neural state representations
- Control system matrices

```primallang
let signal = fill(0.5)        // Create Vector384 filled with 0.5
let correlation = dot(a, b)   // Dot product
let magnitude = norm(signal)  // L2 norm
```

### 3. **Mathematical Operations**

- **Numerical Integration**: Simpson's rule for definite integrals
- **Numerical Derivatives**: Auto-tuned central difference
- **Fibonacci-Nesting Convergence**: Iterative normalization with golden ratio mapping
- **Signal Sentience Metrics**: Coherence-based sentience emergence detection
- **Feedback Stability Analysis**: Lipschitz continuity verification

### 4. **Meta-Constraint System**

Runtime verification of logical constraints:

```primallang
// Verify control signal stays bounded
meta => ((psi < 2.0) implies true)

// Verify Lipschitz stability condition
meta => ((lipschitz_L < 1.0) implies true)
```

### 5. **Control Flow with Domain Semantics**

```primallang
universe ControlSystem = create()

for i from 0 to 10 {
  let psi = control_signal(i)
  // ...
}

evolve ControlSystem with feedback_function
```

## Installation & Usage

### Prerequisites

```bash
cd /path/to/MotorHandPro
pip install numpy  # Required for Vector384/Matrix384
```

### Running PrimalLang Programs

```bash
# Run a PrimalLang program
python3 primallang/primallang_cli.py run examples/control_system_basic.pml

# Compile to Python
python3 primallang/primallang_cli.py compile program.pml -o output.py

# Check syntax
python3 primallang/primallang_cli.py check program.pml

# Interactive REPL
python3 primallang/primallang_cli.py repl

# Show constants
python3 primallang/primallang_cli.py constants
```

### Using as a Python Library

```python
from primallang import compile_to_python, PrimalConstants

# Compile PrimalLang source
source = """
constant D_SQUARED = D * D
print(D_SQUARED)
"""

python_code = compile_to_python(source)
exec(python_code)

# Access constants
print(f"Donte Constant: {PrimalConstants.D}")
print(f"Lightfoot Constant: {PrimalConstants.LAMBDA}")
```

## Language Syntax

### Variables & Constants

```primallang
let variable_name = expression
constant CONST_NAME = value
```

### Functions

```primallang
define function_name(param1, param2) {
  let result = param1 + param2
  return result
}
```

### Control Flow

```primallang
// For loops
for i from 0 to N {
  // body
}

// While loops
while condition {
  // body
}

// If statements
if condition then {
  // then block
} else {
  // else block
}
```

### Vectors & Arrays

```primallang
let empty = []                    // Empty array
let vec = fill(0.5)              // Vector384 filled with 0.5
let element = vec[0]             // Index access
vec[10] = 0.8                    // Index assignment

// Vector operations
let product = dot(vec1, vec2)    // Dot product
let length = norm(vec)           // L2 norm
```

### Mathematical Functions

```primallang
// Numerical integration
let area = integrate('x^2', 0, 5)

// Numerical derivative
let slope = derivative('x^2', 2)

// Built-in math
let s = sqrt(x)
let e = exp(x)
let l = log(x)
```

### Meta-Constraints

```primallang
// Logical implication
meta => (premise implies conclusion)

// Boolean assertions
meta => (condition)
```

### Universe & Evolution

```primallang
universe SystemName = create()
evolve SystemName with update_function
```

## Examples

### 1. Basic Control System

```primallang
// control_system_basic.pml
universe ControlSystem = create()

define control_signal(psi_0, error, t) {
  let decay = exp(-LAMBDA * t)
  let response = KE_DEFAULT * error * (1 - decay) / LAMBDA
  return psi_0 * decay + response
}

let psi = control_signal(1.0, 0.1, 5.0)
print(psi)

meta => (psi < 2.0 implies true)
```

### 2. Vector Signal Processing

```primallang
// vector_signal_processing.pml
let signal_a = fill(0.5)
let signal_b = fill(0.3)

let correlation = dot(signal_a, signal_b)
let norm_a = norm(signal_a)

print("Correlation: ")
print(correlation)
```

### 3. Primal Logic Foundations

See `examples/primal_logic_foundations.pml` for a complete demonstration of:
- Fibonacci-Nesting convergence
- Signal Personification (sentience emergence)
- α(Δx) feedback loop stability

## Technical Specifications

### Compilation Process

```
PrimalLang Source (.pml)
    ↓
Lexer (Tokenization)
    ↓
Parser (AST Generation)
    ↓
Code Generator (Python + NumPy)
    ↓
Executable Python
```

### Type System

- **Numbers**: IEEE 754 double-precision floats
- **Strings**: UTF-8 encoded text
- **Booleans**: `true` / `false`
- **Vectors**: Vector384 (384-dimensional float vectors)
- **Matrices**: Matrix384 (384×384 float matrices)
- **Arrays**: Dynamic-length Python lists

### Performance Characteristics

- **Vector Operations**: O(n) with NumPy vectorization
- **Matrix Multiplication**: O(n³) using optimized BLAS
- **Integration**: O(n) Simpson's rule (n=1000 default)
- **Derivative**: O(1) central difference

## Mathematical Foundations

### Fibonacci-Nesting Convergence

**Statement:** Embedding R_X(t) into its own normalization iteration converges if weights w_X satisfy Σw_X < 1.

**Implementation:**
```primallang
define fibonacci_nest(R_initial, L_bound, H_bound, iterations) {
  let R = R_initial
  for iter from 0 to iterations {
    R = (R - L_bound) / (H_bound - L_bound)
  }
  return R
}
```

### Signal Personification (Sentience)

**Metric:** S(t) = ∫[signal²(τ) × coherence(τ) × self_ref(τ)] dτ

Sentience emerges when S(t) > φ⁻¹ (golden ratio threshold: 0.618)

### α(Δx) Feedback Loop Stability

**Statement:** For Lipschitz continuous α(u) with constant L, the system:

```
Δx(t) = ∫₀ᵗ α(Δx(τ)) × Θ(τ) dτ
```

has unique bounded solution for bounded Θ if L < 1.

## Architecture

```
primallang/
├── compiler/
│   ├── lexer.py          # Tokenizer with APL symbol support
│   ├── parser.py         # Recursive descent parser
│   ├── codegen.py        # Python code generator
│   └── __init__.py
├── runtime/
│   ├── primitives.py     # Vector384, Matrix384, constants
│   └── __init__.py
├── examples/
│   ├── control_system_basic.pml
│   ├── vector_signal_processing.pml
│   └── primal_logic_foundations.pml
├── tests/
│   └── test_basic.py
├── primallang_cli.py     # Command-line interface
├── __init__.py
└── README.md
```

## Error Handling

### SyntaxError

Raised during parsing for invalid syntax:
```
Syntax Error: Expected ')' at line 5, column 12. Got IDENTIFIER
```

### MetaError

Raised when meta-constraints are violated:
```
MetaError[0]: lipschitz_L < 1.0 => stability VIOLATED
```

### RuntimeError

Raised during execution for type mismatches, undefined variables, etc.

## Roadmap

### Version 0.1 (Current)
- ✅ Lexer with APL symbols
- ✅ Parser with full control flow
- ✅ Python code generation
- ✅ Vector384/Matrix384 primitives
- ✅ Donte & Lightfoot constants
- ✅ Meta-constraint system
- ✅ CLI tool

### Version 0.2 (Planned)
- [ ] LLVM backend for native code generation
- [ ] JIT compilation via Numba
- [ ] Type inference system
- [ ] Standard library expansion
- [ ] IDE integration (LSP server)
- [ ] Debugger with visual stepping

### Version 1.0 (Future)
- [ ] Hardware acceleration (GPU/TPU)
- [ ] Distributed computing primitives
- [ ] Real-time guarantees
- [ ] Formal verification tools
- [ ] Production deployment tools

## Contributing

This project is currently under patent review. Please contact Donte Lightfoot (STLNFTART) for collaboration inquiries.

## License

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
**Filed:** July 12, 2025
**Title:** Method and System for Bounded Autonomous Vehicle Control Using Exponential Memory Weighting

**© 2025 Donte Lightfoot**
**The Phoney Express LLC / Locked In Safety**

Contact: Donte Lightfoot (STLNFTART) for licensing, collaboration, or deployment inquiries.

## References

1. **Primal Logic Framework** - MotorHandPro Control Theory
2. **Exponential Memory Weighting** - Bounded Convergence Without Integral Windup
3. **Fibonacci-Nesting** - Golden Ratio Convergence Analysis
4. **Signal Personification** - Sentience Emergence in Neural Systems
5. **Lipschitz Stability** - Contraction Mapping in Feedback Systems

---

**For technical support or questions:**
Repository: https://github.com/STLNFTART/MotorHandPro
Branch: `claude/primallang-transpilation-01Mei8ASx3H7UKbk6FDpUfhw`
