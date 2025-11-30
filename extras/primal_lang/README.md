# Primal Lang - Domain-Specific Language for Primal Logic

Experimental domain-specific language (DSL) for expressing and compiling Primal Logic control laws.

## Overview

**Primal Lang** is a high-level programming language designed specifically for:
- Declarative control law specification
- Automatic code generation for embedded systems
- Educational tool for teaching Primal Logic concepts
- Rapid prototyping of control algorithms

## Status

⚠️ **EXPERIMENTAL** - Research prototype

- Parser: Partial implementation
- Compiler: In development
- Runtime: Virtual environment based
- Stability: Not production-ready

## Language Features

### Declarative Control Syntax

```primal
// Define constants
const lambda = 0.16905;
const D = 149.9992314000;
const KE = 0.3;

// Define control law
control MotorHandController {
    state psi: double;
    state gamma: double;
    state Ec: double;

    // Primal Logic dynamics
    dynamics {
        d(psi)/dt = -lambda * psi + KE * gamma;
        d(gamma)/dt = error_signal();
        d(Ec)/dt = psi * gamma;
    }

    // Stability constraints
    constraints {
        lipschitz_constant < 1.0;
        Ec >= 0.0;
    }

    // Output
    output actuator_command = psi;
}
```

### Built-in Functions

**Mathematical:**
- `exp(x)` - Exponential
- `sin(x)`, `cos(x)` - Trigonometric
- `sqrt(x)` - Square root
- `abs(x)` - Absolute value

**Primal Logic Specific:**
- `planck_tail(X)` - Planck tail series
- `fixed_point_iter(f, x0)` - Fixed-point iteration
- `lipschitz_const(f, x)` - Lipschitz constant estimation

**Control Theory:**
- `error_signal()` - Tracking error computation
- `saturate(x, min, max)` - Saturation limits
- `filter(x, tau)` - Low-pass filter

### Type System

**Primitive Types:**
- `double` - 64-bit floating-point
- `int` - 32-bit integer
- `bool` - Boolean
- `vector<T>` - Dynamic array
- `matrix<T>` - 2D matrix

**Control Types:**
- `state<T>` - Time-varying state variable
- `parameter<T>` - Constant parameter (compile-time or runtime)
- `signal<T>` - Input/output signal

### Compilation Targets

**Supported:**
1. **Arduino C++** - Generate .ino and .h files
2. **Python** - Generate standalone Python module
3. **C** - Generate ANSI C for embedded systems
4. **MATLAB/Simulink** - Generate S-function blocks

**Planned:**
5. **Rust** - Safe systems programming
6. **VHDL/Verilog** - FPGA synthesis
7. **WebAssembly** - Browser execution

## Installation

```bash
cd extras/primal_lang

# Create virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies (when available)
pip install -r requirements.txt  # Future: primal-lang compiler

# Test installation
primal --version  # Future: after compiler implementation
```

## Usage

### Compiling Primal Programs

```bash
# Compile to Arduino C++
primal compile --target arduino controller.primal -o MotorHandController.ino

# Compile to Python
primal compile --target python controller.primal -o controller_module.py

# Compile to C
primal compile --target c controller.primal -o controller.c

# With optimization
primal compile --target arduino --optimize -O2 controller.primal
```

### Interactive REPL

```bash
primal repl

> const lambda = 0.16905;
> const D = planck_depth();
D = 149.9992314000

> def control_law(psi, gamma):
>     return -lambda * psi + 0.3 * gamma;

> simulate(control_law, duration=10.0, dt=0.01);
[Simulation output...]
```

### Validation Mode

```bash
# Validate against reference implementation
primal validate controller.primal --reference ../primal/kernel_v4.py

# Output:
# ✓ Stability constraints satisfied
# ✓ Lipschitz constant F'(D) = 0.000129931830 < 1.0
# ✓ Simulation matches reference (error < 1e-10)
```

## Example Programs

### Simple PD Controller

```primal
// simple_pd.primal
const Kp = 1.0;
const Kd = 0.1;

control PDController {
    input reference: double;
    input feedback: double;

    state error: double;
    state error_derivative: double;

    dynamics {
        error = reference - feedback;
        error_derivative = d(error)/dt;
    }

    output command = Kp * error + Kd * error_derivative;
}
```

### Full Primal Logic Controller

```primal
// primal_controller.primal
import primal.constants;  // Imports D, lambda, I3, etc.

control PrimalController {
    // Parameters
    parameter lambda = KERNEL_MU;  // 0.16905
    parameter KE = 0.3;

    // States
    state psi: double = 0.0;
    state gamma: double = 0.0;
    state Ec: double = 0.0;

    // Inputs
    input desired_position: double;
    input actual_position: double;

    // Control law
    dynamics {
        gamma = desired_position - actual_position;
        d(psi)/dt = -lambda * psi + KE * gamma;
        d(Ec)/dt = psi * gamma;
    }

    // Stability assertion
    assert {
        lipschitz_constant(psi) < 1.0;
        Ec >= 0.0 && Ec < infinity;
    }

    // Output
    output actuator_command = psi;

    // Telemetry
    monitor {
        log("psi", psi);
        log("Ec", Ec);
        log("gamma", gamma);
    }
}
```

### Multi-Agent Swarm

```primal
// swarm_control.primal
const N = 10;  // Number of agents

control SwarmController {
    state psi[N]: vector<double>;
    state positions[N]: vector<double>;

    dynamics {
        for i in 0..N {
            // Primal Logic for each agent
            d(psi[i])/dt = -lambda * psi[i] + consensus_term(i);
        }
    }

    function consensus_term(i: int) -> double {
        // Average neighbor positions
        sum = 0.0;
        for j in neighbors(i) {
            sum += positions[j] - positions[i];
        }
        return sum / |neighbors(i)|;
    }
}
```

## Language Grammar (BNF)

```bnf
<program> ::= <import>* <control_def>+

<import> ::= "import" <identifier> ";"

<control_def> ::= "control" <identifier> "{" <control_body> "}"

<control_body> ::= <parameter_def>* <state_def>* <input_def>* <dynamics>* <output_def>*

<parameter_def> ::= "parameter" <identifier> "=" <expression> ";"

<state_def> ::= "state" <identifier> ":" <type> ("=" <expression>)? ";"

<dynamics> ::= "dynamics" "{" <equation>+ "}"

<equation> ::= "d(" <identifier> ")/dt" "=" <expression> ";"
             | <identifier> "=" <expression> ";"

<expression> ::= <number>
               | <identifier>
               | <expression> <binop> <expression>
               | <unop> <expression>
               | <function_call>
               | "(" <expression> ")"

<binop> ::= "+" | "-" | "*" | "/" | "^"

<unop> ::= "-" | "!"

<function_call> ::= <identifier> "(" <arg_list>? ")"
```

## Compiler Architecture

```
┌─────────────┐
│ .primal file│
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Lexer     │  Tokenization
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Parser    │  AST Construction
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Semantic   │  Type checking, validation
│  Analysis   │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│  Optimizer  │  Constant folding, etc.
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ Code Gen    │  Target-specific output
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ .ino/.py/.c │
└─────────────┘
```

## Development Status

### Implemented
- [x] Basic grammar specification
- [x] Constant definitions
- [x] Virtual environment setup

### In Progress
- [ ] Lexer implementation
- [ ] Parser (partial)
- [ ] Type system (design phase)

### Planned
- [ ] Code generator (Arduino)
- [ ] Code generator (Python)
- [ ] Standard library
- [ ] Optimization passes
- [ ] Error reporting
- [ ] IDE integration (VS Code extension)

## Contributing to Language Development

Areas for contribution:

1. **Compiler Frontend:**
   - Lexer improvements
   - Parser completion
   - Error recovery

2. **Type System:**
   - Type inference
   - Generic types
   - Dimensional analysis (units)

3. **Code Generation:**
   - Additional target platforms
   - Optimization passes
   - Dead code elimination

4. **Standard Library:**
   - Control algorithms
   - Mathematical functions
   - Utility modules

5. **Tooling:**
   - Syntax highlighting
   - Language server protocol (LSP)
   - Debugger integration

## Educational Use

**Primal Lang** is designed for teaching:

1. **Control Theory Courses:**
   - Declarative control law specification
   - Visual feedback through simulation
   - Automatic stability checking

2. **Embedded Systems:**
   - High-level to low-level translation
   - Understanding code generation
   - Performance vs. abstraction trade-offs

3. **Programming Languages:**
   - DSL design
   - Compiler construction
   - Domain-specific optimization

## Related Work

**Similar DSLs:**
- **Modelica** - Multi-domain modeling language
- **Ptolemy II** - Actor-oriented design
- **Simulink** - Visual control system design
- **LabVIEW** - Graphical programming for instrumentation

**Primal Lang differentiators:**
- Native Primal Logic constructs
- Exponential memory weighting primitives
- Embedded target optimization
- Stability-by-construction guarantees

## Future Directions

1. **Formal Verification:**
   - Prove stability automatically
   - Bounded execution time guarantees
   - Safety-critical certification

2. **Visual Programming:**
   - Block diagram editor
   - Visual state machine design
   - Integration with control_panel/

3. **Machine Learning Integration:**
   - Differentiable controllers
   - Neural-Primal hybrid systems
   - Reinforcement learning environments

## Limitations

- **Not Turing-complete** (by design - no arbitrary loops)
- **Real-time constraints** (execution time bounds required)
- **Limited standard library** (minimal functions available)
- **No dynamic memory** (embedded target compatibility)

## Related Documentation

- [Extras Overview](../README.md) - Parent directory documentation
- [Primal Logic Framework](../../PRIMAL_LOGIC_FRAMEWORK.md) - Mathematical foundations
- [Main README](../../README.md) - System overview

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

**Language Design Collaboration:** Contact Donte Lightfoot (STLNFTART) for academic partnerships in programming language research.
