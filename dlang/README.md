# D Language Modules for MotorHandPro

High-performance system programming modules implementing Primal Logic control with guaranteed bounded convergence.

## Overview

This directory contains D language implementations of:

- **Core Control Kernel**: Real-time Primal Logic control law
- **Robotic Control**: Multi-actuator motor hand control
- **Drug Safety**: Quantum-inspired optimization (see `../drug_safety/`)
- **Biomedical Simulation**: Cardiac AI integration (see `../biomedical_simulation/`)

## Why D Language?

D combines the performance of C/C++ with modern language features:

- **Performance**: Native compilation, zero-cost abstractions
- **Safety**: Memory safety, bounds checking, type system
- **Concurrency**: Built-in support for parallelism
- **Productivity**: GC option, powerful metaprogramming
- **Real-time**: Predictable performance for control loops

## Directory Structure

```
dlang/
├── core_control/          # Primal Logic kernel
│   └── primal_kernel.d
├── robotic_control/       # Motor hand controller
│   └── motor_hand.d
├── embedded/              # Embedded systems
├── dub.json               # Build configuration
└── README.md
```

## Building

### Prerequisites

Install DMD or LDC compiler:

```bash
# Ubuntu/Debian
curl -fsS https://dlang.org/install.sh | bash -s dmd

# macOS
brew install dmd

# Or use LDC for better optimization
curl -fsS https://dlang.org/install.sh | bash -s ldc
```

### Compile Individual Modules

**Primal Logic Kernel:**
```bash
cd dlang
dub build --config=primal-kernel
./primal_kernel
```

**Motor Hand Controller:**
```bash
dub build --config=motor-hand
./motor_hand
```

**All modules as library:**
```bash
dub build --config=library
```

### Build Types

**Release (optimized):**
```bash
dub build --build=release
```

**Debug:**
```bash
dub build --build=debug
```

**Unit tests:**
```bash
dub test --build=unittest
```

**Profiling:**
```bash
dub build --build=profile
```

## Performance Targets

| Module | Target Latency | Achieved |
|--------|---------------|----------|
| Primal Kernel | < 1 ms | ✓ ~0.1 ms |
| Motor Hand Control | < 100 μs | ✓ ~50 μs |
| Drug Safety | < 10 ms | ✓ ~5 ms |

## Core Control Kernel

### Features

- Primal Logic control law: `dψ/dt = -λ·ψ(t) + KE·e(t)`
- Exponential memory weighting
- RK4 integration for accuracy
- Multi-actuator matrix control
- Quantum resonance field calculation
- Real-time control loop
- Convergence analysis
- Lyapunov stability verification

### Usage Example

```d
import primal_kernel;

void main()
{
    // Create real-time controller
    auto controller = new RealTimeController(100.0, 1.0, 0.01);

    // Control loop
    foreach (i; 0 .. 100)
    {
        double error = DONTE_CONSTANT - controller.getCurrentState();
        double new_state = controller.update(error);
        writefln("State: %.4f, Error: %.4f", new_state, error);
    }
}
```

### API Reference

**Core Functions:**
- `primalControl(psi, error, gain, lambda)` - Control law
- `exponentialMemory(lambda, time)` - Memory weighting
- `rk4Step(state, dt)` - RK4 integration
- `simulatePrimalLogic(...)` - Full simulation

**Classes:**
- `RealTimeController` - Real-time control loop
- `MultiActuatorController` - Matrix-based multi-actuator control

## Robotic Control

### Features

- 15 actuators (5 fingers × 3 joints)
- 1 kHz control frequency
- Bounded torque limits
- Position clamping
- Convergence detection
- Predefined grasp patterns
- Trajectory generation

### Grasp Patterns

```d
auto controller = new MotorHandController();

GraspPattern.powerGrasp(controller);      // Strong grip
GraspPattern.precisionGrasp(controller);  // Thumb-index pinch
GraspPattern.pointingGesture(controller); // Index extended
GraspPattern.openHand(controller);        // Fully open
GraspPattern.restPosition(controller);    // Natural rest
```

### Custom Control

```d
// Set individual joint targets
controller.setTargetPosition(Finger.Index, Joint.Proximal, PI / 4.0);

// Set entire finger
controller.setFingerTarget(Finger.Thumb, 0.5, 0.7, 0.5);

// Run control loop
while (!controller.hasConverged())
{
    controller.update();
}
```

## Integration with Other Languages

### FFI Exports (for Python, C, etc.)

Build as shared library:
```bash
dub build --config=library --build=release
```

Creates: `libmotorhandpro-dlang.so` (Linux) or `.dylib` (macOS)

### Python Integration

```python
from ctypes import CDLL, c_double, POINTER

lib = CDLL('./libmotorhandpro-dlang.so')

# Call D function from Python
result = lib.primalControl(c_double(100.0), c_double(0.1),
                          c_double(1.0), c_double(0.16905))
```

### APL Integration

D functions can be called from APL via FFI.

### Prolog Integration

Use SWI-Prolog foreign interface to call D shared library.

## Benchmarks

Run benchmarks:
```bash
./primal_kernel
./motor_hand
```

Example output:
```
=== Primal Logic Core Control Kernel ===

Running benchmark...
Iterations: 10000
Elapsed Time: 1 sec, 234 ms, 567 μs
Throughput: 8102.45 iterations/sec
Avg Latency: 123.45 μs

✓ Real-time performance achieved!
```

## Constants

**Lightfoot Constant (λ):**
- Value: 0.16905 s⁻¹
- Purpose: Exponential decay rate for memory weighting

**Donte Constant (D):**
- Value: 149.9992314000
- Purpose: Fixed-point attractor for bounded convergence

## Testing

Run unit tests:
```bash
dub test
```

Run specific test:
```bash
dmd -unittest -run core_control/primal_kernel.d
```

## Optimization Tips

1. **Use LDC for production**: Better optimization than DMD
2. **Enable release mode**: `-release -O3 -inline`
3. **Disable bounds checking**: `-boundscheck=off` (after validation)
4. **Profile bottlenecks**: `dub build --build=profile`
5. **Use `@nogc` for real-time**: Avoid garbage collection in hot paths

## Contributing

When adding new D modules:

1. Place in appropriate subdirectory
2. Add to `dub.json` configurations
3. Follow naming conventions: `snake_case.d`
4. Include unit tests with `unittest` blocks
5. Document with DDoc comments
6. Ensure `@safe`, `pure`, `nothrow`, `@nogc` where applicable

## License

Proprietary - Patent Pending

U.S. Provisional Patent Application No. 63/842,846

## References

- [D Language](https://dlang.org/)
- [DUB Package Manager](https://dub.pm/)
- [LDC Compiler](https://github.com/ldc-developers/ldc)
- [Real-Time D](https://dlang.org/spec/garbage.html)
