# APL Primal Logic Core

High-performance mathematical implementation of the Primal Logic control framework using APL (Array Programming Language).

## Overview

This module implements the core Primal Logic control law with exponential memory weighting in APL, providing:

- **Concise notation**: APL's mathematical symbols match the theoretical formulation
- **Array operations**: Native support for multi-actuator control
- **High performance**: Optimized for matrix computations
- **Patent protection**: U.S. Provisional Patent Application No. 63/842,846

## Core Functions

### PrimalControl
```apl
PrimalControl ψ e KE λ
```
Implements: dψ/dt = -λ·ψ(t) + KE·e(t)
- `ψ`: Current state
- `e`: Error signal
- `KE`: Control gain
- `λ`: Exponential decay rate (Lightfoot Constant)

### SimulatePrimalLogic
```apl
SimulatePrimalLogic ψ0 e_vec KE λ Δt steps
```
Simulates the full system trajectory over time.

### MultiActuatorControl
```apl
MultiActuatorControl Ψ E KE_matrix Λ_matrix
```
Matrix-based control for multiple actuators simultaneously.

### QuantumResonanceField
```apl
QuantumResonanceField A λ t r σ
```
Calculates quantum-inspired resonance field: Φ(r,t) = A·exp(-λ·t)·exp(-|r|/σ)

## Constants

- **LIGHTFOOT_LAMBDA**: 0.16905 s⁻¹ (exponential decay rate)
- **DONTE_CONSTANT**: 149.9992314000 (fixed-point attractor)

## Integration Methods

1. **Euler**: Simple first-order integration (`EulerStep`)
2. **RK4**: Fourth-order Runge-Kutta for higher accuracy (`RK4Step`)

## Usage

### GNU APL
```bash
apl
      )LOAD core.apl
      result ← SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50
```

### Dyalog APL
```apl
)COPY core.apl
trajectory ← SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50
```

## FFI Export

Functions exported for Foreign Function Interface:
- `FFI_PrimalControl`: Core control law
- `FFI_SimulatePrimalLogic`: Full simulation
- `FFI_QuantumResonanceField`: Resonance field calculation

## Performance

Target: <1ms for 10x10 matrix operations

Benchmark:
```apl
BenchmarkPrimalLogic 1000  ⍝ 1000 iterations
```

## Mathematical Foundation

### Control Law
```
dψ/dt = -λ·ψ(t) + KE·e(t)
```

### Exponential Memory Weighting
```
W(t) = exp(-λ·t) for t ≥ 0
```

### Lipschitz Contractivity
```
|F'(D)| < 1  ⟹  Guaranteed convergence
```

### Energy Function (Lyapunov)
```
V(ψ) = 0.5·(ψ - D)²
dV/dt < 0  ⟹  Stability
```

## Applications

- Robotic motor control
- Multi-actuator synchronization
- LAM quantum resonance fields
- Space mission trajectory optimization
- Biomedical system regulation

## References

- Patent: U.S. Provisional Patent Application No. 63/842,846
- APL Language: [Dyalog APL](https://www.dyalog.com/), [GNU APL](https://www.gnu.org/software/apl/)
