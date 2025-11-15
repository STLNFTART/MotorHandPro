# MotorHandPro

High-precision robotic hand control and analysis framework integrating Primal Logic kernels with exponential memory weighting for bounded stability.

## Overview

MotorHandPro implements the Primal Logic control framework for robotic actuator control, using exponential decay of historical state to guarantee bounded convergence without integral windup.

**Key Innovation:** Exponential memory weighting ensures Lipschitz contractivity and prevents unbounded integration.

## Complete Variable Definitions

### Core State Variables

- **ψ(t)** [psi]: Control command signal to actuator (position/velocity command)
  - Units: Varies by actuator (typically dimensionless or rad/s)
  - Typical range: 1.0 to 1.5 in benchmark data
  - Physical meaning: The setpoint command sent to the motor controller

- **γ(t)** [gamma]: Tracking error signal or error derivative
  - Units: Same as ψ or derivative thereof
  - Typical range: 0.004 to 0.12 in benchmark data
  - Physical meaning: Difference between desired and actual actuator response

- **Ec(t)**: Control energy functional (Lyapunov-like stability metric)
  - Definition: `Ec(t) = ∫₀^t ψ(τ)·γ(τ) dτ`
  - Units: Energy-like (product of command × error integrated over time)
  - Purpose: Serves as a Lyapunov-like metric ensuring bounded convergence
  - Stability condition: Lipschitz constant < 1 guarantees Ec remains bounded

### Control Parameters

- **λ (lambda)**: Lightfoot constant = 0.16905 s⁻¹
  - Alternative names: `KERNEL_MU`, `mu` in code
  - Physical meaning: Exponential decay rate for memory weighting
  - Time constant: τ = 1/λ ≈ 5.92 seconds
  - Effect: System "forgets" ~63% of past state every 5.92 seconds

- **KE**: Proportional error gain
  - Typical values: 0.0, 0.3, 0.5 (see benchmark runs)
  - Purpose: Controls responsiveness to tracking error
  - Tuning: Higher KE = faster response but potential overshoot

- **μ (mu)**: Kernel iteration parameter (numerically equal to λ)
  - Value: 0.169050000000
  - Used in fixed-point iteration for stability analysis

### Universal Constants

- **D** (Donte constant): 149.9992314000
  - Alternative names: `DONTE_CONSTANT`, `PLANCK_D`, `D0`
  - Physical meaning: Fixed-point attractor of the Primal Logic kernel
  - Stability role: System naturally converges toward this value
  - Derivation: Emerges from Planck tail cutoff calculation

- **I3**: 6.4939394023
  - Purpose: Normalization constant for energy integrals
  - Used in Planck tail series calculations

- **S** (Scaling ratio): 23.0983417165
  - Definition: S = D / I3
  - Physical meaning: Fundamental ratio relating control authority to energy dissipation

### Derived Quantities (from quant_full.h)

- **Xc** (Cutoff threshold): ≈ 19.358674138784
  - Found by solving: planckTail(Xc)/I3 = 0.000005124
  - Purpose: Threshold where tail series becomes negligible

- **F'(D)** (Lipschitz constant at D): ≈ 0.000129931830
  - Definition: `c·μ·exp(-μ·D)` where `c = (150-D)·exp(μ·D)`
  - Stability proof: F'(D) < 1 proves contraction mapping
  - Guarantees: Bounded convergence to fixed point

## Control Law

The simplified control equation implemented is:

```
dψ/dt = -λ·ψ(t) + KE·e(t)

where:
  e(t) = y_desired(t) - y_actual(t)  [tracking error]
  γ(t) = e(t) or de/dt               [error signal]
  λ = 0.16905                        [exponential decay rate]
```

This is a specialization of the general Primal Logic equation:

```
dψ/dt = A(t)|u(t)⟩ - Λψ(t) + K[y_d(t) - y(t)] + γΓ(t)
```

For the full mathematical framework, see `PRIMAL_LOGIC_FRAMEWORK.md`.

## Data Format

Benchmark runs are stored as CSV files with the following format:

```csv
# MU=0.16905 KE=0.00000
# Core: D0=149.9992314000 I3=6.4939394023 S=23.0983417165 F'(D0)=0.000129931830
# t,psi,gamma,Ec
0.00,1.0071595000,0.0041887679,0.0000000000
0.01,1.0143597383,0.0083891661,0.0000031246
...
```

**Columns:**
- `t`: Time (seconds)
- `psi`: Control command ψ(t)
- `gamma`: Error signal γ(t)
- `Ec`: Integrated control energy Ec(t)

## Hardware Implementation

The Arduino implementation (`MotorHandPro.ino`) computes the Primal Logic constants at runtime using the Planck tail series and fixed-point iteration.

**Files:**
- `MotorHandPro.ino`: Main Arduino sketch
- `quant_full.h`: Complete Primal Logic kernel implementation
- `quant_runtime.h`: Lightweight runtime version

**Serial Output:**
- Baud rate: 115200
- Outputs: D, I3, S, Xc, μ, c, F'(D), x* (fixed point)

## Analysis Tools

- `analyze_runs.py`: Processes CSV benchmark data
  - Computes: max ψ, zero-crossing time, damping slope, Lipschitz estimate
  - Generates: Time-series plots and summary statistics

- `analysis/heatmap_fit.py`: Visualizes parameter sensitivity

## Additional Modules

### Drug Safety Modeling System (D Language)

A high-performance drug safety modeling system implemented in D programming language with quantum-inspired optimization.

**Location:** `/drug_safety/`

**Features:**
- Quantum-inspired memory lattice for model state storage
- Convergence detection with pattern analysis
- Algorithm integration framework
- Meta-learning controller for adaptive optimization
- Native complex number support

**Quick Start:**
```bash
cd drug_safety
./build.sh --run
```

**Documentation:** See [drug_safety/README.md](drug_safety/README.md) for complete documentation.

---

## Usage

---
**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846 — Filed July 12, 2025

---
> **Notice:** Repository for research evaluation only.  
> **Patent Pending:** U.S. Provisional Patent Application No. 63/842,846 — Filed July 12, 2025  
> Method and System for Bounded Autonomous Vehicle Control Using Exponential Memory Weighting  
> © 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety.  
> Contact: Donte Lightfoot (STLNFTART) for collaboration, licensing, or deployment inquiries.

