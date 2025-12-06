# MotorHandPro

High-precision robotic hand control and analysis framework integrating Primal Logic kernels with exponential memory weighting for bounded stability.

## Overview

MotorHandPro implements the Primal Logic control framework for robotic actuator control, using exponential decay of historical state to guarantee bounded convergence without integral windup.

**Key Innovation:** Exponential memory weighting ensures Lipschitz contractivity and prevents unbounded integration.

## Quick Setup

### Automated Setup (Recommended)

Use the automated setup script to configure your environment:

```bash
# Clone the repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Run automated setup
bash setup.sh --full
```

**Setup Options:**
- `--minimal` - Core dependencies only (~2-3 min)
- `--full` - Complete installation (~5-10 min, recommended)
- `--dev` - Include development tools

**See [SETUP.md](SETUP.md) for detailed installation guide.**

### Manual Setup

```bash
# Install Python dependencies
pip install -r requirements.txt
pip install -r lam_requirements.txt

# Install Node.js dependencies (optional)
npm install

# Run smoke test
python3 lam/smoke_test.py
```

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

## Repository Structure & Tokenization

MotorHandPro is organized into modular, well-documented components. Each directory serves a specific purpose with comprehensive README documentation.

### 📁 Core Control System

| Directory | Purpose | Key Files |
|-----------|---------|-----------|
| `/` (root) | Core Primal Logic implementation | `quant_full.h`, `quant_runtime.h`, `MotorHandPro.ino` |
| `/gen/` | Generated constants (compile-time) | `quant_bridge.h`, `quant_full.h` |
| `/analysis/` | Parameter analysis & visualization | `heatmap_fit.py`, validation reports |
| `/validation_results/` | Comparative validation plots | Step response, disturbance rejection |

### 🎛️ User Interfaces & Visualization

| Directory | Purpose | Technologies |
|-----------|---------|--------------|
| `/control_panel/` | Web-based real-time control | Three.js, Chart.js, WebSocket |
| `/comprehensive_visualizations/` | Mission simulation plots | Matplotlib, space scenarios |

### 🧠 LAM System (Large Action Model)

| Directory | Purpose | Key Features |
|-----------|---------|--------------|
| `/lam/` | Intelligent orchestration layer | Async control, WebSocket, MQTT |
| `/lam/core/` | Core LAM logic | State management, routing |
| `/lam/api/` | REST API endpoints | FastAPI, authentication |
| `/lam/temporal_displacement.py` | **NEW** Time-aware fields | 3 methods, causality-safe |

**LAM Temporal Displacement** (Latest Addition):
- `temporal_displacement.py` - Python implementation (3 methods)
- `temporal_displacement.d` - D language (25-100x faster)
- `test_temporal_displacement.py` - Comprehensive validation
- `TEMPORAL_DISPLACEMENT.md` - Complete documentation
- `QUICKSTART_TEMPORAL.md` - 5-minute tutorial

### 🔬 Experimental & Research

| Directory | Purpose | Language |
|-----------|---------|----------|
| `/extras/` | Experimental implementations | Various |
| `/extras/primal/` | Python Primal Logic variants | Python |
| `/extras/primal_lang/` | Domain-specific language (DSL) | Custom |
| `/extras/primal_llm/` | LLM-assisted control interface | JavaScript, HTML |
| `/extras/quant_final/` | High-performance kernel | D language |

### 🏗️ Infrastructure & Deployment

| Directory | Purpose | Technologies |
|-----------|---------|--------------|
| `/infrastructure/` | Production infrastructure | Docker, Nginx, Prometheus |
| `/k8s/` | Kubernetes deployment | K8s manifests, Helm |
| `/docker-compose.yml` | Local deployment | Docker Compose |

### 🔗 Integrations

| Directory | Purpose | Protocols |
|-----------|---------|-----------|
| `/integrations/` | External system integrations | Various APIs |
| `/node-red/` | Visual flow programming | MQTT, WebSocket |
| `/regulatory-api/` | FDA compliance interfaces | REST API |
| `/mobile/` | Mobile app (LAMApp) | React Native |

### 📊 Data & ML

| Directory | Purpose | Use Case |
|-----------|---------|----------|
| `/ml_datasets/` | Machine learning datasets | RAG, training data |
| `/biomedical_simulation/` | Biomedical modeling | Space missions, crew health |
| `/experiments/` | Experiment configurations | Parameter sweeps |

### 📚 Documentation

| Location | Content |
|----------|---------|
| `/docs/` | Comprehensive documentation hub |
| `/docs/ARCHITECTURE.md` | System architecture & design |
| `/docs/guides/USER_GUIDE.md` | Complete user guide |
| `/docs/guides/DEPLOYMENT.md` | Deployment across environments |
| `/docs/api/PYTHON_API.md` | Python API reference |
| `LICENSE` | Research evaluation license |
| `CONTRIBUTING.md` | Contribution guidelines |
| `CODE_OF_CONDUCT.md` | Community standards |

### 🗂️ Tokenization Schema

The repository follows a **modular tokenization strategy**:

1. **Core Components** (root level)
   - Mathematical constants and kernels
   - Arduino/embedded implementations
   - Benchmark data and analysis scripts

2. **System Layers** (subdirectories)
   - **Control Layer**: `/lam/` - Orchestration and coordination
   - **Interface Layer**: `/control_panel/`, `/node-red/` - User interaction
   - **Infrastructure Layer**: `/infrastructure/`, `/k8s/` - Deployment
   - **Integration Layer**: `/integrations/`, `/regulatory-api/` - External systems

3. **Research & Experimental** (`/extras/`)
   - Isolated from production code
   - Self-contained with own dependencies
   - Migration path: `extras/` → production when validated

4. **Documentation Co-location**
   - Each major directory has its own `README.md`
   - Specific documentation in `/docs/` for cross-cutting concerns
   - API references in `/docs/api/`

### 📦 Module Dependencies

```
┌─────────────────────────────────────────┐
│         User Interfaces                  │
│  (control_panel, node-red, mobile)      │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│      LAM Orchestration Layer            │
│   (lam/, temporal_displacement)         │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│       Primal Logic Kernel               │
│  (quant_full.h, kernel_v4.py, gen/)    │
└──────────────┬──────────────────────────┘
               │
               ▼
┌─────────────────────────────────────────┐
│    Hardware / Sensors / Actuators       │
│  (Arduino, Raspberry Pi, Serial/GPIO)   │
└─────────────────────────────────────────┘
```

### 🔍 Finding Specific Functionality

| Need | Location | File |
|------|----------|------|
| **Primal Logic math** | Root | `PRIMAL_LOGIC_FRAMEWORK.md` |
| **Temporal displacement** | `/lam/` | `TEMPORAL_DISPLACEMENT.md` |
| **Quick start** | `/lam/` | `QUICKSTART_TEMPORAL.md` |
| **API reference** | `/docs/api/` | `PYTHON_API.md` |
| **Deploy to production** | `/docs/guides/` | `DEPLOYMENT.md` |
| **User tutorial** | `/docs/guides/` | `USER_GUIDE.md` |
| **System architecture** | `/docs/` | `ARCHITECTURE.md` |
| **Hardware setup** | Root | `MotorHandPro.ino` |
| **Performance benchmarks** | `/lam/` | `benchmark_temporal_displacement.py` |
| **Integration examples** | `/lam/` | `example_distributed_control.py` |

### 📈 Codebase Statistics

- **Total Files**: ~500+ files
- **Core Implementation**: Python (60%), C++ (20%), D (10%), JS (10%)
- **Documentation**: 27 comprehensive README files
- **Lines of Code**: ~50,000+ lines
- **Test Coverage**: Smoke tests, benchmarks, validation suites

### 🎯 Quick Navigation

**New to MotorHandPro?** Start here:
1. Read this README for overview
2. Check `/docs/guides/USER_GUIDE.md` for detailed tutorial
3. Try `/lam/smoke_test.py` to verify installation
4. Explore `/lam/QUICKSTART_TEMPORAL.md` for latest features

**Want to deploy?** See:
1. `/docs/guides/DEPLOYMENT.md` - Deployment guide
2. `/docker-compose.yml` - Docker setup
3. `/k8s/` - Kubernetes manifests

**Looking for specific features?**
- **Temporal Displacement**: `/lam/TEMPORAL_DISPLACEMENT.md`
- **LAM System**: `/lam/README.md`
- **Primal Logic Theory**: `PRIMAL_LOGIC_FRAMEWORK.md`
- **Hardware Integration**: `MotorHandPro.ino`, `/docs/guides/USER_GUIDE.md`

### 🏷️ Semantic Versioning & Modules

Each major component follows semantic versioning:
- **Core Kernel**: Stable (v1.0+)
- **LAM System**: Active development (v0.9+)
- **Temporal Displacement**: New (v0.1+) ⚠️ Experimental
- **Integrations**: Various stages

**Import Paths** (Python):
```python
# Core Primal Logic
from extras.primal.kernel_v4 import PrimalKernel

# Temporal Displacement
from lam.temporal_displacement import TemporalDisplacedField

# LAM Integration
from lam.lam_temporal_integration import LAMTemporalController
```

**C++ Includes** (Arduino/Embedded):
```cpp
#include "gen/quant_full.h"        // Full implementation
#include "gen/quant_bridge.h"      // Constants only
```

**D Language** (High-performance):
```d
import lam.temporal_displacement;
```

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

