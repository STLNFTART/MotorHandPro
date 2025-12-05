# Multi-Language Quickstart Guide

MotorHandPro now implements Primal Logic using **three powerful languages**, each optimized for specific domains:

- **APL**: Array-based mathematical operations
- **Prolog**: Logic-based reasoning and planning
- **D Language**: High-performance system programming

## Quick Start

### 1. Build All Modules

```bash
./integration/build_all.sh
```

This builds:
- ✓ D language executables and library
- ✓ Validates Prolog modules
- ✓ Checks APL interpreter availability

### 2. Run Individual Components

**D Language (Primal Logic Kernel):**
```bash
cd dlang
./primal_kernel
```

**D Language (Motor Hand Controller):**
```bash
./motor_hand
```

**APL (Primal Logic):**
```bash
apl
)LOAD apl/primal_logic/core.apl
trajectory ← SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50
```

**APL (NASA Simulation):**
```bash
apl
)LOAD apl/nasa_simulation/mars_mission.apl
mission ← MarsMissionProfile SHIELD_10G
```

**Prolog (LAM Reasoning):**
```bash
swipl -s prolog/lam_reasoning/core.pl
?- plan_task(trip_to_mars, Actions).
?- lam_cycle.
```

**Prolog (Regulatory Compliance):**
```bash
swipl -s prolog/regulatory/compliance.pl
?- check_all_compliance.
```

**Prolog (Blockchain Verification):**
```bash
swipl -s prolog/blockchain/contract_verification.pl
?- full_verification.
```

## Language Comparison

| Feature | APL | Prolog | D Language |
|---------|-----|--------|------------|
| **Best For** | Math, arrays | Rules, planning | Performance |
| **Speed** | Fast (array ops) | Medium | Very Fast |
| **Conciseness** | Very High | High | Medium |
| **Type Safety** | Dynamic | Dynamic | Static |
| **Learning Curve** | Steep | Moderate | Moderate |

## Architecture Overview

```
┌─────────────────────────────────────────────────┐
│                 Application Layer               │
│  (Python, TypeScript, Mobile Apps)              │
└────────────┬────────────────────────────────────┘
             │
    ┌────────┴────────┬──────────────┬─────────────┐
    │                 │              │             │
┌───▼────┐    ┌──────▼─────┐  ┌─────▼──────┐  ┌──▼───┐
│  APL   │    │  Prolog    │  │  D Lang    │  │ REST │
│ Engine │    │  Engine    │  │  Runtime   │  │ API  │
└───┬────┘    └──────┬─────┘  └─────┬──────┘  └──┬───┘
    │                │              │            │
    │         ┌──────┴──────────────┴────────────┘
    │         │
┌───▼─────────▼─────────────────────────┐
│     Integration & FFI Layer           │
│  (Shared libraries, IPC, JSON)        │
└───────────────────────────────────────┘
```

## What Each Language Does

### APL - Mathematical Core

**Responsibilities:**
- Primal Logic control law implementation
- NASA space mission radiation modeling
- Time-series data processing
- Matrix operations for multi-actuator control
- Statistical analysis

**Why APL?**
- Concise notation matches mathematical formulas
- Native array operations (10-100x faster than loops)
- Perfect for scientific computing

**Example:**
```apl
⍝ One line of APL can replace 20+ lines of Python
MovingAverage ← {(+⌿⍵)÷≢⍵}
```

### Prolog - Intelligence Layer

**Responsibilities:**
- LAM (Large Action Model) reasoning
- Task planning and goal decomposition
- Regulatory compliance checking (FDA/NHTSA/FAA)
- Blockchain smart contract verification
- Rule-based expert systems

**Why Prolog?**
- Declarative: Express *what*, not *how*
- Automatic inference and backtracking
- Perfect for rules and constraints

**Example:**
```prolog
% Define rules, get inference for free
can_transfer(From, To, Amount) :-
    balance_of(From, Balance),
    Balance >= Amount,
    \+ is_blacklisted(From).
```

### D Language - Performance Core

**Responsibilities:**
- Real-time control loops (< 100 μs latency)
- Motor hand robotic control
- High-performance data processing
- Embedded systems
- Drug safety optimization

**Why D?**
- C++ performance with memory safety
- Compile-time features (templates, CTFE)
- Native parallelism
- Perfect for real-time systems

**Example:**
```d
// Type-safe, fast, memory-safe
auto controller = new RealTimeController(100.0, 1.0, 0.01);
double new_state = controller.update(error);  // <100μs
```

## Installation

### APL

**Option 1: GNU APL (Free)**
```bash
sudo apt-get install apl  # Ubuntu/Debian
brew install gnu-apl      # macOS
```

**Option 2: Dyalog APL (Commercial, free personal)**
https://www.dyalog.com/

### Prolog

**SWI-Prolog (Recommended)**
```bash
sudo apt-get install swi-prolog  # Ubuntu/Debian
brew install swi-prolog          # macOS
```

### D Language

**DMD (Reference compiler)**
```bash
curl -fsS https://dlang.org/install.sh | bash -s dmd
```

**LDC (LLVM-based, better optimization)**
```bash
curl -fsS https://dlang.org/install.sh | bash -s ldc
```

## Project Structure

```
MotorHandPro/
├── apl/                          # APL modules
│   ├── primal_logic/             # Core math
│   ├── nasa_simulation/          # Space missions
│   └── data_analysis/            # Data pipeline
├── prolog/                       # Prolog modules
│   ├── lam_reasoning/            # LAM engine
│   ├── regulatory/               # Compliance
│   └── blockchain/               # Contract verification
├── dlang/                        # D language modules
│   ├── core_control/             # Primal kernel
│   ├── robotic_control/          # Motor hand
│   └── dub.json                  # Build config
├── integration/                  # Cross-language
│   └── build_all.sh              # Build script
└── MULTI_LANGUAGE_ARCHITECTURE.md
```

## Performance Benchmarks

| Component | Language | Latency | Throughput |
|-----------|----------|---------|------------|
| Primal Logic Kernel | D | 123 μs | 8,102 iter/s |
| Motor Hand Control | D | 50 μs | 1 kHz loop |
| Matrix Operations | APL | <1 ms | 10,000 ops/s |
| LAM Reasoning | Prolog | <10 ms | 100 queries/s |
| NASA Simulation | APL | 5 ms | 200 sims/s |

## Common Tasks

### Run Full Test Suite

```bash
# D language tests
cd dlang
dub test

# Prolog validation
swipl -s prolog/lam_reasoning/core.pl -g "lam_cycle, halt"
swipl -s prolog/regulatory/compliance.pl -g "check_all_compliance, halt"
swipl -s prolog/blockchain/contract_verification.pl -g "full_verification, halt"

# APL benchmarks
apl -f apl/primal_logic/core.apl
```

### Build Release Binaries

```bash
cd dlang
dub build --build=release --config=primal-kernel
dub build --build=release --config=motor-hand
```

### Profile Performance

```bash
cd dlang
dub build --build=profile
./primal_kernel
gprof primal_kernel gmon.out > profile.txt
```

## Integration Examples

### Python → D Language

```python
from ctypes import CDLL, c_double

lib = CDLL('./dlang/libmotorhandpro-dlang.so')
result = lib.primalControl(100.0, 0.1, 1.0, 0.16905)
```

### Python → Prolog

```python
from pyswip import Prolog

prolog = Prolog()
prolog.consult("prolog/lam_reasoning/core.pl")
for result in prolog.query("plan_task(trip_to_mars, X)"):
    print(result["X"])
```

### TypeScript → All

```typescript
// REST API gateway handles routing to appropriate language
const response = await fetch('/api/primal-logic/simulate', {
    method: 'POST',
    body: JSON.stringify({psi0: 100, steps: 50})
});
```

## Next Steps

1. **Read detailed docs:**
   - [APL Modules](apl/README.md)
   - [Prolog Modules](prolog/README.md)
   - [D Language Modules](dlang/README.md)

2. **Explore examples:**
   - Run each demo to see languages in action
   - Modify parameters and observe behavior

3. **Integration:**
   - See `integration/` for cross-language bindings
   - Build FFI interfaces for your language

4. **Contribute:**
   - Add new APL functions
   - Write Prolog rules
   - Implement D modules

## Troubleshooting

**APL not found:**
```bash
# Install GNU APL
wget http://ftp.gnu.org/gnu/apl/apl-1.8.tar.gz
tar xzf apl-1.8.tar.gz
cd apl-1.8 && ./configure && make && sudo make install
```

**Prolog errors:**
```bash
# Check SWI-Prolog version (need ≥8.0)
swipl --version

# Reinstall if needed
sudo apt-get install --reinstall swi-prolog
```

**D compiler errors:**
```bash
# Activate D environment
source ~/dlang/dmd-*/activate

# Or use LDC
source ~/dlang/ldc-*/activate
```

## Resources

- **APL**: [GNU APL Manual](https://www.gnu.org/software/apl/)
- **Prolog**: [SWI-Prolog Docs](https://www.swi-prolog.org/)
- **D**: [D Language Tour](https://tour.dlang.org/)
- **Patent**: U.S. Provisional Application No. 63/842,846

## Support

For issues or questions:
- APL: Check `apl/*/README.md`
- Prolog: Check `prolog/*/README.md`
- D Language: Check `dlang/README.md`
- Integration: Check `integration/README.md`

---

**Welcome to the multi-language future of MotorHandPro! 🚀**
