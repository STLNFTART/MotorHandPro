# APL Modules for MotorHandPro

Array-based mathematical implementations leveraging APL's concise notation for high-performance computations.

## Overview

APL (Array Programming Language) excels at:
- **Mathematical operations**: Natural notation matching theoretical formulations
- **Array processing**: Native multi-dimensional array support
- **Concise code**: Express complex operations in single lines
- **Performance**: Optimized array operations

## Modules

### 1. Primal Logic Core (`primal_logic/`)

Mathematical implementation of the Primal Logic control framework.

**Key Functions:**
- `PrimalControl` - Core control law: dψ/dt = -λ·ψ(t) + KE·e(t)
- `SimulatePrimalLogic` - Full system simulation
- `MultiActuatorControl` - Matrix-based multi-actuator control
- `QuantumResonanceField` - Quantum resonance calculations

**Constants:**
- Lightfoot Lambda: 0.16905 s⁻¹
- Donte Constant: 149.9992314000

**Usage:**
```apl
)LOAD primal_logic/core.apl
trajectory ← SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50
```

### 2. NASA Mars Mission Simulation (`nasa_simulation/`)

Radiation exposure modeling and crew health tracking.

**Features:**
- GCR (Galactic Cosmic Rays) modeling
- SPE (Solar Particle Events) simulation
- Multi-crew consciousness tracking
- Shield effectiveness analysis
- Mission profiles (180d, 500d, 860d)

**Usage:**
```apl
)LOAD nasa_simulation/mars_mission.apl
mission ← MarsMissionProfile SHIELD_10G
health ← CrewHealthDashboard 6 860 SHIELD_10G
```

### 3. Data Processing Pipeline (`data_analysis/`)

High-performance data analytics and signal processing.

**Capabilities:**
- Time series processing (moving average, EWMA)
- Statistical analysis (mean, std dev, quartiles)
- Signal processing (filters, FFT, peak detection)
- Anomaly detection
- Primal Logic weighted statistics

**Usage:**
```apl
)LOAD data_analysis/pipeline.apl
processed ← DataPipeline raw_data
ma ← MovingAverage data 5
anomalies ← ZScoreAnomaly data 2.5
```

## Installation

### GNU APL (Free, Open Source)

**Ubuntu/Debian:**
```bash
sudo apt-get install apl
```

**macOS:**
```bash
brew install gnu-apl
```

**From source:**
```bash
wget http://ftp.gnu.org/gnu/apl/apl-1.8.tar.gz
tar xzf apl-1.8.tar.gz
cd apl-1.8
./configure && make && sudo make install
```

### Dyalog APL (Commercial, Free Personal Edition)

Download from: https://www.dyalog.com/download-zone.htm

## Running APL Code

### Interactive Mode

```bash
apl
```

Then load a workspace:
```apl
)LOAD primal_logic/core.apl
result ← SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50
```

### Script Mode

```bash
apl -f primal_logic/core.apl
```

### From Command Line

```bash
echo "2 + 2" | apl
```

## APL Primer

### Basic Symbols

| Symbol | Meaning | Example |
|--------|---------|---------|
| `←` | Assignment | `x ← 5` |
| `⍴` | Shape/Reshape | `3 4 ⍴ ⍳12` |
| `⍳` | Index generator | `⍳5` → `1 2 3 4 5` |
| `+` `-` `×` `÷` | Arithmetic | `2 + 3` → `5` |
| `*` | Power/Exp | `*1` → `e` |
| `○` | Pi/Trig | `○1` → `π`, `1○x` → `sin(x)` |
| `⌈` `⌊` | Ceiling/Floor | `⌈2.3` → `3` |
| `∣` | Absolute value | `∣¯5` → `5` |
| `+/` | Sum | `+/1 2 3` → `6` |
| `×/` | Product | `×/1 2 3` → `6` |
| `⌈/` `⌊/` | Max/Min | `⌈/1 5 3` → `5` |

### Array Operations

```apl
⍝ Create array
A ← 3 4 ⍴ ⍳12

⍝ Matrix multiplication
C ← A +.× B

⍝ Element-wise operations
D ← A × 2

⍝ Transpose
E ← ⍉A
```

### Control Structures

```apl
⍝ For loop
:For i :In ⍳10
    result[i] ← i * 2
:EndFor

⍝ If statement
:If condition
    action
:Else
    other_action
:EndIf
```

## Integration with Other Languages

### Python Integration (via aplpy)

```python
import aplpy
apl = aplpy.APL()
result = apl.eval('SimulatePrimalLogic 100 (50⍴0.1) 1.0 0.16905 0.01 50')
```

### D Language Integration

Call APL via system command or shared library FFI.

### Prolog Integration

Use Prolog's shell interface to invoke APL.

## Performance

APL excels at:
- Matrix operations: O(n) speedup vs. loops
- Array transformations: Native optimization
- Parallel operations: Implicit vectorization

**Benchmarks:**
```apl
BenchmarkPrimalLogic 1000  ⍝ Run 1000 iterations
BenchmarkNASA 1000
BenchmarkPipeline 1000
```

## Best Practices

1. **Use array operations**: Avoid explicit loops
2. **Leverage reduction operators**: `+/`, `×/`, etc.
3. **Think in arrays**: Design for array-at-a-time processing
4. **Document with comments**: Use `⍝` for clarity
5. **Test incrementally**: Build complex expressions step-by-step

## Examples

### Matrix Operations
```apl
⍝ Covariance matrix
CovarianceMatrix X

⍝ Correlation matrix
CorrelationMatrix X
```

### Signal Processing
```apl
⍝ Low-pass filter
filtered ← LowPassFilter data 10 0.01

⍝ Peak detection
peaks ← PeakDetection data 0.5
```

### Primal Logic
```apl
⍝ Control law
dpsi ← PrimalControl 100 0.1 1.0 0.16905

⍝ Convergence analysis
result ← ConvergenceAnalysis trajectory 149.9992314000 0.01
```

## Resources

- [GNU APL Documentation](https://www.gnu.org/software/apl/apl.html)
- [Dyalog APL Documentation](https://docs.dyalog.com/)
- [APL Wiki](https://aplwiki.com/)
- [Learn APL](https://xpqz.github.io/learnapl/)
- [APL Cart](https://aplcart.info/) - Code snippets

## Directory Contents

```
apl/
├── primal_logic/
│   ├── core.apl          # Primal Logic implementation
│   └── README.md
├── nasa_simulation/
│   ├── mars_mission.apl  # Mars mission simulation
│   └── README.md
├── data_analysis/
│   └── pipeline.apl      # Data processing pipeline
└── README.md (this file)
```

## License

Proprietary - Patent Pending
U.S. Provisional Patent Application No. 63/842,846
