# Temporal Displacement Framework

Mathematical framework for time-weighted fields with causal stability guarantees in the LAM orchestration system.

**Author:** Donte Lightfoot
**Date:** September 20, 2025
**Patent Pending:** U.S. Provisional Application No. 63/842,846

## Table of Contents

- [Overview](#overview)
- [Mathematical Foundation](#mathematical-foundation)
- [Three Implementation Approaches](#three-implementation-approaches)
- [Python API](#python-api)
- [D Language API](#d-language-api)
- [LAM Integration](#lam-integration)
- [Applications](#applications)
- [Validation](#validation)
- [Examples](#examples)

## Overview

The Temporal Displacement Framework extends Primal Logic control with time-aware field equations. It treats equations as time-weighted fields where **temporal displacement** (Δ) defines which past (or future) slice of the world the field should read from.

### Key Concepts

- **Δ(t) > 0**: Retarded (reads from past) → causally safe for real-time systems
- **Δ(t) < 0**: Advanced (peeks into future) → offline inference or planning only
- **Δ(t) can encode**: Propagation delays, computation latencies, decision delays, trust metrics, load shedding

### Stability Guarantees

- **Causality**: K(s) = 0 for s < 0 (no influence from future)
- **Boundedness**: Field E(t) remains bounded for all time
- **Convergence**: System converges to equilibrium with proper parameter selection

## Mathematical Foundation

### Fundamental Concept

**Definition:** A temporal displacement field evaluates time-weighted equations where displacement Δ(t) shifts the sampling time while maintaining causality and stability.

### Stability Constraints

1. **Units**: Δ in seconds; kernels integrate to 1 for interpretable gains
2. **No Paradoxes**: Enforce Δ(t) ≥ 0 for online systems
3. **Stability**:
   - ODE form: Linearized pole at -β; require β > 0
   - Kernel form: K ≥ 0 and bounded ||Δ||∞
4. **Bounded Influence**: Cap ||α|| and ||κ||; apply saturation if E feeds back into Δ

## Three Implementation Approaches

### Method 1: Time-Warp (Retarded/Advanced Evaluation)

**Equation:**
```
E(t) = α·E₀(t - Δ(t)) - λ·D(t)
```

**Characteristics:**
- Simplest implementation
- Direct time-shifted evaluation
- Good for fixed or slowly-varying delays
- Computationally efficient

**When to use:**
- Known propagation delays (e.g., d/c for speed of light)
- Network latency compensation
- Simple sensor fusion with known delays

**Python:**
```python
from lam.temporal_displacement import TimeWarpField, TemporalDisplacementConfig

config = TemporalDisplacementConfig(alpha=1.0, kappa=0.1)
field = TimeWarpField(config, history_length=1000)

E = field.update(t=1.0, E0=1.5, Delta=0.1, D=0.0)
```

---

### Method 2: Memory Kernel (Primal Style)

**Equation:**
```
E(t) = ∫₀ᵗ K(t - τ - Δ(τ)) [α·E₀(τ) - κ·d(τ)] dτ
```

where K is the memory kernel (typically exponential for Primal Logic).

**Characteristics:**
- Unifies field strength, timing, and persistence
- State-dependent displacement Δ(τ)
- Natural low-pass filtering
- Primal Logic compatible (exponential kernel)

**When to use:**
- Complex temporal dependencies
- Adaptive displacement based on system state
- Trust-weighted signal fusion
- Systems requiring memory effects

**Python:**
```python
from lam.temporal_displacement import MemoryKernelField

field = MemoryKernelField(config, history_length=10000)

E = field.update(t=1.0, E0=1.5, Delta=0.1, d=0.0, dt=0.01)
```

**Exponential Kernel (default):**
```
K(s) = λ·exp(-λ·s)  for s ≥ 0
     = 0             for s < 0   (causality)
```

where λ = 0.16905 (Primal Logic Lightfoot constant)

---

### Method 3: Delay Differential Equation (DDE)

**Equation:**
```
dE/dt = -β·E(t) + α·E₀(t - Δ(t)) - κ·d(t)
```

**Characteristics:**
- Natural representation for transport phenomena
- Leaky integrator with delay
- Good for physical propagation modeling
- Compatible with standard DDE solvers

**When to use:**
- Physical transport delays (fluid flow, heat transfer)
- Communication latencies in distributed systems
- Systems with known relaxation dynamics (β)
- Real-time control with predictable delays

**Python:**
```python
from lam.temporal_displacement import DDEField

field = DDEField(config, history_length=1000)

E = field.update(t=1.0, E0=1.5, Delta=0.1, d=0.0, dt=0.01)
```

---

## Python API

### Unified Interface

```python
from lam.temporal_displacement import TemporalDisplacedField, TemporalDisplacementConfig

# Configuration
config = TemporalDisplacementConfig(
    alpha=1.0,         # Field strength
    beta=0.1,          # Decay rate (for DDE)
    kappa=0.1,         # Disturbance coupling
    lambda_val=0.16905 # Primal Logic constant
)

# Create field (choose method)
field = TemporalDisplacedField(method='timewarp', config=config)
# or method='kernel' or method='dde'

# Update
E = field.update(
    t=1.0,        # Current time
    E0=1.5,       # Driver field value
    Delta=0.1,    # Temporal displacement
    d=0.0,        # Disturbance
    dt=0.01       # Time step (for kernel/dde)
)

# Get current value
current_E = field.get_current_value()

# Reset
field.reset()
```

### Advanced Features

#### Trust-Gated Displacement

```python
from lam.temporal_displacement import TrustGatedDisplacement

trust_gate = TrustGatedDisplacement(
    Delta_0=0.0,      # Baseline displacement
    Delta_trust=1.0   # Trust sensitivity
)

# Compute displacement based on confidence
Delta = trust_gate.compute_displacement(confidence=0.8)
# Low confidence → higher displacement → down-weight signal
```

#### Load Shedding

```python
from lam.temporal_displacement import LoadSheddingDisplacement

load_shed = LoadSheddingDisplacement(
    Delta_base=0.0,     # Baseline (low load)
    Delta_max=2.0,      # Maximum (high load)
    load_threshold=0.8  # Start shedding at 80% load
)

# Compute displacement based on system load
Delta = load_shed.compute_displacement(system_load=0.9)
# High load → increase displacement → defer low-priority effects
```

### Validation Utilities

```python
from lam.temporal_displacement import verify_stability

# Verify stability
results = verify_stability(
    field=field,
    duration=100.0,
    dt=0.01
)

print(f"Bounded: {results['bounded']}")
print(f"Converges: {results['converges']}")
print(f"Final value: {results['final_value']}")
```

## D Language API

High-performance implementation for real-time embedded systems.

### Basic Usage

```d
import lam.temporal_displacement;

// Create field
auto field = TemporalDisplacedField(
    history: 1000,   // Buffer size
    alpha: 1.0,
    beta: 0.1,
    kappa: 0.1
);

// Update loop
foreach (t; 0.0 .. 10.0; 0.01) {
    double E0 = getSensorValue();
    double Delta = 0.1;  // 100ms delay
    double d = 0.0;

    double E = field.step(E0, Delta, d);
}
```

### Compilation

```bash
# Compile D implementation
cd lam
dmd -O -release temporal_displacement.d -of=temporal_displacement

# Run tests
./temporal_displacement
```

## LAM Integration

### AsyncLAM Controller

```python
from lam.lam_temporal_integration import AsyncLAMTemporal

# Create async controller
controller = AsyncLAMTemporal(update_rate=100.0)  # 100 Hz

# Start control loop
await controller.start()

# ... runs continuously ...

# Stop
await controller.stop()
```

### Integration with Existing LAM Orchestrator

```python
from lam.lam_temporal_integration import LAMTemporalController

# Create controller with trust gating and load shedding
controller = LAMTemporalController(
    method='dde',
    enable_trust_gating=True,
    enable_load_shedding=True
)

# In your LAM control loop:
state = controller.update(
    E0=reference_signal,
    disturbance=external_disturbance,
    confidence=signal_confidence,   # 0-1
    system_load=cpu_utilization,    # 0-1
    base_Delta=network_latency,     # seconds
    dt=0.01
)

# Use state.E in downstream control
print(f"Displaced field: {state.E}")
print(f"Adaptive displacement: {state.Delta}")
```

## Applications

### 1. Causality-Aware Sensing

Use Δ = d/c to enforce realistic signal propagation delays.

```python
from lam.lam_temporal_integration import CausalitySensingExample

sensor = CausalitySensingExample(
    propagation_distance=100.0,  # meters
    speed_of_light=3e8           # m/s
)

corrected_value = sensor.update(sensor_reading=1.5)
```

**Use cases:**
- Distributed sensor networks
- GPS timing corrections
- Astronomical observations
- Radar/lidar systems

### 2. Counterfactual Planning

Offline planning: "What input would we need earlier to achieve target now?"

```python
from lam.lam_temporal_integration import CounterfactualPlanningExample

planner = CounterfactualPlanningExample()

# Find required E0 values to produce target_E
Delta_range = np.linspace(0, 2.0, 100)
E0_required = planner.inverse_design(target_E=1.0, Delta_range=Delta_range)
```

**Use cases:**
- Trajectory optimization
- Predictive maintenance
- Resource planning
- What-if analysis

### 3. Trust and Aging Mechanisms

Make Δ(t) grow with staleness or low confidence.

```python
# Old or unverified signals pushed farther back in time
Delta = trust_gate.compute_displacement(confidence=0.3)
# Delta is larger → signal down-weighted by kernel
```

**Use cases:**
- Sensor fusion with varying reliability
- Multi-source data integration
- Anomaly detection
- Cybersecurity (trust decay)

### 4. Load Shedding

Increase Δ when system saturated → defer low-priority effects.

```python
# Under high load, increase displacement
Delta = load_shed.compute_displacement(system_load=0.95)
# Gracefully degrade by processing older data
```

**Use cases:**
- Real-time systems under overload
- Cloud resource management
- Network congestion control
- Embedded systems with limited compute

### 5. Multi-Agent Synchronization

Tie Δ to clock offsets/latencies across agents.

```python
from lam.lam_temporal_integration import MultiAgentSynchronizationExample

sync = MultiAgentSynchronizationExample(num_agents=5)

# Each agent sees global reference with their specific delay
E_values = sync.update_all(E0_global=1.0)
```

**Use cases:**
- Distributed control systems
- Swarm robotics
- Blockchain consensus
- Networked control

## Validation

### Test Suite

```bash
# Run comprehensive validation tests
cd lam
python test_temporal_displacement.py
```

**Tests included:**
1. **Causality Test**: Step response occurs exactly Δ steps after input
2. **Energy/Aging Test**: Field converges to expected steady-state
3. **Robustness Test**: Sweep Δ and add noise; check for stability
4. **Memory Kernel Test**: Verify kernel integrates to 1
5. **Trust Gating Test**: Verify adaptive displacement monotonicity
6. **Load Shedding Test**: Verify load-based displacement behavior
7. **Comparison Test**: All three methods on same scenario

### Expected Output

```
Test 1: Causality Test
E0 step at:      t = 1.00 s
E response at:   t = 1.50 s
Expected delay:  Δ = 0.50 s
Measured delay:  0.50 s
Error:           0.0000 s
Status:          ✓ PASS

Test 2: Energy Convergence Test
Time-Warp:
  Final E:      1.0000
  Expected E:   1.0000
  Error:        0.00%
  Status:       ✓ PASS

...

✓ ALL TESTS PASSED
```

### Visualization

Test suite generates comparison plot: `temporal_displacement_comparison.png`

Shows all three methods on:
- Step response with delay
- Disturbance rejection
- Noisy reference tracking

## Examples

### Example 1: Simple Time-Warp

```python
from lam.temporal_displacement import TimeWarpField, TemporalDisplacementConfig

config = TemporalDisplacementConfig(alpha=1.0)
field = TimeWarpField(config)

# Simulate with 100ms delay
dt = 0.01
for n in range(1000):
    t = n * dt
    E0 = 1.0 if t >= 1.0 else 0.0  # Step at t=1s
    Delta = 0.1  # 100ms

    E = field.update(t, E0, Delta, D=0.0)

    if n % 100 == 0:
        print(f"t={t:.2f}: E0={E0:.2f}, E={E:.4f}")
```

### Example 2: Adaptive Displacement

```python
from lam.lam_temporal_integration import LAMTemporalController

controller = LAMTemporalController(
    method='kernel',
    enable_trust_gating=True
)

# Simulate varying confidence
for n in range(1000):
    t = n * 0.01

    # Reference signal
    E0 = np.sin(2 * np.pi * 0.5 * t)

    # Confidence varies with signal quality
    confidence = 1.0 if abs(E0) > 0.5 else 0.5

    state = controller.update(E0, confidence=confidence, dt=0.01)

    print(f"t={t:.2f}: conf={confidence:.1f}, Δ={state.Delta:.3f}, E={state.E:.4f}")
```

### Example 3: Real-Time Embedded (D Language)

```d
// In your embedded control loop
auto field = TemporalDisplacedField(1000, 1.0, 0.1, 0.1);

while (true) {
    double E0 = readSensor();
    double Delta = getNetworkLatency();  // Dynamic
    double d = getDisturbance();

    double E = field.step(E0, Delta, d);

    setActuator(E);

    sleep(10.msecs);  // 100 Hz
}
```

## Performance

### Python Implementation

- **Time-Warp**: ~50 µs/update
- **Memory Kernel**: ~200 µs/update (depends on history length)
- **DDE**: ~60 µs/update

### D Language Implementation

- **Optimized**: ~2 µs/update
- **Suitable for**: > 10 kHz control loops
- **Speedup**: 25-100x vs Python

## Troubleshooting

### Issue: Field diverges or oscillates

**Cause:** β too small or Δ changing too rapidly

**Solution:**
```python
config = TemporalDisplacementConfig(
    beta=0.2,  # Increase decay
    max_delta_rate=0.1  # Limit Δ rate of change
)
```

### Issue: Delayed response doesn't match Δ

**Cause:** Insufficient history buffer or interpolation error

**Solution:**
```python
field = TimeWarpField(config, history_length=int(max_Delta / dt) * 2)
```

### Issue: Memory usage growing

**Cause:** History buffer unbounded

**Solution:** Set `history_length` appropriately based on max expected Δ

## References

- [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md)
- [LAM Implementation](LAM_IMPLEMENTATION.md)
- [LAM Orchestrator](../lam_orchestrator.py)

---

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
