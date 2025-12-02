# Temporal Displacement Quickstart Guide

Get started with Temporal Displacement in 5 minutes.

## What is Temporal Displacement?

Temporal displacement adds **time-aware field evaluation** to your control systems. Instead of using signals at the current time, you can:

- Read from the **past** (Δ > 0) - causally safe, real-time ready
- Weight signals by **age** and **confidence**
- Handle **propagation delays** naturally
- Implement **graceful degradation** under load

## Quick Install

```bash
cd /home/user/MotorHandPro/lam
# Dependencies already installed with main project
```

## Your First Temporal Field (30 seconds)

```python
from temporal_displacement import TemporalDisplacedField

# Create a time-warp field
field = TemporalDisplacedField(method='timewarp')

# Update with a 100ms delay
for i in range(100):
    t = i * 0.01  # Time in seconds
    E0 = 1.0      # Driver signal
    Delta = 0.1   # 100ms displacement

    E = field.update(t, E0, Delta, d=0.0, dt=0.01)

    print(f"t={t:.2f}s: E={E:.4f}")
```

**Output:** You'll see E lag behind E0 by exactly 100ms!

## Three Methods at a Glance

### Method 1: Time-Warp (Simplest)

**Best for:** Known delays, fastest performance

```python
from temporal_displacement import TimeWarpField, TemporalDisplacementConfig

config = TemporalDisplacementConfig(alpha=1.0, kappa=0.1)
field = TimeWarpField(config)

E = field.update(t=1.0, E0=1.5, Delta=0.1, D=0.0)
```

**Equation:** `E(t) = α·E₀(t-Δ) - κ·D(t)`

**Performance:** ~50,000 updates/sec

---

### Method 2: Memory Kernel (Primal Logic Compatible)

**Best for:** Complex temporal dependencies, trust-weighted fusion

```python
from temporal_displacement import MemoryKernelField

field = MemoryKernelField(config)

E = field.update(t=1.0, E0=1.5, Delta=0.1, d=0.0, dt=0.01)
```

**Equation:** `E(t) = ∫ K(t-τ-Δ(τ))[α·E₀(τ) - κ·d(τ)]dτ`

**Performance:** ~1,600 updates/sec

---

### Method 3: DDE (Physical Transport)

**Best for:** Physical propagation, relaxation dynamics

```python
from temporal_displacement import DDEField

field = DDEField(config)

E = field.update(t=1.0, E0=1.5, Delta=0.1, d=0.0, dt=0.01)
```

**Equation:** `dE/dt = -β·E + α·E₀(t-Δ) - κ·d`

**Performance:** ~42,000 updates/sec

---

## Real-World Example: Sensor Fusion with Delays

```python
from lam.lam_temporal_integration import CausalitySensingExample

# Sensor 100 meters away
sensor = CausalitySensingExample(
    propagation_distance=100.0,  # meters
    speed_of_light=3e8           # m/s (actually speed of EM waves)
)

# Sensor reading (happens in the past due to propagation)
sensor_value = 2.5

# Get corrected value accounting for ~333ns delay
corrected = sensor.update(sensor_value)

print(f"Sensor reading: {sensor_value}")
print(f"Corrected value: {corrected:.4f}")
```

## Advanced: Trust-Gated Displacement

Adapt displacement based on signal confidence:

```python
from temporal_displacement import TrustGatedDisplacement
from lam.lam_temporal_integration import LAMTemporalController

# Create controller with trust gating
controller = LAMTemporalController(
    method='kernel',
    enable_trust_gating=True
)

# Low confidence → higher displacement → down-weight signal
state = controller.update(
    E0=sensor_reading,
    confidence=0.3,  # Low confidence (30%)
    dt=0.01
)

print(f"Adaptive Δ: {state.Delta:.3f}s")
print(f"Weighted field: {state.E:.4f}")
```

**How it works:**
- High confidence (1.0): Normal displacement
- Medium confidence (0.5): Moderate displacement
- Low confidence (0.0): Maximum displacement (signal mostly ignored)

## Testing Your Installation

```bash
# Quick smoke test
python smoke_test.py

# Performance benchmark
python benchmark_temporal_displacement.py

# Full validation (takes ~1-2 minutes)
python test_temporal_displacement.py
```

## Common Use Cases

### 1. Distributed Sensor Network

```python
# Each sensor has different network latency
sensors = [
    {"id": 1, "latency": 0.05},  # 50ms
    {"id": 2, "latency": 0.12},  # 120ms
    {"id": 3, "latency": 0.03},  # 30ms
]

fields = [TemporalDisplacedField('timewarp') for _ in sensors]

# Update each with its specific delay
for sensor, field in zip(sensors, fields):
    E = field.update(
        t=current_time,
        E0=sensor_reading,
        Delta=sensor["latency"]
    )
```

### 2. Graceful Degradation Under Load

```python
from lam.lam_temporal_integration import LAMTemporalController

controller = LAMTemporalController(
    method='dde',
    enable_load_shedding=True
)

# System under heavy load
state = controller.update(
    E0=reference,
    system_load=0.95,  # 95% CPU
    dt=0.01
)

# Δ automatically increases → defers low-priority processing
print(f"Load shedding Δ: {state.Delta:.3f}s")
```

### 3. Multi-Agent Synchronization

```python
from lam.lam_temporal_integration import MultiAgentSynchronizationExample

# 5 agents with different clock offsets
sync = MultiAgentSynchronizationExample(num_agents=5)

# Broadcast global reference
E_values = sync.update_all(E0_global=1.0)

# Each agent compensates for its clock offset
for i, E in enumerate(E_values):
    print(f"Agent {i}: E={E:.4f}")
```

## Integration with LAM Orchestrator

### Async Control Loop

```python
from lam.lam_temporal_integration import AsyncLAMTemporal

# Create async controller
controller = AsyncLAMTemporal(update_rate=100.0)  # 100 Hz

# Start (runs indefinitely)
await controller.start()

# ... system running ...

# Stop gracefully
await controller.stop()
```

### Sync Control Loop

```python
from lam.lam_temporal_integration import LAMTemporalController

controller = LAMTemporalController(method='timewarp')

# In your main control loop:
while running:
    # Read sensors
    E0 = read_sensor()
    confidence = assess_confidence(E0)

    # Update with temporal displacement
    state = controller.update(
        E0=E0,
        confidence=confidence,
        dt=0.01
    )

    # Use displaced field
    apply_control(state.E)

    time.sleep(0.01)  # 100 Hz
```

## Performance Tips

### 1. Choose the Right Method

- **Time-Warp**: Fastest, use for simple delays
- **Memory Kernel**: Slower but handles complex dependencies
- **DDE**: Good balance, best for physical systems

### 2. Limit History Buffer

```python
# Don't need more than max_Delta / dt samples
max_Delta = 2.0  # seconds
dt = 0.01        # 100 Hz
history_needed = int(max_Delta / dt)  # 200 samples

field = TimeWarpField(config, history_length=history_needed)
```

### 3. Use D Language for High Performance

For control loops > 10kHz, use the D implementation:

```bash
cd lam
dmd -O -release temporal_displacement.d -of=temporal_fast
./temporal_fast
```

**Expected speedup:** 25-100x over Python

## Troubleshooting

### Issue: Field value diverges

**Solution:** Check stability parameters

```python
config = TemporalDisplacementConfig(
    alpha=1.0,
    beta=0.2,  # Increase decay (must be > 0)
    kappa=0.1
)
```

### Issue: Response delayed more than expected

**Solution:** Check history buffer size

```python
# Ensure history > Delta / dt
field = TimeWarpField(config, history_length=1000)
```

### Issue: Memory usage growing

**Solution:** Reduce history length or use Time-Warp instead of Memory Kernel

```python
# Memory Kernel keeps full history
# Use Time-Warp for lower memory footprint
field = TemporalDisplacedField(method='timewarp')
```

## Next Steps

📖 **Full Documentation:** [TEMPORAL_DISPLACEMENT.md](TEMPORAL_DISPLACEMENT.md)

🧪 **Run Tests:**
```bash
python smoke_test.py                        # Quick test
python benchmark_temporal_displacement.py   # Performance
python test_temporal_displacement.py        # Full validation
```

🔗 **Integration Examples:** [lam_temporal_integration.py](lam_temporal_integration.py)

📊 **See Performance:** Benchmarks show ~50k updates/sec (Python), 1M+ updates/sec (D)

## Summary

Temporal Displacement adds powerful time-aware capabilities to your control systems:

✅ **Causality-Safe**: Δ ≥ 0 enforced for online systems
✅ **Stable**: Mathematical guarantees of bounded convergence
✅ **Flexible**: Three methods for different use cases
✅ **Fast**: Python for prototyping, D for production
✅ **Adaptive**: Trust-gating and load shedding built-in

**Start simple with Time-Warp, graduate to Memory Kernel when needed!**

---

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
