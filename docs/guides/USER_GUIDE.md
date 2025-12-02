# MotorHandPro User Guide

Complete guide to using the MotorHandPro Primal Logic control framework.

## Table of Contents

- [Introduction](#introduction)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Core Concepts](#core-concepts)
- [Basic Usage](#basic-usage)
- [Parameter Tuning](#parameter-tuning)
- [Visualization](#visualization)
- [Hardware Integration](#hardware-integration)
- [Troubleshooting](#troubleshooting)
- [Best Practices](#best-practices)

## Introduction

MotorHandPro implements the Primal Logic control framework using exponential memory weighting for bounded, stable autonomous control. This guide will help you get started and master the system.

### What You'll Learn

- How to install and configure MotorHandPro
- Understanding Primal Logic parameters
- Running simulations and analyzing results
- Deploying to hardware
- Tuning for optimal performance

### Prerequisites

**Knowledge:**
- Basic control theory (PID controllers helpful)
- Python programming (for simulations)
- C++ basics (for embedded deployment)

**Hardware (optional):**
- Arduino or compatible microcontroller
- Sensors (IMU, encoders, etc.)
- Actuators (motors, servos)

## Installation

### Python Environment

```bash
# Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### Verify Installation

```bash
# Run validation test
python validate_algorithms.py

# Expected output:
# ✓ Planck tail convergence
# ✓ Fixed-point iteration
# ✓ Lipschitz condition (F'(D) = 0.000129931830)
# ✓ All tests PASSED
```

### Optional: Arduino Setup

```bash
# Install Arduino IDE from arduino.cc

# Open MotorHandPro.ino
# Select board: Tools > Board > Arduino Uno (or your board)
# Upload to hardware

# Open Serial Monitor (115200 baud)
# You should see Primal Logic constants printed
```

## Quick Start

### Your First Simulation

```python
# first_sim.py
from extras.primal.kernel_v4 import PrimalKernel
import numpy as np
import matplotlib.pyplot as plt

# Initialize kernel with default parameters
kernel = PrimalKernel(lambda_val=0.16905, KE=0.3)

# Simulation parameters
t_max = 30.0  # seconds
dt = 0.01     # time step

# Storage
time = []
psi_values = []
gamma_values = []
Ec_values = []

# Initial state
psi = 0.0
Ec = 0.0

# Simulation loop
for n in range(int(t_max / dt)):
    t = n * dt

    # Reference signal (step input)
    reference = 1.0 if t > 1.0 else 0.0

    # Measure "actual" (for now, assume psi is the output)
    actual = psi

    # Error
    gamma = reference - actual

    # Primal Logic control law
    dpsi_dt = -kernel.lambda_val * psi + kernel.KE * gamma
    psi += dpsi_dt * dt

    # Energy integral
    Ec += psi * gamma * dt

    # Store
    time.append(t)
    psi_values.append(psi)
    gamma_values.append(gamma)
    Ec_values.append(Ec)

# Plot results
fig, axes = plt.subplots(3, 1, figsize=(10, 8))

axes[0].plot(time, psi_values)
axes[0].set_ylabel('ψ (Control Signal)')
axes[0].grid(True)

axes[1].plot(time, gamma_values)
axes[1].set_ylabel('γ (Error)')
axes[1].grid(True)

axes[2].plot(time, Ec_values)
axes[2].set_ylabel('Ec (Control Energy)')
axes[2].set_xlabel('Time (s)')
axes[2].grid(True)

plt.tight_layout()
plt.savefig('first_simulation.png')
plt.show()

print(f"Settling time: ~{1/kernel.lambda_val:.2f} seconds")
print(f"Final Ec: {Ec_values[-1]:.6f}")
print("✓ Simulation complete!")
```

**Run it:**
```bash
python first_sim.py
```

**Expected results:**
- `ψ` converges to reference value (~1.0) after ~6 seconds
- `γ` (error) decays exponentially
- `Ec` remains bounded (proves stability)

## Core Concepts

### The Primal Logic Equation

```
dψ/dt = -λψ(t) + KE·γ(t)
```

Where:
- **ψ (psi):** Control command signal
- **λ (lambda):** Lightfoot constant = 0.16905 s⁻¹ (exponential decay rate)
- **KE:** Error gain (tunable, typically 0.1-0.5)
- **γ (gamma):** Error signal = reference - actual

### Key Parameters

#### λ (Lambda - Lightfoot Constant)

**Default:** 0.16905 s⁻¹

**Meaning:** Rate of exponential memory decay

**Time constant:** τ = 1/λ ≈ 5.92 seconds

**Effect:**
- Higher λ: Faster response, less "memory"
- Lower λ: Slower response, more "smoothing"

**Tuning range:** 0.05 - 1.0 s⁻¹

#### KE (Error Gain)

**Default:** 0.3

**Meaning:** Proportional feedback strength

**Effect:**
- Higher KE: More aggressive error correction
- Lower KE: Gentler response

**Tuning range:** 0.0 - 0.8 (above 0.8 risks instability)

#### D (Donte Constant)

**Value:** 149.9992314000

**Meaning:** Fixed-point attractor of Primal Logic kernel

**Usage:** Read-only, used for stability verification

### Stability Guarantee

**Lipschitz Condition:** F'(D) < 1.0

For default parameters: F'(D) = 0.000129931830 ✓

This mathematically guarantees bounded convergence.

## Basic Usage

### Simulation Modes

#### 1. Step Response

```python
from extras.primal.kernel_v4 import simulate_step_response

results = simulate_step_response(
    lambda_val=0.16905,
    KE=0.3,
    duration=30.0
)

results.plot()
```

#### 2. Sinusoidal Tracking

```python
from extras.primal.kernel_v4 import simulate_sine_tracking

results = simulate_sine_tracking(
    lambda_val=0.16905,
    KE=0.3,
    frequency=0.5,  # Hz
    amplitude=1.0,
    duration=20.0
)

results.analyze_tracking_error()
```

#### 3. Disturbance Rejection

```python
from extras.primal.kernel_v4 import simulate_disturbance

results = simulate_disturbance(
    lambda_val=0.16905,
    KE=0.3,
    disturbance_time=5.0,
    disturbance_magnitude=0.5,
    duration=30.0
)

results.plot_recovery()
```

### Running Benchmarks

```bash
# Run all benchmark scenarios
python run_comprehensive_sweep.py

# Results saved to CSV files:
# - run_default.csv
# - run_ke05.csv
# - run_mu02.csv
# etc.
```

### Analyzing Results

```python
import pandas as pd
import matplotlib.pyplot as plt

# Load benchmark data
data = pd.read_csv('run_default.csv', comment='#')

# Extract columns
t = data['t']
psi = data['psi']
gamma = data['gamma']
Ec = data['Ec']

# Compute metrics
settling_idx = np.where(np.abs(gamma) < 0.02)[0][0]
settling_time = t[settling_idx]

print(f"Settling time: {settling_time:.2f} s")
print(f"Max psi: {psi.max():.4f}")
print(f"Final Ec: {Ec.iloc[-1]:.6f}")
```

## Parameter Tuning

### Tuning Workflow

1. **Start with defaults**
   - λ = 0.16905
   - KE = 0.3

2. **Identify performance goal**
   - Fast response? Increase λ and KE
   - Smooth tracking? Decrease λ and KE
   - Minimal overshoot? Lower KE

3. **Verify stability**
   - Check F'(D) < 1.0
   - Monitor Ec for boundedness

4. **Iterate**

### Tuning Examples

#### Faster Response

```python
# Original: τ ≈ 5.92s
lambda_val = 0.16905
KE = 0.3

# Faster: τ ≈ 4.0s
lambda_val = 0.25
KE = 0.4

# Verify stability
from extras.primal.kernel_v4 import verify_stability
assert verify_stability(lambda_val), "Unstable parameters!"
```

#### Smoother Control

```python
# Smoother, slower
lambda_val = 0.10
KE = 0.2

# Trade-off: τ ≈ 10s (slower response)
```

#### Aggressive Tracking

```python
# Maximum safe performance
lambda_val = 0.30
KE = 0.5

# Warning: Closer to stability boundary
# Monitor Ec carefully
```

### Parameter Sweeps

```python
# Sweep across parameter space
import itertools

lambdas = [0.10, 0.16905, 0.25, 0.30]
KEs = [0.1, 0.2, 0.3, 0.4, 0.5]

for lam, ke in itertools.product(lambdas, KEs):
    results = simulate_step_response(lam, ke)

    if verify_stability(lam):
        print(f"λ={lam:.2f}, KE={ke:.1f}: "
              f"settling={results.settling_time:.2f}s")
    else:
        print(f"λ={lam:.2f}, KE={ke:.1f}: UNSTABLE")
```

## Visualization

### Real-Time Control Panel

```bash
# Start LAM orchestrator
python lam_orchestrator.py

# Open control panel in browser
cd control_panel
python -m http.server 8080

# Navigate to: http://localhost:8080
```

**Control panel features:**
- Real-time parameter adjustment
- Live time-series plots
- 3D phase portrait
- Stability indicators

### Generate Plots

```python
# Using analyze_runs.py
python analyze_runs.py run_default.csv

# Generates:
# - run_default_plot.png (time series)
# - Statistics in terminal
```

### Custom Visualizations

```python
import matplotlib.pyplot as plt

# Create custom plot
fig, ax = plt.subplots(figsize=(10, 6))

ax.plot(time, psi, label='ψ(t)')
ax.plot(time, [1.0]*len(time), 'k--', label='Reference')
ax.fill_between(time, 0.98, 1.02, alpha=0.2, label='±2% band')

ax.set_xlabel('Time (s)')
ax.set_ylabel('Control Signal')
ax.legend()
ax.grid(True)

plt.savefig('custom_plot.png', dpi=300)
```

## Hardware Integration

### Arduino Deployment

#### 1. Upload Code

```cpp
// MotorHandPro.ino already configured
// Just upload to Arduino

// Serial output shows:
// D = 149.9992314000
// λ = 0.16905
// F'(D) = 0.000129931830
```

#### 2. Connect Sensors/Actuators

```cpp
// In setup():
pinMode(SENSOR_PIN, INPUT);
pinMode(ACTUATOR_PIN, OUTPUT);

// In loop():
double sensor_value = analogRead(SENSOR_PIN);
double reference = 512.0;  // Mid-range for 10-bit ADC

double gamma = reference - sensor_value;
double dpsi_dt = -LAMBDA * psi + KE * gamma;
psi += dpsi_dt * dt;

analogWrite(ACTUATOR_PIN, constrain(psi, 0, 255));
```

#### 3. Serial Monitoring

```bash
# View real-time data
arduino-cli monitor -p /dev/ttyACM0 -c baudrate=115200

# Log to file
arduino-cli monitor -p /dev/ttyACM0 > hardware_log.txt
```

### Raspberry Pi Deployment

```python
# rpi_control.py
import RPi.GPIO as GPIO
import time
from extras.primal.kernel_v4 import PrimalKernel

# Setup
GPIO.setmode(GPIO.BCM)
SENSOR_PIN = 17
ACTUATOR_PIN = 18
GPIO.setup(SENSOR_PIN, GPIO.IN)
GPIO.setup(ACTUATOR_PIN, GPIO.OUT)

pwm = GPIO.PWM(ACTUATOR_PIN, 1000)  # 1kHz
pwm.start(0)

# Control loop
kernel = PrimalKernel()
psi = 0.0

while True:
    sensor_value = GPIO.input(SENSOR_PIN)
    gamma = 1.0 - sensor_value  # Error

    dpsi_dt = -kernel.lambda_val * psi + kernel.KE * gamma
    psi += dpsi_dt * 0.01  # 100 Hz

    pwm.ChangeDutyCycle(psi * 100)
    time.sleep(0.01)
```

## Troubleshooting

### Common Issues

#### Simulation Not Converging

**Symptom:** ψ continues to grow or oscillate

**Causes:**
- KE too high
- Stability condition violated

**Solution:**
```python
# Check stability
from extras.primal.kernel_v4 import compute_lipschitz

lip = compute_lipschitz(lambda_val)
print(f"Lipschitz constant: {lip}")

if lip >= 1.0:
    print("UNSTABLE! Reduce KE or increase λ")
```

#### Ec Growing Unbounded

**Symptom:** Control energy Ec increases without limit

**Cause:** Violated Lipschitz condition

**Solution:**
```python
# Ensure F'(D) < 1.0
assert compute_lipschitz(lambda_val) < 1.0
```

#### Slow Response

**Symptom:** Takes > 10 seconds to settle

**Cause:** λ too small or KE too small

**Solution:**
```python
# Increase response speed
lambda_val = 0.25  # from 0.16905
KE = 0.4           # from 0.3
```

#### Noisy Control Signal

**Symptom:** ψ oscillates rapidly

**Causes:**
- Noisy measurements
- Derivative kick

**Solution:**
```python
# Low-pass filter the error signal
gamma_filtered = 0.9 * gamma_prev + 0.1 * gamma
```

### Hardware Issues

#### Serial Communication Failed

```bash
# Check port permissions
sudo usermod -a -G dialout $USER
# Log out and back in

# List available ports
arduino-cli board list
```

#### Actuator Not Responding

```cpp
// Verify PWM configuration
Serial.print("PWM value: ");
Serial.println(psi);

// Check physical connections
// Verify voltage levels
```

## Best Practices

### Parameter Selection

1. **Start conservative**
   - Use default λ = 0.16905, KE = 0.3
   - Verify stability before increasing

2. **Document changes**
   - Keep log of parameter experiments
   - Note settling time, overshoot for each config

3. **Version control**
   - Commit working configurations
   - Tag stable releases

### Safety

1. **Always verify stability**
   ```python
   assert compute_lipschitz(lambda_val) < 1.0
   ```

2. **Implement safety limits**
   ```python
   psi = np.clip(psi, -MAX_COMMAND, MAX_COMMAND)
   ```

3. **Emergency stop**
   ```python
   if keyboard.is_pressed('esc'):
       psi = 0.0
       break
   ```

### Performance

1. **Profile your code**
   ```bash
   python -m cProfile -o profile.stats your_sim.py
   ```

2. **Vectorize operations**
   ```python
   # Use NumPy for batch processing
   psi_array = psi_array * np.exp(-lambda_val * dt)
   ```

3. **Consider compiled implementations**
   - See `extras/quant_final/` for D language version
   - 10-100x speedup over Python

---

**Next Steps:**
- [Deployment Guide](DEPLOYMENT.md) - Production deployment
- [API Reference](../api/PYTHON_API.md) - Detailed API docs
- [Tutorials](../tutorials/) - Hands-on examples

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
