# Quick Start Guide

Get MotorHandPro running in 5 minutes!

## ⚡ 5-Minute Quick Start

### Step 1: Clone & Install (2 minutes)

```bash
# Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install numpy matplotlib
```

### Step 2: Run Smoke Test (1 minute)

```bash
cd lam
python smoke_test.py
```

**Expected Output:**
```
✓ Temporal displacement imports working
✓ Core LAM modules loaded
✓ All systems operational
```

### Step 3: Try Interactive Demo (2 minutes)

```bash
# Launch interactive LAM session
python interactive_lam_session.py
```

This will demonstrate:
- Temporal displacement calculations
- Control field evolution
- Real-time parameter updates

## 🎯 What Just Happened?

You just ran:
1. **Smoke Test** - Verified all core modules are working
2. **Interactive Demo** - Experienced LAM temporal displacement in action

## 🚀 Next Steps

### Option A: Hardware Control

If you have Arduino hardware:

```bash
# 1. Open Arduino IDE
# 2. Load MotorHandPro.ino
# 3. Upload to board
# 4. Open Serial Monitor (115200 baud)
```

See [Hardware Setup](Hardware-Setup) for details.

### Option B: Web Interface

Launch the web control panel:

```bash
cd control_panel
# Open index.html in your browser
```

### Option C: Analyze Data

Run benchmark analysis:

```bash
python analyze_runs.py
```

View results in `validation_results/` directory.

## 📊 Understanding Your First Run

### Primal Logic Constants

The smoke test computed these key constants:

- **D** (Donte constant): 149.9992314000
  - Fixed-point attractor value

- **λ** (Lambda/Lightfoot): 0.16905 s⁻¹
  - Exponential decay rate

- **I3**: 6.4939394023
  - Normalization constant

### Control Equation

The simplified control law:

```
dψ/dt = -λ·ψ(t) + KE·e(t)

where:
  ψ(t) = control command
  e(t) = tracking error
  λ = 0.16905 (exponential decay)
  KE = error gain
```

## 🧪 Try These Examples

### Example 1: Temporal Displacement

```bash
cd lam
python test_temporal_displacement.py
```

Tests three displacement methods:
- Direct displacement
- Buffered displacement
- Interpolated displacement

### Example 2: Distributed Control

```bash
cd lam
python example_distributed_control.py
```

Demonstrates multi-agent coordination.

### Example 3: Benchmark Performance

```bash
cd lam
python benchmark_temporal_displacement.py
```

Compare Python vs D language performance.

## 📁 Essential Files

| File | Purpose |
|------|---------|
| `MotorHandPro.ino` | Arduino control sketch |
| `lam/smoke_test.py` | Quick validation |
| `lam/temporal_displacement.py` | Core LAM logic |
| `analyze_runs.py` | Data analysis |

## 🔧 Configuration

### Basic Configuration

Edit `lam/config.py`:

```python
# Control parameters
LAMBDA = 0.16905  # Decay rate
KE = 0.5          # Error gain
D = 149.9992      # Fixed point

# Communication
WEBSOCKET_PORT = 8765
MQTT_BROKER = "localhost"
```

### Hardware Configuration

Edit `MotorHandPro.ino`:

```cpp
#define SERVO_PIN 9
#define BAUD_RATE 115200
```

## 🎓 Learn More

### Core Concepts

1. **[Primal Logic Framework](Primal-Logic-Framework)** - Mathematical theory
2. **[Temporal Displacement](Temporal-Displacement)** - Time-aware control
3. **[LAM System](LAM-System-Guide)** - Orchestration layer

### Tutorials

1. **[User Guide](User-Guide)** - Complete walkthrough
2. **[Hardware Setup](Hardware-Setup)** - Physical deployment
3. **[API Reference](API-Reference)** - Programming interface

## 🐛 Quick Troubleshooting

### "Module not found" Error

```bash
# Ensure virtual environment is activated
source venv/bin/activate

# Install missing packages
pip install -r requirements.txt
```

### Serial Port Issues

```bash
# Linux: Grant permissions
sudo usermod -a -G dialout $USER
# Log out and back in

# Check available ports
ls /dev/tty*
```

### Import Path Errors

```bash
# Run from project root
cd /path/to/MotorHandPro

# Set PYTHONPATH if needed
export PYTHONPATH="${PYTHONPATH}:$(pwd)"
```

## ✅ Verification Checklist

- [ ] Repository cloned
- [ ] Virtual environment created
- [ ] Dependencies installed
- [ ] Smoke test passed
- [ ] Interactive demo ran successfully
- [ ] Understood basic concepts

## 🎯 Your Next Mission

Choose your path:

1. **Hardware Hacker**: Go to [Hardware Setup](Hardware-Setup)
2. **Software Developer**: Check [API Reference](API-Reference)
3. **Data Scientist**: Explore [Analysis Tools](Analysis-Tools)
4. **System Operator**: Review [Deployment Guide](Deployment-Guide)

---

**Congratulations!** You've completed the quick start. Ready for more? See [Getting Started](Getting-Started) for the full guide.
