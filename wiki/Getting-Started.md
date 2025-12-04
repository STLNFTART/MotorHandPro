# Getting Started with MotorHandPro

This guide will help you get MotorHandPro up and running on your system.

## 📋 Prerequisites

### Hardware Requirements

- **For Embedded Systems:**
  - Arduino board (Uno, Mega, or compatible)
  - Servo motors or robotic hand hardware
  - USB cable for programming

- **For Software Development:**
  - Linux, macOS, or Windows with WSL2
  - 4GB+ RAM
  - Python 3.8 or higher

### Software Requirements

- **Python**: 3.8+
- **Git**: For version control
- **Optional**: Docker, Node.js, D compiler (DMD/LDC)

## 🚀 Installation

### Step 1: Clone the Repository

```bash
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro
```

### Step 2: Python Environment Setup

```bash
# Create virtual environment
python3 -m venv venv

# Activate virtual environment
# On Linux/macOS:
source venv/bin/activate
# On Windows:
# venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### Step 3: Verify Installation

```bash
# Run smoke test
cd lam
python smoke_test.py
```

You should see output confirming all core components are working.

## 🎮 Quick Start Paths

Choose your path based on your goals:

### Path 1: Hardware Control (Arduino)

If you want to control physical robotic hardware:

1. **Upload Arduino Sketch**
   ```bash
   # Open MotorHandPro.ino in Arduino IDE
   # Select your board and port
   # Upload the sketch
   ```

2. **Verify Constants**
   - Open Serial Monitor (115200 baud)
   - Verify Primal Logic constants are computed
   - Check D, I3, S values match expected ranges

3. **Test Control**
   - Connect servo motors
   - Run example control scripts
   - Monitor serial output

See [Hardware Setup](Hardware-Setup) for detailed instructions.

### Path 2: LAM System Development

If you want to work with the Large Action Model orchestration:

1. **Setup LAM Environment**
   ```bash
   cd lam
   pip install -r requirements.txt
   ```

2. **Run LAM Smoke Test**
   ```bash
   python smoke_test.py
   ```

3. **Try Temporal Displacement**
   ```bash
   # Interactive demo
   python interactive_lam_session.py
   ```

See [LAM System Guide](LAM-System-Guide) for advanced features.

### Path 3: Analysis & Visualization

If you want to analyze benchmark data or visualize results:

1. **Run Analysis Script**
   ```bash
   python analyze_runs.py
   ```

2. **Generate Visualizations**
   ```bash
   cd comprehensive_visualizations
   python generate_plots.py
   ```

3. **View Results**
   - Check `validation_results/` directory
   - Review generated plots

### Path 4: Web Interface

If you want to use the web-based control panel:

1. **Start Control Panel**
   ```bash
   cd control_panel
   # Open index.html in browser
   ```

2. **Configure Connection**
   - Set WebSocket endpoint
   - Connect to LAM backend

3. **Monitor Real-time**
   - View 3D visualization
   - Monitor control parameters

## 🧪 Running Tests

### Unit Tests

```bash
# Python tests
pytest

# LAM tests
cd lam
python -m pytest test_*.py
```

### Benchmark Tests

```bash
# Run benchmarks
python benchmark_temporal_displacement.py

# View results
cat benchmark_results.txt
```

### Hardware Validation

```bash
# Run hardware validation suite
cd validation_results
python run_validation.py
```

## 📚 Next Steps

### Learn the Theory

1. Read [Primal Logic Framework](Primal-Logic-Framework)
2. Understand [Control Theory](Control-Theory)
3. Explore [Temporal Displacement](Temporal-Displacement)

### Start Developing

1. Review [API Reference](API-Reference)
2. Check [Development Setup](Development-Setup)
3. Read [Contributing](Contributing)

### Deploy to Production

1. Follow [Deployment Guide](Deployment-Guide)
2. Set up [Docker Setup](Docker-Setup)
3. Configure [Kubernetes](Kubernetes-Deployment)

## 🔍 Understanding the Data Flow

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

## 📊 Key Directories

| Directory | Purpose |
|-----------|---------|
| `/` | Core Primal Logic (C++, Arduino) |
| `/lam/` | LAM orchestration layer |
| `/control_panel/` | Web UI |
| `/docs/` | Documentation |
| `/analysis/` | Data analysis tools |
| `/validation_results/` | Test results |

## 🐛 Troubleshooting

### Common Issues

**Import Errors**
```bash
# Make sure you're in the right directory
cd /path/to/MotorHandPro
source venv/bin/activate
pip install -r requirements.txt
```

**Serial Connection Issues**
```bash
# Check available ports
ls /dev/tty*

# Grant permissions (Linux)
sudo usermod -a -G dialout $USER
```

**Missing Dependencies**
```bash
# Install system dependencies (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install python3-dev build-essential

# For D language support
sudo apt-get install dmd-compiler
```

See [Troubleshooting](Troubleshooting) for more solutions.

## 💡 Tips for Success

1. **Start Simple**: Begin with smoke tests before complex deployments
2. **Read the Docs**: Check `/docs/guides/USER_GUIDE.md` for detailed info
3. **Use Examples**: Explore example scripts in `/lam/` directory
4. **Monitor Logs**: Enable debug logging for troubleshooting
5. **Join Community**: Check GitHub discussions for support

## 📖 Additional Resources

- [User Guide](User-Guide) - Complete user documentation
- [Architecture](Architecture) - System design
- [FAQ](FAQ) - Frequently asked questions
- [Glossary](Glossary) - Technical terms

---

**Next**: Check out the [Quick Start Guide](Quick-Start-Guide) for a 5-minute tutorial!
