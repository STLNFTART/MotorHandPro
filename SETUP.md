# MotorHandPro Setup Guide

This guide explains how to use the `setup.sh` script to configure your MotorHandPro environment.

## Overview

The `setup.sh` script automates the complete setup process for MotorHandPro, including:

- ✅ Dependency installation (Python, Node.js, system packages)
- ✅ Environment configuration
- ✅ Building native components (D, APL, Prolog)
- ✅ Directory structure creation
- ✅ Initial testing and validation
- ✅ Git hooks setup

## Quick Start

### Minimal Installation (Fastest)

For basic functionality with core Python dependencies only:

```bash
bash setup.sh --minimal
```

**Installs:**
- Core Python dependencies (numpy, scipy, matplotlib)
- LAM system dependencies
- Basic directory structure

**Time:** ~2-3 minutes

### Full Installation (Recommended)

For complete functionality including all optional components:

```bash
bash setup.sh --full
```

**Installs:**
- Everything from minimal installation
- Node.js dependencies for control panel
- D language components (if compiler available)
- APL, Prolog integrations
- Docker configuration
- ML dataset tools

**Time:** ~5-10 minutes

### Development Installation

For contributors and developers:

```bash
bash setup.sh --full --dev
```

**Additional installs:**
- pytest, pytest-cov for testing
- black, flake8, mypy for code quality
- ipython, jupyter for interactive development
- pre-commit hooks

## Usage Options

```bash
./setup.sh [OPTIONS]
```

### Available Options

| Option | Description |
|--------|-------------|
| `--minimal` | Install only core dependencies (no optional components) |
| `--full` | Install everything including optional components (default) |
| `--dev` | Install development tools and dependencies |
| `--skip-tests` | Skip running initial tests |
| `--help` | Show help message |

### Examples

```bash
# Full installation with development tools
./setup.sh --full --dev

# Minimal installation without running tests
./setup.sh --minimal --skip-tests

# Show help
./setup.sh --help
```

## Prerequisites

### Required

- **Python 3.8+** - Core runtime
- **pip3** - Python package manager
- **Git** - Version control

### Optional (for full installation)

- **Node.js 14+** - For control panel UI
- **npm 6+** - Node package manager
- **Docker** - For containerized deployment
- **Docker Compose** - For multi-container setup
- **D Compiler** (dmd/ldc2/gdc) - For D language components

## What Gets Installed

### Python Packages

**Core (`requirements.txt`):**
- numpy - Numerical computing
- scipy - Scientific computing
- matplotlib - Plotting and visualization
- plotly - Interactive 3D visualizations
- requests - HTTP library
- websockets - WebSocket support

**LAM System (`lam_requirements.txt`):**
- asyncio - Async I/O
- aiohttp - Async HTTP
- asyncpg - Async PostgreSQL
- paho-mqtt - MQTT client
- redis - Redis client
- docker - Docker API
- cryptography - Security

**Development (with `--dev`):**
- pytest - Testing framework
- black - Code formatter
- flake8 - Linter
- mypy - Type checker
- ipython - Interactive shell
- jupyter - Notebooks

### Node.js Packages

**Main (`package.json`):**
- @openzeppelin/contracts - Smart contracts
- ethers - Ethereum library
- dotenv - Environment variables
- hardhat - Ethereum development

**Control Panel:**
- Three.js - 3D visualization
- Chart.js - Data visualization
- WebSocket client

### Native Components

**D Language:**
- Drug safety modeling system
- Temporal displacement module
- High-performance kernel

**APL:**
- Array programming implementations

**Prolog:**
- Logic programming integrations

## Directory Structure Created

```
MotorHandPro/
├── logs/                      # Runtime logs
├── data/                      # Data files
├── output/                    # Generated output
├── results/                   # Analysis results
├── experiments/results/       # Experiment outputs
├── validation_results/        # Validation data
└── ~/.motorhand/             # User configuration
```

## Configuration Files

The setup script creates or uses these configuration files:

### `.env`
Environment variables for runtime configuration.

**Created from:** `.env.example` (if not exists)

**Contains:**
- Hedera network credentials
- API keys and secrets
- Service endpoints
- Token configuration

**⚠️ Important:** Edit this file with your actual credentials!

### `.npmrc`
npm configuration for package publishing.

**Created from:** `.npmrc.template`

### `.env.production`
Production environment configuration for Docker deployment.

## Post-Installation

### Verify Installation

The setup script automatically runs tests unless `--skip-tests` is specified. You can manually verify:

```bash
# Run LAM smoke tests
python3 lam/smoke_test.py

# Test Python imports
python3 -c "import numpy, scipy, matplotlib; print('✅ Core libraries OK')"

# Test async libraries
python3 -c "import asyncio, aiohttp; print('✅ Async libraries OK')"
```

### Configure Hedera Integration

1. Create a Hedera testnet account at https://portal.hedera.com
2. Edit `.env` file:
   ```bash
   HEDERA_OPERATOR_ID=0.0.YOUR_ACCOUNT_ID
   HEDERA_OPERATOR_KEY=0xYOUR_PRIVATE_KEY
   ```

3. Test integration:
   ```bash
   python3 lam/integrations/gotrax_hoverboard_integration.py
   ```

### Quick Start Commands

After setup completes, try these commands:

```bash
# Interactive LAM session
python3 -i lam_interactive_session.py

# Start LAM system
./start_lam_system.sh

# Run analysis
python3 analyze_runs.py

# Start control panel (if Node.js installed)
cd control_panel && npm start

# Docker deployment (if Docker installed)
docker-compose up -d
```

## Troubleshooting

### Python Import Errors

```bash
# Reinstall dependencies
pip3 install -r requirements.txt --force-reinstall

# Check Python version
python3 --version  # Should be >= 3.8
```

### Permission Denied

```bash
# Make script executable
chmod +x setup.sh

# Run with bash
bash setup.sh
```

### Pip Upgrade Fails

The script handles system-managed pip automatically. If issues persist:

```bash
# Use user install
python3 -m pip install --user -r requirements.txt
```

### Docker Not Found

Install Docker:
```bash
# Ubuntu/Debian
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# macOS
brew install docker
```

### Node.js Not Found

Install Node.js:
```bash
# Ubuntu/Debian
curl -fsSL https://deb.nodesource.com/setup_lts.x | sudo -E bash -
sudo apt-get install -y nodejs

# macOS
brew install node
```

### D Compiler Not Found

The setup script skips D components if no compiler is found. To install:

```bash
# Install DMD (fastest compiler)
curl -fsS https://dlang.org/install.sh | bash -s dmd

# Or install LDC2 (best performance)
curl -fsS https://dlang.org/install.sh | bash -s ldc
```

## Advanced Usage

### Custom Python Environment

If using virtual environment:

```bash
# Create venv
python3 -m venv venv
source venv/bin/activate

# Run setup
bash setup.sh --full
```

### Offline Installation

For air-gapped systems:

1. Download dependencies on connected machine:
   ```bash
   pip3 download -r requirements.txt -d packages/
   ```

2. Transfer `packages/` directory to offline machine

3. Install offline:
   ```bash
   pip3 install --no-index --find-links=packages/ -r requirements.txt
   ```

### Build Individual Components

```bash
# D language drug safety module
cd drug_safety && ./build.sh

# APL components
cd apl && ./build.sh

# Prolog components
cd prolog && ./build.sh

# All integrations
cd integration && ./build_all.sh
```

## Environment Modes

### Minimal Mode
- **Purpose:** Quick testing, CI/CD, embedded systems
- **Time:** ~2-3 minutes
- **Disk:** ~500 MB
- **Components:** Python core only

### Full Mode
- **Purpose:** Production deployment, complete functionality
- **Time:** ~5-10 minutes
- **Disk:** ~2-3 GB
- **Components:** Everything except dev tools

### Development Mode
- **Purpose:** Contributing, debugging, testing
- **Time:** ~8-12 minutes
- **Disk:** ~3-4 GB
- **Components:** Everything including dev tools

## Continuous Integration

For CI/CD pipelines:

```yaml
# .github/workflows/setup.yml
- name: Setup MotorHandPro
  run: |
    bash setup.sh --minimal --skip-tests
    python3 lam/smoke_test.py
```

## Uninstallation

To remove installed components:

```bash
# Remove Python packages
pip3 uninstall -y -r requirements.txt
pip3 uninstall -y -r lam_requirements.txt

# Remove Node modules
rm -rf node_modules control_panel/node_modules

# Remove built components
rm -rf drug_safety/bin dlang/bin apl/bin prolog/bin

# Remove runtime directories (⚠️ will delete logs and data)
rm -rf logs/ data/ output/ results/
```

## Support

- **Documentation:** See `docs/` directory
- **Issues:** https://github.com/STLNFTART/MotorHandPro/issues
- **Contact:** Donte Lightfoot (STLNFTART)

## License

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

See `LICENSE` file for details.

---

**Last Updated:** December 2025
