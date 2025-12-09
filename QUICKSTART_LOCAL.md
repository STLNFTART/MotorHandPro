# 🚀 MotorHandPro - Local Machine Setup Guide

Complete guide to clone, install, and launch MotorHandPro on your local machine with Hedera smart contract integration for actuator token burns.

---

## 📋 Prerequisites

### Required:
- **Git** - Version control
- **Python 3.11+** - Core runtime
- **pip** - Python package manager
- **Node.js 18+** - For control panel and APIs
- **npm** - Node package manager

### Optional (for full stack):
- **Docker & Docker Compose** - For production services
- **D Compiler (dmd/ldc2)** - For high-performance modules
- **Hedera Account** - For blockchain token burns

---

## 🔧 Step 1: Clone the Repository

```bash
# Clone from GitHub
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Verify you're on the main branch (or your desired branch)
git branch
```

---

## 📦 Step 2: Install Dependencies

### Python Dependencies

```bash
# Install core Python packages
pip install numpy matplotlib scipy

# Install async support
pip install asyncio aiohttp

# Install environment variable support
pip install python-dotenv

# Install development tools (optional)
pip install pytest pytest-asyncio
```

### Node.js Dependencies (for Control Panel)

```bash
# Install Node.js dependencies
cd control_panel
npm install
cd ..

# Install Node-RED integration (optional)
cd node-red
npm install
cd ..
```

### D Language (Optional - High Performance)

```bash
# Ubuntu/Debian
sudo apt-get install dmd dub

# macOS
brew install dmd dub

# Test D compiler
dmd --version
```

---

## ⚙️ Step 3: Configure Hedera Smart Contract (Token Burn Integration)

### What This Does:
**Every time an actuator is used, tokens are automatically burned on the Hedera blockchain:**
- **1 Token = 1 Second** of smooth robotic actuation
- Motors won't run without sufficient tokens
- Provides economic incentive model for robot usage

### Setup Instructions:

1. **Copy the environment template:**
```bash
cp .env.example .env
```

2. **Get Hedera Testnet Credentials:**
   - Go to https://portal.hedera.com
   - Create a testnet account (free)
   - Copy your Account ID and Private Key

3. **Edit `.env` file with your credentials:**
```bash
# Open in your favorite editor
nano .env   # or vim .env, or code .env

# Fill in these values:
HEDERA_NETWORK=testnet
HEDERA_OPERATOR_ID=0.0.YOUR_ACCOUNT_ID
HEDERA_OPERATOR_KEY=0xYOUR_PRIVATE_KEY_HERE
HEDERA_EVM_ADDRESS=0xYOUR_EVM_ADDRESS
HEDERA_CONTRACT_ID=0xYOUR_CONTRACT_ADDRESS

TOKEN_RATE=1.0
MIN_TOKENS_REQUIRED=0.1
```

4. **Verify configuration:**
```bash
python -c "from dotenv import load_dotenv; import os; load_dotenv(); print('Hedera Network:', os.getenv('HEDERA_NETWORK'))"
```

---

## 🎮 Step 4: Launch the LAM System

### Option A: Interactive LAM Session (Recommended for Learning)

```bash
# Start interactive Python session with all demos loaded
python -i lam_interactive_session.py

# Once loaded, run demonstrations:
>>> demo1()  # Distributed sensor fusion
>>> demo2()  # Temporal displacement comparison
>>> demo3()  # Trust-gated adaptive control
>>> demo4()  # Load shedding
>>> help()   # See all available commands
```

### Option B: Run LAM Orchestrator (Full System)

```bash
# Start the full LAM orchestrator
python lam_orchestrator.py

# Follow the interactive menu:
# 1. Configure credentials
# 2. Map services
# 3. Check system health
# 4. Deploy services
```

### Option C: Test Hoverboard Integration (Actuator + Token Burn)

```bash
# Run the GoTrax hoverboard test
# This demonstrates the actuator token burn mechanism
python lam/integrations/gotrax_hoverboard_integration.py
```

**Expected Output:**
```
======================================================================
GOTRAX EDGE HOVERBOARD INTEGRATION TEST
1 Token = 1 Second of Perfectly Smooth Robotic Actuation
======================================================================

Connecting to Hedera testnet...
  Operator ID: 0.0.YOUR_ACCOUNT_ID
  Contract: 0xYOUR_CONTRACT_ADDRESS

1. Depositing tokens...
✓ Deposited 10.0 tokens

3. Executing forward movement (2 seconds)...
✓ Burned 2.0 tokens
✓ Actuation complete
  Smoothness score: 0.98
  Remaining balance: 8.0 tokens
```

---

## 🌐 Step 5: Launch Control Panel (Web UI)

```bash
# Terminal 1: Start the control panel server
cd control_panel
npm start

# Open browser to:
http://localhost:3000
```

**Features:**
- Real-time parameter control (λ, KE, D)
- Live telemetry visualization
- Motor status monitoring
- Token balance display

---

## 🐳 Step 6: Start Docker Services (Optional - Full Stack)

If you have Docker installed:

```bash
# Option 1: Basic services
docker-compose up -d

# Option 2: Full production stack
docker-compose -f docker-compose.production.yml up -d

# Check service status
docker-compose ps

# View logs
docker-compose logs -f
```

**Services Started:**
- **TimescaleDB** (Port 5432) - Time-series database
- **MQTT Broker** (Port 1883, 9001) - Real-time messaging
- **Redis** (Port 6379) - Caching
- **Grafana** (Port 3001) - Dashboards
- **FastAPI** (Port 8000) - Core API
- **Node.js Gateway** (Port 3000) - Integration layer

---

## 🧪 Step 7: Run Tests

```bash
# Quick smoke test (6 tests, <1 second)
python lam/smoke_test.py

# Temporal displacement validation
python lam/test_temporal_displacement.py

# Performance benchmarks
python lam/benchmark_temporal_displacement.py

# Distributed control demo
python lam/example_distributed_control.py
```

---

## 🔌 How Actuator Token Burns Work

### Architecture:

```
┌─────────────────┐
│  LAM Controller │
│  (Python/D)     │
└────────┬────────┘
         │
         ▼
┌─────────────────────────────────────┐
│  GoTraxHoverboardController         │
│  - Receives actuation request       │
│  - Checks token balance             │
│  - Calculates required tokens       │
│    (duration × token_rate)          │
└────────┬────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│  HederaSmartContractInterface       │
│  - Connects to Hedera testnet       │
│  - Executes burn transaction        │
│  - Returns transaction hash         │
└────────┬────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│  Hedera Blockchain                  │
│  - Token permanently burned         │
│  - Transaction recorded on-chain    │
│  - Immutable audit trail            │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│  Physical Actuators (Motors)        │
│  - Execute smooth trajectory        │
│  - Primal Logic control             │
│  - Exponential memory weighting     │
└─────────────────────────────────────┘
```

### Code Flow:

1. **Request Actuation** (lam_temporal_integration.py:330):
```python
request = ActuationRequest(
    mode=ActuationMode.FORWARD,
    duration_seconds=2.0,  # 2 seconds
    power_level=0.6,
    tokens_allocated=2.0   # 2 tokens required
)
```

2. **Check Balance** (gotrax_hoverboard_integration.py:336):
```python
tokens_required = duration_seconds * token_rate  # 2.0 * 1.0 = 2.0
if tokens_required > self.token_balance:
    return ActuationResult(success=False, error="Insufficient tokens")
```

3. **Burn Tokens** (gotrax_hoverboard_integration.py:202):
```python
def _burn_tokens(self, amount: float) -> bool:
    self.token_balance -= amount
    self.tokens_burned_total += amount

    # Call Hedera smart contract
    if self.token_config.burn_callback:
        self.token_config.burn_callback(amount)  # Blockchain transaction
```

4. **Execute Motor Control** (gotrax_hoverboard_integration.py:369):
```python
for point in trajectory:
    # Apply Primal Logic smooth control
    duty_cycle = power_level * scale * stability_factor

    # Update motors
    left_motor["duty_cycle"] = duty_cycle
    right_motor["duty_cycle"] = duty_cycle
```

5. **Record Results**:
```python
result = ActuationResult(
    success=True,
    tokens_burned=2.0,
    actuation_duration_actual=2.0,
    smoothness_score=0.98,  # Primal Logic smoothness
    transaction_hash="0x..."  # Hedera blockchain tx
)
```

### Key Features:

✅ **Economic Model**: 1 token = 1 second of actuation
✅ **Pre-flight Check**: Won't run without sufficient balance
✅ **Immutable Audit**: Every actuation recorded on Hedera blockchain
✅ **Smooth Control**: Primal Logic exponential memory weighting
✅ **Stability Guarantee**: Lipschitz bound < 1.0
✅ **Real-time Feedback**: Smoothness score, metrics, transaction hash

---

## 📊 Available Demos & Examples

### 1. **Interactive LAM Session**
```bash
python -i lam_interactive_session.py
>>> demo1()  # 4-sensor distributed fusion
>>> demo2()  # Methods comparison
>>> demo3()  # Trust-gated control
>>> demo4()  # Load shedding
>>> demo5()  # Multi-agent sync
>>> demo6()  # Step response
```

### 2. **Temporal Displacement**
```bash
# Quick validation
python lam/smoke_test.py

# Full test suite with plots
python lam/test_temporal_displacement.py

# Performance benchmarks
python lam/benchmark_temporal_displacement.py
```

### 3. **Actuator Control with Token Burns**
```bash
# GoTrax hoverboard integration
python lam/integrations/gotrax_hoverboard_integration.py

# Shows:
# - Token deposit
# - Balance checking
# - Token burn on actuation
# - Smooth motor control
# - Blockchain transaction
```

### 4. **Distributed Control**
```bash
# 4 sensors with varying latencies
python lam/example_distributed_control.py

# Demonstrates:
# - Trust-weighted sensor fusion
# - Latency compensation
# - Temporal displacement
# - Real-time synchronization
```

---

## 🎯 Quick Test Commands

```bash
# Test 1: Verify Python environment
python -c "import numpy, matplotlib; print('✓ Dependencies OK')"

# Test 2: Check Primal Logic constants
python -c "from extras.primal.primal_constants import DONTE_CONSTANT, KERNEL_MU; print(f'D={DONTE_CONSTANT}, λ={KERNEL_MU}')"

# Test 3: Test Hedera configuration
python -c "from dotenv import load_dotenv; import os; load_dotenv(); print('Network:', os.getenv('HEDERA_NETWORK'))"

# Test 4: Run smoke test
python lam/smoke_test.py

# Test 5: Test actuator integration
python lam/integrations/gotrax_hoverboard_integration.py
```

---

## 🛠️ Troubleshooting

### Issue: "ModuleNotFoundError: numpy"
```bash
pip install numpy matplotlib scipy
```

### Issue: "No module named 'dotenv'"
```bash
pip install python-dotenv
```

### Issue: Import errors in LAM modules
```bash
# Make sure you're in the repo root
cd /path/to/MotorHandPro
export PYTHONPATH=$PWD:$PYTHONPATH
python lam_interactive_session.py
```

### Issue: Docker services not starting
```bash
# Check Docker is running
docker --version
docker ps

# View logs
docker-compose logs

# Restart services
docker-compose down
docker-compose up -d
```

### Issue: Hedera connection failed
```bash
# Verify .env file exists
ls -la .env

# Check credentials are loaded
python -c "from dotenv import load_dotenv; import os; load_dotenv(); print(os.getenv('HEDERA_OPERATOR_ID'))"

# Test with mock mode (no real blockchain)
# The integration uses mock mode by default for testing
```

---

## 📁 Project Structure

```
MotorHandPro/
├── lam/                              # LAM orchestration layer
│   ├── temporal_displacement.py      # Core temporal framework
│   ├── lam_temporal_integration.py   # LAM integration
│   ├── integrations/
│   │   └── gotrax_hoverboard_integration.py  # Actuator + token burn
│   ├── smoke_test.py                 # Quick validation
│   ├── benchmark_temporal_displacement.py
│   └── example_distributed_control.py
├── lam_interactive_session.py        # Interactive demo environment
├── lam_orchestrator.py               # Full system orchestrator
├── control_panel/                    # Web UI
├── extras/primal/                    # Primal Logic core
├── docs/                             # Documentation
├── .env.example                      # Hedera config template
└── docker-compose.yml                # Service definitions
```

---

## 🎓 Learning Path

### Day 1: Basics
1. Clone repository
2. Install dependencies
3. Run `lam/smoke_test.py`
4. Explore `demo1()` in interactive session

### Day 2: Temporal Displacement
1. Run `lam/test_temporal_displacement.py`
2. Experiment with `demo2()`, `demo3()`, `demo4()`
3. Read `lam/TEMPORAL_DISPLACEMENT.md`

### Day 3: Actuator Integration
1. Set up Hedera testnet account
2. Configure `.env` file
3. Run `gotrax_hoverboard_integration.py`
4. Observe token burns on actuation

### Day 4: Full Stack
1. Start Docker services
2. Launch control panel
3. Monitor real-time telemetry
4. Build custom integrations

---

## 🔗 Useful Links

- **Hedera Portal**: https://portal.hedera.com
- **Hedera Docs**: https://docs.hedera.com
- **Project Docs**: `docs/README.md`
- **API Reference**: `docs/api/PYTHON_API.md`
- **Architecture**: `docs/ARCHITECTURE.md`

---

## 💡 Next Steps

After local setup, you can:

1. **Customize Primal Logic parameters** (λ, D, KE)
2. **Integrate your own actuators** (follow GoTrax example)
3. **Deploy to production** (see `docs/guides/DEPLOYMENT.md`)
4. **Connect real Hedera mainnet** (change `HEDERA_NETWORK=mainnet`)
5. **Build distributed control systems** (multi-robot coordination)

---

## 📞 Support

- **Issues**: https://github.com/STLNFTART/MotorHandPro/issues
- **Discussions**: https://github.com/STLNFTART/MotorHandPro/discussions
- **Email**: contact@stlnftart.com

---

**Patent Pending**: U.S. Provisional Patent Application No. 63/842,846

**License**: See LICENSE file for research evaluation terms
