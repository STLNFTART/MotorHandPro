## # MotorHandPro: APL & Prolog Rewrite

**Complete rewrite of MotorHandPro in APL (mathematics) and Prolog (logic)**

This repository has been rewritten to leverage the strengths of both APL and Prolog, creating a powerful hybrid system for mathematical computation and logical reasoning.

## 🎯 Overview

The MotorHandPro system implements the **Primal Logic** framework using:
- **APL** - Array-oriented mathematical operations
- **Prolog** - Logic programming and rule-based inference
- **Python Bridge** - Integration layer between both systems

### Why APL + Prolog?

| Language | Strengths | Use Cases |
|----------|-----------|-----------|
| **APL** | Concise array operations, mathematical precision, implicit parallelism | Primal Logic kernel, time series analysis, NASA simulations |
| **Prolog** | Declarative logic, inference engine, knowledge representation | LAM orchestration, goal tracking, experiment management |

## 📁 Repository Structure

```
MotorHandPro/
├── apl/                          # APL mathematical components
│   ├── core/
│   │   ├── constants.apl         # Universal constants
│   │   └── quant.apl             # Primal Logic kernel
│   ├── data/
│   │   └── timeseries.apl        # Time series analysis
│   ├── simulations/
│   │   └── nasa.apl              # Mars mission simulations
│   └── utils/
│       └── io.apl                # I/O utilities
│
├── prolog/                       # Prolog logic components
│   ├── core/
│   │   ├── kb.pl                 # Knowledge base
│   │   └── lam.pl                # LAM orchestration
│   ├── actions/
│   │   ├── trip.pl               # Trip planning
│   │   └── reserve.pl            # Reservations
│   ├── experiments/
│   │   ├── goals.pl              # Goal management
│   │   └── datasets.pl           # Dataset discovery
│   └── blockchain/
│       └── token.pl              # Token logic
│
├── bridge/                       # Integration layer
│   ├── apl_prolog_bridge.py      # Python bridge
│   ├── rest_server.py            # REST API
│   └── mqtt_bridge.py            # MQTT integration
│
└── docs/                         # Documentation
    ├── APL_PROLOG_ARCHITECTURE.md
    └── GETTING_STARTED.md
```

## 🚀 Quick Start

### Prerequisites

```bash
# Install Dyalog APL
# Download from: https://www.dyalog.com/download-zone.htm

# Install SWI-Prolog
sudo apt-get install swi-prolog

# Install Python dependencies
pip install -r bridge/requirements.txt
```

### Running APL Components

```bash
# Load APL workspace
dyalog

# In APL session:
]LOAD apl/core/constants.apl
]LOAD apl/core/quant.apl

# Run demonstration
Main

# Run tests
RunTests
```

### Running Prolog Components

```bash
# Start SWI-Prolog
swipl

# Load modules
?- [prolog/core/kb].
?- [prolog/core/lam].

# Validate constants
?- validate_constants.

# Run tests
?- run_tests.
```

### Running Integration Bridge

```bash
# Start REST API server
cd bridge
python rest_server.py

# API available at http://localhost:8000
# Documentation at http://localhost:8000/docs
```

## 🔷 Core Components

### 1. Primal Logic Kernel (APL)

The mathematical heart of the system:

```apl
⍝ Load the kernel
]LOAD apl/core/quant.apl

⍝ Compute fixed-point iteration
result ← PrimalConverge 0
⍝ → 149.9992314 (Donte constant)

⍝ Verify stability
stable ← VerifyStability
⍝ → 1 (stable)

⍝ Generate exponential weights
weights ← ExponentialWeights 100
⍝ → 100-element decay vector using λ = 0.16905
```

**Key Functions:**
- `PrimalConverge` - Fixed-point iteration
- `ExponentialWeights` - Memory weighting
- `VerifyStability` - Stability analysis
- `PlanckTailSum` - Series computation
- `UpdateQuantumField` - Field dynamics

### 2. LAM Orchestration (Prolog)

Logical action planning and execution:

```prolog
% Load LAM system
?- [prolog/core/lam].

% Execute an action
?- execute_action(
    action(question_answering, ["What is Lightfoot?"], context(1, user, env)),
    context(1, user, env),
    Result
).

% Check quantum stability
?- quantum_field_stable(context(1, user, env)).

% Get action statistics
?- action_stats(Stats).
```

**Key Predicates:**
- `execute_action/3` - Execute with stability checking
- `plan_action/3` - Decompose into steps
- `quantum_field_stable/1` - Verify field state
- `track_goal/3` - Goal management
- `coordinate_actions/3` - Multi-action execution

### 3. Goal Management (Prolog)

Track research goals with dependencies:

```prolog
% Load goal system
?- [prolog/experiments/goals].

% Define a goal
?- define_goal(
    analyze_data,
    "Analyze Mars mission data",
    [collect_data, clean_data]
).

% Update progress
?- update_progress(analyze_data, 0.5, "Halfway done").

% Check status
?- track_goal(analyze_data, Status, Recommendations).

% Generate report
?- goal_report(analyze_data).
```

### 4. Integration Bridge (Python)

Connect APL and Prolog via HTTP/REST:

```python
from bridge.apl_prolog_bridge import IntegrationBridge

bridge = IntegrationBridge()

# Verify system stability
stable = bridge.verify_stability()

# Execute LAM action
result = bridge.execute_lam_action(
    "trip_planning",
    ["NYC", "SF", "2025-06-01"],
    {"time": 1.0, "user": "test", "env": "prod"}
)

# Run Mars simulation
sim_result = bridge.run_mars_mission_simulation(
    days=520,
    shielding=10
)
```

## 📊 REST API

### Endpoints

#### Primal Logic
- `GET /constants` - Get universal constants
- `GET /stability` - Check system stability
- `GET /quantum/state` - Get quantum field state
- `POST /quantum/update` - Update with observations

#### LAM Actions
- `POST /action/execute` - Execute action
- `GET /action/history/{type}` - Get action history
- `GET /action/stats` - Get statistics

#### NASA Simulations
- `POST /nasa/simulate` - Run Mars mission simulation
- `GET /nasa/missions` - Get stored results

#### Goal Management
- `POST /goals/define` - Define new goal
- `POST /goals/progress` - Update progress
- `GET /goals/status/{id}` - Get goal status
- `GET /goals/summary` - Get all goals summary

### Example API Usage

```bash
# Get constants
curl http://localhost:8000/constants

# Check stability
curl http://localhost:8000/stability

# Execute action
curl -X POST http://localhost:8000/action/execute \
  -H "Content-Type: application/json" \
  -d '{
    "action_type": "question_answering",
    "params": ["What is the Donte constant?"],
    "context": {"time": 1.0, "user": "api", "env": "prod"}
  }'

# Run Mars simulation
curl -X POST http://localhost:8000/nasa/simulate \
  -H "Content-Type: application/json" \
  -d '{"days": 520, "shielding": 10}'
```

## 🧪 Testing

### APL Tests

```apl
]LOAD apl/core/quant.apl
RunTests
⍝ Test 1 - Convergence: PASS
⍝ Test 2 - Lipschitz: PASS
⍝ Test 3 - Weights: PASS
⍝ Test 4 - Stability: PASS
⍝ Test 5 - Semantic bounds: PASS
```

### Prolog Tests

```prolog
?- [prolog/core/kb].
?- run_tests.
% Test 1 - Convergence: PASS
% Test 2 - Stability: PASS
% Test 3 - Bounds check: PASS
% Test 4 - Quantum field: PASS

?- [prolog/core/lam].
?- run_tests.
% Test 1 - Validation: PASS
% Test 2 - Quantum stability: PASS
% Test 3 - Execution: PASS
% Test 4 - Planning: PASS
```

### Integration Tests

```bash
cd bridge
python apl_prolog_bridge.py --test-integration

# 🔷 Testing Full Integration...
# System stable: True
# Action result: {'success': True, ...}
```

## 🔑 Key Concepts

### Primal Logic Constants

| Constant | Value | Symbol | Purpose |
|----------|-------|--------|---------|
| **Lightfoot** | 0.16905 | λ | Exponential decay rate |
| **Donte** | 149.9992314 | D | Fixed-point attractor |
| **I3** | 6.4939394023 | I3 | Normalization constant |
| **Scaling** | 23.0983417165 | S | Control/energy ratio |
| **Lipschitz** | 0.000129931830 | F'(D) | Stability guarantee |

### Quantum Resonance Field

The quantum field tracks system state with three components:
- **Position** - Current state value
- **Velocity** - Rate of change
- **Acceleration** - Rate of velocity change

Updated using exponential decay:
```
new_pos = old_pos + e^(-λ) × (observation - old_pos)
```

### Semantic Bounds

Valid operational range: **[-6.5, 6.5]**

Values outside this range indicate system instability.

## 📚 Documentation

- [Architecture Overview](APL_PROLOG_ARCHITECTURE.md)
- [Branch Consolidation Plan](BRANCH_CLEANUP_PLAN.md)
- [Original Primal Logic Framework](PRIMAL_LOGIC_FRAMEWORK.md)

## 🎓 Learning Resources

### APL
- [Dyalog APL Documentation](https://docs.dyalog.com/)
- [APL Wiki](https://aplwiki.com/)
- [Learning APL](https://xpqz.github.io/learnapl/)

### Prolog
- [SWI-Prolog Documentation](https://www.swi-prolog.org/pldoc/)
- [Learn Prolog Now](https://www.learnprolognow.org/)
- [The Power of Prolog](https://www.metalevel.at/prolog)

## 🔧 Development

### Adding New APL Functions

```apl
⍝ Create new file in apl/utils/myfunction.apl
∇ result ← MyFunction arg
  ⍝ Your implementation
  result ← arg × 2
∇

⍝ Load and test
]LOAD apl/utils/myfunction.apl
MyFunction 5
⍝ → 10
```

### Adding New Prolog Predicates

```prolog
% Create new file in prolog/actions/myaction.pl
:- module(myaction, [my_predicate/2]).

my_predicate(Input, Output) :-
    % Your implementation
    Output is Input * 2.

% Load and test
?- [prolog/actions/myaction].
?- my_predicate(5, Result).
% Result = 10
```

### Extending the API

```python
# In bridge/rest_server.py

@app.post("/myendpoint")
async def my_endpoint(data: MyRequest):
    """My new endpoint"""
    result = bridge.my_function(data.param)
    return {"result": result}
```

## 🐛 Troubleshooting

### APL Issues

**Problem**: "FILE NOT FOUND"
```bash
# Ensure you're in the correct directory
cd /home/user/MotorHandPro
dyalog
]LOAD apl/core/quant.apl
```

**Problem**: "SYNTAX ERROR"
```bash
# Check APL character encoding
# Use APL keyboard or copy from symbols.apl
```

### Prolog Issues

**Problem**: "ERROR: ... does not exist"
```bash
# Load dependencies first
?- [prolog/core/kb].
?- [prolog/core/lam].
```

**Problem**: "Out of local stack"
```bash
# Increase stack size
swipl --stack_limit=1g
```

### Bridge Issues

**Problem**: "Command not found: dyalog"
```bash
# Add Dyalog to PATH
export PATH=$PATH:/path/to/dyalog
```

**Problem**: "Connection refused on port 8000"
```bash
# Check if server is running
lsof -i :8000

# Restart server
python bridge/rest_server.py
```

## 📈 Performance

### APL Performance
- **Vector operations**: ~10-100x faster than Python loops
- **Memory efficiency**: Compact array representation
- **Parallelism**: Automatic SIMD vectorization

### Prolog Performance
- **Inference speed**: ~1000 queries/sec
- **Backtracking**: Efficient search strategies
- **Indexing**: Fast predicate lookup

### Integration Overhead
- **HTTP latency**: ~1-5ms per request
- **IPC overhead**: ~0.1ms per call
- **JSON parsing**: ~0.5ms per payload

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📝 License

See LICENSE file for details.

## 🙏 Acknowledgments

- **Dyalog Ltd** - For the excellent APL implementation
- **SWI-Prolog Team** - For the powerful Prolog system
- **FastAPI** - For the modern Python web framework

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/STLNFTART/MotorHandPro/issues)
- **Discussions**: [GitHub Discussions](https://github.com/STLNFTART/MotorHandPro/discussions)
- **Email**: support@example.com

---

**Built with 💻 using APL + Prolog**
