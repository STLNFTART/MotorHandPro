# LAM - Large Action Model Framework

**Quantum-Semantic Intelligence for Real-World Task Execution**

A Large Action Model that combines quantum-inspired algorithms with practical task execution, powered by the Lightfoot and Donte constants from the Primal Logic framework.

## Overview

LAM (Large Action Model) is an intelligent agent system that helps users accomplish real-world tasks through natural interaction. It integrates:

- **Primal Logic Constants**: Lightfoot (λ = 0.16905) and Donte (D = 149.9992314000) constants for quantum-semantic stability
- **Task Execution**: Trip planning, reservations, food ordering, subscription management
- **Setup Wizard**: Secure credential management for service integration
- **Lab Assistant**: Experiment goal tracking and scientific workflow support
- **Quantum Resonance Field**: Stability guarantees through bounded convergence

## Key Features

### 🎯 Actions
- **Trip Planning**: Optimize travel routes, find flights, book accommodations
- **Reservations**: Make and manage restaurant/event reservations
- **Food Ordering**: Order delivery from restaurants
- **Subscription Management**: Cancel or modify subscriptions

### 🔧 Setup & Management
- **Setup Wizard**: Interactive configuration for services
- **Credential Management**: Encrypted storage of API keys and credentials
- **Lab Assistant**: Experiment goal tracking and guidance

### 💬 Intelligence
- **Question Answering**: Answer questions about repository and experiments
- **Task Completion**: Execute general tasks with quantum optimization
- **Resonance Monitoring**: Real-time stability tracking with Lightfoot/Donte constants

## Installation

### Requirements

```bash
pip install -r requirements.txt
```

**requirements.txt:**
```
requests>=2.31.0
cryptography>=41.0.0
```

### Quick Start

```bash
# Interactive mode (recommended)
python lam/lam_main.py

# Setup wizard only
python lam/lam_main.py wizard

# Lab assistant only
python lam/lam_main.py lab

# System status
python lam/lam_main.py status
```

## Architecture

### Core Components

```
lam/
├── core/
│   └── primal_lam.py          # Core LAM engine with resonance field
├── actions/
│   └── action_executors.py    # Task execution (trips, reservations, etc.)
├── wizards/
│   └── setup_wizard.py         # Credential and service configuration
├── assistants/
│   └── lab_assistant.py        # Experiment goal management
├── config/                     # Configuration storage
├── lam_main.py                 # Main unified interface
└── README.md                   # This file
```

### Lightfoot & Donte Constants

The LAM framework integrates the Primal Logic constants for quantum-semantic stability:

- **λ (Lightfoot Constant)**: 0.16905
  - Exponential decay rate for memory weighting
  - Time constant τ = 1/λ ≈ 5.92 seconds
  - Controls how quickly the system "forgets" past states

- **D (Donte Constant)**: 149.9992314000
  - Fixed-point attractor for system convergence
  - Ensures bounded stability without integral windup
  - Derived from Planck tail cutoff calculation

- **Lipschitz Constant**: F'(D) ≈ 0.000129931830
  - Guarantees contraction mapping (< 1.0)
  - Ensures bounded convergence to optimal solutions
  - Stability proof through exponential memory weighting

### Quantum Resonance Field

The resonance field monitors system stability using:

```python
alpha: 0.54        # Temporal weighting (bounds: 0.52-0.56)
lambda: 0.12       # Memory decay (bounds: 0.11-0.12)
```

These parameters drift over time but are constrained by semantic bounds to maintain stability.

## Usage Examples

### Interactive Mode

```bash
python lam/lam_main.py
```

Commands:
- `/trip` - Plan a trip
- `/reserve` - Make a reservation
- `/order` - Order food
- `/cancel-sub` - Cancel subscription
- `/wizard` - Run setup wizard
- `/lab` - Launch lab assistant
- `/ask <question>` - Ask a question
- `/task <description>` - Complete a task
- `/status` - System status
- `/quit` - Exit

### Setup Wizard

Configure services and credentials:

```bash
python lam/lam_main.py wizard
```

Services supported:
1. API Server
2. Email Service
3. Calendar Service
4. Food Delivery
5. Travel Booking
6. Subscription Management

Credentials are encrypted using `cryptography.fernet` and stored securely.

### Lab Assistant

Manage experiment goals:

```bash
python lam/lam_main.py lab
```

Features:
- Create experiment goals from templates
- Track progress and status
- Record results
- Analyze outcomes against success criteria
- Get recommendations and next steps

Templates available:
- Motor Control Experiment
- Algorithm Validation
- Quantum Resonance Field Study
- Satellite System Integration

### Programmatic Usage

```python
from lam.lam_main import LAM

# Initialize LAM
lam = LAM()

# Plan a trip
trip = lam.plan_trip(
    destination="Paris",
    departure="2025-12-15",
    return_date="2025-12-22",
    budget=2000.00
)

# Make a reservation
reservation = lam.make_reservation(
    venue_type="restaurant",
    venue_name="The French Bistro",
    date="2025-12-16",
    time="19:00",
    party_size=2
)

# Order food
order = lam.order_food(
    restaurant="Joe's Pizza",
    items=[
        {"name": "Large Pepperoni", "price": 18.99, "quantity": 1}
    ],
    address="123 Main St"
)

# Ask a question
answer = lam.ask_question("What are the Lightfoot and Donte constants?")

# Get system status
status = lam.get_status()
```

## Component Details

### Primal LAM (Core Engine)

File: `core/primal_lam.py`

**QuantumResonanceField Class:**
- Manages resonance parameters (alpha, lambda)
- Updates with exponential decay based on Lightfoot constant
- Monitors semantic bounds and stability
- Computes Lipschitz constant for convergence guarantee

**PrimalLAM Class:**
- Main LAM interface
- Action recording with resonance updates
- Trip planning with quantum optimization
- Question answering using semantic resonance
- Task completion with stability monitoring

### Action Executors

File: `actions/action_executors.py`

**TripPlanner:**
- Budget analysis
- Flight search (simulated, extendable to real APIs)
- Accommodation search
- Itinerary generation
- Cost estimation

**ReservationManager:**
- Make reservations
- Cancel reservations
- Confirmation code generation

**FoodOrderer:**
- Food ordering with item lists
- Delivery time estimation
- Price calculation with tax and fees

**SubscriptionManager:**
- Cancel subscriptions
- Modify subscription plans
- List active subscriptions

**ActionOrchestrator:**
- Unified interface for all executors
- Credential management
- Error handling

### Setup Wizard

File: `wizards/setup_wizard.py`

**Features:**
- Interactive service configuration
- Encrypted credential storage
- Service templates
- Configuration testing
- Export/import capabilities

**Security:**
- Fernet encryption for credentials
- Key stored with 0600 permissions
- Credentials file with 0600 permissions
- No plaintext credential storage

### Lab Assistant

File: `assistants/lab_assistant.py`

**ExperimentGoal Class:**
- Goal metadata and tracking
- Success criteria definition
- Parameter storage
- Results recording
- Status management

**LabAssistant Class:**
- Goal creation and management
- Template-based experiment design
- Progress calculation
- Recommendation generation
- Next step suggestions

## Configuration

Configuration is stored in `~/.lam/` by default:

```
~/.lam/
├── config/
│   ├── credentials.enc    # Encrypted credentials
│   ├── services.json      # Service configuration
│   └── .key               # Encryption key (0600 permissions)
└── experiments/
    └── goals.json         # Experiment goals
```

## Advanced Features

### Resonance Field Monitoring

Monitor quantum-semantic stability in real-time:

```python
from lam.core.primal_lam import QuantumResonanceField

field = QuantumResonanceField()

# Get current state
state = field.get_state()
print(f"Lipschitz: {state['lipschitz_constant']}")
print(f"Stability: {field.check_semantic_bounds()}")

# Update with action count
field.update_resonance_parameters(action_count=100)
```

### Custom Action Executors

Extend the framework with custom executors:

```python
from lam.actions.action_executors import ActionExecutor

class CustomExecutor(ActionExecutor):
    def execute(self, **kwargs):
        # Your implementation
        result = {"success": True, "data": "..."}
        self._record_execution("custom_action", kwargs, result)
        return result

# Add to orchestrator
orchestrator.custom = CustomExecutor()
```

## Theory & Mathematics

### Exponential Memory Weighting

The LAM uses exponential memory weighting to maintain bounded stability:

```
α(t+1) = α(t) + drift · e^(-t/τ)

where:
  τ = 1/λ = 1/0.16905 ≈ 5.92 seconds
  drift ~ U(-0.0001, 0.0001)
```

This ensures the system naturally forgets old state with time constant τ, preventing unbounded integration.

### Fixed-Point Convergence

The Donte constant D acts as an attractor:

```
F(x) = convergence function
F(D) = D  (fixed point)
F'(D) < 1 (contraction)
```

The Lipschitz constant F'(D) ≈ 0.000129931830 proves bounded convergence.

### Semantic Bounds

Parameters are constrained to semantic validation bounds:

```
0.52 ≤ α ≤ 0.56
0.11 ≤ λ ≤ 0.12
```

Exceeding these bounds triggers system protection to prevent collapse.

## Patent & License

**Patent Pending**: U.S. Provisional Patent Application No. 63/842,846
**Filed**: July 12, 2025
**Title**: Method and System for Bounded Autonomous Vehicle Control Using Exponential Memory Weighting

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety

**Notice**: Repository for research evaluation only.

Contact: Donte Lightfoot (STLNFTART) for collaboration, licensing, or deployment inquiries.

## Integration with MotorHandPro

This LAM framework integrates seamlessly with the MotorHandPro repository:

- Uses primal constants from `extras/primal/primal_constants.py`
- Compatible with Primal Logic control framework
- Supports experiment tracking for motor control tests
- Quantum-semantic resonance monitoring for all operations

## Troubleshooting

### Import Errors

If you get import errors:

```bash
# Ensure you're in the repo root
cd /path/to/MotorHandPro

# Run with Python module syntax
python -m lam.lam_main
```

### Credential Issues

If credentials aren't loading:

```bash
# Check permissions
ls -la ~/.lam/config/

# Should show:
# -rw------- .key
# -rw------- credentials.enc

# Fix permissions if needed
chmod 600 ~/.lam/config/.key
chmod 600 ~/.lam/config/credentials.enc
```

### Resonance Field Instability

If you see "RESONANCE FIELD UNSTABLE" messages:

- This is a safety feature, not an error
- The system has detected parameters approaching semantic bounds
- Reduce action frequency to allow field stabilization
- Check `/status` for current resonance parameters

## Development

### Running Tests

```bash
# Test action executors
python lam/actions/action_executors.py

# Test setup wizard
python lam/wizards/setup_wizard.py

# Test lab assistant
python lam/assistants/lab_assistant.py

# Test core LAM
python lam/core/primal_lam.py
```

### Adding New Features

1. Create executor in `actions/`
2. Add to `ActionOrchestrator`
3. Add command to `lam_main.py` interactive mode
4. Update documentation

## Future Enhancements

- [ ] Real API integrations (flights, hotels, food delivery)
- [ ] Natural language processing for commands
- [ ] Multi-modal interaction (voice, image)
- [ ] Distributed resonance field across multiple agents
- [ ] Learning from action outcomes
- [ ] Integration with external knowledge bases
- [ ] Mobile app interface

## References

- [Primal Logic Framework Documentation](../PRIMAL_LOGIC_FRAMEWORK.md)
- [MotorHandPro README](../README.md)
- [Patent Application](U.S. Provisional No. 63/842,846)

## Support

For questions, issues, or collaboration:

- GitHub Issues: https://github.com/STLNFTART/MotorHandPro/issues
- Contact: Donte Lightfoot (STLNFTART)

---

**Built with quantum-semantic intelligence. Powered by Lightfoot & Donte constants.**
