# Python API Reference

Complete reference for the MotorHandPro Python API.

## Table of Contents

- [Core Kernel](#core-kernel)
- [Simulation Functions](#simulation-functions)
- [Analysis Utilities](#analysis-utilities)
- [LAM Orchestrator](#lam-orchestrator)
- [Constants and Configuration](#constants-and-configuration)
- [Examples](#examples)

## Core Kernel

### Module: `extras.primal.kernel_v4`

#### PrimalKernel

```python
class PrimalKernel:
    """
    Main Primal Logic kernel implementation.

    Attributes:
        lambda_val (float): Lightfoot constant (exponential decay rate)
        KE (float): Error gain
        D (float): Donte constant (fixed point)
        lipschitz (float): Lipschitz constant F'(D)
    """

    def __init__(self, lambda_val=0.16905, KE=0.3):
        """
        Initialize Primal Logic kernel.

        Args:
            lambda_val (float): Lightfoot constant (default: 0.16905)
            KE (float): Error gain (default: 0.3)

        Raises:
            ValueError: If parameters violate stability conditions
        """
```

**Example:**
```python
from extras.primal.kernel_v4 import PrimalKernel

kernel = PrimalKernel(lambda_val=0.16905, KE=0.3)
print(f"Lipschitz constant: {kernel.lipschitz}")
```

---

#### compute_D()

```python
def compute_D(mu=0.16905, tol=1e-12, max_iter=1000000):
    """
    Compute Donte constant via fixed-point iteration.

    Args:
        mu (float): Kernel parameter (typically equals lambda_val)
        tol (float): Convergence tolerance (default: 1e-12)
        max_iter (int): Maximum iterations (default: 1000000)

    Returns:
        float: Donte constant D (approximately 149.9992314000)

    Raises:
        RuntimeError: If iteration doesn't converge
    """
```

**Example:**
```python
D = compute_D(mu=0.16905)
print(f"D = {D:.10f}")  # D = 149.9992314000
```

---

#### planck_tail()

```python
def planck_tail(X, eps_term=1e-20):
    """
    Compute Planck tail series.

    Args:
        X (float): Cutoff threshold
        eps_term (float): Termination epsilon (default: 1e-20)

    Returns:
        float: Sum of Planck tail series
    """
```

**Example:**
```python
tail = planck_tail(19.358674138784)
I3 = 6.4939394023  # π^4/15
print(f"δ(Xc)/I3 = {tail/I3:.10e}")
```

---

#### compute_lipschitz()

```python
def compute_lipschitz(lambda_val, D=None):
    """
    Compute Lipschitz constant F'(D).

    Args:
        lambda_val (float): Lightfoot constant
        D (float, optional): Donte constant (computed if None)

    Returns:
        float: Lipschitz constant (must be < 1.0 for stability)
    """
```

**Example:**
```python
lipschitz = compute_lipschitz(0.16905)
assert lipschitz < 1.0, "Stability violated!"
print(f"F'(D) = {lipschitz:.12f}")  # 0.000129931830
```

---

#### verify_stability()

```python
def verify_stability(lambda_val, KE, verbose=False):
    """
    Verify stability conditions for given parameters.

    Args:
        lambda_val (float): Lightfoot constant
        KE (float): Error gain
        verbose (bool): Print diagnostic information

    Returns:
        bool: True if stable, False otherwise
    """
```

**Example:**
```python
if verify_stability(0.16905, 0.3, verbose=True):
    print("Parameters are stable ✓")
else:
    print("WARNING: Unstable parameters!")
```

---

## Simulation Functions

### Module: `extras.primal.primal_algorithms`

#### simulate_step_response()

```python
def simulate_step_response(lambda_val=0.16905, KE=0.3, duration=30.0, dt=0.01):
    """
    Simulate step response.

    Args:
        lambda_val (float): Lightfoot constant
        KE (float): Error gain
        duration (float): Simulation duration (seconds)
        dt (float): Time step (seconds)

    Returns:
        SimulationResults: Object containing:
            - time (np.ndarray): Time array
            - psi (np.ndarray): Control signal
            - gamma (np.ndarray): Error signal
            - Ec (np.ndarray): Control energy
            - settling_time (float): Time to settle within 2%
            - overshoot (float): Maximum overshoot percentage
    """
```

**Example:**
```python
results = simulate_step_response(lambda_val=0.16905, KE=0.3, duration=30.0)

print(f"Settling time: {results.settling_time:.2f} s")
print(f"Overshoot: {results.overshoot:.2f} %")

results.plot()  # Generate plots
```

---

#### simulate_sine_tracking()

```python
def simulate_sine_tracking(
    lambda_val=0.16905,
    KE=0.3,
    frequency=0.5,
    amplitude=1.0,
    duration=20.0,
    dt=0.01
):
    """
    Simulate sinusoidal reference tracking.

    Args:
        lambda_val (float): Lightfoot constant
        KE (float): Error gain
        frequency (float): Sine wave frequency (Hz)
        amplitude (float): Sine wave amplitude
        duration (float): Simulation duration (seconds)
        dt (float): Time step (seconds)

    Returns:
        SimulationResults: Tracking results with RMS error
    """
```

**Example:**
```python
results = simulate_sine_tracking(
    lambda_val=0.16905,
    KE=0.3,
    frequency=0.5,
    amplitude=1.0
)

print(f"RMS tracking error: {results.rms_error:.6f}")
```

---

#### simulate_disturbance()

```python
def simulate_disturbance(
    lambda_val=0.16905,
    KE=0.3,
    disturbance_time=5.0,
    disturbance_magnitude=0.5,
    duration=30.0,
    dt=0.01
):
    """
    Simulate external disturbance rejection.

    Args:
        lambda_val (float): Lightfoot constant
        KE (float): Error gain
        disturbance_time (float): Time when disturbance occurs
        disturbance_magnitude (float): Disturbance strength
        duration (float): Simulation duration (seconds)
        dt (float): Time step (seconds)

    Returns:
        SimulationResults: Disturbance rejection results
    """
```

**Example:**
```python
results = simulate_disturbance(
    disturbance_time=5.0,
    disturbance_magnitude=0.5
)

print(f"Recovery time: {results.recovery_time:.2f} s")
```

---

## Analysis Utilities

### Module: `analyze_runs`

#### analyze_csv()

```python
def analyze_csv(filename):
    """
    Analyze benchmark CSV file.

    Args:
        filename (str): Path to CSV file

    Returns:
        dict: Analysis results containing:
            - max_psi: Maximum control signal
            - zero_crossing_time: First zero-crossing time
            - settling_time: 2% settling time
            - final_Ec: Final control energy
            - lipschitz_estimate: Estimated Lipschitz constant
    """
```

**Example:**
```python
from analyze_runs import analyze_csv

results = analyze_csv('run_default.csv')

print(f"Max ψ: {results['max_psi']:.4f}")
print(f"Settling time: {results['settling_time']:.2f} s")
print(f"Final Ec: {results['final_Ec']:.6f}")
```

---

#### generate_plots()

```python
def generate_plots(filename, output_filename=None):
    """
    Generate time-series plots from CSV data.

    Args:
        filename (str): Input CSV file
        output_filename (str, optional): Output PNG file
            (default: input filename with _plot.png suffix)

    Returns:
        str: Path to generated plot file
    """
```

**Example:**
```python
plot_file = generate_plots('run_default.csv')
print(f"Plot saved to: {plot_file}")
```

---

### Module: `comprehensive_data_visualization`

#### visualize_mission_data()

```python
def visualize_mission_data(csv_file, output_dir='comprehensive_visualizations'):
    """
    Generate comprehensive multi-panel visualizations.

    Args:
        csv_file (str): Mission data CSV file
        output_dir (str): Output directory for plots

    Creates:
        - Time-series plots
        - Phase portraits
        - Statistical summaries
    """
```

**Example:**
```python
visualize_mission_data('nasa_compliant_moderate_shield10gcm2_mission_data.csv')
```

---

## LAM Orchestrator

### Module: `lam_orchestrator`

#### LAMOrchestrator

```python
class LAMOrchestrator:
    """
    Main orchestration layer for MotorHandPro system.

    Coordinates WebSocket, REST API, and MQTT communication.
    """

    def __init__(self, config=None):
        """
        Initialize LAM orchestrator.

        Args:
            config (dict, optional): Configuration dictionary
        """

    async def start(self):
        """Start orchestrator and all services."""

    async def stop(self):
        """Gracefully shutdown orchestrator."""

    async def send_command(self, command_type, parameters):
        """
        Send control command to system.

        Args:
            command_type (str): Command type ('set_params', 'start', 'stop', etc.)
            parameters (dict): Command parameters

        Returns:
            dict: Command response
        """
```

**Example:**
```python
import asyncio
from lam_orchestrator import LAMOrchestrator

async def main():
    orchestrator = LAMOrchestrator()
    await orchestrator.start()

    # Send command
    response = await orchestrator.send_command('set_params', {
        'lambda': 0.25,
        'KE': 0.4
    })

    print(f"Response: {response}")

    await orchestrator.stop()

asyncio.run(main())
```

---

#### REST API Endpoints

**GET /health**
```python
@app.get("/health")
async def health():
    """
    Health check endpoint.

    Returns:
        dict: {'status': 'healthy', 'version': '1.0'}
    """
```

**POST /api/params**
```python
@app.post("/api/params")
async def set_parameters(params: Parameters):
    """
    Set Primal Logic parameters.

    Args:
        params (Parameters): {
            'lambda': float,
            'KE': float
        }

    Returns:
        dict: {
            'status': 'success',
            'lipschitz': float,
            'stable': bool
        }
    """
```

**GET /api/state**
```python
@app.get("/api/state")
async def get_state():
    """
    Get current system state.

    Returns:
        dict: {
            'psi': float,
            'gamma': float,
            'Ec': float,
            'timestamp': str
        }
    """
```

**Example:**
```python
import requests

# Set parameters
response = requests.post('http://localhost:8000/api/params', json={
    'lambda': 0.16905,
    'KE': 0.3
})

print(response.json())

# Get state
state = requests.get('http://localhost:8000/api/state').json()
print(f"Current ψ: {state['psi']}")
```

---

## Constants and Configuration

### Module: `extras.primal.primal_constants`

#### Fundamental Constants

```python
# Donte constant (fixed point)
DONTE_CONSTANT = 149.9992314000

# Planck constants
PLANCK_I3 = 6.4939394023  # π^4/15
PLANCK_SCALE = 23.098341716530  # D/I3

# Kernel parameters
LIGHTFOOT_LAMBDA = 0.16905  # Exponential decay rate
KERNEL_MU = 0.16905  # Kernel iteration parameter (= lambda)

# Cutoff parameters
CUTOFF_XC = 19.358674138784
CUTOFF_DELTA = 0.000005124001

# Lipschitz constant
F_PRIME_D = 0.000129931830  # Must be < 1.0 for stability
```

**Usage:**
```python
from extras.primal.primal_constants import *

print(f"D = {DONTE_CONSTANT}")
print(f"λ = {LIGHTFOOT_LAMBDA}")
print(f"F'(D) = {F_PRIME_D}")
```

---

## Examples

### Complete Control Loop

```python
import numpy as np
from extras.primal.kernel_v4 import PrimalKernel

# Initialize
kernel = PrimalKernel(lambda_val=0.16905, KE=0.3)

# State variables
psi = 0.0
Ec = 0.0

# Simulation parameters
dt = 0.01  # 10ms time step
duration = 30.0  # seconds

# Storage
history = {
    'time': [],
    'psi': [],
    'gamma': [],
    'Ec': []
}

# Main loop
for n in range(int(duration / dt)):
    t = n * dt

    # Reference signal
    reference = 1.0 if t > 1.0 else 0.0

    # Feedback (for now, assume psi is the output)
    actual = psi

    # Error
    gamma = reference - actual

    # Primal Logic control law
    dpsi_dt = -kernel.lambda_val * psi + kernel.KE * gamma
    psi += dpsi_dt * dt

    # Control energy
    Ec += psi * gamma * dt

    # Store history
    history['time'].append(t)
    history['psi'].append(psi)
    history['gamma'].append(gamma)
    history['Ec'].append(Ec)

# Convert to numpy arrays
for key in history:
    history[key] = np.array(history[key])

# Analysis
settling_idx = np.where(np.abs(history['gamma']) < 0.02)[0]
if len(settling_idx) > 0:
    settling_time = history['time'][settling_idx[0]]
    print(f"Settling time: {settling_time:.2f} s")

print(f"Final Ec: {history['Ec'][-1]:.6f}")
print(f"Max ψ: {history['psi'].max():.4f}")
```

### Parameter Sweep

```python
import itertools
from extras.primal.kernel_v4 import verify_stability, simulate_step_response

lambdas = [0.10, 0.16905, 0.25]
KEs = [0.2, 0.3, 0.4]

results = []

for lam, ke in itertools.product(lambdas, KEs):
    if not verify_stability(lam, ke):
        print(f"λ={lam}, KE={ke}: UNSTABLE")
        continue

    sim = simulate_step_response(lam, ke, duration=30.0)

    results.append({
        'lambda': lam,
        'KE': ke,
        'settling_time': sim.settling_time,
        'overshoot': sim.overshoot,
        'final_Ec': sim.Ec[-1]
    })

# Find best configuration
best = min(results, key=lambda r: r['settling_time'])
print(f"Best config: λ={best['lambda']}, KE={best['KE']}")
print(f"  Settling time: {best['settling_time']:.2f} s")
```

### Real-Time Data Streaming

```python
import asyncio
import websockets
import json

async def stream_data():
    uri = "ws://localhost:8765"

    async with websockets.connect(uri) as websocket:
        # Subscribe to updates
        await websocket.send(json.dumps({
            'type': 'subscribe',
            'channels': ['state']
        }))

        # Receive updates
        while True:
            message = await websocket.recv()
            data = json.loads(message)

            print(f"t={data['time']:.2f}: "
                  f"ψ={data['psi']:.4f}, "
                  f"γ={data['gamma']:.4f}, "
                  f"Ec={data['Ec']:.6f}")

asyncio.run(stream_data())
```

---

**See Also:**
- [C++ API Reference](CPP_API.md)
- [JavaScript API Reference](JAVASCRIPT_API.md)
- [REST API Reference](REST_API.md)
- [WebSocket API Reference](WEBSOCKET_API.md)

---

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
