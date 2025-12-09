# API Reference

Complete API documentation for MotorHandPro across all supported languages.

## 📚 Language-Specific APIs

- [Python API](#python-api)
- [C++ API](#c-api)
- [D Language API](#d-language-api)
- [REST API](#rest-api)
- [WebSocket API](#websocket-api)

---

## Python API

### Core Primal Logic Kernel

#### `PrimalKernel` Class

```python
from extras.primal.kernel_v4 import PrimalKernel

# Initialize kernel
kernel = PrimalKernel(
    lambda_val=0.16905,  # Exponential decay rate
    ke=0.5,              # Error gain
    d0=149.9992          # Donte constant
)

# Compute control output
psi_next = kernel.step(
    psi_current=1.0,     # Current control command
    error=0.01,          # Tracking error
    dt=0.01              # Time step
)
```

**Methods**:

- `step(psi_current, error, dt)` → `float`
  - Compute next control command
  - Returns: Updated ψ value

- `reset()` → `None`
  - Reset kernel state

- `get_energy()` → `float`
  - Get current control energy Ec(t)

- `compute_constants()` → `dict`
  - Compute Primal Logic constants
  - Returns: `{'D': ..., 'I3': ..., 'S': ..., 'Xc': ...}`

### Temporal Displacement

#### `TemporalDisplacedField` Class

```python
from lam.temporal_displacement import TemporalDisplacedField

# Create temporal field
field = TemporalDisplacedField(
    field_name="control_signal",
    displacement_sec=2.0,      # 2 second displacement
    method="interpolated"       # 'direct', 'buffered', or 'interpolated'
)

# Set value at current time
field.set(value=1.5, timestamp=None)  # None = current time

# Get displaced value
displaced_value = field.get_displaced(query_time=None)
```

**Methods**:

- `set(value, timestamp=None)` → `None`
  - Store value at timestamp
  - `timestamp=None` uses current time

- `get_displaced(query_time=None)` → `float`
  - Get value from the past
  - Returns value from (query_time - displacement)

- `get_causality_safe(query_time=None)` → `float`
  - Get displaced value with causality check
  - Returns None if requesting future data

- `clear_old_data(retain_seconds=3600)` → `None`
  - Clean up old historical data

**Properties**:

- `displacement_sec` - Displacement duration
- `method` - Displacement algorithm
- `history` - Historical data buffer

### LAM System

#### `LAMTemporalController` Class

```python
from lam.lam_temporal_integration import LAMTemporalController

# Initialize LAM controller
controller = LAMTemporalController(
    control_fields=['psi', 'gamma', 'error'],
    displacement_sec=1.5,
    update_rate_hz=100
)

# Start controller
await controller.start()

# Update control field
controller.update_field('psi', value=1.2)

# Get displaced field value
psi_displaced = controller.get_displaced('psi')

# Stop controller
await controller.stop()
```

**Async Methods**:

- `async start()` → `None`
  - Start LAM controller

- `async stop()` → `None`
  - Stop controller gracefully

- `async process_command(command: dict)` → `dict`
  - Process control command
  - Returns: Response dict

**Synchronous Methods**:

- `update_field(name, value)` → `None`
  - Update control field

- `get_displaced(name)` → `float`
  - Get displaced value

- `get_status()` → `dict`
  - Get controller status

### Analysis Tools

#### `analyze_runs` Module

```python
from analyze_runs import load_run, compute_metrics, plot_timeseries

# Load benchmark data
data = load_run('run_ke_0.5.csv')

# Compute metrics
metrics = compute_metrics(data)
# Returns: {'max_psi', 'zero_crossing_time', 'damping_slope', 'lipschitz_estimate'}

# Plot time series
plot_timeseries(data, output='plot.png')
```

---

## C++ API

### Primal Logic Kernel (Arduino/Embedded)

#### `quant_full.h` - Full Implementation

```cpp
#include "gen/quant_full.h"

// Compute constants at runtime
void setup() {
  Serial.begin(115200);

  // Compute Primal Logic constants
  double D = computeDonteConstant();
  double I3 = computeI3();
  double S = D / I3;
  double Xc = computeCutoff();

  Serial.print("D0 = ");
  Serial.println(D, 10);
}

// Control loop
void loop() {
  // Read sensor
  double y_actual = readSensor();
  double y_desired = getSetpoint();
  double error = y_desired - y_actual;

  // Apply control law
  psi += (-LAMBDA * psi + KE * error) * dt;

  // Send to actuator
  writeActuator(psi);
}
```

**Functions**:

- `double computeDonteConstant()` - Compute D₀ constant
- `double computeI3()` - Compute I3 normalization
- `double computeCutoff()` - Compute Xc cutoff
- `double planckTail(double x)` - Planck tail series

**Constants**:

```cpp
#define LAMBDA 0.16905      // Exponential decay rate
#define KE 0.5              // Error gain
#define D0 149.9992314000   // Donte constant
#define I3 6.4939394023     // Normalization
```

#### `quant_bridge.h` - Constants Only

```cpp
#include "gen/quant_bridge.h"

// Pre-computed constants (no runtime calculation)
// Use for production when constants are fixed
```

---

## D Language API

### High-Performance Temporal Displacement

```d
import lam.temporal_displacement;

// Create temporal field
auto field = new TemporalDisplacedField(
    "control_signal",
    2.0,  // displacement_sec
    DisplacementMethod.Interpolated
);

// Set value
field.set(1.5, Clock.currTime);

// Get displaced value
double displaced = field.getDisplaced(Clock.currTime);
```

**Performance**: 25-100x faster than Python implementation.

---

## REST API

### LAM REST Endpoints

Base URL: `http://localhost:8000/api/v1`

#### Authentication

```bash
# Login
POST /auth/login
{
  "username": "user",
  "password": "password"
}

# Response
{
  "token": "eyJ0eXAiOiJKV1QiLCJhbGc...",
  "expires": 3600
}
```

#### Control Commands

```bash
# Send control command
POST /control/command
Authorization: Bearer <token>
{
  "field": "psi",
  "value": 1.5,
  "timestamp": "2025-11-30T12:00:00Z"
}

# Response
{
  "status": "success",
  "displaced_value": 1.48,
  "energy": 0.23
}
```

#### Status & Telemetry

```bash
# Get system status
GET /status

# Response
{
  "status": "running",
  "uptime": 3600,
  "fields": {
    "psi": 1.5,
    "gamma": 0.01,
    "Ec": 0.23
  }
}

# Get telemetry history
GET /telemetry?field=psi&duration=60

# Response
{
  "field": "psi",
  "data": [
    {"t": 0.0, "value": 1.0},
    {"t": 0.01, "value": 1.01},
    ...
  ]
}
```

#### Configuration

```bash
# Get configuration
GET /config

# Update configuration
PUT /config
{
  "lambda": 0.16905,
  "ke": 0.5,
  "displacement_sec": 2.0
}
```

---

## WebSocket API

### Connection

```javascript
const ws = new WebSocket('ws://localhost:8765');

ws.onopen = () => {
  console.log('Connected to LAM');
};
```

### Message Format

```javascript
// Send control command
ws.send(JSON.stringify({
  type: 'control',
  field: 'psi',
  value: 1.5,
  timestamp: Date.now()
}));

// Receive updates
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Update:', data);
  // {type: 'update', field: 'psi', value: 1.48, displaced: 1.45}
};
```

### Event Types

| Type | Direction | Description |
|------|-----------|-------------|
| `control` | Client → Server | Send control command |
| `query` | Client → Server | Query field value |
| `subscribe` | Client → Server | Subscribe to field updates |
| `update` | Server → Client | Field value update |
| `status` | Server → Client | System status update |
| `error` | Server → Client | Error notification |

### Subscribe to Updates

```javascript
// Subscribe to real-time updates
ws.send(JSON.stringify({
  type: 'subscribe',
  fields: ['psi', 'gamma', 'Ec']
}));

// Receive periodic updates
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  if (data.type === 'update') {
    updateChart(data.field, data.value);
  }
};
```

---

## MQTT API

### Topics

```
motorhand/
  ├─ control/
  │  ├─ psi          (publish control commands)
  │  ├─ gamma        (publish error signals)
  │  └─ enable       (enable/disable control)
  │
  ├─ telemetry/
  │  ├─ psi          (subscribe to psi updates)
  │  ├─ gamma        (subscribe to gamma updates)
  │  └─ energy       (subscribe to Ec updates)
  │
  └─ status/
     ├─ health       (system health)
     └─ config       (configuration updates)
```

### Publishing Commands

```python
import paho.mqtt.client as mqtt

client = mqtt.Client()
client.connect("localhost", 1883)

# Send control command
client.publish("motorhand/control/psi", "1.5")
```

### Subscribing to Telemetry

```python
def on_message(client, userdata, msg):
    print(f"{msg.topic}: {msg.payload}")

client = mqtt.Client()
client.on_message = on_message
client.connect("localhost", 1883)
client.subscribe("motorhand/telemetry/#")
client.loop_forever()
```

---

## Error Handling

### Python Exceptions

```python
from lam.temporal_displacement import (
    TemporalDisplacementError,
    CausalityViolation
)

try:
    value = field.get_displaced()
except CausalityViolation:
    # Requested future data
    print("Cannot access future data")
except TemporalDisplacementError as e:
    # General temporal error
    print(f"Error: {e}")
```

### HTTP Status Codes

| Code | Meaning |
|------|---------|
| 200 | Success |
| 400 | Bad request (invalid parameters) |
| 401 | Unauthorized (missing/invalid token) |
| 404 | Resource not found |
| 422 | Validation error |
| 500 | Internal server error |

---

## Code Examples

### Full Python Example

```python
from lam.temporal_displacement import TemporalDisplacedField
from lam.lam_temporal_integration import LAMTemporalController
import asyncio

async def main():
    # Create controller
    controller = LAMTemporalController(
        control_fields=['psi', 'gamma'],
        displacement_sec=1.5
    )

    # Start controller
    await controller.start()

    # Control loop
    for t in range(100):
        # Read sensor (simulated)
        error = 0.01 * (1.0 - controller.get_displaced('psi'))

        # Update control
        controller.update_field('gamma', error)

        # Compute new psi
        psi = controller.get_displaced('psi')
        psi += (-0.16905 * psi + 0.5 * error) * 0.01
        controller.update_field('psi', psi)

        # Sleep
        await asyncio.sleep(0.01)

    # Stop controller
    await controller.stop()

asyncio.run(main())
```

### Full C++ Example

See `MotorHandPro.ino` for complete Arduino example.

---

## Related Documentation

- [User Guide](User-Guide) - Complete tutorials
- [LAM System Guide](LAM-System-Guide) - LAM architecture
- [Hardware Setup](Hardware-Setup) - Arduino integration
- [Deployment Guide](Deployment-Guide) - Production deployment

---

For detailed API documentation, see also:
- `/docs/api/PYTHON_API.md` in repository
- Inline code documentation
- Example scripts in `/lam/` directory
