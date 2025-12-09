# LAM System Guide

Complete guide to the Large Action Model (LAM) orchestration layer.

## 📖 What is LAM?

The **Large Action Model (LAM)** is MotorHandPro's intelligent orchestration layer that coordinates multiple control agents, manages communication protocols, and provides advanced temporal displacement capabilities.

### Key Features

- ✅ **Multi-agent Coordination**: Manage multiple control loops simultaneously
- ✅ **Temporal Displacement**: Time-aware control fields
- ✅ **Protocol Agnostic**: WebSocket, MQTT, REST API support
- ✅ **Async Architecture**: High-performance concurrent operation
- ✅ **State Management**: Distributed state synchronization
- ✅ **Event-Driven**: Reactive control flow

## 🏗️ Architecture

```
┌─────────────────────────────────────────┐
│         LAM System Architecture          │
│                                          │
│  ┌────────────────────────────────────┐ │
│  │     LAM Core Engine                │ │
│  │  - State Management                │ │
│  │  - Event Router                    │ │
│  │  - Session Handler                 │ │
│  └────────────┬───────────────────────┘ │
│               │                          │
│  ┌────────────▼───────────────────────┐ │
│  │  Temporal Displacement Engine      │ │
│  │  - Direct Displacement             │ │
│  │  - Buffered Displacement           │ │
│  │  - Interpolated Displacement       │ │
│  └────────────┬───────────────────────┘ │
│               │                          │
│  ┌────────────▼───────────────────────┐ │
│  │     Protocol Layer                 │ │
│  │  - WebSocket Handler               │ │
│  │  - MQTT Bridge                     │ │
│  │  - REST API Endpoints              │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

## 🚀 Getting Started with LAM

### Quick Start

```python
from lam.lam_temporal_integration import LAMTemporalController
import asyncio

async def main():
    # Create LAM controller
    controller = LAMTemporalController(
        control_fields=['psi', 'gamma', 'error'],
        displacement_sec=1.5,
        update_rate_hz=100
    )

    # Start controller
    await controller.start()
    print("LAM controller started")

    # Update control field
    controller.update_field('psi', 1.5)

    # Get displaced value (from 1.5 seconds ago)
    psi_displaced = controller.get_displaced('psi')
    print(f"Displaced psi: {psi_displaced}")

    # Stop controller
    await controller.stop()

# Run
asyncio.run(main())
```

### Running LAM Server

```bash
cd lam
python lam_server.py
```

**Output**:
```
LAM Server starting...
WebSocket server on port 8765
REST API on port 8000
MQTT bridge connected to localhost:1883
```

## 🔧 Core Components

### 1. Temporal Displacement

Time-aware control fields that remember historical values.

#### Creating Temporal Fields

```python
from lam.temporal_displacement import TemporalDisplacedField

# Create field with 2-second displacement
field = TemporalDisplacedField(
    field_name="control_signal",
    displacement_sec=2.0,
    method="interpolated"  # 'direct', 'buffered', or 'interpolated'
)

# Set current value
field.set(value=1.5)

# Get value from 2 seconds ago
past_value = field.get_displaced()
```

#### Displacement Methods

**1. Direct Displacement**
- Simple lookup from history
- O(n) complexity where n = history length
- Best for: Small datasets, prototyping

**2. Buffered Displacement**
- Circular buffer with O(1) access
- Fixed memory footprint
- Best for: Real-time systems, embedded

**3. Interpolated Displacement**
- Linear interpolation between samples
- Smooth values even with sparse data
- Best for: High-accuracy control, analysis

### 2. Multi-Agent Coordination

Manage multiple control agents with synchronized state.

```python
from lam.multi_agent import AgentCoordinator

# Create coordinator
coordinator = AgentCoordinator(
    num_agents=3,
    shared_fields=['target_position', 'formation']
)

# Add agents
agent1 = ControlAgent(agent_id=1, role='leader')
agent2 = ControlAgent(agent_id=2, role='follower')
agent3 = ControlAgent(agent_id=3, role='follower')

coordinator.register(agent1)
coordinator.register(agent2)
coordinator.register(agent3)

# Update shared state
coordinator.update_shared('target_position', [1.0, 2.0, 3.0])

# Each agent receives update automatically
```

### 3. State Management

Distributed state with consistency guarantees.

```python
from lam.state import StateManager

# Create state manager
state_mgr = StateManager(
    backend='redis',  # or 'memory' for local
    consistency='strong'  # or 'eventual'
)

# Set state
await state_mgr.set('psi', 1.5)

# Get state
psi = await state_mgr.get('psi')

# Watch for changes
async def on_change(key, value):
    print(f"{key} changed to {value}")

state_mgr.watch('psi', on_change)
```

## 🌐 Communication Protocols

### WebSocket

**Client Example** (JavaScript):

```javascript
const ws = new WebSocket('ws://localhost:8765');

ws.onopen = () => {
  // Send control command
  ws.send(JSON.stringify({
    type: 'control',
    field: 'psi',
    value: 1.5
  }));
};

ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  console.log('Received:', data);
};
```

**Server Handler** (Python):

```python
from lam.websocket import WebSocketHandler

async def handle_client(websocket, path):
    handler = WebSocketHandler(websocket)

    async for message in websocket:
        # Process message
        response = await handler.process(message)

        # Send response
        await websocket.send(response)
```

### MQTT

**Publisher**:

```python
import paho.mqtt.client as mqtt

client = mqtt.Client()
client.connect("localhost", 1883)

# Publish control command
client.publish("motorhand/control/psi", "1.5")
```

**Subscriber**:

```python
def on_message(client, userdata, msg):
    print(f"{msg.topic}: {msg.payload}")

client = mqtt.Client()
client.on_message = on_message
client.connect("localhost", 1883)
client.subscribe("motorhand/telemetry/#")
client.loop_forever()
```

### REST API

**Endpoints**:

```bash
# Get status
GET http://localhost:8000/api/v1/status

# Send control command
POST http://localhost:8000/api/v1/control/command
{
  "field": "psi",
  "value": 1.5
}

# Get telemetry
GET http://localhost:8000/api/v1/telemetry?field=psi&duration=60
```

## 🎯 Use Cases

### Use Case 1: Robotic Arm Control

```python
# Setup
controller = LAMTemporalController(
    control_fields=['joint1', 'joint2', 'joint3', 'joint4'],
    displacement_sec=0.5,  # 500ms displacement
    update_rate_hz=200      # 200 Hz control
)

# Control loop
async def control_arm():
    while True:
        # Read sensors
        positions = read_joint_sensors()

        # Update fields
        for i, pos in enumerate(positions):
            controller.update_field(f'joint{i+1}', pos)

        # Compute control with temporal awareness
        for i in range(4):
            displaced = controller.get_displaced(f'joint{i+1}')
            error = target[i] - displaced
            control = -0.16905 * displaced + 0.5 * error
            send_to_actuator(i, control)

        await asyncio.sleep(0.005)  # 200 Hz
```

### Use Case 2: Distributed Sensor Fusion

```python
# Multiple sensor agents
sensors = [
    SensorAgent('imu', location='base'),
    SensorAgent('encoder', location='joint1'),
    SensorAgent('force', location='gripper')
]

# Coordinator fuses data
coordinator = AgentCoordinator(num_agents=len(sensors))

for sensor in sensors:
    coordinator.register(sensor)

# Shared state automatically synchronized
fused_state = coordinator.get_fused_state()
```

### Use Case 3: Cloud-Edge Coordination

```python
# Edge controller (Raspberry Pi)
edge = LAMTemporalController(
    control_fields=['local_psi', 'local_gamma'],
    displacement_sec=1.0
)

# Cloud coordinator (AWS)
cloud = LAMTemporalController(
    control_fields=['global_target', 'global_state'],
    displacement_sec=2.0
)

# MQTT bridge connects them
bridge = MQTTBridge(
    local=edge,
    remote=cloud,
    sync_fields=['global_target']
)
```

## 📊 Performance Tuning

### Optimization Tips

**1. Adjust Update Rate**

```python
# Lower rate for less critical fields
controller.set_update_rate('gamma', 50)  # 50 Hz

# Higher rate for critical fields
controller.set_update_rate('psi', 200)   # 200 Hz
```

**2. Clean Old Data**

```python
# Automatically clean data older than 5 minutes
field.clear_old_data(retain_seconds=300)
```

**3. Use Buffered Method**

```python
# For real-time: use buffered (O(1) access)
field = TemporalDisplacedField(
    "control_signal",
    displacement_sec=2.0,
    method="buffered",
    buffer_size=1000  # Fixed size
)
```

**4. Batch Updates**

```python
# Batch multiple updates
updates = {
    'psi': 1.5,
    'gamma': 0.01,
    'error': 0.02
}
controller.update_fields(updates)  # Single operation
```

### Benchmarking

```bash
# Run performance benchmark
cd lam
python benchmark_temporal_displacement.py
```

**Expected Results**:
- Python Direct: ~10,000 ops/sec
- Python Buffered: ~50,000 ops/sec
- Python Interpolated: ~8,000 ops/sec
- D Language: ~500,000+ ops/sec

## 🔒 Security

### Authentication

```python
from lam.auth import JWTAuthenticator

# Create authenticator
auth = JWTAuthenticator(secret_key='your-secret-key')

# Generate token
token = auth.generate_token(user_id='user123', expires=3600)

# Verify token
async def handle_request(request):
    token = request.headers.get('Authorization')
    user = auth.verify_token(token)
    if not user:
        raise Unauthorized()
```

### Authorization

```python
from lam.auth import RBACManager

# Define roles
rbac = RBACManager()
rbac.define_role('operator', permissions=['control.read', 'control.write'])
rbac.define_role('viewer', permissions=['control.read'])

# Check permissions
@rbac.require_permission('control.write')
async def send_command(user, command):
    # Only users with control.write can execute
    pass
```

## 🧪 Testing LAM

### Unit Tests

```python
import pytest
from lam.temporal_displacement import TemporalDisplacedField

def test_temporal_displacement():
    field = TemporalDisplacedField("test", 1.0)
    field.set(1.5, timestamp=0.0)
    field.set(2.0, timestamp=1.0)

    # Query at t=2.0 with 1.0s displacement gets t=1.0 value
    value = field.get_displaced(query_time=2.0)
    assert value == 2.0
```

### Integration Tests

```bash
# Run smoke test
cd lam
python smoke_test.py

# Run integration tests
python -m pytest test_*.py
```

### Load Testing

```bash
# Simulate 100 concurrent clients
cd lam
python load_test.py --clients 100 --duration 60
```

## 📚 Advanced Features

### Custom Protocol Adapter

```python
from lam.protocols import ProtocolAdapter

class MyProtocol(ProtocolAdapter):
    async def connect(self):
        # Custom connection logic
        pass

    async def send(self, data):
        # Custom send logic
        pass

    async def receive(self):
        # Custom receive logic
        pass

# Register with LAM
controller.register_protocol('my_protocol', MyProtocol())
```

### Event Hooks

```python
from lam.events import EventHook

# Define hook
@controller.on_event('field_updated')
async def on_field_updated(field_name, value):
    print(f"{field_name} updated to {value}")

# Trigger events automatically when fields update
controller.update_field('psi', 1.5)  # Triggers hook
```

## 🔗 Related Documentation

- [API Reference](API-Reference) - Complete API docs
- [Temporal Displacement](Temporal-Displacement) - Deep dive into displacement
- [Architecture](Architecture) - System design
- [Integration Examples](Integration-Examples) - Real-world integrations

---

**Next**: Explore [Temporal Displacement](Temporal-Displacement) for advanced temporal control.
