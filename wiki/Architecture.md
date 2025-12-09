# System Architecture

Comprehensive overview of MotorHandPro's architecture and design principles.

## 🏗️ High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     User Layer                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐      │
│  │  Web Panel   │  │   Node-RED   │  │  Mobile App  │      │
│  │  (Three.js)  │  │   (Visual)   │  │ (React Nat.) │      │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘      │
└─────────┼──────────────────┼──────────────────┼─────────────┘
          │                  │                  │
          └──────────────────┼──────────────────┘
                             │ WebSocket/MQTT/REST
┌─────────────────────────────────────────────────────────────┐
│                 LAM Orchestration Layer                      │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  LAM Core (Python/D)                                 │   │
│  │  - Temporal Displacement Engine                      │   │
│  │  - State Management                                  │   │
│  │  - Multi-agent Coordination                          │   │
│  │  - Protocol Routing (WebSocket, MQTT, REST)          │   │
│  └──────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
                             │
                             │ Control Commands
┌─────────────────────────────────────────────────────────────┐
│                  Primal Logic Kernel                         │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Core Control Algorithms                             │   │
│  │  - Exponential Memory Weighting                      │   │
│  │  - Fixed-Point Iteration                             │   │
│  │  - Stability Analysis                                │   │
│  │  - Lyapunov Metrics                                  │   │
│  └──────────────────────────────────────────────────────┘   │
│  Languages: C++ (embedded), Python, D (high-performance)     │
└─────────────────────────────────────────────────────────────┘
                             │
                             │ Actuator Commands
┌─────────────────────────────────────────────────────────────┐
│                    Hardware Layer                            │
│  ┌────────────┐  ┌────────────┐  ┌────────────────────┐    │
│  │  Arduino   │  │ Raspberry  │  │  Servo Motors /    │    │
│  │  (Control) │  │    Pi      │  │  Robotic Hand      │    │
│  └────────────┘  └────────────┘  └────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

## 🧩 Component Breakdown

### 1. User Interface Layer

#### Web Control Panel (`/control_panel/`)
- **Technology**: HTML5, Three.js, Chart.js
- **Purpose**: Real-time 3D visualization and control
- **Features**:
  - Live parameter adjustment
  - 3D robotic hand visualization
  - Real-time plotting of control signals
  - WebSocket communication

#### Node-RED Integration (`/node-red/`)
- **Technology**: Node-RED visual programming
- **Purpose**: Low-code workflow automation
- **Features**:
  - Visual flow design
  - Custom MotorHandPro nodes
  - MQTT/WebSocket bridges
  - Integration with IoT devices

#### Mobile App (`/mobile/`)
- **Technology**: React Native
- **Purpose**: Mobile control and monitoring
- **Features**:
  - Cross-platform (iOS/Android)
  - Real-time telemetry
  - Remote configuration
  - Push notifications

### 2. LAM Orchestration Layer (`/lam/`)

The **Large Action Model (LAM)** system provides intelligent orchestration and coordination.

#### Core Components

**LAM Core** (`lam/core/`)
- State management
- Event routing
- Session handling
- Multi-agent coordination

**Temporal Displacement Engine** (`lam/temporal_displacement.py`)
- Time-aware control fields
- Causality preservation
- Three displacement methods:
  1. Direct displacement
  2. Buffered displacement
  3. Interpolated displacement

**API Layer** (`lam/api/`)
- FastAPI REST endpoints
- WebSocket handlers
- MQTT bridge
- Authentication/authorization

**Integration Layer** (`lam/integrations/`)
- External system connectors
- Protocol adapters
- Data transformers

#### Communication Protocols

```
┌─────────────┐
│   Clients   │
└─────┬───────┘
      │
      ├─── WebSocket ──→ Real-time bidirectional
      │
      ├─── MQTT ───────→ Pub/Sub messaging
      │
      └─── REST ───────→ Request/Response
             │
      ┌──────▼──────┐
      │  LAM Router │
      └─────────────┘
```

### 3. Primal Logic Kernel

The mathematical core implementing bounded control theory.

#### Mathematical Framework

**Control Equation**:
```
dψ/dt = -λ·ψ(t) + KE·e(t)

where:
  ψ(t) = control command signal
  e(t) = tracking error
  λ = 0.16905 s⁻¹ (Lightfoot constant)
  KE = proportional error gain
```

**Stability Guarantee**:
```
Ec(t) = ∫₀^t ψ(τ)·γ(τ) dτ  [bounded]

Lipschitz condition: F'(D) < 1
```

#### Implementation Variants

| Language | File | Use Case |
|----------|------|----------|
| **C++** | `quant_full.h` | Arduino/embedded |
| **Python** | `extras/primal/kernel_v4.py` | Analysis/simulation |
| **D** | `extras/quant_final/` | High-performance |

#### Key Constants

- **D** (Donte constant): 149.9992314000
  - Fixed-point attractor

- **λ** (Lightfoot constant): 0.16905 s⁻¹
  - Exponential decay rate
  - Time constant: τ = 1/λ ≈ 5.92 seconds

- **I3**: 6.4939394023
  - Normalization constant

- **S** (Scaling ratio): 23.0983417165
  - S = D / I3

### 4. Hardware Layer

#### Arduino Implementation

**Main Sketch**: `MotorHandPro.ino`
```cpp
#include "gen/quant_full.h"

void setup() {
  // Initialize serial
  Serial.begin(115200);

  // Compute Primal Logic constants
  computePrimalConstants();

  // Initialize servos
  initializeHardware();
}

void loop() {
  // Read sensors
  // Apply Primal Logic control
  // Update actuators
}
```

**Runtime Optimization**: `quant_runtime.h`
- Lightweight kernel
- Minimal memory footprint
- Optimized for 8-bit/32-bit MCUs

#### Raspberry Pi Integration

- Bridge between Arduino and network
- MQTT broker
- Data logging
- Web server for control panel

## 🔄 Data Flow

### Control Loop

```
1. Sensor Input
   └→ Hardware reads position/velocity

2. Error Calculation
   └→ e(t) = y_desired - y_actual

3. Primal Logic Computation
   └→ dψ/dt = -λ·ψ(t) + KE·e(t)

4. Temporal Displacement (if LAM active)
   └→ Adjust for time-aware fields

5. Actuator Command
   └→ Send ψ(t) to servos

6. Telemetry
   └→ Log data, update visualizations
```

### Message Flow

```
User Action (Web/Mobile)
  │
  ▼
WebSocket/MQTT Message
  │
  ▼
LAM Router
  │
  ├─→ Temporal Processing
  ├─→ State Updates
  └─→ Multi-agent Coordination
      │
      ▼
Protocol Adapter
  │
  ▼
Hardware Interface (Serial/GPIO)
  │
  ▼
Arduino/Actuators
```

## 🗄️ Data Storage

### Time-Series Data

**Format**: CSV with metadata
```csv
# MU=0.16905 KE=0.00000
# Core: D0=149.9992314000 I3=6.4939394023 S=23.0983417165
# t,psi,gamma,Ec
0.00,1.0071595000,0.0041887679,0.0000000000
0.01,1.0143597383,0.0083891661,0.0000031246
```

**Columns**:
- `t`: Time (seconds)
- `psi`: Control command ψ(t)
- `gamma`: Error signal γ(t)
- `Ec`: Integrated control energy

### Configuration Storage

- **JSON**: LAM configuration
- **YAML**: Kubernetes deployment
- **INI**: Arduino parameters

## 🔐 Security Architecture

### Authentication Flow

```
Client → API Gateway → JWT Validation → LAM Core
```

### Authorization Layers

1. **API Level**: JWT tokens, rate limiting
2. **LAM Level**: Role-based access control
3. **Hardware Level**: Command validation

## 📊 Monitoring & Observability

### Metrics Collection

- **Prometheus**: Time-series metrics
- **Grafana**: Dashboards
- **Custom**: Real-time plotting

### Logging

- **Application Logs**: Python logging framework
- **Hardware Logs**: Serial output
- **System Logs**: Systemd journals

## 🚀 Deployment Architectures

### Development (Local)

```
Laptop
  ├─→ Python LAM (dev mode)
  ├─→ Arduino (USB serial)
  └─→ Web browser (localhost)
```

### Production (Cloud)

```
Load Balancer
  │
  ▼
Kubernetes Cluster
  ├─→ LAM Pods (replicated)
  ├─→ Redis (state)
  ├─→ PostgreSQL (persistence)
  └─→ MQTT Broker
      │
      ▼
Edge Devices (Raspberry Pi)
  └─→ Arduino Controllers
      └─→ Robotic Hardware
```

### Hybrid (Edge + Cloud)

```
Cloud (AWS/Azure)
  └─→ LAM Coordination
      │
      ▼ (MQTT over TLS)
Edge Gateway (Raspberry Pi)
  └─→ Local LAM Instance
      └─→ Arduino
          └─→ Hardware
```

## 🧪 Testing Architecture

### Test Pyramid

```
     ┌────────────┐
     │   E2E      │  Smoke tests, integration
     ├────────────┤
     │ Integration│  LAM + Kernel tests
     ├────────────┤
     │   Unit     │  Component tests
     └────────────┘
```

### Validation Layers

1. **Unit Tests**: Individual functions
2. **Integration Tests**: LAM + Primal Logic
3. **Hardware Tests**: With actual servos
4. **Benchmark Tests**: Performance validation

## 📚 Design Principles

1. **Modularity**: Clear separation of concerns
2. **Extensibility**: Plugin architecture
3. **Performance**: Optimized critical paths
4. **Reliability**: Bounded stability guarantees
5. **Observability**: Comprehensive logging/metrics

## 🔗 Related Documentation

- [Primal Logic Framework](Primal-Logic-Framework) - Mathematical details
- [LAM System Guide](LAM-System-Guide) - Orchestration layer
- [Deployment Guide](Deployment-Guide) - Production setup
- [API Reference](API-Reference) - Programming interfaces

---

**Next**: Explore [Primal Logic Framework](Primal-Logic-Framework) for mathematical foundations.
