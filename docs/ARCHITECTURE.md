# MotorHandPro System Architecture

Comprehensive architectural overview of the MotorHandPro Primal Logic control framework.

## Table of Contents

- [System Overview](#system-overview)
- [Core Components](#core-components)
- [Architectural Layers](#architectural-layers)
- [Data Flow](#data-flow)
- [Integration Architecture](#integration-architecture)
- [Deployment Models](#deployment-models)
- [Technology Stack](#technology-stack)
- [Design Principles](#design-principles)
- [Scalability](#scalability)
- [Security Architecture](#security-architecture)

## System Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     MotorHandPro System                          │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐  │
│  │  User Layer  │      │   Web UI     │      │   External   │  │
│  │  (Control    │◄────►│  (Control    │◄────►│   Systems    │  │
│  │   Panel)     │      │   Panel)     │      │  (Node-RED,  │  │
│  └──────────────┘      └──────────────┘      │   MQTT, etc) │  │
│         │                      │              └──────────────┘  │
│         ▼                      ▼                      ▼          │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │          LAM Orchestration Layer (Python)               │   │
│  │  - WebSocket Server    - REST API    - MQTT Bridge      │   │
│  └─────────────────────────────────────────────────────────┘   │
│         │                      │                      │          │
│         ▼                      ▼                      ▼          │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐  │
│  │ Primal Logic │      │  Simulation  │      │   Database   │  │
│  │    Kernel    │      │    Engine    │      │  (TimescaleDB│  │
│  │  (Python/C++) │      │   (Python)   │      │   /PostgreSQL)│  │
│  └──────────────┘      └──────────────┘      └──────────────┘  │
│         │                      │                      │          │
│         ▼                      ▼                      ▼          │
│  ┌─────────────────────────────────────────────────────────┐   │
│  │         Hardware Abstraction Layer (HAL)                │   │
│  │  - Serial/USB      - GPIO       - Network              │   │
│  └─────────────────────────────────────────────────────────┘   │
│         │                      │                      │          │
│         ▼                      ▼                      ▼          │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐  │
│  │   Arduino/   │      │  Sensors/    │      │   Actuators  │  │
│  │   Embedded   │      │  Feedback    │      │   (Motors,   │  │
│  │   Hardware   │      │              │      │    Servos)   │  │
│  └──────────────┘      └──────────────┘      └──────────────┘  │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### Component Relationships

```
┌─────────────────────────────────────────────────────────┐
│                    Component Graph                       │
│                                                           │
│                  ┌────────────┐                          │
│                  │Control Panel│                          │
│                  └─────┬──────┘                          │
│                        │                                  │
│                        ▼                                  │
│           ┌────────────────────────┐                     │
│           │   LAM Orchestrator     │                     │
│           │  (lam_orchestrator.py) │                     │
│           └───┬────────────────┬───┘                     │
│               │                │                          │
│      ┌────────▼──────┐    ┌───▼────────┐                │
│      │ Primal Kernel │    │ Assistants │                │
│      │  (kernel_v4)  │    │  (Voice,   │                │
│      └───┬───────────┘    │   GraphQL) │                │
│          │                └────────────┘                 │
│          │                                                │
│    ┌─────▼─────────────────────┐                        │
│    │   Hardware Interface      │                        │
│    │  (Serial, WebSocket, etc) │                        │
│    └───────────────────────────┘                        │
└─────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Primal Logic Kernel

**Location:** `extras/primal/kernel_v4.py`, `quant_full.h`, `gen/`

**Responsibilities:**
- Fixed-point iteration and convergence
- Planck tail series computation
- Lipschitz constant evaluation
- Stability verification

**Interfaces:**
- `compute_D()` - Calculate Donte constant
- `kernel_iterate()` - Fixed-point iteration
- `verify_stability()` - Lipschitz condition check

**Implementation variants:**
- **Python:** `kernel_v4.py` (reference implementation)
- **C++:** `quant_full.h` (embedded/Arduino)
- **D language:** `extras/quant_final/quantum_norm.d` (high-performance)

**Thread safety:** Pure functions, thread-safe

**Performance:** O(n) where n = iterations to convergence (~50 typical)

---

### 2. LAM Orchestration Layer

**Location:** `lam/`, `lam_orchestrator.py`

**Responsibilities:**
- System coordination and state management
- Multi-protocol communication (WebSocket, REST, MQTT)
- Request routing and load balancing
- Authentication and authorization (if configured)

**Key modules:**
- `lam/api/` - REST API endpoints
- `lam/websocket/` - WebSocket server
- `lam/core/` - Core orchestration logic
- `lam/assistants/` - AI assistant integration

**Concurrency model:** Asyncio event loop

**Scaling:** Horizontal (multiple orchestrator instances with load balancer)

---

### 3. Control Panel

**Location:** `control_panel/`

**Responsibilities:**
- Real-time visualization
- User parameter input
- System monitoring and alerts

**Technologies:**
- HTML5 + JavaScript (ES6+)
- Three.js (3D phase portraits)
- Chart.js (time series plots)
- WebSocket client (real-time updates)

**Browser support:** Chrome 89+, Edge 89+, Firefox 88+

---

### 4. Simulation Engine

**Location:** `run_comprehensive_sweep.py`, `biomedical_simulation/`

**Responsibilities:**
- Offline simulation and analysis
- Parameter sweeps and optimization
- Validation against benchmarks

**Modes:**
- Real-time simulation (matches hardware timing)
- Accelerated simulation (for parameter sweeps)
- Batch processing (multiple scenarios)

---

### 5. Data Storage Layer

**Technologies:**
- **TimescaleDB** - Time-series data (sensor readings, control signals)
- **PostgreSQL** - Relational data (configurations, users)
- **CSV files** - Experiment results, validation data

**Schema design:**
- Hypertables for high-frequency sensor data
- B-tree indexes for metadata queries
- Continuous aggregates for dashboards

---

### 6. Hardware Abstraction Layer (HAL)

**Location:** `infrastructure/`, embedded code

**Responsibilities:**
- Platform-independent hardware access
- Serial/USB communication
- GPIO control
- Network interface abstraction

**Supported platforms:**
- Arduino (Uno, Mega, Due)
- Raspberry Pi
- Desktop (simulation mode)

---

## Architectural Layers

### Layer 1: Presentation (UI/UX)

**Components:**
- Control Panel web interface
- Mobile app (experimental, `mobile/LAMApp/`)
- Command-line tools

**Protocols:**
- HTTP/HTTPS (static assets)
- WebSocket (real-time data)
- Server-Sent Events (SSE) for notifications

---

### Layer 2: Application (Business Logic)

**Components:**
- LAM orchestrator
- Assistants (voice, GraphQL)
- Workflow engines

**Patterns:**
- Event-driven architecture
- Pub/Sub for decoupling
- Command pattern for operations

---

### Layer 3: Domain (Core Logic)

**Components:**
- Primal Logic kernel
- Control algorithms
- Physics/mathematical models

**Characteristics:**
- Framework-independent
- Highly testable
- Pure functions where possible

---

### Layer 4: Infrastructure (Platform)

**Components:**
- Database layer
- Message queues (MQTT broker)
- Monitoring (Prometheus, Grafana)

**Operations:**
- Docker/Kubernetes deployment
- CI/CD pipelines
- Logging and telemetry

---

### Layer 5: Hardware (Physical)

**Components:**
- Microcontrollers (Arduino, ARM)
- Sensors (IMU, position encoders)
- Actuators (motors, servos)

---

## Data Flow

### Real-Time Control Loop

```
┌──────────┐                                          ┌──────────┐
│  Sensor  │                                          │ Actuator │
│ Hardware │                                          │ Hardware │
└────┬─────┘                                          └────▲─────┘
     │                                                      │
     │ 1. Read (100 Hz)                      6. Write PWM  │
     ▼                                                      │
┌─────────────────┐                           ┌────────────┴──────┐
│  Serial/HAL     │                           │    Serial/HAL     │
│  Interface      │                           │    Interface      │
└────┬────────────┘                           └───────────────────┘
     │                                                      ▲
     │ 2. Deserialize                           5. Serialize
     ▼                                                      │
┌─────────────────┐                           ┌────────────┴──────┐
│  LAM            │                           │   Control         │
│  Orchestrator   │                           │   Command         │
└────┬────────────┘                           └───────────────────┘
     │                                                      ▲
     │ 3. Route to Kernel                      4. Return ψ(t)
     ▼                                                      │
┌─────────────────────────────────────────────────────────┴──────┐
│              Primal Logic Kernel                                │
│  - Compute: dψ/dt = -λψ + KE·γ                                 │
│  - Update: Ec += ψ·γ·dt                                         │
│  - Verify: F'(D) < 1.0                                          │
└─────────────────────────────────────────────────────────────────┘
```

### Simulation Data Flow

```
┌───────────────┐
│ Configuration │
│  (YAML/JSON)  │
└───────┬───────┘
        │
        ▼
┌───────────────────┐
│ Simulation Engine │
│ - Initial state   │
│ - Time step (dt)  │
│ - Duration        │
└────────┬──────────┘
         │
         ▼
┌──────────────────────┐
│  Primal Logic Kernel │
│  (Pure Python)       │
└────────┬─────────────┘
         │
         ▼
┌──────────────────────┐
│  Results (CSV/JSON)  │
│  - Time series       │
│  - Statistics        │
└────────┬─────────────┘
         │
         ▼
┌──────────────────────┐
│  Visualization       │
│  (matplotlib/plotly) │
└──────────────────────┘
```

## Integration Architecture

### External System Integration

**Supported protocols:**
- **REST API:** HTTP/JSON for system control
- **WebSocket:** Real-time bidirectional communication
- **MQTT:** Pub/Sub for IoT integration
- **Serial:** Direct hardware communication
- **GraphQL:** Flexible query API (experimental)

### Integration Points

1. **Node-RED** (`node-red/`)
   - Visual flow programming
   - MQTT bridge
   - Custom LAM nodes

2. **Regulatory APIs** (`regulatory-api/`)
   - FDA compliance interfaces
   - Safety reporting
   - Audit logging

3. **Hedera/Blockchain** (`integrations/`)
   - Immutable audit trail
   - Token-based access control
   - Distributed consensus

4. **Space Systems** (`integrations/`)
   - Satellite telemetry
   - Radiation monitoring
   - Mission planning

## Deployment Models

### 1. Standalone (Development)

```
Single machine running all components
- Python virtual environment
- Local database (SQLite or PostgreSQL)
- File-based configuration
```

### 2. Docker Compose (Testing/Staging)

```
docker-compose.yml orchestrates:
- LAM orchestrator container
- TimescaleDB container
- MQTT broker (Mosquitto)
- Grafana/Prometheus monitoring
```

### 3. Kubernetes (Production)

```
k8s/ manifests for:
- LAM orchestrator deployment (replicas)
- StatefulSet for database
- Ingress for load balancing
- ConfigMaps and Secrets
- HorizontalPodAutoscaler
```

## Technology Stack

### Backend

| Component | Technology | Version |
|-----------|------------|---------|
| Primary language | Python | 3.10+ |
| Web framework | FastAPI | 0.104+ |
| WebSocket | Uvicorn/Starlette | Latest |
| Database | TimescaleDB | 2.11+ |
| Message broker | Mosquitto (MQTT) | 2.0+ |
| Task queue | Celery (optional) | 5.3+ |

### Frontend

| Component | Technology | Version |
|-----------|------------|---------|
| UI framework | Vanilla JS | ES6+ |
| 3D graphics | Three.js | r160+ |
| Charts | Chart.js | 4.4+ |
| Build tool | None (native ES modules) | - |

### Embedded

| Component | Technology | Version |
|-----------|------------|---------|
| Language | C++ / Arduino | C++11+ |
| Build system | Arduino IDE / PlatformIO | Latest |
| RTOS | FreeRTOS (optional) | 10.4+ |

### Infrastructure

| Component | Technology | Version |
|-----------|------------|---------|
| Container | Docker | 24.0+ |
| Orchestration | Kubernetes | 1.28+ |
| Monitoring | Prometheus + Grafana | Latest |
| CI/CD | GitHub Actions | - |

## Design Principles

### 1. Separation of Concerns

- Mathematical kernel isolated from infrastructure
- Pure functions for core logic
- Clear module boundaries

### 2. Dependency Inversion

- Core depends on abstractions, not implementations
- Hardware abstraction layer
- Pluggable communication protocols

### 3. Open/Closed Principle

- Extensible through configuration
- New control laws via plugins
- Custom assistants without core changes

### 4. Single Responsibility

- Each module has one reason to change
- Kernel: computation only
- Orchestrator: coordination only
- HAL: hardware access only

### 5. DRY (Don't Repeat Yourself)

- Constants defined once (`gen/quant_bridge.h`)
- Shared utilities in modules
- Configuration inheritance

## Scalability

### Horizontal Scaling

**LAM Orchestrator:**
- Stateless design enables replication
- Load balancer distributes requests
- Session affinity via WebSocket routing

**Database:**
- Read replicas for queries
- Write leader with async replication
- Partitioning by time (TimescaleDB hypertables)

### Vertical Scaling

**Computational:**
- Multi-core: Parallel simulations
- GPU: Vectorized kernel operations (future)
- FPGA: Ultra-low-latency control (research)

### Performance Targets

| Metric | Target | Current |
|--------|--------|---------|
| Control loop latency | < 10 ms | ~5 ms |
| WebSocket throughput | 1000 msg/s | 800 msg/s |
| Simulation speed | 100x real-time | 50-150x |
| Database writes | 10k/s | 5k/s |

## Security Architecture

### Authentication

- JWT tokens for API access
- WebSocket authentication handshake
- Hardware: Serial port permissions

### Authorization

- Role-based access control (RBAC)
- Parameter bounds enforcement
- Audit logging for critical operations

### Data Security

- TLS/SSL for network communication
- Encrypted storage for sensitive configs
- Secrets management (Kubernetes Secrets, Vault)

### Safety Boundaries

- Parameter validation (KE, λ ranges)
- Lipschitz condition enforcement
- Emergency stop mechanisms
- Watchdog timers

---

**See also:**
- [Component Details](COMPONENTS.md)
- [Data Flow Diagrams](DATAFLOW.md)
- [Integration Guide](INTEGRATION.md)

---

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
