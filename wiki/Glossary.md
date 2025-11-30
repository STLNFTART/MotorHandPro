# Glossary

Technical terms and definitions used in MotorHandPro.

## A

### Actuator
A mechanical device that converts control signals into physical motion (e.g., servo motors, linear actuators).

## B

### Bounded Convergence
A stability property guaranteeing that a control system will converge to a target state while keeping all signals within finite bounds.

### Buffered Displacement
A temporal displacement method using a circular buffer for O(1) constant-time access to historical values.

## C

### Causality
The principle that effects cannot precede their causes. In temporal displacement, this means only past values (not future) can be accessed.

### Control Energy (Ec)
The Lyapunov-like stability metric defined as:
```
Ec(t) = ∫₀^t ψ(τ)·γ(τ) dτ
```
Bounded Ec guarantees system stability.

### Control Law
The mathematical equation governing actuator commands based on sensor feedback. For MotorHandPro:
```
dψ/dt = -λ·ψ(t) + KE·e(t)
```

## D

### D (Donte Constant)
The fixed-point attractor value (149.9992314000) of the Primal Logic kernel, derived from Planck tail calculations.

### Direct Displacement
A simple temporal displacement method using direct lookup from historical data with O(n) complexity.

### Displacement
See [Temporal Displacement](#temporal-displacement).

## E

### Error Signal (e(t))
The difference between desired and actual system state:
```
e(t) = y_desired(t) - y_actual(t)
```

### Exponential Memory Weighting
The core innovation of Primal Logic: using exponential decay (`e^(-λt)`) to weight historical states, preventing integral windup while maintaining bounded convergence.

## F

### Fixed-Point Attractor
A value toward which a dynamical system naturally converges. For Primal Logic, D = 149.9992314000.

## G

### Gamma (γ)
The error signal or error derivative used in the control law:
```
γ(t) = e(t) or de/dt
```

## I

### I3
A normalization constant (6.4939394023) used in Primal Logic calculations, derived from the Planck tail series integral.

### Integral Windup
An undesirable phenomenon in traditional PID controllers where the integral term accumulates without bound, causing instability. Primal Logic prevents this via exponential decay.

### Interpolated Displacement
A temporal displacement method that uses linear interpolation between historical samples for smooth, high-accuracy value retrieval.

## K

### KE (Error Gain)
The proportional gain applied to the tracking error in the control law. Typical values: 0.0 to 1.0.

## L

### Lambda (λ) / Lightfoot Constant
The exponential decay rate (0.16905 s⁻¹) that governs memory weighting in Primal Logic. Time constant τ = 1/λ ≈ 5.92 seconds.

### LAM (Large Action Model)
The orchestration layer in MotorHandPro that coordinates multiple control agents, manages communication protocols, and provides temporal displacement capabilities.

### Lipschitz Constant
A measure of how much a function can change. For stability, the Lipschitz constant must be < 1:
```
F'(D) = c·μ·exp(-μ·D) ≈ 0.00013 < 1
```

### Lyapunov Function
A mathematical function used to prove system stability. For Primal Logic, the control energy Ec(t) serves as a Lyapunov-like metric.

## M

### Multi-Agent Coordination
The capability of LAM to manage and synchronize multiple independent control agents.

### Mu (μ)
The kernel iteration parameter, numerically equal to λ (0.16905).

## P

### Planck Tail
A mathematical series used in computing Primal Logic constants, derived from quantum mechanics.

### Primal Logic
The control framework implemented by MotorHandPro, using exponential memory weighting to guarantee bounded convergence without integral windup.

### Psi (ψ)
The control command signal sent to actuators. Units vary by application (position, velocity, etc.).

## R

### Real-Time Control
Control systems that must respond to inputs within strict timing constraints. MotorHandPro supports real-time control on embedded systems.

## S

### S (Scaling Ratio)
The fundamental ratio S = D / I3 = 23.0983417165, relating control authority to energy dissipation.

### Servo Motor
A rotary or linear actuator that allows precise control of angular or linear position.

### Stability
The property that a control system returns to equilibrium after disturbances and doesn't exhibit unbounded behavior.

### State Management
The LAM subsystem responsible for maintaining and synchronizing distributed control state across multiple agents.

## T

### Temporal Displacement
A LAM feature providing time-aware control fields that can access historical values with configurable time offsets. Three methods: direct, buffered, interpolated.

### Time Constant (τ)
The characteristic time for exponential decay: τ = 1/λ ≈ 5.92 seconds for MotorHandPro.

### Tracking Error
See [Error Signal](#error-signal-et).

## W

### WebSocket
A communication protocol providing full-duplex communication channels over TCP. Used by LAM for real-time bidirectional data flow.

## X

### Xc (Cutoff Threshold)
The value (≈ 19.36) where the Planck tail series becomes negligible (< 0.000005124).

---

## Mathematical Symbols

| Symbol | Name | Meaning |
|--------|------|---------|
| ψ(t) | Psi | Control command signal |
| γ(t) | Gamma | Error signal or derivative |
| λ | Lambda / Lightfoot | Exponential decay rate (0.16905 s⁻¹) |
| μ | Mu | Kernel iteration parameter |
| D | Donte constant | Fixed-point attractor (149.9992) |
| KE | - | Error gain parameter |
| Ec(t) | - | Control energy functional |
| I3 | - | Normalization constant (6.4939) |
| S | - | Scaling ratio (23.0983) |
| Xc | - | Cutoff threshold (19.36) |
| e(t) | - | Tracking error |
| τ | Tau | Time constant (5.92 s) |

---

## Acronyms

| Acronym | Full Form |
|---------|-----------|
| **API** | Application Programming Interface |
| **CSV** | Comma-Separated Values |
| **GPIO** | General Purpose Input/Output |
| **HTTP** | Hypertext Transfer Protocol |
| **JSON** | JavaScript Object Notation |
| **JWT** | JSON Web Token |
| **K8s** | Kubernetes |
| **LAM** | Large Action Model |
| **MQTT** | Message Queuing Telemetry Transport |
| **PID** | Proportional-Integral-Derivative |
| **PWM** | Pulse Width Modulation |
| **RBAC** | Role-Based Access Control |
| **REST** | Representational State Transfer |
| **TLS** | Transport Layer Security |
| **YAML** | YAML Ain't Markup Language |

---

## Related Documentation

- [Primal Logic Framework](Primal-Logic-Framework) - Mathematical details
- [Control Theory](Control-Theory) - Control concepts
- [Architecture](Architecture) - System design
- [FAQ](FAQ) - Frequently asked questions

---

**Note**: For detailed mathematical definitions and derivations, see [Primal Logic Framework](Primal-Logic-Framework).
