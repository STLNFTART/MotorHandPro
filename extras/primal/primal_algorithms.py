"""
Primal Logic Algorithms Implementation
Based on Technical Deep Dive by Donte Lightfoot (2025)

Implements six core algorithms:
1. Primal Logic Core Formulation (Temporal Kernel)
2. Temporal Displacement and Fractional Delay
3. Fixed Ground Anchor Stability Field
4. Orbital UAV Synchronization
5. Actuator Logic (Second-Order System)
6. Drone Swarm Coordination (Consensus Algorithm)

Author: Donte (Lightfoot Technology)
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import quad, odeint, solve_ivp
from scipy.interpolate import interp1d
from dataclasses import dataclass
from typing import Tuple, List, Callable, Optional
import os

# Import Primal Logic constants
from primal_constants import KERNEL_MU, DONTE_CONSTANT

# Algorithm-specific constants
LIGHTFOOT_CONSTANT = KERNEL_MU  # λ = 0.16905
DEFAULT_ALPHA = 0.5  # Derivative coupling coefficient
DEFAULT_EPSILON = 1e-6  # Numerical stability constant


@dataclass
class PrimalLogicConfig:
    """Configuration for Primal Logic Core Formulation"""
    lambda_decay: float = LIGHTFOOT_CONSTANT
    alpha: float = DEFAULT_ALPHA  # Derivative coupling
    t_max: float = 10.0
    dt: float = 0.01


@dataclass
class FractionalDelayConfig:
    """Configuration for Fractional Delay Kernel"""
    max_delay: float = 1.0
    kernel_width: float = 0.5
    t_max: float = 10.0
    dt: float = 0.01


@dataclass
class AnchorFieldConfig:
    """Configuration for Ground Anchor Stability Field"""
    k: float = 1.0  # Potential strength
    epsilon: float = DEFAULT_EPSILON  # Singularity regularization
    anchor_position: np.ndarray = None  # p_0


@dataclass
class OrbitalConfig:
    """Configuration for UAV Orbital Synchronization"""
    omega: float = 2.0 * np.pi / 10.0  # Angular velocity (rad/s)
    phi_0: float = 0.0  # Initial phase
    radius: float = 100.0  # Orbital radius (meters)


@dataclass
class ActuatorConfig:
    """Configuration for Second-Order Actuator"""
    m: float = 1.0  # Mass
    c: float = 0.5  # Damping coefficient
    k: float = 2.0  # Spring constant
    natural_freq: float = None  # Computed: sqrt(k/m)
    damping_ratio: float = None  # Computed: c/(2*sqrt(m*k))

    def __post_init__(self):
        self.natural_freq = np.sqrt(self.k / self.m)
        self.damping_ratio = self.c / (2 * np.sqrt(self.m * self.k))


@dataclass
class SwarmConfig:
    """Configuration for Drone Swarm Coordination"""
    n_agents: int = 10
    dim: int = 3  # Spatial dimensions
    communication_radius: float = 50.0  # meters
    dt: float = 0.1


class PrimalLogicKernel:
    """
    Algorithm 1: Primal Logic Core Formulation

    T(t) = ∫₀ᵗ Θ(τ)[e(τ) + α de(τ)/dτ] e^(-λ(t-τ)) dτ

    This is the fundamental temporal integration kernel with:
    - Exponential memory weighting: e^(-λ(t-τ))
    - Error coupling: e(τ)
    - Derivative feedback: α de(τ)/dτ
    - Gating function: Θ(τ)
    """

    def __init__(self, config: PrimalLogicConfig = None):
        self.config = config or PrimalLogicConfig()
        self.lambda_decay = self.config.lambda_decay
        self.alpha = self.config.alpha

    def theta_gate(self, tau: float) -> float:
        """
        Gating function Θ(τ)
        Controls temporal attention - can be constant, sigmoid, or adaptive
        """
        # Simple implementation: constant gate
        # Can be extended to: sigmoid, exponential, or learned gate
        return 1.0

    def error_function(self, tau: float, e_signal: Callable) -> float:
        """Compute error signal e(τ)"""
        return e_signal(tau)

    def error_derivative(self, tau: float, e_signal: Callable, delta: float = 1e-5) -> float:
        """Compute de(τ)/dτ using numerical differentiation"""
        return (e_signal(tau + delta) - e_signal(tau - delta)) / (2 * delta)

    def integrand(self, tau: float, t: float, e_signal: Callable) -> float:
        """
        Core integrand: Θ(τ)[e(τ) + α de(τ)/dτ] e^(-λ(t-τ))
        """
        gate = self.theta_gate(tau)
        error = self.error_function(tau, e_signal)
        error_deriv = self.error_derivative(tau, e_signal)
        exponential_weight = np.exp(-self.lambda_decay * (t - tau))

        return gate * (error + self.alpha * error_deriv) * exponential_weight

    def compute_T(self, t: float, e_signal: Callable) -> float:
        """
        Compute T(t) = ∫₀ᵗ Θ(τ)[e(τ) + α de(τ)/dτ] e^(-λ(t-τ)) dτ
        """
        if t <= 0:
            return 0.0

        result, error = quad(self.integrand, 0, t, args=(t, e_signal))
        return result

    def compute_trajectory(self, t_array: np.ndarray, e_signal: Callable) -> np.ndarray:
        """Compute T(t) over entire time array"""
        T_array = np.zeros_like(t_array)
        for i, t in enumerate(t_array):
            T_array[i] = self.compute_T(t, e_signal)
        return T_array


class FractionalDelayKernel:
    """
    Algorithm 2: Temporal Displacement and Fractional Delay

    x_d(t) = ∫₀ᵗ x(τ) h(t - τ - Δ(τ)) dτ

    Where:
    - x(τ): Input signal
    - h(·): Delay kernel (impulse response)
    - Δ(τ): Time-varying fractional delay function
    """

    def __init__(self, config: FractionalDelayConfig = None):
        self.config = config or FractionalDelayConfig()

    def delay_function(self, tau: float) -> float:
        """
        Time-varying delay Δ(τ)
        Can be constant, linear, or nonlinear
        """
        # Example: sinusoidal varying delay
        max_delay = self.config.max_delay
        return max_delay * 0.5 * (1 + np.sin(2 * np.pi * tau / 5.0))

    def delay_kernel(self, t_delayed: float) -> float:
        """
        Delay kernel h(·)
        Typically a windowed sinc or Gaussian kernel
        """
        # Gaussian delay kernel
        sigma = self.config.kernel_width
        return (1.0 / (np.sqrt(2 * np.pi) * sigma)) * np.exp(-t_delayed**2 / (2 * sigma**2))

    def integrand(self, tau: float, t: float, x_signal: Callable) -> float:
        """
        Integrand: x(τ) h(t - τ - Δ(τ))
        """
        delta_tau = self.delay_function(tau)
        t_delayed = t - tau - delta_tau

        # Only include contributions where kernel is defined
        if t_delayed < 0:
            return 0.0

        return x_signal(tau) * self.delay_kernel(t_delayed)

    def compute_output(self, t: float, x_signal: Callable) -> float:
        """
        Compute x_d(t) = ∫₀ᵗ x(τ) h(t - τ - Δ(τ)) dτ
        """
        if t <= 0:
            return 0.0

        result, error = quad(self.integrand, 0, t, args=(t, x_signal))
        return result

    def compute_trajectory(self, t_array: np.ndarray, x_signal: Callable) -> np.ndarray:
        """Compute x_d(t) over entire time array"""
        xd_array = np.zeros_like(t_array)
        for i, t in enumerate(t_array):
            xd_array[i] = self.compute_output(t, x_signal)
        return xd_array


class GroundAnchorField:
    """
    Algorithm 3: Fixed Ground Anchor Stability Field

    V(p) = k / (||p - p₀|| + ε)

    Provides attractive potential field anchored at p₀
    - k: Potential strength
    - ε: Regularization to prevent singularity
    - p: Current position vector
    - p₀: Anchor position
    """

    def __init__(self, config: AnchorFieldConfig = None):
        self.config = config or AnchorFieldConfig()
        if self.config.anchor_position is None:
            self.config.anchor_position = np.zeros(3)  # Default origin

        self.k = self.config.k
        self.epsilon = self.config.epsilon
        self.p0 = self.config.anchor_position

    def potential(self, p: np.ndarray) -> float:
        """
        Compute potential V(p) = k / (||p - p₀|| + ε)
        """
        distance = np.linalg.norm(p - self.p0)
        return self.k / (distance + self.epsilon)

    def force(self, p: np.ndarray) -> np.ndarray:
        """
        Compute force F(p) = -∇V(p)

        For attractive potential V = k/(||p-p₀||+ε):
        F(p) = -k(p - p₀) / (||p - p₀|| + ε)²

        Force points toward anchor (attractive)
        """
        diff = p - self.p0
        distance = np.linalg.norm(diff)
        denominator = (distance + self.epsilon) ** 2

        return -self.k * diff / denominator

    def compute_potential_field(self, x_range: Tuple[float, float],
                               y_range: Tuple[float, float],
                               resolution: int = 50) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Compute 2D potential field for visualization
        Returns: X, Y, V meshgrids
        """
        x = np.linspace(x_range[0], x_range[1], resolution)
        y = np.linspace(y_range[0], y_range[1], resolution)
        X, Y = np.meshgrid(x, y)

        V = np.zeros_like(X)
        for i in range(resolution):
            for j in range(resolution):
                p = np.array([X[i, j], Y[i, j], 0])
                V[i, j] = self.potential(p)

        return X, Y, V


class OrbitalUAVSync:
    """
    Algorithm 4: Orbital UAV Synchronization

    θ(t) = ωt + φ₀

    Simple but fundamental orbital phase equation for synchronized flight
    - ω: Angular velocity (rad/s)
    - φ₀: Initial phase offset
    - Can be extended to full 3D orbital mechanics
    """

    def __init__(self, config: OrbitalConfig = None):
        self.config = config or OrbitalConfig()
        self.omega = self.config.omega
        self.phi_0 = self.config.phi_0
        self.radius = self.config.radius

    def phase(self, t: float) -> float:
        """
        Compute orbital phase θ(t) = ωt + φ₀
        """
        return self.omega * t + self.phi_0

    def position_2d(self, t: float, center: np.ndarray = None) -> np.ndarray:
        """
        Compute 2D circular orbital position
        x(t) = r cos(θ(t))
        y(t) = r sin(θ(t))
        """
        if center is None:
            center = np.zeros(2)

        theta = self.phase(t)
        x = center[0] + self.radius * np.cos(theta)
        y = center[1] + self.radius * np.sin(theta)

        return np.array([x, y])

    def velocity_2d(self, t: float) -> np.ndarray:
        """
        Compute 2D orbital velocity
        vx(t) = -rω sin(θ(t))
        vy(t) = rω cos(θ(t))
        """
        theta = self.phase(t)
        vx = -self.radius * self.omega * np.sin(theta)
        vy = self.radius * self.omega * np.cos(theta)

        return np.array([vx, vy])

    def compute_trajectory(self, t_array: np.ndarray) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Compute full orbital trajectory
        Returns: positions, velocities, phases
        """
        n = len(t_array)
        positions = np.zeros((n, 2))
        velocities = np.zeros((n, 2))
        phases = np.zeros(n)

        for i, t in enumerate(t_array):
            positions[i] = self.position_2d(t)
            velocities[i] = self.velocity_2d(t)
            phases[i] = self.phase(t)

        return positions, velocities, phases


class SecondOrderActuator:
    """
    Algorithm 5: Actuator Logic (Second-Order System)

    m ẍ + c ẋ + k x = u(t)

    Classic mass-spring-damper system for actuator modeling
    - m: Mass
    - c: Damping coefficient
    - k: Spring constant
    - u(t): Control input (force)

    State-space form:
    ẋ₁ = x₂
    ẋ₂ = (u - c*x₂ - k*x₁) / m
    """

    def __init__(self, config: ActuatorConfig = None):
        self.config = config or ActuatorConfig()
        self.m = self.config.m
        self.c = self.config.c
        self.k = self.config.k
        self.omega_n = self.config.natural_freq
        self.zeta = self.config.damping_ratio

    def dynamics(self, state: np.ndarray, t: float, u_func: Callable) -> np.ndarray:
        """
        State-space dynamics:
        state = [x, ẋ]

        ẋ₁ = x₂
        ẋ₂ = (u(t) - c*x₂ - k*x₁) / m
        """
        x, x_dot = state
        u = u_func(t)

        x_ddot = (u - self.c * x_dot - self.k * x) / self.m

        return np.array([x_dot, x_ddot])

    def simulate(self, t_span: np.ndarray, x0: np.ndarray,
                 u_func: Callable) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Simulate actuator response

        Returns: time, position, velocity
        """
        solution = odeint(self.dynamics, x0, t_span, args=(u_func,))

        position = solution[:, 0]
        velocity = solution[:, 1]

        return t_span, position, velocity

    def step_response(self, t_span: np.ndarray, x0: np.ndarray = None,
                     step_amplitude: float = 1.0) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
        """
        Compute step response
        """
        if x0 is None:
            x0 = np.array([0.0, 0.0])

        u_step = lambda t: step_amplitude
        return self.simulate(t_span, x0, u_step)

    def frequency_response(self, freq_range: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Compute frequency response (Bode plot data)

        H(jω) = 1 / (m(jω)² + c(jω) + k)
        """
        omega = 2 * np.pi * freq_range

        H = 1.0 / (self.k - self.m * omega**2 + 1j * self.c * omega)

        magnitude = np.abs(H)
        phase = np.angle(H, deg=True)

        return magnitude, phase


class SwarmConsensus:
    """
    Algorithm 6: Drone Swarm Coordination (Consensus Algorithm)

    ṗᵢ = -Σⱼ∈N(i) (pᵢ - pⱼ)

    Basic consensus dynamics for multi-agent coordination
    - pᵢ: Position of agent i
    - N(i): Neighbors of agent i within communication radius
    - Agents converge to average position
    """

    def __init__(self, config: SwarmConfig = None):
        self.config = config or SwarmConfig()
        self.n_agents = self.config.n_agents
        self.dim = self.config.dim
        self.comm_radius = self.config.communication_radius

        # Initialize random positions
        self.positions = np.random.randn(self.n_agents, self.dim) * 20.0
        self.velocities = np.zeros((self.n_agents, self.dim))

        # History tracking
        self.position_history = []
        self.connectivity_history = []

    def get_neighbors(self, agent_idx: int) -> List[int]:
        """
        Get neighbors N(i) within communication radius
        """
        neighbors = []
        p_i = self.positions[agent_idx]

        for j in range(self.n_agents):
            if j != agent_idx:
                p_j = self.positions[j]
                distance = np.linalg.norm(p_i - p_j)

                if distance <= self.comm_radius:
                    neighbors.append(j)

        return neighbors

    def consensus_dynamics(self, agent_idx: int) -> np.ndarray:
        """
        Compute ṗᵢ = -Σⱼ∈N(i) (pᵢ - pⱼ)
        """
        neighbors = self.get_neighbors(agent_idx)

        if len(neighbors) == 0:
            return np.zeros(self.dim)

        p_i = self.positions[agent_idx]
        dp_i = np.zeros(self.dim)

        for j in neighbors:
            p_j = self.positions[j]
            dp_i -= (p_i - p_j)

        return dp_i

    def update(self, dt: float):
        """
        Update all agent positions using consensus dynamics
        """
        # Compute all velocities
        new_velocities = np.zeros_like(self.velocities)

        for i in range(self.n_agents):
            new_velocities[i] = self.consensus_dynamics(i)

        # Update positions (Euler integration)
        self.positions += new_velocities * dt
        self.velocities = new_velocities

    def compute_connectivity(self) -> float:
        """
        Compute average connectivity (neighbors per agent)
        """
        total_neighbors = 0
        for i in range(self.n_agents):
            total_neighbors += len(self.get_neighbors(i))

        return total_neighbors / self.n_agents

    def compute_spread(self) -> float:
        """
        Compute swarm spread (average distance from centroid)
        """
        centroid = np.mean(self.positions, axis=0)
        distances = np.linalg.norm(self.positions - centroid, axis=1)
        return np.mean(distances)

    def simulate(self, t_max: float, dt: float = None) -> dict:
        """
        Simulate swarm consensus evolution

        Returns dictionary with:
        - time: time array
        - positions: position history
        - connectivity: connectivity over time
        - spread: swarm spread over time
        """
        if dt is None:
            dt = self.config.dt

        t_array = np.arange(0, t_max, dt)
        n_steps = len(t_array)

        # Storage
        position_history = np.zeros((n_steps, self.n_agents, self.dim))
        connectivity_history = np.zeros(n_steps)
        spread_history = np.zeros(n_steps)

        # Simulate
        for i, t in enumerate(t_array):
            position_history[i] = self.positions.copy()
            connectivity_history[i] = self.compute_connectivity()
            spread_history[i] = self.compute_spread()

            self.update(dt)

        return {
            'time': t_array,
            'positions': position_history,
            'connectivity': connectivity_history,
            'spread': spread_history
        }


# Utility Functions

def create_example_signals():
    """Create example test signals for validation"""

    # Error signal for Primal Logic
    def error_signal(t):
        return np.sin(2 * np.pi * 0.5 * t) * np.exp(-0.1 * t)

    # Input signal for Fractional Delay
    def input_signal(t):
        return np.sin(2 * np.pi * t) + 0.5 * np.sin(2 * np.pi * 2 * t)

    # Control input for Actuator
    def control_input(t):
        if t < 1.0:
            return 0.0
        else:
            return 1.0  # Step input

    return error_signal, input_signal, control_input


if __name__ == "__main__":
    """Quick demonstration of all algorithms"""

    print("="*70)
    print("PRIMAL LOGIC ALGORITHMS DEMONSTRATION")
    print("Based on Technical Deep Dive by Donte Lightfoot (2025)")
    print("="*70)

    # Create test signals
    error_sig, input_sig, control_sig = create_example_signals()

    # Test each algorithm
    print("\n1. Primal Logic Core Formulation")
    print("-" * 50)
    kernel = PrimalLogicKernel()
    t_test = np.linspace(0, 10, 100)
    T_result = kernel.compute_trajectory(t_test, error_sig)
    print(f"   ✓ Computed T(t) for {len(t_test)} time points")
    print(f"   Final value: T({t_test[-1]:.2f}) = {T_result[-1]:.6f}")

    print("\n2. Temporal Displacement and Fractional Delay")
    print("-" * 50)
    delay_kernel = FractionalDelayKernel()
    xd_result = delay_kernel.compute_trajectory(t_test, input_sig)
    print(f"   ✓ Computed x_d(t) for {len(t_test)} time points")
    print(f"   Final value: x_d({t_test[-1]:.2f}) = {xd_result[-1]:.6f}")

    print("\n3. Fixed Ground Anchor Stability Field")
    print("-" * 50)
    anchor = GroundAnchorField()
    test_position = np.array([5.0, 5.0, 2.0])
    potential = anchor.potential(test_position)
    force = anchor.force(test_position)
    print(f"   ✓ Test position: {test_position}")
    print(f"   Potential V(p) = {potential:.6f}")
    print(f"   Force ||F(p)|| = {np.linalg.norm(force):.6f}")

    print("\n4. Orbital UAV Synchronization")
    print("-" * 50)
    orbital = OrbitalUAVSync()
    positions, velocities, phases = orbital.compute_trajectory(t_test)
    print(f"   ✓ Computed orbital trajectory for {len(t_test)} time points")
    print(f"   Final phase: θ({t_test[-1]:.2f}) = {phases[-1]:.6f} rad")
    print(f"   Orbital radius: {orbital.radius:.1f} m")

    print("\n5. Actuator Logic (Second-Order System)")
    print("-" * 50)
    actuator = ActuatorConfig(m=1.0, c=0.5, k=2.0)
    print(f"   Natural frequency: ω_n = {actuator.natural_freq:.3f} rad/s")
    print(f"   Damping ratio: ζ = {actuator.damping_ratio:.3f}")
    print(f"   System type: ", end="")
    if actuator.damping_ratio < 1.0:
        print("Underdamped")
    elif actuator.damping_ratio == 1.0:
        print("Critically damped")
    else:
        print("Overdamped")

    print("\n6. Drone Swarm Coordination")
    print("-" * 50)
    swarm = SwarmConsensus(SwarmConfig(n_agents=10, communication_radius=30.0))
    initial_spread = swarm.compute_spread()
    initial_connectivity = swarm.compute_connectivity()
    print(f"   Number of agents: {swarm.n_agents}")
    print(f"   Initial spread: {initial_spread:.3f} m")
    print(f"   Initial connectivity: {initial_connectivity:.3f} neighbors/agent")

    # Run brief simulation
    results = swarm.simulate(t_max=5.0, dt=0.1)
    final_spread = results['spread'][-1]
    final_connectivity = results['connectivity'][-1]
    print(f"   Final spread: {final_spread:.3f} m")
    print(f"   Final connectivity: {final_connectivity:.3f} neighbors/agent")
    print(f"   Convergence: {(1 - final_spread/initial_spread)*100:.1f}%")

    print("\n" + "="*70)
    print("ALL ALGORITHMS DEMONSTRATED SUCCESSFULLY")
    print("="*70)
