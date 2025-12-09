"""
Temporal Displacement Framework for LAM

Implements three methods for time-weighted field evaluation:
1. Time-Warp: Retarded/advanced evaluation
2. Memory Kernel: Integration with temporal displacement
3. DDE: Delay differential equation approach

Author: Donte Lightfoot
Date: September 20, 2025
Patent Pending: U.S. Provisional Application No. 63/842,846
"""

import numpy as np
from typing import Callable, Optional, Union
from collections import deque
from dataclasses import dataclass
import warnings


@dataclass
class TemporalDisplacementConfig:
    """Configuration for temporal displacement field."""
    alpha: float = 1.0  # Field strength coefficient
    beta: float = 0.1   # Decay rate (for DDE/leaky integrator)
    kappa: float = 0.1  # Disturbance coupling
    lambda_val: float = 0.16905  # Primal Logic Lightfoot constant
    gamma_smooth: float = 0.0  # Smoothing factor for Delta (0 = no smoothing)
    max_delta_rate: Optional[float] = None  # Maximum rate of change for Delta


class TimeWarpField:
    """
    Method 1: Time-Warp (Retarded/Advanced Evaluation)

    E(t) = α * E0(t - Δ(t)) - λ * D(t)

    - Δ(t) > 0: retarded (reads from past) → causally safe
    - Δ(t) < 0: advanced (peeks into future) → offline only
    - Δ(t) can encode propagation, latency, or decision delays
    """

    def __init__(self, config: TemporalDisplacementConfig, history_length: int = 10000):
        """
        Initialize Time-Warp field.

        Args:
            config: Temporal displacement configuration
            history_length: Maximum history buffer size (in samples)
        """
        self.config = config
        self.history_length = history_length

        # Circular buffer for E0 history
        self.E0_buffer = deque(maxlen=history_length)
        self.time_buffer = deque(maxlen=history_length)

        # Smoothed displacement
        self.Delta_smoothed = 0.0
        self.prev_Delta = 0.0

        # Current field value
        self.E = 0.0

    def push_sample(self, t: float, E0: float):
        """Store E0 sample with timestamp."""
        self.E0_buffer.append(E0)
        self.time_buffer.append(t)

    def get_displaced_value(self, t: float, Delta: float) -> float:
        """
        Retrieve E0 value at displaced time t - Delta.

        Args:
            t: Current time
            Delta: Temporal displacement (seconds)

        Returns:
            E0 value at t - Delta (interpolated if necessary)
        """
        if len(self.time_buffer) == 0:
            return 0.0

        target_time = t - Delta

        # Find surrounding samples for interpolation
        times = np.array(self.time_buffer)

        # Handle edge cases
        if target_time <= times[0]:
            return self.E0_buffer[0]
        if target_time >= times[-1]:
            return self.E0_buffer[-1]

        # Linear interpolation
        idx = np.searchsorted(times, target_time)
        if idx == 0:
            return self.E0_buffer[0]
        if idx >= len(times):
            return self.E0_buffer[-1]

        t0, t1 = times[idx-1], times[idx]
        E0_0, E0_1 = self.E0_buffer[idx-1], self.E0_buffer[idx]

        # Interpolate
        frac = (target_time - t0) / (t1 - t0) if (t1 - t0) > 0 else 0.0
        return E0_0 + frac * (E0_1 - E0_0)

    def update(self, t: float, E0: float, Delta: float, D: float = 0.0) -> float:
        """
        Update field with time-warp evaluation.

        Args:
            t: Current time
            E0: Driver field value at current time
            Delta: Temporal displacement (seconds)
            D: Disturbance value

        Returns:
            Updated field value E(t)
        """
        # Store current sample
        self.push_sample(t, E0)

        # Enforce causality for online systems
        if Delta < 0:
            warnings.warn(f"Negative Delta ({Delta}) detected - only use offline!")

        # Smooth Delta if requested
        if self.config.gamma_smooth > 0:
            self.Delta_smoothed = (
                (1 - self.config.gamma_smooth) * self.Delta_smoothed +
                self.config.gamma_smooth * Delta
            )
            Delta_use = self.Delta_smoothed
        else:
            Delta_use = Delta

        # Rate limiting
        if self.config.max_delta_rate is not None:
            delta_change = abs(Delta_use - self.prev_Delta)
            if delta_change > self.config.max_delta_rate:
                Delta_use = self.prev_Delta + np.sign(Delta_use - self.prev_Delta) * self.config.max_delta_rate

        self.prev_Delta = Delta_use

        # Get displaced E0 value
        E0_displaced = self.get_displaced_value(t, Delta_use)

        # Compute field: E(t) = α * E0(t - Δ) - κ * D(t)
        self.E = self.config.alpha * E0_displaced - self.config.kappa * D

        return self.E


class MemoryKernelField:
    """
    Method 2: Displacement via Memory Kernel (Primal Style)

    E(t) = ∫₀ᵗ K(t - τ - Δ(τ)) [α * E0(τ) - κ * d(τ)] dτ

    where K is the memory kernel (exponential for Primal Logic)
    """

    def __init__(self, config: TemporalDisplacementConfig,
                 kernel_func: Optional[Callable] = None,
                 history_length: int = 10000):
        """
        Initialize Memory Kernel field.

        Args:
            config: Temporal displacement configuration
            kernel_func: Custom kernel function K(s). If None, uses exponential kernel
            history_length: Maximum history buffer size
        """
        self.config = config
        self.history_length = history_length

        # Default: Exponential kernel K(s) = λ * exp(-λ * s)
        if kernel_func is None:
            self.kernel_func = lambda s: (
                self.config.lambda_val * np.exp(-self.config.lambda_val * s)
                if s >= 0 else 0.0
            )
        else:
            self.kernel_func = kernel_func

        # History storage
        self.history = []  # List of (time, E0, d, Delta) tuples

        # Current field value
        self.E = 0.0

    def update(self, t: float, E0: float, Delta: float, d: float = 0.0, dt: float = 0.01) -> float:
        """
        Update field using memory kernel integration.

        Args:
            t: Current time
            E0: Driver field value
            Delta: Temporal displacement
            d: Disturbance value
            dt: Integration time step

        Returns:
            Updated field value E(t)
        """
        # Store current sample
        self.history.append((t, E0, d, Delta))

        # Trim history if too long
        if len(self.history) > self.history_length:
            self.history = self.history[-self.history_length:]

        # Integrate: E(t) = ∫ K(t - τ - Δ(τ)) [α*E0(τ) - κ*d(τ)] dτ
        integral = 0.0

        for tau, E0_tau, d_tau, Delta_tau in self.history:
            # Kernel argument: t - τ - Δ(τ)
            s = t - tau - Delta_tau

            # Causality: K(s) = 0 for s < 0
            if s < 0:
                continue

            # Kernel value
            K_s = self.kernel_func(s)

            # Integrand: K(s) * [α*E0(τ) - κ*d(τ)]
            integrand = K_s * (self.config.alpha * E0_tau - self.config.kappa * d_tau)

            # Accumulate (simple rectangular integration)
            integral += integrand * dt

        self.E = integral
        return self.E

    def compute_unit_area_kernel(self) -> float:
        """
        Verify kernel integrates to 1 (for unit-area normalization).

        Returns:
            Integral of kernel from 0 to infinity
        """
        # For exponential kernel: ∫₀^∞ λ*e^(-λs) ds = 1
        # Numerical verification
        s_vals = np.linspace(0, 10/self.config.lambda_val, 1000)
        K_vals = [self.kernel_func(s) for s in s_vals]
        integral = np.trapz(K_vals, s_vals)
        return integral


class DDEField:
    """
    Method 3: Delay/Transport Dynamics (DDE/PDE View)

    dE/dt = -β*E(t) + α*E0(t - Δ(t)) - κ*d(t)

    Natural representation for transport phenomena with delays.
    """

    def __init__(self, config: TemporalDisplacementConfig, history_length: int = 10000):
        """
        Initialize DDE field.

        Args:
            config: Temporal displacement configuration
            history_length: Maximum history buffer size
        """
        self.config = config
        self.history_length = history_length

        # Circular buffer for E0 history
        self.E0_buffer = deque(maxlen=history_length)
        self.time_buffer = deque(maxlen=history_length)

        # Current field value
        self.E = 0.0

    def push_sample(self, t: float, E0: float):
        """Store E0 sample with timestamp."""
        self.E0_buffer.append(E0)
        self.time_buffer.append(t)

    def get_delayed_value(self, t: float, Delta: float) -> float:
        """
        Retrieve E0(t - Delta) with interpolation.

        Args:
            t: Current time
            Delta: Delay time

        Returns:
            E0 value at t - Delta
        """
        if len(self.time_buffer) == 0:
            return 0.0

        target_time = t - Delta
        times = np.array(self.time_buffer)

        if target_time <= times[0]:
            return self.E0_buffer[0]
        if target_time >= times[-1]:
            return self.E0_buffer[-1]

        # Linear interpolation
        idx = np.searchsorted(times, target_time)
        if idx == 0:
            return self.E0_buffer[0]
        if idx >= len(times):
            return self.E0_buffer[-1]

        t0, t1 = times[idx-1], times[idx]
        E0_0, E0_1 = self.E0_buffer[idx-1], self.E0_buffer[idx]

        frac = (target_time - t0) / (t1 - t0) if (t1 - t0) > 0 else 0.0
        return E0_0 + frac * (E0_1 - E0_0)

    def update(self, t: float, E0: float, Delta: float, d: float = 0.0, dt: float = 0.01) -> float:
        """
        Update field using DDE integration.

        Args:
            t: Current time
            E0: Driver field value at current time
            Delta: Temporal delay
            d: Disturbance value
            dt: Time step

        Returns:
            Updated field value E(t)
        """
        # Store current sample
        self.push_sample(t, E0)

        # Get delayed E0 value
        E0_delayed = self.get_delayed_value(t, Delta)

        # DDE: dE/dt = -β*E + α*E0(t-Δ) - κ*d
        dE_dt = -self.config.beta * self.E + self.config.alpha * E0_delayed - self.config.kappa * d

        # Euler integration
        self.E += dE_dt * dt

        return self.E


class TemporalDisplacedField:
    """
    Unified interface for all three temporal displacement methods.

    Automatically selects and manages the appropriate implementation.
    """

    def __init__(self,
                 method: str = 'timewarp',
                 config: Optional[TemporalDisplacementConfig] = None,
                 **kwargs):
        """
        Initialize temporal displaced field.

        Args:
            method: 'timewarp', 'kernel', or 'dde'
            config: Configuration (uses defaults if None)
            **kwargs: Additional arguments passed to specific implementation
        """
        if config is None:
            config = TemporalDisplacementConfig()

        self.method = method.lower()
        self.config = config

        # Instantiate appropriate implementation
        if self.method == 'timewarp':
            self.impl = TimeWarpField(config, **kwargs)
        elif self.method == 'kernel':
            self.impl = MemoryKernelField(config, **kwargs)
        elif self.method == 'dde':
            self.impl = DDEField(config, **kwargs)
        else:
            raise ValueError(f"Unknown method: {method}. Use 'timewarp', 'kernel', or 'dde'")

    def update(self, t: float, E0: float, Delta: float, d: float = 0.0, dt: float = 0.01) -> float:
        """
        Update field value.

        Args:
            t: Current time
            E0: Driver field value
            Delta: Temporal displacement
            d: Disturbance value
            dt: Time step (for kernel/dde methods)

        Returns:
            Updated field value E(t)
        """
        if self.method == 'timewarp':
            return self.impl.update(t, E0, Delta, d)
        else:
            return self.impl.update(t, E0, Delta, d, dt)

    def get_current_value(self) -> float:
        """Get current field value."""
        return self.impl.E

    def reset(self):
        """Reset field to initial state."""
        self.impl.E = 0.0
        if hasattr(self.impl, 'E0_buffer'):
            self.impl.E0_buffer.clear()
            self.impl.time_buffer.clear()
        if hasattr(self.impl, 'history'):
            self.impl.history.clear()


# Advanced Features

class TrustGatedDisplacement:
    """
    Adaptive displacement based on trust/confidence.

    Δ(t) = Δ₀ + Δ_trust * (1 - confidence(t))

    Low confidence → higher displacement → down-weight old/unreliable signals
    """

    def __init__(self, Delta_0: float = 0.0, Delta_trust: float = 1.0):
        """
        Initialize trust-gated displacement.

        Args:
            Delta_0: Baseline displacement
            Delta_trust: Trust sensitivity factor
        """
        self.Delta_0 = Delta_0
        self.Delta_trust = Delta_trust

    def compute_displacement(self, confidence: float) -> float:
        """
        Compute displacement based on confidence level.

        Args:
            confidence: Confidence value in [0, 1]

        Returns:
            Displacement value
        """
        confidence = np.clip(confidence, 0.0, 1.0)
        return self.Delta_0 + self.Delta_trust * (1.0 - confidence)


class LoadSheddingDisplacement:
    """
    Increase displacement under high load for graceful degradation.

    When system is saturated, defer low-priority effects.
    """

    def __init__(self, Delta_base: float = 0.0, Delta_max: float = 5.0, load_threshold: float = 0.8):
        """
        Initialize load shedding displacement.

        Args:
            Delta_base: Baseline displacement (low load)
            Delta_max: Maximum displacement (high load)
            load_threshold: Load level at which to start increasing Delta
        """
        self.Delta_base = Delta_base
        self.Delta_max = Delta_max
        self.load_threshold = load_threshold

    def compute_displacement(self, load: float) -> float:
        """
        Compute displacement based on system load.

        Args:
            load: System load in [0, 1]

        Returns:
            Displacement value
        """
        load = np.clip(load, 0.0, 1.0)

        if load < self.load_threshold:
            return self.Delta_base

        # Linear ramp from Delta_base to Delta_max
        excess_load = (load - self.load_threshold) / (1.0 - self.load_threshold)
        return self.Delta_base + excess_load * (self.Delta_max - self.Delta_base)


# Utility functions

def validate_causality(Delta_history: np.ndarray, times: np.ndarray) -> bool:
    """
    Validate that displacement maintains causality.

    Args:
        Delta_history: Array of displacement values
        times: Corresponding time values

    Returns:
        True if causal (all Delta >= 0 for online systems)
    """
    return np.all(Delta_history >= 0)


def verify_stability(field: TemporalDisplacedField,
                     duration: float = 100.0,
                     dt: float = 0.01,
                     E0_func: Optional[Callable] = None) -> dict:
    """
    Verify stability of temporal displaced field.

    Args:
        field: Temporal displaced field instance
        duration: Test duration
        dt: Time step
        E0_func: Driver function E0(t). If None, uses step input.

    Returns:
        Dictionary with stability metrics
    """
    if E0_func is None:
        E0_func = lambda t: 1.0 if t > 1.0 else 0.0

    times = np.arange(0, duration, dt)
    E_values = []

    field.reset()

    for t in times:
        E0 = E0_func(t)
        Delta = 0.1  # Fixed displacement for test
        d = 0.0

        E = field.update(t, E0, Delta, d, dt)
        E_values.append(E)

    E_values = np.array(E_values)

    # Stability metrics
    is_bounded = np.all(np.abs(E_values) < 1e6)
    converges = np.std(E_values[-100:]) < 0.01  # Last 100 samples stable
    final_value = E_values[-1]

    return {
        'bounded': is_bounded,
        'converges': converges,
        'final_value': final_value,
        'max_value': np.max(np.abs(E_values)),
        'E_history': E_values,
        'time': times
    }
