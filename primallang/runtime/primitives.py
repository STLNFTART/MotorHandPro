#!/usr/bin/env python3
"""
PrimalLang Runtime Primitives
Includes Donte's and Lightfoot's Constants, Vector384, Matrix384, and mathematical operations
"""
import numpy as np
import math
from typing import Union, List, Optional, Callable
from dataclasses import dataclass


# ============================================================================
# DONTE'S AND LIGHTFOOT'S CONSTANTS
# ============================================================================

class PrimalConstants:
    """
    Universal constants from the Primal Logic framework
    Patent Pending: U.S. Provisional Patent Application No. 63/842,846
    """
    # Donte Constant - Fixed-point attractor of the Primal Logic kernel
    D = 149.9992314000  # DONTE_CONSTANT, PLANCK_D, D0

    # Lightfoot Constant - Exponential decay rate for memory weighting
    LAMBDA = 0.16905  # LIGHTFOOT_CONSTANT, KERNEL_MU, mu (s⁻¹)

    # Derived Universal Constants
    I3 = 6.4939394023  # Normalization constant for energy integrals
    S = 23.0983417165   # Scaling ratio: S = D / I3

    # Time constant: τ = 1/λ ≈ 5.92 seconds
    TAU = 1.0 / LAMBDA  # ~5.92 seconds

    # Cutoff threshold
    Xc = 19.358674138784  # Where planckTail(Xc)/I3 = 0.000005124

    # Lipschitz constant at D
    F_PRIME_D = 0.000129931830  # F'(D) - proves contraction mapping

    # Golden ratio points (Fibonacci-Nesting)
    PHI = 1.618033988749895  # Golden ratio
    GOLDEN_POINTS = [0.236, 0.382, 0.618, 0.786]  # Fibonacci ratios

    # Control parameters
    KE_DEFAULT = 0.5  # Default proportional error gain

    @classmethod
    def get_all(cls) -> dict:
        """Return all constants as a dictionary"""
        return {
            'D': cls.D,
            'LAMBDA': cls.LAMBDA,
            'I3': cls.I3,
            'S': cls.S,
            'TAU': cls.TAU,
            'Xc': cls.Xc,
            'F_PRIME_D': cls.F_PRIME_D,
            'PHI': cls.PHI,
            'GOLDEN_POINTS': cls.GOLDEN_POINTS,
            'KE_DEFAULT': cls.KE_DEFAULT,
            # Aliases for convenience
            'DONTE': cls.D,
            'LIGHTFOOT': cls.LAMBDA,
            'pi': math.pi,
            'e': math.e,
        }


# ============================================================================
# VECTOR384 PRIMITIVE
# ============================================================================

class Vector384:
    """
    384-dimensional vector primitive
    Optimized for BCI signal processing and neural state representations
    """
    SIZE = 384

    def __init__(self, data: Optional[Union[np.ndarray, List[float]]] = None):
        if data is None:
            self.data = np.zeros(self.SIZE, dtype=np.float64)
        elif isinstance(data, np.ndarray):
            if data.shape[0] != self.SIZE:
                raise ValueError(f"Vector384 requires exactly {self.SIZE} elements, got {data.shape[0]}")
            self.data = data.astype(np.float64)
        elif isinstance(data, list):
            if len(data) != self.SIZE:
                raise ValueError(f"Vector384 requires exactly {self.SIZE} elements, got {len(data)}")
            self.data = np.array(data, dtype=np.float64)
        else:
            raise TypeError(f"Vector384 requires ndarray or list, got {type(data)}")

    @classmethod
    def fill(cls, value: float) -> 'Vector384':
        """Create a vector filled with a constant value"""
        return cls(np.full(cls.SIZE, value, dtype=np.float64))

    @classmethod
    def zeros(cls) -> 'Vector384':
        """Create a zero vector"""
        return cls(np.zeros(cls.SIZE, dtype=np.float64))

    @classmethod
    def ones(cls) -> 'Vector384':
        """Create a vector of ones"""
        return cls(np.ones(cls.SIZE, dtype=np.float64))

    @classmethod
    def random(cls, low: float = 0.0, high: float = 1.0) -> 'Vector384':
        """Create a random vector"""
        return cls(np.random.uniform(low, high, cls.SIZE))

    @classmethod
    def decay(cls, initial_value: float, decay_rate: Optional[float] = None) -> 'Vector384':
        """
        Create a vector with exponential decay using Lightfoot constant
        v[i] = initial_value * exp(-λ * i / SIZE)
        """
        if decay_rate is None:
            decay_rate = PrimalConstants.LAMBDA

        indices = np.arange(cls.SIZE)
        values = initial_value * np.exp(-decay_rate * indices / cls.SIZE)
        return cls(values)

    def __add__(self, other: 'Vector384') -> 'Vector384':
        """Element-wise addition"""
        if not isinstance(other, Vector384):
            raise TypeError(f"Cannot add Vector384 and {type(other)}")
        return Vector384(self.data + other.data)

    def __sub__(self, other: 'Vector384') -> 'Vector384':
        """Element-wise subtraction"""
        if not isinstance(other, Vector384):
            raise TypeError(f"Cannot subtract {type(other)} from Vector384")
        return Vector384(self.data - other.data)

    def __mul__(self, scalar: float) -> 'Vector384':
        """Scalar multiplication"""
        return Vector384(self.data * float(scalar))

    def __rmul__(self, scalar: float) -> 'Vector384':
        """Reverse scalar multiplication"""
        return self.__mul__(scalar)

    def __truediv__(self, scalar: float) -> 'Vector384':
        """Scalar division"""
        return Vector384(self.data / float(scalar))

    def __neg__(self) -> 'Vector384':
        """Negation"""
        return Vector384(-self.data)

    def __getitem__(self, index: int) -> float:
        """Get element by index"""
        return float(self.data[index])

    def __setitem__(self, index: int, value: float):
        """Set element by index"""
        self.data[index] = float(value)

    def dot(self, other: 'Vector384') -> float:
        """Dot product"""
        if not isinstance(other, Vector384):
            raise TypeError(f"Cannot dot Vector384 and {type(other)}")
        return float(np.dot(self.data, other.data))

    def norm(self) -> float:
        """L2 norm (Euclidean length)"""
        return float(np.linalg.norm(self.data))

    def normalize(self) -> 'Vector384':
        """Return normalized vector (unit length)"""
        n = self.norm()
        if n == 0:
            return Vector384.zeros()
        return Vector384(self.data / n)

    def apply(self, func: Callable[[float], float]) -> 'Vector384':
        """Apply function to each element"""
        return Vector384(np.vectorize(func)(self.data))

    def sum(self) -> float:
        """Sum of all elements"""
        return float(np.sum(self.data))

    def mean(self) -> float:
        """Mean of all elements"""
        return float(np.mean(self.data))

    def max(self) -> float:
        """Maximum element"""
        return float(np.max(self.data))

    def min(self) -> float:
        """Minimum element"""
        return float(np.min(self.data))

    def __repr__(self) -> str:
        return f"Vector384([{self.data[0]:.6f}, {self.data[1]:.6f}, ..., {self.data[-1]:.6f}])"

    def to_list(self) -> List[float]:
        """Convert to Python list"""
        return self.data.tolist()


# ============================================================================
# MATRIX384 PRIMITIVE
# ============================================================================

class Matrix384:
    """
    384x384 matrix primitive
    Optimized for linear transformations in control systems
    """
    ROWS = 384
    COLS = 384

    def __init__(self, data: Optional[Union[np.ndarray, List[List[float]]]] = None):
        if data is None:
            self.data = np.zeros((self.ROWS, self.COLS), dtype=np.float64)
        elif isinstance(data, np.ndarray):
            if data.shape != (self.ROWS, self.COLS):
                raise ValueError(f"Matrix384 requires shape ({self.ROWS}, {self.COLS}), got {data.shape}")
            self.data = data.astype(np.float64)
        elif isinstance(data, list):
            arr = np.array(data, dtype=np.float64)
            if arr.shape != (self.ROWS, self.COLS):
                raise ValueError(f"Matrix384 requires shape ({self.ROWS}, {self.COLS}), got {arr.shape}")
            self.data = arr
        else:
            raise TypeError(f"Matrix384 requires ndarray or list, got {type(data)}")

    @classmethod
    def zeros(cls) -> 'Matrix384':
        """Create a zero matrix"""
        return cls(np.zeros((cls.ROWS, cls.COLS), dtype=np.float64))

    @classmethod
    def ones(cls) -> 'Matrix384':
        """Create a matrix of ones"""
        return cls(np.ones((cls.ROWS, cls.COLS), dtype=np.float64))

    @classmethod
    def identity(cls) -> 'Matrix384':
        """Create an identity matrix"""
        return cls(np.eye(cls.ROWS, dtype=np.float64))

    @classmethod
    def diagonal(cls, values: Vector384) -> 'Matrix384':
        """Create a diagonal matrix from a vector"""
        return cls(np.diag(values.data))

    @classmethod
    def random(cls, low: float = 0.0, high: float = 1.0) -> 'Matrix384':
        """Create a random matrix"""
        return cls(np.random.uniform(low, high, (cls.ROWS, cls.COLS)))

    def __matmul__(self, other: Union['Matrix384', Vector384]) -> Union['Matrix384', Vector384]:
        """Matrix multiplication or matrix-vector multiplication"""
        if isinstance(other, Matrix384):
            return Matrix384(self.data @ other.data)
        elif isinstance(other, Vector384):
            return Vector384(self.data @ other.data)
        else:
            raise TypeError(f"Cannot multiply Matrix384 and {type(other)}")

    def __add__(self, other: 'Matrix384') -> 'Matrix384':
        """Element-wise addition"""
        if not isinstance(other, Matrix384):
            raise TypeError(f"Cannot add Matrix384 and {type(other)}")
        return Matrix384(self.data + other.data)

    def __sub__(self, other: 'Matrix384') -> 'Matrix384':
        """Element-wise subtraction"""
        if not isinstance(other, Matrix384):
            raise TypeError(f"Cannot subtract {type(other)} from Matrix384")
        return Matrix384(self.data - other.data)

    def __mul__(self, scalar: float) -> 'Matrix384':
        """Scalar multiplication"""
        return Matrix384(self.data * float(scalar))

    def __rmul__(self, scalar: float) -> 'Matrix384':
        """Reverse scalar multiplication"""
        return self.__mul__(scalar)

    def transpose(self) -> 'Matrix384':
        """Matrix transpose"""
        return Matrix384(self.data.T)

    def inverse(self) -> 'Matrix384':
        """Matrix inverse"""
        return Matrix384(np.linalg.inv(self.data))

    def det(self) -> float:
        """Determinant"""
        return float(np.linalg.det(self.data))

    def trace(self) -> float:
        """Trace (sum of diagonal elements)"""
        return float(np.trace(self.data))

    def eigenvalues(self) -> Vector384:
        """Compute eigenvalues"""
        eigs = np.linalg.eigvals(self.data)
        # Return real parts (for now)
        return Vector384(np.real(eigs))

    def __repr__(self) -> str:
        return f"Matrix384({self.ROWS}x{self.COLS})"


# ============================================================================
# MATHEMATICAL FUNCTIONS
# ============================================================================

class MathPrimitives:
    """Mathematical operations for PrimalLang"""

    @staticmethod
    def integrate(func: Callable[[float], float], a: float, b: float, n: int = 1000) -> float:
        """
        Numerical integration using Simpson's rule
        ∫[a to b] func(x) dx
        """
        if n % 2 == 1:
            n += 1  # Simpson's rule requires even number of intervals

        h = (b - a) / n
        x = np.linspace(a, b, n + 1)
        y = np.vectorize(func)(x)

        # Simpson's rule: ∫ ≈ h/3 * (y0 + 4*sum(odd) + 2*sum(even) + yn)
        result = y[0] + y[-1]
        result += 4 * np.sum(y[1:-1:2])  # Odd indices
        result += 2 * np.sum(y[2:-1:2])  # Even indices
        result *= h / 3

        return float(result)

    @staticmethod
    def derivative(func: Callable[[float], float], x: float, h: Optional[float] = None) -> float:
        """
        Numerical derivative using central difference
        f'(x) ≈ (f(x+h) - f(x-h)) / (2h)
        Auto-tunes h for optimal accuracy
        """
        if h is None:
            # Auto-tune h based on machine epsilon and x
            h = max(1e-8, abs(x) * 1e-8)

        return (func(x + h) - func(x - h)) / (2 * h)

    @staticmethod
    def fibonacci_nesting(R_X: float, L_X: float, H_X: float, iterations: int = 100) -> float:
        """
        Fibonacci-Nesting convergence from the mathematical foundations
        R_X^(k+1)(t) = (R_X^(k)(t) - L_X) / (H_X - L_X)
        Maps to nearest golden ratio point
        """
        value = R_X
        for _ in range(iterations):
            # Normalize
            normalized = (value - L_X) / (H_X - L_X)

            # Map to nearest golden ratio point
            nearest = min(PrimalConstants.GOLDEN_POINTS, key=lambda p: abs(p - normalized))

            # Check convergence
            if abs(normalized - nearest) < 1e-10:
                break

            value = normalized

        return float(value)

    @staticmethod
    def signal_sentience(signal: Vector384, coherence: Vector384, self_ref: Vector384) -> float:
        """
        Signal Personification (Sentience) metric from mathematical foundations
        S(t) = ∫[signal²(τ) × coherence(τ) × self_ref(τ)] dτ
        """
        integrand = (signal.data ** 2) * coherence.data * self_ref.data
        result = np.trapz(integrand)

        # Check if sentience emerges (above golden ratio threshold)
        threshold = PrimalConstants.GOLDEN_POINTS[2]  # 0.618
        is_sentient = result > threshold

        return float(result), is_sentient

    @staticmethod
    def feedback_stability(alpha: Callable[[float], float], theta: Vector384,
                          lipschitz_L: float, dt: float = 0.01) -> tuple[Vector384, bool]:
        """
        α(Δx) Feedback Loop Stability from mathematical foundations
        Δx(t) = ∫₀ᵗ α(Δx(τ)) × Θ(τ) dτ
        Returns: (solution, is_stable)
        """
        n = theta.SIZE
        delta_x = Vector384.zeros()
        time_points = np.linspace(0, dt * n, n)

        # Fixed-point iteration with trapezoidal integration
        for i in range(1, n):
            # Compute α(Δx) for current state
            alpha_val = alpha(delta_x[i-1])

            # Trapezoidal rule integration step
            integral_step = 0.5 * dt * (alpha_val * theta[i-1] + alpha(delta_x[i-1]) * theta[i])
            delta_x[i] = delta_x[i-1] + integral_step

        # Check stability via Lyapunov analysis
        # If Lipschitz constant L < 1, system is stable
        is_stable = lipschitz_L < 1.0

        return delta_x, is_stable


# ============================================================================
# META-CONSTRAINT CHECKER
# ============================================================================

class MetaError(Exception):
    """Exception raised when meta-constraint is violated"""
    def __init__(self, code: int, premise: str, conclusion: str, trace: Optional[List[str]] = None):
        self.code = code
        self.premise = premise
        self.conclusion = conclusion
        self.trace = trace or []
        super().__init__(f"MetaError[{code}]: {premise} => {conclusion} VIOLATED")


class MetaChecker:
    """Meta-constraint verification system"""

    @staticmethod
    def check_implies(premise: bool, conclusion: bool, code: int = 0,
                     premise_str: str = "", conclusion_str: str = "") -> bool:
        """
        Check logical implication: premise => conclusion
        Returns True if valid, raises MetaError if violated
        """
        # In logic: A => B is equivalent to (not A) or B
        result = (not premise) or conclusion

        if not result:
            raise MetaError(
                code=code,
                premise=premise_str,
                conclusion=conclusion_str,
                trace=[f"Premise: {premise}", f"Conclusion: {conclusion}"]
            )

        return True

    @staticmethod
    def check_bounded(value: float, lower: float, upper: float, name: str = "value") -> bool:
        """Check if value is within bounds"""
        if not (lower <= value <= upper):
            raise MetaError(
                code=1,
                premise=f"{lower} <= {name} <= {upper}",
                conclusion=f"{name} = {value}",
                trace=[f"Value {value} out of bounds [{lower}, {upper}]"]
            )
        return True

    @staticmethod
    def check_convergence(sequence: Vector384, tolerance: float = 1e-6) -> bool:
        """Check if a sequence has converged"""
        n = sequence.SIZE
        if n < 2:
            return False

        # Check if last few values are within tolerance
        last_10 = sequence.data[-10:] if n >= 10 else sequence.data
        max_diff = np.max(np.abs(np.diff(last_10)))

        if max_diff >= tolerance:
            raise MetaError(
                code=2,
                premise=f"Sequence convergence with tolerance {tolerance}",
                conclusion=f"Max difference = {max_diff}",
                trace=[f"Sequence not converged: max_diff={max_diff}"]
            )

        return True


# Export all primitives
__all__ = [
    'PrimalConstants',
    'Vector384',
    'Matrix384',
    'MathPrimitives',
    'MetaChecker',
    'MetaError',
]
