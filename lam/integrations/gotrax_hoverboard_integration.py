#!/usr/bin/env python3
"""
GoTrax Edge Hoverboard Integration Module
Enables MotorHandPro actuators to control GoTrax Edge hoverboard motors
with Hedera smart contract token burn integration.

1 Token = 1 Second of smooth robotic actuation

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""
import sys
import json
import math
import asyncio
import os
from pathlib import Path
from typing import Dict, Any, List, Optional, Callable
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Add parent paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

# Import Primal Logic constants
try:
    from extras.primal.primal_constants import (
        DONTE_CONSTANT, KERNEL_MU, I3, S_RATIO
    )
    CONSTANTS_AVAILABLE = True
except ImportError:
    # Fallback values
    DONTE_CONSTANT = 149.9992314000
    KERNEL_MU = 0.16905
    I3 = 6.4939394023
    S_RATIO = 23.0983417165
    CONSTANTS_AVAILABLE = False


class HoverboardMotorType(Enum):
    """GoTrax Edge motor types"""
    LEFT_HUB = "left_hub"
    RIGHT_HUB = "right_hub"


class ActuationMode(Enum):
    """Actuation modes for hoverboard control"""
    FORWARD = "forward"
    REVERSE = "reverse"
    TURN_LEFT = "turn_left"
    TURN_RIGHT = "turn_right"
    SPIN = "spin"
    STOP = "stop"


@dataclass
class HoverboardMotorSpec:
    """GoTrax Edge Motor Specifications (reverse engineered)"""
    # Motor electrical specifications
    voltage_nominal: float = 36.0  # V (nominal battery voltage)
    current_max: float = 15.0  # A (max continuous current)
    power_rating: float = 250.0  # W per motor (350W models exist)

    # Mechanical specifications
    wheel_diameter_mm: float = 165.1  # 6.5 inch wheels
    max_rpm: float = 250.0  # Approximate max RPM
    torque_nm: float = 2.5  # Approximate torque in Nm

    # Control parameters (reverse engineered PWM control)
    pwm_frequency_hz: float = 20000.0  # 20kHz PWM
    min_duty_cycle: float = 0.0
    max_duty_cycle: float = 1.0
    deadband: float = 0.05  # 5% deadband for safety


@dataclass
class TokenBurnConfig:
    """Hedera Smart Contract Token Burn Configuration"""
    contract_address: str = ""  # Hedera smart contract address
    token_rate: float = 1.0  # 1 token = 1 second of actuation
    min_tokens_required: float = 0.1  # Minimum tokens for any operation
    burn_callback: Optional[Callable] = None  # Callback for token burn events


@dataclass
class ActuationRequest:
    """Request for hoverboard actuation"""
    mode: ActuationMode
    duration_seconds: float
    power_level: float  # 0.0 to 1.0
    tokens_allocated: float
    timestamp: str = field(default_factory=lambda: datetime.now().isoformat())
    request_id: str = ""

    def __post_init__(self):
        if not self.request_id:
            self.request_id = f"ACT_{datetime.now().strftime('%Y%m%d%H%M%S%f')}"


@dataclass
class ActuationResult:
    """Result of hoverboard actuation"""
    request_id: str
    success: bool
    tokens_burned: float
    actuation_duration_actual: float
    smoothness_score: float  # 0.0 to 1.0 (1.0 = perfectly smooth)
    primal_logic_metrics: Dict[str, Any]
    error_message: Optional[str] = None


class GoTraxHoverboardController:
    """
    Main controller for GoTrax Edge hoverboard integration.
    Uses Primal Logic control framework for smooth actuation.
    """

    def __init__(self, motor_spec: HoverboardMotorSpec = None,
                 token_config: TokenBurnConfig = None):
        """Initialize hoverboard controller"""
        self.motor_spec = motor_spec or HoverboardMotorSpec()
        self.token_config = token_config or TokenBurnConfig()

        # Primal Logic parameters
        self.lightfoot_constant = KERNEL_MU  # λ = 0.16905
        self.donte_attractor = DONTE_CONSTANT  # D = 149.9992314

        # Motor state
        self.left_motor_state = {
            "duty_cycle": 0.0,
            "direction": 1,
            "rpm": 0.0,
            "current": 0.0
        }
        self.right_motor_state = {
            "duty_cycle": 0.0,
            "direction": 1,
            "rpm": 0.0,
            "current": 0.0
        }

        # Token accounting
        self.token_balance = 0.0
        self.tokens_burned_total = 0.0
        self.actuation_time_total = 0.0

        # History
        self.actuation_history: List[ActuationResult] = []

        # Lipschitz constant for stability guarantee
        self.lipschitz_constant = self._compute_lipschitz()

        print(f"GoTrax Hoverboard Controller initialized")
        print(f"  Lightfoot constant (λ): {self.lightfoot_constant}")
        print(f"  Donte attractor (D): {self.donte_attractor}")
        print(f"  Lipschitz bound: {self.lipschitz_constant:.9f}")
        print(f"  Token rate: {self.token_config.token_rate} token/second")

    def _compute_lipschitz(self) -> float:
        """
        Compute Lipschitz constant for stability verification.
        F'(D) = c·μ·exp(-μ·D) where c = (150-D)·exp(μ·D)
        """
        D = self.donte_attractor
        mu = self.lightfoot_constant
        c = (150.0 - D) * math.exp(mu * D)
        lipschitz = c * mu * math.exp(-mu * D)
        return lipschitz

    def set_token_balance(self, tokens: float) -> Dict[str, Any]:
        """Set available token balance for actuation"""
        self.token_balance = max(0.0, tokens)
        return {
            "success": True,
            "token_balance": self.token_balance,
            "max_actuation_time": self.token_balance / self.token_config.token_rate
        }

    def deposit_tokens(self, tokens: float) -> Dict[str, Any]:
        """Deposit tokens for actuation"""
        if tokens <= 0:
            return {"success": False, "error": "Token amount must be positive"}

        self.token_balance += tokens
        return {
            "success": True,
            "tokens_deposited": tokens,
            "new_balance": self.token_balance,
            "max_actuation_time": self.token_balance / self.token_config.token_rate
        }

    def _calculate_tokens_required(self, duration_seconds: float) -> float:
        """Calculate tokens required for specified duration"""
        return duration_seconds * self.token_config.token_rate

    def _burn_tokens(self, amount: float) -> bool:
        """
        Burn tokens for actuation.
        Calls Hedera smart contract callback if configured.
        """
        if amount > self.token_balance:
            return False

        self.token_balance -= amount
        self.tokens_burned_total += amount

        # Call smart contract callback if configured
        if self.token_config.burn_callback:
            try:
                self.token_config.burn_callback(amount)
            except Exception as e:
                print(f"Token burn callback error: {e}")

        return True

    def _compute_smooth_trajectory(self, duration: float, power_level: float,
                                   mode: ActuationMode) -> List[Dict[str, float]]:
        """
        Compute smooth motor trajectory using Primal Logic exponential memory weighting.
        Returns trajectory points at 20ms intervals.
        """
        trajectory = []
        dt = 0.02  # 20ms intervals (50Hz control rate)
        num_points = int(duration / dt)

        for i in range(num_points):
            t = i * dt

            # Apply exponential memory weighting for smooth start/stop
            # S-curve profile using logistic function modulated by Lightfoot constant
            time_constant = 1.0 / self.lightfoot_constant  # ~5.92 seconds

            # Smooth acceleration phase
            if t < duration * 0.1:
                # Acceleration phase - exponential ramp-up
                phase_progress = t / (duration * 0.1)
                scale = 1.0 - math.exp(-self.lightfoot_constant * phase_progress * time_constant)
            elif t > duration * 0.9:
                # Deceleration phase - exponential ramp-down
                phase_progress = (t - duration * 0.9) / (duration * 0.1)
                scale = math.exp(-self.lightfoot_constant * phase_progress * time_constant)
            else:
                # Cruise phase - constant output
                scale = 1.0

            # Apply Donte attractor for stability
            stability_factor = min(1.0, self.donte_attractor / 150.0)
            duty_cycle = power_level * scale * stability_factor

            # Ensure duty cycle is within bounds
            duty_cycle = max(self.motor_spec.min_duty_cycle,
                             min(self.motor_spec.max_duty_cycle, duty_cycle))

            # Apply deadband
            if duty_cycle < self.motor_spec.deadband:
                duty_cycle = 0.0

            # Calculate motor directions based on mode
            left_dir = 1
            right_dir = 1
            left_duty = duty_cycle
            right_duty = duty_cycle

            if mode == ActuationMode.REVERSE:
                left_dir = -1
                right_dir = -1
            elif mode == ActuationMode.TURN_LEFT:
                left_duty = duty_cycle * 0.5
                right_duty = duty_cycle
            elif mode == ActuationMode.TURN_RIGHT:
                left_duty = duty_cycle
                right_duty = duty_cycle * 0.5
            elif mode == ActuationMode.SPIN:
                right_dir = -1
            elif mode == ActuationMode.STOP:
                left_duty = 0.0
                right_duty = 0.0

            trajectory.append({
                "time": t,
                "left_duty": left_duty,
                "left_direction": left_dir,
                "right_duty": right_duty,
                "right_direction": right_dir,
                "smoothness": scale
            })

        return trajectory

    def _compute_smoothness_score(self, trajectory: List[Dict[str, float]]) -> float:
        """
        Compute smoothness score for trajectory (0.0 to 1.0).
        Based on jerk (rate of change of acceleration) minimization.
        """
        if len(trajectory) < 3:
            return 1.0

        # Calculate acceleration (second derivative of position)
        accelerations = []
        for i in range(1, len(trajectory) - 1):
            prev_duty = trajectory[i - 1]["left_duty"]
            curr_duty = trajectory[i]["left_duty"]
            next_duty = trajectory[i + 1]["left_duty"]

            dt = trajectory[i]["time"] - trajectory[i - 1]["time"]
            if dt > 0:
                accel = (next_duty - 2 * curr_duty + prev_duty) / (dt * dt)
                accelerations.append(abs(accel))

        if not accelerations:
            return 1.0

        # Lower average jerk = higher smoothness
        avg_accel = sum(accelerations) / len(accelerations)
        max_accel = max(accelerations)

        # Normalize to 0-1 scale (lower acceleration = smoother)
        smoothness = math.exp(-avg_accel * 0.1)
        return min(1.0, max(0.0, smoothness))

    async def execute_actuation(self, request: ActuationRequest) -> ActuationResult:
        """
        Execute hoverboard actuation with token burn.
        1 token = 1 second of perfectly smooth robotic actuation.
        """
        # Calculate required tokens
        tokens_required = self._calculate_tokens_required(request.duration_seconds)

        # Check if sufficient tokens
        if tokens_required > self.token_balance:
            return ActuationResult(
                request_id=request.request_id,
                success=False,
                tokens_burned=0.0,
                actuation_duration_actual=0.0,
                smoothness_score=0.0,
                primal_logic_metrics={},
                error_message=f"Insufficient tokens. Required: {tokens_required:.2f}, Available: {self.token_balance:.2f}"
            )

        # Check minimum token requirement
        if tokens_required < self.token_config.min_tokens_required:
            return ActuationResult(
                request_id=request.request_id,
                success=False,
                tokens_burned=0.0,
                actuation_duration_actual=0.0,
                smoothness_score=0.0,
                primal_logic_metrics={},
                error_message=f"Below minimum token requirement: {self.token_config.min_tokens_required}"
            )

        # Compute smooth trajectory
        trajectory = self._compute_smooth_trajectory(
            request.duration_seconds,
            request.power_level,
            request.mode
        )

        # Execute trajectory (simulation - in production, send to hardware)
        start_time = datetime.now()

        for point in trajectory:
            # Update motor states
            self.left_motor_state["duty_cycle"] = point["left_duty"]
            self.left_motor_state["direction"] = point["left_direction"]
            self.right_motor_state["duty_cycle"] = point["right_duty"]
            self.right_motor_state["direction"] = point["right_direction"]

            # Simulate motor RPM from duty cycle
            self.left_motor_state["rpm"] = point["left_duty"] * self.motor_spec.max_rpm
            self.right_motor_state["rpm"] = point["right_duty"] * self.motor_spec.max_rpm

            # Small delay to simulate real-time execution (skip every 10th point for performance)
            if len(trajectory) > 100 and (trajectory.index(point) % 10 == 0):
                await asyncio.sleep(0.001)
            elif len(trajectory) <= 100:
                await asyncio.sleep(0.001)  # 1ms simulation step for short trajectories

        end_time = datetime.now()
        actual_duration = (end_time - start_time).total_seconds()

        # Burn tokens
        tokens_to_burn = request.duration_seconds * self.token_config.token_rate
        burn_success = self._burn_tokens(tokens_to_burn)

        # Compute smoothness score
        smoothness = self._compute_smoothness_score(trajectory)

        # Stop motors
        self.left_motor_state["duty_cycle"] = 0.0
        self.right_motor_state["duty_cycle"] = 0.0
        self.left_motor_state["rpm"] = 0.0
        self.right_motor_state["rpm"] = 0.0

        # Update totals
        self.actuation_time_total += request.duration_seconds

        # Primal Logic metrics
        primal_metrics = {
            "lightfoot_constant": self.lightfoot_constant,
            "donte_attractor": self.donte_attractor,
            "lipschitz_bound": self.lipschitz_constant,
            "stability_guaranteed": self.lipschitz_constant < 1.0,
            "time_constant_seconds": 1.0 / self.lightfoot_constant,
            "trajectory_points": len(trajectory)
        }

        result = ActuationResult(
            request_id=request.request_id,
            success=burn_success,
            tokens_burned=tokens_to_burn if burn_success else 0.0,
            actuation_duration_actual=request.duration_seconds,
            smoothness_score=smoothness,
            primal_logic_metrics=primal_metrics,
            error_message=None if burn_success else "Token burn failed"
        )

        self.actuation_history.append(result)
        return result

    def get_motor_state(self) -> Dict[str, Any]:
        """Get current motor states"""
        return {
            "left_motor": self.left_motor_state.copy(),
            "right_motor": self.right_motor_state.copy(),
            "token_balance": self.token_balance,
            "tokens_burned_total": self.tokens_burned_total,
            "actuation_time_total": self.actuation_time_total
        }

    def get_status(self) -> Dict[str, Any]:
        """Get comprehensive controller status"""
        return {
            "controller": "GoTrax Edge Hoverboard",
            "motor_spec": {
                "voltage": self.motor_spec.voltage_nominal,
                "power_per_motor": self.motor_spec.power_rating,
                "max_rpm": self.motor_spec.max_rpm
            },
            "token_config": {
                "token_rate": self.token_config.token_rate,
                "balance": self.token_balance,
                "total_burned": self.tokens_burned_total
            },
            "primal_logic": {
                "lightfoot_constant": self.lightfoot_constant,
                "donte_attractor": self.donte_attractor,
                "lipschitz_bound": self.lipschitz_constant,
                "stability": "GUARANTEED" if self.lipschitz_constant < 1.0 else "AT RISK"
            },
            "actuation_history_count": len(self.actuation_history)
        }


class HederaSmartContractInterface:
    """
    Interface to Hedera smart contract for token burn operations.
    Implements token burn mechanism: 1 token = 1 second of actuation.
    """

    def __init__(self, contract_address: str = "",
                 network: str = "testnet",
                 operator_id: str = "",
                 operator_key: str = ""):
        """Initialize smart contract interface"""
        # Load from environment if not provided
        self.contract_address = contract_address or os.getenv("HEDERA_CONTRACT_ID", "")
        self.network = network or os.getenv("HEDERA_NETWORK", "testnet")
        self.operator_id = operator_id or os.getenv("HEDERA_OPERATOR_ID", "")
        self.operator_key = operator_key or os.getenv("HEDERA_OPERATOR_KEY", "")
        self.evm_address = os.getenv("HEDERA_EVM_ADDRESS", "")

        self.is_connected = False

        # Mock balance for simulation
        self._mock_balance = 100.0  # Default 100 tokens

    def connect(self) -> bool:
        """Connect to the Hedera network"""
        # In production, this would establish Hedera SDK connection
        # using the operator ID and key to create a client
        print(f"Connecting to Hedera {self.network}...")
        print(f"  Operator ID: {self.operator_id}")
        print(f"  Contract: {self.contract_address}")
        print(f"  EVM Address: {self.evm_address}")
        self.is_connected = True
        return True

    def get_balance(self, wallet_address: str = "") -> float:
        """Get token balance for wallet"""
        # In production, call smart contract balanceOf()
        return self._mock_balance

    def burn_tokens(self, amount: float, actuation_duration: float) -> Dict[str, Any]:
        """
        Burn tokens for actuation.
        1 token = 1 second of perfectly smooth robotic actuation.
        """
        if amount > self._mock_balance:
            return {
                "success": False,
                "error": "Insufficient balance",
                "requested": amount,
                "available": self._mock_balance
            }

        self._mock_balance -= amount

        return {
            "success": True,
            "tokens_burned": amount,
            "actuation_seconds": actuation_duration,
            "remaining_balance": self._mock_balance,
            "transaction_hash": f"0x{'0' * 10}MOCK{'0' * 50}",  # Clearly marked mock hash
            "timestamp": datetime.now().isoformat()
        }

    def get_contract_info(self) -> Dict[str, Any]:
        """Get smart contract information"""
        return {
            "network": self.network,
            "operator_id": self.operator_id,
            "contract_address": self.contract_address or "Not configured",
            "evm_address": self.evm_address or "Not configured",
            "token_name": "Hedera Actuation Token",
            "token_symbol": "HAT",
            "token_rate": "1 HAT = 1 second of actuation",
            "is_connected": self.is_connected
        }


class LAMHoverboardInterface:
    """
    LAM interface for hoverboard control.
    Integrates with PrimalLAM for action execution.
    """

    def __init__(self):
        """Initialize LAM interface"""
        # Load token configuration from environment
        token_config = TokenBurnConfig(
            contract_address=os.getenv("HEDERA_CONTRACT_ID", ""),
            token_rate=float(os.getenv("TOKEN_RATE", "1.0")),
            min_tokens_required=float(os.getenv("MIN_TOKENS_REQUIRED", "0.1"))
        )

        self.controller = GoTraxHoverboardController(token_config=token_config)
        self.smart_contract = HederaSmartContractInterface()

        # Connect to Hedera network
        self.smart_contract.connect()

    async def execute_move(self, mode: str, duration: float,
                           power: float = 0.5) -> Dict[str, Any]:
        """
        Execute a hoverboard movement through LAM.

        Args:
            mode: Movement mode (forward, reverse, turn_left, turn_right, spin, stop)
            duration: Duration in seconds
            power: Power level 0.0-1.0

        Returns:
            Execution result with token burn details
        """
        # Map string mode to enum
        mode_map = {
            "forward": ActuationMode.FORWARD,
            "reverse": ActuationMode.REVERSE,
            "turn_left": ActuationMode.TURN_LEFT,
            "turn_right": ActuationMode.TURN_RIGHT,
            "spin": ActuationMode.SPIN,
            "stop": ActuationMode.STOP
        }

        actuation_mode = mode_map.get(mode.lower(), ActuationMode.STOP)

        # Create actuation request
        request = ActuationRequest(
            mode=actuation_mode,
            duration_seconds=duration,
            power_level=power,
            tokens_allocated=duration * self.controller.token_config.token_rate
        )

        # Execute actuation
        result = await self.controller.execute_actuation(request)

        return {
            "success": result.success,
            "mode": mode,
            "duration_requested": duration,
            "duration_actual": result.actuation_duration_actual,
            "tokens_burned": result.tokens_burned,
            "smoothness_score": result.smoothness_score,
            "primal_logic": result.primal_logic_metrics,
            "error": result.error_message
        }

    def deposit_tokens(self, amount: float) -> Dict[str, Any]:
        """Deposit tokens for actuation"""
        return self.controller.deposit_tokens(amount)

    def get_status(self) -> Dict[str, Any]:
        """Get interface status"""
        return {
            "hoverboard": self.controller.get_status(),
            "smart_contract": self.smart_contract.get_contract_info()
        }


async def main():
    """Test GoTrax hoverboard integration"""
    print("=" * 70)
    print("GOTRAX EDGE HOVERBOARD INTEGRATION TEST")
    print("1 Token = 1 Second of Perfectly Smooth Robotic Actuation")
    print("=" * 70)

    # Initialize interface
    interface = LAMHoverboardInterface()

    # Deposit tokens
    print("\n1. Depositing tokens...")
    deposit_result = interface.deposit_tokens(10.0)
    print(json.dumps(deposit_result, indent=2))

    # Get status
    print("\n2. Getting status...")
    status = interface.get_status()
    print(json.dumps(status, indent=2))

    # Execute forward movement
    print("\n3. Executing forward movement (2 seconds)...")
    result = await interface.execute_move("forward", 2.0, power=0.6)
    print(json.dumps(result, indent=2))

    # Execute turn
    print("\n4. Executing left turn (1.5 seconds)...")
    result = await interface.execute_move("turn_left", 1.5, power=0.4)
    print(json.dumps(result, indent=2))

    # Final status
    print("\n5. Final status...")
    status = interface.get_status()
    print(json.dumps(status, indent=2))

    print("\n" + "=" * 70)
    print("INTEGRATION TEST COMPLETE")
    print("=" * 70)


if __name__ == "__main__":
    asyncio.run(main())
