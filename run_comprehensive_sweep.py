#!/usr/bin/env python3
"""
Comprehensive Parameter Sweep for MotorHandPro Control System
Supports high-torque regimes, memory mode comparison, and long-horizon tests
"""
import sys
import argparse
import numpy as np
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent))

from experiments.framework import ParamGrid, run_parameter_sweep


def simulate_control_system(config):
    """
    Simulate MotorHandPro control system with Primal Logic controller

    Args:
        config: dict with keys:
            - torque_max: Maximum torque limit (N·m)
            - steps: Number of simulation steps
            - lambda_val: Lightfoot constant (decay rate)
            - KE: Proportional error gain
            - setpoint_freq: Frequency of setpoint changes (Hz)
            - memory_mode: Memory comparison mode ('short', 'long', 'adaptive')

    Returns:
        (metrics, time_series) tuple
    """
    # Extract config
    torque_max = config['torque_max']
    steps = config['steps']
    lambda_val = config.get('lambda_val', 0.16905)
    KE = config.get('KE', 0.3)
    setpoint_freq = config.get('setpoint_freq', 1.0)
    memory_mode = config.get('memory_mode', 'short')

    dt = 0.001  # 1ms timestep (1kHz control)

    # Actuator parameters
    mass = 0.5  # kg
    friction = 0.1  # N·m·s/rad
    omega_n = 2 * np.pi * 50.0  # 50 Hz bandwidth
    zeta = 0.7  # Damping ratio

    # Initialize state
    position = 0.0
    velocity = 0.0
    psi = 0.0  # Control command
    Ec = 0.0  # Control energy

    # Memory buffer (for memory mode comparison)
    if memory_mode == 'short':
        memory_length = 100  # 100ms history
    elif memory_mode == 'long':
        memory_length = 1000  # 1s history
    else:  # adaptive
        memory_length = 500

    error_history = np.zeros(memory_length)

    # Constants (Primal Logic)
    D = 149.9992314000  # Donte constant
    F_prime_D = 0.000129931830  # Lipschitz constant

    # Time series storage
    time_series = {
        't': [],
        'position': [],
        'setpoint': [],
        'error': [],
        'psi': [],
        'torque': [],
        'Ec': []
    }

    # Metrics accumulators
    total_error = 0.0
    max_error = 0.0
    settling_time = None
    overshoot = 0.0
    total_energy = 0.0
    torque_violations = 0

    # Setpoint profile (square wave for testing)
    setpoint_period = int(1.0 / (setpoint_freq * dt))

    for step in range(steps):
        t = step * dt

        # Generate setpoint
        if (step // setpoint_period) % 2 == 0:
            setpoint = 1.0  # rad
        else:
            setpoint = -1.0  # rad

        # Compute error
        error = setpoint - position

        # Update error history (FIFO)
        error_history = np.roll(error_history, -1)
        error_history[-1] = error

        # Primal Logic control law
        # ψ̇ = -λψ + KE·error
        psi_dot = -lambda_val * psi + KE * error
        psi += psi_dot * dt

        # Control energy (with memory consideration)
        if memory_mode == 'adaptive':
            # Adaptive memory: weight recent errors more
            weights = np.exp(-0.01 * np.arange(memory_length)[::-1])
            memory_contribution = np.sum(weights * np.abs(error_history)) / np.sum(weights)
        else:
            memory_contribution = np.mean(np.abs(error_history))

        Ec = D * np.abs(error) * (1.0 + 0.1 * memory_contribution)

        # Compute torque command (limited by torque_max)
        torque_cmd = psi * Ec
        torque_actual = np.clip(torque_cmd, -torque_max, torque_max)

        if abs(torque_cmd) > torque_max:
            torque_violations += 1

        # Actuator dynamics (second-order system)
        # State-space: [θ̇, θ̈]' = A[θ, θ̇]' + B*u
        accel = -omega_n**2 * position - 2*zeta*omega_n * velocity + torque_actual / mass
        accel -= friction * velocity / mass  # Add friction

        # Euler integration
        velocity += accel * dt
        position += velocity * dt

        # Record metrics
        abs_error = abs(error)
        total_error += abs_error
        max_error = max(max_error, abs_error)
        total_energy += abs(torque_actual * velocity) * dt

        # Check settling (within 2% of setpoint)
        if settling_time is None and abs_error < 0.02 * abs(setpoint):
            settling_time = t

        # Check overshoot
        if abs(position) > abs(setpoint):
            overshoot = max(overshoot, abs(position) - abs(setpoint))

        # Record time series (downsample for storage)
        if step % 10 == 0:  # 100 Hz logging
            time_series['t'].append(t)
            time_series['position'].append(round(position, 6))
            time_series['setpoint'].append(setpoint)
            time_series['error'].append(round(error, 6))
            time_series['psi'].append(round(psi, 6))
            time_series['torque'].append(round(torque_actual, 4))
            time_series['Ec'].append(round(Ec, 4))

    # Calculate final metrics
    avg_error = total_error / steps
    rmse = np.sqrt(np.mean(np.array(time_series['error'])**2))

    # Lipschitz stability check
    if len(time_series['error']) > 1:
        error_diffs = np.diff(time_series['error'])
        time_diffs = np.diff(time_series['t'])
        lipschitz = np.max(np.abs(error_diffs / time_diffs))
    else:
        lipschitz = 0.0

    # Stability: Lipschitz < 1.0, final error < 5%, no oscillations
    final_error = abs(time_series['error'][-1])
    stable = (lipschitz < 1.0) and (final_error < 0.05) and (settling_time is not None)

    metrics = {
        "avg_error": round(avg_error, 6),
        "max_error": round(max_error, 6),
        "rmse": round(rmse, 6),
        "settling_time_s": round(settling_time, 4) if settling_time else None,
        "overshoot_rad": round(overshoot, 6),
        "total_energy_J": round(total_energy, 4),
        "lipschitz": round(lipschitz, 6),
        "torque_violations": torque_violations,
        "torque_violation_rate": round(torque_violations / steps, 4),
        "stable": stable
    }

    return metrics, time_series


def main():
    parser = argparse.ArgumentParser(description='Comprehensive control system parameter sweep')
    parser.add_argument('--torque-max', type=float, nargs='+',
                        help='Torque limits to sweep (N·m)')
    parser.add_argument('--steps', type=int, default=2000,
                        help='Number of simulation steps (default: 2000)')
    parser.add_argument('--memory-compare', action='store_true',
                        help='Compare short/long/adaptive memory modes')
    parser.add_argument('--lambda-sweep', action='store_true',
                        help='Sweep lambda values (Lightfoot constant)')
    parser.add_argument('--KE-sweep', action='store_true',
                        help='Sweep KE values (proportional gain)')
    parser.add_argument('--tag', type=str, default='comprehensive_full_sweep',
                        help='Run tag for output directory')

    args = parser.parse_args()

    # Build parameter grid
    params = {
        'steps': [args.steps]
    }

    # Torque sweep
    if args.torque_max:
        params['torque_max'] = args.torque_max
    else:
        params['torque_max'] = [50.0]  # Default

    # Memory mode comparison
    if args.memory_compare:
        params['memory_mode'] = ['short', 'long', 'adaptive']
        print("\n🧠 Memory Mode Comparison Enabled")
        print("   - short:    100ms history")
        print("   - long:     1s history")
        print("   - adaptive: weighted recent history\n")
    else:
        params['memory_mode'] = ['short']

    # Lambda sweep
    if args.lambda_sweep:
        params['lambda_val'] = [0.10, 0.16905, 0.25, 0.35]
    else:
        params['lambda_val'] = [0.16905]

    # KE sweep
    if args.KE_sweep:
        params['KE'] = [0.2, 0.3, 0.4, 0.5]
    else:
        params['KE'] = [0.3]

    # Setpoint frequency
    params['setpoint_freq'] = [1.0]

    # Create param grid
    grid = ParamGrid(params)

    # Count configurations
    total_configs = 1
    for v in params.values():
        total_configs *= len(v)

    print(f"\n🔬 Comprehensive Parameter Sweep")
    print(f"=" * 60)
    print(f"Steps per config: {args.steps:,}")
    print(f"Total configs:    {total_configs}")
    print(f"Tag:              {args.tag}")
    print(f"\nParameter ranges:")
    for k, v in params.items():
        print(f"  {k:20s}: {v}")
    print("=" * 60)
    print()

    # Run sweep
    output_dir = run_parameter_sweep(
        sim_name="control_system",
        param_grid=grid,
        simulate_fn=simulate_control_system,
        tag=args.tag
    )

    print(f"\n✓ Sweep complete! Results in: {output_dir}")
    print(f"\n  📊 View summary: {output_dir}/summary/summary.csv")
    print(f"  📈 View stats:   {output_dir}/summary/stats.json")
    print(f"  📝 View report:  {output_dir}/REPORT.md")
    print(f"\n  🎨 Generate heatmaps:")
    print(f"     python generate_heatmaps.py --sweep-dir {Path(output_dir).name}")
    print()


if __name__ == "__main__":
    main()
