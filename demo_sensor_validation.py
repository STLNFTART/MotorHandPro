#!/usr/bin/env python3
"""
Sensor Data Integration Demo

Demonstrates Primal Logic validation against synthetic sensor data
without requiring multi-GB dataset downloads.

This demo:
1. Generates realistic synthetic IMU data with known λ = 0.115
2. Validates empirical λ extraction from step responses
3. Checks Lipschitz stability with sensor noise
4. Compares theoretical vs. empirical constants

Patent: U.S. Provisional 63/842,846 (July 12, 2025)
"""

import numpy as np
import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
import matplotlib.pyplot as plt
from pathlib import Path

from sensor_data_integration import (
    SensorData,
    validate_primal_logic,
    extract_gravity_vector,
    detect_step_responses,
    fit_exponential_decay,
)


# ============================================================================
# Synthetic Data Generator
# ============================================================================

def generate_realistic_flight_data(
    duration: float = 60.0,
    sample_rate: float = 200.0,
    lambda_true: float = 0.115,
) -> SensorData:
    """
    Generate realistic quadcopter flight data with step maneuvers

    Simulates:
    - Gravity vector (9.81 m/s² downward)
    - Control maneuvers with exponential settling (λ decay)
    - Sensor noise (IMU quantization, bias drift)
    - Wind disturbances (random walk)

    Args:
        duration: Flight duration (seconds)
        sample_rate: IMU sampling rate (Hz)
        lambda_true: True Lightfoot constant for control decay

    Returns:
        SensorData with realistic telemetry
    """
    dt = 1.0 / sample_rate
    t = np.arange(0, duration, dt)
    N = len(t)

    print(f"Generating {duration}s of synthetic IMU data @ {sample_rate} Hz...")
    print(f"  Samples: {N:,}")
    print(f"  True λ (Lightfoot constant): {lambda_true:.6f} s⁻¹")

    # Gravity vector (body frame, z-down)
    gravity = np.array([0, 0, 9.81])
    accel = np.tile(gravity, (N, 1))
    gyro = np.zeros((N, 3))

    # Generate step response maneuvers (simulating position hold corrections)
    maneuver_times = [5, 12, 20, 28, 35, 42, 50]
    maneuver_axes = [0, 1, 0, 1, 0, 1, 0]  # Alternate x/y
    maneuver_mags = [2.5, -3.0, 1.8, -2.2, 3.5, -1.5, 2.0]  # m/s²

    print(f"\nSimulating {len(maneuver_times)} control maneuvers with λ = {lambda_true}...")

    for t_man, axis, mag in zip(maneuver_times, maneuver_axes, maneuver_mags):
        idx_start = int(t_man * sample_rate)
        if idx_start >= N:
            continue

        # Exponential decay: a(t) = mag * exp(-λ * (t - t_man))
        t_decay = t[idx_start:] - t_man
        decay_profile = mag * np.exp(-lambda_true * t_decay)

        accel[idx_start:, axis] += decay_profile

        # Gyroscope response (angular rate proportional to linear accel)
        gyro_mag = mag * 0.1  # rad/s (scaled)
        gyro_profile = gyro_mag * np.exp(-lambda_true * t_decay)
        gyro[idx_start:, axis] += gyro_profile

    # Add sensor noise
    # IMU noise model: white noise + slow drift
    accel_noise = np.random.normal(0, 0.02, accel.shape)  # 20 mg RMS
    gyro_noise = np.random.normal(0, 0.001, gyro.shape)   # 1 mrad/s RMS

    # Bias drift (random walk, ~0.01 m/s² over 60s)
    bias_drift = np.cumsum(np.random.normal(0, 0.0002, (N, 3)), axis=0)

    accel += accel_noise + bias_drift
    gyro += gyro_noise

    # Add wind disturbance (low-frequency, ~0.2 Hz sine wave)
    wind_freq = 0.2  # Hz
    wind_mag = 0.5   # m/s²
    wind_disturbance = wind_mag * np.sin(2 * np.pi * wind_freq * t)
    accel[:, 0] += wind_disturbance  # Wind along x-axis

    metadata = {
        'dataset': 'Synthetic Quadcopter Flight',
        'sensor': 'Simulated MPU6050 IMU',
        'rate_hz': sample_rate,
        'duration_s': duration,
        'num_samples': N,
        'lambda_true': lambda_true,
        'num_maneuvers': len(maneuver_times),
    }

    print("✓ Synthetic data generation complete")

    return SensorData(
        timestamp=t,
        imu_accel=accel,
        imu_gyro=gyro,
        metadata=metadata
    )


# ============================================================================
# Visualization
# ============================================================================

def plot_validation_results(sensor_data: SensorData, output_dir: Path):
    """
    Generate validation plots

    Args:
        sensor_data: Sensor telemetry
        output_dir: Output directory for plots
    """
    output_dir.mkdir(exist_ok=True)
    print(f"\nGenerating validation plots...")

    # Extract components
    t = sensor_data.timestamp
    accel = sensor_data.imu_accel
    gyro = sensor_data.imu_gyro

    # Gravity-compensated acceleration
    gravity = extract_gravity_vector(accel)
    accel_dynamic = accel - gravity

    # ---------- Plot 1: IMU Time Series ----------
    fig, axes = plt.subplots(3, 1, figsize=(12, 8))

    # Acceleration
    axes[0].plot(t, accel[:, 0], 'r-', alpha=0.7, linewidth=0.5, label='ax')
    axes[0].plot(t, accel[:, 1], 'g-', alpha=0.7, linewidth=0.5, label='ay')
    axes[0].plot(t, accel[:, 2], 'b-', alpha=0.7, linewidth=0.5, label='az')
    axes[0].set_ylabel('Accel (m/s²)')
    axes[0].legend(loc='upper right')
    axes[0].grid(True, alpha=0.3)
    axes[0].set_title('Synthetic IMU Data - Acceleration')

    # Gyroscope
    axes[1].plot(t, gyro[:, 0], 'r-', alpha=0.7, linewidth=0.5, label='ωx')
    axes[1].plot(t, gyro[:, 1], 'g-', alpha=0.7, linewidth=0.5, label='ωy')
    axes[1].plot(t, gyro[:, 2], 'b-', alpha=0.7, linewidth=0.5, label='ωz')
    axes[1].set_ylabel('Gyro (rad/s)')
    axes[1].legend(loc='upper right')
    axes[1].grid(True, alpha=0.3)
    axes[1].set_title('Angular Velocity')

    # Dynamic acceleration (gravity removed)
    axes[2].plot(t, accel_dynamic[:, 0], 'r-', alpha=0.7, linewidth=0.5, label='ax (dynamic)')
    axes[2].plot(t, accel_dynamic[:, 1], 'g-', alpha=0.7, linewidth=0.5, label='ay (dynamic)')
    axes[2].set_ylabel('Dynamic Accel (m/s²)')
    axes[2].set_xlabel('Time (s)')
    axes[2].legend(loc='upper right')
    axes[2].grid(True, alpha=0.3)
    axes[2].set_title('Gravity-Compensated Acceleration (Control Maneuvers)')

    plt.tight_layout()
    plot_file = output_dir / 'imu_time_series.png'
    plt.savefig(plot_file, dpi=150)
    print(f"  ✓ Saved: {plot_file}")
    plt.close()

    # ---------- Plot 2: Step Response Fitting ----------
    # Detect steps
    steps = detect_step_responses(accel, t, threshold=0.5)
    print(f"\n  Detected {len(steps)} step responses")

    if steps:
        fig, axes = plt.subplots(2, 2, figsize=(12, 8))
        axes = axes.flatten()

        lambda_true = sensor_data.metadata.get('lambda_true', 0.115)

        for i, (start, end) in enumerate(steps[:4]):  # Plot first 4 steps
            t_step = t[start:end] - t[start]
            accel_mag = np.linalg.norm(accel_dynamic[start:end], axis=1)

            # Normalize for fitting
            accel_norm = (accel_mag - accel_mag.min()) / (accel_mag.max() - accel_mag.min() + 1e-12)

            # Fit exponential
            x0, lam, xss = fit_exponential_decay(t_step, accel_norm)

            # Plot data and fit
            axes[i].plot(t_step, accel_norm, 'b.', markersize=2, alpha=0.5, label='Data')

            if not np.isnan(lam):
                t_fit = np.linspace(0, t_step[-1], 200)
                fit_curve = x0 * np.exp(-lam * t_fit) + xss
                axes[i].plot(t_fit, fit_curve, 'r-', linewidth=2, label=f'Fit: λ={lam:.4f}')

                # True model
                true_curve = x0 * np.exp(-lambda_true * t_fit) + xss
                axes[i].plot(t_fit, true_curve, 'g--', linewidth=1.5, label=f'True: λ={lambda_true:.4f}')

            axes[i].set_xlabel('Time (s)')
            axes[i].set_ylabel('Normalized Accel')
            axes[i].set_title(f'Step Response {i+1} (t={t[start]:.1f}s)')
            axes[i].legend()
            axes[i].grid(True, alpha=0.3)

        plt.tight_layout()
        plot_file = output_dir / 'step_response_fits.png'
        plt.savefig(plot_file, dpi=150)
        print(f"  ✓ Saved: {plot_file}")
        plt.close()


# ============================================================================
# Main Demo
# ============================================================================

def main():
    """Run sensor validation demo"""
    print("=" * 70)
    print("  MotorHandPro - Sensor Data Integration Demo")
    print("=" * 70)
    print("\nThis demo validates Primal Logic using synthetic sensor data")
    print("(No downloads required)\n")

    # Generate synthetic data
    sensor_data = generate_realistic_flight_data(
        duration=60.0,
        sample_rate=200.0,
        lambda_true=0.115  # Lightfoot constant
    )

    # Run validation
    print("\n" + "=" * 70)
    results = validate_primal_logic(sensor_data)
    print("=" * 70)

    # Display results
    print("\n" + "=" * 70)
    print("  Validation Results Summary")
    print("=" * 70)

    lambda_true = sensor_data.metadata['lambda_true']

    print(f"\n{'Parameter':<30} {'Value':<20} {'Status':<10}")
    print("-" * 70)
    print(f"{'Dataset':<30} {results.dataset_name:<20}")
    print(f"{'Samples':<30} {results.num_samples:,} @ {results.sampling_rate} Hz")
    print(f"{'Duration':<30} {sensor_data.metadata['duration_s']:.1f} s")
    print()

    if not np.isnan(results.lambda_empirical):
        error_pct = abs(results.lambda_empirical - lambda_true) / lambda_true * 100
        status = "✓ PASS" if error_pct < 20 else "⚠ WARN"

        print(f"{'λ (Lightfoot constant)':<30}")
        print(f"  {'True value':<28} {lambda_true:.6f} s⁻¹")
        print(f"  {'Empirical (from data)':<28} {results.lambda_empirical:.6f} ± {results.lambda_std:.6f} s⁻¹")
        print(f"  {'Relative error':<28} {error_pct:.1f}% {status}")
    else:
        print(f"{'λ (Lightfoot constant)':<30} {'NaN':<20} ✗ FAIL")
        print(f"  {'Reason':<28} No valid step responses detected")

    print()
    print(f"{'Lipschitz Stability':<30}")
    print(f"  {'Estimate':<28} {results.lipschitz_estimate:.6e}")
    print(f"  {'Bounded (<1.0 for stability)':<28} {results.lipschitz_bounded} {'✓ PASS' if results.lipschitz_bounded else '✗ FAIL'}")

    print()
    print(f"{'Gravity Error (RMS)':<30} {results.rms_error:.4f} m/s² {'✓ PASS' if results.rms_error < 1.0 else '⚠ WARN'}")
    print(f"{'Gravity Error (Max)':<30} {results.max_error:.4f} m/s²")

    # Generate plots
    output_dir = Path("validation_plots")
    plot_validation_results(sensor_data, output_dir)

    # Summary
    print("\n" + "=" * 70)
    print("  Validation Complete!")
    print("=" * 70)
    print(f"\nPlots saved to: {output_dir.resolve()}")
    print("\nKey Findings:")

    if not np.isnan(results.lambda_empirical):
        error_pct = abs(results.lambda_empirical - lambda_true) / lambda_true * 100
        if error_pct < 10:
            print(f"  ✓ Empirical λ within 10% of true value ({error_pct:.1f}%)")
            print("    → Primal Logic decay constant validated")
        elif error_pct < 20:
            print(f"  ⚠ Empirical λ within 20% of true value ({error_pct:.1f}%)")
            print("    → Acceptable for noisy sensor data")
        else:
            print(f"  ⚠ Empirical λ deviates by {error_pct:.1f}%")
            print("    → May need more/cleaner step responses")

    if results.lipschitz_bounded:
        print("  ✓ Lipschitz stability confirmed (bounded state evolution)")
        print("    → System remains stable with sensor noise")
    else:
        print("  ✗ Lipschitz bound violated")

    print("\nNext Steps:")
    print("  1. Run unit tests: python3 test_sensor_integration.py")
    print("  2. Download real datasets: python3 sensor_data_integration.py --download --datasets euroc_mav")
    print("  3. Validate with hardware: Connect real IMU and log telemetry")
    print("  4. Update constants: Use empirical λ in field_coupled_validation.py")
    print("=" * 70)


if __name__ == '__main__':
    main()
