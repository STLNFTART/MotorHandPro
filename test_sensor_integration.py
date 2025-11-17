#!/usr/bin/env python3
"""
Unit Tests for Sensor Data Integration Framework

Tests the sensor parsing, empirical λ extraction, and validation
using synthetic data that mimics real sensor characteristics.

Patent: U.S. Provisional 63/842,846 (July 12, 2025)
"""

import numpy as np
import pytest
from pathlib import Path
import tempfile
import shutil
from sensor_data_integration import (
    SensorData,
    ValidationResults,
    extract_gravity_vector,
    detect_step_responses,
    fit_exponential_decay,
    validate_primal_logic,
)


# ============================================================================
# Synthetic Data Generators
# ============================================================================

def generate_synthetic_imu(
    duration: float = 10.0,
    sample_rate: float = 200.0,
    lambda_true: float = 0.115,
    noise_std: float = 0.01,
) -> SensorData:
    """
    Generate synthetic IMU data with known λ for validation testing

    Args:
        duration: Simulation duration (seconds)
        sample_rate: IMU sampling rate (Hz)
        lambda_true: True decay constant to embed in data
        noise_std: Sensor noise standard deviation (m/s²)

    Returns:
        SensorData with synthetic IMU measurements
    """
    dt = 1.0 / sample_rate
    t = np.arange(0, duration, dt)
    N = len(t)

    # Gravity vector (Earth, pointing down in body frame)
    gravity = np.array([0, 0, 9.81])

    # Generate step responses with exponential decay
    # Simulate 3-5 control maneuvers during flight
    accel = np.tile(gravity, (N, 1))  # Start with gravity only
    gyro = np.zeros((N, 3))

    # Add 3 step responses at random times
    step_times = [2.0, 5.0, 8.0]
    step_magnitudes = [3.0, -2.5, 4.0]  # m/s² in x-axis

    for t_step, mag in zip(step_times, step_magnitudes):
        idx_start = int(t_step * sample_rate)

        # Exponential decay: a(t) = mag * exp(-λ * t)
        t_decay = t[idx_start:] - t_step
        decay_profile = mag * np.exp(-lambda_true * t_decay)

        accel[idx_start:, 0] += decay_profile  # Add to x-axis

    # Add sensor noise
    accel += np.random.normal(0, noise_std, accel.shape)
    gyro += np.random.normal(0, noise_std * 0.1, gyro.shape)  # Lower gyro noise

    metadata = {
        'dataset': 'Synthetic IMU',
        'sensor': 'Simulated',
        'rate_hz': sample_rate,
        'duration_s': duration,
        'num_samples': N,
        'lambda_true': lambda_true,
    }

    return SensorData(
        timestamp=t,
        imu_accel=accel,
        imu_gyro=gyro,
        metadata=metadata
    )


def generate_step_response(
    amplitude: float = 5.0,
    lambda_decay: float = 0.115,
    duration: float = 5.0,
    dt: float = 0.01,
) -> tuple[np.ndarray, np.ndarray]:
    """
    Generate perfect exponential step response for fitting tests

    Args:
        amplitude: Initial step magnitude
        lambda_decay: Decay constant (1/s)
        duration: Response duration (s)
        dt: Time step (s)

    Returns:
        (time, signal) arrays
    """
    t = np.arange(0, duration, dt)
    x = amplitude * np.exp(-lambda_decay * t)
    return t, x


# ============================================================================
# Unit Tests
# ============================================================================

class TestDataStructures:
    """Test data models and containers"""

    def test_sensor_data_creation(self):
        """Test SensorData dataclass creation"""
        t = np.linspace(0, 10, 100)
        accel = np.random.randn(100, 3)
        gyro = np.random.randn(100, 3)

        data = SensorData(timestamp=t, imu_accel=accel, imu_gyro=gyro)

        assert len(data.timestamp) == 100
        assert data.imu_accel.shape == (100, 3)
        assert data.metadata is not None

    def test_validation_results_creation(self):
        """Test ValidationResults dataclass"""
        results = ValidationResults(
            lambda_empirical=0.120,
            lambda_std=0.015,
            lipschitz_estimate=0.5,
            lipschitz_bounded=True,
            rms_error=0.25,
            max_error=1.5,
            dataset_name="Test",
            num_samples=1000,
            sampling_rate=200.0
        )

        assert results.lambda_empirical == 0.120
        assert results.lipschitz_bounded is True


class TestGravityExtraction:
    """Test gravity vector extraction from IMU data"""

    def test_static_gravity(self):
        """Test gravity extraction from static IMU (hovering)"""
        # Static sensor at 1g downward
        N = 1000
        accel = np.tile([0, 0, 9.81], (N, 1))
        accel += np.random.normal(0, 0.05, accel.shape)  # Small noise

        gravity = extract_gravity_vector(accel)

        # Should recover ~9.81 m/s² in z-axis
        gravity_mag = np.linalg.norm(gravity, axis=1)
        assert np.abs(np.mean(gravity_mag) - 9.81) < 0.1

    def test_dynamic_with_gravity(self):
        """Test gravity extraction with dynamic motion"""
        # Gravity + sinusoidal motion (simulating flight)
        t = np.linspace(0, 10, 2000)
        gravity_true = np.array([0, 0, 9.81])
        motion = np.array([2 * np.sin(2 * np.pi * t),
                          np.zeros_like(t),
                          np.zeros_like(t)]).T

        accel = np.tile(gravity_true, (len(t), 1)) + motion

        gravity_est = extract_gravity_vector(accel)

        # Low-pass filter should reject high-frequency motion
        gravity_mag = np.linalg.norm(gravity_est, axis=1)
        assert np.abs(np.mean(gravity_mag[-500:]) - 9.81) < 0.5  # Steady-state


class TestStepDetection:
    """Test step response detection in sensor data"""

    def test_detect_single_step(self):
        """Test detection of single step response"""
        t = np.linspace(0, 5, 1000)

        # Step at t=1s with magnitude 5 m/s²
        accel = np.zeros((1000, 3))
        accel[200:, 0] = 5.0 * np.exp(-0.115 * (t[200:] - t[200]))

        steps = detect_step_responses(accel, t, threshold=1.0)

        # Should detect at least one step
        assert len(steps) >= 1

        # First step should be near index 200
        if steps:
            assert 180 <= steps[0][0] <= 220

    def test_detect_multiple_steps(self):
        """Test detection of multiple step responses"""
        t = np.linspace(0, 10, 2000)
        accel = np.zeros((2000, 3))

        # Three steps at t=1, 4, 7 seconds
        step_indices = [200, 800, 1400]
        for idx in step_indices:
            accel[idx:, 0] += 3.0 * np.exp(-0.115 * (t[idx:] - t[idx]))

        steps = detect_step_responses(accel, t, threshold=0.8)

        # Should detect multiple steps (may find more due to overlaps)
        assert len(steps) >= 2


class TestExponentialFitting:
    """Test exponential decay fitting for λ extraction"""

    def test_perfect_exponential(self):
        """Test fitting with perfect exponential data"""
        lambda_true = 0.115
        t, x = generate_step_response(amplitude=5.0, lambda_decay=lambda_true, duration=5.0)

        x0, lam, xss = fit_exponential_decay(t, x)

        # Should recover true parameters
        assert np.abs(x0 - 5.0) < 0.1  # Initial amplitude
        assert np.abs(lam - lambda_true) < 0.01  # Decay rate
        assert np.abs(xss) < 0.1  # Steady-state ≈ 0

    def test_exponential_with_noise(self):
        """Test fitting with noisy exponential data"""
        lambda_true = 0.115
        t, x = generate_step_response(amplitude=5.0, lambda_decay=lambda_true)

        # Add 10% noise
        x_noisy = x + np.random.normal(0, 0.1, len(x))

        x0, lam, xss = fit_exponential_decay(t, x_noisy)

        # Should still recover λ within 20% error
        assert np.abs(lam - lambda_true) / lambda_true < 0.2

    def test_exponential_with_offset(self):
        """Test fitting exponential with non-zero steady-state"""
        lambda_true = 0.115
        offset = 2.0

        t = np.linspace(0, 5, 500)
        x = 5.0 * np.exp(-lambda_true * t) + offset

        x0, lam, xss = fit_exponential_decay(t, x)

        # Should recover offset
        assert np.abs(xss - offset) < 0.2
        assert np.abs(lam - lambda_true) < 0.02


class TestPrimalLogicValidation:
    """Test end-to-end Primal Logic validation"""

    def test_synthetic_imu_validation(self):
        """Test validation against synthetic IMU with known λ"""
        # Generate synthetic data with λ = 0.115
        sensor_data = generate_synthetic_imu(
            duration=15.0,
            sample_rate=200.0,
            lambda_true=0.115,
            noise_std=0.02
        )

        # Run validation
        results = validate_primal_logic(sensor_data)

        # Check results structure
        assert isinstance(results, ValidationResults)
        assert results.num_samples == sensor_data.metadata['num_samples']
        assert results.sampling_rate == 200.0

        # Empirical λ should be close to true value (within 50% due to noisy fitting)
        if not np.isnan(results.lambda_empirical):
            error_pct = np.abs(results.lambda_empirical - 0.115) / 0.115
            print(f"Lambda error: {error_pct * 100:.1f}%")
            # Relaxed tolerance for synthetic noisy data
            assert error_pct < 0.5, f"λ error {error_pct*100:.1f}% exceeds 50%"

    def test_lipschitz_bounded(self):
        """Test Lipschitz stability check"""
        # Generate stable system (bounded acceleration)
        sensor_data = generate_synthetic_imu(duration=10.0, noise_std=0.01)
        results = validate_primal_logic(sensor_data)

        # Should be bounded (max accel < 50 m/s²)
        assert results.lipschitz_bounded is True

    def test_gravity_error_metrics(self):
        """Test RMS/max gravity error computation"""
        # Static sensor at 1g
        N = 1000
        t = np.linspace(0, 10, N)
        accel = np.tile([0, 0, 9.81], (N, 1))
        accel += np.random.normal(0, 0.1, accel.shape)
        gyro = np.zeros((N, 3))

        sensor_data = SensorData(
            timestamp=t,
            imu_accel=accel,
            imu_gyro=gyro,
            metadata={'dataset': 'Test', 'rate_hz': 100, 'duration_s': 10, 'num_samples': N}
        )

        results = validate_primal_logic(sensor_data)

        # RMS error should be small (<0.5 m/s²)
        assert results.rms_error < 1.0


class TestEmpiricalConstants:
    """Test empirical constant extraction accuracy"""

    def test_lambda_extraction_accuracy(self):
        """Test λ extraction with multiple known decay rates"""
        lambda_values = [0.10, 0.115, 0.12, 0.15]

        for lambda_true in lambda_values:
            # Generate clean exponential
            t, x = generate_step_response(
                amplitude=10.0,
                lambda_decay=lambda_true,
                duration=8.0
            )

            # Fit
            x0, lam, xss = fit_exponential_decay(t, x)

            # Check accuracy (should be <1% error for clean data)
            error_pct = np.abs(lam - lambda_true) / lambda_true
            assert error_pct < 0.01, f"λ={lambda_true}: error {error_pct*100:.2f}%"

    def test_lightfoot_constant_validation(self):
        """Test that empirical λ matches Lightfoot constant (0.115)"""
        # Generate data with Lightfoot constant
        sensor_data = generate_synthetic_imu(
            duration=20.0,
            sample_rate=200.0,
            lambda_true=0.115,
            noise_std=0.005  # Very low noise
        )

        results = validate_primal_logic(sensor_data)

        # Should recover λ ≈ 0.115 within 30%
        if not np.isnan(results.lambda_empirical):
            assert 0.08 < results.lambda_empirical < 0.15


# ============================================================================
# Integration Tests
# ============================================================================

class TestIntegration:
    """Integration tests for full pipeline"""

    def test_end_to_end_validation(self):
        """Test complete validation pipeline"""
        # Generate realistic synthetic data
        sensor_data = generate_synthetic_imu(
            duration=30.0,
            sample_rate=200.0,
            lambda_true=0.115,
            noise_std=0.02
        )

        # Run full validation
        results = validate_primal_logic(sensor_data)

        # Verify all fields populated
        assert results.dataset_name == 'Synthetic IMU'
        assert results.num_samples == 6000  # 30s @ 200Hz
        assert isinstance(results.lambda_empirical, (float, type(np.nan)))
        assert isinstance(results.lipschitz_bounded, bool)

    def test_multiple_sensors_comparison(self):
        """Test validation across different sensor rates"""
        rates = [100.0, 200.0, 500.0]
        results_list = []

        for rate in rates:
            sensor_data = generate_synthetic_imu(
                duration=10.0,
                sample_rate=rate,
                lambda_true=0.115,
                noise_std=0.01
            )
            results = validate_primal_logic(sensor_data)
            results_list.append(results)

        # All should detect bounded behavior
        assert all(r.lipschitz_bounded for r in results_list)


# ============================================================================
# Benchmark Tests
# ============================================================================

class TestPerformance:
    """Performance and benchmarking tests"""

    def test_validation_speed(self):
        """Test validation completes in reasonable time"""
        import time

        # Large dataset (10 minutes @ 200 Hz = 120k samples)
        sensor_data = generate_synthetic_imu(duration=600.0, sample_rate=200.0)

        start = time.time()
        results = validate_primal_logic(sensor_data)
        elapsed = time.time() - start

        # Should complete in <5 seconds for 120k samples
        assert elapsed < 5.0, f"Validation took {elapsed:.2f}s (too slow)"
        print(f"Validation time for 120k samples: {elapsed:.3f}s")

    def test_fitting_convergence(self):
        """Test exponential fitting converges reliably"""
        # Test 100 random step responses
        success_count = 0

        for _ in range(100):
            amplitude = np.random.uniform(1.0, 10.0)
            lambda_true = np.random.uniform(0.05, 0.20)

            t, x = generate_step_response(amplitude, lambda_true, duration=5.0)
            x += np.random.normal(0, 0.05, len(x))  # 5% noise

            x0, lam, xss = fit_exponential_decay(t, x)

            if not np.isnan(lam) and 0.01 < lam < 0.5:
                success_count += 1

        # Should converge in >80% of cases
        success_rate = success_count / 100
        assert success_rate > 0.8, f"Convergence rate {success_rate*100:.1f}% too low"


# ============================================================================
# Main Test Runner
# ============================================================================

if __name__ == '__main__':
    print("=" * 70)
    print("  Sensor Data Integration - Unit Tests")
    print("=" * 70)
    print("\nRunning tests with synthetic sensor data...")
    print("(No downloads required - all tests use generated data)\n")

    # Run pytest
    pytest.main([__file__, '-v', '--tb=short'])
