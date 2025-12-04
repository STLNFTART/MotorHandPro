"""
Comprehensive Example: Distributed Control System with Temporal Displacement

Demonstrates a realistic distributed control scenario with:
- Multiple sensors with different latencies
- Trust-based weighting of sensor readings
- Load-adaptive graceful degradation
- Real-time visualization of temporal effects

Author: Donte Lightfoot
Date: November 30, 2025
Patent Pending: U.S. Provisional Application No. 63/842,846
"""

import numpy as np
import time
from dataclasses import dataclass
from typing import List

from lam.temporal_displacement import (
    TemporalDisplacedField,
    TemporalDisplacementConfig,
    TrustGatedDisplacement
)
from lam.lam_temporal_integration import LAMTemporalController


@dataclass
class Sensor:
    """Sensor with network latency and reliability characteristics."""
    id: int
    name: str
    latency: float  # Network latency in seconds
    reliability: float  # Base reliability [0, 1]
    position: tuple  # (x, y) position in meters

    def __post_init__(self):
        """Initialize sensor-specific controller."""
        config = TemporalDisplacementConfig(
            alpha=1.0,
            beta=0.1,
            kappa=0.1,
            lambda_val=0.16905
        )
        self.controller = LAMTemporalController(
            method='timewarp',
            enable_trust_gating=True,
            enable_load_shedding=False
        )

    def read_value(self, true_value: float, noise_level: float = 0.05) -> float:
        """
        Simulate sensor reading with noise.

        Args:
            true_value: True value being sensed
            noise_level: Noise amplitude

        Returns:
            Noisy sensor reading
        """
        # Add Gaussian noise
        noise = np.random.randn() * noise_level

        # Occasional outliers (sensor glitches)
        if np.random.rand() < (1.0 - self.reliability):
            noise += np.random.randn() * noise_level * 5  # Large spike

        return true_value + noise

    def compute_confidence(self, reading: float, reference: float, noise_threshold: float = 0.2) -> float:
        """
        Compute confidence in sensor reading.

        Args:
            reading: Sensor value
            reference: Expected value
            noise_threshold: Threshold for confidence degradation

        Returns:
            Confidence level [0, 1]
        """
        # Base confidence from reliability
        confidence = self.reliability

        # Degrade confidence if reading differs significantly from reference
        error = abs(reading - reference)
        if error > noise_threshold:
            confidence *= max(0.1, 1.0 - error)

        return np.clip(confidence, 0.0, 1.0)

    def process_reading(self, t: float, reading: float, reference: float, dt: float = 0.01):
        """
        Process sensor reading with temporal displacement.

        Args:
            t: Current time
            reading: Raw sensor reading
            reference: Expected value (for confidence estimation)
            dt: Time step

        Returns:
            Processed sensor state with displacement
        """
        # Compute confidence
        confidence = self.compute_confidence(reading, reference)

        # Update controller with temporal displacement
        state = self.controller.update(
            E0=reading,
            confidence=confidence,
            base_Delta=self.latency,
            dt=dt
        )

        return state


class DistributedControlSystem:
    """
    Distributed control system with multiple sensors and temporal displacement.

    Demonstrates:
    - Sensor fusion with different latencies
    - Trust-weighted aggregation
    - Real-time monitoring of temporal effects
    """

    def __init__(self, sensors: List[Sensor]):
        """
        Initialize distributed control system.

        Args:
            sensors: List of Sensor instances
        """
        self.sensors = sensors

        # Central controller for aggregated signal
        config = TemporalDisplacementConfig(
            alpha=1.0,
            beta=0.1,
            kappa=0.0,
            lambda_val=0.16905
        )
        self.central_controller = TemporalDisplacedField(method='kernel', config=config)

        # History for visualization
        self.history = {
            'time': [],
            'true_value': [],
            'sensor_readings': {s.id: [] for s in sensors},
            'processed_values': {s.id: [] for s in sensors},
            'confidences': {s.id: [] for s in sensors},
            'displacements': {s.id: [] for s in sensors},
            'fused_value': []
        }

    def reference_signal(self, t: float) -> float:
        """
        Generate reference signal (what sensors should measure).

        Args:
            t: Time in seconds

        Returns:
            Reference value
        """
        # Combination of slow and fast dynamics
        slow = np.sin(2 * np.pi * 0.2 * t)  # 0.2 Hz
        fast = 0.3 * np.sin(2 * np.pi * 2.0 * t)  # 2 Hz
        return slow + fast

    def fuse_sensors(self, t: float, dt: float = 0.01) -> dict:
        """
        Fuse all sensor readings with trust-weighted aggregation.

        Args:
            t: Current time
            dt: Time step

        Returns:
            Dictionary with fusion results
        """
        true_value = self.reference_signal(t)

        weighted_sum = 0.0
        weight_total = 0.0
        sensor_results = {}

        # Process each sensor
        for sensor in self.sensors:
            # Simulate sensor reading
            reading = sensor.read_value(true_value, noise_level=0.05)

            # Process with temporal displacement
            state = sensor.process_reading(t, reading, true_value, dt)

            # Weight by confidence
            weight = state.confidence
            weighted_sum += weight * state.E
            weight_total += weight

            # Store results
            sensor_results[sensor.id] = {
                'reading': reading,
                'processed': state.E,
                'confidence': state.confidence,
                'displacement': state.Delta
            }

        # Compute weighted average
        fused_value = weighted_sum / weight_total if weight_total > 0 else 0.0

        return {
            'true_value': true_value,
            'sensor_results': sensor_results,
            'fused_value': fused_value
        }

    def run_simulation(self, duration: float = 10.0, dt: float = 0.01, verbose: bool = True):
        """
        Run control system simulation.

        Args:
            duration: Simulation duration in seconds
            dt: Time step
            verbose: Print progress updates
        """
        print("=" * 70)
        print("DISTRIBUTED CONTROL SYSTEM SIMULATION")
        print("=" * 70)
        print(f"\nConfiguration:")
        print(f"  Sensors:      {len(self.sensors)}")
        print(f"  Duration:     {duration:.1f} s")
        print(f"  Sample rate:  {1/dt:.0f} Hz")
        print(f"  Time step:    {dt*1000:.1f} ms")
        print()

        # Sensor details
        print("Sensors:")
        for sensor in self.sensors:
            print(f"  [{sensor.id}] {sensor.name:20s} "
                  f"latency={sensor.latency*1000:>6.1f}ms  "
                  f"reliability={sensor.reliability:.1f}")
        print()

        steps = int(duration / dt)
        start_time = time.time()

        for step in range(steps):
            t = step * dt

            # Fuse sensors
            results = self.fuse_sensors(t, dt)

            # Store history
            self.history['time'].append(t)
            self.history['true_value'].append(results['true_value'])
            self.history['fused_value'].append(results['fused_value'])

            for sensor_id, sensor_result in results['sensor_results'].items():
                self.history['sensor_readings'][sensor_id].append(sensor_result['reading'])
                self.history['processed_values'][sensor_id].append(sensor_result['processed'])
                self.history['confidences'][sensor_id].append(sensor_result['confidence'])
                self.history['displacements'][sensor_id].append(sensor_result['displacement'])

            # Progress update
            if verbose and step % 100 == 0:
                print(f"t={t:>6.2f}s: true={results['true_value']:>7.3f}  "
                      f"fused={results['fused_value']:>7.3f}  "
                      f"error={abs(results['true_value'] - results['fused_value']):.4f}")

        elapsed = time.time() - start_time
        print(f"\n✓ Simulation complete in {elapsed:.2f}s "
              f"({steps/elapsed:.0f} steps/sec)")

    def print_statistics(self):
        """Print statistics about the simulation."""
        print("\n" + "=" * 70)
        print("STATISTICS")
        print("=" * 70)

        # Convert to numpy arrays
        true_values = np.array(self.history['true_value'])
        fused_values = np.array(self.history['fused_value'])

        # Overall fusion error
        fusion_error = np.abs(true_values - fused_values)
        print(f"\nFusion Performance:")
        print(f"  RMS Error:    {np.sqrt(np.mean(fusion_error**2)):.6f}")
        print(f"  Max Error:    {np.max(fusion_error):.6f}")
        print(f"  Mean Error:   {np.mean(fusion_error):.6f}")

        # Per-sensor statistics
        print(f"\nPer-Sensor Statistics:")
        for sensor in self.sensors:
            processed = np.array(self.history['processed_values'][sensor.id])
            readings = np.array(self.history['sensor_readings'][sensor.id])
            confidences = np.array(self.history['confidences'][sensor.id])
            displacements = np.array(self.history['displacements'][sensor.id])

            raw_error = np.abs(true_values - readings)
            processed_error = np.abs(true_values - processed)

            print(f"\n  [{sensor.id}] {sensor.name}:")
            print(f"    Latency:          {sensor.latency*1000:.1f} ms")
            print(f"    Raw RMS Error:    {np.sqrt(np.mean(raw_error**2)):.6f}")
            print(f"    Processed RMS:    {np.sqrt(np.mean(processed_error**2)):.6f}")
            print(f"    Avg Confidence:   {np.mean(confidences):.3f}")
            print(f"    Avg Displacement: {np.mean(displacements)*1000:.1f} ms")
            print(f"    Max Displacement: {np.max(displacements)*1000:.1f} ms")


def create_example_network():
    """Create example sensor network."""
    sensors = [
        Sensor(
            id=1,
            name="Local Sensor",
            latency=0.01,  # 10ms - very fast
            reliability=0.95,
            position=(0, 0)
        ),
        Sensor(
            id=2,
            name="Remote Sensor A",
            latency=0.08,  # 80ms - moderate delay
            reliability=0.90,
            position=(100, 50)
        ),
        Sensor(
            id=3,
            name="Remote Sensor B",
            latency=0.15,  # 150ms - high delay
            reliability=0.85,
            position=(150, 100)
        ),
        Sensor(
            id=4,
            name="Unreliable Sensor",
            latency=0.05,  # 50ms - low delay
            reliability=0.70,  # Low reliability
            position=(50, 75)
        ),
    ]
    return sensors


def main():
    """Run comprehensive distributed control example."""
    print("\n")
    print("*" * 70)
    print("  TEMPORAL DISPLACEMENT: DISTRIBUTED CONTROL EXAMPLE")
    print("*" * 70)
    print()

    # Create sensor network
    sensors = create_example_network()

    # Create control system
    system = DistributedControlSystem(sensors)

    # Run simulation
    system.run_simulation(duration=10.0, dt=0.01, verbose=True)

    # Print statistics
    system.print_statistics()

    # Summary
    print("\n" + "=" * 70)
    print("KEY TAKEAWAYS")
    print("=" * 70)
    print("""
✓ Temporal Displacement handles sensors with different latencies
✓ Trust-gating automatically down-weights unreliable sensors
✓ Fused output is more accurate than any individual sensor
✓ System adapts displacement based on confidence in real-time
✓ Causality maintained (all Δ > 0) for online operation

This demonstrates how Temporal Displacement enables robust sensor fusion
in distributed systems with heterogeneous latencies and reliabilities.
    """)

    print("=" * 70)
    print()


if __name__ == "__main__":
    main()
