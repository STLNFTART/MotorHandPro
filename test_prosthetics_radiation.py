#!/usr/bin/env python3
"""
Comprehensive Test Suite for Prosthetics Integration and Radiation Testing
Validates effectiveness of prosthetics in radiated space environments
"""
import sys
import numpy as np
from pathlib import Path
from typing import Dict, Any, List
import matplotlib.pyplot as plt

# Add paths
sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent / "lam" / "integrations"))

from lam.integrations.prosthetics_integration import ProstheticsController
from lam.integrations.radiation_testing import RadiationSimulator, EffectivenessMetrics


def test_basic_prosthetics():
    """Test basic prosthetics functionality without radiation"""
    print("\n" + "="*70)
    print("TEST 1: Basic Prosthetics Control (No Radiation)")
    print("="*70)

    controller = ProstheticsController(
        dataset="emg_gesture_recognition_2024",
        radiation_environment=None,
        num_channels=8
    )

    # Load synthetic data
    emg_data = controller.load_dataset()

    # Test gesture recognition
    print("\nTesting gesture recognition...")
    gestures_detected = []

    for i in range(0, min(len(emg_data), controller.window_samples * 10), controller.window_samples):
        window = emg_data[i:i + controller.window_samples]
        result = controller.execute_gesture(window)
        gestures_detected.append(result['gesture'])

        if len(gestures_detected) <= 5:  # Show first 5
            print(f"  [{len(gestures_detected)}] Gesture: {result['gesture']:15s} | "
                  f"Confidence: {result['confidence']:.3f} | "
                  f"Resonance: {result['quantum_resonance']:.3f}")

    # Get statistics
    stats = controller.get_statistics()
    print(f"\nStatistics:")
    print(f"  Total actions: {stats['total_actions']}")
    print(f"  Unique gestures: {stats['unique_gestures']}")
    print(f"  Average confidence: {stats['average_confidence']:.3f}")
    print(f"  Gesture distribution: {stats['gesture_distribution']}")

    assert stats['total_actions'] > 0, "No actions recorded"
    assert stats['average_confidence'] > 0.5, "Low confidence detected"

    print("\n✓ TEST 1 PASSED")
    return controller, stats


def test_radiation_environments():
    """Test prosthetics under different radiation environments"""
    print("\n" + "="*70)
    print("TEST 2: Radiation Environment Effects")
    print("="*70)

    environments = [
        ("leo", "LEO (ISS)", 180),
        ("mars_transit", "Mars Transit", 180),
        ("mars_surface", "Mars Surface", 500)
    ]

    results = {}

    for env_name, description, duration in environments:
        print(f"\n--- Testing {description} ---")

        controller = ProstheticsController(
            dataset="emg_gesture_recognition_2024",
            radiation_environment=env_name,
            num_channels=8
        )

        # Simulate mission
        emg_data = controller.load_dataset()

        # Test at different radiation dose levels
        dose_levels = [0, 50, 100, 150, 200]
        confidence_by_dose = []

        for dose in dose_levels:
            confidences = []

            # Test 5 gestures at each dose
            for i in range(5):
                idx = (i * controller.window_samples) % (len(emg_data) - controller.window_samples)
                window = emg_data[idx:idx + controller.window_samples]
                result = controller.execute_gesture(window, context={"radiation_dose": dose})
                confidences.append(result['confidence'])

            avg_confidence = np.mean(confidences)
            confidence_by_dose.append(avg_confidence)

            print(f"  Dose: {dose:3d} mSv → Avg Confidence: {avg_confidence:.3f}")

        results[description] = {
            "environment": env_name,
            "dose_levels": dose_levels,
            "confidence": confidence_by_dose,
            "degradation": confidence_by_dose[0] - confidence_by_dose[-1]
        }

        # Check that confidence degrades with radiation
        assert confidence_by_dose[0] >= confidence_by_dose[-1], \
            f"Confidence should degrade with radiation in {description}"

    print("\n✓ TEST 2 PASSED")
    return results


def test_radiation_simulation():
    """Test radiation simulation framework"""
    print("\n" + "="*70)
    print("TEST 3: Radiation Simulation Framework")
    print("="*70)

    # Test Mars surface environment
    print("\nInitializing Mars surface radiation simulator...")
    sim = RadiationSimulator(
        environment="mars_surface",
        shield_thickness=20.0,
        mission_duration=500
    )

    print(f"  Total radiation events: {len(sim.radiation_events)}")
    print(f"  Cumulative dose: {sim.cumulative_dose:.1f} mSv")

    # Count event types
    gcr_events = sum(1 for e in sim.radiation_events if e.source.value == "galactic_cosmic_rays")
    spe_events = sum(1 for e in sim.radiation_events if e.source.value == "solar_particle_event")

    print(f"  GCR events: {gcr_events}")
    print(f"  SPE events: {spe_events}")

    assert len(sim.radiation_events) > 0, "No radiation events generated"
    assert sim.cumulative_dose > 0, "No cumulative dose calculated"
    assert spe_events >= 0, "SPE events should be non-negative"

    print("\n✓ TEST 3 PASSED")
    return sim


def test_prosthetic_effectiveness():
    """Test prosthetic effectiveness under radiation"""
    print("\n" + "="*70)
    print("TEST 4: Prosthetic Effectiveness Testing")
    print("="*70)

    environments = ["leo", "mars_transit", "mars_surface"]
    metrics_by_env = {}

    for env in environments:
        print(f"\n--- Testing {env.upper()} ---")

        # Create radiation simulator
        duration = 500 if "mars" in env else 180
        sim = RadiationSimulator(
            environment=env,
            mission_duration=duration
        )

        # Create prosthetic controller
        prosthetic = ProstheticsController(
            radiation_environment=env,
            num_channels=8
        )

        # Run effectiveness test
        metrics = sim.test_prosthetic_effectiveness(
            prosthetic=prosthetic,
            test_suite="fine_motor_tasks",
            num_trials=50  # Reduced for faster testing
        )

        metrics_by_env[env] = metrics

        print(f"\n  Results for {env.upper()}:")
        print(f"    Task Completion: {metrics.task_completion_rate:.1f}%")
        print(f"    Control Accuracy: {metrics.control_accuracy:.1f}%")
        print(f"    Response Time: {metrics.response_time_ms:.1f} ms")
        print(f"    MTBF: {metrics.mtbf_hours:.1f} hours")
        print(f"    SEE Events: {metrics.see_events}")
        print(f"    Total Dose: {metrics.total_dose_msv:.1f} mSv")

        # Validate metrics
        assert 0 <= metrics.task_completion_rate <= 100, "Invalid completion rate"
        assert 0 <= metrics.control_accuracy <= 100, "Invalid accuracy"
        assert metrics.response_time_ms > 0, "Invalid response time"
        assert metrics.mtbf_hours > 0, "Invalid MTBF"

    print("\n✓ TEST 4 PASSED")
    return metrics_by_env


def test_dataset_compatibility():
    """Test compatibility with different dataset formats"""
    print("\n" + "="*70)
    print("TEST 5: Dataset Compatibility")
    print("="*70)

    datasets = [
        "emg_gesture_recognition_2024",
        "multi_day_emg_2022",
        "high_density_semg_2021"
    ]

    for dataset_name in datasets:
        print(f"\n--- Testing {dataset_name} ---")

        controller = ProstheticsController(
            dataset=dataset_name,
            num_channels=8
        )

        # Load synthetic data (in real use, would load actual dataset)
        data = controller.load_dataset()

        print(f"  Data shape: {data.shape}")
        print(f"  Channels: {data.shape[1]}")
        print(f"  Samples: {data.shape[0]}")

        # Extract features from first window
        window = data[:controller.window_samples]
        features = controller.extract_features(window)

        print(f"  Features extracted: {len(features)}")

        assert data.shape[0] > 0, f"No data loaded for {dataset_name}"
        assert data.shape[1] == controller.num_channels, "Channel mismatch"
        assert len(features) > 0, "No features extracted"

    print("\n✓ TEST 5 PASSED")


def test_temporal_displacement():
    """Test LAM temporal displacement with confidence gating"""
    print("\n" + "="*70)
    print("TEST 6: LAM Temporal Displacement")
    print("="*70)

    controller = ProstheticsController(num_channels=8)
    emg_data = controller.load_dataset()

    # Test with different confidence levels
    confidence_levels = [0.95, 0.75, 0.50, 0.25]
    displacements = []

    print("\nTesting temporal displacement with varying confidence:")
    for confidence in confidence_levels:
        window = emg_data[:controller.window_samples]
        result = controller.execute_gesture(window, context={"confidence": confidence})
        displacement = result['temporal_displacement']
        displacements.append(displacement)

        print(f"  Confidence: {confidence:.2f} → Displacement: {displacement:.4f}s")

    # Verify that lower confidence increases displacement
    assert displacements[0] < displacements[-1], \
        "Lower confidence should increase temporal displacement"

    print("\n✓ TEST 6 PASSED")


def generate_visualization(radiation_results, metrics_by_env):
    """Generate visualization of test results"""
    print("\n" + "="*70)
    print("Generating Visualizations")
    print("="*70)

    try:
        fig, axes = plt.subplots(2, 2, figsize=(14, 10))

        # Plot 1: Confidence degradation by radiation dose
        ax1 = axes[0, 0]
        for env_name, data in radiation_results.items():
            ax1.plot(data['dose_levels'], data['confidence'], marker='o', label=env_name)
        ax1.set_xlabel('Radiation Dose (mSv)')
        ax1.set_ylabel('Average Confidence')
        ax1.set_title('Confidence Degradation vs Radiation Dose')
        ax1.legend()
        ax1.grid(True, alpha=0.3)

        # Plot 2: Task completion by environment
        ax2 = axes[0, 1]
        envs = list(metrics_by_env.keys())
        completion_rates = [metrics_by_env[e].task_completion_rate for e in envs]
        ax2.bar(envs, completion_rates, color=['#2ecc71', '#f39c12', '#e74c3c'])
        ax2.set_ylabel('Task Completion Rate (%)')
        ax2.set_title('Task Completion by Environment')
        ax2.set_ylim(0, 100)
        ax2.grid(True, alpha=0.3, axis='y')

        # Plot 3: Control accuracy vs dose
        ax3 = axes[1, 0]
        doses = [metrics_by_env[e].total_dose_msv for e in envs]
        accuracies = [metrics_by_env[e].control_accuracy for e in envs]
        ax3.scatter(doses, accuracies, s=100, alpha=0.6, c=['#2ecc71', '#f39c12', '#e74c3c'])
        for i, env in enumerate(envs):
            ax3.annotate(env, (doses[i], accuracies[i]), xytext=(5, 5), textcoords='offset points')
        ax3.set_xlabel('Total Dose (mSv)')
        ax3.set_ylabel('Control Accuracy (%)')
        ax3.set_title('Control Accuracy vs Total Radiation Dose')
        ax3.grid(True, alpha=0.3)

        # Plot 4: MTBF comparison
        ax4 = axes[1, 1]
        mtbf_values = [metrics_by_env[e].mtbf_hours for e in envs]
        ax4.barh(envs, mtbf_values, color=['#2ecc71', '#f39c12', '#e74c3c'])
        ax4.set_xlabel('MTBF (hours)')
        ax4.set_title('Mean Time Between Failures')
        ax4.grid(True, alpha=0.3, axis='x')

        plt.tight_layout()
        output_path = Path(__file__).parent / "prosthetics_radiation_test_results.png"
        plt.savefig(output_path, dpi=150, bbox_inches='tight')
        print(f"\nVisualization saved to: {output_path}")

    except Exception as e:
        print(f"\nWarning: Could not generate visualization: {e}")


def main():
    """Run all tests"""
    print("\n" + "="*70)
    print("MOTORHANDPRO PROSTHETICS & RADIATION TESTING SUITE")
    print("="*70)

    try:
        # Run all tests
        controller, basic_stats = test_basic_prosthetics()
        radiation_results = test_radiation_environments()
        sim = test_radiation_simulation()
        metrics_by_env = test_prosthetic_effectiveness()
        test_dataset_compatibility()
        test_temporal_displacement()

        # Generate visualization
        generate_visualization(radiation_results, metrics_by_env)

        # Print summary
        print("\n" + "="*70)
        print("TEST SUMMARY")
        print("="*70)
        print("\n✓ All tests passed successfully!")
        print("\nKey Findings:")
        print(f"  - Basic prosthetics control: {basic_stats['average_confidence']:.1%} avg confidence")
        print(f"  - Radiation environments tested: {len(radiation_results)}")
        print(f"  - Effectiveness metrics collected: {len(metrics_by_env)}")

        print("\nNext Steps:")
        print("  1. Download real EMG datasets from scientific repositories")
        print("  2. Integrate with NASA radiation testing facilities")
        print("  3. Validate with hardware prosthetic devices")
        print("  4. Publish findings and benchmarks")

        print("\n" + "="*70)
        print("Testing complete!")
        print("="*70 + "\n")

        return 0

    except Exception as e:
        print(f"\n❌ TEST FAILED: {e}")
        import traceback
        traceback.print_exc()
        return 1


if __name__ == "__main__":
    sys.exit(main())
