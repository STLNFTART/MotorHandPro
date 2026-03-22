#!/usr/bin/env python3
"""
Complete Prosthetics Integration Pipeline
Seamlessly integrates all components:
1. EMG Dataset Loading
2. Hardware Integration (Arduino)
3. NASA NSRL Radiation Testing
4. Benchmark Publishing
"""
import sys
from pathlib import Path

# Ensure the repo root is importable without a hard-coded absolute path.
_REPO_ROOT = Path(__file__).parent.resolve()
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))


def main():
    print("=" * 70)
    print("MOTORHANDPRO COMPLETE PROSTHETICS INTEGRATION PIPELINE")
    print("=" * 70)
    print()
    print("This pipeline demonstrates seamless integration of:")
    print("  1. Automated EMG dataset loading")
    print("  2. Physical hardware control (Arduino)")
    print("  3. NASA radiation testing")
    print("  4. Benchmark publishing")
    print()

    # Step 1: Dataset Loading
    print("\n" + "="*70)
    print("STEP 1: EMG Dataset Loading")
    print("="*70)

    from lam.integrations.dataset_loader import EMGDatasetLoader

    loader = EMGDatasetLoader()

    print("\nAvailable datasets:")
    datasets = loader.list_datasets()
    for ds in datasets:
        print(f"  - {ds['name']} ({ds['subjects']} subjects, {ds['gestures']} gestures)")

    # Load a dataset
    print("\nLoading emg_gesture_recognition_2024...")
    data_dict = loader.load_dataset("emg_gesture_recognition_2024")
    print(f"✓ Loaded: {data_dict['data'].shape[0]} samples, {data_dict['num_channels']} channels")

    # Step 2: Hardware Integration
    print("\n" + "="*70)
    print("STEP 2: Hardware Integration")
    print("="*70)

    from lam.integrations.hardware_bridge import LAMHardwareIntegration

    print("\nInitializing hardware bridge...")
    hardware = LAMHardwareIntegration()

    # Try to connect (will fail gracefully if no hardware)
    if hardware.connect():
        print("✓ Connected to Arduino prosthetic hardware")

        # Calibrate
        print("\nCalibrating sensors...")
        hardware.hardware.calibrate()

        # Execute LAM-controlled gesture
        print("\nExecuting LAM-controlled gesture...")
        result = hardware.execute_lam_controlled_gesture(radiation_dose=0)
        print(f"✓ Gesture: {result['gesture']}, Confidence: {result['confidence']:.2f}")

        hardware.disconnect()
    else:
        print("⚠ Hardware not available - continuing with simulation")

    # Step 3: NASA NSRL Testing
    print("\n" + "="*70)
    print("STEP 3: NASA NSRL Radiation Testing")
    print("="*70)

    from lam.integrations.nasa_nsrl_integration import NASANSRLIntegration, test_prosthetic_device_at_nsrl
    from lam.integrations.prosthetics_integration import ProstheticsController

    print("\nInitializing NASA NSRL integration...")
    nsrl = NASANSRLIntegration()

    # Create test device
    print("Creating test device...")
    prosthetic = ProstheticsController(
        radiation_environment="mars_surface",
        num_channels=8
    )

    # Run radiation test
    print("\nRunning radiation test...")
    beam_configs = ["gcr_proton", "gcr_helium", "spe_proton"]

    report = nsrl.run_radiation_test(
        device_name="MotorHandPro Prosthetic Controller v1.0",
        beam_configs=beam_configs,
        device_under_test=prosthetic,
        test_function=test_prosthetic_device_at_nsrl
    )

    print(f"\n✓ Radiation test complete: {report.qualification_status}")
    print(f"  Total dose: {report.total_ionizing_dose_gray:.3f} Gy")
    print(f"  MTBF: {report.mtbf_hours:.1f} hours")

    # Generate NASA report
    report_path = nsrl.generate_nasa_report_pdf(report)
    print(f"  Report: {report_path}")

    # Step 4: Benchmark Publishing
    print("\n" + "="*70)
    print("STEP 4: Benchmark Publishing")
    print("="*70)

    from lam.integrations.benchmark_publisher import BenchmarkPublisher

    print("\nInitializing benchmark publisher...")
    publisher = BenchmarkPublisher()

    # Run comprehensive benchmarks
    print("\nRunning comprehensive benchmarks...")
    suite = publisher.run_comprehensive_benchmark()

    # Save and publish
    print("\nPublishing results...")
    publisher.save_suite(suite)
    leaderboard = publisher.generate_leaderboard(suite)
    pub_report = publisher.generate_publication_report(suite)
    pwc_export = publisher.export_to_papers_with_code(suite)

    print(f"\n✓ Benchmark suite complete:")
    print(f"  Leaderboard: {leaderboard}")
    print(f"  Report: {pub_report}")
    print(f"  Papers with Code: {pwc_export}")

    # Final Summary
    print("\n" + "="*70)
    print("PIPELINE COMPLETE!")
    print("="*70)

    print(f"""
Summary:
  ✓ EMG Datasets: {len(datasets)} available
  ✓ Hardware: {'Connected' if hardware.hardware.connected else 'Simulation mode'}
  ✓ NASA Testing: {report.qualification_status}
  ✓ Benchmarks: {suite.summary_statistics['total_benchmarks']} tests
  ✓ Average Accuracy: {suite.summary_statistics['avg_accuracy']:.1%}
  ✓ Average Response Time: {suite.summary_statistics['avg_response_time_ms']:.1f} ms

Next Steps:
  1. Review benchmark leaderboard: {leaderboard}
  2. Review NASA test report: {report_path}
  3. Download real EMG datasets from scientific repositories
  4. Connect physical Arduino hardware for validation
  5. Submit to Papers with Code: {pwc_export}

Documentation:
  - PROSTHETICS_INTEGRATION.md - Complete integration guide
  - Dataset links and references included
  - Hardware setup instructions provided
  - NASA testing standards documented
""")

    print("=" * 70)
    print("Thank you for using MotorHandPro!")
    print("=" * 70)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nPipeline interrupted by user")
    except Exception as e:
        print(f"\n\nERROR: {e}")
        import traceback
        traceback.print_exc()
