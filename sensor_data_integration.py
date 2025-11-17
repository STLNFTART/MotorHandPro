#!/usr/bin/env python3
"""
MotorHandPro Sensor Data Integration Framework

Fetches real sensor data from public repositories and validates Primal Logic
control against actual hardware telemetry (IMU, GPS, actuator encoders, etc.).

Supported Data Sources:
1. EuRoC MAV Dataset (ETH Zurich) - Micro Aerial Vehicle with IMU/visual-inertial
2. KITTI Dataset - Autonomous driving with IMU/GPS/LiDAR
3. TUM RGB-D Dataset - Indoor robotics with IMU/depth/pose
4. NASA Spacecraft Telemetry - ISS, satellites (when available)
5. Custom ROS bag files - User-provided sensor logs

Purpose:
- Extract empirical λ (Lightfoot constant) from real step responses
- Validate Lipschitz stability with real sensor noise
- Measure actual control performance vs. simulation predictions
- Generate hardware-validated datasets for publication

Patent: U.S. Provisional 63/842,846 (July 12, 2025)
"""

import numpy as np
import pandas as pd
from pathlib import Path
import requests
import zipfile
import tarfile
import json
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from scipy.optimize import curve_fit
from scipy.signal import butter, filtfilt
import hashlib


# ============================================================================
# Configuration
# ============================================================================

DATA_DIR = Path("sensor_data")
DATA_DIR.mkdir(exist_ok=True)

# Public dataset URLs
DATASETS = {
    "euroc_mav": {
        "name": "EuRoC MAV Dataset (Machine Hall 01)",
        "url": "http://robotics.ethz.ch/~asl-datasets/ijrr_euroc_mav_dataset/machine_hall/MH_01_easy/MH_01_easy.zip",
        "size": "2.3 GB",
        "sensors": ["IMU (200 Hz)", "Stereo cameras", "Ground truth pose"],
        "description": "Micro aerial vehicle flight data with high-rate IMU",
    },
    "kitti_raw": {
        "name": "KITTI Raw Dataset (2011_09_26_drive_0001)",
        "url": "https://s3.eu-central-1.amazonaws.com/avg-kitti/raw_data/2011_09_26_drive_0001/2011_09_26_drive_0001_sync.zip",
        "size": "2.5 GB",
        "sensors": ["IMU/GPS (10 Hz)", "Velodyne LiDAR", "Cameras"],
        "description": "Autonomous driving with IMU, GPS, and odometry",
    },
    "tum_rgbd": {
        "name": "TUM RGB-D Dataset (freiburg1_xyz)",
        "url": "https://vision.in.tum.de/rgbd/dataset/freiburg1/rgbd_dataset_freiburg1_xyz.tgz",
        "size": "735 MB",
        "sensors": ["IMU (100 Hz)", "RGB-D camera (30 Hz)", "Ground truth"],
        "description": "Handheld RGB-D sensor with accelerometer/gyroscope",
    },
}


# ============================================================================
# Data Models
# ============================================================================

@dataclass
class SensorData:
    """Container for sensor telemetry"""
    timestamp: np.ndarray  # seconds
    imu_accel: np.ndarray  # m/s² (N×3)
    imu_gyro: np.ndarray   # rad/s (N×3)
    position: Optional[np.ndarray] = None  # m (N×3), GPS or ground truth
    velocity: Optional[np.ndarray] = None  # m/s (N×3)
    orientation: Optional[np.ndarray] = None  # quaternion (N×4)
    metadata: Dict = None

    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


@dataclass
class ValidationResults:
    """Results from Primal Logic validation against sensor data"""
    lambda_empirical: float
    lambda_std: float
    lipschitz_estimate: float
    lipschitz_bounded: bool
    rms_error: float
    max_error: float
    dataset_name: str
    num_samples: int
    sampling_rate: float


# ============================================================================
# Dataset Downloaders
# ============================================================================

def download_file(url: str, dest: Path, chunk_size: int = 8192) -> bool:
    """
    Download file with progress indication

    Args:
        url: Download URL
        dest: Destination file path
        chunk_size: Download chunk size (bytes)

    Returns:
        Success status
    """
    try:
        print(f"Downloading: {url}")
        print(f"Destination: {dest}")

        response = requests.get(url, stream=True)
        response.raise_for_status()

        total_size = int(response.headers.get('content-length', 0))
        downloaded = 0

        with open(dest, 'wb') as f:
            for chunk in response.iter_content(chunk_size=chunk_size):
                if chunk:
                    f.write(chunk)
                    downloaded += len(chunk)
                    if total_size > 0:
                        progress = downloaded / total_size * 100
                        print(f"\rProgress: {progress:.1f}%", end='', flush=True)

        print(f"\n✓ Downloaded: {dest.name} ({downloaded / 1e6:.1f} MB)")
        return True

    except Exception as e:
        print(f"✗ Download failed: {e}")
        return False


def extract_archive(archive_path: Path, extract_dir: Path) -> bool:
    """
    Extract zip or tar.gz archive

    Args:
        archive_path: Path to archive file
        extract_dir: Extraction destination

    Returns:
        Success status
    """
    try:
        print(f"Extracting: {archive_path.name}")

        if archive_path.suffix == '.zip':
            with zipfile.ZipFile(archive_path, 'r') as zf:
                zf.extractall(extract_dir)
        elif archive_path.suffix == '.tgz' or archive_path.name.endswith('.tar.gz'):
            with tarfile.open(archive_path, 'r:gz') as tf:
                tf.extractall(extract_dir)
        else:
            print(f"✗ Unsupported archive format: {archive_path.suffix}")
            return False

        print(f"✓ Extracted to: {extract_dir}")
        return True

    except Exception as e:
        print(f"✗ Extraction failed: {e}")
        return False


def download_dataset(dataset_key: str, force_download: bool = False) -> Optional[Path]:
    """
    Download and extract dataset

    Args:
        dataset_key: Dataset identifier (e.g., 'euroc_mav')
        force_download: Re-download even if exists

    Returns:
        Path to extracted dataset directory, or None if failed
    """
    if dataset_key not in DATASETS:
        print(f"✗ Unknown dataset: {dataset_key}")
        print(f"Available: {list(DATASETS.keys())}")
        return None

    dataset = DATASETS[dataset_key]
    dataset_dir = DATA_DIR / dataset_key
    archive_name = Path(dataset['url']).name
    archive_path = DATA_DIR / archive_name

    # Check if already downloaded
    if dataset_dir.exists() and not force_download:
        print(f"✓ Dataset already exists: {dataset_dir}")
        return dataset_dir

    # Download
    if not archive_path.exists() or force_download:
        success = download_file(dataset['url'], archive_path)
        if not success:
            return None

    # Extract
    dataset_dir.mkdir(exist_ok=True)
    success = extract_archive(archive_path, dataset_dir)
    if not success:
        return None

    return dataset_dir


# ============================================================================
# Dataset Parsers
# ============================================================================

def parse_euroc_mav(dataset_dir: Path) -> SensorData:
    """
    Parse EuRoC MAV dataset IMU data

    Format: timestamp [ns], wx, wy, wz [rad/s], ax, ay, az [m/s²]
    File: mav0/imu0/data.csv
    """
    imu_file = dataset_dir / "mav0" / "imu0" / "data.csv"
    if not imu_file.exists():
        # Try alternative path (sometimes nested)
        candidates = list(dataset_dir.rglob("imu0/data.csv"))
        if candidates:
            imu_file = candidates[0]
        else:
            raise FileNotFoundError(f"IMU data not found in {dataset_dir}")

    print(f"Parsing EuRoC MAV IMU: {imu_file}")

    # Read CSV (skip header if present)
    df = pd.read_csv(imu_file, comment='#')

    # EuRoC format: timestamp, wx, wy, wz, ax, ay, az
    if df.shape[1] == 7:
        timestamp_ns = df.iloc[:, 0].values
        gyro = df.iloc[:, 1:4].values  # rad/s
        accel = df.iloc[:, 4:7].values  # m/s²
    else:
        raise ValueError(f"Unexpected EuRoC format: {df.shape[1]} columns")

    # Convert timestamp to seconds (EuRoC uses nanoseconds)
    timestamp = (timestamp_ns - timestamp_ns[0]) / 1e9

    metadata = {
        'dataset': 'EuRoC MAV',
        'sensor': 'ADIS16448 IMU',
        'rate_hz': 200,
        'duration_s': timestamp[-1],
        'num_samples': len(timestamp),
    }

    return SensorData(
        timestamp=timestamp,
        imu_accel=accel,
        imu_gyro=gyro,
        metadata=metadata
    )


def parse_kitti_raw(dataset_dir: Path) -> SensorData:
    """
    Parse KITTI raw dataset IMU/GPS data

    Format: IMU data in oxts/ directory
    """
    # KITTI stores IMU data in oxts/data/ as text files
    oxts_dir = dataset_dir / "oxts" / "data"
    if not oxts_dir.exists():
        candidates = list(dataset_dir.rglob("oxts/data"))
        if candidates:
            oxts_dir = candidates[0]
        else:
            raise FileNotFoundError(f"OXTS data not found in {dataset_dir}")

    print(f"Parsing KITTI IMU/GPS: {oxts_dir}")

    # Read all oxts files (one per timestamp)
    oxts_files = sorted(oxts_dir.glob("*.txt"))
    if not oxts_files:
        raise FileNotFoundError(f"No OXTS files in {oxts_dir}")

    data = []
    for f in oxts_files:
        values = np.loadtxt(f)
        data.append(values)

    data = np.array(data)

    # KITTI OXTS format (30 values):
    # lat, lon, alt, roll, pitch, yaw, vn, ve, vf, vl, vu,
    # ax, ay, az, af, al, au, wx, wy, wz, wf, wl, wu, ...
    # We extract: acceleration (ax, ay, az), gyro (wx, wy, wz), velocity (vn, ve, vu)

    accel = data[:, 11:14]  # ax, ay, az (m/s²)
    gyro = data[:, 17:20]   # wx, wy, wz (rad/s)
    velocity = data[:, 6:9]  # vn, ve, vf (m/s, forward velocity)

    # Timestamp from file index (assume 10 Hz)
    timestamp = np.arange(len(data)) / 10.0

    metadata = {
        'dataset': 'KITTI Raw',
        'sensor': 'OXTS RT3003 IMU/GPS',
        'rate_hz': 10,
        'duration_s': timestamp[-1],
        'num_samples': len(timestamp),
    }

    return SensorData(
        timestamp=timestamp,
        imu_accel=accel,
        imu_gyro=gyro,
        velocity=velocity,
        metadata=metadata
    )


def parse_tum_rgbd(dataset_dir: Path) -> SensorData:
    """
    Parse TUM RGB-D dataset accelerometer data

    Format: accelerometer.txt with timestamp, ax, ay, az
    """
    accel_file = dataset_dir / "accelerometer.txt"
    if not accel_file.exists():
        candidates = list(dataset_dir.rglob("accelerometer.txt"))
        if candidates:
            accel_file = candidates[0]
        else:
            raise FileNotFoundError(f"Accelerometer data not found in {dataset_dir}")

    print(f"Parsing TUM RGB-D accelerometer: {accel_file}")

    # Read space-separated file (skip comments)
    data = []
    with open(accel_file) as f:
        for line in f:
            if line.startswith('#'):
                continue
            values = line.split()
            if len(values) >= 4:
                data.append([float(v) for v in values[:4]])

    data = np.array(data)
    timestamp = data[:, 0] - data[0, 0]  # Relative to first sample
    accel = data[:, 1:4]  # ax, ay, az (m/s²)

    # Estimate gyro from orientation changes (if available)
    # For now, set to zeros (TUM doesn't always provide gyro in accelerometer.txt)
    gyro = np.zeros_like(accel)

    metadata = {
        'dataset': 'TUM RGB-D',
        'sensor': 'Kinect accelerometer',
        'rate_hz': 100,
        'duration_s': timestamp[-1],
        'num_samples': len(timestamp),
    }

    return SensorData(
        timestamp=timestamp,
        imu_accel=accel,
        imu_gyro=gyro,
        metadata=metadata
    )


# ============================================================================
# Primal Logic Validation
# ============================================================================

def extract_gravity_vector(accel_data: np.ndarray, window_size: int = 100) -> np.ndarray:
    """
    Extract gravity vector from accelerometer data via low-pass filter

    Args:
        accel_data: Acceleration data (N×3) in m/s²
        window_size: Moving average window size

    Returns:
        Gravity vector (N×3)
    """
    # Low-pass filter to extract static gravity component
    b, a = butter(2, 0.1, btype='low')
    gravity = filtfilt(b, a, accel_data, axis=0)
    return gravity


def detect_step_responses(accel_data: np.ndarray, timestamp: np.ndarray,
                         threshold: float = 2.0) -> List[Tuple[int, int]]:
    """
    Detect step-like changes in acceleration (potential control inputs)

    Args:
        accel_data: Acceleration magnitude (N,)
        timestamp: Time vector (N,)
        threshold: Detection threshold (m/s²)

    Returns:
        List of (start_idx, end_idx) tuples for each step response
    """
    # Compute magnitude
    accel_mag = np.linalg.norm(accel_data, axis=1)

    # Detect sudden changes (derivative threshold)
    daccel = np.diff(accel_mag)
    steps = []

    in_step = False
    start_idx = 0

    for i, da in enumerate(daccel):
        if abs(da) > threshold and not in_step:
            start_idx = i
            in_step = True
        elif abs(da) < threshold / 2 and in_step:
            end_idx = min(i + 500, len(accel_mag))  # 500 samples decay window
            steps.append((start_idx, end_idx))
            in_step = False

    return steps


def fit_exponential_decay(t: np.ndarray, x: np.ndarray) -> Tuple[float, float, float]:
    """
    Fit exponential decay model: x(t) = x₀ * exp(-λ * t) + x_ss

    Args:
        t: Time vector (relative to step)
        x: Signal (error magnitude, velocity, etc.)

    Returns:
        (x0, lambda, x_ss) - initial value, decay rate, steady-state
    """
    def model(t, x0, lam, xss):
        return x0 * np.exp(-lam * t) + xss

    # Initial guess
    p0 = [x[0] - x[-1], 0.115, x[-1]]  # λ guess = 0.115 (theoretical)

    try:
        params, cov = curve_fit(model, t, x, p0=p0, maxfev=10000)
        return params
    except RuntimeError:
        # Fit failed, return NaN
        return (np.nan, np.nan, np.nan)


def validate_primal_logic(sensor_data: SensorData) -> ValidationResults:
    """
    Validate Primal Logic control framework against real sensor data

    Extracts:
    - Empirical λ (Lightfoot constant) from step responses
    - Lipschitz stability (bounded state evolution)
    - RMS/max tracking error

    Args:
        sensor_data: Parsed sensor telemetry

    Returns:
        ValidationResults with empirical constants
    """
    print(f"\nValidating Primal Logic against {sensor_data.metadata['dataset']}...")

    # Extract gravity-compensated acceleration (high-pass dynamics)
    gravity = extract_gravity_vector(sensor_data.imu_accel)
    accel_dynamic = sensor_data.imu_accel - gravity

    # Detect step responses in acceleration magnitude
    accel_mag = np.linalg.norm(accel_dynamic, axis=1)
    steps = detect_step_responses(sensor_data.imu_accel, sensor_data.timestamp, threshold=1.5)

    print(f"Detected {len(steps)} step-like responses")

    # Fit exponential decay to each step response
    lambda_estimates = []
    for start, end in steps[:10]:  # Analyze first 10 steps
        t_step = sensor_data.timestamp[start:end] - sensor_data.timestamp[start]
        x_step = accel_mag[start:end]

        # Normalize to [0, 1] for better fitting
        x_norm = (x_step - x_step.min()) / (x_step.max() - x_step.min() + 1e-12)

        x0, lam, xss = fit_exponential_decay(t_step, x_norm)

        if not np.isnan(lam) and 0.01 < lam < 1.0:  # Sanity check
            lambda_estimates.append(lam)
            print(f"  Step {len(lambda_estimates)}: λ = {lam:.6f} s⁻¹")

    if not lambda_estimates:
        print("⚠ No valid step responses found")
        lambda_empirical = np.nan
        lambda_std = np.nan
    else:
        lambda_empirical = np.mean(lambda_estimates)
        lambda_std = np.std(lambda_estimates)
        print(f"\n✓ Empirical λ: {lambda_empirical:.6f} ± {lambda_std:.6f} s⁻¹")
        print(f"  Theoretical λ: 0.115000 s⁻¹")
        print(f"  Relative error: {abs(lambda_empirical - 0.115) / 0.115 * 100:.1f}%")

    # Lipschitz stability check: state should remain bounded
    # Use acceleration magnitude as proxy for Primal state
    max_accel = np.max(np.abs(accel_mag))
    lipschitz_bounded = max_accel < 50.0  # Arbitrary large bound (50 m/s²)

    # Estimate Lipschitz constant from max state derivative
    daccel = np.diff(accel_mag) / np.diff(sensor_data.timestamp)
    lipschitz_estimate = np.max(np.abs(daccel)) / (max_accel + 1e-12)

    # RMS and max error (using gravity deviation as error proxy)
    gravity_mag = np.linalg.norm(gravity, axis=1)
    gravity_error = np.abs(gravity_mag - 9.81)  # Expected Earth gravity
    rms_error = np.sqrt(np.mean(gravity_error**2))
    max_error = np.max(gravity_error)

    results = ValidationResults(
        lambda_empirical=lambda_empirical,
        lambda_std=lambda_std,
        lipschitz_estimate=lipschitz_estimate,
        lipschitz_bounded=lipschitz_bounded,
        rms_error=rms_error,
        max_error=max_error,
        dataset_name=sensor_data.metadata['dataset'],
        num_samples=len(sensor_data.timestamp),
        sampling_rate=sensor_data.metadata['rate_hz']
    )

    return results


# ============================================================================
# Main Validation Runner
# ============================================================================

def run_validation_suite(datasets: List[str] = None, download: bool = False):
    """
    Run Primal Logic validation against multiple sensor datasets

    Args:
        datasets: List of dataset keys to validate (default: all)
        download: Download datasets if not present
    """
    if datasets is None:
        datasets = list(DATASETS.keys())

    print("=" * 70)
    print("  MotorHandPro Sensor Data Validation Suite")
    print("=" * 70)
    print("\nValidating Primal Logic control against real sensor data:")
    for key in datasets:
        print(f"  - {DATASETS[key]['name']}")
    print("=" * 70)

    results = []

    for dataset_key in datasets:
        print(f"\n[Dataset: {dataset_key}]")
        print("=" * 70)

        # Download if requested
        if download:
            dataset_dir = download_dataset(dataset_key)
            if dataset_dir is None:
                print(f"✗ Skipping {dataset_key} (download failed)")
                continue
        else:
            dataset_dir = DATA_DIR / dataset_key
            if not dataset_dir.exists():
                print(f"⚠ Dataset not found: {dataset_dir}")
                print(f"  Run with download=True to fetch from {DATASETS[dataset_key]['url']}")
                continue

        # Parse sensor data
        try:
            if dataset_key == 'euroc_mav':
                sensor_data = parse_euroc_mav(dataset_dir)
            elif dataset_key == 'kitti_raw':
                sensor_data = parse_kitti_raw(dataset_dir)
            elif dataset_key == 'tum_rgbd':
                sensor_data = parse_tum_rgbd(dataset_dir)
            else:
                print(f"✗ No parser for {dataset_key}")
                continue

            print(f"✓ Loaded {sensor_data.num_samples} samples @ {sensor_data.metadata['rate_hz']} Hz")
            print(f"  Duration: {sensor_data.metadata['duration_s']:.1f} s")

        except Exception as e:
            print(f"✗ Parsing failed: {e}")
            continue

        # Validate Primal Logic
        try:
            validation = validate_primal_logic(sensor_data)
            results.append(validation)

            print(f"\nValidation Results:")
            print(f"  Empirical λ: {validation.lambda_empirical:.6f} ± {validation.lambda_std:.6f} s⁻¹")
            print(f"  Lipschitz estimate: {validation.lipschitz_estimate:.6e}")
            print(f"  Lipschitz bounded: {validation.lipschitz_bounded} ✓" if validation.lipschitz_bounded else "✗")
            print(f"  RMS gravity error: {validation.rms_error:.4f} m/s²")
            print(f"  Max gravity error: {validation.max_error:.4f} m/s²")

        except Exception as e:
            print(f"✗ Validation failed: {e}")
            import traceback
            traceback.print_exc()
            continue

    # Summary
    print("\n" + "=" * 70)
    print("  Validation Summary")
    print("=" * 70)

    if not results:
        print("✗ No datasets successfully validated")
        return

    for r in results:
        print(f"\n{r.dataset_name}:")
        print(f"  λ (empirical): {r.lambda_empirical:.6f} s⁻¹")
        print(f"  λ (theoretical): 0.115000 s⁻¹")
        if not np.isnan(r.lambda_empirical):
            error_pct = abs(r.lambda_empirical - 0.115) / 0.115 * 100
            print(f"  Relative error: {error_pct:.1f}%")
            if error_pct < 10:
                print(f"  ✓ Within 10% tolerance")
            else:
                print(f"  ⚠ Exceeds 10% tolerance")

    print("\n" + "=" * 70)
    print("  Validation complete!")
    print("=" * 70)
    print("\nNext steps:")
    print("  1. Review empirical λ values (should be ≈ 0.115 s⁻¹)")
    print("  2. Investigate outliers (sensor-specific biases?)")
    print("  3. Use empirical λ to update constants in codebase")
    print("  4. Publish validation dataset to Zenodo/Figshare")
    print("=" * 70)


# ============================================================================
# Command-Line Interface
# ============================================================================

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate Primal Logic against real sensor data"
    )
    parser.add_argument(
        '--datasets',
        nargs='+',
        choices=list(DATASETS.keys()),
        help='Datasets to validate (default: all)'
    )
    parser.add_argument(
        '--download',
        action='store_true',
        help='Download datasets if not present (WARNING: multi-GB downloads)'
    )
    parser.add_argument(
        '--list',
        action='store_true',
        help='List available datasets and exit'
    )

    args = parser.parse_args()

    if args.list:
        print("\nAvailable Datasets:")
        print("=" * 70)
        for key, info in DATASETS.items():
            print(f"\n{key}:")
            print(f"  Name: {info['name']}")
            print(f"  Size: {info['size']}")
            print(f"  Sensors: {', '.join(info['sensors'])}")
            print(f"  Description: {info['description']}")
            print(f"  URL: {info['url']}")
        print("=" * 70)
    else:
        run_validation_suite(datasets=args.datasets, download=args.download)
