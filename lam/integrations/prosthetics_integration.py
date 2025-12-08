#!/usr/bin/env python3
"""
LAM Integration with Prosthetics Control Systems
Enables myoelectric control, EMG signal processing, and radiation-hardened operation
"""
import sys
import json
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime
from dataclasses import dataclass

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))


@dataclass
class EMGSignal:
    """Represents an EMG signal sample"""
    timestamp: float
    channels: np.ndarray  # Multi-channel EMG data
    amplitude: float
    noise_level: float = 0.0
    confidence: float = 1.0


@dataclass
class GestureAction:
    """Represents a recognized gesture action"""
    gesture_type: str
    confidence: float
    emg_features: Dict[str, float]
    timestamp: float
    radiation_affected: bool = False


class ProstheticsController:
    """
    Interface between LAM and prosthetic control systems
    Supports EMG signal processing, gesture recognition, and radiation-hardened operation
    """

    # Standard gesture types
    GESTURES = [
        "hand_open",
        "hand_close",
        "wrist_flexion",
        "wrist_extension",
        "wrist_supination",
        "wrist_pronation",
        "pinch_grip",
        "power_grip",
        "rest"
    ]

    def __init__(self,
                 dataset: str = "emg_gesture_recognition_2024",
                 radiation_environment: Optional[str] = None,
                 num_channels: int = 8):
        """
        Initialize prosthetics controller

        Args:
            dataset: Name of EMG dataset to use
            radiation_environment: Optional radiation environment (None, "leo", "mars_transit", "mars_surface")
            num_channels: Number of EMG channels
        """
        self.dataset = dataset
        self.radiation_environment = radiation_environment
        self.num_channels = num_channels

        # Signal processing parameters
        self.sample_rate = 1000  # Hz
        self.temporal_window = 0.2  # 200ms window
        self.window_samples = int(self.sample_rate * self.temporal_window)

        # LAM integration parameters
        self.alpha = 0.54  # Temporal weighting (Primal Logic constant)
        self.lambda_decay = 0.16905  # Lightfoot constant

        # Radiation effects
        self.radiation_dose = 0.0  # mSv cumulative
        self.noise_floor = 0.01  # Baseline noise

        # Action history
        self.action_history: List[GestureAction] = []

        # Initialize radiation effects if environment specified
        if radiation_environment:
            self._initialize_radiation_environment(radiation_environment)

        print(f"ProstheticsController initialized:")
        print(f"  Dataset: {dataset}")
        print(f"  Channels: {num_channels}")
        print(f"  Radiation: {radiation_environment or 'None'}")

    def _initialize_radiation_environment(self, environment: str):
        """Set radiation parameters based on environment"""
        radiation_profiles = {
            "leo": {
                "dose_rate": 0.0005,  # mSv/hour (ISS-like)
                "noise_multiplier": 1.2,
                "see_rate": 0.001  # Events per hour
            },
            "mars_transit": {
                "dose_rate": 0.002,  # mSv/hour
                "noise_multiplier": 1.5,
                "see_rate": 0.005
            },
            "mars_surface": {
                "dose_rate": 0.001,  # mSv/hour (with some shielding)
                "noise_multiplier": 1.3,
                "see_rate": 0.003
            }
        }

        if environment in radiation_profiles:
            profile = radiation_profiles[environment]
            self.dose_rate = profile["dose_rate"]
            self.noise_multiplier = profile["noise_multiplier"]
            self.see_rate = profile["see_rate"]
            self.noise_floor *= self.noise_multiplier

    def load_dataset(self,
                     path: Optional[Path] = None,
                     normalize: bool = True,
                     temporal_window: Optional[float] = None) -> np.ndarray:
        """
        Load EMG dataset from file

        Args:
            path: Path to dataset CSV/NPY file
            normalize: Whether to normalize signals
            temporal_window: Override default temporal window (seconds)

        Returns:
            Loaded EMG data array
        """
        if temporal_window:
            self.temporal_window = temporal_window
            self.window_samples = int(self.sample_rate * self.temporal_window)

        if path is None:
            # Generate synthetic EMG data for demonstration
            print("No dataset path provided, generating synthetic EMG data...")
            return self._generate_synthetic_emg(duration=10.0)

        path = Path(path)
        if not path.exists():
            raise FileNotFoundError(f"Dataset not found: {path}")

        # Load data based on file type
        if path.suffix == '.npy':
            data = np.load(path)
        elif path.suffix == '.csv':
            data = np.loadtxt(path, delimiter=',')
        else:
            raise ValueError(f"Unsupported file format: {path.suffix}")

        if normalize:
            data = self._normalize_emg(data)

        print(f"Loaded dataset: {data.shape}")
        return data

    def _generate_synthetic_emg(self, duration: float = 10.0) -> np.ndarray:
        """Generate synthetic EMG signals for testing"""
        num_samples = int(self.sample_rate * duration)

        # Generate realistic EMG-like signals
        t = np.linspace(0, duration, num_samples)
        signals = np.zeros((num_samples, self.num_channels))

        for ch in range(self.num_channels):
            # Base frequency components
            base = np.sin(2 * np.pi * 50 * t + ch * 0.5)  # 50 Hz muscle activity
            noise = np.random.randn(num_samples) * 0.1

            # Add bursts for gesture events
            for i in range(0, num_samples, self.sample_rate):
                if np.random.rand() > 0.5:
                    burst_duration = int(0.3 * self.sample_rate)
                    end_idx = min(i + burst_duration, num_samples)
                    signals[i:end_idx, ch] = base[i:end_idx] * 2.0

            signals[:, ch] = base + noise

        return self._normalize_emg(signals)

    def _normalize_emg(self, data: np.ndarray) -> np.ndarray:
        """Normalize EMG signals to [-1, 1] range"""
        for ch in range(data.shape[1]):
            ch_data = data[:, ch]
            max_val = np.abs(ch_data).max()
            if max_val > 0:
                data[:, ch] = ch_data / max_val
        return data

    def extract_features(self, emg_window: np.ndarray) -> Dict[str, float]:
        """
        Extract features from EMG window for gesture recognition

        Args:
            emg_window: EMG data window [samples x channels]

        Returns:
            Dictionary of extracted features
        """
        features = {}

        for ch in range(emg_window.shape[1]):
            ch_data = emg_window[:, ch]

            # Time-domain features
            features[f'ch{ch}_mav'] = np.mean(np.abs(ch_data))  # Mean Absolute Value
            features[f'ch{ch}_rms'] = np.sqrt(np.mean(ch_data**2))  # RMS
            features[f'ch{ch}_var'] = np.var(ch_data)  # Variance
            features[f'ch{ch}_zc'] = self._zero_crossings(ch_data)  # Zero crossings

            # Frequency-domain features (simplified)
            fft = np.fft.rfft(ch_data)
            features[f'ch{ch}_freq_mean'] = np.mean(np.abs(fft))
            features[f'ch{ch}_freq_max'] = np.max(np.abs(fft))

        return features

    def _zero_crossings(self, signal: np.ndarray) -> int:
        """Count zero crossings in signal"""
        return np.sum(np.diff(np.sign(signal)) != 0)

    def classify_gesture(self, features: Dict[str, float]) -> Tuple[str, float]:
        """
        Classify gesture from EMG features (simplified classifier)

        Args:
            features: Extracted EMG features

        Returns:
            Tuple of (gesture_type, confidence)
        """
        # Simple rule-based classifier (in production, use trained ML model)
        avg_mav = np.mean([v for k, v in features.items() if 'mav' in k])
        avg_rms = np.mean([v for k, v in features.items() if 'rms' in k])

        # Apply radiation degradation to confidence
        radiation_factor = 1.0
        if self.radiation_environment:
            radiation_factor = 1.0 - min(self.radiation_dose / 1000.0, 0.5)

        if avg_mav < 0.1:
            return "rest", 0.95 * radiation_factor
        elif avg_rms > 0.6:
            return "power_grip", 0.85 * radiation_factor
        elif avg_mav > 0.4:
            return "hand_close", 0.80 * radiation_factor
        elif avg_mav > 0.2:
            return "hand_open", 0.75 * radiation_factor
        else:
            return "wrist_flexion", 0.70 * radiation_factor

    def execute_gesture(self,
                       emg_signal: np.ndarray,
                       context: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Execute gesture recognition and control with LAM integration

        Args:
            emg_signal: EMG signal window [samples x channels]
            context: Optional context including confidence, radiation_dose

        Returns:
            Execution result with gesture and control parameters
        """
        if context is None:
            context = {}

        # Update radiation dose if provided
        if 'radiation_dose' in context:
            self.radiation_dose = context['radiation_dose']

        # Apply radiation noise
        if self.radiation_environment:
            emg_signal = self._apply_radiation_noise(emg_signal)

        # Extract features
        features = self.extract_features(emg_signal)

        # Classify gesture
        gesture_type, confidence = self.classify_gesture(features)

        # Apply LAM temporal displacement for trust-gated control
        base_confidence = context.get('confidence', confidence)
        displacement = self._compute_temporal_displacement(base_confidence)

        # Create action
        action = GestureAction(
            gesture_type=gesture_type,
            confidence=confidence,
            emg_features=features,
            timestamp=datetime.now().timestamp(),
            radiation_affected=(self.radiation_environment is not None)
        )

        self.action_history.append(action)

        # Compute quantum resonance field update
        resonance = self._update_resonance_field(action)

        result = {
            "success": True,
            "gesture": gesture_type,
            "confidence": float(confidence),
            "temporal_displacement": float(displacement),
            "quantum_resonance": float(resonance),
            "radiation_dose": float(self.radiation_dose),
            "features": {k: float(v) for k, v in features.items()},
            "timestamp": action.timestamp
        }

        return result

    def _apply_radiation_noise(self, signal: np.ndarray) -> np.ndarray:
        """Apply radiation-induced noise to EMG signal"""
        noise = np.random.randn(*signal.shape) * self.noise_floor

        # Simulate Single Event Effects (SEE) randomly
        if np.random.rand() < self.see_rate / 3600.0:  # Per sample probability
            # Inject a transient spike
            spike_idx = np.random.randint(0, signal.shape[0])
            spike_ch = np.random.randint(0, signal.shape[1])
            signal[spike_idx, spike_ch] += np.random.randn() * 5.0

        return signal + noise

    def _compute_temporal_displacement(self, confidence: float) -> float:
        """
        Compute LAM temporal displacement based on confidence
        Trust-gated displacement: low confidence → higher displacement

        Args:
            confidence: Action confidence [0, 1]

        Returns:
            Temporal displacement in seconds
        """
        # Base displacement
        base_delta = 0.05  # 50ms base delay

        # Trust-gated: lower confidence increases displacement
        trust_factor = 1.0 - confidence
        delta = base_delta * (1.0 + trust_factor * 2.0)

        # Add radiation-induced delay
        if self.radiation_environment:
            radiation_delay = self.radiation_dose / 1000.0 * 0.02  # Up to 20ms per Sv
            delta += radiation_delay

        return delta

    def _update_resonance_field(self, action: GestureAction) -> float:
        """
        Update quantum resonance field based on action
        Uses Primal Logic exponential decay

        Args:
            action: Gesture action

        Returns:
            Resonance field value
        """
        # Compute resonance based on recent actions
        if len(self.action_history) == 0:
            return 1.0

        # Exponential memory weighting
        resonance = 0.0
        current_time = action.timestamp

        for i, hist_action in enumerate(reversed(self.action_history[-10:])):
            time_diff = current_time - hist_action.timestamp
            weight = np.exp(-self.lambda_decay * time_diff)
            resonance += hist_action.confidence * weight

        # Normalize
        resonance = resonance / min(len(self.action_history), 10)

        # Apply alpha temporal weighting
        resonance = self.alpha * resonance

        return resonance

    def get_statistics(self) -> Dict[str, Any]:
        """Get prosthetic control statistics"""
        if len(self.action_history) == 0:
            return {"error": "No actions recorded"}

        gestures = [a.gesture_type for a in self.action_history]
        confidences = [a.confidence for a in self.action_history]

        return {
            "total_actions": len(self.action_history),
            "unique_gestures": len(set(gestures)),
            "gesture_distribution": {g: gestures.count(g) for g in set(gestures)},
            "average_confidence": float(np.mean(confidences)),
            "min_confidence": float(np.min(confidences)),
            "max_confidence": float(np.max(confidences)),
            "radiation_dose_msv": float(self.radiation_dose),
            "radiation_affected_actions": sum(1 for a in self.action_history if a.radiation_affected)
        }


def demo_prosthetics_integration():
    """Demonstration of prosthetics integration"""
    print("=" * 60)
    print("MotorHandPro - Prosthetics Integration Demo")
    print("=" * 60)

    # Test 1: Normal operation
    print("\n[Test 1] Normal operation (no radiation)")
    controller = ProstheticsController(
        dataset="emg_gesture_recognition_2024",
        radiation_environment=None,
        num_channels=8
    )

    # Load synthetic data
    emg_data = controller.load_dataset()

    # Execute gestures
    for i in range(0, len(emg_data) - controller.window_samples, controller.window_samples):
        window = emg_data[i:i + controller.window_samples]
        result = controller.execute_gesture(window)
        print(f"  Gesture: {result['gesture']:15s} | Confidence: {result['confidence']:.3f} | Resonance: {result['quantum_resonance']:.3f}")

        if i >= controller.window_samples * 5:  # Show first 5
            break

    stats = controller.get_statistics()
    print(f"\n  Statistics: {stats['total_actions']} actions, avg confidence: {stats['average_confidence']:.3f}")

    # Test 2: Mars transit environment
    print("\n[Test 2] Mars transit environment (radiation)")
    controller_mars = ProstheticsController(
        dataset="emg_gesture_recognition_2024",
        radiation_environment="mars_transit",
        num_channels=8
    )

    emg_data_mars = controller_mars.load_dataset()

    # Simulate mission with increasing radiation dose
    for dose, i in zip([0, 50, 100, 150, 200],
                       range(0, min(len(emg_data_mars), controller_mars.window_samples * 5),
                             controller_mars.window_samples)):
        window = emg_data_mars[i:i + controller_mars.window_samples]
        result = controller_mars.execute_gesture(
            window,
            context={"radiation_dose": dose}
        )
        print(f"  Dose: {dose:3d} mSv | Gesture: {result['gesture']:15s} | Confidence: {result['confidence']:.3f} | Displacement: {result['temporal_displacement']:.4f}s")

    stats_mars = controller_mars.get_statistics()
    print(f"\n  Statistics: {stats_mars['total_actions']} actions, avg confidence: {stats_mars['average_confidence']:.3f}")
    print(f"  Total radiation dose: {stats_mars['radiation_dose_msv']:.1f} mSv")

    print("\n" + "=" * 60)
    print("Demo complete!")
    print("=" * 60)


if __name__ == "__main__":
    demo_prosthetics_integration()
