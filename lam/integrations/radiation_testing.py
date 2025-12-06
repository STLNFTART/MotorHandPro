#!/usr/bin/env python3
"""
Radiation Testing Framework for Prosthetic Effectiveness in Space Environments
Based on NASA radiation testing standards and space environment models
"""
import sys
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple
from datetime import datetime, timedelta
from dataclasses import dataclass
from enum import Enum

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))


class RadiationSource(Enum):
    """Types of space radiation sources"""
    GCR = "galactic_cosmic_rays"  # Continuous background
    SPE = "solar_particle_event"  # Acute bursts
    TRAPPED = "trapped_radiation"  # Van Allen belts


class SpaceEnvironment(Enum):
    """Space environment types"""
    LEO = "low_earth_orbit"  # ISS altitude
    GEO = "geostationary_orbit"
    LUNAR = "lunar_surface"
    MARS_TRANSIT = "mars_transit"
    MARS_SURFACE = "mars_surface"


@dataclass
class RadiationProfile:
    """Radiation environment profile"""
    environment: SpaceEnvironment
    gcr_dose_rate: float  # mSv/day (continuous)
    spe_frequency: float  # Events per year
    spe_avg_dose: float  # mSv per event
    trapped_dose_rate: float  # mSv/day (if applicable)
    shield_thickness: float  # g/cm² aluminum equivalent


@dataclass
class RadiationEvent:
    """Single radiation event"""
    timestamp: datetime
    source: RadiationSource
    dose: float  # mSv
    duration: timedelta
    particle_flux: float  # particles/cm²/s


@dataclass
class EffectivenessMetrics:
    """Prosthetic effectiveness metrics"""
    task_completion_rate: float  # Percentage [0, 100]
    control_accuracy: float  # Percentage [0, 100]
    response_time_ms: float  # Milliseconds
    mtbf_hours: float  # Mean time between failures
    safe_mode_activations: int
    see_events: int  # Single Event Effects
    total_dose_msv: float


class RadiationSimulator:
    """
    Simulates space radiation effects on prosthetic control systems
    Based on NASA testing standards
    """

    # Radiation profiles for different environments
    RADIATION_PROFILES = {
        SpaceEnvironment.LEO: RadiationProfile(
            environment=SpaceEnvironment.LEO,
            gcr_dose_rate=0.15,  # mSv/day (ISS typical)
            spe_frequency=5,  # Events per year
            spe_avg_dose=20,  # mSv per event
            trapped_dose_rate=0.05,  # Some trapped radiation
            shield_thickness=5.0  # Light shielding
        ),
        SpaceEnvironment.GEO: RadiationProfile(
            environment=SpaceEnvironment.GEO,
            gcr_dose_rate=0.25,
            spe_frequency=8,
            spe_avg_dose=30,
            trapped_dose_rate=0.20,  # Significant trapped radiation
            shield_thickness=10.0
        ),
        SpaceEnvironment.LUNAR: RadiationProfile(
            environment=SpaceEnvironment.LUNAR,
            gcr_dose_rate=0.35,  # No magnetic field protection
            spe_frequency=10,
            spe_avg_dose=50,
            trapped_dose_rate=0.0,  # No trapped radiation
            shield_thickness=10.0  # Habitat shielding
        ),
        SpaceEnvironment.MARS_TRANSIT: RadiationProfile(
            environment=SpaceEnvironment.MARS_TRANSIT,
            gcr_dose_rate=0.50,  # Deep space
            spe_frequency=12,
            spe_avg_dose=80,
            trapped_dose_rate=0.0,
            shield_thickness=15.0  # Spacecraft shielding
        ),
        SpaceEnvironment.MARS_SURFACE: RadiationProfile(
            environment=SpaceEnvironment.MARS_SURFACE,
            gcr_dose_rate=0.30,  # Thin atmosphere provides some protection
            spe_frequency=8,
            spe_avg_dose=60,
            trapped_dose_rate=0.0,
            shield_thickness=20.0  # Habitat + regolith
        )
    }

    def __init__(self,
                 environment: str = "mars_surface",
                 shield_thickness: Optional[float] = None,
                 mission_duration: int = 500):
        """
        Initialize radiation simulator

        Args:
            environment: Space environment type
            shield_thickness: Override shielding thickness (g/cm²)
            mission_duration: Mission duration in days
        """
        # Parse environment
        env_map = {
            "leo": SpaceEnvironment.LEO,
            "geo": SpaceEnvironment.GEO,
            "lunar": SpaceEnvironment.LUNAR,
            "lunar_surface": SpaceEnvironment.LUNAR,
            "mars_transit": SpaceEnvironment.MARS_TRANSIT,
            "mars_surface": SpaceEnvironment.MARS_SURFACE
        }

        self.environment = env_map.get(environment.lower(), SpaceEnvironment.MARS_SURFACE)
        self.profile = self.RADIATION_PROFILES[self.environment]

        # Override shielding if specified
        if shield_thickness is not None:
            self.profile.shield_thickness = shield_thickness

        self.mission_duration = mission_duration
        self.radiation_events: List[RadiationEvent] = []
        self.cumulative_dose = 0.0

        # Generate radiation event timeline
        self._generate_radiation_timeline()

        print(f"RadiationSimulator initialized:")
        print(f"  Environment: {self.environment.value}")
        print(f"  Mission duration: {mission_duration} days")
        print(f"  Shielding: {self.profile.shield_thickness} g/cm²")
        print(f"  Expected dose: {self.cumulative_dose:.1f} mSv")

    def _generate_radiation_timeline(self):
        """Generate timeline of radiation events for mission"""
        start_time = datetime.now()

        # Generate GCR background (continuous)
        for day in range(self.mission_duration):
            event = RadiationEvent(
                timestamp=start_time + timedelta(days=day),
                source=RadiationSource.GCR,
                dose=self.profile.gcr_dose_rate * self._shielding_attenuation(RadiationSource.GCR),
                duration=timedelta(days=1),
                particle_flux=100.0  # particles/cm²/s typical for GCR
            )
            self.radiation_events.append(event)
            self.cumulative_dose += event.dose

        # Generate SPE events (stochastic)
        expected_spe = self.profile.spe_frequency * (self.mission_duration / 365.0)
        num_spe = np.random.poisson(expected_spe)

        for _ in range(num_spe):
            spe_day = np.random.uniform(0, self.mission_duration)
            spe_duration = np.random.uniform(6, 48)  # Hours
            spe_dose = np.random.lognormal(
                np.log(self.profile.spe_avg_dose),
                0.5  # Log-normal variance
            ) * self._shielding_attenuation(RadiationSource.SPE)

            event = RadiationEvent(
                timestamp=start_time + timedelta(days=spe_day),
                source=RadiationSource.SPE,
                dose=spe_dose,
                duration=timedelta(hours=spe_duration),
                particle_flux=10000.0  # Much higher flux during SPE
            )
            self.radiation_events.append(event)
            self.cumulative_dose += event.dose

        # Generate trapped radiation (if applicable)
        if self.profile.trapped_dose_rate > 0:
            for day in range(self.mission_duration):
                event = RadiationEvent(
                    timestamp=start_time + timedelta(days=day),
                    source=RadiationSource.TRAPPED,
                    dose=self.profile.trapped_dose_rate * self._shielding_attenuation(RadiationSource.TRAPPED),
                    duration=timedelta(days=1),
                    particle_flux=500.0  # Intermediate flux
                )
                self.radiation_events.append(event)
                self.cumulative_dose += event.dose

        # Sort events by timestamp
        self.radiation_events.sort(key=lambda e: e.timestamp)

    def _shielding_attenuation(self, source: RadiationSource) -> float:
        """
        Calculate shielding attenuation factor

        Args:
            source: Radiation source type

        Returns:
            Attenuation factor [0, 1]
        """
        # Simplified exponential attenuation model
        # Real implementation would use particle transport codes (GEANT4, etc.)

        thickness = self.profile.shield_thickness

        if source == RadiationSource.GCR:
            # GCR: High energy, harder to shield
            half_value_layer = 50.0  # g/cm²
            attenuation = np.exp(-0.693 * thickness / half_value_layer)
            return max(0.5, attenuation)  # Min 50% dose even with heavy shielding

        elif source == RadiationSource.SPE:
            # SPE: Lower energy, easier to shield
            half_value_layer = 15.0  # g/cm²
            attenuation = np.exp(-0.693 * thickness / half_value_layer)
            return attenuation

        elif source == RadiationSource.TRAPPED:
            # Trapped: Varies, moderate shielding effectiveness
            half_value_layer = 25.0  # g/cm²
            attenuation = np.exp(-0.693 * thickness / half_value_layer)
            return attenuation

        return 1.0

    def test_prosthetic_effectiveness(self,
                                     prosthetic: Any,
                                     test_suite: str = "fine_motor_tasks",
                                     radiation_profile: str = "realistic_spe",
                                     num_trials: int = 100) -> EffectivenessMetrics:
        """
        Test prosthetic effectiveness under radiation

        Args:
            prosthetic: Prosthetic controller instance
            test_suite: Test suite name
            radiation_profile: Radiation scenario
            num_trials: Number of test trials

        Returns:
            Effectiveness metrics
        """
        print(f"\nTesting prosthetic effectiveness:")
        print(f"  Test suite: {test_suite}")
        print(f"  Radiation profile: {radiation_profile}")
        print(f"  Trials: {num_trials}")

        # Initialize metrics
        successful_tasks = 0
        total_accuracy = 0.0
        total_response_time = 0.0
        failures = 0
        see_events = 0
        safe_mode_count = 0

        # Simulate test trials
        for trial in range(num_trials):
            # Get current radiation dose
            trial_time = (trial / num_trials) * self.mission_duration
            dose_at_time = self._get_dose_at_time(trial_time)

            # Simulate Single Event Effect
            see_probability = self._compute_see_probability(trial_time)
            if np.random.rand() < see_probability:
                see_events += 1
                # SEE causes task failure
                failures += 1
                continue

            # Test task execution
            success, accuracy, response_time = self._simulate_task_execution(
                prosthetic, dose_at_time, test_suite
            )

            if success:
                successful_tasks += 1
                total_accuracy += accuracy
                total_response_time += response_time
            else:
                failures += 1

                # Check if safe mode activated
                if dose_at_time > 200:  # High dose threshold
                    safe_mode_count += 1

        # Calculate metrics
        completion_rate = (successful_tasks / num_trials) * 100
        avg_accuracy = (total_accuracy / max(successful_tasks, 1)) * 100
        avg_response_time = total_response_time / max(successful_tasks, 1)

        # Calculate MTBF
        mission_hours = self.mission_duration * 24
        failure_rate = failures / mission_hours if failures > 0 else 0
        mtbf = 1.0 / failure_rate if failure_rate > 0 else mission_hours * 10

        metrics = EffectivenessMetrics(
            task_completion_rate=completion_rate,
            control_accuracy=avg_accuracy,
            response_time_ms=avg_response_time,
            mtbf_hours=mtbf,
            safe_mode_activations=safe_mode_count,
            see_events=see_events,
            total_dose_msv=self.cumulative_dose
        )

        return metrics

    def _get_dose_at_time(self, time_days: float) -> float:
        """Get cumulative dose at specific time"""
        dose = 0.0
        for event in self.radiation_events:
            event_day = (event.timestamp - self.radiation_events[0].timestamp).total_seconds() / 86400.0
            if event_day <= time_days:
                dose += event.dose
        return dose

    def _compute_see_probability(self, time_days: float) -> float:
        """Compute Single Event Effect probability at given time"""
        # Check for SPE events at this time
        for event in self.radiation_events:
            event_day = (event.timestamp - self.radiation_events[0].timestamp).total_seconds() / 86400.0

            if event.source == RadiationSource.SPE and abs(event_day - time_days) < 1.0:
                # During SPE, increased SEE probability
                return 0.01  # 1% per task

        # Background SEE rate
        return 0.0001  # 0.01% per task

    def _simulate_task_execution(self,
                                prosthetic: Any,
                                dose: float,
                                test_suite: str) -> Tuple[bool, float, float]:
        """
        Simulate single task execution

        Args:
            prosthetic: Prosthetic controller
            dose: Cumulative radiation dose (mSv)
            test_suite: Test suite name

        Returns:
            Tuple of (success, accuracy, response_time_ms)
        """
        # Radiation degradation model
        # Higher dose → lower performance

        # Base performance (no radiation)
        base_accuracy = 0.95
        base_response_time = 150.0  # ms

        # Degradation factors
        dose_factor = 1.0 / (1.0 + dose / 1000.0)  # Sigmoid-like degradation

        # Task difficulty modifiers
        difficulty = {
            "fine_motor_tasks": 1.2,
            "gross_motor_tasks": 0.8,
            "precision_tasks": 1.5,
            "adaptive_tasks": 1.1
        }
        task_difficulty = difficulty.get(test_suite, 1.0)

        # Compute metrics
        accuracy = base_accuracy * dose_factor / task_difficulty
        response_time = base_response_time * (2.0 - dose_factor) * task_difficulty

        # Add noise
        accuracy += np.random.normal(0, 0.05)
        response_time += np.random.normal(0, 20)

        # Clamp values
        accuracy = np.clip(accuracy, 0.0, 1.0)
        response_time = max(50, response_time)

        # Success threshold
        success = accuracy > 0.5 and response_time < 500

        return success, accuracy, response_time

    def generate_report(self, metrics: EffectivenessMetrics) -> str:
        """Generate radiation testing report"""
        report = f"""
{'='*70}
RADIATION TESTING REPORT
{'='*70}

ENVIRONMENT:
  Type: {self.environment.value}
  Mission Duration: {self.mission_duration} days
  Shielding: {self.profile.shield_thickness} g/cm² aluminum equivalent

RADIATION EXPOSURE:
  Total Dose: {metrics.total_dose_msv:.1f} mSv
  GCR Dose Rate: {self.profile.gcr_dose_rate:.3f} mSv/day
  SPE Events: {sum(1 for e in self.radiation_events if e.source == RadiationSource.SPE)}
  SEE Events: {metrics.see_events}

PROSTHETIC EFFECTIVENESS:
  Task Completion Rate: {metrics.task_completion_rate:.1f}%
  Control Accuracy: {metrics.control_accuracy:.1f}%
  Average Response Time: {metrics.response_time_ms:.1f} ms
  MTBF: {metrics.mtbf_hours:.1f} hours
  Safe Mode Activations: {metrics.safe_mode_activations}

RADIATION LIMITS (NASA):
  Annual Limit: 500 mSv/year
  Career Limit: 1000-4000 mSv (age/gender dependent)
  Current Dose: {metrics.total_dose_msv:.1f} mSv
  Percentage of Annual: {(metrics.total_dose_msv / 500 * 100):.1f}%

CONCLUSION:
  {'PASS' if metrics.task_completion_rate > 80 and metrics.total_dose_msv < 500 else 'MARGINAL' if metrics.task_completion_rate > 60 else 'FAIL'}
  - Radiation dose {'within' if metrics.total_dose_msv < 500 else 'EXCEEDS'} NASA limits
  - Task performance {'acceptable' if metrics.task_completion_rate > 80 else 'degraded'}
  - {'Suitable' if metrics.task_completion_rate > 80 and metrics.total_dose_msv < 500 else 'Requires improvement'} for space missions

{'='*70}
"""
        return report


def demo_radiation_testing():
    """Demonstration of radiation testing"""
    print("=" * 70)
    print("MotorHandPro - Radiation Testing Demo")
    print("=" * 70)

    # Import prosthetics controller
    try:
        from prosthetics_integration import ProstheticsController
    except ImportError:
        print("Note: Using mock prosthetic controller for demo")
        ProstheticsController = None

    # Test multiple environments
    environments = [
        ("leo", "ISS Mission"),
        ("mars_transit", "Mars Transit"),
        ("mars_surface", "Mars Surface Base")
    ]

    for env, description in environments:
        print(f"\n{'='*70}")
        print(f"Testing: {description}")
        print('='*70)

        # Create radiation simulator
        sim = RadiationSimulator(
            environment=env,
            mission_duration=500 if "mars" in env else 180
        )

        # Create prosthetic controller (mock if not available)
        if ProstheticsController:
            prosthetic = ProstheticsController(
                radiation_environment=env,
                num_channels=8
            )
        else:
            prosthetic = None  # Mock

        # Run effectiveness test
        metrics = sim.test_prosthetic_effectiveness(
            prosthetic=prosthetic,
            test_suite="fine_motor_tasks",
            num_trials=100
        )

        # Generate and print report
        report = sim.generate_report(metrics)
        print(report)

    print("\n" + "=" * 70)
    print("Demo complete!")
    print("=" * 70)


if __name__ == "__main__":
    demo_radiation_testing()
