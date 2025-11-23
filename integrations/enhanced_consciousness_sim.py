#!/usr/bin/env python3
"""
Enhanced Consciousness Simulation with Explicit Adaptation Logging

Extends the crew health monitoring simulation to explicitly track and log:
- Consciousness level evolution over mission time
- φ-scaled threshold adjustments in real-time
- Bounded drift calculations via λ exponential decay
- Adaptive learning triggers and responses
- Consciousness learning curves per crew member

This simulation provides full observability into PRIMAL Logic adaptation dynamics.
"""

import random
import math
import time
from dataclasses import dataclass
from typing import List, Dict, Tuple
from datetime import datetime, timedelta

# PRIMAL Logic constants
PHI = 1.618033988749  # Golden ratio
PHI_SQUARED = PHI ** 2  # 2.618... for IMMEDIATE intervention threshold
LAMBDA_LIGHTFOOT = 0.16905  # Lightfoot constant for bounded drift

# Baseline health parameters
BASELINE_HBC = 15.0  # g/dL hemoglobin
BASELINE_TREMOR = 0.45  # amplitude units
BASELINE_GRIP = 45.0  # kg
BASELINE_REACTION = 250.0  # ms
BASELINE_COGNITIVE = 95.0  # score

# Degradation rates (per day)
RADIATION_HBC_DEGRADATION = 0.0003  # per mSv
ISOLATION_TREMOR_INCREASE = 0.0008  # per day


@dataclass
class CrewMember:
    """Enhanced crew member with consciousness tracking"""
    crew_id: str
    consciousness_target: float  # Target consciousness level (0.5-0.9)
    consciousness_current: float  # Current adapted consciousness
    consciousness_history: List[Tuple[int, float]]  # (day, consciousness) history

    # Adaptive learning parameters
    learning_rate: float = 0.05  # How fast consciousness adapts
    adaptation_threshold: float = 0.1  # Trigger adaptation when variance exceeds this

    # Health baselines
    baseline_hbc: float = BASELINE_HBC
    baseline_tremor: float = BASELINE_TREMOR
    baseline_grip: float = BASELINE_GRIP
    baseline_reaction: float = BASELINE_REACTION
    baseline_cognitive: float = BASELINE_COGNITIVE

    # Variance tracking for adaptation
    tremor_variance_window: List[float] = None
    window_size: int = 10

    def __post_init__(self):
        self.consciousness_current = self.consciousness_target
        self.consciousness_history = [(0, self.consciousness_current)]
        self.tremor_variance_window = []

    def calculate_phi_threshold(self, base_threshold: float) -> float:
        """Calculate φ-scaled effective threshold

        σ_eff = σ_base / (consciousness × φ)
        """
        return base_threshold / (self.consciousness_current * PHI)

    def calculate_bounded_drift(self, drift: float, mission_day: int) -> float:
        """Calculate bounded drift using Lightfoot constant

        drift_bounded = drift × e^(-λt)
        """
        return drift * math.exp(-LAMBDA_LIGHTFOOT * mission_day)

    def update_consciousness(self, tremor_amplitude: float, mission_day: int) -> Dict:
        """Update consciousness based on tremor variance

        Returns adaptation event details if consciousness changed
        """
        self.tremor_variance_window.append(tremor_amplitude)

        # Keep only recent window
        if len(self.tremor_variance_window) > self.window_size:
            self.tremor_variance_window.pop(0)

        # Need at least window_size samples to adapt
        if len(self.tremor_variance_window) < self.window_size:
            return None

        # Calculate coefficient of variation
        mean_tremor = sum(self.tremor_variance_window) / len(self.tremor_variance_window)
        variance = sum((x - mean_tremor) ** 2 for x in self.tremor_variance_window) / len(self.tremor_variance_window)
        std_tremor = math.sqrt(variance)
        cv = (std_tremor / mean_tremor) if mean_tremor > 0 else 0

        # Check if adaptation needed
        old_consciousness = self.consciousness_current
        adaptation_event = None

        if cv > self.adaptation_threshold:
            # High variance → Increase consciousness for tighter control
            adjustment = self.learning_rate * (cv - self.adaptation_threshold)
            self.consciousness_current = min(0.95, self.consciousness_current + adjustment)

            adaptation_event = {
                'day': mission_day,
                'trigger': 'HIGH_VARIANCE',
                'cv': cv,
                'old_consciousness': old_consciousness,
                'new_consciousness': self.consciousness_current,
                'adjustment': adjustment
            }
        elif cv < self.adaptation_threshold / 2 and self.consciousness_current > self.consciousness_target:
            # Low variance → Can reduce consciousness (energy conservation)
            adjustment = -self.learning_rate * (self.adaptation_threshold - cv)
            self.consciousness_current = max(self.consciousness_target, self.consciousness_current + adjustment)

            adaptation_event = {
                'day': mission_day,
                'trigger': 'LOW_VARIANCE',
                'cv': cv,
                'old_consciousness': old_consciousness,
                'new_consciousness': self.consciousness_current,
                'adjustment': adjustment
            }

        # Log consciousness history
        if old_consciousness != self.consciousness_current:
            self.consciousness_history.append((mission_day, self.consciousness_current))

        return adaptation_event


@dataclass
class HealthMetrics:
    """Current health state"""
    hbc: float
    tremor_amplitude: float
    tremor_frequency: float
    grip_strength: float
    reaction_time: float
    cognitive_score: float
    cumulative_radiation: float
    current_dose_rate: float
    cortisol: float
    hrv: float


class EnhancedSimulation:
    """Enhanced simulation with consciousness tracking"""

    def __init__(self):
        self.crew: List[CrewMember] = []
        self.spe_events: List[Tuple[int, float, str]] = []  # (day, dose_mSv, description)
        self.mission_days = 0
        self.current_day = 0

        # Logging
        self.daily_logs: List[Dict] = []
        self.adaptation_events: List[Dict] = []
        self.threshold_logs: List[Dict] = []

    def add_crew_member(self, crew_id: str, consciousness_target: float, learning_rate: float = 0.05):
        """Add crew member with specified consciousness target"""
        member = CrewMember(
            crew_id=crew_id,
            consciousness_target=consciousness_target,
            consciousness_current=consciousness_target,
            consciousness_history=[],
            learning_rate=learning_rate
        )
        self.crew.append(member)
        print(f"✅ Added {crew_id} (consciousness target: {consciousness_target:.2f}, learning rate: {learning_rate:.3f})")

    def schedule_spe(self, day: int, dose_msv: float, description: str):
        """Schedule a Solar Particle Event"""
        self.spe_events.append((day, dose_msv, description))
        print(f"📅 Scheduled SPE: Day {day}, {dose_msv} mSv ({description})")

    def get_spe_dose(self, day: int) -> float:
        """Get SPE dose for given day"""
        for spe_day, dose, _ in self.spe_events:
            if spe_day == day:
                return dose
        return 0.0

    def simulate_health_metrics(self, crew: CrewMember, mission_day: int, cumulative_radiation: float) -> HealthMetrics:
        """Simulate health metrics with realistic degradation"""

        # HBC degradation from radiation
        hbc = crew.baseline_hbc - (cumulative_radiation * RADIATION_HBC_DEGRADATION)
        hbc += random.gauss(0, 0.3)  # Daily variance

        # Tremor increase from isolation + radiation stress
        isolation_factor = mission_day * ISOLATION_TREMOR_INCREASE
        radiation_factor = (cumulative_radiation / 1000) * 0.05
        tremor = crew.baseline_tremor + isolation_factor + radiation_factor
        tremor += random.gauss(0, 0.05)  # Noise
        tremor = max(0.1, tremor)  # Prevent negative

        # Apply consciousness-based control (lower consciousness → less control)
        consciousness_control = crew.consciousness_current
        tremor = tremor * (1.5 - consciousness_control)  # Higher consciousness reduces tremor

        # Other metrics
        tremor_freq = 4.0 + random.gauss(0, 0.5)
        grip = crew.baseline_grip - (mission_day * 0.02) - (cumulative_radiation / 100)
        grip += random.gauss(0, 2.0)

        reaction = crew.baseline_reaction + (mission_day * 0.3) + (cumulative_radiation / 50)
        reaction += random.gauss(0, 10)

        cognitive = crew.baseline_cognitive - (mission_day * 0.03) - (cumulative_radiation / 200)
        cognitive += random.gauss(0, 2)

        # Stress markers
        cortisol = 15 + (mission_day * 0.05) + (cumulative_radiation / 300)
        cortisol += random.gauss(0, 2)

        hrv = 50 - (mission_day * 0.02) - (cumulative_radiation / 400)
        hrv += random.gauss(0, 5)

        current_dose_rate = self.get_spe_dose(mission_day)

        return HealthMetrics(
            hbc=hbc,
            tremor_amplitude=tremor,
            tremor_frequency=tremor_freq,
            grip_strength=max(0, grip),
            reaction_time=max(100, reaction),
            cognitive_score=max(0, min(100, cognitive)),
            cumulative_radiation=cumulative_radiation,
            current_dose_rate=current_dose_rate,
            cortisol=max(0, cortisol),
            hrv=max(0, hrv)
        )

    def calculate_thresholds(self, crew: CrewMember, mission_day: int) -> Dict:
        """Calculate adaptive thresholds for crew member"""

        # Base thresholds (% deviation from baseline)
        base_hbc_threshold = 5.0  # 5% HBC drop
        base_tremor_threshold = 10.0  # 10% tremor increase

        # φ-scaled effective thresholds
        eff_hbc_threshold = crew.calculate_phi_threshold(base_hbc_threshold)
        eff_tremor_threshold = crew.calculate_phi_threshold(base_tremor_threshold)

        # Bounded drift
        hbc_drift = 8.0  # Expected 8% HBC drop by day 120
        tremor_drift = 15.0  # Expected 15% tremor increase by day 120

        hbc_drift_bounded = crew.calculate_bounded_drift(hbc_drift, mission_day)
        tremor_drift_bounded = crew.calculate_bounded_drift(tremor_drift, mission_day)

        return {
            'mission_day': mission_day,
            'crew_id': crew.crew_id,
            'consciousness': crew.consciousness_current,
            'base_hbc_threshold': base_hbc_threshold,
            'eff_hbc_threshold': eff_hbc_threshold,
            'base_tremor_threshold': base_tremor_threshold,
            'eff_tremor_threshold': eff_tremor_threshold,
            'hbc_drift_bounded': hbc_drift_bounded,
            'tremor_drift_bounded': tremor_drift_bounded,
            'phi': PHI,
            'lambda': LAMBDA_LIGHTFOOT
        }

    def run_simulation(self, mission_days: int):
        """Run enhanced simulation with consciousness tracking"""
        self.mission_days = mission_days

        print("\n" + "=" * 100)
        print("🚀 STARTING ENHANCED CONSCIOUSNESS SIMULATION")
        print("=" * 100)
        print(f"   Mission Duration: {mission_days} days")
        print(f"   Crew Members: {len(self.crew)}")
        print(f"   SPE Events: {len(self.spe_events)}")
        print()

        # Initialize crew radiation tracking
        crew_radiation = {crew.crew_id: 0.0 for crew in self.crew}

        for day in range(mission_days):
            self.current_day = day

            # Check for SPE
            spe_dose = self.get_spe_dose(day)
            if spe_dose > 0:
                spe_event = [e for e in self.spe_events if e[0] == day][0]
                print(f"\n☀️  Day {day}: SPE EVENT - {spe_dose} mSv ({spe_event[2]})")

            # Simulate each crew member
            for crew in self.crew:
                # Update cumulative radiation
                crew_radiation[crew.crew_id] += spe_dose

                # Simulate health metrics
                metrics = self.simulate_health_metrics(crew, day, crew_radiation[crew.crew_id])

                # Update consciousness based on tremor variance
                adaptation_event = crew.update_consciousness(metrics.tremor_amplitude, day)

                if adaptation_event:
                    self.adaptation_events.append(adaptation_event)
                    print(f"   🧠 {crew.crew_id}: Consciousness {adaptation_event['old_consciousness']:.3f} → "
                          f"{adaptation_event['new_consciousness']:.3f} (trigger: {adaptation_event['trigger']}, CV: {adaptation_event['cv']:.3f})")

                # Calculate thresholds
                thresholds = self.calculate_thresholds(crew, day)
                self.threshold_logs.append(thresholds)

                # Log daily data
                log_entry = {
                    'crew_id': crew.crew_id,
                    'mission_day': day,
                    'consciousness': crew.consciousness_current,
                    'consciousness_target': crew.consciousness_target,
                    'hbc': metrics.hbc,
                    'tremor_amplitude': metrics.tremor_amplitude,
                    'tremor_frequency': metrics.tremor_frequency,
                    'grip_strength': metrics.grip_strength,
                    'reaction_time': metrics.reaction_time,
                    'cognitive_score': metrics.cognitive_score,
                    'cumulative_radiation': metrics.cumulative_radiation,
                    'current_dose_rate': metrics.current_dose_rate,
                    'cortisol': metrics.cortisol,
                    'hrv': metrics.hrv,
                    'eff_hbc_threshold': thresholds['eff_hbc_threshold'],
                    'eff_tremor_threshold': thresholds['eff_tremor_threshold'],
                    'hbc_drift_bounded': thresholds['hbc_drift_bounded'],
                    'tremor_drift_bounded': thresholds['tremor_drift_bounded']
                }

                self.daily_logs.append(log_entry)

        print("\n" + "=" * 100)
        print("✅ SIMULATION COMPLETE")
        print("=" * 100)
        print()

    def export_to_csv(self, filename: str):
        """Export simulation data to CSV"""
        import csv

        with open(filename, 'w', newline='') as f:
            if not self.daily_logs:
                print("⚠️  No data to export")
                return

            fieldnames = list(self.daily_logs[0].keys())
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(self.daily_logs)

        print(f"✅ Exported {len(self.daily_logs)} data points to {filename}")

    def export_consciousness_curves(self, filename: str):
        """Export consciousness evolution curves"""
        import csv

        with open(filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['crew_id', 'mission_day', 'consciousness', 'consciousness_target'])

            for crew in self.crew:
                for day, consciousness in crew.consciousness_history:
                    writer.writerow([crew.crew_id, day, consciousness, crew.consciousness_target])

        print(f"✅ Exported consciousness curves to {filename}")

    def export_adaptation_events(self, filename: str):
        """Export consciousness adaptation events"""
        import csv

        if not self.adaptation_events:
            print("⚠️  No adaptation events to export")
            return

        with open(filename, 'w', newline='') as f:
            fieldnames = list(self.adaptation_events[0].keys())
            writer = csv.DictWriter(f, fieldnames=fieldnames)
            writer.writeheader()
            writer.writerows(self.adaptation_events)

        print(f"✅ Exported {len(self.adaptation_events)} adaptation events to {filename}")

    def print_summary(self):
        """Print simulation summary"""
        print("=" * 100)
        print("📊 SIMULATION SUMMARY")
        print("=" * 100)
        print()

        for crew in self.crew:
            baseline_consciousness = crew.consciousness_target
            final_consciousness = crew.consciousness_current
            consciousness_change = final_consciousness - baseline_consciousness

            # Get final metrics
            crew_logs = [log for log in self.daily_logs if log['crew_id'] == crew.crew_id]
            final_log = crew_logs[-1]
            baseline_log = crew_logs[0]

            hbc_change = ((final_log['hbc'] - baseline_log['hbc']) / baseline_log['hbc']) * 100
            tremor_change = ((final_log['tremor_amplitude'] - baseline_log['tremor_amplitude']) / baseline_log['tremor_amplitude']) * 100

            print(f"  {crew.crew_id}")
            print(f"  {'=' * 80}")
            print(f"    Consciousness: {baseline_consciousness:.3f} → {final_consciousness:.3f} ({consciousness_change:+.3f})")
            print(f"    HBC:           {baseline_log['hbc']:.2f} → {final_log['hbc']:.2f} ({hbc_change:+.1f}%)")
            print(f"    Tremor:        {baseline_log['tremor_amplitude']:.4f} → {final_log['tremor_amplitude']:.4f} ({tremor_change:+.1f}%)")
            print(f"    Radiation:     {final_log['cumulative_radiation']:.0f} mSv")

            # Count adaptation events
            crew_adaptations = [e for e in self.adaptation_events if e.get('crew_id') == crew.crew_id or any(log['crew_id'] == crew.crew_id and log['mission_day'] == e['day'] for log in crew_logs)]
            print(f"    Adaptations:   {len([e for e in self.adaptation_events if any(log['crew_id'] == crew.crew_id and log['mission_day'] == e['day'] for log in crew_logs)])}")
            print()


if __name__ == "__main__":
    # Create enhanced simulation
    sim = EnhancedSimulation()

    # Add crew with varying consciousness targets and learning rates
    sim.add_crew_member("CDR-ALPHA", consciousness_target=0.82, learning_rate=0.03)
    sim.add_crew_member("SCI-BETA", consciousness_target=0.70, learning_rate=0.05)
    sim.add_crew_member("ENG-GAMMA", consciousness_target=0.55, learning_rate=0.07)  # Highest learning rate
    sim.add_crew_member("MED-DELTA", consciousness_target=0.75, learning_rate=0.04)

    # Schedule intense SPE events (200-410 mSv range)
    sim.schedule_spe(25, 220, "Strong M-class Solar Flare")
    sim.schedule_spe(48, 380, "X-class Solar Flare (Major)")
    sim.schedule_spe(73, 285, "Moderate GLE Event")
    sim.schedule_spe(89, 410, "Extreme GLE Event")
    sim.schedule_spe(105, 195, "M-class Flare Cluster")
    sim.schedule_spe(128, 340, "X-class with CME")
    sim.schedule_spe(145, 265, "Proton Storm")
    sim.schedule_spe(162, 295, "Strong GLE")
    sim.schedule_spe(175, 225, "Late Mission Flare")

    # Run 180-day simulation
    sim.run_simulation(mission_days=180)

    # Export results
    sim.export_to_csv("enhanced_consciousness_180day.csv")
    sim.export_consciousness_curves("consciousness_evolution_curves.csv")
    sim.export_adaptation_events("consciousness_adaptation_events.csv")

    # Print summary
    sim.print_summary()

    print("\n📊 Exported Files:")
    print("   1. enhanced_consciousness_180day.csv - Full mission data with consciousness tracking")
    print("   2. consciousness_evolution_curves.csv - Consciousness learning curves per crew")
    print("   3. consciousness_adaptation_events.csv - All adaptation trigger events")
    print()
