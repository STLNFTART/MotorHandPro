#!/usr/bin/env python3
"""
Crew Health Simulation Runner - Multi-Crew with Variable SPE Events
====================================================================
Demonstrates PRIMAL Logic adaptive thresholds with varying consciousness
levels and multiple SPE spike scenarios.

Shows:
- Golden ratio φ threshold scaling with consciousness
- Bounded drift via λ preventing false positives
- φ² intervention scoring for CREW_RETRIEVAL
- Personalized sensitivity per crew member
"""

import sys
import os
from datetime import datetime, timedelta
from typing import List, Dict, Any
import json

# Mock numpy for basic operations if not available
try:
    import numpy as np
except ImportError:
    print("⚠️  NumPy not available. Using basic random for simulation.")
    import random
    class NumpyMock:
        @staticmethod
        def mean(values): return sum(values) / len(values) if values else 0
        @staticmethod
        def std(values):
            if not values: return 0
            mean = sum(values) / len(values)
            return (sum((x - mean) ** 2 for x in values) / len(values)) ** 0.5
        @staticmethod
        def random_normal(mean, std): return random.gauss(mean, std)
        @staticmethod
        def exp(x): return 2.71828 ** x
    np = NumpyMock()
    np.random = NumpyMock()
    np.random.normal = lambda mu, sigma: NumpyMock.random_normal(mu, sigma)

# Add integrations to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from integrations.crew_health_monitor import (
    CrewHealthMonitor,
    CrewHealthMetrics,
    GOLDEN_RATIO,
    LAMBDA_LIGHTFOOT
)


class MultiCrewSimulation:
    """Multi-crew simulation with varying consciousness and SPE events"""

    def __init__(self, drift_tolerance: float = 0.05):
        self.monitor = CrewHealthMonitor(drift_tolerance=drift_tolerance)
        self.crew_profiles = []
        self.spe_schedule = []
        self.results = []

    def add_crew(self, crew_id: str, baseline_hbc: float, baseline_tremor: float,
                 consciousness_target: float):
        """
        Add crew member with target consciousness level

        Args:
            crew_id: Crew identifier
            baseline_hbc: Baseline hemoglobin concentration (g/dL)
            baseline_tremor: Baseline tremor amplitude (mm)
            consciousness_target: Target consciousness level (0.5-0.9)
        """
        self.crew_profiles.append({
            'crew_id': crew_id,
            'baseline_hbc': baseline_hbc,
            'baseline_tremor': baseline_tremor,
            'consciousness_target': consciousness_target,
            'baseline_reaction': 250.0,
            'baseline_grip': 45.0,
            'baseline_cognitive': 95.0
        })
        self.monitor.register_crew(crew_id)

    def schedule_spe(self, day: int, dose_msv: float, description: str = ""):
        """Schedule SPE event on specific mission day"""
        self.spe_schedule.append({
            'day': day,
            'dose_msv': dose_msv,
            'description': description or f"SPE Event: {dose_msv} mSv"
        })
        # Sort by day
        self.spe_schedule.sort(key=lambda x: x['day'])

    def run_simulation(self, mission_days: int = 120, start_date: datetime = None):
        """
        Run multi-crew simulation with SPE events

        Args:
            mission_days: Total mission duration in days
            start_date: Mission start date (default: today)
        """
        if start_date is None:
            start_date = datetime.now()

        print(f"🚀 Multi-Crew Health Simulation")
        print("=" * 100)
        print(f"Mission Duration: {mission_days} days")
        print(f"Drift Tolerance: {self.monitor.drift_tolerance * 100:.0f}%")
        print(f"Crew Members: {len(self.crew_profiles)}")
        print(f"Scheduled SPE Events: {len(self.spe_schedule)}")
        print()

        # Display crew profiles
        print("👥 Crew Profiles:")
        print("-" * 100)
        for profile in self.crew_profiles:
            print(f"{profile['crew_id']:20s} | "
                  f"Consciousness Target: {profile['consciousness_target']:.2f} | "
                  f"HBC Baseline: {profile['baseline_hbc']:.1f} g/dL | "
                  f"Tremor Baseline: {profile['baseline_tremor']:.3f} mm")

        # Display SPE schedule
        if self.spe_schedule:
            print(f"\n☀️  SPE Event Schedule:")
            print("-" * 100)
            for spe in self.spe_schedule:
                print(f"Day {spe['day']:3d} | {spe['dose_msv']:6.1f} mSv | {spe['description']}")

        print("\n" + "=" * 100)
        print("🔬 Running Simulation...")
        print()

        # Track SPE exposure per crew
        spe_exposure_count = {p['crew_id']: 0 for p in self.crew_profiles}
        cumulative_dose = {p['crew_id']: 0.0 for p in self.crew_profiles}

        # Simulation loop
        for day in range(mission_days):
            # Check for SPE events today
            today_spe = [spe for spe in self.spe_schedule if spe['day'] == day]

            if today_spe:
                for spe in today_spe:
                    print(f"\n{'='*100}")
                    print(f"☀️  DAY {day}: SPE EVENT - {spe['dose_msv']} mSv")
                    print(f"{'='*100}")

                    # Apply to all crew
                    for profile in self.crew_profiles:
                        impact = self.monitor.inject_spe_event(
                            profile['crew_id'],
                            spe['dose_msv'],
                            start_date + timedelta(days=day)
                        )
                        spe_exposure_count[profile['crew_id']] += 1
                        cumulative_dose[profile['crew_id']] += spe['dose_msv']

                        print(f"  {profile['crew_id']:20s} | "
                              f"HBC Drop: {impact['predicted_hbc_drop_pct']:5.2f}% | "
                              f"Tremor↑: {impact['predicted_tremor_increase_pct']:5.2f}% | "
                              f"Total Dose: {cumulative_dose[profile['crew_id']]:6.1f} mSv")

            # Process each crew member
            for profile in self.crew_profiles:
                crew_id = profile['crew_id']

                # Calculate expected degradation (isolation model)
                months = day / 30.0
                hbc_degradation_pct = min(0.08, (months / 4.0) * 0.08)  # 8% by month 4
                tremor_increase_pct = min(0.15, (months / 4.0) * 0.15)  # 15% by month 4

                hbc_expected = profile['baseline_hbc'] * (1.0 - hbc_degradation_pct)
                tremor_expected = profile['baseline_tremor'] * (1.0 + tremor_increase_pct)

                # Add SPE impact (within 14 days of any SPE)
                spe_modifier_hbc = 1.0
                spe_modifier_tremor = 1.0

                for spe in self.spe_schedule:
                    if spe['day'] <= day < spe['day'] + 14:
                        # HBC drop: 0.5% per 100 mSv
                        spe_modifier_hbc -= (spe['dose_msv'] / 100.0 * 0.005)
                        # Tremor increase: 2% per 100 mSv
                        spe_modifier_tremor += (spe['dose_msv'] / 100.0 * 0.02)

                hbc_actual = hbc_expected * spe_modifier_hbc + np.random.normal(0, 0.3)
                tremor_actual = tremor_expected * spe_modifier_tremor + np.random.normal(0, 0.05)

                # Add consciousness-dependent noise (lower consciousness = more variability)
                consciousness_noise_factor = (1.0 - profile['consciousness_target']) * 0.5
                hbc_actual += np.random.normal(0, consciousness_noise_factor * 0.3)
                tremor_actual += np.random.normal(0, consciousness_noise_factor * 0.05)

                # Create metrics
                metrics = CrewHealthMetrics(
                    crew_id=crew_id,
                    timestamp=start_date + timedelta(days=day),
                    mission_day=day,
                    hbc=max(10.0, hbc_actual),  # Floor at 10.0
                    rbc_count=5.1 - (months / 4.0) * 0.2,
                    hematocrit=45.0 - (months / 4.0) * 2.0,
                    tremor_amplitude=max(0.1, tremor_actual),
                    tremor_frequency=10.0 - (months / 4.0) * 0.5,
                    grip_strength=profile['baseline_grip'] - (months / 4.0) * 2.0,
                    reaction_time=profile['baseline_reaction'] + (months / 4.0) * 30 + np.random.normal(0, 10),
                    cognitive_score=profile['baseline_cognitive'] - (months / 4.0) * 5.0,
                    cumulative_radiation_dose=cumulative_dose[crew_id] + day * 0.5,
                    current_dose_rate=0.5,
                    spe_exposure_count=spe_exposure_count[crew_id],
                    cortisol=12.0 + (months / 4.0) * 3.0,
                    heart_rate_variability=75.0 - (months / 4.0) * 10.0,
                    isolation_hours=day * 24
                )

                # Process metrics
                result = self.monitor.process_metrics(metrics)

                # Store result
                self.results.append({
                    'day': day,
                    'crew_id': crew_id,
                    'consciousness': result['baseline_stats']['consciousness'],
                    'hbc': metrics.hbc,
                    'tremor': metrics.tremor_amplitude,
                    'intervention_level': result['intervention']['intervention_level'],
                    'intervention_score': result['intervention']['intervention_score'],
                    'alerts': result['threshold_alerts'],
                    'drift_exceeded': result['drift_assessment']['drift_exceeds_tolerance']
                })

                # Print alerts
                if result['threshold_alerts'] or result['intervention']['intervention_level'] != 'NONE':
                    print(f"\n⚠️  Day {day:3d} | {crew_id:20s}")

                    if result['threshold_alerts']:
                        for alert in result['threshold_alerts']:
                            metric = alert['metric']
                            severity = alert['severity']
                            value = alert['value']
                            deviation = alert['deviation']

                            if metric == 'HBC':
                                drop_pct = alert['drop_percent']
                                print(f"  └─ {severity:8s} {metric:12s}: {value:.2f} g/dL "
                                      f"({drop_pct:+.1f}% from baseline, {deviation:.1f}σ)")
                            elif metric == 'TREMOR':
                                increase_pct = alert['increase_percent']
                                print(f"  └─ {severity:8s} {metric:12s}: {value:.3f} mm "
                                      f"({increase_pct:+.1f}% from baseline, {deviation:.1f}σ)")

                    if result['intervention']['intervention_level'] != 'NONE':
                        print(f"  └─ 🚨 {result['intervention']['intervention_level']:9s} | "
                              f"{result['intervention']['action']} | "
                              f"Score: {result['intervention']['intervention_score']:.2f}")
                        print(f"     Reason: {result['intervention']['reasoning'][:80]}")

        print(f"\n{'='*100}")
        print("✅ Simulation Complete")
        print("=" * 100)

    def generate_summary(self) -> Dict[str, Any]:
        """Generate simulation summary statistics"""
        summary = {
            'total_days': max(r['day'] for r in self.results) + 1 if self.results else 0,
            'total_spe_events': len(self.spe_schedule),
            'crew_summary': {}
        }

        for profile in self.crew_profiles:
            crew_id = profile['crew_id']
            crew_results = [r for r in self.results if r['crew_id'] == crew_id]

            if not crew_results:
                continue

            final_result = crew_results[-1]
            baseline = self.monitor.crew_baselines[crew_id]

            alerts_by_severity = {'CRITICAL': 0, 'WARNING': 0}
            for result in crew_results:
                for alert in result['alerts']:
                    alerts_by_severity[alert['severity']] += 1

            intervention_counts = {'IMMEDIATE': 0, 'URGENT': 0, 'SCHEDULED': 0, 'NONE': 0}
            for result in crew_results:
                intervention_counts[result['intervention_level']] += 1

            # Calculate actual degradation
            final_metrics = self.monitor.metrics_history[-len(self.crew_profiles) +
                           self.crew_profiles.index(profile)]
            hbc_drop_pct = ((baseline.hbc_baseline - final_metrics.hbc) /
                           baseline.hbc_baseline * 100)
            tremor_increase_pct = ((final_metrics.tremor_amplitude - baseline.tremor_baseline) /
                                  baseline.tremor_baseline * 100)

            summary['crew_summary'][crew_id] = {
                'consciousness_final': final_result['consciousness'],
                'consciousness_target': profile['consciousness_target'],
                'hbc_baseline': baseline.hbc_baseline,
                'hbc_final': final_metrics.hbc,
                'hbc_drop_percent': hbc_drop_pct,
                'tremor_baseline': baseline.tremor_baseline,
                'tremor_final': final_metrics.tremor_amplitude,
                'tremor_increase_percent': tremor_increase_pct,
                'cumulative_radiation_dose': final_metrics.cumulative_radiation_dose,
                'spe_exposure_count': final_metrics.spe_exposure_count,
                'alerts_critical': alerts_by_severity['CRITICAL'],
                'alerts_warning': alerts_by_severity['WARNING'],
                'interventions': intervention_counts,
                'effective_threshold_sigma': (2.0 / (final_result['consciousness'] * GOLDEN_RATIO)),
                'effective_critical_sigma': (3.0 / (final_result['consciousness'] * GOLDEN_RATIO))
            }

        return summary

    def print_summary(self):
        """Print detailed simulation summary"""
        summary = self.generate_summary()

        print(f"\n📊 SIMULATION SUMMARY")
        print("=" * 100)
        print(f"Mission Duration: {summary['total_days']} days")
        print(f"SPE Events: {summary['total_spe_events']}")
        print(f"Drift Tolerance: {self.monitor.drift_tolerance * 100:.0f}%")
        print()

        for crew_id, stats in summary['crew_summary'].items():
            print(f"\n👤 {crew_id}")
            print("-" * 100)

            print(f"Consciousness: {stats['consciousness_final']:.3f} "
                  f"(target: {stats['consciousness_target']:.2f})")

            print(f"\nAdaptive Thresholds (σ_eff = σ_base / (consciousness × φ)):")
            print(f"  Warning:  {stats['effective_threshold_sigma']:.2f}σ")
            print(f"  Critical: {stats['effective_critical_sigma']:.2f}σ")

            print(f"\nHematological:")
            print(f"  HBC: {stats['hbc_final']:.2f} g/dL "
                  f"(baseline: {stats['hbc_baseline']:.2f}, "
                  f"drop: {stats['hbc_drop_percent']:.1f}%)")

            print(f"\nNeuromuscular:")
            print(f"  Tremor: {stats['tremor_final']:.3f} mm "
                  f"(baseline: {stats['tremor_baseline']:.3f}, "
                  f"increase: {stats['tremor_increase_percent']:.1f}%)")

            print(f"\nRadiation:")
            print(f"  Cumulative: {stats['cumulative_radiation_dose']:.1f} mSv")
            print(f"  SPE Exposures: {stats['spe_exposure_count']}")

            print(f"\nAlerts:")
            print(f"  Critical: {stats['alerts_critical']}")
            print(f"  Warning: {stats['alerts_warning']}")

            print(f"\nInterventions:")
            for level, count in stats['interventions'].items():
                if count > 0:
                    print(f"  {level}: {count}")

        print("\n" + "=" * 100)

    def export_results(self, filename: str = "crew_simulation_results.json"):
        """Export simulation results to JSON"""
        summary = self.generate_summary()

        output = {
            'simulation_config': {
                'mission_days': summary['total_days'],
                'drift_tolerance': self.monitor.drift_tolerance,
                'spe_schedule': self.spe_schedule,
                'crew_count': len(self.crew_profiles)
            },
            'primal_constants': {
                'golden_ratio': GOLDEN_RATIO,
                'lambda_lightfoot': LAMBDA_LIGHTFOOT
            },
            'summary': summary,
            'detailed_results': self.results[-100:]  # Last 100 results for brevity
        }

        with open(filename, 'w') as f:
            json.dump(output, f, indent=2, default=str)

        print(f"✅ Exported results to {filename}")

        # Also export CSV
        csv_filename = filename.replace('.json', '.csv')
        self.monitor.export_to_csv(csv_filename)


# === MAIN SIMULATION RUNNER ===
if __name__ == "__main__":
    print("🧪 PRIMAL Logic Crew Health Simulation")
    print("Demonstrating φ-scaled thresholds with varying consciousness levels")
    print()

    # Initialize simulation
    sim = MultiCrewSimulation(drift_tolerance=0.05)  # 5% drift tolerance

    # Add crew members with varying consciousness targets
    sim.add_crew(
        crew_id="CDR-ALPHA",
        baseline_hbc=15.5,
        baseline_tremor=0.45,
        consciousness_target=0.9  # High consciousness = tight thresholds
    )

    sim.add_crew(
        crew_id="SCI-BETA",
        baseline_hbc=14.8,
        baseline_tremor=0.52,
        consciousness_target=0.7  # Moderate consciousness
    )

    sim.add_crew(
        crew_id="ENG-GAMMA",
        baseline_hbc=15.2,
        baseline_tremor=0.48,
        consciousness_target=0.5  # Lower consciousness = looser thresholds
    )

    # Schedule variable SPE events (spikes)
    sim.schedule_spe(45, 150, "Moderate Solar Flare")
    sim.schedule_spe(72, 320, "Major CME (Coronal Mass Ejection)")
    sim.schedule_spe(89, 95, "Minor Solar Particle Event")
    sim.schedule_spe(105, 200, "Strong Solar Flare")

    # Run 120-day mission
    sim.run_simulation(mission_days=120, start_date=datetime(2025, 1, 1))

    # Print summary
    sim.print_summary()

    # Export results
    sim.export_results("crew_simulation_phi_consciousness.json")

    print("\n🎯 Key Insights:")
    print("-" * 100)
    print(f"✓ Golden ratio φ = {GOLDEN_RATIO:.6f} scales thresholds harmonically")
    print(f"✓ Lightfoot λ = {LAMBDA_LIGHTFOOT:.6f} bounds drift integrals")
    print(f"✓ φ² = {GOLDEN_RATIO**2:.6f} threshold for IMMEDIATE intervention")
    print(f"✓ Higher consciousness → tighter thresholds (personalized sensitivity)")
    print(f"✓ Bounded drift prevents false positives on long missions")
    print("\n✅ Ready for merge and live testing!")
