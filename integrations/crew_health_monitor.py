#!/usr/bin/env python3
"""
Crew Health Monitoring with PRIMAL Logic Adaptive Baselines
============================================================
Integrates with space environment SPE events for real-time crew health tracking
during long-duration isolation missions (space, underwater, polar, etc.)

Features:
- 180-day rolling baseline with auto-scaling thresholds
- HBC (Hemoglobin Concentration) tracking with 8% degradation model
- Tremor analysis with 15% increase by month 4
- PRIMAL Logic consciousness-weighted intervention triggers
- Drift tolerance with bounded integrals
- SPE event correlation and response

Author: MotorHandPro Biomedical Team
"""

from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass, field
from collections import deque
import json
import csv
import math
import random

# NumPy fallback for basic operations
try:
    import numpy as np
except ImportError:
    class NumpyMock:
        @staticmethod
        def mean(values):
            vals = list(values) if hasattr(values, '__iter__') else [values]
            return sum(vals) / len(vals) if vals else 0
        @staticmethod
        def std(values):
            vals = list(values) if hasattr(values, '__iter__') else [values]
            if not vals: return 0
            mean_val = sum(vals) / len(vals)
            return math.sqrt(sum((x - mean_val) ** 2 for x in vals) / len(vals))
        @staticmethod
        def normal(mu, sigma): return random.gauss(mu, sigma)
        @staticmethod
        def exp(x): return math.exp(x)
    np = NumpyMock()
    np.random = NumpyMock()

# PRIMAL Logic constants
LAMBDA_LIGHTFOOT = 0.16905  # s⁻¹
DONTE_CONSTANT = 149.9992314000
I3_CONSTANT = 6.4939394023
GOLDEN_RATIO = 1.618033988749


@dataclass
class CrewHealthMetrics:
    """Individual crew member health metrics"""
    crew_id: str
    timestamp: datetime

    # Hematological
    hbc: float  # Hemoglobin concentration (g/dL) - normal: 13.5-17.5
    rbc_count: float  # Red blood cell count (M/µL) - normal: 4.5-5.9
    hematocrit: float  # % - normal: 38.3-48.6

    # Neuromuscular
    tremor_amplitude: float  # mm - baseline varies, increase indicates degradation
    tremor_frequency: float  # Hz - normal: 8-12 Hz
    grip_strength: float  # kg - baseline varies by individual

    # Cognitive
    reaction_time: float  # ms - baseline ~200-300ms
    cognitive_score: float  # 0-100 standardized score

    # Radiation exposure (from SPE events)
    cumulative_radiation_dose: float  # mSv
    current_dose_rate: float  # mSv/day

    # Physiological stress markers
    cortisol: float  # µg/dL - normal: 6-23 morning
    heart_rate_variability: float  # SDNN in ms - normal: 50-100

    # Mission context
    mission_day: int
    isolation_hours: float
    spe_exposure_count: int  # Number of SPE events experienced


@dataclass
class CrewBaseline:
    """180-day rolling baseline for each crew member"""
    crew_id: str
    window_days: int = 180

    # Rolling statistics (stored as deques)
    hbc_history: deque = field(default_factory=lambda: deque(maxlen=180))
    tremor_history: deque = field(default_factory=lambda: deque(maxlen=180))
    reaction_time_history: deque = field(default_factory=lambda: deque(maxlen=180))

    # Baseline values (updated daily)
    hbc_baseline: float = 15.0
    hbc_std: float = 0.5
    tremor_baseline: float = 0.5
    tremor_std: float = 0.1
    reaction_baseline: float = 250.0
    reaction_std: float = 20.0

    # Auto-scaling threshold multipliers
    threshold_sigma: float = 2.0  # Number of std deviations for warning
    critical_sigma: float = 3.0  # Number of std deviations for intervention

    # PRIMAL Logic consciousness factor (adapts based on consistency)
    consciousness: float = 0.5

    def update_baseline(self, metrics: CrewHealthMetrics):
        """Update rolling baseline with new metrics"""
        # Add to history
        self.hbc_history.append(metrics.hbc)
        self.tremor_history.append(metrics.tremor_amplitude)
        self.reaction_time_history.append(metrics.reaction_time)

        # Recalculate baselines (mean and std)
        if len(self.hbc_history) >= 30:  # Minimum 30 days for stable baseline
            self.hbc_baseline = np.mean(self.hbc_history)
            self.hbc_std = np.std(self.hbc_history)

            self.tremor_baseline = np.mean(self.tremor_history)
            self.tremor_std = np.std(self.tremor_history)

            self.reaction_baseline = np.mean(self.reaction_time_history)
            self.reaction_std = np.std(self.reaction_time_history)

            # Update PRIMAL consciousness based on measurement consistency
            variance_normalized = (self.hbc_std / max(0.01, self.hbc_baseline))
            self.consciousness = max(0.1, min(1.0, 1.0 - variance_normalized * 5))

    def get_deviation(self, value: float, baseline: float, std: float) -> float:
        """Calculate standardized deviation (z-score)"""
        return (value - baseline) / max(0.01, std)

    def check_thresholds(self, metrics: CrewHealthMetrics) -> Dict[str, Any]:
        """Check if metrics exceed adaptive thresholds"""
        # Apply PRIMAL Logic consciousness weighting
        # Higher consciousness = more sensitive to deviations
        effective_threshold = self.threshold_sigma / (self.consciousness * GOLDEN_RATIO)
        effective_critical = self.critical_sigma / (self.consciousness * GOLDEN_RATIO)

        hbc_dev = self.get_deviation(metrics.hbc, self.hbc_baseline, self.hbc_std)
        tremor_dev = self.get_deviation(metrics.tremor_amplitude,
                                       self.tremor_baseline, self.tremor_std)
        reaction_dev = self.get_deviation(metrics.reaction_time,
                                         self.reaction_baseline, self.reaction_std)

        alerts = []

        # HBC drop detection (negative deviation is concerning)
        if hbc_dev < -effective_threshold:
            severity = "CRITICAL" if hbc_dev < -effective_critical else "WARNING"
            alerts.append({
                'metric': 'HBC',
                'severity': severity,
                'deviation': hbc_dev,
                'value': metrics.hbc,
                'baseline': self.hbc_baseline,
                'drop_percent': ((self.hbc_baseline - metrics.hbc) / self.hbc_baseline) * 100
            })

        # Tremor increase detection (positive deviation is concerning)
        if tremor_dev > effective_threshold:
            severity = "CRITICAL" if tremor_dev > effective_critical else "WARNING"
            alerts.append({
                'metric': 'TREMOR',
                'severity': severity,
                'deviation': tremor_dev,
                'value': metrics.tremor_amplitude,
                'baseline': self.tremor_baseline,
                'increase_percent': ((metrics.tremor_amplitude - self.tremor_baseline) /
                                    max(0.01, self.tremor_baseline)) * 100
            })

        # Reaction time increase detection
        if reaction_dev > effective_threshold:
            severity = "CRITICAL" if reaction_dev > effective_critical else "WARNING"
            alerts.append({
                'metric': 'REACTION_TIME',
                'severity': severity,
                'deviation': reaction_dev,
                'value': metrics.reaction_time,
                'baseline': self.reaction_baseline
            })

        return {
            'alerts': alerts,
            'consciousness': self.consciousness,
            'effective_threshold_sigma': effective_threshold,
            'effective_critical_sigma': effective_critical
        }


class CrewHealthMonitor:
    """Main crew health monitoring system with PRIMAL Logic integration"""

    def __init__(self, drift_tolerance: float = 0.05):
        """
        Initialize crew health monitor

        Args:
            drift_tolerance: Maximum allowed drift before intervention (0-1)
                           0.05 = 5% drift triggers crew intervention warning
        """
        self.crew_baselines: Dict[str, CrewBaseline] = {}
        self.drift_tolerance = drift_tolerance
        self.metrics_history: List[CrewHealthMetrics] = []

        # Expected degradation models (from isolation studies)
        self.expected_hbc_drop_4months = 0.08  # 8% drop by month 4
        self.expected_tremor_increase_4months = 0.15  # 15% increase by month 4

        # SPE event correlation tracking
        self.spe_correlation_window = 14  # days after SPE to monitor effects

    def register_crew(self, crew_id: str):
        """Register new crew member and initialize baseline"""
        if crew_id not in self.crew_baselines:
            self.crew_baselines[crew_id] = CrewBaseline(crew_id=crew_id)

    def process_metrics(self, metrics: CrewHealthMetrics) -> Dict[str, Any]:
        """
        Process crew health metrics with PRIMAL Logic adaptive analysis

        Returns:
            Analysis results including alerts, drift assessment, intervention needs
        """
        # Ensure crew is registered
        self.register_crew(metrics.crew_id)
        baseline = self.crew_baselines[metrics.crew_id]

        # Update rolling baseline
        baseline.update_baseline(metrics)

        # Check adaptive thresholds
        threshold_check = baseline.check_thresholds(metrics)

        # Calculate drift from expected degradation curve
        drift_assessment = self._assess_drift(metrics, baseline)

        # Determine intervention needs
        intervention = self._determine_intervention(
            threshold_check, drift_assessment, metrics
        )

        # Store metrics
        self.metrics_history.append(metrics)

        return {
            'crew_id': metrics.crew_id,
            'mission_day': metrics.mission_day,
            'timestamp': metrics.timestamp.isoformat(),
            'threshold_alerts': threshold_check['alerts'],
            'drift_assessment': drift_assessment,
            'intervention': intervention,
            'baseline_stats': {
                'hbc_baseline': baseline.hbc_baseline,
                'tremor_baseline': baseline.tremor_baseline,
                'consciousness': baseline.consciousness
            }
        }

    def _assess_drift(self, metrics: CrewHealthMetrics,
                     baseline: CrewBaseline) -> Dict[str, Any]:
        """
        Assess drift from expected degradation curves using bounded integrals
        """
        # Calculate expected values based on mission day
        mission_months = metrics.mission_day / 30.0

        # Expected HBC drop (linear model for first 4 months)
        expected_hbc_drop_pct = min(
            self.expected_hbc_drop_4months,
            (mission_months / 4.0) * self.expected_hbc_drop_4months
        )
        expected_hbc = baseline.hbc_baseline * (1.0 - expected_hbc_drop_pct)

        # Expected tremor increase (linear model for first 4 months)
        expected_tremor_increase_pct = min(
            self.expected_tremor_increase_4months,
            (mission_months / 4.0) * self.expected_tremor_increase_4months
        )
        expected_tremor = baseline.tremor_baseline * (1.0 + expected_tremor_increase_pct)

        # Calculate drift (actual vs expected)
        hbc_drift = abs(metrics.hbc - expected_hbc) / expected_hbc
        tremor_drift = abs(metrics.tremor_amplitude - expected_tremor) / max(0.01, expected_tremor)

        # PRIMAL Logic bounded integral - prevents drift accumulation
        # Uses Lightfoot constant for exponential decay
        hbc_drift_bounded = hbc_drift * np.exp(-LAMBDA_LIGHTFOOT * metrics.mission_day)
        tremor_drift_bounded = tremor_drift * np.exp(-LAMBDA_LIGHTFOOT * metrics.mission_day)

        return {
            'hbc_drift': hbc_drift,
            'hbc_drift_bounded': hbc_drift_bounded,
            'tremor_drift': tremor_drift,
            'tremor_drift_bounded': tremor_drift_bounded,
            'expected_hbc': expected_hbc,
            'expected_tremor': expected_tremor,
            'actual_hbc': metrics.hbc,
            'actual_tremor': metrics.tremor_amplitude,
            'drift_exceeds_tolerance': (hbc_drift > self.drift_tolerance or
                                       tremor_drift > self.drift_tolerance)
        }

    def _determine_intervention(self, threshold_check: Dict,
                               drift_assessment: Dict,
                               metrics: CrewHealthMetrics) -> Dict[str, Any]:
        """
        Determine if crew intervention is needed based on PRIMAL Logic analysis
        """
        critical_alerts = [a for a in threshold_check['alerts']
                          if a['severity'] == 'CRITICAL']
        warning_alerts = [a for a in threshold_check['alerts']
                         if a['severity'] == 'WARNING']

        drift_exceeded = drift_assessment['drift_exceeds_tolerance']

        # PRIMAL Logic intervention scoring
        # Golden ratio weighting: critical alerts > drift > warnings
        intervention_score = (
            len(critical_alerts) * GOLDEN_RATIO +
            (1.0 if drift_exceeded else 0.0) +
            len(warning_alerts) / GOLDEN_RATIO
        )

        # Intervention thresholds
        if intervention_score >= GOLDEN_RATIO * 2:  # ~3.236
            intervention_level = "IMMEDIATE"
            action = "CREW_RETRIEVAL"
        elif intervention_score >= GOLDEN_RATIO:  # ~1.618
            intervention_level = "URGENT"
            action = "MEDICAL_INTERVENTION"
        elif intervention_score >= 1.0:
            intervention_level = "SCHEDULED"
            action = "ENHANCED_MONITORING"
        else:
            intervention_level = "NONE"
            action = "ROUTINE_MONITORING"

        return {
            'intervention_level': intervention_level,
            'action': action,
            'intervention_score': intervention_score,
            'critical_count': len(critical_alerts),
            'warning_count': len(warning_alerts),
            'drift_exceeded': drift_exceeded,
            'reasoning': self._generate_intervention_reasoning(
                critical_alerts, warning_alerts, drift_exceeded, metrics
            )
        }

    def _generate_intervention_reasoning(self, critical_alerts: List,
                                        warning_alerts: List,
                                        drift_exceeded: bool,
                                        metrics: CrewHealthMetrics) -> str:
        """Generate human-readable intervention reasoning"""
        reasons = []

        for alert in critical_alerts:
            if alert['metric'] == 'HBC':
                reasons.append(
                    f"HBC dropped {alert['drop_percent']:.1f}% "
                    f"(critical threshold exceeded)"
                )
            elif alert['metric'] == 'TREMOR':
                reasons.append(
                    f"Tremor increased {alert['increase_percent']:.1f}% "
                    f"(critical threshold exceeded)"
                )

        for alert in warning_alerts:
            if alert['metric'] == 'HBC':
                reasons.append(
                    f"HBC drop {alert['drop_percent']:.1f}% detected "
                    f"({alert['deviation']:.1f}σ from baseline)"
                )
            elif alert['metric'] == 'TREMOR':
                reasons.append(
                    f"Tremor increase {alert['increase_percent']:.1f}% detected "
                    f"({alert['deviation']:.1f}σ from baseline)"
                )

        if drift_exceeded:
            reasons.append(
                f"Drift exceeds {self.drift_tolerance*100:.0f}% tolerance "
                f"(deviation from expected degradation curve)"
            )

        if metrics.spe_exposure_count > 0:
            reasons.append(
                f"{metrics.spe_exposure_count} SPE exposures recorded "
                f"(cumulative dose: {metrics.cumulative_radiation_dose:.1f} mSv)"
            )

        return " | ".join(reasons) if reasons else "Nominal parameters"

    def inject_spe_event(self, crew_id: str, spe_dose_msv: float,
                        event_time: datetime) -> Dict[str, Any]:
        """
        Inject SPE (Solar Particle Event) and model health impact

        Args:
            crew_id: Crew member ID
            spe_dose_msv: Radiation dose from SPE in mSv
            event_time: Event timestamp

        Returns:
            Predicted health impact
        """
        # Model HBC drop from radiation (studies show ~0.5% drop per 100 mSv)
        hbc_drop_factor = spe_dose_msv / 100.0 * 0.005

        # Model tremor increase from radiation (neural effects)
        tremor_increase_factor = spe_dose_msv / 100.0 * 0.02

        return {
            'crew_id': crew_id,
            'spe_dose_msv': spe_dose_msv,
            'event_time': event_time.isoformat(),
            'predicted_hbc_drop_pct': hbc_drop_factor * 100,
            'predicted_tremor_increase_pct': tremor_increase_factor * 100,
            'monitoring_window_days': self.spe_correlation_window,
            'recommendation': (
                'ENHANCED_MONITORING' if spe_dose_msv > 50 else 'ROUTINE_MONITORING'
            )
        }

    def export_to_csv(self, filename: str):
        """Export metrics history to CSV for analysis"""
        with open(filename, 'w', newline='') as csvfile:
            fieldnames = [
                'crew_id', 'timestamp', 'mission_day', 'hbc', 'tremor_amplitude',
                'tremor_frequency', 'grip_strength', 'reaction_time', 'cognitive_score',
                'cumulative_radiation_dose', 'current_dose_rate', 'spe_exposure_count',
                'cortisol', 'heart_rate_variability'
            ]
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()

            for metrics in self.metrics_history:
                writer.writerow({
                    'crew_id': metrics.crew_id,
                    'timestamp': metrics.timestamp.isoformat(),
                    'mission_day': metrics.mission_day,
                    'hbc': metrics.hbc,
                    'tremor_amplitude': metrics.tremor_amplitude,
                    'tremor_frequency': metrics.tremor_frequency,
                    'grip_strength': metrics.grip_strength,
                    'reaction_time': metrics.reaction_time,
                    'cognitive_score': metrics.cognitive_score,
                    'cumulative_radiation_dose': metrics.cumulative_radiation_dose,
                    'current_dose_rate': metrics.current_dose_rate,
                    'spe_exposure_count': metrics.spe_exposure_count,
                    'cortisol': metrics.cortisol,
                    'heart_rate_variability': metrics.heart_rate_variability
                })

        print(f"✅ Exported {len(self.metrics_history)} metrics to {filename}")

    def import_from_csv(self, filename: str) -> int:
        """Import metrics from CSV for analysis"""
        count = 0
        with open(filename, 'r') as csvfile:
            reader = csv.DictReader(csvfile)
            for row in reader:
                metrics = CrewHealthMetrics(
                    crew_id=row['crew_id'],
                    timestamp=datetime.fromisoformat(row['timestamp']),
                    mission_day=int(row['mission_day']),
                    hbc=float(row['hbc']),
                    rbc_count=0.0,  # Not in CSV, use default
                    hematocrit=0.0,
                    tremor_amplitude=float(row['tremor_amplitude']),
                    tremor_frequency=float(row['tremor_frequency']),
                    grip_strength=float(row['grip_strength']),
                    reaction_time=float(row['reaction_time']),
                    cognitive_score=float(row['cognitive_score']),
                    cumulative_radiation_dose=float(row['cumulative_radiation_dose']),
                    current_dose_rate=float(row['current_dose_rate']),
                    spe_exposure_count=int(row['spe_exposure_count']),
                    cortisol=float(row['cortisol']),
                    heart_rate_variability=float(row['heart_rate_variability']),
                    isolation_hours=0.0
                )

                self.process_metrics(metrics)
                count += 1

        print(f"✅ Imported and processed {count} metrics from {filename}")
        return count


# Example usage and testing
if __name__ == "__main__":
    print("🏥 Crew Health Monitoring with PRIMAL Logic - Live Test")
    print("=" * 80)

    # Initialize monitor with 5% drift tolerance
    monitor = CrewHealthMonitor(drift_tolerance=0.05)

    # Register crew member
    crew_id = "CREW-001-COMMANDER"
    monitor.register_crew(crew_id)

    # Simulate mission progression with SPE events
    start_date = datetime(2025, 1, 1)

    print("\n📅 Simulating 120-day mission with variable SPE events...")
    print("-" * 80)

    # Baseline (Day 0-30)
    for day in range(30):
        metrics = CrewHealthMetrics(
            crew_id=crew_id,
            timestamp=start_date + timedelta(days=day),
            mission_day=day,
            hbc=15.2 + np.random.normal(0, 0.3),
            rbc_count=5.1,
            hematocrit=45.0,
            tremor_amplitude=0.5 + np.random.normal(0, 0.05),
            tremor_frequency=10.0,
            grip_strength=45.0,
            reaction_time=250 + np.random.normal(0, 10),
            cognitive_score=95.0,
            cumulative_radiation_dose=day * 0.5,  # 0.5 mSv/day background
            current_dose_rate=0.5,
            spe_exposure_count=0,
            cortisol=12.0,
            heart_rate_variability=75.0,
            isolation_hours=day * 24
        )

        result = monitor.process_metrics(metrics)

    print(f"✅ Baseline established (Day 0-30)")
    print(f"   HBC baseline: {monitor.crew_baselines[crew_id].hbc_baseline:.2f} g/dL")
    print(f"   Tremor baseline: {monitor.crew_baselines[crew_id].tremor_baseline:.3f} mm")

    # Inject SPE event on Day 45
    print(f"\n🌞 INJECTING SPE EVENT (Day 45): 150 mSv dose")
    spe_impact = monitor.inject_spe_event(
        crew_id,
        spe_dose_msv=150.0,
        event_time=start_date + timedelta(days=45)
    )
    print(f"   Predicted HBC drop: {spe_impact['predicted_hbc_drop_pct']:.2f}%")
    print(f"   Predicted tremor increase: {spe_impact['predicted_tremor_increase_pct']:.2f}%")
    print(f"   Recommendation: {spe_impact['recommendation']}")

    # Continue simulation to Day 120 with degradation
    for day in range(30, 121):
        # Model expected degradation (8% HBC drop, 15% tremor increase by month 4)
        months = day / 30.0
        hbc_degradation = 15.2 * (1.0 - min(0.08, (months / 4.0) * 0.08))
        tremor_increase = 0.5 * (1.0 + min(0.15, (months / 4.0) * 0.15))

        # Add SPE impact if within 14 days of event
        spe_count = 0
        cumulative_dose = day * 0.5
        if 45 <= day <= 59:
            hbc_degradation *= 0.985  # Additional 1.5% drop from SPE
            tremor_increase *= 1.03  # Additional 3% increase from SPE
            spe_count = 1
            cumulative_dose += 150.0

        metrics = CrewHealthMetrics(
            crew_id=crew_id,
            timestamp=start_date + timedelta(days=day),
            mission_day=day,
            hbc=hbc_degradation + np.random.normal(0, 0.3),
            rbc_count=5.1 - (months / 4.0) * 0.2,
            hematocrit=45.0 - (months / 4.0) * 2.0,
            tremor_amplitude=tremor_increase + np.random.normal(0, 0.05),
            tremor_frequency=10.0 - (months / 4.0) * 0.5,
            grip_strength=45.0 - (months / 4.0) * 2.0,
            reaction_time=250 + (months / 4.0) * 30 + np.random.normal(0, 10),
            cognitive_score=95.0 - (months / 4.0) * 5.0,
            cumulative_radiation_dose=cumulative_dose,
            current_dose_rate=0.5,
            spe_exposure_count=spe_count,
            cortisol=12.0 + (months / 4.0) * 3.0,
            heart_rate_variability=75.0 - (months / 4.0) * 10.0,
            isolation_hours=day * 24
        )

        result = monitor.process_metrics(metrics)

        # Print alerts
        if result['threshold_alerts']:
            print(f"\n⚠️  ALERT on Day {day}:")
            for alert in result['threshold_alerts']:
                print(f"   {alert['severity']}: {alert['metric']} - "
                      f"{alert.get('drop_percent', alert.get('increase_percent', 0)):.1f}% deviation")

        if result['intervention']['intervention_level'] != 'NONE':
            print(f"\n🚨 INTERVENTION NEEDED on Day {day}:")
            print(f"   Level: {result['intervention']['intervention_level']}")
            print(f"   Action: {result['intervention']['action']}")
            print(f"   Reason: {result['intervention']['reasoning']}")

    # Final report
    print(f"\n📊 MISSION DAY 120 SUMMARY:")
    print("=" * 80)
    final_metrics = monitor.metrics_history[-1]
    baseline = monitor.crew_baselines[crew_id]

    print(f"Crew ID: {crew_id}")
    print(f"\nHematological:")
    print(f"  HBC: {final_metrics.hbc:.2f} g/dL (baseline: {baseline.hbc_baseline:.2f})")
    print(f"  Drop: {((baseline.hbc_baseline - final_metrics.hbc) / baseline.hbc_baseline * 100):.1f}%")

    print(f"\nNeuromuscular:")
    print(f"  Tremor: {final_metrics.tremor_amplitude:.3f} mm (baseline: {baseline.tremor_baseline:.3f})")
    print(f"  Increase: {((final_metrics.tremor_amplitude - baseline.tremor_baseline) / baseline.tremor_baseline * 100):.1f}%")

    print(f"\nRadiation:")
    print(f"  Cumulative dose: {final_metrics.cumulative_radiation_dose:.1f} mSv")
    print(f"  SPE exposures: {final_metrics.spe_exposure_count}")

    print(f"\nPRIMAL Logic:")
    print(f"  Consciousness: {baseline.consciousness:.3f}")
    print(f"  Drift tolerance: {monitor.drift_tolerance * 100:.0f}%")

    # Export to CSV
    monitor.export_to_csv("crew_health_120day_mission.csv")

    print("\n✅ Live test complete. Ready for variable SPE event injection!")
    print(f"💾 Data exported to: crew_health_120day_mission.csv")
    print(f"\n🎯 DRIFT TOLERANCE: {monitor.drift_tolerance * 100:.0f}% (5% threshold for intervention)")
