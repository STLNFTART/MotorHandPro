#!/usr/bin/env python3
"""
Mars Mission Parameter Sweep
Uses unified experiment framework to test consciousness adaptation strategies
"""
import sys
import math
import random
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent))

from experiments.framework import ParamGrid, run_parameter_sweep


def simulate_mars_mission(config):
    """
    Simplified Mars mission radiation/consciousness simulation

    Args:
        config: dict with keys:
            - mission_days: mission duration
            - consciousness_target: target consciousness level (0.0-1.0)
            - learning_rate: adaptation speed
            - spe_intensity: solar particle event intensity
            - shield_thickness_gcm2: shielding (g/cm²)

    Returns:
        (metrics, time_series) tuple
    """
    mission_days = config['mission_days']
    consciousness_target = config['consciousness_target']
    learning_rate = config['learning_rate']
    spe_intensity = config.get('spe_intensity', 1.0)
    shield_thickness = config.get('shield_thickness_gcm2', 10.0)

    # Simplified physics model
    # GCR background: ~0.48 mSv/day in deep space
    gcr_dose_per_day = 0.48

    # Shielding effectiveness (simplified)
    shielding_factor = 1.0 / (1.0 + shield_thickness / 10.0)

    # SPE events (simplified - assume 3 events during mission)
    spe_days = [30, 90, 150]
    spe_dose = 50.0 * spe_intensity * shielding_factor  # mSv per event

    # Initialize state
    consciousness = 0.5  # Start at baseline
    total_dose = 0.0
    cognitive_degradation = 0.0

    # Time series
    time_series = {
        "day": [],
        "consciousness": [],
        "total_dose_msv": [],
        "cognitive_performance": []
    }

    for day in range(mission_days):
        # Daily GCR dose
        daily_dose = gcr_dose_per_day * shielding_factor

        # SPE event?
        if day in spe_days:
            daily_dose += spe_dose

        total_dose += daily_dose

        # Consciousness adaptation (moves toward target)
        consciousness_delta = (consciousness_target - consciousness) * learning_rate
        consciousness += consciousness_delta
        consciousness = max(0.0, min(1.0, consciousness))

        # Cognitive degradation from radiation (reduced by consciousness)
        # Higher consciousness = better radiation resilience
        rad_degradation_factor = 0.001 * daily_dose * (1.0 - consciousness * 0.5)
        cognitive_degradation += rad_degradation_factor

        # Cognitive performance (starts at 1.0, degrades with radiation)
        cognitive_performance = 1.0 - cognitive_degradation
        cognitive_performance = max(0.0, cognitive_performance)

        # Record
        time_series["day"].append(day)
        time_series["consciousness"].append(round(consciousness, 4))
        time_series["total_dose_msv"].append(round(total_dose, 2))
        time_series["cognitive_performance"].append(round(cognitive_performance, 4))

    # Calculate metrics
    final_consciousness = consciousness
    final_cognitive = cognitive_performance
    avg_consciousness = sum(time_series["consciousness"]) / len(time_series["consciousness"])

    # Mission success criteria:
    # - Final cognitive performance > 0.7
    # - Total dose < 500 mSv (NASA limit)
    # - Average consciousness > 0.6
    mission_success = (
        final_cognitive > 0.7 and
        total_dose < 500.0 and
        avg_consciousness > 0.6
    )

    # Stable if consciousness reached within 10% of target
    stable = abs(final_consciousness - consciousness_target) < 0.1

    metrics = {
        "total_dose_msv": round(total_dose, 2),
        "final_consciousness": round(final_consciousness, 4),
        "avg_consciousness": round(avg_consciousness, 4),
        "final_cognitive_performance": round(final_cognitive, 4),
        "consciousness_convergence": round(abs(final_consciousness - consciousness_target), 4),
        "mission_success": mission_success,
        "stable": stable
    }

    return metrics, time_series


def main():
    """Run parameter sweep on Mars mission configurations"""

    # Define parameter grid
    grid = ParamGrid({
        "mission_days": [180, 360],  # 6 months, 12 months
        "consciousness_target": [0.55, 0.70, 0.82],
        "learning_rate": [0.03, 0.05, 0.07],
        "spe_intensity": [0.5, 1.0, 2.0],  # Low, moderate, high solar activity
        "shield_thickness_gcm2": [5.0, 10.0, 20.0]  # Light, medium, heavy shielding
    })

    # Run sweep
    output_dir = run_parameter_sweep(
        sim_name="mars_mission",
        param_grid=grid,
        simulate_fn=simulate_mars_mission,
        tag="consciousness_sweep"
    )

    print(f"\n✓ Sweep complete! Results in: {output_dir}")
    print(f"\n  View summary: {output_dir}/summary/summary.csv")
    print(f"  View stats:   {output_dir}/summary/stats.json")
    print(f"  View report:  {output_dir}/REPORT.md")


if __name__ == "__main__":
    main()
