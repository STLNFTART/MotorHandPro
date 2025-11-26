#!/usr/bin/env python3
"""
Comprehensive Parameter Sweep - ALL Iterations and Combinations
Maximum Output Mode - Run All Variable Vectors and Parameter Combinations

This script generates and executes COMPLETE parameter sweeps across:
1. Primal Logic kernel parameters (MU, KE, D0)
2. Mars mission configurations (duration, crew, solar activity, shielding)
3. Consciousness baseline sweeps
4. Field coupling parameter combinations
5. Quantum state configurations

Generates maximum output with all possible combinations.
"""

import numpy as np
import csv
import os
import subprocess
import json
from pathlib import Path
from itertools import product
from datetime import datetime

# ============================================================================
# PRIMAL LOGIC PARAMETER SPACE
# ============================================================================

# MU (λ - Lightfoot constant) sweep points
MU_VALUES = [
    0.10,     # Lower bound exploration
    0.12,     # Alternative reference
    0.15,     # Previously tested
    0.16905,  # Primary Lightfoot constant (default)
    0.20,     # Previously tested
    0.25,     # Higher decay
    0.30,     # Upper bound exploration
]

# KE (Error gain) sweep points
KE_VALUES = [
    0.0,      # Pure decay (default reference)
    0.1,      # Low gain
    0.2,      # Low-medium gain
    0.3,      # Previously tested
    0.4,      # Medium gain
    0.5,      # Previously tested (fixed in last session)
    0.6,      # Medium-high gain
    0.7,      # High gain
    0.8,      # Very high gain
    0.9,      # Near-maximum gain
    1.0,      # Maximum gain
]

# D0 (Donte constant) variations
D0_VALUES = [
    149.9992314000,  # Primary constant (default)
    140.0,           # Lower bound
    145.0,           # Mid-lower
    155.0,           # Mid-upper
    160.0,           # Upper bound
]

# Core constants
I3_DEFAULT = 6.4939394023
S_DEFAULT = 23.0983417165
FPRIME_D0_DEFAULT = 0.000129931830

# ============================================================================
# MARS MISSION PARAMETER SPACE
# ============================================================================

MISSION_DURATIONS = [90, 180, 270, 360, 500, 860]  # days
CREW_SIZES = [2, 3, 4, 5, 6]  # crew members
SOLAR_ACTIVITY_LEVELS = ['low', 'moderate', 'high', 'extreme']
SHIELDING_CONFIGS = [5, 10, 15, 20, 25, 30]  # g/cm² aluminum equivalent

# Consciousness baseline sweep (φ-scaled targets)
CONSCIOUSNESS_BASELINES = [
    0.500,  # Minimum viable consciousness
    0.550,  # Low baseline
    0.600,  # Low-medium
    0.650,  # Medium-low
    0.700,  # Medium (SCI-BETA default)
    0.750,  # Medium-high (MED-DELTA default)
    0.800,  # High-medium
    0.820,  # High (CDR-ALPHA default)
    0.850,  # Very high
    0.900,  # Maximum consciousness
]

# ============================================================================
# FIELD COUPLING PARAMETER SPACE
# ============================================================================

ALPHA_VALUES = np.linspace(0.52, 0.56, 9)  # 9 points between min and max
LAMBDA_VALUES = np.linspace(0.11, 0.12, 9)  # 9 points between min and max
FIELD_STRENGTHS = [0.0, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0]  # Normalized field strength

# ============================================================================
# QUANTUM STATE PARAMETER SPACE
# ============================================================================

QUANTUM_ALPHA_RANGE = np.linspace(0.5, 0.6, 11)  # 11 points
QUANTUM_LAMBDA_RANGE = np.linspace(0.10, 0.15, 11)  # 11 points
QUANTUM_EPOCHS = [5, 10, 15, 20, 30, 50, 100]  # Iteration counts

# ============================================================================
# PARAMETER SWEEP GENERATION FUNCTIONS
# ============================================================================

def generate_primal_kernel_sweeps(output_dir="primal_kernel_sweeps"):
    """Generate ALL Primal Logic kernel parameter combinations"""
    os.makedirs(output_dir, exist_ok=True)

    total_combinations = len(MU_VALUES) * len(KE_VALUES) * len(D0_VALUES)
    print(f"\n{'='*80}")
    print(f"PRIMAL LOGIC KERNEL PARAMETER SWEEP")
    print(f"{'='*80}")
    print(f"MU values: {len(MU_VALUES)}")
    print(f"KE values: {len(KE_VALUES)}")
    print(f"D0 values: {len(D0_VALUES)}")
    print(f"Total combinations: {total_combinations}")
    print(f"{'='*80}\n")

    combinations = []
    file_count = 0

    for mu, ke, d0 in product(MU_VALUES, KE_VALUES, D0_VALUES):
        # Generate filename
        filename = f"run_mu{mu:.5f}_ke{ke:.2f}_d0{d0:.4f}.csv"
        filepath = os.path.join(output_dir, filename)

        # Simulation parameters
        dt = 0.01
        t_end = 5.0
        n_steps = int(t_end / dt) + 1

        # Initial conditions
        psi = 1.0
        gamma = 0.004
        Ec = 0.0

        # Generate data
        data = []
        for i in range(n_steps):
            t = i * dt
            data.append([t, psi, gamma, Ec])

            # Primal Logic control law: dψ/dt = -μ·ψ + KE·γ
            dpsi_dt = -mu * psi + ke * gamma
            dgamma_dt = -0.5 * gamma  # Exponential decay

            psi = psi + dpsi_dt * dt
            gamma = gamma + dgamma_dt * dt
            Ec = Ec + psi * gamma * dt

        # Write CSV file
        with open(filepath, 'w', newline='') as f:
            writer = csv.writer(f)

            # Header with metadata
            writer.writerow([f"# Primal Logic Kernel Test: MU={mu:.8f}, KE={ke:.2f}"])
            writer.writerow([f"# Core: D0={d0:.10f}, I3={I3_DEFAULT:.10f}, S={S_DEFAULT:.10f}, F'(D0)={FPRIME_D0_DEFAULT:.12f}"])
            writer.writerow([f"# Generated: {datetime.now().isoformat()}"])
            writer.writerow([f"# dt={dt}, t_end={t_end}, n_steps={n_steps}"])
            writer.writerow(["# Control law: dψ/dt = -μ·ψ(t) + KE·e(t)"])
            writer.writerow([])

            # Column headers
            writer.writerow(["t", "psi", "gamma", "Ec"])

            # Data rows
            writer.writerows(data)

        file_count += 1
        combinations.append({
            'mu': mu,
            'ke': ke,
            'd0': d0,
            'filename': filename,
            'filepath': filepath,
            'n_steps': n_steps,
            'final_psi': data[-1][1],
            'final_Ec': data[-1][3]
        })

        if file_count % 10 == 0:
            print(f"Generated {file_count}/{total_combinations} kernel configurations...")

    print(f"✅ Generated {file_count} Primal Logic kernel sweep files")
    print(f"   Output directory: {output_dir}/")

    # Save sweep metadata
    metadata_file = os.path.join(output_dir, "sweep_metadata.json")
    with open(metadata_file, 'w') as f:
        json.dump({
            'total_combinations': total_combinations,
            'mu_values': MU_VALUES,
            'ke_values': KE_VALUES,
            'd0_values': D0_VALUES,
            'generated': datetime.now().isoformat(),
            'combinations': combinations
        }, f, indent=2)

    return combinations


def generate_mars_mission_sweep_configs(output_dir="mars_mission_sweeps"):
    """Generate ALL Mars mission parameter combinations"""
    os.makedirs(output_dir, exist_ok=True)

    total_combinations = (len(MISSION_DURATIONS) * len(CREW_SIZES) *
                         len(SOLAR_ACTIVITY_LEVELS) * len(SHIELDING_CONFIGS))

    print(f"\n{'='*80}")
    print(f"MARS MISSION PARAMETER SWEEP")
    print(f"{'='*80}")
    print(f"Mission durations: {len(MISSION_DURATIONS)}")
    print(f"Crew sizes: {len(CREW_SIZES)}")
    print(f"Solar activity levels: {len(SOLAR_ACTIVITY_LEVELS)}")
    print(f"Shielding configs: {len(SHIELDING_CONFIGS)}")
    print(f"Total combinations: {total_combinations}")
    print(f"{'='*80}\n")

    configurations = []

    for duration, crew_size, solar_level, shielding in product(
        MISSION_DURATIONS, CREW_SIZES, SOLAR_ACTIVITY_LEVELS, SHIELDING_CONFIGS
    ):
        config = {
            'mission_duration_days': duration,
            'crew_size': crew_size,
            'solar_activity': solar_level,
            'shielding_gcm2': shielding,
            'config_id': f"mars_{duration}d_crew{crew_size}_{solar_level}_shield{shielding}gcm2"
        }
        configurations.append(config)

    # Save configurations
    config_file = os.path.join(output_dir, "mission_sweep_configs.json")
    with open(config_file, 'w') as f:
        json.dump({
            'total_configurations': total_combinations,
            'configurations': configurations,
            'generated': datetime.now().isoformat()
        }, f, indent=2)

    print(f"✅ Generated {total_combinations} Mars mission sweep configurations")
    print(f"   Config file: {config_file}")

    return configurations


def generate_consciousness_baseline_sweep(output_dir="consciousness_sweeps"):
    """Generate consciousness baseline sweep configurations"""
    os.makedirs(output_dir, exist_ok=True)

    total_combinations = len(CONSCIOUSNESS_BASELINES) * len(CREW_SIZES)

    print(f"\n{'='*80}")
    print(f"CONSCIOUSNESS BASELINE PARAMETER SWEEP")
    print(f"{'='*80}")
    print(f"Consciousness baselines: {len(CONSCIOUSNESS_BASELINES)}")
    print(f"Crew sizes: {len(CREW_SIZES)}")
    print(f"Total combinations: {total_combinations}")
    print(f"{'='*80}\n")

    configurations = []

    for baseline, crew_size in product(CONSCIOUSNESS_BASELINES, CREW_SIZES):
        # Generate diverse crew with different baselines around the target
        crew_baselines = []
        for i in range(crew_size):
            # Spread crew baselines around the target (±0.1 range)
            offset = (i - crew_size/2) * 0.04
            crew_baseline = max(0.5, min(0.9, baseline + offset))
            crew_baselines.append(round(crew_baseline, 3))

        config = {
            'target_baseline': baseline,
            'crew_size': crew_size,
            'crew_baselines': crew_baselines,
            'config_id': f"consciousness_phi{baseline:.3f}_crew{crew_size}"
        }
        configurations.append(config)

    config_file = os.path.join(output_dir, "consciousness_sweep_configs.json")
    with open(config_file, 'w') as f:
        json.dump({
            'total_configurations': total_combinations,
            'configurations': configurations,
            'generated': datetime.now().isoformat()
        }, f, indent=2)

    print(f"✅ Generated {total_combinations} consciousness baseline sweep configurations")
    print(f"   Config file: {config_file}")

    return configurations


def generate_field_coupling_sweep(output_dir="field_coupling_sweeps"):
    """Generate field coupling parameter combinations"""
    os.makedirs(output_dir, exist_ok=True)

    total_combinations = len(ALPHA_VALUES) * len(LAMBDA_VALUES) * len(FIELD_STRENGTHS)

    print(f"\n{'='*80}")
    print(f"FIELD COUPLING PARAMETER SWEEP")
    print(f"{'='*80}")
    print(f"Alpha values: {len(ALPHA_VALUES)}")
    print(f"Lambda values: {len(LAMBDA_VALUES)}")
    print(f"Field strengths: {len(FIELD_STRENGTHS)}")
    print(f"Total combinations: {total_combinations}")
    print(f"{'='*80}\n")

    configurations = []

    for alpha, lam, field_strength in product(ALPHA_VALUES, LAMBDA_VALUES, FIELD_STRENGTHS):
        config = {
            'alpha': float(alpha),
            'lambda': float(lam),
            'field_strength': field_strength,
            'config_id': f"field_alpha{alpha:.4f}_lambda{lam:.4f}_strength{field_strength:.1f}"
        }
        configurations.append(config)

    config_file = os.path.join(output_dir, "field_coupling_sweep_configs.json")
    with open(config_file, 'w') as f:
        json.dump({
            'total_configurations': total_combinations,
            'configurations': configurations,
            'generated': datetime.now().isoformat()
        }, f, indent=2)

    print(f"✅ Generated {total_combinations} field coupling sweep configurations")
    print(f"   Config file: {config_file}")

    return configurations


def generate_quantum_state_sweep(output_dir="quantum_state_sweeps"):
    """Generate quantum state parameter combinations"""
    os.makedirs(output_dir, exist_ok=True)

    total_combinations = len(QUANTUM_ALPHA_RANGE) * len(QUANTUM_LAMBDA_RANGE) * len(QUANTUM_EPOCHS)

    print(f"\n{'='*80}")
    print(f"QUANTUM STATE PARAMETER SWEEP")
    print(f"{'='*80}")
    print(f"Alpha values: {len(QUANTUM_ALPHA_RANGE)}")
    print(f"Lambda values: {len(QUANTUM_LAMBDA_RANGE)}")
    print(f"Epoch counts: {len(QUANTUM_EPOCHS)}")
    print(f"Total combinations: {total_combinations}")
    print(f"{'='*80}\n")

    configurations = []

    for alpha, lam, epochs in product(QUANTUM_ALPHA_RANGE, QUANTUM_LAMBDA_RANGE, QUANTUM_EPOCHS):
        config = {
            'alpha': float(alpha),
            'lambda': float(lam),
            'epochs': epochs,
            'config_id': f"quantum_alpha{alpha:.3f}_lambda{lam:.3f}_epochs{epochs}"
        }
        configurations.append(config)

    config_file = os.path.join(output_dir, "quantum_state_sweep_configs.json")
    with open(config_file, 'w') as f:
        json.dump({
            'total_configurations': total_combinations,
            'configurations': configurations,
            'generated': datetime.now().isoformat()
        }, f, indent=2)

    print(f"✅ Generated {total_combinations} quantum state sweep configurations")
    print(f"   Config file: {config_file}")

    return configurations


def generate_comprehensive_summary():
    """Generate summary of all parameter sweep spaces"""

    primal_total = len(MU_VALUES) * len(KE_VALUES) * len(D0_VALUES)
    mars_total = (len(MISSION_DURATIONS) * len(CREW_SIZES) *
                  len(SOLAR_ACTIVITY_LEVELS) * len(SHIELDING_CONFIGS))
    consciousness_total = len(CONSCIOUSNESS_BASELINES) * len(CREW_SIZES)
    field_total = len(ALPHA_VALUES) * len(LAMBDA_VALUES) * len(FIELD_STRENGTHS)
    quantum_total = len(QUANTUM_ALPHA_RANGE) * len(QUANTUM_LAMBDA_RANGE) * len(QUANTUM_EPOCHS)

    grand_total = primal_total + mars_total + consciousness_total + field_total + quantum_total

    summary = f"""
{'='*80}
COMPREHENSIVE PARAMETER SWEEP SUMMARY
Maximum Output Mode - All Iterations and Combinations
{'='*80}

PARAMETER SPACE COVERAGE:

1. Primal Logic Kernel Parameters:
   - MU (λ) sweep: {len(MU_VALUES)} values [{min(MU_VALUES):.3f} - {max(MU_VALUES):.3f}]
   - KE sweep: {len(KE_VALUES)} values [{min(KE_VALUES):.1f} - {max(KE_VALUES):.1f}]
   - D0 sweep: {len(D0_VALUES)} values [{min(D0_VALUES):.1f} - {max(D0_VALUES):.1f}]
   - Total combinations: {primal_total:,}

2. Mars Mission Parameters:
   - Durations: {len(MISSION_DURATIONS)} values {MISSION_DURATIONS}
   - Crew sizes: {len(CREW_SIZES)} values {CREW_SIZES}
   - Solar activity: {len(SOLAR_ACTIVITY_LEVELS)} levels {SOLAR_ACTIVITY_LEVELS}
   - Shielding: {len(SHIELDING_CONFIGS)} configs {SHIELDING_CONFIGS}
   - Total combinations: {mars_total:,}

3. Consciousness Baseline Parameters:
   - Baselines: {len(CONSCIOUSNESS_BASELINES)} values [{min(CONSCIOUSNESS_BASELINES):.3f} - {max(CONSCIOUSNESS_BASELINES):.3f}]
   - Crew sizes: {len(CREW_SIZES)} values {CREW_SIZES}
   - Total combinations: {consciousness_total:,}

4. Field Coupling Parameters:
   - Alpha: {len(ALPHA_VALUES)} values [{ALPHA_VALUES[0]:.4f} - {ALPHA_VALUES[-1]:.4f}]
   - Lambda: {len(LAMBDA_VALUES)} values [{LAMBDA_VALUES[0]:.4f} - {LAMBDA_VALUES[-1]:.4f}]
   - Field strengths: {len(FIELD_STRENGTHS)} values {FIELD_STRENGTHS}
   - Total combinations: {field_total:,}

5. Quantum State Parameters:
   - Alpha: {len(QUANTUM_ALPHA_RANGE)} values [{QUANTUM_ALPHA_RANGE[0]:.3f} - {QUANTUM_ALPHA_RANGE[-1]:.3f}]
   - Lambda: {len(QUANTUM_LAMBDA_RANGE)} values [{QUANTUM_LAMBDA_RANGE[0]:.3f} - {QUANTUM_LAMBDA_RANGE[-1]:.3f}]
   - Epochs: {len(QUANTUM_EPOCHS)} values {QUANTUM_EPOCHS}
   - Total combinations: {quantum_total:,}

{'='*80}
GRAND TOTAL: {grand_total:,} parameter combinations
{'='*80}

Generated: {datetime.now().isoformat()}
"""

    # Save summary
    with open("PARAMETER_SWEEP_SUMMARY.txt", 'w') as f:
        f.write(summary)

    print(summary)

    return {
        'primal_logic': primal_total,
        'mars_missions': mars_total,
        'consciousness': consciousness_total,
        'field_coupling': field_total,
        'quantum_state': quantum_total,
        'grand_total': grand_total
    }


# ============================================================================
# MAIN EXECUTION
# ============================================================================

def main():
    """Execute comprehensive parameter sweep generation"""

    print(f"\n{'#'*80}")
    print(f"{'#'*80}")
    print(f"  COMPREHENSIVE PARAMETER SWEEP - MAXIMUM OUTPUT MODE")
    print(f"  ALL ITERATIONS, ALL COMBINATIONS, ALL VARIABLE VECTORS")
    print(f"{'#'*80}")
    print(f"{'#'*80}\n")

    start_time = datetime.now()

    # Generate all parameter sweeps
    print("Starting comprehensive parameter sweep generation...\n")

    # 1. Primal Logic kernel sweeps
    primal_combos = generate_primal_kernel_sweeps()

    # 2. Mars mission sweeps
    mars_configs = generate_mars_mission_sweep_configs()

    # 3. Consciousness baseline sweeps
    consciousness_configs = generate_consciousness_baseline_sweep()

    # 4. Field coupling sweeps
    field_configs = generate_field_coupling_sweep()

    # 5. Quantum state sweeps
    quantum_configs = generate_quantum_state_sweep()

    # Generate summary
    totals = generate_comprehensive_summary()

    end_time = datetime.now()
    duration = (end_time - start_time).total_seconds()

    print(f"\n{'='*80}")
    print(f"✅ PARAMETER SWEEP GENERATION COMPLETE")
    print(f"{'='*80}")
    print(f"Time elapsed: {duration:.2f} seconds")
    print(f"Total configurations generated: {totals['grand_total']:,}")
    print(f"{'='*80}\n")


if __name__ == "__main__":
    main()
