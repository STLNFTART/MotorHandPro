#!/usr/bin/env python3
"""
Execute Field Coupling Parameter Sweeps
Runs all 567 field coupling parameter combinations
"""

import json
import numpy as np
import csv
import os
from datetime import datetime
from pathlib import Path

def simulate_field_coupled_control(alpha, lam, field_strength, dt=0.01, t_end=5.0):
    """
    Simulate field-coupled Primal Logic control

    Extended control law with field coupling:
    dψ/dt = -λ·ψ(t) + α·e(t) + F_field(t)

    where F_field(t) = field_strength * gravity_coupling(t)
    """

    n_steps = int(t_end / dt) + 1

    # State variables
    psi = 1.0          # Control signal
    e = 0.1            # Error signal
    Ec = 0.0           # Control energy
    v = 0.0            # Velocity (for field coupling)

    # Results storage
    results = []

    for i in range(n_steps):
        t = i * dt

        # Field coupling (inverse square law simulation)
        r = 1.0 + 0.5 * np.sin(2 * np.pi * t / t_end)  # Varying distance
        F_field = field_strength / (r ** 2)

        # Primal Logic control law with field coupling
        dpsi_dt = -lam * psi + alpha * e + F_field
        de_dt = -0.5 * e - 0.1 * psi
        dv_dt = psi + F_field

        # Store state
        results.append([t, psi, e, Ec, v, F_field, r])

        # Update state
        psi = psi + dpsi_dt * dt
        e = e + de_dt * dt
        v = v + dv_dt * dt
        Ec = Ec + abs(psi) * dt

    return np.array(results)

def compute_field_metrics(data):
    """Compute metrics for field-coupled simulation"""

    t = data[:, 0]
    psi = data[:, 1]
    e = data[:, 2]
    Ec = data[:, 3]
    v = data[:, 4]
    F_field = data[:, 5]

    # Compute Lipschitz estimate
    delta_psi = np.abs(np.diff(psi))
    delta_t = np.diff(t)
    rates = delta_psi / (delta_t + 1e-12)
    max_rate = np.max(rates) if len(rates) > 0 else 0
    max_psi = np.max(np.abs(psi))
    lipschitz = max_rate / (max_psi + 1e-12)

    # Stability check
    is_stable = lipschitz < 1.0

    # Energy metrics
    total_energy = np.trapz(np.abs(psi), t)
    field_work = np.trapz(F_field, t)

    metrics = {
        'max_psi': float(np.max(np.abs(psi))),
        'final_psi': float(psi[-1]),
        'max_error': float(np.max(np.abs(e))),
        'final_error': float(e[-1]),
        'total_energy': float(total_energy),
        'field_work': float(field_work),
        'max_velocity': float(np.max(np.abs(v))),
        'lipschitz': float(lipschitz),
        'is_stable': bool(is_stable)
    }

    return metrics

def run_all_field_coupling_sweeps():
    """Execute all field coupling parameter combinations"""

    # Load configurations
    config_file = "field_coupling_sweeps/field_coupling_sweep_configs.json"
    with open(config_file, 'r') as f:
        sweep_data = json.load(f)

    configurations = sweep_data['configurations']
    total = len(configurations)

    print(f"\n{'='*80}")
    print(f"EXECUTING FIELD COUPLING PARAMETER SWEEPS")
    print(f"{'='*80}")
    print(f"Total configurations: {total}")
    print(f"Starting simulations...\n")

    # Create output directory
    output_dir = "field_coupling_results"
    os.makedirs(output_dir, exist_ok=True)

    all_results = []

    for i, config in enumerate(configurations):
        alpha = config['alpha']
        lam = config['lambda']
        field_strength = config['field_strength']
        config_id = config['config_id']

        # Run simulation
        data = simulate_field_coupled_control(alpha, lam, field_strength)

        # Compute metrics
        metrics = compute_field_metrics(data)
        metrics['alpha'] = alpha
        metrics['lambda'] = lam
        metrics['field_strength'] = field_strength
        metrics['config_id'] = config_id

        all_results.append(metrics)

        # Save individual result
        csv_file = f"{output_dir}/{config_id}.csv"
        with open(csv_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([f"# Field-Coupled Primal Logic: alpha={alpha:.6f}, lambda={lam:.6f}, field={field_strength:.2f}"])
            writer.writerow([f"# Lipschitz: {metrics['lipschitz']:.6f}, Stable: {metrics['is_stable']}"])
            writer.writerow([f"# Generated: {datetime.now().isoformat()}"])
            writer.writerow([])
            writer.writerow(["t", "psi", "error", "Ec", "velocity", "F_field", "distance"])
            writer.writerows(data)

        if (i + 1) % 50 == 0:
            print(f"Completed {i+1}/{total} simulations...")

    print(f"✅ Completed all {total} field coupling simulations\n")

    # Save summary
    summary_file = f"{output_dir}/field_coupling_summary.csv"
    with open(summary_file, 'w', newline='') as f:
        fieldnames = ['config_id', 'alpha', 'lambda', 'field_strength',
                     'lipschitz', 'is_stable', 'max_psi', 'final_error',
                     'total_energy', 'field_work']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in all_results:
            writer.writerow({k: r.get(k, '') for k in fieldnames})

    print(f"✅ Saved summary: {summary_file}")

    # Compute statistics
    stable_count = sum(r['is_stable'] for r in all_results)
    lipschitz_values = [r['lipschitz'] for r in all_results]

    stats = {
        'total_configurations': total,
        'stable_configurations': stable_count,
        'stability_rate': (stable_count / total) * 100,
        'lipschitz_min': min(lipschitz_values),
        'lipschitz_max': max(lipschitz_values),
        'lipschitz_mean': np.mean(lipschitz_values),
        'lipschitz_median': np.median(lipschitz_values)
    }

    stats_file = f"{output_dir}/sweep_statistics.json"
    with open(stats_file, 'w') as f:
        json.dump(stats, f, indent=2)

    print(f"✅ Saved statistics: {stats_file}")

    # Print summary
    print(f"\n{'='*80}")
    print(f"FIELD COUPLING SWEEP STATISTICS")
    print(f"{'='*80}")
    print(f"Total configurations: {stats['total_configurations']}")
    print(f"Stable (L < 1.0): {stats['stable_configurations']} ({stats['stability_rate']:.1f}%)")
    print(f"Lipschitz range: [{stats['lipschitz_min']:.6f}, {stats['lipschitz_max']:.6f}]")
    print(f"Lipschitz mean: {stats['lipschitz_mean']:.6f}")
    print(f"{'='*80}\n")

    return all_results, stats

if __name__ == "__main__":
    print(f"\n{'#'*80}")
    print(f"  FIELD COUPLING PARAMETER SWEEP EXECUTION")
    print(f"  567 Parameter Combinations")
    print(f"{'#'*80}\n")

    start_time = datetime.now()
    results, stats = run_all_field_coupling_sweeps()
    end_time = datetime.now()

    duration = (end_time - start_time).total_seconds()

    print(f"{'='*80}")
    print(f"✅ FIELD COUPLING SWEEP COMPLETE")
    print(f"{'='*80}")
    print(f"Time elapsed: {duration:.2f} seconds")
    print(f"Simulations per second: {len(results)/duration:.1f}")
    print(f"{'='*80}\n")
