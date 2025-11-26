#!/usr/bin/env python3
"""
Execute Quantum State Parameter Sweeps
Runs all 847 quantum state parameter combinations
"""

import json
import numpy as np
import csv
import os
from datetime import datetime
from pathlib import Path

# Donte constant (from quant_full.h)
DONTE_CONSTANT = 149.9992314000

def quantum_semantic_resonance(alpha, lam, epochs, initial_distance=None):
    """
    Simulate quantum-semantic resonance with temporal weighting

    State update:
    D(t+1) = D(t) - α·e^(-λt)·∇L

    where:
    - D(t) = semantic distance from Donte constant
    - α = learning rate / drive
    - λ = temporal decay constant
    - ∇L = gradient (simulated as error feedback)
    """

    if initial_distance is None:
        initial_distance = DONTE_CONSTANT - 10.0  # Start 10 units below target

    # State initialization
    D = initial_distance
    accumulated_drift = 0.0

    # Results storage
    results = []

    for epoch in range(epochs + 1):
        # Compute error (distance from target)
        error = DONTE_CONSTANT - D

        # Temporal weighting factor
        temporal_weight = np.exp(-lam * epoch)

        # Gradient estimate (proportional to error)
        gradient = error * 0.1

        # State update with temporal weighting
        delta_D = alpha * temporal_weight * gradient

        # Drift accumulation
        accumulated_drift += abs(delta_D)

        # Store state
        convergence = abs(error) / DONTE_CONSTANT
        results.append([
            epoch,
            D,
            error,
            temporal_weight,
            delta_D,
            accumulated_drift,
            convergence
        ])

        # Update state
        D = D + delta_D

    return np.array(results)

def compute_quantum_metrics(data, alpha, lam):
    """Compute metrics for quantum state simulation"""

    epochs = data[:, 0]
    distances = data[:, 1]
    errors = data[:, 2]
    temporal_weights = data[:, 3]
    deltas = data[:, 4]
    convergence = data[:, 6]

    # Final state
    final_distance = distances[-1]
    final_error = errors[-1]
    final_convergence = convergence[-1]

    # Convergence rate
    if len(convergence) > 1:
        conv_rate = (convergence[0] - convergence[-1]) / len(convergence)
    else:
        conv_rate = 0.0

    # Stability: Check if distance oscillates or diverges
    distance_variance = np.var(distances[len(distances)//2:])  # Variance in second half
    is_stable = distance_variance < 1.0 and final_error < 50.0

    # Lipschitz estimate
    if len(deltas) > 1:
        max_delta = np.max(np.abs(deltas[1:]))  # Ignore first step
        max_distance = np.max(np.abs(distances - DONTE_CONSTANT))
        lipschitz = max_delta / (max_distance + 1e-12)
    else:
        lipschitz = 0.0

    metrics = {
        'final_distance': float(final_distance),
        'final_error': float(abs(final_error)),
        'final_convergence': float(final_convergence),
        'convergence_rate': float(conv_rate),
        'distance_variance': float(distance_variance),
        'lipschitz': float(lipschitz),
        'is_stable': bool(is_stable),
        'reached_target': bool(abs(final_error) < 1.0),  # Within 1 unit
        'total_drift': float(np.sum(np.abs(deltas)))
    }

    return metrics

def run_all_quantum_state_sweeps():
    """Execute all quantum state parameter combinations"""

    # Load configurations
    config_file = "quantum_state_sweeps/quantum_state_sweep_configs.json"
    with open(config_file, 'r') as f:
        sweep_data = json.load(f)

    configurations = sweep_data['configurations']
    total = len(configurations)

    print(f"\n{'='*80}")
    print(f"EXECUTING QUANTUM STATE PARAMETER SWEEPS")
    print(f"{'='*80}")
    print(f"Total configurations: {total}")
    print(f"Starting simulations...\n")

    # Create output directory
    output_dir = "quantum_state_results"
    os.makedirs(output_dir, exist_ok=True)

    all_results = []

    for i, config in enumerate(configurations):
        alpha = config['alpha']
        lam = config['lambda']
        epochs = config['epochs']
        config_id = config['config_id']

        # Run simulation
        data = quantum_semantic_resonance(alpha, lam, epochs)

        # Compute metrics
        metrics = compute_quantum_metrics(data, alpha, lam)
        metrics['alpha'] = alpha
        metrics['lambda'] = lam
        metrics['epochs'] = epochs
        metrics['config_id'] = config_id

        all_results.append(metrics)

        # Save individual result
        csv_file = f"{output_dir}/{config_id}.csv"
        with open(csv_file, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow([f"# Quantum-Semantic Resonance: alpha={alpha:.6f}, lambda={lam:.6f}, epochs={epochs}"])
            writer.writerow([f"# Final error: {metrics['final_error']:.6f}, Stable: {metrics['is_stable']}"])
            writer.writerow([f"# Target (Donte): {DONTE_CONSTANT:.10f}"])
            writer.writerow([f"# Generated: {datetime.now().isoformat()}"])
            writer.writerow([])
            writer.writerow(["epoch", "distance", "error", "temporal_weight", "delta_D", "drift", "convergence"])
            writer.writerows(data)

        if (i + 1) % 100 == 0:
            print(f"Completed {i+1}/{total} simulations...")

    print(f"✅ Completed all {total} quantum state simulations\n")

    # Save summary
    summary_file = f"{output_dir}/quantum_state_summary.csv"
    with open(summary_file, 'w', newline='') as f:
        fieldnames = ['config_id', 'alpha', 'lambda', 'epochs',
                     'final_error', 'convergence_rate', 'is_stable',
                     'reached_target', 'lipschitz', 'total_drift']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in all_results:
            writer.writerow({k: r.get(k, '') for k in fieldnames})

    print(f"✅ Saved summary: {summary_file}")

    # Compute statistics
    stable_count = sum(r['is_stable'] for r in all_results)
    reached_target_count = sum(r['reached_target'] for r in all_results)
    final_errors = [r['final_error'] for r in all_results]
    convergence_rates = [r['convergence_rate'] for r in all_results]

    stats = {
        'total_configurations': total,
        'stable_configurations': stable_count,
        'stability_rate': (stable_count / total) * 100,
        'reached_target': reached_target_count,
        'target_rate': (reached_target_count / total) * 100,
        'final_error_min': min(final_errors),
        'final_error_max': max(final_errors),
        'final_error_mean': np.mean(final_errors),
        'convergence_rate_mean': np.mean(convergence_rates),
        'best_alpha': None,
        'best_lambda': None,
        'best_epochs': None
    }

    # Find best configuration (lowest final error)
    best_idx = np.argmin(final_errors)
    stats['best_alpha'] = all_results[best_idx]['alpha']
    stats['best_lambda'] = all_results[best_idx]['lambda']
    stats['best_epochs'] = all_results[best_idx]['epochs']
    stats['best_final_error'] = all_results[best_idx]['final_error']

    stats_file = f"{output_dir}/sweep_statistics.json"
    with open(stats_file, 'w') as f:
        json.dump(stats, f, indent=2)

    print(f"✅ Saved statistics: {stats_file}")

    # Print summary
    print(f"\n{'='*80}")
    print(f"QUANTUM STATE SWEEP STATISTICS")
    print(f"{'='*80}")
    print(f"Total configurations: {stats['total_configurations']}")
    print(f"Stable: {stats['stable_configurations']} ({stats['stability_rate']:.1f}%)")
    print(f"Reached target: {stats['reached_target']} ({stats['target_rate']:.1f}%)")
    print(f"Final error range: [{stats['final_error_min']:.6f}, {stats['final_error_max']:.6f}]")
    print(f"Mean convergence rate: {stats['convergence_rate_mean']:.8f}")
    print(f"\nBest Configuration:")
    print(f"  Alpha: {stats['best_alpha']:.6f}")
    print(f"  Lambda: {stats['best_lambda']:.6f}")
    print(f"  Epochs: {stats['best_epochs']}")
    print(f"  Final Error: {stats['best_final_error']:.6f}")
    print(f"{'='*80}\n")

    return all_results, stats

if __name__ == "__main__":
    print(f"\n{'#'*80}")
    print(f"  QUANTUM STATE PARAMETER SWEEP EXECUTION")
    print(f"  847 Parameter Combinations")
    print(f"{'#'*80}\n")

    start_time = datetime.now()
    results, stats = run_all_quantum_state_sweeps()
    end_time = datetime.now()

    duration = (end_time - start_time).total_seconds()

    print(f"{'='*80}")
    print(f"✅ QUANTUM STATE SWEEP COMPLETE")
    print(f"{'='*80}")
    print(f"Time elapsed: {duration:.2f} seconds")
    print(f"Simulations per second: {len(results)/duration:.1f}")
    print(f"{'='*80}\n")
