#!/usr/bin/env python3
"""
Analyze ALL Primal Logic Kernel Parameter Sweep Results
Processes 385 kernel sweep files and generates comprehensive analysis
"""

import glob
import csv
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import json

def parse_header_value(line, key):
    """Parse parameter value from CSV header"""
    if key + "=" not in line:
        return None
    try:
        after = line.split(key + "=")[1]
        val = ""
        for ch in after:
            if ch in "0123456789+-.eE":
                val += ch
            else:
                break
        return float(val) if val else None
    except Exception:
        return None

def load_sweep_file(filepath):
    """Load a single sweep file and extract metadata + data"""
    meta = {'MU': None, 'KE': None, 'D0': None}
    data = []

    with open(filepath, 'r') as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue

            line = ",".join(row)

            if row[0].startswith("#"):
                if "MU=" in line:
                    meta['MU'] = parse_header_value(line, "MU")
                if "KE=" in line:
                    meta['KE'] = parse_header_value(line, "KE")
                if "D0=" in line:
                    meta['D0'] = parse_header_value(line, "D0")
                continue

            if row[0].lower() in ("t", "time"):
                continue

            try:
                t = float(row[0])
                psi = float(row[1])
                gamma = float(row[2])
                Ec = float(row[3])
                data.append([t, psi, gamma, Ec])
            except:
                continue

    return meta, np.array(data)

def compute_sweep_metrics(meta, data):
    """Compute metrics for a sweep run"""
    if len(data) == 0:
        return None

    t = data[:, 0]
    psi = data[:, 1]
    gamma = data[:, 2]
    Ec = data[:, 3]

    # Compute Lipschitz estimate
    delta_Ec = np.abs(np.diff(Ec))
    delta_t = np.diff(t)
    rates = delta_Ec / (delta_t + 1e-12)
    max_rate = np.max(rates) if len(rates) > 0 else 0
    max_Ec = np.max(np.abs(Ec))
    lipschitz = max_rate / (max_Ec + 1e-12)

    # Check stability
    is_stable = lipschitz < 1.0

    metrics = {
        'mu': meta['MU'],
        'ke': meta['KE'],
        'd0': meta['D0'],
        'max_psi': np.max(psi),
        'min_psi': np.min(psi),
        'final_psi': psi[-1],
        'max_Ec': np.max(Ec),
        'min_Ec': np.min(Ec),
        'final_Ec': Ec[-1],
        'lipschitz': lipschitz,
        'is_stable': is_stable
    }

    return metrics

def analyze_all_sweeps(sweep_dir="primal_kernel_sweeps"):
    """Analyze all kernel sweep files"""

    pattern = f"{sweep_dir}/run_*.csv"
    files = sorted(glob.glob(pattern))

    print(f"\n{'='*80}")
    print(f"ANALYZING ALL PRIMAL LOGIC KERNEL SWEEPS")
    print(f"{'='*80}")
    print(f"Files found: {len(files)}")
    print(f"Processing...\n")

    all_metrics = []

    for i, filepath in enumerate(files):
        meta, data = load_sweep_file(filepath)
        metrics = compute_sweep_metrics(meta, data)

        if metrics:
            metrics['filename'] = Path(filepath).name
            all_metrics.append(metrics)

        if (i + 1) % 50 == 0:
            print(f"Processed {i+1}/{len(files)} files...")

    print(f"✅ Processed {len(all_metrics)} files successfully\n")

    return all_metrics

def generate_summary_statistics(all_metrics):
    """Generate summary statistics across all sweeps"""

    # Convert to arrays for analysis
    mu_values = np.array([m['mu'] for m in all_metrics])
    ke_values = np.array([m['ke'] for m in all_metrics])
    d0_values = np.array([m['d0'] for m in all_metrics])
    lipschitz_values = np.array([m['lipschitz'] for m in all_metrics])
    stable_count = sum(m['is_stable'] for m in all_metrics)

    summary = {
        'total_configurations': int(len(all_metrics)),
        'stable_configurations': int(stable_count),
        'unstable_configurations': int(len(all_metrics) - stable_count),
        'stability_rate': float(stable_count / len(all_metrics) * 100),
        'mu_range': [float(mu_values.min()), float(mu_values.max())],
        'ke_range': [float(ke_values.min()), float(ke_values.max())],
        'd0_range': [float(d0_values.min()), float(d0_values.max())],
        'lipschitz_stats': {
            'min': float(lipschitz_values.min()),
            'max': float(lipschitz_values.max()),
            'mean': float(lipschitz_values.mean()),
            'median': float(np.median(lipschitz_values)),
            'std': float(lipschitz_values.std())
        }
    }

    # Find best configurations (lowest Lipschitz)
    sorted_indices = np.argsort(lipschitz_values)
    best_configs = []
    for idx in sorted_indices[:10]:
        best_configs.append({
            'mu': float(all_metrics[idx]['mu']),
            'ke': float(all_metrics[idx]['ke']),
            'd0': float(all_metrics[idx]['d0']),
            'lipschitz': float(all_metrics[idx]['lipschitz']),
            'filename': str(all_metrics[idx]['filename'])
        })
    summary['best_configurations'] = best_configs

    # Find worst configurations (highest Lipschitz among stable ones)
    stable_metrics = [m for m in all_metrics if m['is_stable']]
    if stable_metrics:
        stable_lipschitz = np.array([m['lipschitz'] for m in stable_metrics])
        worst_stable_indices = np.argsort(stable_lipschitz)[-10:]
        worst_configs = []
        for idx in worst_stable_indices:
            worst_configs.append({
                'mu': float(stable_metrics[idx]['mu']),
                'ke': float(stable_metrics[idx]['ke']),
                'd0': float(stable_metrics[idx]['d0']),
                'lipschitz': float(stable_metrics[idx]['lipschitz']),
                'filename': str(stable_metrics[idx]['filename'])
            })
        summary['worst_stable_configurations'] = worst_configs

    return summary

def create_heatmaps(all_metrics, output_dir="primal_kernel_sweeps"):
    """Create heatmap visualizations of parameter space"""

    # Get unique parameter values
    unique_mu = sorted(set(m['mu'] for m in all_metrics))
    unique_ke = sorted(set(m['ke'] for m in all_metrics))
    unique_d0 = sorted(set(m['d0'] for m in all_metrics))

    print(f"\n{'='*80}")
    print(f"GENERATING HEATMAP VISUALIZATIONS")
    print(f"{'='*80}")
    print(f"Unique MU values: {len(unique_mu)}")
    print(f"Unique KE values: {len(unique_ke)}")
    print(f"Unique D0 values: {len(unique_d0)}")

    # For each D0 value, create a MU vs KE heatmap
    for d0_val in unique_d0:
        # Filter metrics for this D0
        d0_metrics = [m for m in all_metrics if abs(m['d0'] - d0_val) < 0.01]

        # Create grid
        grid = np.full((len(unique_mu), len(unique_ke)), np.nan)

        for m in d0_metrics:
            mu_idx = unique_mu.index(m['mu'])
            ke_idx = unique_ke.index(m['ke'])
            grid[mu_idx, ke_idx] = m['lipschitz']

        # Create heatmap
        fig, ax = plt.subplots(figsize=(14, 10))

        im = ax.imshow(grid, cmap='RdYlGn_r', aspect='auto',
                      interpolation='nearest', vmin=0, vmax=1.5)

        # Set ticks
        ax.set_xticks(range(len(unique_ke)))
        ax.set_yticks(range(len(unique_mu)))
        ax.set_xticklabels([f"{ke:.2f}" for ke in unique_ke], rotation=45)
        ax.set_yticklabels([f"{mu:.3f}" for mu in unique_mu])

        ax.set_xlabel('KE (Error Gain)', fontsize=12, fontweight='bold')
        ax.set_ylabel('MU (λ - Lightfoot Constant)', fontsize=12, fontweight='bold')
        ax.set_title(f'Lipschitz Constant Heatmap - D0={d0_val:.4f}\n'
                    f'(Green = Stable, Red = Unstable)',
                    fontsize=14, fontweight='bold')

        # Add colorbar
        cbar = plt.colorbar(im, ax=ax)
        cbar.set_label('Lipschitz Constant', fontsize=12, fontweight='bold')

        # Add stability threshold line
        cbar.ax.axhline(y=1.0, color='black', linewidth=2, linestyle='--')

        # Add text annotations for interesting regions
        for i, mu in enumerate(unique_mu):
            for j, ke in enumerate(unique_ke):
                value = grid[i, j]
                if not np.isnan(value):
                    color = 'white' if value > 0.7 else 'black'
                    if value < 1.0:
                        ax.text(j, i, f'{value:.3f}',
                               ha='center', va='center',
                               color=color, fontsize=6)

        plt.tight_layout()

        # Save figure
        d0_str = f"{d0_val:.1f}".replace('.', 'p')
        filename = f"{output_dir}/lipschitz_heatmap_d0_{d0_str}.png"
        plt.savefig(filename, dpi=200, bbox_inches='tight')
        plt.close()

        print(f"✅ Generated heatmap: {filename}")

    # Create overall stability map (MU vs KE averaged over D0)
    avg_grid = np.zeros((len(unique_mu), len(unique_ke)))
    count_grid = np.zeros((len(unique_mu), len(unique_ke)))

    for m in all_metrics:
        mu_idx = unique_mu.index(m['mu'])
        ke_idx = unique_ke.index(m['ke'])
        avg_grid[mu_idx, ke_idx] += m['lipschitz']
        count_grid[mu_idx, ke_idx] += 1

    avg_grid = avg_grid / (count_grid + 1e-12)

    fig, ax = plt.subplots(figsize=(14, 10))

    im = ax.imshow(avg_grid, cmap='RdYlGn_r', aspect='auto',
                  interpolation='nearest', vmin=0, vmax=1.5)

    ax.set_xticks(range(len(unique_ke)))
    ax.set_yticks(range(len(unique_mu)))
    ax.set_xticklabels([f"{ke:.2f}" for ke in unique_ke], rotation=45)
    ax.set_yticklabels([f"{mu:.3f}" for mu in unique_mu])

    ax.set_xlabel('KE (Error Gain)', fontsize=12, fontweight='bold')
    ax.set_ylabel('MU (λ - Lightfoot Constant)', fontsize=12, fontweight='bold')
    ax.set_title('Average Lipschitz Constant Across All D0 Values\n'
                '(Green = Stable, Red = Unstable)',
                fontsize=14, fontweight='bold')

    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label('Average Lipschitz Constant', fontsize=12, fontweight='bold')
    cbar.ax.axhline(y=1.0, color='black', linewidth=2, linestyle='--')

    plt.tight_layout()

    filename = f"{output_dir}/lipschitz_heatmap_average.png"
    plt.savefig(filename, dpi=200, bbox_inches='tight')
    plt.close()

    print(f"✅ Generated average heatmap: {filename}")

    print(f"{'='*80}\n")

def main():
    """Main analysis pipeline"""

    print(f"\n{'#'*80}")
    print(f"  COMPREHENSIVE PRIMAL LOGIC KERNEL SWEEP ANALYSIS")
    print(f"  Processing 385 Parameter Combinations")
    print(f"{'#'*80}\n")

    # Analyze all sweeps
    all_metrics = analyze_all_sweeps()

    # Generate summary statistics
    summary = generate_summary_statistics(all_metrics)

    # Print summary
    print(f"{'='*80}")
    print(f"SUMMARY STATISTICS")
    print(f"{'='*80}")
    print(f"Total configurations: {summary['total_configurations']}")
    print(f"Stable configurations (L < 1.0): {summary['stable_configurations']} "
          f"({summary['stability_rate']:.1f}%)")
    print(f"Unstable configurations: {summary['unstable_configurations']}")
    print(f"\nLipschitz Statistics:")
    print(f"  Min:    {summary['lipschitz_stats']['min']:.6f}")
    print(f"  Max:    {summary['lipschitz_stats']['max']:.6f}")
    print(f"  Mean:   {summary['lipschitz_stats']['mean']:.6f}")
    print(f"  Median: {summary['lipschitz_stats']['median']:.6f}")
    print(f"  Std:    {summary['lipschitz_stats']['std']:.6f}")

    print(f"\nTop 10 Most Stable Configurations (Lowest Lipschitz):")
    for i, config in enumerate(summary['best_configurations'], 1):
        print(f"  {i}. MU={config['mu']:.5f}, KE={config['ke']:.2f}, "
              f"D0={config['d0']:.4f} → L={config['lipschitz']:.6f}")

    print(f"{'='*80}\n")

    # Save summary to JSON
    with open("primal_kernel_sweeps/sweep_analysis_summary.json", 'w') as f:
        json.dump(summary, f, indent=2)

    print("✅ Saved summary: primal_kernel_sweeps/sweep_analysis_summary.json")

    # Save all metrics to CSV
    with open("primal_kernel_sweeps/all_sweep_metrics.csv", 'w', newline='') as f:
        fieldnames = ['filename', 'mu', 'ke', 'd0', 'max_psi', 'final_psi',
                     'max_Ec', 'final_Ec', 'lipschitz', 'is_stable']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for m in all_metrics:
            writer.writerow({k: m.get(k, '') for k in fieldnames})

    print("✅ Saved metrics CSV: primal_kernel_sweeps/all_sweep_metrics.csv")

    # Generate heatmaps
    create_heatmaps(all_metrics)

    print(f"\n{'='*80}")
    print(f"✅ ANALYSIS COMPLETE")
    print(f"{'='*80}\n")

if __name__ == "__main__":
    main()
