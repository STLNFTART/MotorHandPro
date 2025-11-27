#!/usr/bin/env python3
"""
Auto-Heatmap Generation Suite
Visualizes parameter sweep results as heatmaps and comparison plots
"""
import sys
import argparse
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from pathlib import Path


def load_sweep_data(sweep_dir):
    """Load summary data from sweep directory"""
    summary_path = sweep_dir / "summary" / "summary.csv"

    if not summary_path.exists():
        print(f"❌ Error: Summary file not found at {summary_path}")
        sys.exit(1)

    df = pd.read_csv(summary_path)
    print(f"✓ Loaded {len(df)} configurations from {summary_path}")
    return df


def create_heatmap_2d(df, x_param, y_param, metric, output_dir, title_suffix=""):
    """Create 2D heatmap for metric vs two parameters"""

    # Check if parameters exist
    if x_param not in df.columns or y_param not in df.columns:
        print(f"⚠️  Skipping heatmap: {x_param} x {y_param} (parameters not in dataset)")
        return None

    # Get unique values
    x_vals = sorted(df[x_param].unique())
    y_vals = sorted(df[y_param].unique())

    if len(x_vals) < 2 or len(y_vals) < 2:
        print(f"⚠️  Skipping heatmap: {x_param} x {y_param} (need at least 2x2 grid)")
        return None

    # Create pivot table
    pivot = df.pivot_table(
        values=metric,
        index=y_param,
        columns=x_param,
        aggfunc='mean'  # Average if multiple runs
    )

    # Create figure
    fig, ax = plt.subplots(figsize=(10, 8))

    # Plot heatmap
    im = ax.imshow(pivot.values, cmap='RdYlGn_r', aspect='auto', origin='lower')

    # Set ticks
    ax.set_xticks(np.arange(len(x_vals)))
    ax.set_yticks(np.arange(len(y_vals)))
    ax.set_xticklabels([f"{v:.3f}" for v in x_vals])
    ax.set_yticklabels([f"{v:.3f}" for v in y_vals])

    # Labels
    ax.set_xlabel(x_param, fontsize=12)
    ax.set_ylabel(y_param, fontsize=12)
    ax.set_title(f"{metric.upper()} vs {x_param} x {y_param}{title_suffix}", fontsize=14, fontweight='bold')

    # Add colorbar
    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label(metric, rotation=270, labelpad=20)

    # Add value annotations
    for i in range(len(y_vals)):
        for j in range(len(x_vals)):
            val = pivot.values[i, j]
            if not np.isnan(val):
                text = ax.text(j, i, f'{val:.3f}',
                             ha="center", va="center", color="black", fontsize=8)

    plt.tight_layout()

    # Save
    filename = f"heatmap_{metric}_{x_param}_vs_{y_param}.png"
    filepath = output_dir / filename
    plt.savefig(filepath, dpi=150, bbox_inches='tight')
    plt.close()

    print(f"  ✓ Created {filename}")
    return filepath


def create_stability_map(df, x_param, y_param, output_dir):
    """Create binary stability heatmap"""

    if x_param not in df.columns or y_param not in df.columns or 'stable' not in df.columns:
        return None

    x_vals = sorted(df[x_param].unique())
    y_vals = sorted(df[y_param].unique())

    if len(x_vals) < 2 or len(y_vals) < 2:
        return None

    # Create pivot table (count stable configs)
    pivot = df.pivot_table(
        values='stable',
        index=y_param,
        columns=x_param,
        aggfunc='sum'  # Count True values
    )

    # Create figure
    fig, ax = plt.subplots(figsize=(10, 8))

    # Plot heatmap
    im = ax.imshow(pivot.values, cmap='RdYlGn', aspect='auto', origin='lower', vmin=0, vmax=1)

    # Set ticks
    ax.set_xticks(np.arange(len(x_vals)))
    ax.set_yticks(np.arange(len(y_vals)))
    ax.set_xticklabels([f"{v:.3f}" for v in x_vals])
    ax.set_yticklabels([f"{v:.3f}" for v in y_vals])

    # Labels
    ax.set_xlabel(x_param, fontsize=12)
    ax.set_ylabel(y_param, fontsize=12)
    ax.set_title(f"STABILITY MAP: {x_param} x {y_param}", fontsize=14, fontweight='bold')

    # Add colorbar
    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label('Stable (1) / Unstable (0)', rotation=270, labelpad=20)

    # Add stability annotations
    for i in range(len(y_vals)):
        for j in range(len(x_vals)):
            val = pivot.values[i, j]
            if not np.isnan(val):
                color = "white" if val > 0.5 else "black"
                text = "✓" if val > 0.5 else "✗"
                ax.text(j, i, text, ha="center", va="center", color=color, fontsize=14, fontweight='bold')

    plt.tight_layout()

    # Save
    filename = f"stability_map_{x_param}_vs_{y_param}.png"
    filepath = output_dir / filename
    plt.savefig(filepath, dpi=150, bbox_inches='tight')
    plt.close()

    print(f"  ✓ Created {filename}")
    return filepath


def create_memory_comparison(df, output_dir):
    """Create memory mode comparison plots"""

    if 'memory_mode' not in df.columns:
        print("⚠️  No memory_mode column found, skipping memory comparison")
        return

    metrics = ['avg_error', 'rmse', 'settling_time_s', 'total_energy_J']
    memory_modes = df['memory_mode'].unique()

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    axes = axes.flatten()

    for idx, metric in enumerate(metrics):
        ax = axes[idx]

        if metric not in df.columns:
            continue

        # Box plot for each memory mode
        data_to_plot = []
        labels = []
        for mode in memory_modes:
            mode_data = df[df['memory_mode'] == mode][metric].dropna()
            if len(mode_data) > 0:
                data_to_plot.append(mode_data)
                labels.append(mode)

        if data_to_plot:
            bp = ax.boxplot(data_to_plot, labels=labels, patch_artist=True)

            # Color boxes
            colors = ['lightblue', 'lightgreen', 'lightcoral']
            for patch, color in zip(bp['boxes'], colors[:len(bp['boxes'])]):
                patch.set_facecolor(color)

            ax.set_ylabel(metric, fontsize=11)
            ax.set_title(f"{metric.upper()} by Memory Mode", fontsize=12, fontweight='bold')
            ax.grid(True, alpha=0.3)

    plt.suptitle("Memory Mode Performance Comparison", fontsize=14, fontweight='bold')
    plt.tight_layout()

    # Save
    filename = "memory_mode_comparison.png"
    filepath = output_dir / filename
    plt.savefig(filepath, dpi=150, bbox_inches='tight')
    plt.close()

    print(f"  ✓ Created {filename}")
    return filepath


def create_torque_regime_analysis(df, output_dir):
    """Analyze high-torque regime performance"""

    if 'torque_max' not in df.columns:
        print("⚠️  No torque_max column found, skipping torque analysis")
        return

    # Create multi-metric plot
    fig, axes = plt.subplots(2, 2, figsize=(14, 10))

    torque_vals = sorted(df['torque_max'].unique())

    # Plot 1: Error vs Torque
    ax = axes[0, 0]
    for metric in ['avg_error', 'max_error', 'rmse']:
        if metric in df.columns:
            means = [df[df['torque_max'] == t][metric].mean() for t in torque_vals]
            ax.plot(torque_vals, means, marker='o', label=metric, linewidth=2)
    ax.set_xlabel('Torque Max (N·m)', fontsize=11)
    ax.set_ylabel('Error (rad)', fontsize=11)
    ax.set_title('Error Metrics vs Torque Limit', fontsize=12, fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)

    # Plot 2: Settling Time vs Torque
    ax = axes[0, 1]
    if 'settling_time_s' in df.columns:
        means = [df[df['torque_max'] == t]['settling_time_s'].mean() for t in torque_vals]
        stds = [df[df['torque_max'] == t]['settling_time_s'].std() for t in torque_vals]
        ax.errorbar(torque_vals, means, yerr=stds, marker='o', linewidth=2, capsize=5)
    ax.set_xlabel('Torque Max (N·m)', fontsize=11)
    ax.set_ylabel('Settling Time (s)', fontsize=11)
    ax.set_title('Settling Time vs Torque Limit', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)

    # Plot 3: Energy vs Torque
    ax = axes[1, 0]
    if 'total_energy_J' in df.columns:
        means = [df[df['torque_max'] == t]['total_energy_J'].mean() for t in torque_vals]
        ax.plot(torque_vals, means, marker='o', linewidth=2, color='orange')
    ax.set_xlabel('Torque Max (N·m)', fontsize=11)
    ax.set_ylabel('Total Energy (J)', fontsize=11)
    ax.set_title('Energy Consumption vs Torque Limit', fontsize=12, fontweight='bold')
    ax.grid(True, alpha=0.3)

    # Plot 4: Stability Rate vs Torque
    ax = axes[1, 1]
    if 'stable' in df.columns:
        stability_rates = [df[df['torque_max'] == t]['stable'].mean() * 100 for t in torque_vals]
        ax.bar(range(len(torque_vals)), stability_rates, color='green', alpha=0.7)
        ax.set_xticks(range(len(torque_vals)))
        ax.set_xticklabels([f"{t:.1f}" for t in torque_vals])
        ax.set_ylim([0, 105])
        ax.axhline(100, color='red', linestyle='--', linewidth=1, label='100% Stable')
    ax.set_xlabel('Torque Max (N·m)', fontsize=11)
    ax.set_ylabel('Stability Rate (%)', fontsize=11)
    ax.set_title('Stability Rate vs Torque Limit', fontsize=12, fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3, axis='y')

    plt.suptitle("High-Torque Regime Analysis", fontsize=14, fontweight='bold')
    plt.tight_layout()

    # Save
    filename = "torque_regime_analysis.png"
    filepath = output_dir / filename
    plt.savefig(filepath, dpi=150, bbox_inches='tight')
    plt.close()

    print(f"  ✓ Created {filename}")
    return filepath


def main():
    parser = argparse.ArgumentParser(description='Generate heatmaps from sweep results')
    parser.add_argument('--sweep-dir', type=str, required=True,
                        help='Sweep directory name (e.g., 20251126_205409_comprehensive_full_sweep)')
    parser.add_argument('--sim-name', type=str, default='control_system',
                        help='Simulation name (default: control_system)')

    args = parser.parse_args()

    # Build path
    sweep_path = Path('experiments') / 'runs' / args.sim_name / args.sweep_dir

    if not sweep_path.exists():
        print(f"❌ Error: Sweep directory not found: {sweep_path}")
        sys.exit(1)

    print(f"\n🎨 Auto-Heatmap Generation Suite")
    print(f"=" * 60)
    print(f"Sweep dir: {sweep_path}")
    print(f"=" * 60)
    print()

    # Load data
    df = load_sweep_data(sweep_path)

    # Create output directory for plots
    plots_dir = sweep_path / 'plots'
    plots_dir.mkdir(exist_ok=True)

    print(f"\n📊 Generating visualizations...")
    print()

    # Identify parameters for heatmaps (exclude metrics)
    # Metrics are outputs, parameters are inputs
    metric_keywords = ['error', 'time', 'energy', 'lipschitz', 'violation', 'overshoot', 'rmse']
    params = []
    for col in df.columns:
        # Skip if it's a metric or constant
        is_metric = any(keyword in col.lower() for keyword in metric_keywords)
        is_stable = col == 'stable'
        is_constant = df[col].nunique() <= 1

        # Keep if it's a numeric parameter that varies
        if not is_metric and not is_stable and not is_constant:
            if df[col].dtype in [np.float64, np.int64, np.float32, np.int32]:
                params.append(col)

    # If memory_mode exists, treat it as categorical parameter
    if 'memory_mode' in df.columns and df['memory_mode'].nunique() > 1:
        # Will be handled separately in memory comparison
        pass

    print(f"Detected sweep parameters: {params}")
    print()

    # Key metrics to visualize
    metrics = ['avg_error', 'rmse', 'lipschitz', 'settling_time_s', 'total_energy_J']

    # Generate 2D heatmaps for all parameter pairs
    count = 0
    for i, x_param in enumerate(params):
        for y_param in params[i+1:]:
            for metric in metrics:
                if metric in df.columns:
                    result = create_heatmap_2d(df, x_param, y_param, metric, plots_dir)
                    if result:
                        count += 1

            # Stability map
            result = create_stability_map(df, x_param, y_param, plots_dir)
            if result:
                count += 1

    # Memory comparison (if applicable)
    create_memory_comparison(df, plots_dir)
    count += 1

    # Torque regime analysis (if applicable)
    create_torque_regime_analysis(df, plots_dir)
    count += 1

    print()
    print(f"✓ Generated {count} visualizations in: {plots_dir}")
    print()
    print(f"📁 Output files:")
    for f in sorted(plots_dir.glob("*.png")):
        print(f"   - {f.name}")
    print()


if __name__ == "__main__":
    main()
