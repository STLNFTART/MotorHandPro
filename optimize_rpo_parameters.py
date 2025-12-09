#!/usr/bin/env python3
"""
RPO Parameter Optimization for Comet Outburst Detection

Sweeps through parameter space to find optimal μ, α, β values
for maximizing F1-score while maintaining high precision.

Addresses the key question: Is μ = 0.16905 truly optimal?
"""

import numpy as np
import pandas as pd
import json
from datetime import datetime
from typing import List, Dict, Tuple
import sys
from pathlib import Path

sys.path.append('/home/user/MotorHandPro/network_simulation_cluster/data_sources')
from nasa_comet_data import RecursivePlanckOperator, CometObservation


def load_dataset(filepath: str) -> List[CometObservation]:
    """Load 12P/Pons-Brooks dataset"""
    with open(filepath, 'r') as f:
        data = json.load(f)

    observations = []
    for obs_data in data['observations']:
        obs = CometObservation(
            timestamp=datetime.fromisoformat(obs_data['timestamp']),
            ra=obs_data['ra_deg'],
            dec=obs_data['dec_deg'],
            distance_au=obs_data['distance_au'],
            velocity_km_s=obs_data['velocity_km_s'],
            magnitude=obs_data['magnitude'],
            elongation=obs_data['elongation_deg'],
            phase_angle=obs_data['phase_angle_deg'],
            gas_production_rate=obs_data['ni_flux_g_s'],
            quality_flag=obs_data.get('quality_flag', 'NOMINAL')
        )
        observations.append(obs)

    return observations


def run_rpo_with_params(
    observations: List[CometObservation],
    mu: float,
    alpha: float = 1.618,
    beta: float = 0.5,
    threshold_percentile: float = 95.0
) -> Dict:
    """
    Run RPO with specified parameters

    Args:
        observations: Comet observations
        mu: Damping constant
        alpha: Memory decay rate
        beta: Coupling strength
        threshold_percentile: Detection threshold (percentile of anomaly scores)

    Returns:
        Performance metrics dictionary
    """
    rpo = RecursivePlanckOperator(mu=mu, alpha=alpha, beta=beta)

    anomaly_scores = []
    states = []

    for obs in observations:
        state = rpo.update(obs, dt=0.01)
        anomaly_scores.append(state.error / 10.0)
        states.append(state.n)

    # Convert to numpy
    anomaly_scores = np.array(anomaly_scores)
    states = np.array(states)

    # True labels
    true_outbursts = np.array([obs.quality_flag == "OUTBURST" for obs in observations])

    # Adaptive threshold
    threshold = np.percentile(anomaly_scores, threshold_percentile)
    detected = anomaly_scores > threshold

    # Confusion matrix
    tp = np.sum(true_outbursts & detected)
    fp = np.sum(~true_outbursts & detected)
    fn = np.sum(true_outbursts & ~detected)
    tn = np.sum(~true_outbursts & ~detected)

    # Metrics
    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0
    accuracy = (tp + tn) / len(observations)

    return {
        "mu": mu,
        "alpha": alpha,
        "beta": beta,
        "threshold_percentile": threshold_percentile,
        "threshold_value": threshold,
        "precision": precision,
        "recall": recall,
        "f1_score": f1,
        "accuracy": accuracy,
        "tp": int(tp),
        "fp": int(fp),
        "fn": int(fn),
        "tn": int(tn),
        "max_state": float(np.max(np.abs(states))),
        "mean_anomaly": float(np.mean(anomaly_scores)),
        "max_anomaly": float(np.max(anomaly_scores))
    }


def parameter_sweep_mu(
    observations: List[CometObservation],
    mu_values: np.ndarray = np.linspace(0.10, 0.30, 21),
    alpha: float = 1.618,
    beta: float = 0.5
) -> pd.DataFrame:
    """
    Sweep through μ values to find optimal damping constant

    Tests the hypothesis: Is μ = 0.16905 truly optimal?
    """
    print(f"🔬 PARAMETER SWEEP: μ (Damping Constant)")
    print(f"   Testing {len(mu_values)} values from {mu_values[0]:.3f} to {mu_values[-1]:.3f}")
    print(f"   Fixed: α={alpha}, β={beta}")
    print()

    results = []

    for i, mu in enumerate(mu_values):
        print(f"   [{i+1}/{len(mu_values)}] Testing μ = {mu:.5f}...", end="")

        metrics = run_rpo_with_params(observations, mu, alpha, beta)
        results.append(metrics)

        print(f" F1={metrics['f1_score']:.3f}, P={metrics['precision']:.3f}, R={metrics['recall']:.3f}")

    return pd.DataFrame(results)


def threshold_optimization(
    observations: List[CometObservation],
    mu: float = 0.16905,
    alpha: float = 1.618,
    beta: float = 0.5,
    percentiles: np.ndarray = np.linspace(80, 99, 20)
) -> pd.DataFrame:
    """
    Optimize detection threshold for better precision-recall trade-off
    """
    print(f"\n🎯 THRESHOLD OPTIMIZATION")
    print(f"   Testing {len(percentiles)} threshold values")
    print(f"   Fixed: μ={mu}, α={alpha}, β={beta}")
    print()

    results = []

    for i, percentile in enumerate(percentiles):
        print(f"   [{i+1}/{len(percentiles)}] Testing {percentile:.1f}th percentile...", end="")

        metrics = run_rpo_with_params(observations, mu, alpha, beta, percentile)
        results.append(metrics)

        print(f" F1={metrics['f1_score']:.3f}, FP={metrics['fp']}")

    return pd.DataFrame(results)


def multi_scale_rpo(
    observations: List[CometObservation],
    mu: float = 0.16905,
    alpha_values: List[float] = [1.0, 1.618, 2.5]
) -> Dict:
    """
    Run RPO at multiple memory timescales

    Ensemble approach: Combine fast (sensitive) and slow (conservative) detectors
    """
    print(f"\n🔀 MULTI-SCALE RPO ENSEMBLE")
    print(f"   Running {len(alpha_values)} RPO instances with different α values")
    print()

    all_results = []
    all_anomalies = []

    for alpha in alpha_values:
        print(f"   α = {alpha:.3f} (memory timescale τ = {1/alpha:.3f})...", end="")

        rpo = RecursivePlanckOperator(mu=mu, alpha=alpha, beta=0.5)
        anomaly_scores = []

        for obs in observations:
            state = rpo.update(obs, dt=0.01)
            anomaly_scores.append(state.error / 10.0)

        all_anomalies.append(anomaly_scores)

        # Individual performance
        threshold = np.percentile(anomaly_scores, 95)
        detected = np.array(anomaly_scores) > threshold
        true_outbursts = np.array([obs.quality_flag == "OUTBURST" for obs in observations])

        tp = np.sum(true_outbursts & detected)
        fp = np.sum(~true_outbursts & detected)

        print(f" Detections={np.sum(detected)}, FP={fp}")

        all_results.append({
            "alpha": alpha,
            "detections": int(np.sum(detected)),
            "false_positives": int(fp)
        })

    # Ensemble: Vote (detection if ANY RPO triggers)
    all_anomalies = np.array(all_anomalies)
    ensemble_scores = np.max(all_anomalies, axis=0)  # Max across scales

    threshold = np.percentile(ensemble_scores, 95)
    ensemble_detected = ensemble_scores > threshold
    true_outbursts = np.array([obs.quality_flag == "OUTBURST" for obs in observations])

    tp = np.sum(true_outbursts & ensemble_detected)
    fp = np.sum(~true_outbursts & ensemble_detected)
    fn = np.sum(true_outbursts & ~ensemble_detected)

    precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0
    recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0
    f1 = 2 * precision * recall / (precision + recall) if (precision + recall) > 0 else 0.0

    print(f"\n   📊 Ensemble Results:")
    print(f"      Precision: {precision:.3f}")
    print(f"      Recall: {recall:.3f}")
    print(f"      F1-Score: {f1:.3f}")
    print(f"      False Positives: {fp}")

    return {
        "individual_results": all_results,
        "ensemble": {
            "precision": precision,
            "recall": recall,
            "f1_score": f1,
            "tp": int(tp),
            "fp": int(fp),
            "fn": int(fn)
        }
    }


def main():
    """Main optimization pipeline"""
    print("=" * 80)
    print("RPO PARAMETER OPTIMIZATION FOR COMET OUTBURST DETECTION")
    print("=" * 80)
    print()

    # Load dataset
    dataset_path = "/home/user/MotorHandPro/data/12p_pons_brooks_realistic.json"
    print(f"📂 Loading dataset: {dataset_path}")
    observations = load_dataset(dataset_path)
    print(f"✅ Loaded {len(observations)} observations")
    print()

    # ========================================================================
    # EXPERIMENT 1: μ Parameter Sweep
    # ========================================================================
    print("=" * 80)
    print("EXPERIMENT 1: OPTIMAL μ (DAMPING CONSTANT)")
    print("=" * 80)

    # Test μ around 0.16905
    mu_fine = np.linspace(0.14, 0.20, 31)
    results_mu = parameter_sweep_mu(observations, mu_fine)

    # Find best F1
    best_f1_idx = results_mu['f1_score'].idxmax()
    best_f1 = results_mu.loc[best_f1_idx]

    # Find best precision with F1 > 0.2
    high_f1_results = results_mu[results_mu['f1_score'] > 0.2]
    if len(high_f1_results) > 0:
        best_precision_idx = high_f1_results['precision'].idxmax()
        best_precision = high_f1_results.loc[best_precision_idx]
    else:
        best_precision = best_f1

    print(f"\n📊 Results:")
    print(f"   Best F1-Score: {best_f1['f1_score']:.3f} at μ = {best_f1['mu']:.5f}")
    print(f"      Precision: {best_f1['precision']:.3f}")
    print(f"      Recall: {best_f1['recall']:.3f}")
    print(f"      False Positives: {best_f1['fp']}")
    print()
    print(f"   Best Precision (F1>0.2): {best_precision['precision']:.3f} at μ = {best_precision['mu']:.5f}")
    print(f"      F1-Score: {best_precision['f1_score']:.3f}")
    print(f"      Recall: {best_precision['recall']:.3f}")
    print(f"      False Positives: {best_precision['fp']}")
    print()
    print(f"   Current (μ=0.16905): F1={results_mu[results_mu['mu'].apply(lambda x: abs(x-0.16905) < 0.001)]['f1_score'].values[0]:.3f}")

    # Save results
    results_mu.to_csv("/home/user/MotorHandPro/analysis_results/mu_optimization.csv", index=False)
    print(f"\n✅ Saved: analysis_results/mu_optimization.csv")

    # ========================================================================
    # EXPERIMENT 2: Threshold Optimization
    # ========================================================================
    print("\n" + "=" * 80)
    print("EXPERIMENT 2: THRESHOLD OPTIMIZATION")
    print("=" * 80)

    # Use best μ from experiment 1
    optimal_mu = best_f1['mu']
    percentiles = np.linspace(70, 98, 29)
    results_threshold = threshold_optimization(observations, optimal_mu, percentiles=percentiles)

    # Find best trade-off
    best_threshold_idx = results_threshold['f1_score'].idxmax()
    best_threshold = results_threshold.loc[best_threshold_idx]

    print(f"\n📊 Results:")
    print(f"   Best Threshold: {best_threshold['threshold_percentile']:.1f}th percentile")
    print(f"      F1-Score: {best_threshold['f1_score']:.3f}")
    print(f"      Precision: {best_threshold['precision']:.3f}")
    print(f"      Recall: {best_threshold['recall']:.3f}")
    print(f"      False Positives: {best_threshold['fp']}")

    # Save results
    results_threshold.to_csv("/home/user/MotorHandPro/analysis_results/threshold_optimization.csv", index=False)
    print(f"\n✅ Saved: analysis_results/threshold_optimization.csv")

    # ========================================================================
    # EXPERIMENT 3: Multi-Scale Ensemble
    # ========================================================================
    print("\n" + "=" * 80)
    print("EXPERIMENT 3: MULTI-SCALE RPO ENSEMBLE")
    print("=" * 80)

    alpha_scales = [0.8, 1.0, 1.618, 2.0, 2.5]  # Different memory timescales
    ensemble_results = multi_scale_rpo(observations, optimal_mu, alpha_scales)

    # Save results
    with open("/home/user/MotorHandPro/analysis_results/ensemble_results.json", 'w') as f:
        json.dump(ensemble_results, f, indent=2)
    print(f"\n✅ Saved: analysis_results/ensemble_results.json")

    # ========================================================================
    # FINAL SUMMARY
    # ========================================================================
    print("\n" + "=" * 80)
    print("OPTIMIZATION SUMMARY")
    print("=" * 80)
    print()

    print("🔬 KEY FINDINGS:")
    print()
    print(f"1. OPTIMAL μ:")
    print(f"   Best F1: μ = {best_f1['mu']:.5f} → F1 = {best_f1['f1_score']:.3f}")
    print(f"   Current: μ = 0.16905 → F1 = {results_mu[results_mu['mu'].apply(lambda x: abs(x-0.16905) < 0.001)]['f1_score'].values[0]:.3f}")

    if abs(best_f1['mu'] - 0.16905) < 0.01:
        print(f"   ✅ μ = 0.16905 is near-optimal!")
    else:
        improvement = 100 * (best_f1['f1_score'] - results_mu[results_mu['mu'].apply(lambda x: abs(x-0.16905) < 0.001)]['f1_score'].values[0]) / results_mu[results_mu['mu'].apply(lambda x: abs(x-0.16905) < 0.001)]['f1_score'].values[0]
        print(f"   ⚠️  Could improve F1 by {improvement:.1f}% using μ = {best_f1['mu']:.5f}")

    print()
    print(f"2. OPTIMAL THRESHOLD:")
    print(f"   Best: {best_threshold['threshold_percentile']:.1f}th percentile")
    print(f"   Achieves: Precision={best_threshold['precision']:.3f}, Recall={best_threshold['recall']:.3f}")
    print(f"   False Positives: {best_threshold['fp']}")

    print()
    print(f"3. ENSEMBLE METHOD:")
    print(f"   F1-Score: {ensemble_results['ensemble']['f1_score']:.3f}")
    print(f"   Precision: {ensemble_results['ensemble']['precision']:.3f}")
    print(f"   Recall: {ensemble_results['ensemble']['recall']:.3f}")
    print(f"   False Positives: {ensemble_results['ensemble']['fp']}")

    print()
    print("=" * 80)
    print("✅ OPTIMIZATION COMPLETE")
    print("=" * 80)
    print()
    print("📁 Results saved to analysis_results/")
    print("   - mu_optimization.csv")
    print("   - threshold_optimization.csv")
    print("   - ensemble_results.json")
    print()


if __name__ == "__main__":
    main()
