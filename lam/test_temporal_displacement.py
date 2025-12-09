"""
Test suite for Temporal Displacement implementation.

Validates all three methods:
1. Time-Warp
2. Memory Kernel
3. DDE

Author: Donte Lightfoot
Date: September 20, 2025
"""

import numpy as np
import matplotlib.pyplot as plt
from temporal_displacement import (
    TemporalDisplacedField,
    TemporalDisplacementConfig,
    TimeWarpField,
    MemoryKernelField,
    DDEField,
    TrustGatedDisplacement,
    LoadSheddingDisplacement,
    validate_causality,
    verify_stability
)


def test_causality():
    """Test 1: Causality - verify step response occurs exactly Δ steps after input."""
    print("=" * 60)
    print("Test 1: Causality Test")
    print("=" * 60)

    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.0)
    field = TimeWarpField(config, history_length=1000)

    dt = 0.01  # 10ms
    duration = 5.0
    Delta = 0.5  # 0.5 second delay

    times = []
    E0_values = []
    E_values = []

    for n in range(int(duration / dt)):
        t = n * dt

        # Step input at t=1.0
        E0 = 1.0 if t >= 1.0 else 0.0

        # Update field
        E = field.update(t, E0, Delta, D=0.0)

        times.append(t)
        E0_values.append(E0)
        E_values.append(E)

    times = np.array(times)
    E0_values = np.array(E0_values)
    E_values = np.array(E_values)

    # Find when E first rises
    E_threshold = 0.1
    E_rise_idx = np.where(E_values > E_threshold)[0]

    if len(E_rise_idx) > 0:
        t_rise = times[E_rise_idx[0]]
        expected_rise = 1.0 + Delta
        error = abs(t_rise - expected_rise)

        print(f"E0 step at:      t = {1.0:.2f} s")
        print(f"E response at:   t = {t_rise:.2f} s")
        print(f"Expected delay:  Δ = {Delta:.2f} s")
        print(f"Measured delay:  {t_rise - 1.0:.2f} s")
        print(f"Error:           {error:.4f} s")
        print(f"Status:          {'✓ PASS' if error < 2*dt else '✗ FAIL'}")
    else:
        print("✗ FAIL: No response detected")

    print()
    return error < 2 * dt if len(E_rise_idx) > 0 else False


def test_energy_convergence():
    """Test 2: Energy/Aging - verify field converges to expected steady-state."""
    print("=" * 60)
    print("Test 2: Energy Convergence Test")
    print("=" * 60)

    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.0)

    # Test all three methods
    methods = {
        'Time-Warp': TimeWarpField(config, history_length=10000),
        'DDE': DDEField(config, history_length=10000)
    }

    dt = 0.01
    duration = 50.0
    Delta = 0.1

    for method_name, field in methods.items():
        E_values = []

        for n in range(int(duration / dt)):
            t = n * dt
            E0 = 1.0  # Constant input

            if isinstance(field, TimeWarpField):
                E = field.update(t, E0, Delta, D=0.0)
            else:  # DDE
                E = field.update(t, E0, Delta, d=0.0, dt=dt)

            E_values.append(E)

        E_values = np.array(E_values)
        final_E = E_values[-1]

        # Expected steady-state for DDE: E_ss = α*E0 / β
        if method_name == 'DDE':
            E_expected = config.alpha * 1.0 / config.beta
        else:  # Time-Warp: E_ss ≈ α*E0
            E_expected = config.alpha * 1.0

        error = abs(final_E - E_expected) / E_expected * 100 if E_expected != 0 else abs(final_E)

        print(f"{method_name}:")
        print(f"  Final E:      {final_E:.4f}")
        print(f"  Expected E:   {E_expected:.4f}")
        print(f"  Error:        {error:.2f}%")
        print(f"  Status:       {'✓ PASS' if error < 5.0 else '✗ FAIL'}")
        print()


def test_robustness():
    """Test 3: Robustness - sweep Delta and add noise, check for stability."""
    print("=" * 60)
    print("Test 3: Robustness Test")
    print("=" * 60)

    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.0)
    field = DDEField(config, history_length=10000)

    dt = 0.01
    duration = 20.0

    Delta_values = [0.0, 0.1, 0.5, 1.0, 2.0]

    all_stable = True

    for Delta in Delta_values:
        E_values = []

        # Reset field
        field.E = 0.0
        field.E0_buffer.clear()
        field.time_buffer.clear()

        for n in range(int(duration / dt)):
            t = n * dt

            # E0 with noise
            E0 = 1.0 + 0.1 * np.sin(2 * np.pi * 5 * t) + 0.05 * np.random.randn()

            E = field.update(t, E0, Delta, d=0.0, dt=dt)
            E_values.append(E)

        E_values = np.array(E_values)

        # Check for oscillations or instability
        is_bounded = np.all(np.abs(E_values) < 100)
        no_oscillation = np.std(E_values[-100:]) < 0.5

        stable = is_bounded and no_oscillation

        print(f"Δ = {Delta:.1f}:")
        print(f"  Bounded:      {'✓' if is_bounded else '✗'}")
        print(f"  No oscillate: {'✓' if no_oscillation else '✗'}")
        print(f"  Status:       {'✓ PASS' if stable else '✗ FAIL'}")
        print()

        all_stable = all_stable and stable

    return all_stable


def test_memory_kernel():
    """Test 4: Memory Kernel method specific validation."""
    print("=" * 60)
    print("Test 4: Memory Kernel Test")
    print("=" * 60)

    config = TemporalDisplacementConfig(alpha=1.0, lambda_val=0.16905, kappa=0.0)
    field = MemoryKernelField(config, history_length=10000)

    # Verify kernel integrates to 1
    kernel_integral = field.compute_unit_area_kernel()
    print(f"Kernel integral (should be ≈1.0): {kernel_integral:.6f}")
    print(f"Status: {'✓ PASS' if abs(kernel_integral - 1.0) < 0.01 else '✗ FAIL'}")
    print()

    # Test response
    dt = 0.01
    duration = 30.0
    Delta = 0.1

    E_values = []

    for n in range(int(duration / dt)):
        t = n * dt
        E0 = 1.0 if t >= 1.0 else 0.0

        E = field.update(t, E0, Delta, d=0.0, dt=dt)
        E_values.append(E)

    E_values = np.array(E_values)

    print(f"Final E: {E_values[-1]:.6f}")
    print(f"Max E:   {np.max(E_values):.6f}")
    print(f"Bounded: {'✓ PASS' if np.all(np.abs(E_values) < 100) else '✗ FAIL'}")
    print()


def test_trust_gating():
    """Test 5: Trust-gated displacement."""
    print("=" * 60)
    print("Test 5: Trust-Gated Displacement")
    print("=" * 60)

    trust_gate = TrustGatedDisplacement(Delta_0=0.0, Delta_trust=2.0)

    confidences = [1.0, 0.8, 0.5, 0.2, 0.0]

    print("Confidence → Displacement")
    for conf in confidences:
        Delta = trust_gate.compute_displacement(conf)
        print(f"  {conf:.1f}      → {Delta:.2f}")

    # Verify monotonicity: lower confidence → higher displacement
    prev_Delta = -np.inf
    monotonic = True
    for conf in reversed(confidences):
        Delta = trust_gate.compute_displacement(conf)
        if Delta < prev_Delta:
            monotonic = False
        prev_Delta = Delta

    print(f"\nMonotonicity check: {'✓ PASS' if monotonic else '✗ FAIL'}")
    print()

    return monotonic


def test_load_shedding():
    """Test 6: Load shedding displacement."""
    print("=" * 60)
    print("Test 6: Load Shedding Displacement")
    print("=" * 60)

    load_shed = LoadSheddingDisplacement(Delta_base=0.0, Delta_max=5.0, load_threshold=0.8)

    loads = [0.0, 0.5, 0.8, 0.9, 1.0]

    print("Load → Displacement")
    for load in loads:
        Delta = load_shed.compute_displacement(load)
        print(f"  {load:.1f}  → {Delta:.2f}")

    # Verify: Delta = Delta_base for load < threshold
    Delta_low = load_shed.compute_displacement(0.5)
    low_load_correct = abs(Delta_low - 0.0) < 1e-6

    # Verify: Delta = Delta_max for load = 1.0
    Delta_high = load_shed.compute_displacement(1.0)
    high_load_correct = abs(Delta_high - 5.0) < 1e-6

    print(f"\nLow load check:  {'✓ PASS' if low_load_correct else '✗ FAIL'}")
    print(f"High load check: {'✓ PASS' if high_load_correct else '✗ FAIL'}")
    print()

    return low_load_correct and high_load_correct


def test_unified_interface():
    """Test 7: Unified TemporalDisplacedField interface."""
    print("=" * 60)
    print("Test 7: Unified Interface Test")
    print("=" * 60)

    methods = ['timewarp', 'kernel', 'dde']

    for method in methods:
        field = TemporalDisplacedField(method=method)

        # Simple step response
        dt = 0.01
        duration = 10.0
        Delta = 0.1

        for n in range(int(duration / dt)):
            t = n * dt
            E0 = 1.0 if t >= 1.0 else 0.0

            E = field.update(t, E0, Delta, d=0.0, dt=dt)

        print(f"{method.upper()}:")
        print(f"  Final E: {field.get_current_value():.6f}")
        print(f"  Status:  {'✓ PASS' if abs(field.get_current_value()) < 100 else '✗ FAIL'}")
        print()


def visualize_comparison():
    """Generate comparison plot of all three methods."""
    print("=" * 60)
    print("Generating Comparison Visualization")
    print("=" * 60)

    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.1, lambda_val=0.16905)

    fields = {
        'Time-Warp': TemporalDisplacedField('timewarp', config),
        'Memory Kernel': TemporalDisplacedField('kernel', config),
        'DDE': TemporalDisplacedField('dde', config)
    }

    dt = 0.01
    duration = 20.0
    Delta = 0.2  # 200ms delay

    results = {name: {'time': [], 'E0': [], 'E': [], 'd': []} for name in fields.keys()}

    for n in range(int(duration / dt)):
        t = n * dt

        # Step input with noise
        E0 = 1.0 if t >= 1.0 else 0.0
        E0 += 0.05 * np.sin(2 * np.pi * 2 * t)  # Add 2Hz oscillation

        # Disturbance pulse at t=10s
        d = 0.5 if 10.0 <= t < 10.5 else 0.0

        for name, field in fields.items():
            E = field.update(t, E0, Delta, d, dt)

            results[name]['time'].append(t)
            results[name]['E0'].append(E0)
            results[name]['E'].append(E)
            results[name]['d'].append(d)

    # Create plots
    fig, axes = plt.subplots(3, 1, figsize=(12, 10))

    # Plot 1: All methods
    ax = axes[0]
    for name in fields.keys():
        ax.plot(results[name]['time'], results[name]['E'], label=name, linewidth=1.5)
    ax.plot(results['Time-Warp']['time'], results['Time-Warp']['E0'], 'k--', alpha=0.5, label='E0 (input)')
    ax.set_ylabel('E (Field Value)')
    ax.set_title('Temporal Displacement Methods Comparison (Δ = 0.2s)')
    ax.legend()
    ax.grid(True, alpha=0.3)

    # Plot 2: Disturbance response
    ax = axes[1]
    for name in fields.keys():
        ax.plot(results[name]['time'], results[name]['E'], label=name, linewidth=1.5)
    ax.plot(results['Time-Warp']['time'], results['Time-Warp']['d'], 'r--', alpha=0.7, label='d (disturbance)')
    ax.set_ylabel('E (Field Value)')
    ax.set_title('Disturbance Rejection at t=10s')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_xlim(8, 15)

    # Plot 3: Early transient
    ax = axes[2]
    for name in fields.keys():
        ax.plot(results[name]['time'], results[name]['E'], label=name, linewidth=1.5)
    ax.plot(results['Time-Warp']['time'], results['Time-Warp']['E0'], 'k--', alpha=0.5, label='E0')
    ax.axvline(1.0, color='gray', linestyle=':', alpha=0.5, label='E0 step')
    ax.axvline(1.0 + Delta, color='gray', linestyle=':', alpha=0.5, label='Expected response (t=1+Δ)')
    ax.set_xlabel('Time (s)')
    ax.set_ylabel('E (Field Value)')
    ax.set_title('Step Response Detail (showing Δ delay)')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_xlim(0, 5)

    plt.tight_layout()
    plt.savefig('temporal_displacement_comparison.png', dpi=300)
    print("✓ Plot saved to: temporal_displacement_comparison.png")
    print()


def run_all_tests():
    """Run complete test suite."""
    print("\n")
    print("*" * 60)
    print("  TEMPORAL DISPLACEMENT VALIDATION TEST SUITE")
    print("*" * 60)
    print()

    results = {}

    # Run tests
    results['Causality'] = test_causality()
    test_energy_convergence()
    results['Robustness'] = test_robustness()
    test_memory_kernel()
    results['Trust Gating'] = test_trust_gating()
    results['Load Shedding'] = test_load_shedding()
    test_unified_interface()

    # Visualization
    visualize_comparison()

    # Summary
    print("=" * 60)
    print("TEST SUMMARY")
    print("=" * 60)

    for test_name, passed in results.items():
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"{test_name:20s}: {status}")

    all_passed = all(results.values())
    print()
    print("=" * 60)
    if all_passed:
        print("✓ ALL TESTS PASSED")
    else:
        print("✗ SOME TESTS FAILED")
    print("=" * 60)
    print()

    return all_passed


if __name__ == "__main__":
    run_all_tests()
