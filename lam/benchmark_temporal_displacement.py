"""
Performance Benchmarks for Temporal Displacement

Measures execution time and throughput for all three methods.

Author: Donte Lightfoot
Date: November 30, 2025
"""

import time
import numpy as np
from temporal_displacement import (
    TemporalDisplacedField,
    TemporalDisplacementConfig,
    TimeWarpField,
    MemoryKernelField,
    DDEField
)


def benchmark_method(method_name, field, iterations=10000, dt=0.01):
    """
    Benchmark a temporal displacement method.

    Args:
        method_name: Name of method for display
        field: Field instance to benchmark
        iterations: Number of iterations to run
        dt: Time step

    Returns:
        dict: Benchmark results
    """
    print(f"\nBenchmarking {method_name}...")

    # Warmup
    for i in range(10):
        t = i * dt
        E0 = np.sin(2 * np.pi * t)
        if isinstance(field, TimeWarpField):
            field.update(t, E0, Delta=0.1, D=0.0)
        else:
            field.update(t, E0, Delta=0.1, d=0.0, dt=dt)

    # Reset for actual benchmark (if method exists)
    if hasattr(field, 'reset'):
        field.reset()

    # Benchmark
    start_time = time.time()

    for i in range(iterations):
        t = i * dt
        E0 = np.sin(2 * np.pi * t)
        Delta = 0.1

        if isinstance(field, TimeWarpField):
            E = field.update(t, E0, Delta, D=0.0)
        else:
            E = field.update(t, E0, Delta, d=0.0, dt=dt)

    end_time = time.time()

    elapsed = end_time - start_time
    updates_per_sec = iterations / elapsed
    us_per_update = (elapsed / iterations) * 1e6

    results = {
        'method': method_name,
        'iterations': iterations,
        'elapsed_sec': elapsed,
        'updates_per_sec': updates_per_sec,
        'us_per_update': us_per_update
    }

    print(f"  Iterations:       {iterations:,}")
    print(f"  Elapsed time:     {elapsed:.3f} s")
    print(f"  Updates/sec:      {updates_per_sec:,.0f}")
    print(f"  Time/update:      {us_per_update:.2f} µs")

    return results


def run_benchmarks():
    """Run comprehensive benchmarks for all methods."""
    print("=" * 70)
    print("TEMPORAL DISPLACEMENT PERFORMANCE BENCHMARKS")
    print("=" * 70)

    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.1, lambda_val=0.16905)

    # Configuration
    iterations = 1000  # Reduced for faster benchmarking
    print(f"\nConfiguration:")
    print(f"  Iterations:   {iterations:,}")
    print(f"  α (alpha):    {config.alpha}")
    print(f"  β (beta):     {config.beta}")
    print(f"  κ (kappa):    {config.kappa}")
    print(f"  λ (lambda):   {config.lambda_val}")

    # Benchmark all three methods
    results = []

    # Time-Warp
    field = TimeWarpField(config, history_length=10000)
    result = benchmark_method("Time-Warp", field, iterations)
    results.append(result)

    # Memory Kernel
    field = MemoryKernelField(config, history_length=10000)
    result = benchmark_method("Memory Kernel", field, iterations)
    results.append(result)

    # DDE
    field = DDEField(config, history_length=10000)
    result = benchmark_method("DDE", field, iterations)
    results.append(result)

    # Summary comparison
    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print(f"\n{'Method':<20} {'Updates/sec':<15} {'µs/update':<12} {'Relative':<10}")
    print("-" * 70)

    baseline_speed = results[0]['updates_per_sec']

    for r in results:
        relative = r['updates_per_sec'] / baseline_speed
        print(f"{r['method']:<20} {r['updates_per_sec']:>12,.0f}  "
              f"{r['us_per_update']:>10.2f}  {relative:>9.2f}x")

    # Determine fastest
    fastest = max(results, key=lambda x: x['updates_per_sec'])
    print(f"\n✓ Fastest: {fastest['method']} at {fastest['updates_per_sec']:,.0f} updates/sec")

    # System info
    print("\n" + "=" * 70)
    print("SYSTEM INFO")
    print("=" * 70)
    import platform
    import sys

    print(f"  Platform:   {platform.system()} {platform.release()}")
    print(f"  Processor:  {platform.processor() or 'Unknown'}")
    print(f"  Python:     {sys.version.split()[0]}")
    print(f"  NumPy:      {np.__version__}")

    # Control loop frequency estimates
    print("\n" + "=" * 70)
    print("CONTROL LOOP FREQUENCY ESTIMATES")
    print("=" * 70)
    print("\nMaximum sustainable control loop frequencies:")
    for r in results:
        freq_hz = r['updates_per_sec']
        print(f"  {r['method']:<20} {freq_hz:>10,.0f} Hz")

    # Practical recommendations
    print("\n" + "=" * 70)
    print("RECOMMENDATIONS")
    print("=" * 70)
    print("\nPython Implementation:")
    print("  Time-Warp:      Best for simple delays, fastest performance")
    print("  Memory Kernel:  Best for complex temporal dependencies")
    print("  DDE:            Best for physical transport phenomena")
    print("\nFor > 10kHz control loops, use D language implementation")
    print("(Expected speedup: 25-100x)")

    return results


if __name__ == "__main__":
    results = run_benchmarks()
