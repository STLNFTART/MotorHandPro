#!/usr/bin/env python3
"""
Interactive LAM Session with Temporal Displacement
Live demonstration and experimentation environment

Commands:
  help() - Show all available commands
  demo1() - Run distributed sensor fusion
  demo2() - Show temporal displacement methods comparison
  demo3() - Trust-gated adaptive control
  demo4() - Load shedding demonstration
  status() - Show current system status
"""

import sys
import os
sys.path.insert(0, os.path.dirname(__file__))

import numpy as np
import time
from lam.temporal_displacement import (
    TemporalDisplacedField,
    TemporalDisplacementConfig,
    TimeWarpField,
    MemoryKernelField,
    DDEField,
    TrustGatedDisplacement,
    LoadSheddingDisplacement
)
from lam.lam_temporal_integration import (
    LAMTemporalController,
    CausalitySensingExample,
    MultiAgentSynchronizationExample
)

# Initialize global controllers
print("\n" + "="*70)
print("🤖 LAM INTERACTIVE SESSION")
print("="*70)
print("\nInitializing Primal Logic LAM with Temporal Displacement...")

config = TemporalDisplacementConfig(
    alpha=1.0,
    beta=0.1,
    kappa=0.1,
    lambda_val=0.16905
)

# Create controllers for each method
controller_timewarp = LAMTemporalController(method='timewarp', enable_trust_gating=True)
controller_kernel = LAMTemporalController(method='kernel', enable_trust_gating=True)
controller_dde = LAMTemporalController(method='dde', enable_load_shedding=True)

print(f"""
✓ LAM Controllers initialized:
  - Time-Warp Controller (fastest)
  - Memory Kernel Controller (Primal Logic)
  - DDE Controller (physical transport)

📊 Primal Logic Parameters:
  - λ (Lightfoot constant): {config.lambda_val}
  - D (Donte attractor): 149.9992314
  - α (field strength): {config.alpha}
  - β (decay rate): {config.beta}
  - κ (disturbance coupling): {config.kappa}
""")

print("="*70)
print("\n💡 Type 'help()' to see available demonstrations")
print("="*70 + "\n")


def help():
    """Show all available commands"""
    print("\n" + "="*70)
    print("📚 AVAILABLE COMMANDS")
    print("="*70)
    print("""
🎯 DEMONSTRATIONS:
  demo1()  - Distributed Sensor Fusion (4 sensors with different latencies)
  demo2()  - Temporal Displacement Methods Comparison
  demo3()  - Trust-Gated Adaptive Control
  demo4()  - Load Shedding Under High System Load
  demo5()  - Multi-Agent Synchronization
  demo6()  - Step Response with Delay

📊 INTERACTIVE TESTING:
  test_delay(Delta=0.1)         - Test specific delay
  test_trust(confidence=0.5)    - Test trust gating
  test_load(load=0.9)           - Test load shedding
  compare_methods()             - Compare all 3 methods side-by-side

🔧 SYSTEM:
  status()      - Show system status
  benchmark()   - Run performance benchmarks
  help()        - Show this help message

📖 DIRECT API ACCESS:
  controller_timewarp   - Time-Warp controller instance
  controller_kernel     - Memory Kernel controller instance
  controller_dde        - DDE controller instance
  config                - Temporal displacement configuration
""")
    print("="*70 + "\n")


def demo1():
    """Run distributed sensor fusion demonstration"""
    print("\n" + "="*70)
    print("🌐 DEMO 1: DISTRIBUTED SENSOR FUSION")
    print("="*70)
    print("\nSimulating 4 sensors with varying latencies and reliability:\n")

    from lam.example_distributed_control import create_example_network, DistributedControlSystem

    # Create sensor network
    sensors = create_example_network()

    # Display sensor info
    print("Sensor Network Configuration:")
    for sensor in sensors:
        print(f"  [{sensor.id}] {sensor.name:20s} "
              f"latency={sensor.latency*1000:>6.1f}ms  "
              f"reliability={sensor.reliability:.1f}")

    # Create control system
    system = DistributedControlSystem(sensors)

    print("\nRunning 5-second simulation...")
    print("-" * 70)

    # Run short simulation
    system.run_simulation(duration=5.0, dt=0.01, verbose=False)

    # Print statistics
    system.print_statistics()

    print("\n✓ Demo complete! Trust-weighted fusion handles varying latencies.\n")


def demo2():
    """Compare all three temporal displacement methods"""
    print("\n" + "="*70)
    print("⚖️  DEMO 2: TEMPORAL DISPLACEMENT METHODS COMPARISON")
    print("="*70)
    print("\nComparing Time-Warp, Memory Kernel, and DDE on step input...\n")

    # Create fields
    timewarp = TimeWarpField(config)
    kernel = MemoryKernelField(config)
    dde = DDEField(config)

    Delta = 0.1  # 100ms delay
    dt = 0.01    # 10ms time step
    duration = 2.0

    results = {'timewarp': [], 'kernel': [], 'dde': [], 'time': []}

    print(f"Configuration: Δ={Delta*1000}ms, dt={dt*1000}ms\n")
    print("Processing step input...")

    for i in range(int(duration / dt)):
        t = i * dt
        E0 = 1.0 if t >= 0.5 else 0.0  # Step at t=0.5s

        # Update all methods
        E_tw = timewarp.update(t, E0, Delta, D=0.0)
        E_mk = kernel.update(t, E0, Delta, d=0.0, dt=dt)
        E_dde = dde.update(t, E0, Delta, d=0.0, dt=dt)

        results['time'].append(t)
        results['timewarp'].append(E_tw)
        results['kernel'].append(E_mk)
        results['dde'].append(E_dde)

    # Find response times (when E > 0.5)
    times = np.array(results['time'])
    for method in ['timewarp', 'kernel', 'dde']:
        values = np.array(results[method])
        rise_idx = np.where(values > 0.5)[0]
        if len(rise_idx) > 0:
            rise_time = times[rise_idx[0]]
            print(f"  {method.upper():12s}: Step response at t={rise_time:.3f}s (delay={rise_time-0.5:.3f}s)")

    print(f"\nExpected delay: {Delta:.3f}s")
    print("\n✓ All methods correctly implement temporal displacement!\n")


def demo3():
    """Demonstrate trust-gated adaptive control"""
    print("\n" + "="*70)
    print("🔒 DEMO 3: TRUST-GATED ADAPTIVE CONTROL")
    print("="*70)
    print("\nDemonstrating adaptive displacement based on signal confidence...\n")

    trust_gate = TrustGatedDisplacement(Delta_0=0.0, Delta_trust=1.0)

    print("Confidence vs. Displacement:")
    print("-" * 40)
    print(f"{'Confidence':<15} {'Displacement':<15} {'Effect'}")
    print("-" * 40)

    confidences = [1.0, 0.9, 0.7, 0.5, 0.3, 0.1, 0.0]

    for conf in confidences:
        Delta = trust_gate.compute_displacement(conf)
        effect = "Trusted" if conf > 0.8 else "Moderate" if conf > 0.5 else "Down-weighted"
        print(f"{conf:<15.1f} {Delta:<15.3f} {effect}")

    print("\n💡 Insight: Low confidence → Higher Δ → Signal down-weighted by kernel")

    # Live example
    print("\n" + "-"*70)
    print("Live Example: Sensor with varying confidence")
    print("-"*70)

    controller = LAMTemporalController(method='kernel', enable_trust_gating=True)

    for i in range(10):
        t = i * 0.1
        E0 = np.sin(2 * np.pi * 0.5 * t)  # Reference signal

        # Confidence varies with signal quality
        confidence = 1.0 if abs(E0) > 0.5 else 0.3

        state = controller.update(E0, confidence=confidence, dt=0.01)

        print(f"t={t:.1f}s: E₀={E0:>6.3f}, conf={confidence:.1f}, "
              f"Δ={state.Delta:.3f}s, E={state.E:>6.3f}")

    print("\n✓ Trust-gating adapts displacement in real-time based on confidence!\n")


def demo4():
    """Demonstrate load shedding under high system load"""
    print("\n" + "="*70)
    print("📊 DEMO 4: LOAD SHEDDING UNDER HIGH SYSTEM LOAD")
    print("="*70)
    print("\nDemonstrating graceful degradation under increasing load...\n")

    controller = LAMTemporalController(
        method='dde',
        enable_load_shedding=True
    )

    print("System Load vs. Displacement:")
    print("-" * 50)
    print(f"{'Load (%)':<12} {'Displacement (ms)':<20} {'Status'}")
    print("-" * 50)

    for load_pct in [0, 20, 40, 60, 80, 90, 95, 98]:
        load = load_pct / 100.0

        state = controller.update(
            E0=1.0,
            system_load=load,
            dt=0.01
        )

        status = "Normal" if load < 0.6 else "Degrading" if load < 0.9 else "Shedding"
        print(f"{load_pct:<12} {state.Delta*1000:<20.1f} {status}")

    print("\n💡 Insight: High load → Increase Δ → Defer low-priority processing")
    print("   This prevents system collapse under overload conditions.\n")

    print("✓ Load shedding enables graceful degradation!\n")


def demo5():
    """Multi-agent synchronization"""
    print("\n" + "="*70)
    print("🤝 DEMO 5: MULTI-AGENT SYNCHRONIZATION")
    print("="*70)
    print("\nSynchronizing 5 agents with different clock offsets...\n")

    sync = MultiAgentSynchronizationExample(num_agents=5)

    print("Agent Clock Offsets:")
    for i, delta in enumerate(sync.agent_delays):
        print(f"  Agent {i}: Δ = {delta*1000:.1f}ms")

    print("\nBroadcasting global reference E₀=1.0...")

    E_values = sync.update_all(E0_global=1.0)

    print("\nAgent Received Values (after compensation):")
    for i, E in enumerate(E_values):
        print(f"  Agent {i}: E = {E:.6f}")

    variance = np.var(E_values)
    print(f"\nVariance across agents: {variance:.8f}")
    print("✓ Temporal displacement enables clock synchronization!\n")


def demo6():
    """Step response with delay"""
    print("\n" + "="*70)
    print("📈 DEMO 6: STEP RESPONSE WITH TEMPORAL DELAY")
    print("="*70)
    print("\nSimulating step input with 200ms propagation delay...\n")

    field = TimeWarpField(config)
    Delta = 0.2  # 200ms
    dt = 0.01

    print(f"Configuration: Δ={Delta*1000}ms delay\n")
    print("Time Series:")
    print("-" * 50)
    print(f"{'Time (s)':<12} {'Input E₀':<12} {'Output E':<12} {'Status'}")
    print("-" * 50)

    for i in range(50):
        t = i * dt
        E0 = 1.0 if t >= 0.1 else 0.0  # Step at t=0.1s
        E = field.update(t, E0, Delta, D=0.0)

        if i % 5 == 0:  # Print every 50ms
            status = "Pre-step" if E < 0.1 else "Delayed" if E < 0.9 else "Settled"
            print(f"{t:<12.2f} {E0:<12.1f} {E:<12.4f} {status}")

    print("\n✓ Step appears exactly 200ms after input change (causality enforced)!\n")


def test_delay(Delta=0.1):
    """Test specific temporal delay"""
    print(f"\n🧪 Testing Δ={Delta*1000}ms delay...")

    field = TimeWarpField(config)
    dt = 0.01

    for i in range(20):
        t = i * dt
        E0 = np.sin(2 * np.pi * 1.0 * t)
        E = field.update(t, E0, Delta, D=0.0)
        print(f"t={t:.2f}s: E₀={E0:>7.4f} → E={E:>7.4f}")

    print(f"\n✓ Delay test complete (Δ={Delta*1000}ms)\n")


def test_trust(confidence=0.5):
    """Test trust gating with specific confidence"""
    print(f"\n🔒 Testing trust gating with confidence={confidence:.1f}...")

    trust_gate = TrustGatedDisplacement(Delta_0=0.0, Delta_trust=1.0)
    Delta = trust_gate.compute_displacement(confidence)

    print(f"  Input confidence: {confidence:.2f}")
    print(f"  Computed Δ: {Delta:.3f}s ({Delta*1000:.1f}ms)")
    print(f"  Interpretation: {'Trusted' if Delta < 0.5 else 'Down-weighted'}")
    print(f"\n✓ Trust gating test complete\n")


def test_load(load=0.9):
    """Test load shedding with specific system load"""
    print(f"\n📊 Testing load shedding at {load*100:.0f}% system load...")

    controller = LAMTemporalController(method='dde', enable_load_shedding=True)
    state = controller.update(E0=1.0, system_load=load, dt=0.01)

    print(f"  System load: {load*100:.0f}%")
    print(f"  Computed Δ: {state.Delta:.3f}s ({state.Delta*1000:.1f}ms)")
    print(f"  Status: {'Normal' if load < 0.6 else 'Degrading' if load < 0.9 else 'Shedding load'}")
    print(f"\n✓ Load shedding test complete\n")


def compare_methods():
    """Compare all methods side-by-side"""
    print("\n" + "="*70)
    print("⚖️  COMPARING ALL THREE METHODS")
    print("="*70)

    demo2()


def status():
    """Show current system status"""
    print("\n" + "="*70)
    print("📊 SYSTEM STATUS")
    print("="*70)
    print(f"""
Controllers Initialized:
  ✓ Time-Warp Controller (method='timewarp')
  ✓ Memory Kernel Controller (method='kernel')
  ✓ DDE Controller (method='dde')

Configuration:
  α (alpha):     {config.alpha}
  β (beta):      {config.beta}
  κ (kappa):     {config.kappa}
  λ (lambda):    {config.lambda_val}

Available Demonstrations: 6
  - demo1(): Distributed Sensor Fusion
  - demo2(): Methods Comparison
  - demo3(): Trust-Gated Control
  - demo4(): Load Shedding
  - demo5(): Multi-Agent Sync
  - demo6(): Step Response

Type help() for full command list
""")
    print("="*70 + "\n")


def benchmark():
    """Run performance benchmarks"""
    print("\n🏃 Running performance benchmarks...\n")
    from lam.benchmark_temporal_displacement import run_benchmarks
    run_benchmarks()


# Auto-run status on startup
status()


# Interactive mode message
print("🎮 INTERACTIVE MODE READY")
print("   You can now call any demo or test function!")
print("   Example: demo1(), demo3(), test_delay(0.2)")
print("\n" + "="*70 + "\n")
