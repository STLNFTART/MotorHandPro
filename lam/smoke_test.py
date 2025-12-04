"""
Simple smoke test for Temporal Displacement implementation.

Quick validation to ensure basic functionality works.
"""

import sys
sys.path.insert(0, '/home/user/MotorHandPro/lam')

from temporal_displacement import (
    TemporalDisplacedField,
    TemporalDisplacementConfig,
    TimeWarpField,
    TrustGatedDisplacement,
    LoadSheddingDisplacement
)

print("=" * 60)
print("TEMPORAL DISPLACEMENT SMOKE TEST")
print("=" * 60)
print()

# Test 1: Basic instantiation
print("Test 1: Basic Instantiation")
try:
    config = TemporalDisplacementConfig(alpha=1.0, beta=0.1, kappa=0.1)
    field = TemporalDisplacedField(method='timewarp', config=config)
    print("✓ PASS: TemporalDisplacedField created successfully")
except Exception as e:
    print(f"✗ FAIL: {e}")
    sys.exit(1)

print()

# Test 2: Simple update
print("Test 2: Simple Update")
try:
    E = field.update(t=1.0, E0=1.5, Delta=0.1, d=0.0, dt=0.01)
    print(f"✓ PASS: Update successful, E = {E:.6f}")
except Exception as e:
    print(f"✗ FAIL: {e}")
    sys.exit(1)

print()

# Test 3: All three methods
print("Test 3: All Three Methods")
methods = ['timewarp', 'kernel', 'dde']
for method in methods:
    try:
        test_field = TemporalDisplacedField(method=method, config=config)
        E = test_field.update(t=1.0, E0=1.0, Delta=0.1, d=0.0, dt=0.01)
        print(f"✓ PASS: {method.upper()} method works, E = {E:.6f}")
    except Exception as e:
        print(f"✗ FAIL: {method.upper()} - {e}")
        sys.exit(1)

print()

# Test 4: Trust gating
print("Test 4: Trust-Gated Displacement")
try:
    trust_gate = TrustGatedDisplacement(Delta_0=0.0, Delta_trust=1.0)
    Delta = trust_gate.compute_displacement(confidence=0.5)
    print(f"✓ PASS: Trust gating works, Δ(conf=0.5) = {Delta:.3f}")
except Exception as e:
    print(f"✗ FAIL: {e}")
    sys.exit(1)

print()

# Test 5: Load shedding
print("Test 5: Load Shedding")
try:
    load_shed = LoadSheddingDisplacement(Delta_base=0.0, Delta_max=2.0, load_threshold=0.8)
    Delta = load_shed.compute_displacement(load=0.9)
    print(f"✓ PASS: Load shedding works, Δ(load=0.9) = {Delta:.3f}")
except Exception as e:
    print(f"✗ FAIL: {e}")
    sys.exit(1)

print()

# Test 6: Step response
print("Test 6: Step Response (10 iterations)")
try:
    field.reset()
    for i in range(10):
        t = i * 0.01
        E0 = 1.0 if t >= 0.05 else 0.0
        E = field.update(t, E0, Delta=0.02, d=0.0, dt=0.01)

    print(f"✓ PASS: Step response completed, final E = {E:.6f}")
except Exception as e:
    print(f"✗ FAIL: {e}")
    sys.exit(1)

print()
print("=" * 60)
print("✓ ALL SMOKE TESTS PASSED")
print("=" * 60)
print()
print("Temporal Displacement implementation is functional!")
print("Run 'python test_temporal_displacement.py' for comprehensive tests.")
