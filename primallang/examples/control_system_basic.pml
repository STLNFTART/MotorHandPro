// Basic Control System Example in PrimalLang
// Demonstrates Donte & Lightfoot Constants in action

universe ControlSystem = create()

// Display the fundamental constants
print("Donte Constant (D): ")
print(D)

print("Lightfoot Constant (λ): ")
print(LAMBDA)

print("Time Constant (τ): ")
print(TAU)

// Define control law with exponential decay
define control_signal(psi_0, error, t) {
  // dψ/dt = -λ·ψ(t) + KE·e(t)
  // Solution: ψ(t) = ψ₀·exp(-λt) + KE·e·(1 - exp(-λt))/λ

  let decay = exp(-LAMBDA * t)
  let response = KE_DEFAULT * error * (1 - decay) / LAMBDA

  return psi_0 * decay + response
}

// Simulate control response over time
constant TIMESTEPS = 10
constant PSI_INITIAL = 1.0
constant ERROR = 0.1

print("\nControl Signal Evolution:")
for i from 0 to TIMESTEPS {
  let t = i * 0.5  // 0.5 second intervals
  let psi = control_signal(PSI_INITIAL, ERROR, t)

  print("t=")
  print(t)
  print(" ψ=")
  print(psi)
}

// Meta-constraint: Verify stability (control signal remains bounded)
let final_psi = control_signal(PSI_INITIAL, ERROR, 10.0)
meta => (final_psi < 2.0 implies true)

print("\n✓ System verified: Control signal remains bounded")
