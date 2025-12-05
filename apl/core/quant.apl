⍝ Primal Logic Kernel - Core Mathematical Implementation
⍝ Implements exponential memory weighting, fixed-point iteration,
⍝ and stability analysis using the Recursive Planck Operator framework

⍝ Load constants (assumes constants.apl is already loaded)
⍝ Use: ]LOAD constants

⍝═══════════════════════════════════════════════════════════
⍝ EXPONENTIAL MEMORY WEIGHTING
⍝═══════════════════════════════════════════════════════════

⍝ Generate exponential decay weights using Lightfoot constant
⍝ Usage: weights ← ExponentialWeights N
⍝ N: number of time steps
⍝ Returns: vector of length N with exponential decay
∇ weights ← ExponentialWeights N
  weights ← *(-LIGHTFOOT) × ⍳N
∇

⍝ Apply exponential memory weighting to time series
⍝ Usage: weighted ← WeightedMemory data
⍝ data: vector of historical values
⍝ Returns: exponentially weighted sum
∇ weighted ← WeightedMemory data
  N ← ≢data
  weights ← ExponentialWeights N
  weighted ← +/ weights × ⌽data  ⍝ Reverse so most recent has highest weight
∇

⍝═══════════════════════════════════════════════════════════
⍝ FIXED-POINT ITERATION
⍝═══════════════════════════════════════════════════════════

⍝ The Primal Logic iteration function F(x)
⍝ F(x) = D - I3 × ln(S - x)
⍝ Where S = D/I3 ≈ 23.098
∇ result ← F x
  result ← DONTE - I3 × ⍟ SCALING - x
∇

⍝ Single iteration of Primal Logic operator
⍝ Usage: xnext ← PrimalStep x
∇ xnext ← PrimalStep x
  xnext ← F x
∇

⍝ Iterate until convergence (power operator)
⍝ Usage: converged ← PrimalConverge x0
⍝ x0: initial value
⍝ Returns: converged value (should be ≈ DONTE)
∇ converged ← PrimalConverge x0
  converged ← F⍣≡ x0  ⍝ Power operator with ≡ (match) limit
∇

⍝ Iterate N times (fixed iterations)
⍝ Usage: result ← PrimalIterate N x0
∇ result ← N PrimalIterate x0
  result ← F⍣N ⊢ x0
∇

⍝ Track convergence history
⍝ Usage: history ← PrimalHistory N x0
⍝ Returns: matrix where each row is an iteration
∇ history ← N PrimalHistory x0
  history ← {⍵, F ⍵}⍣N ⊢ x0
∇

⍝═══════════════════════════════════════════════════════════
⍝ PLANCK TAIL SERIES
⍝═══════════════════════════════════════════════════════════

⍝ Compute single term of Planck tail series
⍝ T_k = exp(-λ × k) × f(x_k)
∇ term ← k PlanckTerm x
  term ← (*(-LIGHTFOOT) × k) × F x
∇

⍝ Compute Planck tail series sum
⍝ Usage: sum ← N PlanckTailSum x0
⍝ N: number of terms
⍝ x0: initial value
∇ sum ← N PlanckTailSum x0
  iterations ← F⍣(⍳N) ⊢ x0  ⍝ All N iterations
  weights ← *(-LIGHTFOOT) × ⍳N
  sum ← +/ weights × iterations
∇

⍝═══════════════════════════════════════════════════════════
⍝ STABILITY ANALYSIS
⍝═══════════════════════════════════════════════════════════

⍝ Compute derivative F'(x) = -I3/(S - x)
⍝ For Lipschitz constant estimation
∇ deriv ← FPrime x
  deriv ← -I3 ÷ SCALING - x
∇

⍝ Check if system is contractive at point x
⍝ Returns 1 if |F'(x)| < 1, else 0
∇ stable ← IsStable x
  stable ← (|FPrime x) < 1
∇

⍝ Compute Lipschitz constant at Donte fixed point
⍝ Should match LIPSCHITZ constant (≈ 0.00013)
∇ L ← ComputeLipschitz
  L ← |FPrime DONTE|
∇

⍝ Verify system stability
⍝ Checks multiple conditions for guaranteed convergence
∇ result ← VerifyStability
  ⍝ Check Lipschitz bound at fixed point
  lip ← ComputeLipschitz
  lip_ok ← lip < 1

  ⍝ Check contractivity in neighborhood of fixed point
  neighborhood ← DONTE + (0.01 × ¯10 + ⍳21)
  all_stable ← ∧/ IsStable neighborhood

  ⍝ Check exponential decay
  decay_ok ← LIGHTFOOT > 0

  result ← lip_ok ∧ all_stable ∧ decay_ok

  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← '         STABILITY VERIFICATION'
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← 'Lipschitz constant: ', lip, (lip_ok/' ✓'),(~lip_ok)/' ✗'
  ⎕ ← 'Neighborhood stable: ', (all_stable/' ✓'),(~all_stable)/' ✗'
  ⎕ ← 'Exponential decay: ', (decay_ok/' ✓'),(~decay_ok)/' ✗'
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← 'Overall: ', (result/'STABLE ✓'),(~result)/'UNSTABLE ✗'
∇

⍝═══════════════════════════════════════════════════════════
⍝ QUANTUM RESONANCE FIELD
⍝═══════════════════════════════════════════════════════════

⍝ Initialize quantum field state
⍝ Returns: vector of [position, velocity, acceleration]
∇ state ← InitQuantumField
  state ← DONTE 0 0  ⍝ Start at fixed point with zero derivatives
∇

⍝ Update quantum field with new observation
⍝ Uses exponential memory weighting
∇ new_state ← state UpdateQuantumField obs
  ⍝ Extract current position
  pos ← 1⊃state

  ⍝ Compute new position with exponential decay
  new_pos ← pos + (*(-LIGHTFOOT) × obs - pos)

  ⍝ Compute velocity (discrete derivative)
  vel ← new_pos - pos

  ⍝ Compute acceleration
  old_vel ← 2⊃state
  acc ← vel - old_vel

  new_state ← new_pos vel acc
∇

⍝ Check if field is within semantic bounds
∇ ok ← CheckSemanticBounds state
  pos ← 1⊃state
  ok ← (pos ≥ SEMANTIC_LOWER) ∧ (pos ≤ SEMANTIC_UPPER)
∇

⍝═══════════════════════════════════════════════════════════
⍝ DEMONSTRATION AND TESTING
⍝═══════════════════════════════════════════════════════════

⍝ Run comprehensive demonstration
∇ Demo
  ⎕ ← ''
  ⎕ ← '╔═══════════════════════════════════════════════════╗'
  ⎕ ← '║   PRIMAL LOGIC KERNEL DEMONSTRATION               ║'
  ⎕ ← '╚═══════════════════════════════════════════════════╝'
  ⎕ ← ''

  ⍝ Display constants
  {}DisplayConstants
  ⎕ ← ''

  ⍝ Validate constants
  {}ValidateConstants
  ⎕ ← ''

  ⍝ Test convergence from different initial points
  ⎕ ← '🔷 Testing convergence from x₀ = 0:'
  result0 ← PrimalConverge 0
  ⎕ ← '  Converged to: ', result0
  ⎕ ← '  Error from D: ', |result0 - DONTE|
  ⎕ ← ''

  ⎕ ← '🔷 Testing convergence from x₀ = 100:'
  result100 ← PrimalConverge 100
  ⎕ ← '  Converged to: ', result100
  ⎕ ← '  Error from D: ', |result100 - DONTE|
  ⎕ ← ''

  ⍝ Show 10 iteration history
  ⎕ ← '🔷 Iteration history (first 10 steps from x₀ = 0):'
  hist ← 10 PrimalHistory 0
  ⎕ ← hist
  ⎕ ← ''

  ⍝ Compute Planck tail series
  ⎕ ← '🔷 Planck tail series (50 terms):'
  tail ← 50 PlanckTailSum 0
  ⎕ ← '  Sum: ', tail
  ⎕ ← ''

  ⍝ Verify stability
  {}VerifyStability
  ⎕ ← ''

  ⍝ Test quantum field
  ⎕ ← '🔷 Quantum field simulation (10 random observations):'
  field ← InitQuantumField
  obs ← DONTE + (? 10⍴20) - 10  ⍝ Random observations near fixed point

  field ← field{⍺ UpdateQuantumField ⍵}⍣10 ⊢ obs
  ⎕ ← '  Final field state: ', field
  ⎕ ← '  Within bounds: ', (CheckSemanticBounds field)/'YES','NO'
  ⎕ ← ''

  ⎕ ← '╔═══════════════════════════════════════════════════╗'
  ⎕ ← '║   DEMONSTRATION COMPLETE                          ║'
  ⎕ ← '╚═══════════════════════════════════════════════════╝'
∇

⍝ Run all tests
∇ r ← RunTests
  ⎕ ← 'Running Primal Logic Kernel tests...'

  ⍝ Test 1: Convergence to Donte
  test1 ← (⌊0.5 + PrimalConverge 0) = ⌊0.5 + DONTE
  ⎕ ← 'Test 1 - Convergence: ', (test1/'PASS'),(~test1)/'FAIL'

  ⍝ Test 2: Lipschitz bound
  test2 ← ComputeLipschitz < 1
  ⎕ ← 'Test 2 - Lipschitz: ', (test2/'PASS'),(~test2)/'FAIL'

  ⍝ Test 3: Exponential weights sum
  test3 ← |1 - (+/ ExponentialWeights 1000)| < 0.01
  ⎕ ← 'Test 3 - Weights: ', (test3/'PASS'),(~test3)/'FAIL'

  ⍝ Test 4: Stability verification
  test4 ← VerifyStability
  ⎕ ← 'Test 4 - Stability: ', (test4/'PASS'),(~test4)/'FAIL'

  ⍝ Test 5: Semantic bounds
  field ← InitQuantumField
  test5 ← CheckSemanticBounds field
  ⎕ ← 'Test 5 - Semantic bounds: ', (test5/'PASS'),(~test5)/'FAIL'

  r ← test1 ∧ test2 ∧ test3 ∧ test4 ∧ test5
  ⎕ ← ''
  ⎕ ← 'Overall: ', (r/'ALL TESTS PASSED'),(~r)/'SOME TESTS FAILED'
∇

⍝ Export results as JSON for integration with Prolog
∇ json ← ExportQuantumState state
  json ← '{'
  json ,← '"position": ', (⍕1⊃state), ','
  json ,← '"velocity": ', (⍕2⊃state), ','
  json ,← '"acceleration": ', (⍕3⊃state), ','
  json ,← '"in_bounds": ', (⍕CheckSemanticBounds state), ','
  json ,← '"lipschitz": ', (⍕ComputeLipschitz)
  json ,← '}'
∇

⍝ Main entry point
⍝ Run this when loading the workspace
∇ Main
  Demo
  {}RunTests
∇
