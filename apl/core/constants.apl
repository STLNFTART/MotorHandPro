⍝ Primal Logic Universal Constants
⍝ These constants are fundamental to the MotorHandPro system

⍝ Lightfoot constant (λ) - Exponential decay rate
⍝ Time constant τ ≈ 5.92 seconds
LIGHTFOOT ← 0.16905

⍝ Donte constant (D) - Fixed-point attractor
DONTE ← 149.9992314000

⍝ I3 constant - Normalization/scaling factor
I3 ← 6.4939394023

⍝ Scaling constant (S) - Control/energy ratio
SCALING ← DONTE ÷ I3

⍝ Lipschitz constant - Stability guarantee (must be < 1)
LIPSCHITZ ← 0.000129931830

⍝ Semantic bounds - Valid operational range
SEMANTIC_LOWER ← ¯6.5
SEMANTIC_UPPER ← 6.5

⍝ Planck-scale constants
PLANCK_LENGTH ← 1.616255E¯35  ⍝ meters
PLANCK_TIME ← 5.391247E¯44    ⍝ seconds
PLANCK_ENERGY ← 1.956E9       ⍝ joules

⍝ Physical constants
SPEED_OF_LIGHT ← 299792458    ⍝ m/s
PLANCK_CONSTANT ← 6.62607015E¯34  ⍝ J⋅s
GRAVITATIONAL_CONSTANT ← 6.67430E¯11  ⍝ m³/(kg⋅s²)

⍝ Mars mission constants
MARS_DISTANCE_MIN ← 5.46E10   ⍝ meters (closest approach)
MARS_DISTANCE_MAX ← 4.01E11   ⍝ meters (furthest)
MARS_MISSION_DAYS_MIN ← 180
MARS_MISSION_DAYS_MAX ← 860

⍝ Radiation constants
SOLAR_PARTICLE_EVENT_GY_PER_DAY ← 0.002  ⍝ Gy/day during SPE
GALACTIC_COSMIC_RAY_GY_PER_DAY ← 0.0002  ⍝ Gy/day baseline
SHIELDING_MIN_G_CM2 ← 5
SHIELDING_MAX_G_CM2 ← 20

⍝ Display constants
∇ r ← DisplayConstants
  ⎕ ← '═══════════════════════════════════════════════════════'
  ⎕ ← '         PRIMAL LOGIC UNIVERSAL CONSTANTS'
  ⎕ ← '═══════════════════════════════════════════════════════'
  ⎕ ← ''
  ⎕ ← '🔷 Core Constants:'
  ⎕ ← '  Lightfoot (λ):    ', LIGHTFOOT, ' (τ ≈ 5.92s)'
  ⎕ ← '  Donte (D):        ', DONTE
  ⎕ ← '  I3:               ', I3
  ⎕ ← '  Scaling (S):      ', SCALING
  ⎕ ← '  Lipschitz:        ', LIPSCHITZ, ' (< 1 ✓)'
  ⎕ ← ''
  ⎕ ← '🔷 Semantic Bounds:'
  ⎕ ← '  Lower:            ', SEMANTIC_LOWER
  ⎕ ← '  Upper:            ', SEMANTIC_UPPER
  ⎕ ← ''
  ⎕ ← '🔷 Planck Scale:'
  ⎕ ← '  Length:           ', PLANCK_LENGTH, ' m'
  ⎕ ← '  Time:             ', PLANCK_TIME, ' s'
  ⎕ ← '  Energy:           ', PLANCK_ENERGY, ' J'
  ⎕ ← ''
  ⎕ ← '═══════════════════════════════════════════════════════'
  r ← 1
∇

⍝ Validate that constants satisfy stability conditions
∇ r ← ValidateConstants
  ⍝ Check Lipschitz condition for contractivity
  lipschitz_ok ← LIPSCHITZ < 1

  ⍝ Check Lightfoot decay rate is positive
  lightfoot_ok ← LIGHTFOOT > 0

  ⍝ Check Donte constant is in expected range
  donte_ok ← (DONTE > 149) ∧ (DONTE < 150)

  ⍝ Check I3 scaling factor
  i3_ok ← (I3 > 6) ∧ (I3 < 7)

  ⍝ Check semantic bounds are symmetric (approximately)
  bounds_ok ← |SEMANTIC_LOWER + SEMANTIC_UPPER| < 0.01

  ⍝ All checks must pass
  r ← lipschitz_ok ∧ lightfoot_ok ∧ donte_ok ∧ i3_ok ∧ bounds_ok

  :If r = 0
      ⎕ ← '❌ CONSTANT VALIDATION FAILED'
  :Else
      ⎕ ← '✅ All constants validated successfully'
  :EndIf
∇

⍝ Export constants as JSON for integration
∇ json ← ExportJSON
  json ← '{'
  json ,← '"lightfoot": ', (⍕LIGHTFOOT), ','
  json ,← '"donte": ', (⍕DONTE), ','
  json ,← '"i3": ', (⍕I3), ','
  json ,← '"scaling": ', (⍕SCALING), ','
  json ,← '"lipschitz": ', (⍕LIPSCHITZ), ','
  json ,← '"semantic_lower": ', (⍕SEMANTIC_LOWER), ','
  json ,← '"semantic_upper": ', (⍕SEMANTIC_UPPER)
  json ,← '}'
∇
