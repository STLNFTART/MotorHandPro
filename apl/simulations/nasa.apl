⍝ NASA Mars Mission Simulations
⍝ Crew health, radiation exposure, and mission planning

⍝═══════════════════════════════════════════════════════════
⍝ MISSION PARAMETERS
⍝═══════════════════════════════════════════════════════════

⍝ Mission duration scenarios (days)
MISSION_SHORT ← 180
MISSION_NOMINAL ← 520
MISSION_LONG ← 860

⍝ Radiation exposure rates (Gy/day)
GCR_RATE ← 0.0002      ⍝ Galactic Cosmic Rays (baseline)
SPE_RATE ← 0.002       ⍝ Solar Particle Events
SPE_PROBABILITY ← 0.05  ⍝ 5% chance of SPE per day

⍝ Shielding effectiveness (reduction factor)
SHIELDING_5_GCM2 ← 0.8   ⍝ 5 g/cm² reduces dose to 80%
SHIELDING_10_GCM2 ← 0.6  ⍝ 10 g/cm² reduces to 60%
SHIELDING_20_GCM2 ← 0.4  ⍝ 20 g/cm² reduces to 40%

⍝ Crew health parameters
CONSCIOUSNESS_BASELINE ← 0.95  ⍝ 95% baseline consciousness
CONSCIOUSNESS_DECAY ← 0.00001  ⍝ Daily decay rate
CONSCIOUSNESS_RADIATION_FACTOR ← 0.1  ⍝ Radiation impact

⍝═══════════════════════════════════════════════════════════
⍝ RADIATION MODELING
⍝═══════════════════════════════════════════════════════════

⍝ Simulate daily radiation exposure
⍝ Returns cumulative dose over mission duration
∇ dose ← days SimulateRadiation shielding
  ⍝ Generate SPE events (random)
  spe_events ← SPE_PROBABILITY > ? days⍴0

  ⍝ Daily dose: GCR baseline + SPE when events occur
  daily_dose ← GCR_RATE + SPE_RATE × spe_events

  ⍝ Apply shielding factor
  shield_factor ← shielding SelectShielding 0
  daily_dose ← daily_dose × shield_factor

  ⍝ Cumulative dose
  dose ← +\ daily_dose
∇

⍝ Select shielding factor based on g/cm²
∇ factor ← gcm2 SelectShielding dummy
  :Select gcm2
  :Case 5
      factor ← SHIELDING_5_GCM2
  :Case 10
      factor ← SHIELDING_10_GCM2
  :Case 20
      factor ← SHIELDING_20_GCM2
  :Else
      factor ← 1.0  ⍝ No shielding
  :EndSelect
∇

⍝ Compute total mission dose
∇ total ← days TotalDose shielding
  cumulative ← days SimulateRadiation shielding
  total ← ⊃⌽cumulative  ⍝ Last element
∇

⍝═══════════════════════════════════════════════════════════
⍝ CREW HEALTH MODELING
⍝═══════════════════════════════════════════════════════════

⍝ Simulate consciousness adaptation over time
⍝ Uses Primal Logic exponential decay
∇ consciousness ← days SimulateConsciousness radiation_dose
  ⍝ Base decay from isolation/confinement
  base_decay ← *(-CONSCIOUSNESS_DECAY) × ⍳days

  ⍝ Additional decay from radiation
  radiation_impact ← *(-CONSCIOUSNESS_RADIATION_FACTOR × radiation_dose)

  ⍝ Combined consciousness level
  consciousness ← CONSCIOUSNESS_BASELINE × base_decay × radiation_impact
∇

⍝ Simulate crew health metrics
⍝ Returns matrix: [day, radiation, consciousness, health_score]
∇ metrics ← days SimulateCrewHealth shielding
  ⍝ Radiation exposure
  radiation ← days SimulateRadiation shielding

  ⍝ Consciousness levels
  consciousness ← days SimulateConsciousness radiation

  ⍝ Overall health score (weighted average)
  health_score ← 0.7 × consciousness + 0.3 × (1 - MinMaxNorm radiation)

  ⍝ Combine into matrix
  metrics ← (⍳days),[1.5]radiation,[1.5]consciousness,[1.5]health_score
∇

⍝═══════════════════════════════════════════════════════════
⍝ MISSION ANALYSIS
⍝═══════════════════════════════════════════════════════════

⍝ Compare mission scenarios
⍝ Returns matrix with different duration/shielding combos
∇ results ← CompareMissions
  durations ← MISSION_SHORT MISSION_NOMINAL MISSION_LONG
  shieldings ← 5 10 20

  ⍝ Compute total dose for each combination
  results ← ∘.{⍺ TotalDose ⍵} ⍨ durations shieldings
∇

⍝ Find optimal shielding for mission duration
∇ optimal ← days OptimalShielding target_dose
  shieldings ← 5 10 15 20
  doses ← days∘.TotalDose shieldings

  ⍝ Find shielding that gets closest to target without exceeding
  valid ← shieldings × doses ≤ target_dose
  optimal ← ⌈/ valid
∇

⍝═══════════════════════════════════════════════════════════
⍝ PRIMAL LOGIC INTEGRATION
⍝═══════════════════════════════════════════════════════════

⍝ Apply Primal Logic stability to crew health
⍝ Ensures health metrics stay within bounds
∇ bounded ← BoundHealthMetrics metrics
  ⍝ Extract consciousness column
  consciousness ← metrics[;3]

  ⍝ Apply Primal Logic bounding
  lower ← 0.5  ⍝ Minimum consciousness threshold
  upper ← 1.0  ⍝ Maximum consciousness

  ⍝ Clip to bounds
  bounded_consciousness ← lower⌈upper⌊consciousness

  ⍝ Update matrix
  bounded ← metrics
  bounded[;3] ← bounded_consciousness
∇

⍝ Compute mission stability index using Lipschitz constant
∇ stability ← ComputeMissionStability metrics
  ⍝ Compute rate of change in health score
  health_scores ← metrics[;4]
  changes ← |1↓health_scores - ¯1↓health_scores

  ⍝ Maximum change rate (Lipschitz-like)
  max_change ← ⌈/ changes

  ⍝ Stability index: 1 if stable (change < threshold), 0 otherwise
  threshold ← LIPSCHITZ
  stability ← max_change < threshold
∇

⍝═══════════════════════════════════════════════════════════
⍝ VISUALIZATION DATA EXPORT
⍝═══════════════════════════════════════════════════════════

⍝ Export mission metrics as JSON
∇ json ← ExportMissionJSON metrics
  N ← ⊃⍴metrics

  json ← '{'
  json ,← '"days": [', (1↓∊',',¨⍕¨metrics[;1]), '],'
  json ,← '"radiation": [', (1↓∊',',¨⍕¨metrics[;2]), '],'
  json ,← '"consciousness": [', (1↓∊',',¨⍕¨metrics[;3]), '],'
  json ,← '"health_score": [', (1↓∊',',¨⍕¨metrics[;4]), ']'
  json ,← '}'
∇

⍝ Export comparison matrix as JSON
∇ json ← ExportComparisonJSON results
  json ← '{'
  json ,← '"durations": [', (⍕MISSION_SHORT), ',', (⍕MISSION_NOMINAL), ',', (⍕MISSION_LONG), '],'
  json ,← '"shieldings": [5, 10, 20],'
  json ,← '"doses": [', (1↓∊',',¨⍕¨,results), ']'
  json ,← '}'
∇

⍝═══════════════════════════════════════════════════════════
⍝ DEMONSTRATION
⍝═══════════════════════════════════════════════════════════

∇ Demo
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← '    NASA MARS MISSION SIMULATION'
  ⎕ ← '═══════════════════════════════════════════════'
  ⎕ ← ''

  ⍝ Simulate nominal mission
  days ← MISSION_NOMINAL
  shielding ← 10

  ⎕ ← '🔷 Mission parameters:'
  ⎕ ← '  Duration: ', days, ' days'
  ⎕ ← '  Shielding: ', shielding, ' g/cm²'
  ⎕ ← ''

  ⍝ Compute crew health
  metrics ← days SimulateCrewHealth shielding

  ⎕ ← '🔷 Mission metrics:'
  ⎕ ← '  Total radiation dose: ', (⊃⌽metrics[;2]), ' Gy'
  ⎕ ← '  Final consciousness: ', (⊃⌽metrics[;3])
  ⎕ ← '  Final health score: ', (⊃⌽metrics[;4])
  ⎕ ← ''

  ⍝ Stability check
  stable ← ComputeMissionStability metrics
  ⎕ ← '🔷 Mission stability: ', (stable/'STABLE ✓'),(~stable)/'UNSTABLE ✗'
  ⎕ ← ''

  ⍝ Compare scenarios
  ⎕ ← '🔷 Comparing mission scenarios:'
  comparison ← CompareMissions
  ⎕ ← '  Dose matrix (rows=duration, cols=shielding):'
  ⎕ ← comparison
  ⎕ ← ''

  ⍝ Export JSON
  json ← ExportMissionJSON 10↑metrics
  ⎕ ← '🔷 Sample JSON export (first 10 days):'
  ⎕ ← json
  ⎕ ← ''
∇

⍝ Run tests
∇ r ← RunTests
  ⎕ ← 'Running NASA simulation tests...'

  ⍝ Test 1: Radiation accumulates
  dose ← 100 SimulateRadiation 10
  test1 ← (⊃⌽dose) > (⊃dose)
  ⎕ ← 'Test 1 - Radiation accumulates: ', (test1/'PASS'),(~test1)/'FAIL'

  ⍝ Test 2: Shielding reduces dose
  dose_no_shield ← 100 TotalDose 0
  dose_shield ← 100 TotalDose 20
  test2 ← dose_shield < dose_no_shield
  ⎕ ← 'Test 2 - Shielding effective: ', (test2/'PASS'),(~test2)/'FAIL'

  ⍝ Test 3: Consciousness decays
  consciousness ← 100 SimulateConsciousness (100⍴0.001)
  test3 ← (⊃⌽consciousness) < (⊃consciousness)
  ⎕ ← 'Test 3 - Consciousness decays: ', (test3/'PASS'),(~test3)/'FAIL'

  r ← test1 ∧ test2 ∧ test3
  ⎕ ← ''
  ⎕ ← 'Overall: ', (r/'ALL TESTS PASSED'),(~r)/'SOME TESTS FAILED'
∇
