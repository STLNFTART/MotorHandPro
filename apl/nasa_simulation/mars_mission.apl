⍝ NASA Mars Mission Simulation - APL Implementation
⍝ Radiation exposure modeling and crew health tracking

⍝ Physical Constants
GCR_DOSE_RATE ← 0.00023  ⍝ Gy/day (Galactic Cosmic Rays)
SPE_PROBABILITY ← 0.05    ⍝ Solar Particle Event probability per day
CONSCIOUSNESS_DECAY ← 0.001  ⍝ Daily consciousness decay in space

⍝ Shield effectiveness (aluminum equivalent, g/cm²)
SHIELD_5G ← 0.85   ⍝ 5 g/cm² reduces dose by 15%
SHIELD_10G ← 0.72  ⍝ 10 g/cm² reduces dose by 28%
SHIELD_20G ← 0.58  ⍝ 20 g/cm² reduces dose by 42%

⍝ Mission Duration Presets (days)
MISSION_SHORT ← 180    ⍝ 6 months
MISSION_MEDIUM ← 500   ⍝ ~16 months
MISSION_LONG ← 860     ⍝ ~28 months (round trip)

⍝ Radiation Dose Calculation
⍝ Arguments: days, shield_factor
RadiationDose ← {
    days shield_factor ← ⍵
    GCR_DOSE_RATE × days × shield_factor
}

⍝ Solar Particle Event Simulation
⍝ Returns array of SPE events (1 = event, 0 = no event)
SPE_Simulation ← {
    days ← ⍵
    ? days ⍴ 2  ⍝ Random binary array
    events ← (? days ⍴ 0) < SPE_PROBABILITY
    events
}

⍝ SPE Dose Contribution
⍝ Each SPE adds 0.01-0.5 Gy depending on severity
SPE_Dose ← {
    events ← ⍵
    severity ← 0.01 + (? (+/events) ⍴ 0) × 0.49
    events × severity
}

⍝ Crew Consciousness Evolution
⍝ ψ(t) = ψ0 × exp(-decay × t)
CrewConsciousness ← {
    ψ0 days decay ← ⍵
    ψ0 × *(-decay × ⍳days)
}

⍝ Multi-Crew Tracking
⍝ Matrix: rows = crew members, columns = days
MultiCrewSimulation ← {
    crew_count days initial_states ← ⍵
    decay_rates ← CONSCIOUSNESS_DECAY + (? crew_count ⍴ 0) × 0.0005
    consciousness_matrix ← initial_states ∘.× *(-decay_rates ∘.× ⍳days)
    consciousness_matrix
}

⍝ Cumulative Radiation Exposure
⍝ Integrates GCR + SPE over mission duration
CumulativeExposure ← {
    days shield_factor ← ⍵
    gcr_daily ← GCR_DOSE_RATE × shield_factor
    spe_events ← SPE_Simulation days
    spe_daily ← SPE_Dose spe_events
    daily_dose ← gcr_daily + spe_daily
    cumulative ← +\ daily_dose
    cumulative
}

⍝ Health Risk Assessment
⍝ NASA limit: 1.0 Sv (Sievert) for career
⍝ Returns risk level: 0 (safe), 1 (caution), 2 (danger)
HealthRisk ← {
    cumulative_dose ← ⍵
    final_dose ← ⊃⌽ cumulative_dose
    risk ← (final_dose > 0.5) + (final_dose > 1.0)
    risk
}

⍝ Shield Optimization
⍝ Find minimum shield mass for safe mission
ShieldOptimization ← {
    days target_dose ← ⍵
    shields ← 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
    doses ← RadiationDose ∘(days ,) ¨ shields
    safe_shields ← shields / (doses ≤ target_dose)
    optimal ← ⊃safe_shields
    optimal
}

⍝ Mars Surface Radiation
⍝ Mars atmosphere provides ~20 g/cm² shielding
MarsRadiation ← {
    days ← ⍵
    mars_shield ← 0.65  ⍝ Martian atmosphere effect
    RadiationDose days mars_shield
}

⍝ Complete Mars Mission Profile
⍝ Transit1 (180d) + Surface (500d) + Transit2 (180d)
MarsMissionProfile ← {
    shield_factor ← ⍵

    ⍝ Transit to Mars
    transit1_days ← 180
    transit1_dose ← CumulativeExposure transit1_days shield_factor

    ⍝ Mars surface
    surface_days ← 500
    surface_dose ← MarsRadiation surface_days

    ⍝ Return transit
    transit2_days ← 180
    transit2_dose ← CumulativeExposure transit2_days shield_factor

    ⍝ Combine phases
    total_days ← transit1_days + surface_days + transit2_days
    total_dose ← (⊃⌽ transit1_dose) + surface_dose + (⊃⌽ transit2_dose)

    total_days, total_dose
}

⍝ Crew Health Metrics Dashboard
⍝ Returns: [avg_consciousness, total_dose, risk_level, mission_days]
CrewHealthDashboard ← {
    crew_count days shield_factor ← ⍵

    ⍝ Consciousness tracking
    initial_states ← crew_count ⍴ 100.0
    consciousness ← MultiCrewSimulation crew_count days initial_states
    avg_consciousness ← (+⌿ consciousness) ÷ crew_count

    ⍝ Radiation exposure
    exposure ← CumulativeExposure days shield_factor
    total_dose ← ⊃⌽ exposure

    ⍝ Risk assessment
    risk ← HealthRisk exposure

    avg_consciousness, total_dose, risk, days
}

⍝ Time-Series Analysis
⍝ Compute statistics over mission timeline
TimeSeriesStats ← {
    data ← ⍵
    mean_val ← (+/ data) ÷ ⍴ data
    max_val ← ⌈/ data
    min_val ← ⌊/ data
    std_dev ← ((+/ (data - mean_val) * 2) ÷ ⍴ data) * 0.5
    mean_val, max_val, min_val, std_dev
}

⍝ Compare Shield Configurations
⍝ Test multiple shield densities
CompareShields ← {
    days ← ⍵
    shields ← SHIELD_5G, SHIELD_10G, SHIELD_20G
    doses ← RadiationDose ∘(days ,) ¨ shields
    shield_names ← 'Shield_5g' 'Shield_10g' 'Shield_20g'
    ⍪ shield_names,[0.5] doses
}

⍝ Benchmark NASA Simulation
BenchmarkNASA ← {
    iterations ← ⍵
    crew_count ← 6
    days ← 860
    shield_factor ← SHIELD_10G

    start_time ← ⎕AI[3]
    :For i :In ⍳iterations
        result ← CrewHealthDashboard crew_count days shield_factor
    :EndFor
    end_time ← ⎕AI[3]

    elapsed_ms ← end_time - start_time
    throughput ← iterations ÷ (elapsed_ms ÷ 1000)
    elapsed_ms, throughput
}

⍝ Radiation Hotspot Detection
⍝ Identify periods of elevated exposure
HotspotDetection ← {
    exposure_array threshold ← ⍵
    daily_dose ← 1 ⌽ exposure_array - ¯1 ⌽ exposure_array
    hotspots ← daily_dose > threshold
    hotspot_indices ← hotspots / ⍳⍴ hotspots
    hotspot_indices
}

⍝ Mission Abort Criteria
⍝ Check if mission should abort due to radiation
AbortCriteria ← {
    cumulative_dose abort_threshold ← ⍵
    abort_day ← ⊃ (cumulative_dose > abort_threshold) / ⍳⍴ cumulative_dose
    abort_needed ← abort_day > 0
    abort_needed, abort_day
}

⍝ Primal Logic Integration
⍝ Use exponential memory weighting for health regulation
PrimalHealthControl ← {
    current_health target_health KE λ ← ⍵
    error ← target_health - current_health
    dψdt ← (-λ × current_health) + (KE × error)
    dψdt
}

⍝ Export for FFI
∇ Z ← FFI_MarsMissionProfile params
  Z ← MarsMissionProfile params
∇

∇ Z ← FFI_CrewHealthDashboard params
  Z ← CrewHealthDashboard params
∇

∇ Z ← FFI_CumulativeExposure params
  Z ← CumulativeExposure params
∇

⍝ Example Usage:
⍝ mars_mission ← MarsMissionProfile SHIELD_10G
⍝ crew_health ← CrewHealthDashboard 6 860 SHIELD_10G
⍝ shields_comparison ← CompareShields 860
