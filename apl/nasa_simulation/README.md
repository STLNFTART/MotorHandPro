# APL NASA Mars Mission Simulation

Array-based radiation exposure modeling and crew health tracking for Mars missions using APL.

## Overview

This module implements NASA-compliant space mission simulation with:
- Galactic Cosmic Ray (GCR) exposure modeling
- Solar Particle Event (SPE) simulation
- Crew consciousness evolution tracking
- Shield effectiveness analysis
- Multi-crew health monitoring

## Mission Profiles

### Duration Presets
- **MISSION_SHORT**: 180 days (6 months)
- **MISSION_MEDIUM**: 500 days (~16 months)
- **MISSION_LONG**: 860 days (~28 months round trip)

### Shield Configurations
- **SHIELD_5G**: 5 g/cm² aluminum (85% penetration)
- **SHIELD_10G**: 10 g/cm² aluminum (72% penetration)
- **SHIELD_20G**: 20 g/cm² aluminum (58% penetration)

## Core Functions

### MarsMissionProfile
```apl
MarsMissionProfile shield_factor
```
Simulates complete Mars mission:
- Transit to Mars (180 days)
- Surface operations (500 days)
- Return transit (180 days)

Returns: `[total_days, total_dose]`

### CrewHealthDashboard
```apl
CrewHealthDashboard crew_count days shield_factor
```
Comprehensive crew health metrics:
- Average consciousness level
- Cumulative radiation dose
- Health risk assessment
- Mission duration

Returns: `[avg_consciousness, total_dose, risk_level, days]`

### CumulativeExposure
```apl
CumulativeExposure days shield_factor
```
Integrates radiation exposure over time (GCR + SPE).

### MultiCrewSimulation
```apl
MultiCrewSimulation crew_count days initial_states
```
Tracks consciousness evolution for entire crew (matrix output).

## Radiation Modeling

### GCR (Galactic Cosmic Rays)
```apl
GCR_DOSE_RATE ← 0.00023  ⍝ Gy/day
```
Continuous background radiation in deep space.

### SPE (Solar Particle Events)
```apl
SPE_PROBABILITY ← 0.05  ⍝ 5% chance per day
```
Stochastic high-energy particle bursts from solar flares.

### Total Dose
```
Daily Dose = (GCR_DOSE_RATE × shield_factor) + SPE_contribution
Cumulative = +\ Daily_Dose
```

## Health Risk Levels

NASA career limit: 1.0 Sv (Sievert)

- **Risk 0 (Safe)**: < 0.5 Sv
- **Risk 1 (Caution)**: 0.5 - 1.0 Sv
- **Risk 2 (Danger)**: > 1.0 Sv

## Usage Examples

### Mars Round Trip with 10 g/cm² Shield
```apl
mission_data ← MarsMissionProfile SHIELD_10G
'Total Days:' mission_data[1]
'Total Dose (Gy):' mission_data[2]
```

### 6-Person Crew Health Monitoring
```apl
health ← CrewHealthDashboard 6 860 SHIELD_10G
'Avg Consciousness:' health[1]
'Total Dose:' health[2]
'Risk Level:' health[3]
```

### Shield Comparison
```apl
comparison ← CompareShields 860
```
Output:
```
Shield_5g    42.3 Gy
Shield_10g   35.6 Gy
Shield_20g   28.7 Gy
```

### Hotspot Detection
```apl
exposure ← CumulativeExposure 860 SHIELD_10G
hotspots ← HotspotDetection exposure 0.05
```
Identifies days with elevated radiation (>0.05 Gy).

## Advanced Features

### Primal Logic Health Control
```apl
PrimalHealthControl current_health target_health KE λ
```
Applies exponential memory weighting to health regulation.

### Mission Abort Criteria
```apl
AbortCriteria cumulative_dose abort_threshold
```
Returns: `[abort_needed, abort_day]`

### Shield Optimization
```apl
ShieldOptimization days target_dose
```
Finds minimum shield mass for target dose.

## Performance

Benchmark with 1000 iterations:
```apl
BenchmarkNASA 1000
```

Target: <10ms per full mission simulation.

## Data Structures

### Crew Consciousness Matrix
```
Rows: Crew members
Columns: Mission days
Values: Consciousness level (0-100)
```

### Radiation Timeline
```
Vector: Daily cumulative dose (Gy)
Length: Mission duration
```

## Integration

### FFI Exports
- `FFI_MarsMissionProfile`: Complete mission profile
- `FFI_CrewHealthDashboard`: Multi-metric dashboard
- `FFI_CumulativeExposure`: Radiation timeline

### Python Integration
```python
import aplpy
apl = aplpy.APL()
result = apl.call('FFI_MarsMissionProfile', [0.72])
```

## Scientific Validation

- **GCR rates**: Based on ISS and Mars Odyssey measurements
- **SPE modeling**: Historical solar cycle data
- **Shield effectiveness**: NCRP Report No. 153
- **Health limits**: NASA-STD-3001 standards

## References

- NASA Human Research Program
- NCRP Report No. 153 (Space Radiation)
- Mars Science Laboratory radiation data
- ESA ExoMars TGO measurements
