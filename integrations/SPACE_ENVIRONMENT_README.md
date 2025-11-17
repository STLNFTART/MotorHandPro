# Space Environment Effects Module

## Overview

The `space_environment_effects.py` module adds realistic space environment physics to the MotorHandPro satellite constellation system, including:

- **Van Allen Radiation Belt modeling** with proton/electron flux calculations
- **Electromagnetic Pulse (EMP) weapon effects** simulation
- **Cumulative radiation damage** tracking for satellite components
- **Magnetic field interaction** using Earth's dipole model
- **Solar particle event** simulation

## Features

### 1. Van Allen Radiation Belt Physics

The module implements a physically accurate model of Earth's radiation belts:

```
Altitude Regions:
├── LEO Safe Zone:       0 - 1,000 km      (minimal radiation)
├── Inner Belt:      1,000 - 6,000 km      (proton radiation, ~10 Sv/day peak)
├── Slot Region:     6,000 - 13,000 km     (low radiation zone)
├── Outer Belt:     13,000 - 60,000 km     (electron radiation, ~5 Sv/day peak)
└── GEO+:           60,000+ km             (background cosmic rays)
```

**Key Physics:**
- Gaussian intensity profiles centered at belt peaks
- Latitude-dependent radiation (concentrated at magnetic equator)
- Proton flux in inner belt: 10⁸-10⁹ particles/cm²/s
- Electron flux in outer belt: 10⁹+ particles/cm²/s
- Geomagnetic activity scaling (Kp index)

### 2. EMP Weapon System

Simulates electromagnetic pulse effects on satellites:

**EMP Sources:**
- Nuclear EMP (high-altitude detonation)
- HERF (High Energy Radio Frequency) weapons
- Solar flare-induced EMP
- Cyber-triggered EMP

**Damage Model:**
- Range-dependent intensity (inverse square law)
- Shielding effectiveness (0.0 - 1.0)
- Electronics health degradation
- Recovery time calculation
- Temporary system failures

### 3. Satellite Health Tracking

Each satellite maintains health metrics affected by space environment:

```python
SatelliteHealth:
├── health: Overall health (0.0 - 1.0)
├── radiation_shielding: Protection level (0.0 - 1.0)
├── radiation_hardening: Radiation-resistant design (0.0 - 1.0)
├── emp_shielding: EMP protection (0.0 - 1.0)
├── cumulative_radiation_dose: Total accumulated dose (Sv)
├── solar_panel_efficiency: Degrades with radiation
└── electronics_health: Affected by radiation and EMP
```

**Damage Mechanisms:**
- Solar panel degradation: 1% per 1000 Sv accumulated dose
- Electronics degradation: Scaled by radiation hardening
- Health reduction: 0.005% per Sv of effective dose
- EMP damage: 50% electronics impact, 30% overall health impact

### 4. Magnetic Field Model

Dipole magnetic field using:
```
B = B₀(R_E/r)³√(1 + 3sin²λ)
```

Where:
- B₀ = 31,000 nT (surface field strength)
- R_E = 6,378.137 km (WGS84 equatorial radius)
- r = distance from Earth center
- λ = magnetic latitude

## Usage

### Basic Usage

```python
from space_environment_effects import SpaceEnvironmentSimulator
import numpy as np

# Create simulator
sim = SpaceEnvironmentSimulator()

# Initialize satellite with shielding parameters
sim.initialize_satellite(
    satellite_id=1,
    radiation_shielding=0.6,  # 60% protection
    emp_shielding=0.5,        # 50% protection
    radiation_hardening=0.7   # 70% hardened electronics
)

# Update satellite with position (ECI frame)
position_eci = np.array([0, 0, 9871])  # 3500 km altitude
result = sim.update_satellite(
    satellite_id=1,
    position_eci=position_eci,
    latitude=0.0,
    altitude=3500.0  # km
)

print(f"Radiation dose rate: {result['radiation']['dose_rate']} Sv/day")
print(f"Health: {result['health']['health']}")
print(f"Region: {result['radiation']['region']}")
```

### Creating EMP Events

```python
# Create nuclear EMP at specific position
emp_position = np.array([100, 200, 7871])  # 1500 km altitude
emp_event = sim.emp_system.create_emp_event(
    position=emp_position,
    source_type="NUCLEAR",
    intensity=0.9  # 90% intensity
)

# EMP will automatically affect satellites within range
# Next update_satellite() call will apply damage
```

### Batch Processing

```python
# Process multiple satellites efficiently
satellites_data = [
    {
        'satellite_id': i,
        'position_eci': np.array([x, y, z]),
        'latitude': lat,
        'altitude': alt
    }
    for i, (x, y, z, lat, alt) in enumerate(satellite_positions)
]

results = sim.update_all_satellites(satellites_data)

# Get system statistics
stats = sim.get_statistics()
print(f"Average radiation dose: {stats['avg_radiation_dose']} Sv")
print(f"EMP affected satellites: {stats['emp_affected_count']}")
```

### Solar Particle Events

```python
# Simulate solar flare / coronal mass ejection
spe = sim.radiation_belt.simulate_solar_particle_event(
    intensity=3.0,  # Major storm
    duration_hours=48.0
)

# Radiation levels will be elevated for the duration
print(f"Geomagnetic Kp index: {spe['geomagnetic_kp']}")
```

## Integration with Existing System

To integrate with `satellite_orbital_mechanics.py`:

```python
from satellite_orbital_mechanics import ConstellationTracker
from space_environment_effects import SpaceEnvironmentSimulator

# Create both systems
tracker = ConstellationTracker(orbital_elements)
env_sim = SpaceEnvironmentSimulator()

# Initialize satellites in environment simulator
for sat_id in range(num_satellites):
    env_sim.initialize_satellite(
        sat_id,
        radiation_shielding=0.6,
        emp_shielding=0.5,
        radiation_hardening=0.7
    )

# Update loop
current_time = datetime.utcnow()
states = tracker.propagate_all(current_time)

# Apply space environment effects
env_results = []
for state in states:
    result = env_sim.update_satellite(
        satellite_id=state.satellite_id,
        position_eci=state.position_eci,
        latitude=state.latitude,
        altitude=state.altitude
    )
    env_results.append(result)
```

## Performance

**Benchmarks** (on standard CPU):
- Initialization: 28,000+ satellites/second
- Update (radiation + EMP): 31,000+ satellites/second
- Memory: ~500 bytes per satellite health record

**Optimizations:**
- Vectorized numpy operations
- Minimal object creation
- Efficient batch processing
- Optional EMP event cleanup

## Testing

Run the comprehensive test suite:

```bash
cd integrations/
python test_space_environment.py
```

**Test Coverage:**
- ✓ Radiation belt region classification
- ✓ Radiation intensity calculations
- ✓ Magnetic field strength
- ✓ Cumulative radiation damage
- ✓ EMP weapon effects
- ✓ EMP recovery mechanics
- ✓ Integrated simulation
- ✓ Performance benchmarks

## Scientific Accuracy

### Van Allen Belt Model

Based on:
- NASA Van Allen Probes mission data (2012-2019)
- Trapped proton/electron flux measurements
- Magnetic field dipole approximation (valid for L < 3)

**Simplifications:**
- Gaussian profiles (actual belts have complex structures)
- No L-shell drift modeling
- Static belt configuration (actual belts vary with solar activity)
- Simplified magnetic field (no multipole terms)

**Typical Real-World Values:**
- Inner belt peak: ~10 Sv/day unshielded
- Outer belt peak: ~5 Sv/day unshielded
- Slot region: ~0.1-0.2 Sv/day
- ISS altitude (400 km): < 0.001 Sv/day

### EMP Effects

Based on:
- High-altitude nuclear EMP (HEMP) physics
- IEC 61000 standards for electromagnetic compatibility
- Military specifications for EMP hardening

**Damage Ranges:**
- Nuclear EMP: 500-2000 km effective range
- HERF weapons: 10-100 km range
- Solar flare EMP: Global scale

## Limitations

1. **Radiation Model:**
   - No solar cycle variations
   - No magnetospheric substorms
   - Simplified magnetic field geometry
   - No radiation belt "horns" at high L-shells

2. **EMP Model:**
   - Simplified damage calculation
   - No frequency-dependent coupling
   - No spacecraft geometry effects
   - Instant pulse (no waveform)

3. **Component Degradation:**
   - Linear damage accumulation
   - No annealing effects
   - No single-event upsets (SEUs)
   - No total ionizing dose (TID) vs displacement damage distinction

## Future Enhancements

Potential improvements:
- [ ] Solar cycle radiation variation
- [ ] Real-time space weather data integration (NOAA SWPC API)
- [ ] South Atlantic Anomaly (SAA) modeling
- [ ] Single-event effect (SEE) simulation
- [ ] Component-level damage tracking
- [ ] Radiation shielding material properties
- [ ] Multi-frequency EMP spectrum
- [ ] L-shell coordinate system
- [ ] Magnetopause/magnetotail dynamics

## References

1. NASA Van Allen Probes Mission: https://vanallenprobes.jhuapl.edu/
2. NOAA Space Weather Prediction Center: https://www.swpc.noaa.gov/
3. European Space Agency Radiation Belt Models: https://www.esa.int/
4. "Space Environment and Its Effects on Spacecraft" - ESA Publication
5. "The Van Allen Probes Mission" - Mauk et al., Space Science Reviews (2013)

## License

MIT License - See main repository LICENSE file

## Authors

MotorHandPro Integration Team
- Patent: U.S. Provisional 63/842,846
- Primal Logic Framework for Bounded Autonomous Control

## Contact

For questions or contributions, please refer to the main MotorHandPro repository.

---

**Note:** This module adds value to satellite constellation simulations by modeling realistic space environment hazards. It complements the existing `satellite_orbital_mechanics.py` orbital propagation system with physics-based damage and degradation modeling.
