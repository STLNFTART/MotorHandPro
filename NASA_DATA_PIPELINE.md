# NASA Data Pipeline - 3I/ATLAS Integration

## Overview

This pipeline integrates live NASA data for comet 3I/ATLAS (C/2025 N1) with the MotorHandPro Primal Logic framework. It fetches real-time astronomical data, processes it through the Recursive Planck Operator, and generates interactive visualizations.

**Author:** Donte Lightfoot
**Date:** December 4, 2025
**Repository:** MotorHandPro

---

## Features

### 🌌 Data Sources

- **JPL Horizons API** - High-precision ephemerides and orbital elements
- **Minor Planet Center (MPC)** - Astrometry updates from IAWN campaign
- **NASA/ESA Archives** - Historical observations
- **Simulated Feed** - Fallback for testing/demo

### 🔬 Processing Framework

**Recursive Planck Operator:**
```
dn/dt = -μ n + β ∫ α e^{-α τ} n(t-τ) dτ + S(t)
```

Where:
- `μ = 0.16905` (Lightfoot constant - damping)
- `S(t)` = Signal from comet data (Ni flux, position changes)
- Non-Markovian memory integral for temporal patterns
- Bounded to `[-D, D]` where `D = 149.9992314` AU

**Features:**
- Real-time anomaly detection
- Non-linear signal processing
- Memory-based prediction
- Bounded stability guarantees

### 🎨 Enhanced Visualizations

**Plotly Interactive Dashboards:**
- 3D sky charts with comet trajectory
- Real-time animations
- Multi-panel primal logic analysis
- Interactive controls (zoom, rotate, pan)
- Publication-quality output

**Visualization Types:**
1. **Interactive Sky Chart** - 3D trajectory with Earth/Sun reference
2. **Primal Logic Dashboard** - 6-panel analysis (signal, state, error, memory, anomaly, phase space)
3. **Real-Time Animation** - Time-series motion with playback controls

### 🔗 LAM Framework Integration

Optional integration with MotorHandPro's LAM temporal displacement:
- Time-warp field evaluation: `E(t) = α * E0(t - Δ(t)) - λ * D(t)`
- Memory kernel with exponential decay
- Delay differential equation solver
- Retarded/advanced temporal evaluation

---

## Installation

### Quick Setup

```bash
# Run automated setup
chmod +x setup_nasa_pipeline.sh
./setup_nasa_pipeline.sh
```

### Manual Setup

```bash
# Install dependencies
pip install numpy scipy requests matplotlib plotly kaleido

# Make scripts executable
chmod +x live_nasa_pipeline.py

# Create output directories
mkdir -p nasa_live_output/visualizations
```

---

## Usage

### Single Run

Fetch data, process, and visualize once:

```bash
python3 live_nasa_pipeline.py --mode single
```

Output:
- `nasa_live_output/observations_YYYYMMDD_HHMMSS.json` - Raw observations
- `nasa_live_output/states_YYYYMMDD_HHMMSS.json` - Processed states
- `nasa_live_output/visualizations/interactive_sky_chart.html` - 3D chart
- `nasa_live_output/visualizations/primal_logic_dashboard.html` - Dashboard
- `nasa_live_output/visualizations/comet_animation.html` - Animation

### Continuous Mode

Run pipeline continuously with periodic updates:

```bash
# Update every hour (3600 seconds)
python3 live_nasa_pipeline.py --mode continuous --interval 3600

# Run for 10 iterations then stop
python3 live_nasa_pipeline.py --mode continuous --max-iterations 10

# Custom output directory
python3 live_nasa_pipeline.py --mode continuous --output-dir my_output
```

### Automated Cron Job

Schedule periodic updates:

```bash
# Edit crontab
crontab -e

# Add entry (runs every 6 hours)
0 */6 * * * cd /path/to/MotorHandPro && python3 live_nasa_pipeline.py --mode single >> nasa_pipeline.log 2>&1
```

---

## Architecture

### Data Flow

```
NASA APIs → Fetch Data → Parse Observations → Recursive Planck Operator
                                                        ↓
                                                 Process States
                                                        ↓
                                    ┌───────────────────┴───────────────────┐
                                    ↓                                       ↓
                            LAM Integration                        Visualizations
                           (Time Warp Field)                    (Plotly Dashboards)
                                    ↓                                       ↓
                              Save States                            Save HTML Files
```

### Modules

1. **`nasa_comet_data.py`** - Data fetching and Recursive Planck Operator
   - `NASACometDataClient` - API client for NASA sources
   - `RecursivePlanckOperator` - Primal logic processing
   - `CometObservation` - Data structures

2. **`nasa_data_visualization.py`** - Enhanced visualizations
   - `NASADataVisualizer` - Plotly visualization generator
   - Interactive 3D charts
   - Multi-panel dashboards
   - Animations

3. **`live_nasa_pipeline.py`** - Orchestration and automation
   - `LiveNASAPipeline` - Pipeline controller
   - Continuous operation
   - Data persistence
   - LAM integration

---

## Data Structures

### CometObservation

```python
@dataclass
class CometObservation:
    timestamp: datetime
    ra: float                    # Right Ascension (degrees)
    dec: float                   # Declination (degrees)
    distance_au: float           # Distance from Earth (AU)
    velocity_km_s: float         # Radial velocity (km/s)
    magnitude: float             # Visual magnitude
    elongation: float            # Solar elongation (degrees)
    phase_angle: float           # Phase angle (degrees)
    tail_length_km: float        # Tail extension (km)
    gas_production_rate: float   # Gas flux (g/s or molecules/s)
    dust_production_rate: float  # Dust flux
    coma_diameter_km: float      # Coma size (km)
    source: str                  # Data source
    quality_flag: str            # Quality indicator
```

### RecursivePlanckState

```python
@dataclass
class RecursivePlanckState:
    n: float                     # State variable
    signal: float                # S(t) from comet data
    memory_integral: float       # ∫ K(τ) n(τ) dτ
    error: float                 # G_PL(t) - anomaly metric
    mu: float = 0.16905         # Damping constant
    D: float = 149.9992314      # Bound (AU scale)
```

---

## Integration with Repo Experiments

### Quantro Heart

Pipe Ni flux as "cardiac pulse" signal:

```python
from quantro_heart import FHNModel

fhn = FHNModel(alpha=0.54, lambda_val=0.115)
fhn.input_signal = latest_observation.gas_production_rate
fhn.run_simulation(steps=1000)
```

### MotorHandPro Actuator Control

Use RA/Dec for actuator path planning:

```python
from lam.temporal_displacement import TemporalDisplacement

td = TemporalDisplacement(method='kernel')
td.input_data = (obs.ra, obs.dec)
td.run_displacement(mu=0.16905)
```

### Primal Simulations

Swarm tracking with IAWN centroids:

```python
from primal_swarm import SwarmSim

swarm = SwarmSim(n_agents=100)
swarm.positions = [(obs.ra, obs.dec)] * 100
swarm.update(mu=0.16905, valence=obs.gas_production_rate)
```

---

## Visualization Examples

### Interactive Sky Chart

Features:
- 3D trajectory in equatorial coordinates
- Earth and Sun reference points
- Color-coded by magnitude
- Hover tooltips with full data
- Rotation, zoom, pan controls

### Primal Logic Dashboard

Six-panel analysis:
1. **Signal S(t)** - Ni flux over time
2. **State n(t)** - Anomaly metric with bounds
3. **Error γ(t)** - Deviation from expected
4. **Memory ∫K(τ)n(τ)dτ** - Temporal memory integral
5. **Anomaly Score** - Normalized 0-1 score
6. **Phase Space** - n vs S trajectory

### Animation

Time-series animation with:
- Playback controls (play/pause)
- Time slider
- Real-time position marker
- Trajectory trail

---

## Configuration

### Environment Variables

```bash
# Optional: NASA API keys (if required in future)
export NASA_API_KEY="your_key_here"

# Optional: Space-Track credentials (for satellite data)
export SPACETRACK_USER="username"
export SPACETRACK_PASS="password"
```

### Pipeline Parameters

```python
pipeline = LiveNASAPipeline(
    output_dir='nasa_live_output',
    update_interval_seconds=3600,  # 1 hour
    enable_lam_integration=True
)
```

### Recursive Planck Operator

```python
operator = RecursivePlanckOperator(
    mu=0.16905,       # Lightfoot constant
    alpha=1.618,      # Memory decay (golden ratio)
    beta=0.5,         # Memory coupling
    D=149.9992314     # Bound (AU)
)
```

---

## Troubleshooting

### No Live Data Available

**Symptom:** Pipeline falls back to simulated data

**Causes:**
- NASA APIs temporarily unavailable
- Network connectivity issues
- Rate limiting

**Solution:**
- Wait and retry (APIs update periodically)
- Check network connection
- Use simulated mode for testing: `--mode single`

### Import Errors

**Symptom:** `ModuleNotFoundError`

**Solution:**
```bash
pip install -r requirements.txt
pip install plotly kaleido
```

### Visualization Not Generating

**Symptom:** No HTML files in output

**Solution:**
- Check Plotly installation: `pip install plotly`
- Verify output directory exists
- Check disk space

---

## Performance

### Resource Usage

- **Memory:** ~100-500 MB (depends on data volume)
- **CPU:** Minimal (mostly I/O bound)
- **Disk:** ~1-10 MB per iteration
- **Network:** ~100-500 KB per API call

### Scaling

- Single run: ~5-30 seconds
- Continuous mode: 1 hour intervals recommended
- Can handle 1000+ observations per iteration

---

## Future Enhancements

### Planned Features

1. **Advanced Visualizations**
   - PyVista 3D mesh for comet coma
   - Vispy GPU acceleration for real-time streaming
   - WebGL-based dashboards

2. **Additional Data Sources**
   - Hubble/JWST spectroscopy
   - Amateur astronomy networks
   - Radio telescope data

3. **Machine Learning**
   - Anomaly detection with neural networks
   - Orbit prediction refinement
   - Gas production forecasting

4. **Multi-Object Support**
   - Track multiple comets/asteroids
   - Comparative analysis
   - Collision risk assessment

---

## References

### NASA Data Sources

- [JPL Horizons API](https://ssd.jpl.nasa.gov/horizons/)
- [Minor Planet Center](https://www.minorplanetcenter.net/)
- [JPL Small-Body Database](https://ssd.jpl.nasa.gov/tools/sbdb_lookup.html)
- [TheSkyLive](https://theskylive.com/)

### Scientific Background

- **3I/ATLAS (C/2025 N1)**: First interstellar comet discovered in 2025
- **IAWN Campaign**: International Asteroid Warning Network observation campaign (Nov 27, 2025 - Jan 27, 2026)
- **Closest Approach**: December 19, 2025 at 1.8 AU
- **Significance**: Potential interstellar origin, unique composition

### Primal Logic Framework

- [PRIMAL_LOGIC_FRAMEWORK.md](./PRIMAL_LOGIC_FRAMEWORK.md)
- [LAM_IMPLEMENTATION.md](./LAM_IMPLEMENTATION.md)
- [TEMPORAL_DISPLACEMENT.md](./lam/TEMPORAL_DISPLACEMENT.md)

---

## Citation

If you use this pipeline in research:

```bibtex
@software{lightfoot2025_nasa_pipeline,
  author = {Lightfoot, Donte},
  title = {NASA Data Pipeline for MotorHandPro Primal Logic Framework},
  year = {2025},
  url = {https://github.com/STLNFTART/MotorHandPro},
  note = {3I/ATLAS comet integration with Recursive Planck Operator}
}
```

---

## License

See [LICENSE](./LICENSE) for details.

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

---

## Contact

**Author:** Donte Lightfoot (STLNFTART)
**Email:** contact@stlnftart.com
**Repository:** https://github.com/STLNFTART/MotorHandPro

---

*Last Updated: December 4, 2025*
