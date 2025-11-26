# Unified Experiment Framework

**Standardized parameter sweep and results management for all MotorHandPro simulations**

## Directory Structure

```
experiments/
├── framework.py                 # Core framework (ParamGrid, RunLogger, run_parameter_sweep)
├── configs/                     # Parameter sweep configurations
│   ├── primal_swarm_sweep_*.json
│   └── mars_mission_sweep_*.json
├── runs/                        # Experiment outputs
│   ├── <sim_name>/
│   │   └── YYYYMMDD_HHMMSS_<tag>/
│   │       ├── raw/             # Time-series CSVs (sim_0000.csv, sim_0001.csv, ...)
│   │       ├── summary/         # Aggregated results (summary.csv, stats.json)
│   │       ├── plots/           # Visualizations (optional)
│   │       └── REPORT.md        # Human-readable summary
├── sweep_primal_swarm.py        # Swarm simulation wrapper
└── sweep_mars_mission.py        # Mars mission wrapper

```

## Quick Start

### Run a Parameter Sweep

```bash
# Primal swarm simulation
python3 experiments/sweep_primal_swarm.py

# Mars mission simulation
python3 experiments/sweep_mars_mission.py
```

### View Results

After running a sweep, results are saved to:
```
experiments/runs/<sim_name>/YYYYMMDD_HHMMSS_<tag>/
```

**Files generated:**
- `REPORT.md` - Human-readable summary with meta info
- `summary/summary.csv` - One row per configuration tested
- `summary/stats.json` - Aggregated statistics (min, max, mean, median)
- `raw/sim_NNNN.csv` - Time-series data for each configuration

### Example Output

```bash
$ python3 experiments/sweep_primal_swarm.py

✓ Sweep complete! Results in: experiments/runs/primal_swarm/20251126_202612_full_sweep

  View summary: experiments/runs/primal_swarm/20251126_202612_full_sweep/summary/summary.csv
  View stats:   experiments/runs/primal_swarm/20251126_202612_full_sweep/summary/stats.json
  View report:  experiments/runs/primal_swarm/20251126_202612_full_sweep/REPORT.md
```

## Framework API

### Creating a New Simulation Wrapper

```python
from experiments.framework import ParamGrid, run_parameter_sweep

def simulate_fn(config):
    """
    Your simulation function

    Args:
        config: dict of parameters (e.g., {"n_drones": 50, "lambda": 0.089})

    Returns:
        (metrics, time_series) tuple:
        - metrics: dict of scalar results (e.g., {"stable": True, "lipschitz": 0.63})
        - time_series: dict of arrays (e.g., {"t": [...], "x": [...], "u": [...]})
    """
    # Run your simulation with config parameters
    # ...

    metrics = {
        "some_metric": 0.85,
        "stable": True
    }

    time_series = {
        "t": [0, 1, 2, ...],
        "x": [1.0, 1.1, 1.2, ...],
        "y": [0.0, 0.1, 0.2, ...]
    }

    return metrics, time_series

# Define parameter grid
grid = ParamGrid({
    "param1": [1, 2, 3],
    "param2": [0.1, 0.5, 1.0]
})

# Run sweep
output_dir = run_parameter_sweep(
    sim_name="my_simulation",
    param_grid=grid,
    simulate_fn=simulate_fn,
    tag="test_sweep"
)

print(f"Results in: {output_dir}")
```

## Available Simulations

### 1. Primal Swarm (`sweep_primal_swarm.py`)

Tests UAV swarm coordination with trust dynamics.

**Parameters:**
- `n_drones`: Number of drones (10-200)
- `duration_s`: Simulation duration
- `dt`: Time step
- `lambda_val`: Trust decay rate
- `tau_val`: Minimum trust level

**Metrics:**
- `avg_neighbors`: Average neighbor count
- `final_avg_trust`: Final trust level
- `stable`: Swarm stability (bool)

### 2. Mars Mission (`sweep_mars_mission.py`)

Tests consciousness adaptation during Mars missions with radiation exposure.

**Parameters:**
- `mission_days`: Duration (180-540 days)
- `consciousness_target`: Target consciousness level (0.5-0.9)
- `learning_rate`: Adaptation speed
- `spe_intensity`: Solar particle event intensity
- `shield_thickness_gcm2`: Radiation shielding

**Metrics:**
- `total_dose_msv`: Total radiation dose
- `final_consciousness`: Final consciousness level
- `mission_success`: Mission success criteria (bool)
- `stable`: Consciousness convergence (bool)

## Configuration Files

JSON config files in `experiments/configs/` define parameter grids:

```json
{
  "description": "Quick test sweep",
  "sim_name": "primal_swarm",
  "tag": "quick_test",
  "params": {
    "n_drones": [10, 25],
    "lambda_val": [0.0893, 0.15]
  }
}
```

## Web Dashboard Integration

Experiment results are automatically accessible via the production web interface at:
```
https://www.primaltechinvest.com/experiments/
```

The dashboard provides:
- Browse all experiment runs
- View summary statistics
- Plot time-series data
- Download raw CSVs
- Compare configurations

## Best Practices

1. **Use descriptive tags** - `tag="full_sweep"` instead of `tag="test1"`
2. **Start small** - Test with quick configs before full sweeps
3. **Document parameters** - Add notes in JSON config files
4. **Check stability** - Look for `stable=True` in results
5. **Archive old runs** - Move `experiments/runs/` to backup after major milestones

## Performance

- **Primal Swarm**: ~6-7 configs/second
- **Mars Mission**: ~60 configs/second (simplified physics)

For 100+ configurations, expect:
- Swarm: ~15-20 seconds
- Mars: ~2-3 seconds

## Extending the Framework

To add a new simulation:

1. Create `experiments/sweep_<sim_name>.py`
2. Write `simulate_fn(config)` that returns `(metrics, time_series)`
3. Define parameter grid
4. Call `run_parameter_sweep()`
5. Add config JSONs to `experiments/configs/`

## Repository Integration

This framework standardizes experiments across all repos:
- MotorHandPro (this repo)
- Multi-Heart-Model
- UAV simulations
- Van Allen belt models
- Optimus Prime integration
- Stealth vehicle models
- Mars mission planning

**Same folder structure everywhere:**
```
experiments/
  configs/
  runs/
  framework.py
  sweep_*.py
```

## License

**Patent Pending**: U.S. Provisional Patent Application No. 63/842,846
**Copyright © 2025 Donte Lightfoot (STLNFTART)**
