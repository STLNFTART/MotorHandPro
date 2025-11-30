# Analysis Tools

This directory contains analytical tools and visualizations for evaluating the Primal Logic control framework's performance and stability characteristics.

## Contents

### Visualization Scripts

- **`heatmap_fit.py`** - Parameter sensitivity analysis tool
  - Generates 2D heatmap visualizations showing how control parameters (λ, KE) affect system behavior
  - Outputs: `heatmap_Lec.png`, `heatmap_tzero.png`
  - Usage: `python heatmap_fit.py`

### Analysis Outputs

- **`kernel_v4_analysis.png`** - Comprehensive visualization of Primal Logic kernel behavior
  - Shows convergence characteristics and fixed-point iteration
  - Validates theoretical predictions from `PRIMAL_LOGIC_FRAMEWORK.md`

- **`primal_algorithms_validation.png`** - Algorithm validation comparison
  - Compares different Primal Logic variants
  - Demonstrates convergence and stability properties

- **`heatmap_Lec.png`** - Lipschitz constant sensitivity heatmap
  - Shows how Ec (control energy) varies with parameter changes
  - Critical for stability analysis

- **`heatmap_tzero.png`** - Zero-crossing time heatmap
  - Visualizes settling time across parameter space
  - Useful for tuning responsiveness vs. stability trade-offs

### Data Files

- **`lambda_fit.csv`** - Fitted λ (Lightfoot constant) values
  - Contains empirically determined decay rates
  - Format: Parameter configurations and corresponding λ values

- **`validation_report.json`** - Structured validation results
  - JSON format containing:
    - Test case parameters
    - Performance metrics (settling time, overshoot, etc.)
    - Stability indicators (Lipschitz constants, convergence rates)
    - Comparison with theoretical predictions

## Usage

### Running Parameter Sensitivity Analysis

```bash
cd analysis
python heatmap_fit.py
```

This will:
1. Load benchmark data from parent directory
2. Compute stability metrics across parameter ranges
3. Generate heatmap visualizations
4. Save results to PNG files

### Interpreting Results

**Stability Criteria:**
- Lipschitz constant F'(D) < 1 ensures contraction mapping
- Lower Ec values indicate better energy efficiency
- Shorter zero-crossing times mean faster response

**Color Coding in Heatmaps:**
- Cooler colors (blue/green): Stable, efficient operation
- Warmer colors (yellow/red): Less stable or slower convergence

## Integration with Main Repository

This analysis validates the theoretical framework described in:
- `PRIMAL_LOGIC_FRAMEWORK.md` - Mathematical foundations
- `AGP_PHYSICS_CLARIFICATION.md` - Physical interpretation
- `EMPIRICAL_CONSTANTS_DATABASE.md` - Constant definitions

## Requirements

```bash
pip install numpy matplotlib scipy
```

## Related Documentation

- [Main README](../README.md) - System overview
- [Testing & Benchmarking](../TESTING_AND_BENCHMARKING.md) - Comprehensive test procedures
- [Validation Results](../VALIDATION_RESULTS.md) - Experimental validation data

## Output Interpretation

### validation_report.json Structure

```json
{
  "test_case": "parameter_sweep_mu_ke",
  "metrics": {
    "settling_time": 5.92,
    "overshoot_percent": 2.3,
    "lipschitz_constant": 0.000129931830
  },
  "stability": "PASS"
}
```

---

**See also:** [`analyze_runs.py`](../analyze_runs.py) in the root directory for time-series analysis of individual benchmark runs.
