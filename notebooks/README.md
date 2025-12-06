# MotorHandPro Jupyter Notebooks

Comprehensive interactive notebooks for exploring the Primal Logic framework, experiments, and applications.

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/notebooks/)

## 📚 Notebook Categories

### 🎯 Core Concepts (Root Directory)

1. **01_primal_logic_introduction.ipynb** - Introduction to Primal Logic framework
   - Lightfoot and Donte constants
   - Exponential memory weighting
   - Interactive parameter tuning
   - Step response analysis

2. **02_multi_language_comparison.ipynb** - Compare APL, Python, Prolog, and D implementations
   - Performance benchmarking
   - Language selection guide
   - Cross-language examples
   - When to use each language

3. **03_fixed_point_convergence.ipynb** - Mathematical convergence analysis
   - Fixed-point iteration
   - Bifurcation diagrams
   - Basin of attraction
   - Stability analysis

### 🧪 Experiments

- **01_parameter_sweep_analysis.ipynb** - Systematic parameter exploration
- **02_mars_mission_explorer.ipynb** - Mars mission planning and radiation analysis
- **03_swarm_simulation.ipynb** - Multi-agent UAV swarm coordination
- **04_benchmark_analysis.ipynb** - Performance benchmarking and metrics

### 🚀 NASA Applications

- **01_nasa_data_pipeline.ipynb** - NASA data integration and visualization
- **02_satellite_mechanics.ipynb** - Orbital mechanics and satellite control
- **03_space_environment.ipynb** - Space environment effects and modeling
- **04_crew_health_dashboard.ipynb** - Astronaut health monitoring

### 🤖 LAM System (Large Action Model)

- **01_lam_introduction.ipynb** - Getting started with LAM
- **02_temporal_displacement.ipynb** - Time-aware control fields
- **03_lam_reasoning.ipynb** - Prolog-based planning and reasoning
- **04_lab_assistant.ipynb** - Laboratory experiment tracking

### 🏥 Biomedical Applications

- **01_cardiac_models.ipynb** - Cardiac AI model analysis
- **02_organ_chip_integration.ipynb** - Organ-on-chip experiments
- **03_drug_safety.ipynb** - Drug safety modeling

### ⚙️ Hardware & Sensors

- **01_sensor_integration.ipynb** - Real-time sensor data analysis
- **02_motor_hand_control.ipynb** - 15-actuator robotic hand control
- **03_hardware_validation.ipynb** - Real-world validation results

### ⛓️ Blockchain & Compliance

- **01_rpo_token.ipynb** - Hedera $RPO token burn analysis
- **01_regulatory_compliance.ipynb** - FDA/NHTSA/FAA compliance checking

### 📊 Visualization

- **01_comprehensive_viz.ipynb** - Complete visualization suite
- **02_heatmap_generation.ipynb** - Parameter sensitivity heatmaps
- **03_quantum_state.ipynb** - Quantum state visualization

### 📖 Tutorials

- **01_control_fundamentals.ipynb** - Control theory basics
- **02_theory_to_code.ipynb** - From mathematics to implementation
- **03_multi_language_guide.ipynb** - Multi-language programming guide

### 🔬 Research

- **01_unified_field_theory.ipynb** - Unified field theory exploration
- **02_quantum_algorithms.ipynb** - Quantum-inspired algorithms
- **03_primal_echo_stack.ipynb** - Multi-scale biological networks

## 🚀 Getting Started

### Local Setup

```bash
# Clone the repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Install dependencies
pip install -r requirements.txt

# Install Jupyter
pip install jupyter notebook jupyterlab

# Launch Jupyter
jupyter notebook notebooks/
```

### Google Colab

Click the "Open in Colab" badge at the top of any notebook to run it directly in Google Colab - no installation required!

Each notebook will automatically:
1. Install required dependencies
2. Clone the MotorHandPro repository
3. Set up the Python path
4. Load necessary modules

## 📦 Dependencies

Core requirements:
- `numpy` - Numerical computing
- `matplotlib` - Plotting and visualization
- `scipy` - Scientific computing
- `pandas` - Data analysis
- `seaborn` - Statistical visualization

Optional:
- `ipywidgets` - Interactive controls
- `plotly` - Interactive 3D plots
- `opencv-python` - Image processing
- `scikit-learn` - Machine learning

## 🎓 Learning Path

### Beginners
1. Start with `01_primal_logic_introduction.ipynb`
2. Try `tutorials/01_control_fundamentals.ipynb`
3. Experiment with `experiments/01_parameter_sweep_analysis.ipynb`

### Intermediate
1. `02_multi_language_comparison.ipynb`
2. `03_fixed_point_convergence.ipynb`
3. `experiments/02_mars_mission_explorer.ipynb`
4. `lam/01_lam_introduction.ipynb`

### Advanced
1. `research/01_unified_field_theory.ipynb`
2. `research/02_quantum_algorithms.ipynb`
3. `biomedical/` notebooks
4. `hardware/` notebooks

## 🔧 Customization

All notebooks support parameter customization. Look for cells marked with:
- `# CUSTOMIZE:` for key parameters
- Interactive widgets (when `ipywidgets` is installed)
- Configuration cells at the top

## 📊 Datasets

Some notebooks load data from:
- `experiments/runs/` - Experiment results (CSV files)
- `experiments/configs/` - Configuration files (JSON)
- Live NASA APIs (automatic download)

## 🤝 Contributing

To contribute a notebook:

1. Follow the existing naming convention: `##_descriptive_name.ipynb`
2. Include Google Colab badge at the top
3. Add setup cell for Colab compatibility
4. Document all parameters and outputs
5. Include markdown explanations
6. Add to this README

## 📝 Notebook Template

```python
# Cell 1: Colab badge (markdown)
<a href="https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/notebooks/YOUR_NOTEBOOK.ipynb">
  <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/>
</a>

# Cell 2: Setup
import sys
if 'google.colab' in sys.modules:
    !pip install numpy matplotlib pandas
    !git clone https://github.com/STLNFTART/MotorHandPro.git
    sys.path.append('/content/MotorHandPro')
else:
    sys.path.append('..')

# Cell 3+: Your content
```

## 🐛 Troubleshooting

### Import Errors
- **Local**: Check `requirements.txt` is installed
- **Colab**: Restart runtime after pip install

### Path Errors
- **Local**: Run from project root or adjust `sys.path`
- **Colab**: Repository is cloned to `/content/MotorHandPro`

### Data Not Found
- Check `experiments/runs/` directory exists
- Run experiment generation scripts first
- Some notebooks generate synthetic data if files are missing

## 📖 Documentation

For more information:
- [Main README](../README.md)
- [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md)
- [Multi-Language Architecture](../MULTI_LANGUAGE_ARCHITECTURE.md)
- [NASA Data Pipeline](../NASA_DATA_PIPELINE.md)
- [LAM Implementation](../LAM_IMPLEMENTATION.md)

## 📧 Support

- **Issues**: [GitHub Issues](https://github.com/STLNFTART/MotorHandPro/issues)
- **Discussions**: [GitHub Discussions](https://github.com/STLNFTART/MotorHandPro/discussions)
- **Documentation**: See main repository README

## 📄 License

MIT License - see [LICENSE](../LICENSE) file

---

**Happy Learning! 🎉**

Explore the Primal Logic framework through interactive, hands-on Jupyter notebooks!
