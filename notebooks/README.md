# MotorHandPro Jupyter Notebooks

Interactive computational notebooks for MotorHandPro analysis, visualization, and GPU-accelerated simulations.

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/notebooks/)
[![Run on Brev](https://img.shields.io/badge/Run%20on-Brev-orange)](https://brev.dev)

## 📚 Available Notebooks

### 01_getting_started.ipynb
**Interactive Introduction to Primal Logic Framework**

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/notebooks/01_getting_started.ipynb)

- Core Primal Logic constants (D, I3, S, λ)
- Control law implementation
- Interactive parameter tuning
- Stability analysis
- Real benchmark data visualization

**Time:** ~15 minutes
**Difficulty:** Beginner
**Requirements:** Python 3.8+, numpy, matplotlib, plotly

### 02_nasa_data_visualization.ipynb
**NASA Mission Data Exploration**

[![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/notebooks/02_nasa_data_visualization.ipynb)

- 3D radiation exposure visualization
- Consciousness adaptation tracking
- Mission scenario comparison
- Real-time data streaming simulation
- Interactive heatmaps

**Time:** ~20 minutes
**Difficulty:** Intermediate
**Requirements:** Python 3.8+, numpy, pandas, plotly

### 03_gpu_acceleration.ipynb
**GPU-Accelerated Simulations** 🚀

[![Run on Brev](https://img.shields.io/badge/Run%20on-Brev-orange)](https://brev.dev)

- GPU detection and configuration
- Vectorized batch simulations
- Performance benchmarking (CPU vs GPU)
- Massive parameter sweeps (1000+ runs)
- 3D parameter space visualization
- GPU memory profiling

**Time:** ~30 minutes
**Difficulty:** Advanced
**Requirements:** CUDA GPU, PyTorch, 8GB+ GPU memory recommended
**Optimized for:** Brev GPU instances (Tesla T4, A100)

## 🚀 Quick Start

### Run on Google Colab (Free)

1. Click any "Open in Colab" badge above
2. Sign in to Google account
3. Click "Runtime" → "Run all"
4. Explore and modify!

All dependencies are automatically installed. No setup required.

### Run on Brev (GPU Recommended)

1. Sign up at [brev.dev](https://brev.dev)
2. Create new workspace:
   - **Base Image:** PyTorch Development (CUDA 12.x)
   - **GPU:** Tesla T4 or A100
   - **Repository:** `https://github.com/STLNFTART/MotorHandPro`
3. Open Jupyter Lab
4. Navigate to `notebooks/` directory
5. Start with `03_gpu_acceleration.ipynb` for GPU features

### Run Locally

```bash
# Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Install dependencies
pip install -r requirements.txt
pip install jupyter

# Start Jupyter
jupyter notebook notebooks/
```

## 📊 What You'll Learn

### Primal Logic Framework
- Exponential memory weighting with λ (Lightfoot constant)
- Fixed-point attractor D (Donte constant)
- Lipschitz contractivity and bounded convergence
- Control energy Ec(t) dynamics

### Data Visualization
- Interactive 3D plots with Plotly
- Real-time data streaming
- NASA mission data analysis
- Parameter sensitivity visualization

### GPU Acceleration
- PyTorch tensor operations
- Batch parallel simulations
- CPU vs GPU performance comparison
- Memory optimization techniques

## 🛠 Dependencies

All notebooks automatically install required dependencies on first run.

### Core Dependencies
```
numpy>=1.20.0
scipy>=1.7.0
matplotlib>=3.4.0
plotly>=5.0.0
pandas>=1.3.0
```

### GPU Notebooks (03_gpu_acceleration.ipynb)
```
torch>=2.0.0
torchvision>=0.15.0
```

## 💡 Tips & Best Practices

### For Google Colab Users

1. **Enable GPU (Optional):**
   - Runtime → Change runtime type → GPU
   - Useful for `03_gpu_acceleration.ipynb`

2. **Save Work:**
   - File → Save a copy in Drive
   - Your changes are not automatically saved

3. **Disconnect Warning:**
   - Colab disconnects after 90 min idle
   - Save frequently!

### For Brev Users

1. **GPU Selection:**
   - Tesla T4: Good for learning ($0.40/hr)
   - A100: Best for large-scale sweeps ($2.50/hr)

2. **Auto-Shutdown:**
   - Configure auto-shutdown to save credits
   - Default: 30 minutes idle

3. **Persistent Storage:**
   - Work is automatically saved to workspace
   - Clone repo updates: `git pull origin main`

### General Tips

1. **Run cells in order:** Notebooks are designed to be executed sequentially
2. **Modify parameters:** Change values and re-run cells to experiment
3. **Clear output:** Kernel → Restart & Clear Output to reset
4. **Check GPU status:** Run GPU detection cell first in GPU notebook

## 📈 Performance Benchmarks

Approximate execution times (may vary):

| Notebook | Colab (CPU) | Colab (GPU) | Brev T4 | Brev A100 |
|----------|-------------|-------------|---------|-----------|
| 01_getting_started | 2-3 min | 2-3 min | 1-2 min | 1-2 min |
| 02_nasa_data | 3-5 min | 3-5 min | 2-3 min | 2-3 min |
| 03_gpu_acceleration | 15-20 min | 5-8 min | 3-5 min | 1-2 min |

**Parameter sweep (1000 runs):**
- CPU: ~300 seconds
- Tesla T4: ~15 seconds (**20x faster**)
- A100: ~5 seconds (**60x faster**)

## 🔬 Advanced Usage

### Custom Parameter Sweeps

```python
# In 03_gpu_acceleration.ipynb
KE_values = torch.linspace(0.0, 2.0, 10000, device='cuda')  # 10,000 runs!
results = primal_logic_gpu(t_max=100.0, dt=0.01, KE=KE_values,
                           batch_size=len(KE_values), device='cuda')
```

### Multi-GPU (Brev Only)

```python
# Use multiple GPUs for even larger batches
if torch.cuda.device_count() > 1:
    print(f"Using {torch.cuda.device_count()} GPUs")
    # Distribute batch across GPUs
    # (Advanced: requires DataParallel or DistributedDataParallel)
```

### Export to Production

```python
# Save optimized parameters
optimal_params = {
    'KE': best_KE,
    'LAMBDA': 0.16905,
    'D': 149.9992314000
}

import json
with open('../config/optimal_params.json', 'w') as f:
    json.dump(optimal_params, f)
```

## 🐛 Troubleshooting

### "Module not found" errors
**Solution:** Re-run the setup cell at the beginning of the notebook.

### GPU not detected in Colab
**Solution:** Runtime → Change runtime type → GPU → Save

### Out of memory (GPU)
**Solution:** Reduce batch_size in simulation parameters.

### Notebook won't run
**Solution:**
1. Kernel → Restart & Clear Output
2. Re-run all cells from top

### Slow performance
**Solution:**
- Use GPU runtime for notebook 03
- Reduce t_max or increase dt
- Reduce batch_size

## 📚 Learn More

- [MotorHandPro README](../README.md) - Project overview
- [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md) - Mathematical foundations
- [Setup Guide](../SETUP.md) - Local installation
- [NASA Data Pipeline](../NASA_DATA_PIPELINE.md) - Mission simulation details
- [Visualization Guide](../VISUALIZATION_IMPLEMENTATION_GUIDE.md) - Advanced plotting

## 🤝 Contributing

Found an issue or want to add a notebook?

1. Fork the repository
2. Create your feature branch: `git checkout -b feature/new-notebook`
3. Add your notebook to `notebooks/` directory
4. Update this README with description
5. Submit pull request

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines.

## 📄 License & Citation

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

If you use these notebooks in your research, please cite:

```bibtex
@software{motorhandpro2025,
  author = {Lightfoot, Donte},
  title = {MotorHandPro: High-Precision Robotic Control with Primal Logic Framework},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/STLNFTART/MotorHandPro},
  note = {U.S. Provisional Patent Application No. 63/842,846}
}
```

## 🙋 Support

- **Issues:** [GitHub Issues](https://github.com/STLNFTART/MotorHandPro/issues)
- **Discussions:** [GitHub Discussions](https://github.com/STLNFTART/MotorHandPro/discussions)
- **Contact:** Donte Lightfoot (STLNFTART)

---

© 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety

**Happy Computing!** 🚀
