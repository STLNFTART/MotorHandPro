# MotorHandPro Brev Deployment Guide

**GPU-Accelerated Scientific Computing Environment**

[![Run on Brev](https://img.shields.io/badge/Run%20on-Brev-orange)](https://brev.dev)

Patent Pending: U.S. Provisional Patent Application No. 63/842,846

## Overview

This guide shows how to deploy MotorHandPro as a Brev Launchable - an AlphaFold-style scientific computing environment optimized for GPU-accelerated simulations and interactive Jupyter notebooks.

## Quick Start

### 1. Create Brev Workspace

1. Sign up at [brev.dev](https://brev.dev)
2. Click **"New Workspace"**
3. Configure:
   - **Name:** MotorHandPro GPU Environment
   - **Base Image:** PyTorch Development (CUDA 12.x)
   - **GPU:** Tesla T4 (recommended) or A100
   - **CPU:** 4 cores minimum
   - **RAM:** 16GB minimum
   - **Storage:** 50GB minimum

### 2. Clone Repository

```bash
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro
```

### 3. Run Setup

```bash
# Run automated setup
bash setup.sh --full

# Verify GPU
python -c "import torch; print(f'CUDA Available: {torch.cuda.is_available()}'); print(f'GPU: {torch.cuda.get_device_name(0) if torch.cuda.is_available() else \"None\"}')"
```

### 4. Start Jupyter Lab

```bash
jupyter lab --ip=0.0.0.0 --port=8888 --no-browser --allow-root
```

Access at: `https://your-workspace.brev.dev:8888`

## Docker Deployment on Brev

Brev supports Docker containers. Use the GPU-optimized Dockerfile:

```bash
# Build GPU image
docker build -f Dockerfile.gpu -t motorhandpro-gpu .

# Run with GPU support
docker run --gpus all -p 8888:8888 \
  -v $(pwd)/notebooks:/workspace/notebooks \
  -v $(pwd)/experiments:/workspace/experiments \
  motorhandpro-gpu
```

## Brev Configuration File

Create `.brev/config.yaml` in the repository:

```yaml
# .brev/config.yaml
name: MotorHandPro GPU Environment
description: High-precision robotic control with Primal Logic Framework

# Base configuration
base:
  image: nvcr.io/nvidia/pytorch:24.04-py3
  gpu: tesla-t4  # or a100
  cpu: 4
  memory: 16GB
  storage: 50GB

# Setup script
setup:
  - bash setup.sh --full
  - pip install jupyter jupyterlab ipykernel

# Environment variables
env:
  PYTHONPATH: /workspace
  PYTHONUNBUFFERED: "1"
  CUDA_VISIBLE_DEVICES: "0"

# Exposed ports
ports:
  - 8888  # Jupyter Lab
  - 8000  # FastAPI (if running locally)

# Start command
start:
  command: jupyter lab --ip=0.0.0.0 --port=8888 --no-browser --allow-root

# Workspace directory
workdir: /workspace

# Git repository
repository:
  url: https://github.com/STLNFTART/MotorHandPro.git
  branch: main

# Notebooks
notebooks:
  - notebooks/01_getting_started.ipynb
  - notebooks/02_nasa_data_visualization.ipynb
  - notebooks/03_gpu_acceleration.ipynb
```

## GPU Performance Benchmarks

### Expected Speedups on Brev

| Task | CPU (Local) | Tesla T4 | A100 | Speedup (T4) | Speedup (A100) |
|------|-------------|----------|------|--------------|----------------|
| Single simulation (100s) | 2.5s | 0.15s | 0.05s | 17x | 50x |
| Parameter sweep (1000 runs) | 300s | 15s | 5s | 20x | 60x |
| Large-scale sweep (10,000 runs) | 3000s | 120s | 30s | 25x | 100x |

### GPU Memory Usage

| Task | GPU Memory | Recommended GPU |
|------|------------|-----------------|
| Single notebook | ~1GB | Tesla T4 |
| Parameter sweep (1K) | ~2GB | Tesla T4 |
| Large sweep (10K) | ~4GB | Tesla T4 |
| Massive sweep (100K) | ~12GB | A100 |

## Cost Optimization

### Tesla T4 Pricing (Brev)

- **Hourly rate:** ~$0.40/hour
- **Example workload:** 10,000 parameter sweep
  - Runtime: ~2 minutes
  - Cost: ~$0.013
  - vs. Local CPU: 50 minutes, no GPU cost but time = $$$

### A100 Pricing (Brev)

- **Hourly rate:** ~$2.50/hour
- **Example workload:** 100,000 parameter sweep
  - Runtime: ~30 minutes
  - Cost: ~$1.25
  - vs. Local CPU: 8+ hours

### Cost-Saving Tips

1. **Auto-shutdown:** Configure auto-shutdown after 30 min idle
2. **Use T4 for development:** Reserve A100 for production sweeps
3. **Batch your work:** Queue multiple experiments, run once
4. **Snapshot your environment:** Save setup time on restart

## Available Notebooks

### 01_getting_started.ipynb
**Introduction to Primal Logic**

- Core constants and control law
- Interactive parameter tuning
- Stability analysis
- **Runtime:** ~15 minutes on T4

### 02_nasa_data_visualization.ipynb
**NASA Mission Data Analysis**

- 3D radiation exposure visualization
- Mission scenario comparison
- Real-time streaming simulation
- **Runtime:** ~20 minutes on T4

### 03_gpu_acceleration.ipynb
**GPU-Accelerated Simulations** ⚡

- Performance benchmarking (CPU vs GPU)
- Massive parameter sweeps (1000+ runs)
- 3D parameter space visualization
- **Runtime:** ~5 minutes on T4, ~2 minutes on A100

## Running Production Workloads

### Large-Scale Parameter Sweep

```bash
# In Brev terminal
python run_comprehensive_sweep.py \
  --n-samples 10000 \
  --gpu \
  --output experiments/sweep_results_$(date +%Y%m%d_%H%M%S).csv
```

### NASA Mission Simulation

```bash
python live_nasa_pipeline.py \
  --mission-duration 180 \
  --shield-configs 5,10,20 \
  --gpu \
  --output validation_results/
```

### Batch Processing

Create `batch_jobs.sh`:

```bash
#!/bin/bash
# Batch GPU workloads

echo "Starting batch jobs at $(date)"

# Job 1: Parameter sweep
python run_comprehensive_sweep.py --n-samples 5000 --gpu

# Job 2: NASA simulation
python live_nasa_pipeline.py --mission-duration 180 --gpu

# Job 3: Validation runs
python validate_vs_optimus.py --gpu

echo "All jobs complete at $(date)"
```

Run with:

```bash
bash batch_jobs.sh > logs/batch_$(date +%Y%m%d_%H%M%S).log 2>&1
```

## Monitoring GPU Usage

### In Jupyter Notebook

```python
import torch

# Check GPU status
if torch.cuda.is_available():
    print(f"GPU: {torch.cuda.get_device_name(0)}")
    print(f"Memory Allocated: {torch.cuda.memory_allocated(0) / 1e9:.2f} GB")
    print(f"Memory Reserved: {torch.cuda.memory_reserved(0) / 1e9:.2f} GB")
else:
    print("No GPU available")
```

### In Terminal

```bash
# Real-time GPU monitoring
watch -n 1 nvidia-smi

# Or use nvtop (more user-friendly)
nvtop
```

## Exporting Results

### Download from Brev

```bash
# Package results
tar -czf results_$(date +%Y%m%d).tar.gz \
  experiments/ \
  validation_results/ \
  notebooks/*.ipynb

# Download via Brev CLI or web interface
```

### Sync to Cloud Storage

```bash
# AWS S3
aws s3 sync validation_results/ s3://your-bucket/motorhand/results/

# Google Cloud Storage
gsutil -m rsync -r validation_results/ gs://your-bucket/motorhand/results/
```

## Troubleshooting

### GPU Not Detected

```bash
# Check NVIDIA driver
nvidia-smi

# Check CUDA
nvcc --version

# Check PyTorch CUDA
python -c "import torch; print(torch.version.cuda)"
```

### Out of Memory

```python
# Clear GPU cache in notebook
import torch
torch.cuda.empty_cache()

# Or reduce batch size
batch_size = 1000  # Instead of 10000
```

### Slow Jupyter

```bash
# Restart Jupyter with more resources
jupyter lab --ip=0.0.0.0 --port=8888 --no-browser --allow-root \
  --NotebookApp.iopub_data_rate_limit=10000000
```

## Advanced: Multi-GPU

If using a multi-GPU Brev instance:

```python
# In Python/Jupyter
import torch

if torch.cuda.device_count() > 1:
    print(f"Using {torch.cuda.device_count()} GPUs")

    # Distribute batch across GPUs
    from torch.nn import DataParallel
    # Your model/computation here
```

## Brev-Specific Features

### Snapshot & Resume

1. **Create snapshot:** Save your environment state in Brev dashboard
2. **Resume later:** Restart from snapshot with all packages pre-installed
3. **Share:** Share snapshot URL with collaborators

### Collaboration

Brev workspaces support real-time collaboration:

```bash
# In Brev web UI:
# Settings → Sharing → Invite collaborators
```

### VS Code Integration

Brev supports VS Code remote development:

1. Install "Brev" VS Code extension
2. Connect to workspace
3. Edit code with full IntelliSense + GPU access

## Resource Recommendations

### For Learning & Development
- **GPU:** Tesla T4
- **CPU:** 4 cores
- **RAM:** 16GB
- **Storage:** 50GB
- **Cost:** ~$0.40/hour

### For Production Sweeps
- **GPU:** A100
- **CPU:** 8 cores
- **RAM:** 32GB
- **Storage:** 100GB
- **Cost:** ~$2.50/hour

### For Massive Workloads
- **GPU:** 2x A100 or A100-80GB
- **CPU:** 16 cores
- **RAM:** 64GB
- **Storage:** 200GB
- **Cost:** ~$5.00/hour

## Next Steps

1. ✅ **Start with notebooks:** Begin with `01_getting_started.ipynb`
2. ✅ **Benchmark GPU:** Run `03_gpu_acceleration.ipynb`
3. ✅ **Production workload:** Execute `run_comprehensive_sweep.py`
4. ✅ **Export results:** Download to local or sync to cloud
5. ✅ **Create snapshot:** Save environment for later

## Support

- **Brev Documentation:** [docs.brev.dev](https://docs.brev.dev)
- **MotorHandPro Issues:** [GitHub Issues](https://github.com/STLNFTART/MotorHandPro/issues)
- **Contact:** Donte Lightfoot (STLNFTART)

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
© 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety

**Happy GPU Computing!** 🚀⚡
