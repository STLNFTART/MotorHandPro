#!/usr/bin/env python3
"""
Add Brev.dev support cells to all MotorHandPro notebooks
"""

import json
import sys
from pathlib import Path

def create_brev_setup_markdown():
    """Create comprehensive Brev setup markdown cell"""
    return {
        "cell_type": "markdown",
        "metadata": {
            "id": "brev_setup"
        },
        "source": [
            "---\n",
            "\n",
            "# 🚀 Brev.dev Notebook Support\n",
            "\n",
            "This notebook is optimized for **[Brev.dev](https://brev.dev)** - GPU-accelerated cloud notebooks with up to **60x faster** performance than local CPU execution.\n",
            "\n",
            "## Quick Start on Brev\n",
            "\n",
            "### 1. Launch Brev Instance\n",
            "```bash\n",
            "# Visit https://brev.dev and create a new notebook instance\n",
            "# Select GPU: Tesla T4 or A100 (recommended)\n",
            "```\n",
            "\n",
            "### 2. Clone Repository\n",
            "```bash\n",
            "git clone https://github.com/STLNFTART/MotorHandPro.git\n",
            "cd MotorHandPro\n",
            "```\n",
            "\n",
            "### 3. Install Dependencies\n",
            "```bash\n",
            "pip install -r requirements.txt\n",
            "```\n",
            "\n",
            "### 4. Verify GPU (if using GPU instance)\n",
            "```bash\n",
            "nvidia-smi\n",
            "```\n",
            "\n",
            "## Brev Configuration\n",
            "\n",
            "### Recommended Settings:\n",
            "\n",
            "| Setting | Value | Notes |\n",
            "|---------|-------|-------|\n",
            "| **GPU** | Tesla T4 or A100 | T4 for most tasks, A100 for heavy computation |\n",
            "| **Base Image** | `nvcr.io/nvidia/pytorch:24.04-py3` | Includes CUDA, PyTorch, scientific stack |\n",
            "| **Storage** | 50GB+ | For datasets and model checkpoints |\n",
            "| **Memory** | 16GB+ RAM | Recommended for data processing |\n",
            "\n",
            "### GPU Options:\n",
            "\n",
            "| GPU Model | VRAM | Performance | Cost/Hour | Best For |\n",
            "|-----------|------|-------------|-----------|----------|\n",
            "| **Tesla T4** | 16GB | 20-30x CPU | ~$0.40 | Most experiments, development |\n",
            "| **A100** | 40GB | 50-60x CPU | ~$2.50 | Heavy ML training, large datasets |\n",
            "| **V100** | 16GB | 30-40x CPU | ~$1.50 | Balanced performance/cost |\n",
            "\n",
            "## Performance Comparison\n",
            "\n",
            "Typical speedups for MotorHandPro workloads:\n",
            "\n",
            "| Platform | Relative Speed | Cost | Availability |\n",
            "|----------|----------------|------|-------------|\n",
            "| **Local CPU** | 1x (baseline) | Free | Always |\n",
            "| **Google Colab Free** | 5-10x | Free | Limited GPU time |\n",
            "| **Brev T4** | 20-30x | $0.40/hr | On-demand |\n",
            "| **Brev A100** | 50-60x | $2.50/hr | On-demand |\n",
            "\n",
            "## Brev-Specific Features\n",
            "\n",
            "### Persistent Storage\n",
            "Unlike Colab, Brev instances maintain your data between sessions:\n",
            "```bash\n",
            "# Data persists in your home directory\n",
            "~/MotorHandPro/data/\n",
            "~/MotorHandPro/models/\n",
            "```\n",
            "\n",
            "### SSH Access\n",
            "Connect to your Brev instance via SSH:\n",
            "```bash\n",
            "brev ssh <your-instance-name>\n",
            "```\n",
            "\n",
            "### VSCode Integration\n",
            "Open your Brev instance in VSCode:\n",
            "```bash\n",
            "brev open <your-instance-name>\n",
            "```\n",
            "\n",
            "## Environment Setup for Brev\n",
            "\n",
            "Run this cell to detect and configure for Brev:\n"
        ]
    }

def create_brev_detection_code():
    """Create Brev environment detection code cell"""
    return {
        "cell_type": "code",
        "execution_count": None,
        "metadata": {
            "id": "brev_detection"
        },
        "outputs": [],
        "source": [
            "import os\n",
            "import sys\n",
            "import subprocess\n",
            "\n",
            "# Detect environment\n",
            "IN_COLAB = 'google.colab' in sys.modules\n",
            "IN_BREV = os.path.exists('/home/ubuntu/.brev')  # Brev indicator\n",
            "HAS_GPU = False\n",
            "\n",
            "# Check for GPU\n",
            "try:\n",
            "    result = subprocess.run(['nvidia-smi'], capture_output=True, text=True)\n",
            "    HAS_GPU = result.returncode == 0\n",
            "except FileNotFoundError:\n",
            "    HAS_GPU = False\n",
            "\n",
            "# Print environment info\n",
            "print(\"=\"*60)\n",
            "print(\"ENVIRONMENT DETECTION\")\n",
            "print(\"=\"*60)\n",
            "print(f\"Running on Colab: {IN_COLAB}\")\n",
            "print(f\"Running on Brev:  {IN_BREV}\")\n",
            "print(f\"GPU Available:    {HAS_GPU}\")\n",
            "\n",
            "if HAS_GPU:\n",
            "    print(\"\\n\" + \"=\"*60)\n",
            "    print(\"GPU INFORMATION\")\n",
            "    print(\"=\"*60)\n",
            "    subprocess.run(['nvidia-smi', '--query-gpu=name,memory.total,driver_version', '--format=csv'])\n",
            "\n",
            "# Install dependencies based on environment\n",
            "if IN_COLAB:\n",
            "    print(\"\\nInstalling dependencies for Colab...\")\n",
            "    !pip install -q numpy scipy matplotlib plotly pandas\n",
            "elif IN_BREV:\n",
            "    print(\"\\nBrev environment detected - dependencies should be pre-installed.\")\n",
            "    print(\"If you need additional packages, run: pip install <package>\")\n",
            "else:\n",
            "    print(\"\\nRunning locally - ensure requirements.txt is installed.\")\n",
            "    print(\"Run: pip install -r requirements.txt\")\n",
            "\n",
            "print(\"\\n\" + \"=\"*60)\n",
            "print(\"READY TO START\")\n",
            "print(\"=\"*60)\n"
        ]
    }

def create_brev_comparison():
    """Create Brev vs other platforms comparison cell"""
    return {
        "cell_type": "markdown",
        "metadata": {
            "id": "brev_comparison"
        },
        "source": [
            "## Platform Comparison: Brev vs Colab vs Local\n",
            "\n",
            "### Feature Comparison\n",
            "\n",
            "| Feature | Local | Google Colab Free | Brev.dev |\n",
            "|---------|-------|-------------------|----------|\n",
            "| **Cost** | Free (your hardware) | Free | $0.40-$2.50/hour |\n",
            "| **GPU Access** | Depends on hardware | Limited (T4) | Unlimited (T4/A100) |\n",
            "| **Session Timeout** | None | 12 hours | Configurable |\n",
            "| **Storage** | Local disk | 100GB temporary | Persistent (50GB+) |\n",
            "| **SSH Access** | Yes | No | Yes |\n",
            "| **VSCode Integration** | Native | Browser only | Native |\n",
            "| **Custom Environment** | Full control | Limited | Full control |\n",
            "| **Data Persistence** | Permanent | Lost on disconnect | Permanent |\n",
            "\n",
            "### When to Use Each Platform\n",
            "\n",
            "**🏠 Local Development:**\n",
            "- Quick prototyping\n",
            "- No GPU needed\n",
            "- Working offline\n",
            "- Sensitive data that can't leave your machine\n",
            "\n",
            "**📒 Google Colab:**\n",
            "- Learning and tutorials\n",
            "- Quick experiments\n",
            "- Sharing notebooks publicly\n",
            "- Limited GPU needs (<12 hours)\n",
            "\n",
            "**🚀 Brev.dev:**\n",
            "- Production experiments\n",
            "- Long-running training (>12 hours)\n",
            "- Large datasets requiring persistent storage\n",
            "- Need for SSH/VSCode access\n",
            "- Team collaboration\n",
            "- High-performance GPU workloads\n",
            "\n",
            "### Cost Analysis\n",
            "\n",
            "Example: Training a model for 24 hours\n",
            "\n",
            "| Platform | GPU | Total Cost | Notes |\n",
            "|----------|-----|------------|-------|\n",
            "| Local | Your GPU | $0 + electricity | One-time hardware cost |\n",
            "| Colab Free | T4 (limited) | $0 | May timeout, need to restart |\n",
            "| Colab Pro | T4/V100 | $10/month | Still has timeouts |\n",
            "| Brev T4 | T4 | $9.60 | No timeouts, persistent |\n",
            "| Brev A100 | A100 | $60 | 2-3x faster = cheaper overall |\n",
            "\n",
            "💡 **Pro Tip:** Use Brev for training, export results, then analyze on Colab/Local for free.\n"
        ]
    }

def add_brev_to_notebook(notebook_path):
    """Add Brev support cells to a notebook"""
    try:
        # Load notebook
        with open(notebook_path, 'r', encoding='utf-8') as f:
            notebook = json.load(f)

        cells = notebook.get('cells', [])

        # Check if Brev cells already exist
        has_brev = any(
            cell.get('metadata', {}).get('id') == 'brev_setup'
            for cell in cells
        )

        if has_brev:
            print(f"  ⚠️  {notebook_path.name} already has Brev support")
            return False

        # Find insertion point - after title/setup, before main content
        # Look for the setup/import cell
        insert_idx = 3
        for i, cell in enumerate(cells[:10]):
            if cell.get('cell_type') == 'code':
                insert_idx = i + 1
                break

        # Insert Brev cells
        brev_cells = [
            create_brev_setup_markdown(),
            create_brev_detection_code(),
            create_brev_comparison()
        ]

        for cell in reversed(brev_cells):
            cells.insert(insert_idx, cell)

        notebook['cells'] = cells

        # Save notebook
        with open(notebook_path, 'w', encoding='utf-8') as f:
            json.dump(notebook, f, indent=1, ensure_ascii=False)

        print(f"  ✅ Added Brev support to {notebook_path.name}")
        return True

    except Exception as e:
        print(f"  ❌ Error processing {notebook_path.name}: {e}")
        return False

def main():
    """Add Brev support to all notebooks"""

    # Find all notebooks
    notebooks_dir = Path('/home/user/MotorHandPro/notebooks')

    if len(sys.argv) > 1:
        # Single notebook mode
        notebook_path = Path(sys.argv[1])
        add_brev_to_notebook(notebook_path)
    else:
        # Batch mode - all notebooks
        notebooks = list(notebooks_dir.rglob('*.ipynb'))

        print(f"\n🚀 Adding Brev support to {len(notebooks)} notebooks...\n")

        success_count = 0
        for notebook in sorted(notebooks):
            if add_brev_to_notebook(notebook):
                success_count += 1

        print(f"\n✅ Successfully added Brev support to {success_count}/{len(notebooks)} notebooks")

if __name__ == "__main__":
    main()
