#!/usr/bin/env python3
"""
Generate remaining Jupyter notebooks for MotorHandPro
"""

import json
from pathlib import Path

def create_notebook(title, colab_path, description, cells_content):
    """Create a Jupyter notebook"""
    cells = [
        {
            "cell_type": "markdown",
            "metadata": {"colab_type": "text", "id": "view-in-github"},
            "source": [f'<a href="https://colab.research.google.com/github/STLNFTART/MotorHandPro/blob/main/{colab_path}" target="_parent"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a>']
        },
        {
            "cell_type": "markdown",
            "metadata": {},
            "source": [f"# {title}\n\n{description}"]
        },
        {
            "cell_type": "code",
            "execution_count": None,
            "metadata": {},
            "source": [
                "import sys\n"
                "if 'google.colab' in sys.modules:\n"
                "    !pip install numpy matplotlib pandas\n"
                "    !git clone https://github.com/STLNFTART/MotorHandPro.git\n"
                "    sys.path.append('/content/MotorHandPro')\n"
                "else:\n"
                "    sys.path.append('..' if 'notebooks' not in str(Path.cwd()) else '../..')\n"
                "\n"
                "import numpy as np\n"
                "import matplotlib.pyplot as plt\n"
                "import pandas as pd"
            ],
            "outputs": []
        }
    ]

    # Add custom cells
    for cell in cells_content:
        cells.append(cell)

    notebook = {
        "cells": cells,
        "metadata": {
            "kernelspec": {"display_name": "Python 3", "language": "python", "name": "python3"},
            "language_info": {
                "codemirror_mode": {"name": "ipython", "version": 3},
                "file_extension": ".py",
                "mimetype": "text/x-python",
                "name": "python",
                "nbconvert_exporter": "python",
                "pygments_lexer": "ipython3",
                "version": "3.8.0"
            },
            "colab": {"provenance": [], "include_colab_link": True}
        },
        "nbformat": 4,
        "nbformat_minor": 0
    }

    return notebook

# Define all notebooks to create
notebooks = {
    "experiments/03_swarm_simulation.ipynb": {
        "title": "Swarm Simulation Analysis",
        "description": "Multi-agent UAV swarm coordination using Primal Logic:\n- Swarm consensus\n- Trust dynamics\n- Neighbor graphs\n- Formation control",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## 1. Swarm Initialization"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Swarm parameters\nn_agents = 10\nlambda_val = 0.16905\n\nclass SwarmAgent:\n    def __init__(self, agent_id):\n        self.id = agent_id\n        self.position = np.random.rand(3) * 10\n        self.velocity = np.zeros(3)\n        self.trust = np.ones(10) * 0.5\n        \n    def update(self, neighbors, dt=0.01):\n        # Consensus control\n        consensus = np.mean([n.position for n in neighbors], axis=0)\n        self.velocity = 0.5 * (consensus - self.position)\n        self.position += self.velocity * dt\n        \nagents = [SwarmAgent(i) for i in range(n_agents)]\nprint(f'Initialized {n_agents} swarm agents')"], "outputs": []},
            {"cell_type": "markdown", "metadata": {}, "source": ["## 2. Swarm Visualization"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Simulate swarm\nhistory = []\nfor t in range(100):\n    positions = [a.position.copy() for a in agents]\n    history.append(positions)\n    for agent in agents:\n        neighbors = [a for a in agents if a.id != agent.id]\n        agent.update(neighbors)\n\n# Plot trajectories\nfig = plt.figure(figsize=(12, 5))\nax1 = fig.add_subplot(121, projection='3d')\nfor i in range(n_agents):\n    traj = np.array([h[i] for h in history])\n    ax1.plot(traj[:, 0], traj[:, 1], traj[:, 2], alpha=0.7)\nax1.set_title('Swarm Trajectories', fontweight='bold')\nax1.set_xlabel('X')\nax1.set_ylabel('Y')\nax1.set_zlabel('Z')\n\nax2 = fig.add_subplot(122)\nfor i in range(n_agents):\n    traj = np.array([h[i] for h in history])\n    dist_to_center = np.linalg.norm(traj - np.mean(traj, axis=0), axis=1)\n    ax2.plot(dist_to_center, alpha=0.7)\nax2.set_title('Distance to Swarm Center', fontweight='bold')\nax2.set_xlabel('Time Step')\nax2.set_ylabel('Distance')\nax2.grid(True, alpha=0.3)\nplt.tight_layout()\nplt.show()"], "outputs": []},
        ]
    },

    "experiments/04_benchmark_analysis.ipynb": {
        "title": "Benchmark Analysis",
        "description": "Performance benchmarking and metrics analysis for Primal Logic implementations.",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## Load and analyze benchmark data from experiments"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["from pathlib import Path\nimport time\n\n# Benchmark Primal Logic\nclass PrimalBenchmark:\n    def __init__(self):\n        self.lambda_val = 0.16905\n        self.Ec = 0.0\n        \n    def update(self, error, dt):\n        self.Ec = self.Ec * np.exp(-self.lambda_val * dt) + error * dt\n        psi = error + self.lambda_val * self.Ec\n        return psi\n\ndef benchmark_iterations(n_iter=100000):\n    controller = PrimalBenchmark()\n    errors = np.random.randn(n_iter)\n    dt = 0.001\n    \n    start = time.perf_counter()\n    for e in errors:\n        controller.update(e, dt)\n    end = time.perf_counter()\n    \n    return (end - start) * 1000, n_iter\n\ntotal_time, n = benchmark_iterations()\nprint(f'Benchmark Results:')\nprint(f'Total time: {total_time:.2f} ms')\nprint(f'Iterations: {n}')\nprint(f'Average: {total_time*1000/n:.2f} µs per iteration')"], "outputs": []},
        ]
    },

    "lam/01_lam_introduction.ipynb": {
        "title": "LAM System Introduction",
        "description": "Getting started with the Large Action Model (LAM) system:\n- Quantum resonance fields\n- Action planning\n- Trip planning examples\n- Integration with Primal Logic",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## 1. LAM Architecture Overview"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# LAM system components\nprint('LAM System Components:')\nprint('- Primal LAM engine with resonance field')\nprint('- Trip planning and reservations')\nprint('- Food ordering')\nprint('- Lab assistant')\nprint('- Temporal displacement fields')"], "outputs": []},
            {"cell_type": "markdown", "metadata": {}, "source": ["## 2. Simple LAM Example"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["class SimpleLAM:\n    def __init__(self):\n        self.resonance_field = 0.0\n        self.actions = []\n        \n    def plan_action(self, goal, context):\n        # Simplified action planning\n        action = {\n            'goal': goal,\n            'context': context,\n            'resonance': self.resonance_field,\n            'timestamp': pd.Timestamp.now()\n        }\n        self.actions.append(action)\n        return action\n        \n    def update_resonance(self, feedback):\n        # Update quantum resonance field\n        self.resonance_field = 0.9 * self.resonance_field + 0.1 * feedback\n\nlam = SimpleLAM()\naction = lam.plan_action('book_flight', {'destination': 'Mars', 'date': '2025-01-15'})\nprint(f'Planned action: {action}')"], "outputs": []},
        ]
    },

    "nasa/01_nasa_data_pipeline.ipynb": {
        "title": "NASA Data Pipeline",
        "description": "Interactive NASA data visualization and analysis:\n- Solar particle events (SPE)\n- Galactic cosmic rays (GCR)\n- Mission planning\n- Real-time data integration",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## 1. NASA Data Sources"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Simulate NASA data\ndef generate_spe_data(days=500):\n    t = np.arange(days)\n    # Random SPE events\n    spe_events = np.zeros(days)\n    event_times = np.random.choice(days, size=5, replace=False)\n    for et in event_times:\n        spe_events[et:min(et+3, days)] = np.random.exponential(50, min(3, days-et))\n    return t, spe_events\n\nt, spe = generate_spe_data()\n\nplt.figure(figsize=(14, 5))\nplt.plot(t, spe, 'r-', linewidth=2)\nplt.xlabel('Mission Day')\nplt.ylabel('SPE Dose Rate (mSv/day)')\nplt.title('Solar Particle Events During Mission', fontweight='bold')\nplt.grid(True, alpha=0.3)\nplt.show()"], "outputs": []},
        ]
    },

    "biomedical/01_cardiac_models.ipynb": {
        "title": "Cardiac AI Model Analysis",
        "description": "Analysis of cardiac AI models from GitHub repositories:\n- Model convergence patterns\n- Performance metrics\n- Integration opportunities",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## Cardiac Model Simulation"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Simulate cardiac model\ndef cardiac_model(t, heart_rate=70):\n    # Simple cardiac cycle model\n    period = 60 / heart_rate\n    ecg = np.sin(2 * np.pi * t / period)\n    ecg += 0.3 * np.sin(4 * np.pi * t / period)\n    return ecg\n\nt = np.linspace(0, 3, 1000)\necg = cardiac_model(t)\n\nplt.figure(figsize=(14, 5))\nplt.plot(t, ecg, 'b-', linewidth=2)\nplt.xlabel('Time (s)')\nplt.ylabel('ECG Signal')\nplt.title('Simulated Cardiac Cycle', fontweight='bold')\nplt.grid(True, alpha=0.3)\nplt.show()"], "outputs": []},
        ]
    },

    "hardware/01_sensor_integration.ipynb": {
        "title": "Sensor Integration",
        "description": "Real-time sensor data analysis and integration with Primal Logic control.",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## Sensor Data Simulation"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Simulate sensor data\ndef generate_sensor_data(duration=10, dt=0.01, noise=0.1):\n    t = np.arange(0, duration, dt)\n    # Position sensor\n    position = np.sin(t) + noise * np.random.randn(len(t))\n    # Velocity sensor\n    velocity = np.cos(t) + noise * np.random.randn(len(t))\n    return t, position, velocity\n\nt, pos, vel = generate_sensor_data()\n\nfig, axes = plt.subplots(2, 1, figsize=(14, 8))\naxes[0].plot(t, pos, 'b-', alpha=0.7, linewidth=1)\naxes[0].set_ylabel('Position')\naxes[0].set_title('Position Sensor Data', fontweight='bold')\naxes[0].grid(True, alpha=0.3)\n\naxes[1].plot(t, vel, 'r-', alpha=0.7, linewidth=1)\naxes[1].set_xlabel('Time (s)')\naxes[1].set_ylabel('Velocity')\naxes[1].set_title('Velocity Sensor Data', fontweight='bold')\naxes[1].grid(True, alpha=0.3)\n\nplt.tight_layout()\nplt.show()"], "outputs": []},
        ]
    },

    "visualization/01_comprehensive_viz.ipynb": {
        "title": "Comprehensive Visualization Suite",
        "description": "Complete visualization examples for Primal Logic:\n- Time series plots\n- Heatmaps\n- 3D visualizations\n- Interactive plots",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## Visualization Examples"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["import seaborn as sns\n\n# Generate sample data\nx = np.linspace(-5, 5, 100)\ny = np.linspace(-5, 5, 100)\nX, Y = np.meshgrid(x, y)\nZ = np.sin(np.sqrt(X**2 + Y**2))\n\nfig = plt.figure(figsize=(14, 5))\n\n# 3D surface\nax1 = fig.add_subplot(121, projection='3d')\nax1.plot_surface(X, Y, Z, cmap='viridis')\nax1.set_title('3D Surface Plot', fontweight='bold')\n\n# Heatmap\nax2 = fig.add_subplot(122)\nim = ax2.contourf(X, Y, Z, levels=20, cmap='viridis')\nplt.colorbar(im, ax=ax2)\nax2.set_title('2D Heatmap', fontweight='bold')\n\nplt.tight_layout()\nplt.show()"], "outputs": []},
        ]
    },

    "tutorials/01_control_fundamentals.ipynb": {
        "title": "Control Theory Fundamentals",
        "description": "Introduction to control theory concepts:\n- PID control\n- State-space representation\n- Stability analysis\n- Comparison with Primal Logic",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## 1. PID vs Primal Logic"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["class PIDController:\n    def __init__(self, KP, KI, KD):\n        self.KP, self.KI, self.KD = KP, KI, KD\n        self.integral = 0\n        self.last_error = 0\n        \n    def update(self, error, dt):\n        self.integral += error * dt\n        derivative = (error - self.last_error) / dt\n        self.last_error = error\n        return self.KP * error + self.KI * self.integral + self.KD * derivative\n\nclass PrimalController:\n    def __init__(self, KE, lambda_val):\n        self.KE, self.lambda_val = KE, lambda_val\n        self.Ec = 0\n        \n    def update(self, error, dt):\n        self.Ec = self.Ec * np.exp(-self.lambda_val * dt) + error * dt\n        return error + self.KE * self.Ec\n\nprint('PID: Proportional-Integral-Derivative control')\nprint('Primal Logic: Exponentially weighted memory control')\nprint('\\nKey difference: Primal Logic has bounded memory!')"], "outputs": []},
        ]
    },

    "research/01_unified_field_theory.ipynb": {
        "title": "Unified Field Theory Exploration",
        "description": "Mathematical exploration of the unified field theory framework in MotorHandPro.",
        "cells": [
            {"cell_type": "markdown", "metadata": {}, "source": ["## Unified Field Theory Framework"]},
            {"cell_type": "code", "execution_count": None, "metadata": {}, "source": ["# Field equations\ndef quantum_field(x, t, lambda_val=0.16905):\n    # Simplified quantum field\n    return np.exp(-lambda_val * t) * np.sin(2 * np.pi * x)\n\nx = np.linspace(0, 10, 200)\nt_values = [0, 1, 2, 5, 10]\n\nplt.figure(figsize=(14, 6))\nfor t in t_values:\n    field = quantum_field(x, t)\n    plt.plot(x, field, label=f't={t}s', linewidth=2)\n\nplt.xlabel('Position x')\nplt.ylabel('Field Amplitude')\nplt.title('Quantum Field Evolution', fontweight='bold')\nplt.legend()\nplt.grid(True, alpha=0.3)\nplt.show()"], "outputs": []},
        ]
    },
}

# Generate all notebooks
print("Generating notebooks...")
for path, config in notebooks.items():
    full_path = Path("notebooks") / path
    full_path.parent.mkdir(parents=True, exist_ok=True)

    notebook = create_notebook(
        config["title"],
        f"notebooks/{path}",
        config["description"],
        config["cells"]
    )

    with open(full_path, 'w') as f:
        json.dump(notebook, f, indent=1)

    print(f"✓ Created {path}")

print(f"\n✨ Generated {len(notebooks)} notebooks successfully!")
