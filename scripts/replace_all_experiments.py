#!/usr/bin/env python3
"""
Mass experiment replacement for all notebooks.
"""

import json
import sys
from pathlib import Path

# Import experiment templates
sys.path.append(str(Path(__file__).parent))
from fix_experiments import SATELLITE_EXPERIMENTS
from mars_experiments import MARS_EXPERIMENTS
from swarm_experiments import SWARM_EXPERIMENTS

def replace_experiments_in_notebook(notebook_path, experiments_dict):
    """Replace all experiment code cells with robust implementations"""

    with open(notebook_path, 'r') as f:
        nb = json.load(f)

    cells = nb['cells']
    replacements = 0

    i = 0
    current_experiment = None

    while i < len(cells):
        cell = cells[i]

        # Check if this is an experiment markdown header
        if cell['cell_type'] == 'markdown':
            source_text = ''.join(cell.get('source', []))

            # Match "## Experiment N: ..."
            if source_text.startswith('## Experiment '):
                try:
                    # Extract experiment number
                    exp_num = int(source_text.split()[2].rstrip(':'))
                    current_experiment = exp_num
                    print(f"  Found Experiment {exp_num} at cell {i}")

                    # Replace the title if we have a custom one
                    if exp_num in experiments_dict and 'title' in experiments_dict[exp_num]:
                        new_title = experiments_dict[exp_num]['title']
                        # Keep the methodology section but update title
                        cells[i]['source'] = [
                            f"## Experiment {exp_num}: {new_title}\n",
                            "\n",
                            "### Objective\n",
                            f"{new_title} - Comprehensive Analysis\n",
                            "\n",
                            "### Methodology\n",
                            "Advanced physics-based simulation with Monte Carlo analysis\n",
                            "\n",
                            "### Expected Outcomes\n",
                            "Robust dataset with 10,000+ data points and statistical validation\n",
                            "\n",
                            "---\n",
                            "\n"
                        ]

                except (ValueError, IndexError):
                    pass

        # Replace code cells that belong to current experiment
        elif cell['cell_type'] == 'code' and current_experiment is not None:
            source_text = ''.join(cell.get('source', []))

            # Skip if this is a statistical analysis cell (we'll handle those separately)
            if 'Statistical Analysis' not in source_text and 'Experiment' in source_text:
                # This is the main experiment code cell
                if current_experiment in experiments_dict:
                    print(f"    Replacing code for Experiment {current_experiment}")
                    cells[i]['source'] = [experiments_dict[current_experiment]['code']]
                    replacements += 1
                    current_experiment = None  # Reset after replacing

        i += 1

    # Write back
    with open(notebook_path, 'w') as f:
        json.dump(nb, f, indent=1)

    return replacements

def main():
    notebooks_to_fix = [
        ('/home/user/MotorHandPro/notebooks/nasa/02_satellite_mechanics.ipynb', SATELLITE_EXPERIMENTS),
        ('/home/user/MotorHandPro/notebooks/experiments/02_mars_mission_explorer.ipynb', MARS_EXPERIMENTS),
        ('/home/user/MotorHandPro/notebooks/experiments/03_swarm_simulation.ipynb', SWARM_EXPERIMENTS),
        ('/home/user/MotorHandPro/notebooks/experiments/01_parameter_sweep_analysis.ipynb', SATELLITE_EXPERIMENTS),
        ('/home/user/MotorHandPro/notebooks/experiments/04_benchmark_analysis.ipynb', SATELLITE_EXPERIMENTS),
    ]

    total_replacements = 0

    for nb_path, experiments in notebooks_to_fix:
        print(f"\nProcessing: {Path(nb_path).name}")
        print("="*80)

        if not Path(nb_path).exists():
            print(f"  ERROR: File not found")
            continue

        replacements = replace_experiments_in_notebook(nb_path, experiments)
        total_replacements += replacements

        print(f"  Replaced {replacements} experiments")

    print("\n" + "="*80)
    print(f"TOTAL REPLACEMENTS: {total_replacements}")
    print("="*80)

if __name__ == "__main__":
    main()
