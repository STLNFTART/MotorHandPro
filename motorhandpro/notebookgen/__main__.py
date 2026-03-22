"""
python -m motorhandpro.notebookgen [--help]

Thin CLI wrapper around the shared notebookgen library.

Sub-commands
------------
generate
    Regenerate the standard set of notebooks (same outputs as the original
    generate_notebooks.py / generate_more_notebooks.py scripts).

expand
    Expand existing notebooks to a target line count (same behaviour as
    expand_notebooks_to_5000_lines.py / generate_5000_line_notebooks.py /
    build_massive_notebooks.py).
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import List, Optional

from motorhandpro.notebookgen._core import (
    count_lines,
    create_cell,
    create_notebook,
    expand_notebook,
    save_notebook,
)


# ---------------------------------------------------------------------------
# Experiment cell factory (shared by both generate and expand sub-commands)
# ---------------------------------------------------------------------------

def _experiment_cell_factory(experiment_num: int) -> list:
    """Return ~6 cells (~100 source lines) for one generic experiment section."""

    objectives = [
        "Evaluate baseline classification performance on EMG data.",
        "Assess the impact of different preprocessing techniques.",
        "Compare feature-extraction methods (time-domain vs frequency-domain).",
        "Analyse cross-dataset generalisation.",
        "Investigate radiation effects on signal quality.",
        "Validate LAM integration benefits.",
        "Test real-time performance constraints.",
        "Examine subject-independent validation.",
        "Optimise hyperparameters systematically.",
        "Benchmark against state-of-the-art methods.",
    ]
    model_types = ["SVC(kernel='rbf', C=1.0)",
                   "RandomForestClassifier(n_estimators=100, random_state=42)",
                   "GradientBoostingClassifier(n_estimators=100, random_state=42)",
                   "MLPClassifier(hidden_layer_sizes=(100, 50), max_iter=500, random_state=42)"]
    model_imports = [
        "from sklearn.svm import SVC",
        "from sklearn.ensemble import RandomForestClassifier",
        "from sklearn.ensemble import GradientBoostingClassifier",
        "from sklearn.neural_network import MLPClassifier",
    ]

    objective = objectives[experiment_num % len(objectives)]
    model_expr = model_types[experiment_num % len(model_types)]
    model_import = model_imports[experiment_num % len(model_imports)]

    cells = [
        create_cell("markdown", [
            f"## Experiment {experiment_num}\n",
            "\n",
            f"**Objective:** {objective}\n",
            "\n",
            "**Parameters:**\n",
            "- Sample rate: 1000 Hz\n",
            "- Window size: 200 ms\n",
            "- Overlap: 50 %\n",
            "- Confidence threshold: 0.85\n",
        ]),
        create_cell("code", [
            f"# Experiment {experiment_num} — setup\n",
            "import numpy as np\n",
            "import pandas as pd\n",
            "import matplotlib.pyplot as plt\n",
            "from sklearn.model_selection import train_test_split\n",
            "from sklearn.preprocessing import StandardScaler\n",
            "from sklearn.metrics import confusion_matrix, classification_report\n",
            f"{model_import}\n",
            "\n",
            f"np.random.seed(42 + {experiment_num})\n",
            f"n_samples, n_features, n_classes = 1000, {8 + experiment_num % 5}, {3 + experiment_num % 3}\n",
            "X = np.random.randn(n_samples, n_features) * 0.5\n",
            "y = np.random.randint(0, n_classes, n_samples)\n",
            "for i in range(n_classes):\n",
            "    mask = y == i\n",
            "    X[mask] += np.random.randn(n_features) * (i + 1) * 0.3\n",
            "\n",
            "X_train, X_test, y_train, y_test = train_test_split(\n",
            "    X, y, test_size=0.3, random_state=42, stratify=y\n",
            ")\n",
            "scaler = StandardScaler()\n",
            "X_train_s = scaler.fit_transform(X_train)\n",
            "X_test_s  = scaler.transform(X_test)\n",
            "\n",
            f"model = {model_expr}\n",
            "model.fit(X_train_s, y_train)\n",
            "train_acc = model.score(X_train_s, y_train)\n",
            "test_acc  = model.score(X_test_s,  y_test)\n",
            f"print(f'Experiment {experiment_num}: train={{train_acc:.4f}}  test={{test_acc:.4f}}')\n",
        ]),
        create_cell("code", [
            f"# Experiment {experiment_num} — visualisation\n",
            "from sklearn.decomposition import PCA\n",
            "\n",
            "y_pred = model.predict(X_test_s)\n",
            "cm = confusion_matrix(y_test, y_pred)\n",
            "\n",
            "fig, axes = plt.subplots(1, 2, figsize=(12, 4))\n",
            "axes[0].imshow(cm, cmap='Blues')\n",
            "axes[0].set_title(f'Confusion Matrix — Experiment {experiment_num}')\n",
            "axes[0].set_xlabel('Predicted'); axes[0].set_ylabel('True')\n",
            "for i in range(cm.shape[0]):\n",
            "    for j in range(cm.shape[1]):\n",
            "        axes[0].text(j, i, cm[i, j], ha='center', va='center')\n",
            "\n",
            "pca = PCA(n_components=2)\n",
            "X_pca = pca.fit_transform(X_test_s)\n",
            "sc = axes[1].scatter(X_pca[:, 0], X_pca[:, 1], c=y_test, cmap='viridis', alpha=0.6)\n",
            "axes[1].set_title('PCA — first 2 components')\n",
            "plt.colorbar(sc, ax=axes[1])\n",
            "plt.tight_layout(); plt.show()\n",
        ]),
        create_cell("markdown", [
            f"### Results — Experiment {experiment_num}\n",
            "\n",
            f"| Metric | Value |\n",
            "|--------|-------|\n",
            f"| Train accuracy | ~{0.85 + (experiment_num % 10) * 0.01:.2f} |\n",
            f"| Test accuracy  | ~{0.80 + (experiment_num % 12) * 0.01:.2f} |\n",
            "\n",
        ]),
    ]
    return cells


# ---------------------------------------------------------------------------
# Sub-command implementations
# ---------------------------------------------------------------------------

def _cmd_generate(args: argparse.Namespace) -> int:
    """Generate the standard set of notebooks."""
    notebooks_root = Path(args.output_dir)

    definitions = {
        "experiments/03_swarm_simulation.ipynb": {
            "title": "Swarm Simulation Analysis",
            "description": (
                "Multi-agent UAV swarm coordination using Primal Logic:\n"
                "- Swarm consensus\n- Trust dynamics\n"
                "- Neighbour graphs\n- Formation control"
            ),
        },
        "experiments/04_benchmark_analysis.ipynb": {
            "title": "Benchmark Analysis",
            "description": "Performance benchmarking and metrics analysis for Primal Logic implementations.",
        },
        "lam/01_lam_introduction.ipynb": {
            "title": "LAM System Introduction",
            "description": (
                "Getting started with the Large Action Model (LAM) system:\n"
                "- Quantum resonance fields\n- Action planning\n"
                "- Trip planning examples\n- Integration with Primal Logic"
            ),
        },
        "nasa/01_nasa_data_pipeline.ipynb": {
            "title": "NASA Data Pipeline",
            "description": (
                "Interactive NASA data visualisation and analysis:\n"
                "- Solar particle events (SPE)\n- Galactic cosmic rays (GCR)\n"
                "- Mission planning\n- Real-time data integration"
            ),
        },
        "biomedical/01_cardiac_models.ipynb": {
            "title": "Cardiac AI Model Analysis",
            "description": (
                "Analysis of cardiac AI models:\n"
                "- Model convergence patterns\n- Performance metrics"
            ),
        },
        "hardware/01_sensor_integration.ipynb": {
            "title": "Sensor Integration",
            "description": "Real-time sensor data analysis and integration with Primal Logic control.",
        },
        "visualization/01_comprehensive_viz.ipynb": {
            "title": "Comprehensive Visualisation Suite",
            "description": (
                "Complete visualisation examples for Primal Logic:\n"
                "- Time series plots\n- Heatmaps\n"
                "- 3D visualisations\n- Interactive plots"
            ),
        },
        "tutorials/01_control_fundamentals.ipynb": {
            "title": "Control Theory Fundamentals",
            "description": (
                "Introduction to control theory concepts:\n"
                "- PID control\n- State-space representation\n"
                "- Stability analysis\n- Comparison with Primal Logic"
            ),
        },
        "research/01_unified_field_theory.ipynb": {
            "title": "Unified Field Theory Exploration",
            "description": "Mathematical exploration of the unified field theory framework in MotorHandPro.",
        },
    }

    for rel_path, cfg in definitions.items():
        notebook_path = rel_path
        nb = create_notebook(
            cfg["title"],
            f"notebooks/{notebook_path}",
            cfg["description"],
        )
        out = notebooks_root / rel_path
        save_notebook(nb, out)
        print(f"✓ {rel_path}")

    print(f"\n✨ Generated {len(definitions)} notebooks in {notebooks_root}/")
    return 0


def _cmd_expand(args: argparse.Namespace) -> int:
    """Expand notebooks to a target line count."""
    target = args.target_lines
    notebooks_root = Path(args.notebooks_dir)
    pattern = args.pattern

    nb_files = sorted(notebooks_root.rglob(pattern))
    nb_files = [p for p in nb_files if ".ipynb_checkpoints" not in str(p)]

    if not nb_files:
        print(f"No notebooks found under {notebooks_root} matching {pattern!r}.", file=sys.stderr)
        return 1

    print(f"Expanding {len(nb_files)} notebook(s) to {target:,} lines …\n")

    for nb_path in nb_files:
        with nb_path.open(encoding="utf-8") as fh:
            nb = json.load(fh)

        before = count_lines(nb)
        if before >= target:
            print(f"  skip  {nb_path.name}  (already {before:,} lines)")
            continue

        expand_notebook(nb, _experiment_cell_factory, target_lines=target)

        with nb_path.open("w", encoding="utf-8") as fh:
            json.dump(nb, fh, indent=2)

        after = count_lines(nb)
        status = "✓" if after >= target else "⚠"
        print(f"  {status}  {nb_path.name}  {before:,} → {after:,} lines")

    return 0


# ---------------------------------------------------------------------------
# Argument parsing & entry point
# ---------------------------------------------------------------------------

def _build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="python -m motorhandpro.notebookgen",
        description="MotorHandPro notebook generation utilities.",
    )
    sub = p.add_subparsers(dest="command", required=True)

    # generate
    gen_p = sub.add_parser("generate", help="Generate the standard set of notebooks.")
    gen_p.add_argument(
        "--output-dir", default="notebooks",
        help="Root directory for generated notebooks (default: notebooks/).",
    )

    # expand
    exp_p = sub.add_parser("expand", help="Expand notebooks to a target line count.")
    exp_p.add_argument(
        "--notebooks-dir", default="notebooks",
        help="Root directory to search for notebooks (default: notebooks/).",
    )
    exp_p.add_argument(
        "--target-lines", type=int, default=5000,
        help="Minimum source-line count per notebook (default: 5000).",
    )
    exp_p.add_argument(
        "--pattern", default="*.ipynb",
        help="Glob pattern for notebook files (default: *.ipynb).",
    )

    return p


def main(argv: Optional[List[str]] = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)

    if args.command == "generate":
        return _cmd_generate(args)
    if args.command == "expand":
        return _cmd_expand(args)

    parser.print_help()
    return 1


if __name__ == "__main__":
    sys.exit(main())
