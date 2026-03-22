# Development Guide

This document explains how to set up a local development environment and run
the various tools in this repository.

## Prerequisites

- Python 3.8 or newer
- `pip` (bundled with Python)
- Arduino IDE (for hardware development — see [Arduino setup](#arduino-setup))

## Quick start (Python)

```bash
# 1. Clone the repository (if you haven't already)
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# 2. Install the package in editable mode.
#    This makes all sub-packages importable without sys.path hacks.
pip install -e ".[dev]"

# 3. Run any script or module normally:
python run_experiments.py
python run_lam_interactive.py
python -m lam.lam_main

# LAM API server
pip install -e ".[api]"
python -m lam.api.api_server
```

> **Google Colab users:** Add the following cell at the top of any notebook
> instead of the old `sys.path` manipulation:
> ```python
> !pip install -q git+https://github.com/STLNFTART/MotorHandPro.git
> ```
> After that cell, all imports (`from lam import ...`, `from motorhandpro import ...`)
> will work without path hacks.

## Package structure

```
MotorHandPro/
├── pyproject.toml             # package metadata & console-script entry points
├── motorhandpro/              # top-level installable package
│   └── notebookgen/           # shared notebook-generation library
│       ├── __init__.py
│       ├── _core.py           # count_lines, create_cell, create_notebook, …
│       └── __main__.py        # CLI: `python -m motorhandpro.notebookgen`
├── lam/                       # Large Action Model framework (also a package)
│   ├── lam_main.py            # LAM entry point (uses absolute imports)
│   └── api/
│       └── api_server.py      # FastAPI REST server
├── run_experiments.py         # thin CLI wrapper
├── run_lam_interactive.py     # thin CLI wrapper
├── lam_quick_demo.py          # thin CLI wrapper
├── view_experiment_results.py # thin CLI wrapper
├── run_full_prosthetics_pipeline.py
└── hardware/
    └── prosthetic_arduino/
        └── prosthetic_hand.ino
```

## Notebook generation

All notebook-generation logic lives in `motorhandpro/notebookgen/`.
The top-level scripts (`generate_notebooks.py`, `generate_more_notebooks.py`,
`generate_comprehensive_notebooks.py`, `generate_5000_line_notebooks.py`,
`expand_notebooks_to_5000_lines.py`, `build_massive_notebooks.py`) are thin
wrappers that call the shared library.

```bash
# Generate the standard set of notebooks
python -m motorhandpro.notebookgen generate

# Expand all existing notebooks to 5000+ source lines
python -m motorhandpro.notebookgen expand --target-lines 5000

# Expand notebooks in a custom directory
python -m motorhandpro.notebookgen expand --notebooks-dir path/to/notebooks --target-lines 3000
```

## Running tests

```bash
pytest tests/ lam/tests/
```

## Arduino setup

The Arduino sketch (`hardware/prosthetic_arduino/prosthetic_hand.ino`) uses
**ArduinoJson** for robust JSON parsing.

1. Open the **Arduino Library Manager** (`Sketch → Include Library → Manage Libraries…`).
2. Search for **ArduinoJson** (by Benoît Blanchon) and install version **7.x**.
3. Open `hardware/prosthetic_arduino/prosthetic_hand.ino` in the Arduino IDE.
4. Select your board (`Tools → Board → Arduino Mega 2560`) and port.
5. Click **Upload**.

### JSON command reference

All commands sent over serial (115200 baud) must be terminated with a newline
(`\n`). Fields not listed are ignored.

| Command | JSON | Response |
|---------|------|----------|
| Start EMG stream | `{"cmd":"start_stream"}` | `{"status":"streaming"}` |
| Stop EMG stream | `{"cmd":"stop_stream"}` | `{"status":"stopped"}` |
| Execute gesture | `{"cmd":"gesture","type":"<name>"}` | `{"status":"gesture_executed","gesture":"<name>"}` |
| Set radiation dose | `{"cmd":"set_radiation","dose":<int>}` | `{"status":"radiation_set","dose":<int>}` |
| Get EMG buffer | `{"cmd":"get_buffer"}` | `{"buffer":{"ch0":[…],…}}` |
| Calibrate sensors | `{"cmd":"calibrate"}` | `{"calibration":[…]}` |
| Get device status | `{"cmd":"status"}` | `{"status":{…}}` |

Valid gesture names: `rest`, `hand_close`, `pinch_grip`, `power_grip`,
`pointing`, `peace_sign`.

Error responses use the form `{"status":"error","error":"<message>"}`.

## Import guidelines

- **Library code** (anything inside `lam/` or `motorhandpro/`) must use
  absolute imports and must **not** manipulate `sys.path`.
- **CLI entry-point scripts** at the repo root (e.g. `run_experiments.py`)
  may add the repo root to `sys.path` using `Path(__file__).parent` *only*
  to support running without `pip install -e .`.  No hard-coded absolute
  paths (e.g. `/home/user/…`) are permitted.
- When adding a new console script, register it in `pyproject.toml` under
  `[project.scripts]` rather than adding more path manipulation.
