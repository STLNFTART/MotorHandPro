"""
LAM - Large Action Model Framework
Quantum-semantic intelligence for real-world task execution

Integrates Lightfoot and Donte constants from Primal Logic framework
"""

__version__ = "1.0.0"
__author__ = "Donte Lightfoot"
__license__ = "Patent Pending: U.S. Provisional Patent Application No. 63/842,846"

from pathlib import Path

# Package structure
LAM_ROOT = Path(__file__).parent
CORE_DIR = LAM_ROOT / "core"
ACTIONS_DIR = LAM_ROOT / "actions"
WIZARDS_DIR = LAM_ROOT / "wizards"
ASSISTANTS_DIR = LAM_ROOT / "assistants"
CONFIG_DIR = LAM_ROOT / "config"

__all__ = [
    "LAM_ROOT",
    "CORE_DIR",
    "ACTIONS_DIR",
    "WIZARDS_DIR",
    "ASSISTANTS_DIR",
    "CONFIG_DIR",
]
