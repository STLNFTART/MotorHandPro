"""
Digital Twin Framework for Space & Earth Observation
====================================================

A comprehensive framework for creating Digital Twins of space and Earth systems
with real-time data integration from multiple sources.

Quick Start:
    >>> from digital_twin_framework.core.digital_twin import DigitalTwinFramework
    >>> framework = DigitalTwinFramework()
    >>> # Create and configure your Digital Twins

Modules:
    - core: Digital Twin engine and base classes
    - connectors: Data source connectors
    - security: Encrypted data pipelines
    - sync_engine: Real-time synchronization

Version: 1.0.0
Author: PRIMAL Logic Integration Team
License: MIT
"""

__version__ = "1.0.0"
__author__ = "PRIMAL Logic Integration Team"
__license__ = "MIT"

# Core imports for convenience
from .core.digital_twin import (
    DigitalTwinFramework,
    DigitalTwinState,
    DataSourceType,
    DataQuality,
    DataPoint,
    DataConnector
)

__all__ = [
    'DigitalTwinFramework',
    'DigitalTwinState',
    'DataSourceType',
    'DataQuality',
    'DataPoint',
    'DataConnector'
]
