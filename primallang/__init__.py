#!/usr/bin/env python3
"""
PrimalLang - Domain-Specific Language for Control Systems
With Donte & Lightfoot Constants Integration

Patent Pending: U.S. Provisional Patent Application No. 63/842,846
© 2025 Donte Lightfoot - The Phoney Express LLC
"""
from .compiler import compile_to_python, parse
from .runtime import (
    PrimalConstants,
    Vector384,
    Matrix384,
    MathPrimitives,
    MetaChecker,
    MetaError
)

__version__ = '0.1.0'
__author__ = 'Donte Lightfoot'
__all__ = [
    'compile_to_python',
    'parse',
    'PrimalConstants',
    'Vector384',
    'Matrix384',
    'MathPrimitives',
    'MetaChecker',
    'MetaError'
]
