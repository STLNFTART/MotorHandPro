"""
Network Simulation Cluster - PRIMAL LOGIC & ATAK Integration

This module provides distributed network simulation capabilities with:
- PRIMAL LOGIC CORE: Advanced consciousness and adaptive learning algorithms
- ATAK INTEGRATION: Real-time battlefield situational awareness via CoT messages
- Self-healing network capabilities with redundant routing
- Golden ratio and Fibonacci-based signal processing

Usage:
    from network_simulation_cluster import EnhancedDistributedNetwork, PrimalLogicCore, ATAKReporter

    network = EnhancedDistributedNetwork(num_nodes=10000)
    network.simulate_network_activity(duration_seconds=1200)
"""

from .primal_atak_network_simulator import (
    EnhancedDistributedNetwork,
    PrimalLogicCore,
    ATAKReporter,
    get_current_time,
    KYIV_TZ,
    FIXED_DATE
)

__all__ = [
    'EnhancedDistributedNetwork',
    'PrimalLogicCore',
    'ATAKReporter',
    'get_current_time',
    'KYIV_TZ',
    'FIXED_DATE'
]

__version__ = '1.0.0'
