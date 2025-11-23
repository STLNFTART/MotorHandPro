"""
Network Simulation Cluster - PRIMAL LOGIC & ATAK Integration

This module provides distributed network simulation capabilities with:
- PRIMAL LOGIC CORE: Advanced consciousness and adaptive learning algorithms
- ATAK INTEGRATION: Real-time battlefield situational awareness via CoT messages
- Self-healing network capabilities with redundant routing
- Golden ratio and Fibonacci-based signal processing
- REAL-TIME DATA: FRED economic data, USGS terrain, Space-Track satellites

Usage:
    from network_simulation_cluster import EnhancedDistributedNetwork, RealTimeEnhancedNetwork

    # Basic simulation
    network = EnhancedDistributedNetwork(num_nodes=10000)
    network.simulate_network_activity(duration_seconds=1200)

    # With real-time data integration
    realtime_network = RealTimeEnhancedNetwork(
        num_nodes=10000,
        enable_economic_data=True,
        enable_terrain_data=True
    )
    realtime_network.simulate_network_activity(duration_seconds=1200)
"""

from .primal_atak_network_simulator import (
    EnhancedDistributedNetwork,
    PrimalLogicCore,
    ATAKReporter,
    get_current_time,
    KYIV_TZ,
    FIXED_DATE
)

# Real-time enhanced simulator (optional - requires data source credentials)
try:
    from .enhanced_realtime_simulator import RealTimeEnhancedNetwork
    REALTIME_AVAILABLE = True
except ImportError:
    REALTIME_AVAILABLE = False
    RealTimeEnhancedNetwork = None

__all__ = [
    'EnhancedDistributedNetwork',
    'PrimalLogicCore',
    'ATAKReporter',
    'RealTimeEnhancedNetwork',
    'get_current_time',
    'KYIV_TZ',
    'FIXED_DATE',
    'REALTIME_AVAILABLE'
]

__version__ = '1.1.0'
