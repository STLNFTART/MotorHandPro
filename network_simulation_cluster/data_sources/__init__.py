"""
Real-Time Data Source Integration for PRIMAL Network Simulations

This module provides integration with authoritative data sources:
- FRED: Federal Reserve Economic Data (St. Louis Fed)
- USGS: United States Geological Survey (Terrain/Elevation)
- Space-Track.org: Satellite Two-Line Element (TLE) data

Usage:
    from network_simulation_cluster.data_sources import FREDClient, USGSElevationClient, SpaceTrackClient

    # Economic data
    fred = FREDClient(api_key='your_key')
    threat = fred.get_economic_threat_score()

    # Terrain data
    usgs = USGSElevationClient()
    elevation = usgs.get_elevation(38.6270, -90.1994)

    # Satellite data
    spacetrack = SpaceTrackClient(username='user', password='pass')
    starlink = spacetrack.get_constellation('STARLINK')
"""

from .fred_integration import (
    FREDClient,
    FREDEnhancedPrimalLogic
)

from .usgs_terrain import (
    USGSElevationClient,
    ElevationPoint,
    TerrainAwareNetworkPlanner
)

from .spacetrack_satellites import (
    SpaceTrackClient,
    SatelliteTLE,
    SatelliteNetworkSimulator
)

__all__ = [
    # FRED Economic Data
    'FREDClient',
    'FREDEnhancedPrimalLogic',

    # USGS Terrain
    'USGSElevationClient',
    'ElevationPoint',
    'TerrainAwareNetworkPlanner',

    # Space-Track Satellites
    'SpaceTrackClient',
    'SatelliteTLE',
    'SatelliteNetworkSimulator',
]

__version__ = '1.0.0'
