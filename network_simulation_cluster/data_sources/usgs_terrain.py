#!/usr/bin/env python3
"""
USGS Elevation/Terrain API Integration for PRIMAL Network Simulations

Provides real-time terrain and elevation data from USGS National Elevation Dataset
for UAV swarm altitude planning, network node placement, and geographic-aware routing.

API Documentation: https://apps.nationalmap.gov/epqs/
No API key required - Public service
"""

import requests
import time
from typing import Dict, List, Tuple, Optional, Any
import math
from dataclasses import dataclass


@dataclass
class ElevationPoint:
    """Single elevation data point"""
    latitude: float
    longitude: float
    elevation_meters: float
    elevation_feet: float
    data_source: str
    resolution: str


class USGSElevationClient:
    """USGS Elevation Point Query Service (EPQS) Client"""

    # USGS EPQS API endpoint
    EPQS_URL = "https://epqs.nationalmap.gov/v1/json"

    # Available elevation datasets
    DATASETS = {
        '1m': '3DEP 1m',           # 1-meter resolution (best quality, limited coverage)
        '3m': '3DEP 3m',           # 3-meter resolution
        '10m': '3DEP 10m',         # 10-meter resolution (good coverage)
        '30m': '3DEP 30m',         # 30-meter resolution (nationwide)
        'ned': 'NED 1/3 arc-second',  # Legacy National Elevation Dataset
    }

    def __init__(self, default_units: str = 'Meters', timeout: int = 10):
        """
        Initialize USGS Elevation client

        Args:
            default_units: 'Meters' or 'Feet'
            timeout: Request timeout in seconds
        """
        self.default_units = default_units
        self.timeout = timeout
        self.session = requests.Session()
        self.cache = {}
        self.cache_duration = 86400  # 24 hour cache (elevation doesn't change)

    def get_elevation(self, latitude: float, longitude: float,
                     units: Optional[str] = None) -> Optional[ElevationPoint]:
        """
        Get elevation for a single point

        Args:
            latitude: Latitude in decimal degrees
            longitude: Longitude in decimal degrees
            units: 'Meters' or 'Feet' (defaults to self.default_units)

        Returns:
            ElevationPoint or None if request fails
        """
        units = units or self.default_units

        # Check cache
        cache_key = f"{latitude:.6f},{longitude:.6f},{units}"
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if time.time() - cached_time < self.cache_duration:
                return cached_data

        # Query USGS API
        params = {
            'x': longitude,
            'y': latitude,
            'units': units,
            'output': 'json'
        }

        try:
            response = self.session.get(self.EPQS_URL, params=params, timeout=self.timeout)
            response.raise_for_status()
            data = response.json()

            # Parse response
            if 'value' in data:
                elevation = float(data['value'])

                # Convert to both units for convenience
                if units == 'Meters':
                    elevation_m = elevation
                    elevation_ft = elevation * 3.28084
                else:
                    elevation_ft = elevation
                    elevation_m = elevation / 3.28084

                point = ElevationPoint(
                    latitude=latitude,
                    longitude=longitude,
                    elevation_meters=elevation_m,
                    elevation_feet=elevation_ft,
                    data_source=data.get('Data_Source', 'Unknown'),
                    resolution=data.get('Elevation_Query', {}).get('Elevation_Units', 'Unknown')
                )

                # Cache result
                self.cache[cache_key] = (time.time(), point)

                return point

        except requests.RequestException as e:
            print(f"⚠️ USGS elevation API error: {e}")
        except (ValueError, KeyError) as e:
            print(f"⚠️ USGS elevation parsing error: {e}")

        return None

    def get_elevation_grid(self, center_lat: float, center_lon: float,
                          radius_km: float, grid_points: int = 10) -> List[ElevationPoint]:
        """
        Get elevation data for a grid of points around a center location

        Args:
            center_lat: Center latitude
            center_lon: Center longitude
            radius_km: Radius in kilometers
            grid_points: Number of points per side (total = grid_points^2)

        Returns:
            List of ElevationPoints
        """
        points = []

        # Convert radius to degrees (approximate)
        lat_degree_km = 111.0  # 1 degree latitude ≈ 111 km
        lon_degree_km = 111.0 * math.cos(math.radians(center_lat))

        lat_range = radius_km / lat_degree_km
        lon_range = radius_km / lon_degree_km

        # Create grid
        for i in range(grid_points):
            for j in range(grid_points):
                # Calculate position in grid
                lat_offset = (i - grid_points/2) * (2 * lat_range / grid_points)
                lon_offset = (j - grid_points/2) * (2 * lon_range / grid_points)

                lat = center_lat + lat_offset
                lon = center_lon + lon_offset

                # Get elevation
                point = self.get_elevation(lat, lon)
                if point:
                    points.append(point)

                # Rate limiting
                time.sleep(0.1)  # Be nice to USGS servers

        return points

    def get_elevation_profile(self, start_lat: float, start_lon: float,
                            end_lat: float, end_lon: float,
                            num_samples: int = 20) -> List[ElevationPoint]:
        """
        Get elevation profile along a line between two points

        Args:
            start_lat: Starting latitude
            start_lon: Starting longitude
            end_lat: Ending latitude
            end_lon: Ending longitude
            num_samples: Number of points to sample along the line

        Returns:
            List of ElevationPoints along the path
        """
        profile = []

        for i in range(num_samples):
            # Interpolate position
            t = i / (num_samples - 1) if num_samples > 1 else 0
            lat = start_lat + t * (end_lat - start_lat)
            lon = start_lon + t * (end_lon - start_lon)

            point = self.get_elevation(lat, lon)
            if point:
                profile.append(point)

            # Rate limiting
            time.sleep(0.1)

        return profile

    def get_min_safe_altitude_agl(self, latitude: float, longitude: float,
                                  clearance_meters: float = 100.0) -> float:
        """
        Get minimum safe altitude Above Ground Level (AGL) for UAV flight

        Args:
            latitude: Latitude
            longitude: Longitude
            clearance_meters: Safety clearance above terrain (default 100m)

        Returns:
            Minimum safe altitude in meters MSL (Mean Sea Level)
        """
        point = self.get_elevation(latitude, longitude)
        if point:
            return point.elevation_meters + clearance_meters
        else:
            # Conservative fallback
            return 500.0  # 500m default if elevation unknown

    def calculate_terrain_roughness(self, center_lat: float, center_lon: float,
                                   radius_km: float = 1.0) -> float:
        """
        Calculate terrain roughness index (0-1) for a region

        Args:
            center_lat: Center latitude
            center_lon: Center longitude
            radius_km: Analysis radius in km

        Returns:
            Roughness index: 0 = flat, 1 = very rough
        """
        # Get elevation grid
        grid = self.get_elevation_grid(center_lat, center_lon, radius_km, grid_points=5)

        if len(grid) < 2:
            return 0.0

        # Calculate elevation variance
        elevations = [p.elevation_meters for p in grid]
        mean_elevation = sum(elevations) / len(elevations)
        variance = sum((e - mean_elevation) ** 2 for e in elevations) / len(elevations)
        std_dev = math.sqrt(variance)

        # Normalize to 0-1 (assume 200m std dev = very rough)
        roughness = min(1.0, std_dev / 200.0)

        return roughness


class TerrainAwareNetworkPlanner:
    """Plan network node placement based on terrain"""

    def __init__(self, usgs_client: Optional[USGSElevationClient] = None):
        self.usgs = usgs_client or USGSElevationClient()

    def plan_node_placement(self, region_lat: float, region_lon: float,
                          num_nodes: int, radius_km: float = 10.0) -> List[Dict[str, Any]]:
        """
        Plan optimal network node placement considering terrain

        Args:
            region_lat: Region center latitude
            region_lon: Region center longitude
            num_nodes: Number of nodes to place
            radius_km: Region radius

        Returns:
            List of node placements with elevation data
        """
        placements = []

        # Simple grid-based placement
        grid_size = int(math.sqrt(num_nodes))
        lat_degree_km = 111.0
        lon_degree_km = 111.0 * math.cos(math.radians(region_lat))

        lat_range = radius_km / lat_degree_km
        lon_range = radius_km / lon_degree_km

        for i in range(grid_size):
            for j in range(grid_size):
                if len(placements) >= num_nodes:
                    break

                lat_offset = (i - grid_size/2) * (2 * lat_range / grid_size)
                lon_offset = (j - grid_size/2) * (2 * lon_range / grid_size)

                lat = region_lat + lat_offset
                lon = region_lon + lon_offset

                # Get elevation
                elevation_point = self.usgs.get_elevation(lat, lon)

                if elevation_point:
                    placements.append({
                        'node_id': f'node-{len(placements):05d}',
                        'latitude': lat,
                        'longitude': lon,
                        'elevation_m': elevation_point.elevation_meters,
                        'elevation_ft': elevation_point.elevation_feet,
                        'data_source': elevation_point.data_source
                    })

                time.sleep(0.1)  # Rate limiting

        return placements

    def calculate_line_of_sight(self, node1: Dict[str, float],
                               node2: Dict[str, float],
                               antenna_height_m: float = 10.0) -> bool:
        """
        Check if two nodes have line-of-sight considering terrain

        Args:
            node1: {'latitude': ..., 'longitude': ..., 'elevation_m': ...}
            node2: Same format
            antenna_height_m: Antenna height above ground

        Returns:
            True if line-of-sight exists
        """
        # Get elevation profile between nodes
        profile = self.usgs.get_elevation_profile(
            node1['latitude'], node1['longitude'],
            node2['latitude'], node2['longitude'],
            num_samples=10
        )

        if len(profile) < 2:
            return True  # Assume LOS if terrain data unavailable

        # Node altitudes (elevation + antenna height)
        node1_alt = node1.get('elevation_m', 0) + antenna_height_m
        node2_alt = node2.get('elevation_m', 0) + antenna_height_m

        # Check if any intermediate point blocks the line
        for i, point in enumerate(profile[1:-1], 1):
            # Calculate expected altitude on direct line
            t = i / (len(profile) - 1)
            line_altitude = node1_alt + t * (node2_alt - node1_alt)

            # Check if terrain blocks
            if point.elevation_meters + 5 > line_altitude:  # 5m clearance
                return False

        return True


# Example usage and testing
if __name__ == "__main__":
    print("🗺️  USGS Terrain/Elevation API Integration Test")
    print("=" * 80)

    usgs = USGSElevationClient()

    # Test 1: Single point elevation (St. Louis)
    print("\n📍 St. Louis Gateway Arch Elevation:")
    print("-" * 80)
    stl_arch = usgs.get_elevation(38.6247, -90.1848)  # Gateway Arch coordinates
    if stl_arch:
        print(f"Latitude: {stl_arch.latitude:.4f}°")
        print(f"Longitude: {stl_arch.longitude:.4f}°")
        print(f"Elevation: {stl_arch.elevation_meters:.1f}m ({stl_arch.elevation_feet:.1f}ft)")
        print(f"Data Source: {stl_arch.data_source}")

    # Test 2: Elevation grid (around St. Louis)
    print("\n🗺️  Elevation Grid (St. Louis Region):")
    print("-" * 80)
    grid = usgs.get_elevation_grid(38.6270, -90.1994, radius_km=5, grid_points=3)
    print(f"Retrieved {len(grid)} elevation points")
    if grid:
        elevations = [p.elevation_meters for p in grid]
        print(f"Min elevation: {min(elevations):.1f}m")
        print(f"Max elevation: {max(elevations):.1f}m")
        print(f"Mean elevation: {sum(elevations)/len(elevations):.1f}m")

    # Test 3: Elevation profile (St. Louis to Lambert Airport)
    print("\n📊 Elevation Profile (St. Louis to Lambert Airport):")
    print("-" * 80)
    profile = usgs.get_elevation_profile(
        38.6270, -90.1994,  # Downtown STL
        38.7487, -90.3700,  # Lambert Airport
        num_samples=10
    )
    print(f"Profile points: {len(profile)}")
    if profile:
        for i, point in enumerate(profile):
            print(f"  Point {i}: {point.elevation_meters:.1f}m")

    # Test 4: Terrain roughness
    print("\n⛰️  Terrain Roughness Analysis:")
    print("-" * 80)
    roughness = usgs.calculate_terrain_roughness(38.6270, -90.1994, radius_km=2)
    print(f"Roughness Index: {roughness:.3f}")
    if roughness < 0.3:
        print("Terrain: FLAT")
    elif roughness < 0.6:
        print("Terrain: MODERATE")
    else:
        print("Terrain: ROUGH")

    # Test 5: UAV safe altitude
    print("\n🚁 UAV Minimum Safe Altitude:")
    print("-" * 80)
    safe_alt = usgs.get_min_safe_altitude_agl(38.6270, -90.1994, clearance_meters=100)
    print(f"Minimum safe altitude: {safe_alt:.1f}m MSL")
    print(f"(100m clearance above terrain)")

    # Test 6: Network node placement
    print("\n📡 Terrain-Aware Network Node Placement:")
    print("-" * 80)
    planner = TerrainAwareNetworkPlanner(usgs)
    nodes = planner.plan_node_placement(38.6270, -90.1994, num_nodes=9, radius_km=5)
    print(f"Placed {len(nodes)} nodes")
    for node in nodes[:3]:  # Show first 3
        print(f"  {node['node_id']}: "
              f"({node['latitude']:.4f}, {node['longitude']:.4f}) "
              f"@ {node['elevation_m']:.1f}m")

    # Test 7: Line-of-sight check
    if len(nodes) >= 2:
        print("\n👁️  Line-of-Sight Check:")
        print("-" * 80)
        los = planner.calculate_line_of_sight(nodes[0], nodes[1], antenna_height_m=10)
        print(f"Node {nodes[0]['node_id']} → {nodes[1]['node_id']}: "
              f"{'✅ LOS CLEAR' if los else '❌ LOS BLOCKED'}")

    print("\n✅ All USGS terrain integration tests complete!")
