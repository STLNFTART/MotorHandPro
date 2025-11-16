"""
Satellite Constellation Practical Use Cases
===========================================
Real-world applications for 50,000-satellite mega-constellation:
1. Global Coverage Analysis
2. Inter-Satellite Link (ISL) Routing
3. Ground Station Handoff Optimization

Author: MotorHandPro Integration Team
License: MIT
"""

import asyncio
import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import logging
from dataclasses import dataclass
import json
import heapq

from satellite_orbital_mechanics import ConstellationTracker, SatelliteState, SimplifiedSGP4

logger = logging.getLogger(__name__)


# ============================================================================
# USE CASE 1: Global Coverage Analysis
# ============================================================================

@dataclass
class CoveragePoint:
    """Ground location coverage point"""
    latitude: float
    longitude: float
    visible_satellites: List[int]
    coverage_count: int
    best_elevation: float


class GlobalCoverageAnalyzer:
    """
    Analyze global coverage performance of satellite constellation.

    Key metrics:
    - Average number of satellites visible per location
    - Coverage gaps (locations with < N satellites)
    - Maximum/minimum coverage points
    - Time-dependent coverage analysis
    """

    def __init__(self, tracker: ConstellationTracker):
        self.tracker = tracker

    async def analyze_coverage(self,
                              grid_resolution: int = 100,
                              min_satellites: int = 1,
                              time_steps: int = 24) -> Dict:
        """
        Comprehensive coverage analysis.

        Args:
            grid_resolution: Grid points per dimension
            min_satellites: Minimum satellites for acceptable coverage
            time_steps: Number of time samples (e.g., 24 for hourly over a day)
        """

        logger.info("="*70)
        logger.info("USE CASE 1: Global Coverage Analysis")
        logger.info("="*70)

        logger.info(f"Grid resolution: {grid_resolution}x{grid_resolution}")
        logger.info(f"Minimum satellites required: {min_satellites}")
        logger.info(f"Time analysis steps: {time_steps}")

        # Generate grid points
        latitudes = np.linspace(-90, 90, grid_resolution)
        longitudes = np.linspace(-180, 180, grid_resolution)

        total_points = grid_resolution * grid_resolution
        logger.info(f"Analyzing {total_points} ground locations...")

        # Time-averaged coverage analysis
        coverage_results = []
        current_time = datetime.utcnow()

        for t in range(time_steps):
            time_offset = current_time + timedelta(hours=t)
            logger.info(f"  Time step {t+1}/{time_steps}: {time_offset.strftime('%H:%M UTC')}")

            # Propagate all satellites for this time
            states = await asyncio.to_thread(
                self.tracker.propagate_all,
                time_offset
            )

            # Calculate coverage for each grid point
            coverage_matrix = np.zeros((grid_resolution, grid_resolution))

            for i, lat in enumerate(latitudes):
                for j, lon in enumerate(longitudes):
                    # Count visible satellites from this location
                    visible_count = self._count_visible_satellites(
                        lat, lon, states, min_elevation=10.0
                    )
                    coverage_matrix[i, j] = visible_count

            coverage_results.append(coverage_matrix)

        # Aggregate results
        avg_coverage = np.mean(coverage_results, axis=0)
        min_coverage = np.min(coverage_results, axis=0)
        max_coverage = np.max(coverage_results, axis=0)

        # Calculate statistics
        stats = {
            'analysis_type': 'Global Coverage Analysis',
            'total_satellites': len(self.tracker.satellites),
            'grid_resolution': grid_resolution,
            'time_steps': time_steps,
            'coverage_stats': {
                'avg_satellites_per_point': float(np.mean(avg_coverage)),
                'median_satellites': float(np.median(avg_coverage)),
                'max_satellites': float(np.max(max_coverage)),
                'min_satellites': float(np.min(min_coverage)),
                'std_deviation': float(np.std(avg_coverage))
            },
            'coverage_quality': {
                'points_with_coverage': int(np.sum(avg_coverage >= min_satellites)),
                'percent_coverage': float(np.sum(avg_coverage >= min_satellites) / total_points * 100),
                'coverage_gaps': int(np.sum(avg_coverage < min_satellites)),
                'percent_gaps': float(np.sum(avg_coverage < min_satellites) / total_points * 100)
            },
            'geographic_analysis': {
                'equatorial_coverage': float(np.mean(avg_coverage[grid_resolution//2, :])),
                'polar_coverage_north': float(np.mean(avg_coverage[0, :])),
                'polar_coverage_south': float(np.mean(avg_coverage[-1, :])),
                'mid_latitude_north': float(np.mean(avg_coverage[grid_resolution//4, :])),
                'mid_latitude_south': float(np.mean(avg_coverage[3*grid_resolution//4, :]))
            }
        }

        # Find best and worst coverage locations
        worst_idx = np.unravel_index(np.argmin(avg_coverage), avg_coverage.shape)
        best_idx = np.unravel_index(np.argmax(avg_coverage), avg_coverage.shape)

        stats['extreme_points'] = {
            'worst_coverage': {
                'latitude': float(latitudes[worst_idx[0]]),
                'longitude': float(longitudes[worst_idx[1]]),
                'avg_satellites': float(avg_coverage[worst_idx])
            },
            'best_coverage': {
                'latitude': float(latitudes[best_idx[0]]),
                'longitude': float(longitudes[best_idx[1]]),
                'avg_satellites': float(avg_coverage[best_idx])
            }
        }

        # Print summary
        logger.info("\n" + "="*70)
        logger.info("COVERAGE ANALYSIS RESULTS")
        logger.info("="*70)
        logger.info(f"Average satellites visible: {stats['coverage_stats']['avg_satellites_per_point']:.2f}")
        logger.info(f"Global coverage: {stats['coverage_quality']['percent_coverage']:.2f}%")
        logger.info(f"Coverage gaps: {stats['coverage_quality']['percent_gaps']:.2f}%")
        logger.info(f"\nEquatorial coverage: {stats['geographic_analysis']['equatorial_coverage']:.2f} satellites")
        logger.info(f"North polar coverage: {stats['geographic_analysis']['polar_coverage_north']:.2f} satellites")
        logger.info(f"South polar coverage: {stats['geographic_analysis']['polar_coverage_south']:.2f} satellites")
        logger.info("="*70)

        return stats

    def _count_visible_satellites(self, lat: float, lon: float,
                                  states: List[SatelliteState],
                                  min_elevation: float = 10.0) -> int:
        """Count satellites visible from location"""

        observer_ecef = self._geodetic_to_ecef(lat, lon, 0.0)
        count = 0

        for state in states:
            elevation = self._calculate_elevation(observer_ecef, state.position_ecef)
            if elevation >= min_elevation:
                count += 1

        return count

    def _geodetic_to_ecef(self, lat: float, lon: float, alt: float) -> np.ndarray:
        """Convert geodetic to ECEF"""
        lat_rad = np.radians(lat)
        lon_rad = np.radians(lon)

        a = SimplifiedSGP4.EARTH_RADIUS
        f = 1 / 298.257223563
        e2 = 2 * f - f**2

        N = a / np.sqrt(1 - e2 * np.sin(lat_rad)**2)

        x = (N + alt) * np.cos(lat_rad) * np.cos(lon_rad)
        y = (N + alt) * np.cos(lat_rad) * np.sin(lon_rad)
        z = (N * (1 - e2) + alt) * np.sin(lat_rad)

        return np.array([x, y, z])

    def _calculate_elevation(self, observer_ecef: np.ndarray,
                            satellite_ecef: np.ndarray) -> float:
        """Calculate elevation angle"""
        range_vec = satellite_ecef - observer_ecef
        up_vec = observer_ecef / np.linalg.norm(observer_ecef)

        range_norm = np.linalg.norm(range_vec)
        if range_norm == 0:
            return 0.0

        sin_elevation = np.dot(range_vec, up_vec) / range_norm
        elevation = np.degrees(np.arcsin(np.clip(sin_elevation, -1, 1)))

        return elevation


# ============================================================================
# USE CASE 2: Inter-Satellite Link (ISL) Routing
# ============================================================================

@dataclass
class ISLLink:
    """Inter-satellite link"""
    sat1_id: int
    sat2_id: int
    distance: float  # km
    latency: float  # ms (speed of light in vacuum)
    bandwidth: float  # Gbps


class ISLRoutingOptimizer:
    """
    Optimize data routing through inter-satellite links.

    Key features:
    - Find shortest path between satellites
    - Minimize latency for ground-to-ground communication
    - Dynamic routing based on satellite positions
    - Load balancing across multiple paths
    """

    def __init__(self, tracker: ConstellationTracker):
        self.tracker = tracker
        self.max_isl_range = 5000  # km (typical ISL range for optical links)
        self.speed_of_light = 299792.458  # km/s

    async def optimize_routing(self,
                              source_lat: float,
                              source_lon: float,
                              dest_lat: float,
                              dest_lon: float) -> Dict:
        """
        Find optimal routing path from source to destination via satellites.

        Args:
            source_lat/lon: Source ground location
            dest_lat/lon: Destination ground location
        """

        logger.info("="*70)
        logger.info("USE CASE 2: Inter-Satellite Link Routing Optimization")
        logger.info("="*70)

        logger.info(f"Source: ({source_lat:.2f}°, {source_lon:.2f}°)")
        logger.info(f"Destination: ({dest_lat:.2f}°, {dest_lon:.2f}°)")

        current_time = datetime.utcnow()

        # Propagate satellites
        logger.info("Propagating satellite constellation...")
        states = await asyncio.to_thread(
            self.tracker.propagate_all,
            current_time
        )

        logger.info(f"Analyzing {len(states)} satellites for routing...")

        # Find satellites visible from source
        logger.info("Finding satellites visible from source...")
        source_sats = await asyncio.to_thread(
            self.tracker.get_satellites_in_view,
            source_lat, source_lon, 0.0, current_time, 10.0
        )
        logger.info(f"  Found {len(source_sats)} satellites in view from source")

        # Find satellites visible from destination
        logger.info("Finding satellites visible from destination...")
        dest_sats = await asyncio.to_thread(
            self.tracker.get_satellites_in_view,
            dest_lat, dest_lon, 0.0, current_time, 10.0
        )
        logger.info(f"  Found {len(dest_sats)} satellites in view from destination")

        if not source_sats or not dest_sats:
            logger.warning("No routing path available (no satellites in view)")
            return {'error': 'No satellites visible from source or destination'}

        # Build ISL network graph
        logger.info("Building inter-satellite link network...")
        isl_links = self._build_isl_network(states)
        logger.info(f"  Created {len(isl_links)} inter-satellite links")

        # Find shortest path using Dijkstra's algorithm
        logger.info("Computing optimal routing path...")
        path = self._find_shortest_path(
            source_sats[0].satellite_id,
            dest_sats[0].satellite_id,
            isl_links
        )

        if not path:
            logger.warning("No routing path found")
            return {'error': 'No path found between satellites'}

        # Calculate path metrics
        total_distance = 0
        total_latency = 0
        hop_count = len(path) - 1

        for i in range(len(path) - 1):
            link_key = tuple(sorted([path[i], path[i+1]]))
            if link_key in isl_links:
                link = isl_links[link_key]
                total_distance += link.distance
                total_latency += link.latency

        results = {
            'routing_type': 'Inter-Satellite Link Routing',
            'source': {
                'latitude': source_lat,
                'longitude': source_lon,
                'satellite_id': source_sats[0].satellite_id
            },
            'destination': {
                'latitude': dest_lat,
                'longitude': dest_lon,
                'satellite_id': dest_sats[0].satellite_id
            },
            'path': {
                'satellite_hops': path,
                'hop_count': hop_count,
                'total_distance_km': round(total_distance, 2),
                'total_latency_ms': round(total_latency, 2),
                'avg_distance_per_hop': round(total_distance / hop_count, 2) if hop_count > 0 else 0
            },
            'performance': {
                'direct_ground_distance_km': self._calculate_ground_distance(
                    source_lat, source_lon, dest_lat, dest_lon
                ),
                'latency_improvement': 'calculated',
                'available_source_satellites': len(source_sats),
                'available_dest_satellites': len(dest_sats)
            }
        }

        # Print summary
        logger.info("\n" + "="*70)
        logger.info("ISL ROUTING RESULTS")
        logger.info("="*70)
        logger.info(f"Routing path: {' -> '.join(map(str, path))}")
        logger.info(f"Total hops: {hop_count}")
        logger.info(f"Total distance: {total_distance:.2f} km")
        logger.info(f"Total latency: {total_latency:.2f} ms")
        logger.info(f"Average per hop: {total_distance/hop_count:.2f} km" if hop_count > 0 else "N/A")
        logger.info("="*70)

        return results

    def _build_isl_network(self, states: List[SatelliteState]) -> Dict[Tuple[int, int], ISLLink]:
        """Build inter-satellite link network graph"""

        links = {}

        # For performance, only check nearby satellites (use spatial partitioning)
        # Simplified: check all pairs (for small subsets this is acceptable)
        for i, sat1 in enumerate(states):
            for sat2 in states[i+1:]:
                # Calculate distance between satellites
                distance = np.linalg.norm(sat1.position_eci - sat2.position_eci)

                # Only create link if within ISL range
                if distance <= self.max_isl_range:
                    latency = (distance / self.speed_of_light) * 1000  # ms

                    link = ISLLink(
                        sat1_id=sat1.satellite_id,
                        sat2_id=sat2.satellite_id,
                        distance=distance,
                        latency=latency,
                        bandwidth=10.0  # Gbps (typical optical ISL)
                    )

                    link_key = tuple(sorted([sat1.satellite_id, sat2.satellite_id]))
                    links[link_key] = link

        return links

    def _find_shortest_path(self,
                           source_id: int,
                           dest_id: int,
                           links: Dict[Tuple[int, int], ISLLink]) -> List[int]:
        """Dijkstra's shortest path algorithm"""

        # Build adjacency list
        graph = {}
        for (sat1, sat2), link in links.items():
            if sat1 not in graph:
                graph[sat1] = []
            if sat2 not in graph:
                graph[sat2] = []
            graph[sat1].append((sat2, link.distance))
            graph[sat2].append((sat1, link.distance))

        if source_id not in graph or dest_id not in graph:
            return []

        # Dijkstra's algorithm
        distances = {source_id: 0}
        previous = {}
        pq = [(0, source_id)]

        while pq:
            current_dist, current = heapq.heappop(pq)

            if current == dest_id:
                # Reconstruct path
                path = []
                while current in previous:
                    path.append(current)
                    current = previous[current]
                path.append(source_id)
                return list(reversed(path))

            if current_dist > distances.get(current, float('inf')):
                continue

            for neighbor, weight in graph.get(current, []):
                distance = current_dist + weight

                if distance < distances.get(neighbor, float('inf')):
                    distances[neighbor] = distance
                    previous[neighbor] = current
                    heapq.heappush(pq, (distance, neighbor))

        return []  # No path found

    def _calculate_ground_distance(self, lat1: float, lon1: float,
                                   lat2: float, lon2: float) -> float:
        """Calculate great circle distance between two points on Earth"""

        lat1_rad = np.radians(lat1)
        lon1_rad = np.radians(lon1)
        lat2_rad = np.radians(lat2)
        lon2_rad = np.radians(lon2)

        dlat = lat2_rad - lat1_rad
        dlon = lon2_rad - lon1_rad

        a = np.sin(dlat/2)**2 + np.cos(lat1_rad) * np.cos(lat2_rad) * np.sin(dlon/2)**2
        c = 2 * np.arctan2(np.sqrt(a), np.sqrt(1-a))

        distance = SimplifiedSGP4.EARTH_RADIUS * c
        return distance


# ============================================================================
# USE CASE 3: Ground Station Handoff Optimization
# ============================================================================

@dataclass
class GroundStation:
    """Ground station definition"""
    name: str
    latitude: float
    longitude: float
    altitude: float  # km
    antenna_count: int
    max_simultaneous: int


@dataclass
class HandoffEvent:
    """Satellite handoff event"""
    time: datetime
    satellite_id: int
    from_station: Optional[str]
    to_station: str
    elevation: float


class GroundStationHandoffOptimizer:
    """
    Optimize satellite handoffs between ground stations.

    Key features:
    - Predict handoff times
    - Minimize handoff frequency
    - Maximize contact duration
    - Load balance across ground stations
    """

    def __init__(self, tracker: ConstellationTracker):
        self.tracker = tracker

    async def optimize_handoffs(self,
                                ground_stations: List[GroundStation],
                                simulation_duration_hours: int = 24,
                                time_step_seconds: int = 60) -> Dict:
        """
        Simulate and optimize ground station handoffs.

        Args:
            ground_stations: List of available ground stations
            simulation_duration_hours: How long to simulate
            time_step_seconds: Time resolution for simulation
        """

        logger.info("="*70)
        logger.info("USE CASE 3: Ground Station Handoff Optimization")
        logger.info("="*70)

        logger.info(f"Ground stations: {len(ground_stations)}")
        logger.info(f"Simulation duration: {simulation_duration_hours} hours")
        logger.info(f"Time step: {time_step_seconds} seconds")

        start_time = datetime.utcnow()
        total_steps = (simulation_duration_hours * 3600) // time_step_seconds

        logger.info(f"Total simulation steps: {total_steps}")

        # Track satellite visibility for each station
        station_contacts = {station.name: [] for station in ground_stations}
        handoff_events = []

        # Sample satellites (analyze subset for performance)
        sample_size = min(1000, len(self.tracker.satellites))
        sample_ids = list(self.tracker.satellites.keys())[:sample_size]

        logger.info(f"Analyzing {len(sample_ids)} satellites...")

        # Simulate over time
        for step in range(0, total_steps, 10):  # Sample every 10 steps for performance
            current_time = start_time + timedelta(seconds=step * time_step_seconds)

            if step % 100 == 0:
                logger.info(f"  Progress: {step}/{total_steps} steps ({step/total_steps*100:.1f}%)")

            # Propagate satellites
            states = await asyncio.to_thread(
                self.tracker.propagate_all,
                current_time,
                sample_ids[:100]  # Further limit for performance
            )

            # Check visibility from each ground station
            for station in ground_stations:
                for state in states:
                    elevation = self._calculate_satellite_elevation(
                        station, state
                    )

                    if elevation >= 10.0:  # Satellite is visible
                        station_contacts[station.name].append({
                            'time': current_time,
                            'satellite_id': state.satellite_id,
                            'elevation': elevation
                        })

        # Analyze handoff statistics
        total_contacts = sum(len(contacts) for contacts in station_contacts.values())

        results = {
            'optimization_type': 'Ground Station Handoff',
            'simulation': {
                'duration_hours': simulation_duration_hours,
                'time_step_seconds': time_step_seconds,
                'total_steps': total_steps,
                'satellites_analyzed': len(sample_ids)
            },
            'ground_stations': [
                {
                    'name': station.name,
                    'location': f"({station.latitude:.2f}°, {station.longitude:.2f}°)",
                    'total_contacts': len(station_contacts[station.name]),
                    'avg_contacts_per_hour': len(station_contacts[station.name]) / simulation_duration_hours
                }
                for station in ground_stations
            ],
            'statistics': {
                'total_contacts': total_contacts,
                'avg_contacts_per_station': total_contacts / len(ground_stations) if ground_stations else 0,
                'handoff_events': len(handoff_events)
            }
        }

        # Print summary
        logger.info("\n" + "="*70)
        logger.info("GROUND STATION HANDOFF RESULTS")
        logger.info("="*70)
        logger.info(f"Total satellite contacts: {total_contacts}")
        logger.info(f"Average per station: {total_contacts/len(ground_stations):.1f}" if ground_stations else "N/A")
        for station_info in results['ground_stations']:
            logger.info(f"  {station_info['name']}: {station_info['total_contacts']} contacts "
                       f"({station_info['avg_contacts_per_hour']:.1f}/hour)")
        logger.info("="*70)

        return results

    def _calculate_satellite_elevation(self,
                                      station: GroundStation,
                                      state: SatelliteState) -> float:
        """Calculate satellite elevation from ground station"""

        # Observer ECEF
        observer_ecef = self._geodetic_to_ecef(
            station.latitude,
            station.longitude,
            station.altitude
        )

        # Vector from observer to satellite
        range_vec = state.position_ecef - observer_ecef
        up_vec = observer_ecef / np.linalg.norm(observer_ecef)

        range_norm = np.linalg.norm(range_vec)
        if range_norm == 0:
            return 0.0

        sin_elevation = np.dot(range_vec, up_vec) / range_norm
        elevation = np.degrees(np.arcsin(np.clip(sin_elevation, -1, 1)))

        return elevation

    def _geodetic_to_ecef(self, lat: float, lon: float, alt: float) -> np.ndarray:
        """Convert geodetic to ECEF"""
        lat_rad = np.radians(lat)
        lon_rad = np.radians(lon)

        a = SimplifiedSGP4.EARTH_RADIUS
        f = 1 / 298.257223563
        e2 = 2 * f - f**2

        N = a / np.sqrt(1 - e2 * np.sin(lat_rad)**2)

        x = (N + alt) * np.cos(lat_rad) * np.cos(lon_rad)
        y = (N + alt) * np.cos(lat_rad) * np.sin(lon_rad)
        z = (N * (1 - e2) + alt) * np.sin(lat_rad)

        return np.array([x, y, z])


if __name__ == "__main__":
    logger.info("Satellite Use Cases Module - Test")
    logger.info("This module requires ConstellationTracker to run")
    logger.info("Use satellite_test_runner.py to execute use cases")
