#!/usr/bin/env python3
"""
Space-Track.org Satellite TLE Integration for PRIMAL Network Simulations

Provides real-time satellite Two-Line Element (TLE) data from Space-Track.org
for satellite constellation simulation, orbital mechanics, and space-based networks.

API Documentation: https://www.space-track.org/documentation
Registration required (free): https://www.space-track.org/auth/createAccount
"""

import os
import requests
import time
import json
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime, timedelta
from dataclasses import dataclass
import math


@dataclass
class SatelliteTLE:
    """Two-Line Element satellite orbital data"""
    norad_id: int
    name: str
    tle_line1: str
    tle_line2: str
    epoch: datetime
    inclination: float  # degrees
    eccentricity: float
    mean_motion: float  # revolutions per day
    apogee: float  # km
    perigee: float  # km
    period: float  # minutes


class SpaceTrackClient:
    """Space-Track.org API Client for satellite TLE data"""

    BASE_URL = "https://www.space-track.org"
    LOGIN_URL = f"{BASE_URL}/ajaxauth/login"
    TLE_URL = f"{BASE_URL}/basicspacedata/query/class/gp/format/json"

    # Common satellite constellations
    CONSTELLATIONS = {
        'STARLINK': {'name_pattern': 'STARLINK', 'count': 5000},
        'ONEWEB': {'name_pattern': 'ONEWEB', 'count': 600},
        'GPS': {'name_pattern': 'GPS', 'count': 32},
        'GLONASS': {'name_pattern': 'GLONASS', 'count': 24},
        'GALILEO': {'name_pattern': 'GALILEO', 'count': 30},
        'BEIDOU': {'name_pattern': 'BEIDOU', 'count': 35},
        'IRIDIUM': {'name_pattern': 'IRIDIUM', 'count': 75},
    }

    def __init__(self, username: Optional[str] = None, password: Optional[str] = None):
        """
        Initialize Space-Track client

        Args:
            username: Space-Track.org username (or from SPACETRACK_USER env var)
            password: Space-Track.org password (or from SPACETRACK_PASS env var)
        """
        self.username = username or os.getenv('SPACETRACK_USER')
        self.password = password or os.getenv('SPACETRACK_PASS')

        if not self.username or not self.password:
            raise ValueError(
                "Space-Track credentials required. Register at: "
                "https://www.space-track.org/auth/createAccount\n"
                "Set env vars: SPACETRACK_USER and SPACETRACK_PASS"
            )

        self.session = requests.Session()
        self.authenticated = False
        self.cache = {}
        self.cache_duration = 3600  # 1 hour cache for TLEs

    def login(self) -> bool:
        """Authenticate with Space-Track.org"""
        if self.authenticated:
            return True

        try:
            response = self.session.post(
                self.LOGIN_URL,
                data={'identity': self.username, 'password': self.password},
                timeout=30
            )

            if response.status_code == 200:
                self.authenticated = True
                return True
            else:
                print(f"⚠️ Space-Track login failed: {response.status_code}")
                return False

        except requests.RequestException as e:
            print(f"⚠️ Space-Track login error: {e}")
            return False

    def get_tle_by_norad_id(self, norad_id: int) -> Optional[SatelliteTLE]:
        """Get latest TLE for a satellite by NORAD catalog number"""
        if not self.login():
            return None

        # Check cache
        cache_key = f"norad_{norad_id}"
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if time.time() - cached_time < self.cache_duration:
                return cached_data

        try:
            query = f"{self.TLE_URL}/NORAD_CAT_ID/{norad_id}/orderby/EPOCH%20desc/limit/1"
            response = self.session.get(query, timeout=30)
            response.raise_for_status()

            data = response.json()
            if data:
                tle = self._parse_spacetrack_tle(data[0])
                self.cache[cache_key] = (time.time(), tle)
                return tle

        except requests.RequestException as e:
            print(f"⚠️ Space-Track TLE query error: {e}")

        return None

    def get_constellation(self, constellation_name: str, limit: int = 100) -> List[SatelliteTLE]:
        """
        Get TLEs for a satellite constellation

        Args:
            constellation_name: Name (STARLINK, ONEWEB, GPS, etc.)
            limit: Maximum number of satellites to retrieve

        Returns:
            List of SatelliteTLE objects
        """
        if not self.login():
            return []

        constellation_name = constellation_name.upper()
        if constellation_name not in self.CONSTELLATIONS:
            print(f"⚠️ Unknown constellation: {constellation_name}")
            print(f"Available: {', '.join(self.CONSTELLATIONS.keys())}")
            return []

        # Check cache
        cache_key = f"constellation_{constellation_name}_{limit}"
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if time.time() - cached_time < self.cache_duration:
                return cached_data

        pattern = self.CONSTELLATIONS[constellation_name]['name_pattern']

        try:
            # Query for constellation
            query = (f"{self.TLE_URL}/OBJECT_NAME/{pattern}~~/orderby/NORAD_CAT_ID%20asc/"
                    f"limit/{limit}/metadata/false")

            response = self.session.get(query, timeout=60)
            response.raise_for_status()

            data = response.json()
            tles = [self._parse_spacetrack_tle(item) for item in data if item]

            # Cache results
            self.cache[cache_key] = (time.time(), tles)

            return tles

        except requests.RequestException as e:
            print(f"⚠️ Space-Track constellation query error: {e}")

        return []

    def get_active_satellites(self, limit: int = 1000) -> List[SatelliteTLE]:
        """Get currently active satellites"""
        if not self.login():
            return []

        try:
            # Get satellites decayed in last 30 days or still active
            thirty_days_ago = (datetime.now() - timedelta(days=30)).strftime('%Y-%m-%d')

            query = (f"{self.TLE_URL}/DECAY_DATE/null-val/EPOCH/>now-30/"
                    f"orderby/EPOCH%20desc/limit/{limit}/metadata/false")

            response = self.session.get(query, timeout=60)
            response.raise_for_status()

            data = response.json()
            tles = [self._parse_spacetrack_tle(item) for item in data if item]

            return tles

        except requests.RequestException as e:
            print(f"⚠️ Space-Track active satellites query error: {e}")

        return []

    def _parse_spacetrack_tle(self, data: Dict) -> SatelliteTLE:
        """Parse Space-Track JSON response into SatelliteTLE"""
        # Extract TLE lines
        tle_line1 = data.get('TLE_LINE1', '')
        tle_line2 = data.get('TLE_LINE2', '')

        # Parse epoch from TLE
        epoch_str = data.get('EPOCH', '')
        try:
            epoch = datetime.strptime(epoch_str, '%Y-%m-%dT%H:%M:%S.%f')
        except ValueError:
            epoch = datetime.now()

        # Extract orbital elements
        inclination = float(data.get('INCLINATION', 0))
        eccentricity = float(data.get('ECCENTRICITY', 0)) / 10000000  # Space-Track gives in 10^-7
        mean_motion = float(data.get('MEAN_MOTION', 0))  # revs per day
        apogee = float(data.get('APOGEE', 0))
        perigee = float(data.get('PERIGEE', 0))
        period = float(data.get('PERIOD', 0))

        return SatelliteTLE(
            norad_id=int(data.get('NORAD_CAT_ID', 0)),
            name=data.get('OBJECT_NAME', 'UNKNOWN'),
            tle_line1=tle_line1,
            tle_line2=tle_line2,
            epoch=epoch,
            inclination=inclination,
            eccentricity=eccentricity,
            mean_motion=mean_motion,
            apogee=apogee,
            perigee=perigee,
            period=period
        )

    def get_satellite_statistics(self, satellites: List[SatelliteTLE]) -> Dict[str, Any]:
        """Calculate statistics for a satellite constellation"""
        if not satellites:
            return {}

        inclinations = [s.inclination for s in satellites]
        apogees = [s.apogee for s in satellites]
        perigees = [s.perigee for s in satellites]
        periods = [s.period for s in satellites]

        return {
            'total_satellites': len(satellites),
            'avg_inclination': sum(inclinations) / len(inclinations),
            'avg_apogee_km': sum(apogees) / len(apogees),
            'avg_perigee_km': sum(perigees) / len(perigees),
            'avg_period_min': sum(periods) / len(periods),
            'min_altitude_km': min(perigees),
            'max_altitude_km': max(apogees),
            'orbital_shells': self._count_orbital_shells(perigees, tolerance_km=50)
        }

    def _count_orbital_shells(self, altitudes: List[float], tolerance_km: float = 50) -> int:
        """Count distinct orbital shells"""
        if not altitudes:
            return 0

        sorted_alts = sorted(altitudes)
        shells = 1
        last_alt = sorted_alts[0]

        for alt in sorted_alts[1:]:
            if abs(alt - last_alt) > tolerance_km:
                shells += 1
                last_alt = alt

        return shells


class SatelliteNetworkSimulator:
    """Simulate satellite networks using real TLE data"""

    def __init__(self, spacetrack_client: Optional[SpaceTrackClient] = None):
        self.spacetrack = spacetrack_client

    def create_satellite_network(self, constellation: str, num_satellites: int = 100) -> Dict[str, Any]:
        """
        Create a satellite network from real constellation data

        Args:
            constellation: Constellation name (STARLINK, ONEWEB, etc.)
            num_satellites: Number of satellites to include

        Returns:
            Network configuration dict
        """
        if not self.spacetrack:
            raise ValueError("SpaceTrackClient required for satellite network simulation")

        # Get real satellite TLEs
        satellites = self.spacetrack.get_constellation(constellation, limit=num_satellites)

        if not satellites:
            return {'error': 'No satellites retrieved'}

        # Calculate network statistics
        stats = self.spacetrack.get_satellite_statistics(satellites)

        # Create network nodes
        nodes = []
        for sat in satellites:
            nodes.append({
                'node_id': f"sat-{sat.norad_id}",
                'name': sat.name,
                'norad_id': sat.norad_id,
                'tle_line1': sat.tle_line1,
                'tle_line2': sat.tle_line2,
                'inclination': sat.inclination,
                'period': sat.period,
                'apogee_km': sat.apogee,
                'perigee_km': sat.perigee,
                'avg_altitude_km': (sat.apogee + sat.perigee) / 2
            })

        return {
            'constellation': constellation,
            'nodes': nodes,
            'statistics': stats,
            'timestamp': datetime.now().isoformat()
        }

    def calculate_coverage_area(self, altitude_km: float, min_elevation_deg: float = 10.0) -> float:
        """
        Calculate ground coverage area for a satellite

        Args:
            altitude_km: Satellite altitude in km
            min_elevation_deg: Minimum elevation angle for coverage (degrees)

        Returns:
            Coverage area in square kilometers
        """
        earth_radius_km = 6371.0

        # Calculate central angle
        min_elev_rad = math.radians(min_elevation_deg)
        central_angle = math.acos(
            earth_radius_km / (earth_radius_km + altitude_km) * math.cos(min_elev_rad)
        ) - min_elev_rad

        # Calculate coverage radius
        coverage_radius_km = earth_radius_km * central_angle

        # Calculate coverage area
        coverage_area_km2 = math.pi * coverage_radius_km ** 2

        return coverage_area_km2

    def estimate_global_coverage(self, satellites: List[SatelliteTLE],
                                min_elevation_deg: float = 10.0) -> Dict[str, Any]:
        """
        Estimate global coverage capability of a constellation

        Args:
            satellites: List of satellites
            min_elevation_deg: Minimum elevation angle

        Returns:
            Coverage estimation
        """
        if not satellites:
            return {'coverage_percent': 0}

        # Calculate individual satellite coverage
        total_coverage = 0
        for sat in satellites:
            avg_alt = (sat.apogee + sat.perigee) / 2
            coverage = self.calculate_coverage_area(avg_alt, min_elevation_deg)
            total_coverage += coverage

        # Earth surface area
        earth_surface_km2 = 510_100_000  # km²

        # Estimate coverage (with overlap correction factor)
        overlap_factor = 0.6  # Assume 40% overlap
        effective_coverage = total_coverage * overlap_factor
        coverage_percent = min(100, (effective_coverage / earth_surface_km2) * 100)

        return {
            'total_satellites': len(satellites),
            'individual_coverage_km2': total_coverage / len(satellites),
            'total_coverage_km2': total_coverage,
            'effective_coverage_km2': effective_coverage,
            'earth_surface_km2': earth_surface_km2,
            'coverage_percent': coverage_percent,
            'min_elevation_deg': min_elevation_deg
        }


# Example usage and testing
if __name__ == "__main__":
    print("🛰️  Space-Track.org Satellite TLE Integration Test")
    print("=" * 80)

    # Check for credentials
    username = os.getenv('SPACETRACK_USER')
    password = os.getenv('SPACETRACK_PASS')

    if not username or not password:
        print("⚠️  Space-Track credentials not set!")
        print("📝 Register (free) at: https://www.space-track.org/auth/createAccount")
        print("💡 Set with:")
        print("   export SPACETRACK_USER='your_username'")
        print("   export SPACETRACK_PASS='your_password'")
        exit(1)

    try:
        # Initialize client
        spacetrack = SpaceTrackClient(username, password)

        # Test 1: Login
        print("\n🔐 Space-Track Authentication:")
        print("-" * 80)
        if spacetrack.login():
            print("✅ Successfully authenticated with Space-Track.org")
        else:
            print("❌ Authentication failed")
            exit(1)

        # Test 2: Get single satellite (ISS)
        print("\n🛰️  International Space Station (ISS) TLE:")
        print("-" * 80)
        iss = spacetrack.get_tle_by_norad_id(25544)  # ISS NORAD ID
        if iss:
            print(f"Name: {iss.name}")
            print(f"NORAD ID: {iss.norad_id}")
            print(f"Epoch: {iss.epoch}")
            print(f"Inclination: {iss.inclination:.2f}°")
            print(f"Period: {iss.period:.2f} minutes")
            print(f"Apogee: {iss.apogee:.1f} km")
            print(f"Perigee: {iss.perigee:.1f} km")
            print(f"\nTLE Line 1: {iss.tle_line1}")
            print(f"TLE Line 2: {iss.tle_line2}")

        # Test 3: Get constellation (Starlink sample)
        print("\n🌐 Starlink Constellation (sample):")
        print("-" * 80)
        starlink = spacetrack.get_constellation('STARLINK', limit=50)
        print(f"Retrieved {len(starlink)} Starlink satellites")

        if starlink:
            stats = spacetrack.get_satellite_statistics(starlink)
            print(f"\nConstellation Statistics:")
            print(f"  Total satellites: {stats['total_satellites']}")
            print(f"  Avg inclination: {stats['avg_inclination']:.2f}°")
            print(f"  Avg altitude: {stats['avg_perigee_km']:.1f} km")
            print(f"  Avg period: {stats['avg_period_min']:.2f} minutes")
            print(f"  Orbital shells: {stats['orbital_shells']}")

        # Test 4: GPS Constellation
        print("\n🌍 GPS Constellation:")
        print("-" * 80)
        gps = spacetrack.get_constellation('GPS', limit=32)
        print(f"Retrieved {len(gps)} GPS satellites")

        if gps:
            stats = spacetrack.get_satellite_statistics(gps)
            print(f"  Avg altitude: {stats['avg_perigee_km']:.1f} km")
            print(f"  Avg period: {stats['avg_period_min']:.2f} minutes")

        # Test 5: Network simulation
        print("\n📡 Satellite Network Simulation:")
        print("-" * 80)
        simulator = SatelliteNetworkSimulator(spacetrack)
        network = simulator.create_satellite_network('STARLINK', num_satellites=25)

        if 'error' not in network:
            print(f"Created network: {network['constellation']}")
            print(f"Nodes: {len(network['nodes'])}")
            print(f"Statistics: {network['statistics']}")

            # Test coverage estimation
            if starlink:
                coverage = simulator.estimate_global_coverage(starlink[:25])
                print(f"\nGlobal Coverage Estimate:")
                print(f"  Satellites: {coverage['total_satellites']}")
                print(f"  Coverage: {coverage['coverage_percent']:.1f}%")
                print(f"  Effective coverage: {coverage['effective_coverage_km2']:,.0f} km²")

        print("\n✅ All Space-Track integration tests complete!")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
