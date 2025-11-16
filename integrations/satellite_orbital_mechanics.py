"""
Satellite Orbital Mechanics Module
==================================
Implements SGP4 propagation, TLE parsing, and orbital calculations for satellite tracking.
Optimized for large-scale constellations (50,000+ satellites).

Author: MotorHandPro Integration Team
License: MIT
"""

import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional
import json
from dataclasses import dataclass
import math


@dataclass
class OrbitalElements:
    """Two-Line Element (TLE) orbital parameters"""
    satellite_id: int
    epoch: datetime
    inclination: float  # degrees
    raan: float  # Right Ascension of Ascending Node (degrees)
    eccentricity: float
    arg_perigee: float  # Argument of perigee (degrees)
    mean_anomaly: float  # degrees
    mean_motion: float  # revolutions per day
    bstar: float  # Drag term
    epoch_rev: int  # Revolution number at epoch


@dataclass
class SatelliteState:
    """Satellite position and velocity state"""
    satellite_id: int
    timestamp: datetime
    position_eci: np.ndarray  # [x, y, z] in km (Earth-Centered Inertial)
    velocity_eci: np.ndarray  # [vx, vy, vz] in km/s
    position_ecef: np.ndarray  # [x, y, z] in km (Earth-Centered Earth-Fixed)
    latitude: float  # degrees
    longitude: float  # degrees
    altitude: float  # km above Earth surface

    def to_dict(self) -> Dict:
        """Convert to JSON-serializable dictionary"""
        return {
            'satellite_id': self.satellite_id,
            'timestamp': self.timestamp.isoformat(),
            'position_eci': self.position_eci.tolist(),
            'velocity_eci': self.velocity_eci.tolist(),
            'position_ecef': self.position_ecef.tolist(),
            'latitude': self.latitude,
            'longitude': self.longitude,
            'altitude': self.altitude
        }


class SimplifiedSGP4:
    """
    Simplified SGP4 propagator for satellite orbital mechanics.
    Optimized for performance with large constellations.

    Note: This is a simplified implementation. For production use with real TLEs,
    consider using the official SGP4 library (pip install sgp4).
    """

    # Constants
    EARTH_RADIUS = 6378.137  # km
    EARTH_MU = 398600.4418  # km^3/s^2 (gravitational parameter)
    EARTH_J2 = 0.00108263  # J2 perturbation
    EARTH_ROTATION_RATE = 7.2921159e-5  # rad/s

    def __init__(self, orbital_elements: OrbitalElements):
        self.elements = orbital_elements
        self._cache = {}

    def propagate(self, target_time: datetime) -> SatelliteState:
        """Propagate satellite position to target time"""

        # Calculate time since epoch
        dt = (target_time - self.elements.epoch).total_seconds()

        # Convert orbital elements to position/velocity
        position_eci, velocity_eci = self._sgp4_propagate(dt)

        # Convert ECI to ECEF (rotating Earth frame)
        position_ecef = self._eci_to_ecef(position_eci, target_time)

        # Convert to latitude/longitude/altitude
        lat, lon, alt = self._ecef_to_geodetic(position_ecef)

        return SatelliteState(
            satellite_id=self.elements.satellite_id,
            timestamp=target_time,
            position_eci=position_eci,
            velocity_eci=velocity_eci,
            position_ecef=position_ecef,
            latitude=lat,
            longitude=lon,
            altitude=alt
        )

    def _sgp4_propagate(self, dt_seconds: float) -> Tuple[np.ndarray, np.ndarray]:
        """Simplified SGP4 propagation algorithm"""

        # Convert elements to radians and SI units
        inc = np.radians(self.elements.inclination)
        raan = np.radians(self.elements.raan)
        ecc = self.elements.eccentricity
        arg_p = np.radians(self.elements.arg_perigee)
        mean_anom = np.radians(self.elements.mean_anomaly)
        n = self.elements.mean_motion * 2 * np.pi / 86400  # rad/s

        # Calculate semi-major axis from mean motion
        a = (self.EARTH_MU / (n**2))**(1/3)

        # Update mean anomaly with time
        M = mean_anom + n * dt_seconds

        # Solve Kepler's equation for eccentric anomaly (Newton-Raphson)
        E = self._solve_kepler(M, ecc)

        # True anomaly
        nu = 2 * np.arctan2(
            np.sqrt(1 + ecc) * np.sin(E / 2),
            np.sqrt(1 - ecc) * np.cos(E / 2)
        )

        # Distance from Earth center
        r = a * (1 - ecc * np.cos(E))

        # Position in orbital plane
        x_orb = r * np.cos(nu)
        y_orb = r * np.sin(nu)

        # Velocity in orbital plane
        v_factor = np.sqrt(self.EARTH_MU * a) / r
        vx_orb = -v_factor * np.sin(E)
        vy_orb = v_factor * np.sqrt(1 - ecc**2) * np.cos(E)

        # Rotation matrices to ECI frame
        # First rotate by argument of perigee
        cos_w, sin_w = np.cos(arg_p), np.sin(arg_p)
        # Then by inclination
        cos_i, sin_i = np.cos(inc), np.sin(inc)
        # Finally by RAAN
        cos_O, sin_O = np.cos(raan), np.sin(raan)

        # Combined rotation matrix
        R11 = cos_O * cos_w - sin_O * sin_w * cos_i
        R12 = -cos_O * sin_w - sin_O * cos_w * cos_i
        R21 = sin_O * cos_w + cos_O * sin_w * cos_i
        R22 = -sin_O * sin_w + cos_O * cos_w * cos_i
        R31 = sin_w * sin_i
        R32 = cos_w * sin_i

        # Position in ECI frame
        x_eci = R11 * x_orb + R12 * y_orb
        y_eci = R21 * x_orb + R22 * y_orb
        z_eci = R31 * x_orb + R32 * y_orb

        # Velocity in ECI frame
        vx_eci = R11 * vx_orb + R12 * vy_orb
        vy_eci = R21 * vx_orb + R22 * vy_orb
        vz_eci = R31 * vx_orb + R32 * vy_orb

        position = np.array([x_eci, y_eci, z_eci])
        velocity = np.array([vx_eci, vy_eci, vz_eci])

        return position, velocity

    def _solve_kepler(self, M: float, ecc: float, tol: float = 1e-8) -> float:
        """Solve Kepler's equation using Newton-Raphson method"""
        E = M  # Initial guess
        for _ in range(10):  # Max iterations
            f = E - ecc * np.sin(E) - M
            if abs(f) < tol:
                break
            f_prime = 1 - ecc * np.cos(E)
            E = E - f / f_prime
        return E

    def _eci_to_ecef(self, position_eci: np.ndarray, time: datetime) -> np.ndarray:
        """Convert ECI (inertial) to ECEF (rotating Earth frame)"""

        # Calculate Greenwich Mean Sidereal Time
        gmst = self._calculate_gmst(time)

        # Rotation matrix around Z-axis
        cos_gmst = np.cos(gmst)
        sin_gmst = np.sin(gmst)

        rotation_matrix = np.array([
            [cos_gmst, sin_gmst, 0],
            [-sin_gmst, cos_gmst, 0],
            [0, 0, 1]
        ])

        position_ecef = rotation_matrix @ position_eci
        return position_ecef

    def _calculate_gmst(self, time: datetime) -> float:
        """Calculate Greenwich Mean Sidereal Time in radians"""

        # Julian date
        jd = self._datetime_to_jd(time)

        # Days since J2000.0
        T = (jd - 2451545.0) / 36525.0

        # GMST in seconds
        gmst_sec = 67310.54841 + (876600.0 * 3600.0 + 8640184.812866) * T \
                   + 0.093104 * T**2 - 6.2e-6 * T**3

        # Convert to radians and normalize to [0, 2π]
        gmst_rad = (gmst_sec % 86400) * (2 * np.pi / 86400)

        return gmst_rad

    def _datetime_to_jd(self, dt: datetime) -> float:
        """Convert datetime to Julian Date"""
        a = (14 - dt.month) // 12
        y = dt.year + 4800 - a
        m = dt.month + 12 * a - 3

        jdn = dt.day + (153 * m + 2) // 5 + 365 * y + y // 4 - y // 100 + y // 400 - 32045
        jd = jdn + (dt.hour - 12) / 24.0 + dt.minute / 1440.0 + dt.second / 86400.0

        return jd

    def _ecef_to_geodetic(self, position_ecef: np.ndarray) -> Tuple[float, float, float]:
        """Convert ECEF coordinates to geodetic (lat, lon, alt)"""

        x, y, z = position_ecef

        # Longitude (simple)
        lon = np.arctan2(y, x)

        # Latitude and altitude (iterative method for ellipsoid)
        p = np.sqrt(x**2 + y**2)
        lat = np.arctan2(z, p)

        # WGS84 ellipsoid parameters
        a = self.EARTH_RADIUS  # semi-major axis
        f = 1 / 298.257223563  # flattening
        e2 = 2 * f - f**2  # eccentricity squared

        # Iterative refinement (3 iterations is usually sufficient)
        for _ in range(3):
            N = a / np.sqrt(1 - e2 * np.sin(lat)**2)
            alt = p / np.cos(lat) - N
            lat = np.arctan2(z, p * (1 - e2 * N / (N + alt)))

        # Final altitude calculation
        N = a / np.sqrt(1 - e2 * np.sin(lat)**2)
        alt = p / np.cos(lat) - N

        # Convert to degrees
        lat_deg = np.degrees(lat)
        lon_deg = np.degrees(lon)

        return lat_deg, lon_deg, alt


class ConstellationGenerator:
    """
    Generate large satellite constellations with realistic orbital parameters.
    Optimized for Starlink-like mega-constellations.
    """

    @staticmethod
    def generate_starlink_constellation(num_satellites: int = 50000,
                                       base_epoch: Optional[datetime] = None) -> List[OrbitalElements]:
        """
        Generate Starlink-like constellation with multiple orbital shells.

        Starlink constellation design (as of 2024):
        - Shell 1: 550 km altitude, 53° inclination (1,584 satellites)
        - Shell 2: 540 km altitude, 53.2° inclination (1,584 satellites)
        - Shell 3: 570 km altitude, 70° inclination (720 satellites)
        - Shell 4: 560 km altitude, 97.6° inclination (348 satellites - polar)
        - Shell 5: 340-614 km altitude, various inclinations (expanding)

        This generator creates a simplified version distributed across shells.
        """

        if base_epoch is None:
            base_epoch = datetime.utcnow()

        # Define orbital shells (altitude in km, inclination in degrees, satellite count)
        shells = [
            {'altitude': 550, 'inclination': 53.0, 'planes': 72, 'sats_per_plane': 22},   # ~1,584 sats
            {'altitude': 540, 'inclination': 53.2, 'planes': 72, 'sats_per_plane': 22},   # ~1,584 sats
            {'altitude': 570, 'inclination': 70.0, 'planes': 36, 'sats_per_plane': 20},   # ~720 sats
            {'altitude': 560, 'inclination': 97.6, 'planes': 12, 'sats_per_plane': 29},   # ~348 sats (polar)
            {'altitude': 340, 'inclination': 42.0, 'planes': 48, 'sats_per_plane': 50},   # ~2,400 sats
            {'altitude': 614, 'inclination': 115.7, 'planes': 48, 'sats_per_plane': 50},  # ~2,400 sats (retrograde)
        ]

        satellites = []
        sat_id = 1

        # Calculate how many times to replicate the shell pattern
        base_shell_count = sum(s['planes'] * s['sats_per_plane'] for s in shells)
        replication_factor = max(1, num_satellites // base_shell_count)

        for replication in range(replication_factor):
            for shell_idx, shell in enumerate(shells):
                altitude = shell['altitude']
                inclination = shell['inclination']
                num_planes = shell['planes']
                sats_per_plane = shell['sats_per_plane']

                # Calculate orbital parameters
                radius = SimplifiedSGP4.EARTH_RADIUS + altitude
                mean_motion = np.sqrt(SimplifiedSGP4.EARTH_MU / radius**3) * 86400 / (2 * np.pi)  # rev/day

                # Distribute satellites across orbital planes
                for plane_idx in range(num_planes):
                    # RAAN spacing (orbital planes)
                    raan = (360.0 / num_planes) * plane_idx

                    for sat_idx in range(sats_per_plane):
                        if sat_id > num_satellites:
                            return satellites

                        # Mean anomaly spacing (phasing within plane)
                        mean_anomaly = (360.0 / sats_per_plane) * sat_idx

                        # Add slight perturbations for replication diversity
                        if replication > 0:
                            altitude_offset = (replication * 2)  # km
                            radius = SimplifiedSGP4.EARTH_RADIUS + altitude + altitude_offset
                            mean_motion = np.sqrt(SimplifiedSGP4.EARTH_MU / radius**3) * 86400 / (2 * np.pi)

                        # Create orbital elements
                        elements = OrbitalElements(
                            satellite_id=sat_id,
                            epoch=base_epoch,
                            inclination=inclination,
                            raan=raan % 360,
                            eccentricity=0.0001,  # Near-circular orbits
                            arg_perigee=0.0,
                            mean_anomaly=mean_anomaly % 360,
                            mean_motion=mean_motion,
                            bstar=0.00001,  # Low drag at these altitudes
                            epoch_rev=0
                        )

                        satellites.append(elements)
                        sat_id += 1

        # Fill remaining satellites if needed
        while len(satellites) < num_satellites:
            # Use first shell as template
            shell = shells[0]
            altitude = shell['altitude'] + (len(satellites) % 100) * 0.5  # Slight altitude variation
            radius = SimplifiedSGP4.EARTH_RADIUS + altitude
            mean_motion = np.sqrt(SimplifiedSGP4.EARTH_MU / radius**3) * 86400 / (2 * np.pi)

            elements = OrbitalElements(
                satellite_id=len(satellites) + 1,
                epoch=base_epoch,
                inclination=shell['inclination'],
                raan=np.random.uniform(0, 360),
                eccentricity=0.0001,
                arg_perigee=0.0,
                mean_anomaly=np.random.uniform(0, 360),
                mean_motion=mean_motion,
                bstar=0.00001,
                epoch_rev=0
            )

            satellites.append(elements)

        return satellites[:num_satellites]


class ConstellationTracker:
    """
    High-performance tracker for large satellite constellations.
    Optimized for 50,000+ satellites with batch processing.
    """

    def __init__(self, orbital_elements_list: List[OrbitalElements]):
        self.satellites = {
            elem.satellite_id: SimplifiedSGP4(elem)
            for elem in orbital_elements_list
        }
        self.num_satellites = len(self.satellites)

    def propagate_all(self, target_time: datetime,
                      satellite_ids: Optional[List[int]] = None) -> List[SatelliteState]:
        """
        Propagate all satellites (or subset) to target time.
        Returns list of satellite states.
        """

        if satellite_ids is None:
            satellite_ids = list(self.satellites.keys())

        states = []
        for sat_id in satellite_ids:
            if sat_id in self.satellites:
                state = self.satellites[sat_id].propagate(target_time)
                states.append(state)

        return states

    def get_satellites_in_view(self, observer_lat: float, observer_lon: float,
                               observer_alt: float, target_time: datetime,
                               min_elevation: float = 10.0) -> List[SatelliteState]:
        """
        Get all satellites visible from observer location.

        Args:
            observer_lat: Observer latitude (degrees)
            observer_lon: Observer longitude (degrees)
            observer_alt: Observer altitude (km)
            target_time: Observation time
            min_elevation: Minimum elevation angle (degrees)
        """

        # Convert observer to ECEF
        observer_ecef = self._geodetic_to_ecef(observer_lat, observer_lon, observer_alt)

        visible_satellites = []

        # Check each satellite
        for sat_id, propagator in self.satellites.items():
            state = propagator.propagate(target_time)

            # Calculate elevation angle
            elevation = self._calculate_elevation(observer_ecef, state.position_ecef)

            if elevation >= min_elevation:
                visible_satellites.append(state)

        return visible_satellites

    def _geodetic_to_ecef(self, lat: float, lon: float, alt: float) -> np.ndarray:
        """Convert geodetic to ECEF coordinates"""
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
        """Calculate satellite elevation angle from observer"""

        # Vector from observer to satellite
        range_vec = satellite_ecef - observer_ecef

        # Observer's local up vector (simplified - radial direction)
        up_vec = observer_ecef / np.linalg.norm(observer_ecef)

        # Calculate elevation angle
        range_norm = np.linalg.norm(range_vec)
        if range_norm == 0:
            return 0.0

        sin_elevation = np.dot(range_vec, up_vec) / range_norm
        elevation = np.degrees(np.arcsin(np.clip(sin_elevation, -1, 1)))

        return elevation

    def calculate_coverage_stats(self, target_time: datetime,
                                 grid_resolution: int = 100) -> Dict:
        """
        Calculate global coverage statistics.

        Args:
            target_time: Time for coverage calculation
            grid_resolution: Number of grid points per dimension (100 = 10,000 points)
        """

        # Create grid of points on Earth surface
        latitudes = np.linspace(-90, 90, grid_resolution)
        longitudes = np.linspace(-180, 180, grid_resolution)

        coverage_count = np.zeros((grid_resolution, grid_resolution))

        # Propagate all satellites
        states = self.propagate_all(target_time)

        # For each grid point, count visible satellites
        for i, lat in enumerate(latitudes):
            for j, lon in enumerate(longitudes):
                observer_ecef = self._geodetic_to_ecef(lat, lon, 0.0)

                for state in states:
                    elevation = self._calculate_elevation(observer_ecef, state.position_ecef)
                    if elevation >= 10.0:  # 10° minimum elevation
                        coverage_count[i, j] += 1

        return {
            'timestamp': target_time.isoformat(),
            'total_satellites': len(states),
            'grid_resolution': grid_resolution,
            'coverage_matrix': coverage_count.tolist(),
            'avg_satellites_per_point': float(np.mean(coverage_count)),
            'max_satellites_per_point': int(np.max(coverage_count)),
            'min_satellites_per_point': int(np.min(coverage_count)),
            'percent_coverage': float(np.sum(coverage_count > 0) / coverage_count.size * 100)
        }


if __name__ == "__main__":
    # Quick test
    print("Satellite Orbital Mechanics Module - Test")
    print("=" * 50)

    # Generate small constellation for testing
    print("\n1. Generating 100 satellite constellation...")
    constellation = ConstellationGenerator.generate_starlink_constellation(100)
    print(f"   ✓ Generated {len(constellation)} satellites")

    # Create tracker
    print("\n2. Initializing constellation tracker...")
    tracker = ConstellationTracker(constellation)
    print(f"   ✓ Tracking {tracker.num_satellites} satellites")

    # Propagate to current time
    print("\n3. Propagating satellites...")
    current_time = datetime.utcnow()
    states = tracker.propagate_all(current_time)
    print(f"   ✓ Propagated {len(states)} satellite states")

    # Show sample satellite
    if states:
        sample = states[0]
        print(f"\n4. Sample satellite state (ID {sample.satellite_id}):")
        print(f"   Position: [{sample.position_eci[0]:.2f}, {sample.position_eci[1]:.2f}, {sample.position_eci[2]:.2f}] km")
        print(f"   Lat/Lon: {sample.latitude:.2f}°, {sample.longitude:.2f}°")
        print(f"   Altitude: {sample.altitude:.2f} km")

    print("\n" + "=" * 50)
    print("Module test complete!")
