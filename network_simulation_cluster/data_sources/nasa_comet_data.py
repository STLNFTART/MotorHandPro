#!/usr/bin/env python3
"""
NASA Comet Data Integration for 3I/ATLAS (C/2025 N1)

Provides real-time astronomical data from:
- JPL Horizons API (ephemerides, orbital elements)
- Minor Planet Center (MPC) - astrometry updates
- NASA/IAWN data streams
- TheSkyLive - near-real-time tracking

Integrates with MotorHandPro's Recursive Planck Operator framework
for primal logic simulation and anomaly detection.

Author: Donte Lightfoot
Date: December 4, 2025
"""

import os
import requests
import json
import numpy as np
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime, timedelta
from dataclasses import dataclass, field
import time
import warnings


@dataclass
class CometObservation:
    """Single comet observation/ephemeris point"""
    timestamp: datetime
    ra: float  # Right Ascension (degrees)
    dec: float  # Declination (degrees)
    distance_au: float  # Distance from Earth (AU)
    velocity_km_s: float  # Radial velocity (km/s)
    magnitude: float  # Visual magnitude
    elongation: float  # Solar elongation (degrees)
    phase_angle: float  # Phase angle (degrees)

    # Comet-specific parameters
    tail_length_km: Optional[float] = None  # Tail extension
    gas_production_rate: Optional[float] = None  # Gas flux (molecules/s or g/s)
    dust_production_rate: Optional[float] = None  # Dust flux
    coma_diameter_km: Optional[float] = None  # Coma size

    # Metadata
    source: str = "JPL Horizons"
    quality_flag: str = "NOMINAL"


@dataclass
class CometOrbitalElements:
    """Orbital elements for comet trajectory"""
    epoch: datetime
    eccentricity: float  # e
    perihelion_distance: float  # q (AU)
    time_of_perihelion: datetime  # T
    inclination: float  # i (degrees)
    longitude_ascending_node: float  # Ω (degrees)
    argument_of_perihelion: float  # ω (degrees)

    # Optional
    semi_major_axis: Optional[float] = None  # a (AU)
    orbital_period_years: Optional[float] = None  # P (years)


@dataclass
class RecursivePlanckState:
    """State for Recursive Planck Operator integration"""
    n: float = 0.0  # State variable
    signal: float = 0.0  # S(t) from comet data
    memory_integral: float = 0.0  # ∫ K(τ) n(τ) dτ
    error: float = 0.0  # G_PL(t) - anomaly metric
    mu: float = 0.16905  # Damping constant (Lightfoot constant)
    D: float = 149.9992314  # Bound (AU scale)


class NASACometDataClient:
    """Client for fetching NASA comet data for 3I/ATLAS"""

    # JPL Horizons API
    HORIZONS_BASE = "https://ssd.jpl.nasa.gov/api/horizons.api"

    # Minor Planet Center
    MPC_BASE = "https://www.minorplanetcenter.net"
    MPC_MPEC_BASE = f"{MPC_BASE}/mpec/K25"  # 2025 MPECs

    # TheSkyLive (backup/supplementary)
    THESKYLIVE_BASE = "https://theskylive.com/3dsolarsystem"

    # 3I/ATLAS identifiers
    COMET_NAME = "3I/ATLAS"
    COMET_DESIGNATION = "C/2025 N1"
    JPL_ID = "90009135"  # JPL Small-Body Database ID

    # Physical constants
    AU_TO_KM = 149597870.7  # 1 AU in km

    def __init__(self, cache_duration: int = 3600):
        """
        Initialize NASA Comet Data Client

        Args:
            cache_duration: Cache duration in seconds (default 1 hour)
        """
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'MotorHandPro-NASAIntegration/1.0 (Primal Logic Framework)'
        })

        self.cache = {}
        self.cache_duration = cache_duration
        self.observations_history = []

    def fetch_horizons_ephemeris(
        self,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        step: str = "1h"
    ) -> List[CometObservation]:
        """
        Fetch ephemeris data from JPL Horizons

        Args:
            start_time: Start of ephemeris window (default: now)
            end_time: End of ephemeris window (default: now + 24h)
            step: Time step (e.g., "1h", "6h", "1d")

        Returns:
            List of comet observations
        """
        cache_key = f"ephemeris_{start_time}_{end_time}_{step}"

        # Check cache
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if (datetime.now() - cached_time).seconds < self.cache_duration:
                return cached_data

        # Set defaults
        if start_time is None:
            start_time = datetime.now()
        if end_time is None:
            end_time = start_time + timedelta(hours=24)

        # Format for Horizons
        start_str = start_time.strftime("%Y-%m-%d %H:%M")
        end_str = end_time.strftime("%Y-%m-%d %H:%M")

        # Horizons API parameters
        params = {
            'format': 'json',
            'COMMAND': f"'{self.COMET_DESIGNATION}'",  # C/2025 N1
            'OBJ_DATA': 'NO',
            'MAKE_EPHEM': 'YES',
            'EPHEM_TYPE': 'OBSERVER',
            'CENTER': '500@399',  # Geocentric (Earth center)
            'START_TIME': f"'{start_str}'",
            'STOP_TIME': f"'{end_str}'",
            'STEP_SIZE': f"'{step}'",
            'QUANTITIES': "'1,9,20,23,24'",  # RA/Dec, range, vel, mag, elongation
        }

        try:
            response = self.session.get(
                self.HORIZONS_BASE,
                params=params,
                timeout=30
            )

            if response.status_code != 200:
                warnings.warn(f"Horizons API returned {response.status_code}")
                return []

            data = response.json()

            # Parse ephemeris results
            observations = self._parse_horizons_response(data)

            # Cache results
            self.cache[cache_key] = (datetime.now(), observations)
            self.observations_history.extend(observations)

            return observations

        except requests.RequestException as e:
            warnings.warn(f"Horizons API error: {e}")
            return []
        except json.JSONDecodeError as e:
            warnings.warn(f"Failed to parse Horizons response: {e}")
            return []

    def _parse_horizons_response(self, data: Dict[str, Any]) -> List[CometObservation]:
        """Parse Horizons API JSON response"""
        observations = []

        try:
            if 'result' not in data:
                return observations

            result_text = data['result']

            # Horizons returns text in result field - parse it
            lines = result_text.split('\n')

            # Find start of ephemeris data (after $$SOE marker)
            soe_idx = -1
            for i, line in enumerate(lines):
                if '$$SOE' in line:
                    soe_idx = i + 1
                    break
                if '$$EOE' in line:
                    break

            if soe_idx == -1:
                return observations

            # Parse ephemeris lines
            for line in lines[soe_idx:]:
                if '$$EOE' in line:
                    break

                # Example format (simplified):
                # Date__(UT)__HR:MN  R.A._(ICRF) DEC_(ICRF) delta  deldot  V    S-O-T
                parts = line.split()

                if len(parts) < 7:
                    continue

                try:
                    # Parse timestamp
                    date_str = f"{parts[0]} {parts[1]}"
                    timestamp = datetime.strptime(date_str, "%Y-%b-%d %H:%M")

                    # Parse coordinates (RA in HMS, Dec in DMS - need conversion)
                    ra_deg = self._hms_to_degrees(parts[2], parts[3], parts[4])
                    dec_deg = self._dms_to_degrees(parts[5], parts[6], parts[7])

                    # Parse other quantities
                    distance_au = float(parts[8]) if len(parts) > 8 else 0.0
                    velocity_km_s = float(parts[9]) if len(parts) > 9 else 0.0
                    magnitude = float(parts[10]) if len(parts) > 10 else 10.5
                    elongation = float(parts[11]) if len(parts) > 11 else 0.0

                    obs = CometObservation(
                        timestamp=timestamp,
                        ra=ra_deg,
                        dec=dec_deg,
                        distance_au=distance_au,
                        velocity_km_s=velocity_km_s,
                        magnitude=magnitude,
                        elongation=elongation,
                        phase_angle=0.0,  # Not in basic query
                        source="JPL Horizons"
                    )

                    observations.append(obs)

                except (ValueError, IndexError) as e:
                    continue

        except Exception as e:
            warnings.warn(f"Error parsing Horizons response: {e}")

        return observations

    def fetch_mpc_astrometry(self, days_back: int = 7) -> List[CometObservation]:
        """
        Fetch latest astrometry from Minor Planet Center MPECs

        Args:
            days_back: How many days of MPECs to check

        Returns:
            List of observations from MPC
        """
        observations = []

        # MPC circular format for 2025
        # Example: K25U142 for 3I/ATLAS IAWN campaign (Nov 27, 2025)

        # Try recent MPECs
        current_date = datetime.now()

        for days in range(days_back):
            check_date = current_date - timedelta(days=days)

            # Try different MPEC codes (simplified - would need actual MPEC tracking)
            for letter in ['U', 'V', 'W', 'X']:
                mpec_code = f"K25{letter}142"  # Example MPEC code
                mpec_url = f"{self.MPC_MPEC_BASE}/{mpec_code}.html"

                try:
                    response = self.session.get(mpec_url, timeout=10)

                    if response.status_code == 200:
                        # Parse MPEC HTML (simplified - actual parsing more complex)
                        obs = self._parse_mpc_mpec(response.text)
                        observations.extend(obs)

                except requests.RequestException:
                    continue

        return observations

    def _parse_mpc_mpec(self, html_content: str) -> List[CometObservation]:
        """Parse MPC MPEC HTML content (simplified)"""
        observations = []

        # MPC format is complex - this is a simplified placeholder
        # In production, would use proper HTML parsing and astrometry format

        lines = html_content.split('\n')

        for line in lines:
            if self.COMET_NAME in line or self.COMET_DESIGNATION in line:
                # Extract RA/Dec from line
                # Format typically: DESIGNATION RA Dec ...
                # This is highly simplified
                try:
                    parts = line.split()
                    ra_idx = -1

                    for i, part in enumerate(parts):
                        if 'h' in part and 'm' in parts[i+1]:
                            # Found RA in HMS format
                            ra_deg = self._hms_to_degrees(
                                parts[i].replace('h', ''),
                                parts[i+1].replace('m', ''),
                                parts[i+2].replace('s', '')
                            )

                            dec_deg = self._dms_to_degrees(
                                parts[i+3].replace('d', ''),
                                parts[i+4].replace('m', ''),
                                parts[i+5].replace('s', '')
                            )

                            obs = CometObservation(
                                timestamp=datetime.now(),
                                ra=ra_deg,
                                dec=dec_deg,
                                distance_au=1.8,  # Nominal for Dec 19 flyby
                                velocity_km_s=0.0,
                                magnitude=10.5,
                                elongation=0.0,
                                phase_angle=0.0,
                                source="MPC MPEC"
                            )

                            observations.append(obs)
                            break

                except (ValueError, IndexError):
                    continue

        return observations

    def fetch_orbital_elements(self) -> Optional[CometOrbitalElements]:
        """Fetch current orbital elements from JPL SBDB"""

        sbdb_url = f"https://ssd-api.jpl.nasa.gov/sbdb.api?sstr={self.JPL_ID}"

        try:
            response = self.session.get(sbdb_url, timeout=15)

            if response.status_code != 200:
                return None

            data = response.json()

            # Parse orbital elements
            orbit = data.get('orbit', {})
            elements = orbit.get('elements', [])

            # Extract elements
            elem_dict = {}
            for elem in elements:
                elem_dict[elem['name']] = float(elem['value'])

            # Parse epoch
            epoch_jd = float(orbit.get('epoch', 0))
            epoch = self._jd_to_datetime(epoch_jd)

            # Parse perihelion time
            tp_jd = elem_dict.get('tp', 0)
            tp_datetime = self._jd_to_datetime(tp_jd)

            orbital_elements = CometOrbitalElements(
                epoch=epoch,
                eccentricity=elem_dict.get('e', 0.0),
                perihelion_distance=elem_dict.get('q', 0.0),
                time_of_perihelion=tp_datetime,
                inclination=elem_dict.get('i', 0.0),
                longitude_ascending_node=elem_dict.get('om', 0.0),
                argument_of_perihelion=elem_dict.get('w', 0.0),
                semi_major_axis=elem_dict.get('a', None),
                orbital_period_years=elem_dict.get('per', None)
            )

            return orbital_elements

        except (requests.RequestException, json.JSONDecodeError, KeyError) as e:
            warnings.warn(f"Failed to fetch orbital elements: {e}")
            return None

    def simulate_live_feed(
        self,
        duration_hours: float = 24.0,
        update_rate_hz: float = 0.1
    ) -> List[CometObservation]:
        """
        Simulate live data feed (for testing/demo)

        Args:
            duration_hours: Duration to simulate
            update_rate_hz: Update frequency (Hz)

        Returns:
            List of simulated observations
        """
        observations = []

        # Base position (Dec 19, 2025 closest approach)
        base_ra = 188.5  # ~12h 34m
        base_dec = -56.2  # ~-56° 12'
        base_distance = 1.8  # AU

        num_points = int(duration_hours * 3600 * update_rate_hz)

        for i in range(num_points):
            t = i / (update_rate_hz * 3600)  # hours

            # Add small variations (proper motion + noise)
            ra = base_ra + 0.01 * t + np.random.normal(0, 0.001)
            dec = base_dec + 0.005 * t + np.random.normal(0, 0.001)

            # Distance slowly changing
            distance = base_distance + 0.001 * t

            # Simulate Ni flux (gas production) with variations
            ni_flux = 4.6 + np.random.normal(0, 0.2)  # g/s

            obs = CometObservation(
                timestamp=datetime.now() + timedelta(hours=t),
                ra=ra,
                dec=dec,
                distance_au=distance,
                velocity_km_s=-15.0,  # Approaching
                magnitude=10.5,
                elongation=45.0,
                phase_angle=30.0,
                gas_production_rate=ni_flux,
                tail_length_km=2.5e6,  # 2.5M km
                source="SIMULATED"
            )

            observations.append(obs)

        return observations

    @staticmethod
    def _hms_to_degrees(hours: str, minutes: str, seconds: str) -> float:
        """Convert RA from HMS to degrees"""
        try:
            h = float(hours)
            m = float(minutes)
            s = float(seconds)
            return 15.0 * (h + m/60.0 + s/3600.0)  # 15 deg/hour
        except ValueError:
            return 0.0

    @staticmethod
    def _dms_to_degrees(degrees: str, minutes: str, seconds: str) -> float:
        """Convert Dec from DMS to degrees"""
        try:
            d = float(degrees)
            m = float(minutes)
            s = float(seconds)
            sign = -1 if d < 0 else 1
            return sign * (abs(d) + m/60.0 + s/3600.0)
        except ValueError:
            return 0.0

    @staticmethod
    def _jd_to_datetime(jd: float) -> datetime:
        """Convert Julian Date to datetime"""
        # Simplified conversion
        jd_epoch = 2440587.5  # JD of Unix epoch
        unix_seconds = (jd - jd_epoch) * 86400.0
        return datetime.utcfromtimestamp(unix_seconds)


class RecursivePlanckOperator:
    """
    Recursive Planck Operator for comet data integration

    Implements:
        dn/dt = -μ n + β ∫ α e^{-α τ} n(t-τ) dτ + S(t)

    Where:
        - n: State variable (anomaly metric)
        - μ: Damping constant (0.16905 - Lightfoot constant)
        - S(t): Signal from comet data (flux, position changes)
        - Memory integral: Non-Markovian history
    """

    def __init__(
        self,
        mu: float = 0.16905,
        alpha: float = 1.618,
        beta: float = 0.5,
        D: float = 149.9992314
    ):
        """
        Initialize Recursive Planck Operator

        Args:
            mu: Damping constant (Lightfoot constant)
            alpha: Memory decay rate
            beta: Memory coupling strength
            D: Bounding constant (AU scale)
        """
        self.mu = mu
        self.alpha = alpha
        self.beta = beta
        self.D = D

        self.state = RecursivePlanckState(mu=mu, D=D)
        self.history = []
        self.time_points = []

    def update(
        self,
        observation: CometObservation,
        dt: float = 0.01
    ) -> RecursivePlanckState:
        """
        Update state with new observation

        Args:
            observation: New comet observation
            dt: Time step

        Returns:
            Updated state
        """
        # Extract signal from observation
        # Use gas production rate as primary signal (Ni flux)
        if observation.gas_production_rate is not None:
            signal = observation.gas_production_rate
        else:
            # Fallback: use magnitude variation as signal
            signal = 10.0 - observation.magnitude

        self.state.signal = signal

        # Compute memory integral: ∫ α e^{-α τ} n(t-τ) dτ
        memory_integral = 0.0

        if len(self.history) > 0:
            for i, (t_past, n_past) in enumerate(zip(self.time_points, self.history)):
                tau = observation.timestamp.timestamp() - t_past
                if tau > 0:
                    memory_integral += (
                        self.beta * self.alpha *
                        np.exp(-self.alpha * tau) * n_past * dt
                    )

        self.state.memory_integral = memory_integral

        # Update state: dn/dt = -μ n + memory + S(t)
        dn_dt = -self.mu * self.state.n + memory_integral + signal

        # Euler integration
        n_new = self.state.n + dn_dt * dt

        # Apply bounds [-D, D]
        n_new = max(-self.D, min(self.D, n_new))

        self.state.n = n_new

        # Compute error metric (anomaly detection)
        expected_signal = 4.6  # Expected Ni flux baseline
        self.state.error = abs(signal - expected_signal)

        # Store history
        self.history.append(n_new)
        self.time_points.append(observation.timestamp.timestamp())

        return self.state

    def get_anomaly_score(self) -> float:
        """Compute anomaly score based on error metric"""
        # Normalize error to [0, 1] scale
        max_error = 10.0  # Maximum expected error
        return min(1.0, self.state.error / max_error)


# Example usage and testing
if __name__ == "__main__":
    print("=" * 80)
    print("NASA COMET DATA INTEGRATION - 3I/ATLAS (C/2025 N1)")
    print("=" * 80)
    print()

    # Initialize client
    client = NASACometDataClient()

    # Try fetching live data
    print("🌌 Fetching ephemeris from JPL Horizons...")
    ephemeris = client.fetch_horizons_ephemeris()

    if ephemeris:
        print(f"✅ Retrieved {len(ephemeris)} ephemeris points")
        print(f"   Latest: RA={ephemeris[-1].ra:.4f}°, Dec={ephemeris[-1].dec:.4f}°")
    else:
        print("⚠️  No data from Horizons, using simulated feed")
        ephemeris = client.simulate_live_feed(duration_hours=1.0)

    print()
    print("🔬 Integrating with Recursive Planck Operator...")

    # Initialize operator
    operator = RecursivePlanckOperator()

    # Process observations
    for obs in ephemeris[:10]:  # Process first 10 points
        state = operator.update(obs)
        anomaly = operator.get_anomaly_score()

        print(f"  t={obs.timestamp.strftime('%H:%M:%S')}: "
              f"n={state.n:.4f}, signal={state.signal:.4f}, "
              f"anomaly={anomaly:.4f}")

    print()
    print("=" * 80)
    print("✅ NASA DATA PIPELINE INITIALIZED")
    print("=" * 80)
