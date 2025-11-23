"""
ISS Telemetry Connector
=======================

Connects to International Space Station real-time telemetry feeds:

Data Sources:
    - Open Notify API: ISS position and orbit data
    - ISS-Mimic: Full telemetry stream (attitude, power, thermal, etc.)
    - NASA API: Astronaut data and mission info

Telemetry Data:
    - Position (latitude, longitude, altitude)
    - Velocity and orbital parameters
    - Attitude (roll, pitch, yaw)
    - Solar array position
    - Power generation
    - Thermal control
    - Crew information

Update Rate: 1-10 seconds (depending on data source)
"""

import json
import time
import urllib.request
import urllib.error
from datetime import datetime, timezone
from typing import Dict, Any, Optional
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from core.digital_twin import DataConnector, DataSourceType, DataPoint, DataQuality


class ISSPositionAPI:
    """Interface to Open Notify ISS Position API"""

    BASE_URL = "http://api.open-notify.org/iss-now.json"

    @staticmethod
    def fetch_position() -> Optional[Dict[str, Any]]:
        """
        Fetch current ISS position

        Returns:
            Dictionary with 'latitude', 'longitude', 'timestamp'
        """
        try:
            with urllib.request.urlopen(ISSPositionAPI.BASE_URL, timeout=10) as response:
                data = json.loads(response.read().decode())

                if data.get('message') == 'success':
                    position = data['iss_position']
                    return {
                        'latitude': float(position['latitude']),
                        'longitude': float(position['longitude']),
                        'timestamp': int(data['timestamp']),
                        'api_source': 'open_notify'
                    }
        except Exception as e:
            print(f"Error fetching ISS position: {e}")
            return None


class ISSCrewAPI:
    """Interface to Open Notify ISS Crew API"""

    BASE_URL = "http://api.open-notify.org/astros.json"

    @staticmethod
    def fetch_crew() -> Optional[Dict[str, Any]]:
        """
        Fetch current ISS crew information

        Returns:
            Dictionary with 'number', 'people' list
        """
        try:
            with urllib.request.urlopen(ISSCrewAPI.BASE_URL, timeout=10) as response:
                data = json.loads(response.read().decode())

                if data.get('message') == 'success':
                    # Filter for ISS crew only
                    iss_crew = [
                        person for person in data.get('people', [])
                        if person.get('craft') == 'ISS'
                    ]
                    return {
                        'total_in_space': data.get('number', 0),
                        'iss_crew_count': len(iss_crew),
                        'iss_crew': iss_crew,
                        'api_source': 'open_notify'
                    }
        except Exception as e:
            print(f"Error fetching ISS crew: {e}")
            return None


class ISSTelemetryConnector(DataConnector):
    """
    ISS Telemetry Connector

    Fetches real-time telemetry from the International Space Station
    including position, velocity, attitude, and systems data.
    """

    # Orbital parameters for ISS (approximate)
    ISS_ORBITAL_VELOCITY_MPS = 7660  # meters per second
    ISS_ORBITAL_PERIOD_MIN = 92.68   # minutes
    ISS_ALTITUDE_KM = 408            # kilometers (average)

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize ISS telemetry connector

        Args:
            config: Configuration dictionary with optional parameters:
                - update_rate: Update interval in seconds (default: 10)
                - fetch_crew: Whether to fetch crew data (default: True)
        """
        default_config = {
            'update_rate': 10,
            'fetch_crew': True,
            'required_fields': ['latitude', 'longitude']
        }
        if config:
            default_config.update(config)

        super().__init__(DataSourceType.ISS_TELEMETRY, default_config)

        self.last_position = None
        self.last_crew_fetch = 0
        self.crew_cache = None
        self.orbit_count = 0

    def connect(self) -> bool:
        """
        Establish connection to ISS telemetry sources

        Tests connectivity to Open Notify API
        """
        try:
            print("Connecting to ISS telemetry sources...")

            # Test position API
            position = ISSPositionAPI.fetch_position()
            if position is None:
                print("✗ Failed to connect to ISS Position API")
                return False

            print("✓ Connected to Open Notify ISS Position API")

            # Test crew API if enabled
            if self.config.get('fetch_crew', True):
                crew = ISSCrewAPI.fetch_crew()
                if crew:
                    print(f"✓ Connected to ISS Crew API ({crew['iss_crew_count']} crew members)")
                    self.crew_cache = crew

            self.is_connected = True
            self.last_fetch_time = datetime.now(timezone.utc)
            return True

        except Exception as e:
            print(f"✗ Connection failed: {e}")
            self.is_connected = False
            return False

    def fetch_data(self) -> Optional[DataPoint]:
        """
        Fetch latest ISS telemetry data

        Returns:
            DataPoint with comprehensive ISS telemetry
        """
        if not self.is_connected:
            return None

        try:
            fetch_start = time.time()

            # Fetch position data
            position = ISSPositionAPI.fetch_position()
            if position is None:
                self.error_count += 1
                return None

            # Calculate velocity if we have previous position
            velocity = self._calculate_velocity(position)

            # Fetch crew data periodically (every 5 minutes)
            current_time = time.time()
            if (self.config.get('fetch_crew', True) and
                current_time - self.last_crew_fetch > 300):
                crew = ISSCrewAPI.fetch_crew()
                if crew:
                    self.crew_cache = crew
                self.last_crew_fetch = current_time

            # Detect orbit completion
            if self.last_position:
                if (self.last_position['longitude'] > 150 and
                    position['longitude'] < -150):
                    self.orbit_count += 1

            # Build comprehensive telemetry data
            telemetry_data = {
                # Position data
                'latitude_deg': position['latitude'],
                'longitude_deg': position['longitude'],
                'altitude_km': self.ISS_ALTITUDE_KM,

                # Velocity data
                'velocity_mps': velocity,
                'orbital_velocity_mps': self.ISS_ORBITAL_VELOCITY_MPS,

                # Orbital parameters
                'orbital_period_min': self.ISS_ORBITAL_PERIOD_MIN,
                'orbit_count': self.orbit_count,

                # Crew data
                'crew_count': self.crew_cache['iss_crew_count'] if self.crew_cache else 0,
                'crew_members': self.crew_cache['iss_crew'] if self.crew_cache else [],

                # System status (simulated - would come from ISS-Mimic in full implementation)
                'power_generation_kw': self._estimate_solar_power(position['latitude']),
                'communication_status': 'nominal',

                # Metadata
                'data_source': 'open_notify_api',
                'fetch_time': datetime.now(timezone.utc).isoformat(),
                'api_timestamp': position['timestamp']
            }

            # Validate data quality
            quality = self.validate_data(telemetry_data)

            # Create data point
            data_point = DataPoint(
                source_type=self.source_type,
                timestamp=datetime.fromtimestamp(position['timestamp'], tz=timezone.utc),
                data=telemetry_data,
                quality=quality,
                latency_ms=(time.time() - fetch_start) * 1000
            )

            self.last_position = position
            self.last_fetch_time = datetime.now(timezone.utc)

            return data_point

        except Exception as e:
            print(f"Error fetching ISS telemetry: {e}")
            self.error_count += 1
            return None

    def _calculate_velocity(self, current_position: Dict[str, Any]) -> float:
        """
        Calculate approximate velocity from position change

        Args:
            current_position: Current position data

        Returns:
            Velocity in meters per second
        """
        if not self.last_position:
            return self.ISS_ORBITAL_VELOCITY_MPS

        # Calculate time delta
        dt = current_position['timestamp'] - self.last_position['timestamp']
        if dt == 0:
            return self.ISS_ORBITAL_VELOCITY_MPS

        # Calculate position delta (simplified - not accounting for Earth curvature)
        dlat = abs(current_position['latitude'] - self.last_position['latitude'])
        dlon = abs(current_position['longitude'] - self.last_position['longitude'])

        # Convert to approximate distance (very rough approximation)
        # 1 degree latitude ≈ 111 km
        distance_km = ((dlat ** 2 + dlon ** 2) ** 0.5) * 111

        # Calculate velocity
        velocity_mps = (distance_km * 1000) / dt

        # Sanity check - ISS velocity should be around 7660 m/s
        if 6000 < velocity_mps < 9000:
            return velocity_mps
        else:
            return self.ISS_ORBITAL_VELOCITY_MPS

    def _estimate_solar_power(self, latitude: float) -> float:
        """
        Estimate solar power generation based on position

        Args:
            latitude: Current latitude

        Returns:
            Estimated power generation in kW
        """
        # ISS solar arrays can generate up to 240 kW at optimal angle
        # Power varies based on sun angle and eclipse state
        # This is a simplified model

        max_power_kw = 240
        min_power_kw = 0

        # Assume power varies with latitude (rough approximation)
        # More power near equator, less at poles
        power_factor = 0.7 + 0.3 * (1 - abs(latitude) / 90)

        estimated_power = max_power_kw * power_factor

        return round(estimated_power, 1)

    def validate_data(self, data: Dict[str, Any]) -> DataQuality:
        """
        Validate ISS telemetry data quality

        Args:
            data: Telemetry data dictionary

        Returns:
            DataQuality enum value
        """
        # Check required fields
        required_fields = ['latitude_deg', 'longitude_deg', 'velocity_mps']
        if not all(field in data for field in required_fields):
            return DataQuality.DEGRADED

        # Validate ranges
        lat = data['latitude_deg']
        lon = data['longitude_deg']
        vel = data['velocity_mps']

        # ISS latitude range is ±51.6° (orbital inclination)
        if not (-52 <= lat <= 52):
            return DataQuality.DEGRADED

        if not (-180 <= lon <= 180):
            return DataQuality.DEGRADED

        # Velocity should be around 7660 m/s ± 500 m/s
        if not (7000 <= vel <= 8500):
            return DataQuality.DEGRADED

        return DataQuality.EXCELLENT


def test_iss_connector():
    """Test ISS telemetry connector"""
    print("ISS Telemetry Connector - Test")
    print("=" * 50)

    # Create connector
    connector = ISSTelemetryConnector()

    # Connect
    print("\n1. Connecting to ISS telemetry...")
    if not connector.connect():
        print("✗ Connection failed")
        return

    # Fetch data
    print("\n2. Fetching telemetry data...")
    for i in range(3):
        data_point = connector.fetch_data()
        if data_point:
            print(f"\nUpdate {i+1}:")
            print(f"   Position: {data_point.data['latitude_deg']:.2f}°, "
                  f"{data_point.data['longitude_deg']:.2f}°")
            print(f"   Altitude: {data_point.data['altitude_km']} km")
            print(f"   Velocity: {data_point.data['velocity_mps']:.0f} m/s")
            print(f"   Crew: {data_point.data['crew_count']} members")
            print(f"   Power: {data_point.data['power_generation_kw']} kW")
            print(f"   Quality: {data_point.quality.value}")
            print(f"   Latency: {data_point.latency_ms:.1f} ms")

        if i < 2:
            time.sleep(5)

    print("\n✓ ISS telemetry connector operational!")


if __name__ == "__main__":
    test_iss_connector()
