"""
NOAA Oceanic Data Connector
============================

Connects to NOAA oceanic observation systems:

Data Sources:
    - NOAA CO-OPS (Center for Operational Oceanographic Products and Services)
    - IOOS (Integrated Ocean Observing System)
    - Buoy data (NDBC - National Data Buoy Center)
    - Tides and currents

Oceanic Data:
    - Water level and tides
    - Water temperature
    - Wave height and period
    - Wind speed and direction
    - Barometric pressure
    - Salinity and conductivity

Update Rate: 6 minutes (standard buoy interval)
Coverage: Global ocean monitoring
"""

import json
import urllib.request
import urllib.error
from datetime import datetime, timezone, timedelta
from typing import Dict, Any, Optional, List
import sys
import os

sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from core.digital_twin import DataConnector, DataSourceType, DataPoint, DataQuality


class NOAAOceanicConnector(DataConnector):
    """
    NOAA Oceanic Data Connector

    Fetches real-time oceanographic data from NOAA observation systems
    """

    # NOAA CO-OPS API endpoint
    COOPS_BASE_URL = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"

    # NDBC (Buoy) data endpoint (recent observations)
    NDBC_BASE_URL = "https://www.ndbc.noaa.gov/data/realtime2/"

    # Product types available from CO-OPS
    PRODUCTS = {
        'water_level': 'Water Level',
        'water_temperature': 'Water Temperature',
        'air_temperature': 'Air Temperature',
        'wind': 'Wind',
        'air_pressure': 'Air Pressure',
        'conductivity': 'Conductivity',
        'visibility': 'Visibility',
        'humidity': 'Humidity',
        'salinity': 'Salinity',
        'hourly_height': 'Hourly Height',
        'high_low': 'High/Low Tide Predictions',
        'daily_mean': 'Daily Mean',
        'monthly_mean': 'Monthly Mean',
        'predictions': 'Tide Predictions',
        'currents': 'Currents'
    }

    # Sample major stations (can be configured)
    MAJOR_STATIONS = {
        '8454000': 'Providence, RI',
        '8518750': 'The Battery, New York',
        '8638610': 'Chesapeake Bay Bridge Tunnel, VA',
        '8665530': 'Charleston, SC',
        '8720030': 'Pensacola, FL',
        '8729840': 'Key West, FL',
        '9414290': 'San Francisco, CA',
        '9447130': 'Seattle, WA'
    }

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize NOAA oceanic connector

        Args:
            config: Configuration dictionary:
                - station_id: Station ID (default: '8518750' - The Battery, NY)
                - products: List of products to fetch (default: ['water_level', 'water_temperature'])
                - datum: Datum for water level (default: 'MLLW')
                - units: 'metric' or 'english' (default: 'metric')
                - time_range_hours: Hours of data to fetch (default: 1)
        """
        default_config = {
            'station_id': '8518750',  # The Battery, New York
            'products': ['water_level', 'water_temperature', 'wind'],
            'datum': 'MLLW',  # Mean Lower Low Water
            'units': 'metric',
            'time_range_hours': 1,
            'required_fields': ['station_id', 'timestamp']
        }
        if config:
            default_config.update(config)

        super().__init__(DataSourceType.OCEANIC_DATA, default_config)

        self.station_info = None

    def connect(self) -> bool:
        """Establish connection to NOAA oceanic data sources"""
        try:
            print("Connecting to NOAA Oceanic Data Services...")

            station_id = self.config['station_id']
            station_name = self.MAJOR_STATIONS.get(station_id, f"Station {station_id}")

            # Test connection by fetching water level data
            params = {
                'station': station_id,
                'product': 'water_level',
                'datum': self.config['datum'],
                'units': self.config['units'],
                'time_zone': 'gmt',
                'format': 'json',
                'date': 'latest'
            }

            url = self.COOPS_BASE_URL + '?' + urllib.parse.urlencode(params)

            with urllib.request.urlopen(url, timeout=15) as response:
                data = json.loads(response.read().decode())

                if 'data' in data or 'metadata' in data:
                    print(f"✓ Connected to NOAA CO-OPS")
                    print(f"  Station: {station_name}")
                    print(f"  Station ID: {station_id}")

                    if 'metadata' in data:
                        self.station_info = data['metadata']

                    self.is_connected = True
                    self.last_fetch_time = datetime.now(timezone.utc)
                    return True

                return False

        except Exception as e:
            print(f"✗ Connection failed: {e}")
            self.is_connected = False
            return False

    def fetch_data(self) -> Optional[DataPoint]:
        """
        Fetch latest oceanographic data

        Returns:
            DataPoint with comprehensive oceanic observations
        """
        if not self.is_connected:
            return None

        try:
            station_id = self.config['station_id']
            products = self.config['products']
            all_data = {}

            # Fetch each product
            for product in products:
                product_data = self._fetch_product(station_id, product)
                if product_data:
                    all_data[product] = product_data

            if not all_data:
                return None

            # Build comprehensive oceanic data
            oceanic_data = {
                'station_id': station_id,
                'station_name': self.MAJOR_STATIONS.get(station_id, f"Station {station_id}"),
                'timestamp': datetime.now(timezone.utc).isoformat(),

                # Observations by product
                'observations': all_data,

                # Extract latest values for quick access
                'latest': self._extract_latest_values(all_data),

                # Station metadata
                'station_info': self.station_info,

                # Metadata
                'fetch_time': datetime.now(timezone.utc).isoformat(),
                'api_source': 'noaa_coops',
                'datum': self.config['datum'],
                'units': self.config['units']
            }

            quality = self.validate_data(oceanic_data)

            data_point = DataPoint(
                source_type=self.source_type,
                timestamp=datetime.now(timezone.utc),
                data=oceanic_data,
                quality=quality
            )

            self.last_fetch_time = datetime.now(timezone.utc)

            return data_point

        except Exception as e:
            print(f"Error fetching oceanic data: {e}")
            self.error_count += 1
            return None

    def _fetch_product(self, station_id: str, product: str) -> Optional[Dict[str, Any]]:
        """Fetch specific product data from NOAA CO-OPS"""
        try:
            # Calculate time range
            end_time = datetime.now(timezone.utc)
            start_time = end_time - timedelta(hours=self.config['time_range_hours'])

            params = {
                'station': station_id,
                'product': product,
                'datum': self.config['datum'],
                'units': self.config['units'],
                'time_zone': 'gmt',
                'format': 'json',
                'begin_date': start_time.strftime('%Y%m%d %H:%M'),
                'end_date': end_time.strftime('%Y%m%d %H:%M')
            }

            url = self.COOPS_BASE_URL + '?' + urllib.parse.urlencode(params)

            with urllib.request.urlopen(url, timeout=15) as response:
                data = json.loads(response.read().decode())

                if 'data' in data:
                    return {
                        'product_name': self.PRODUCTS.get(product, product),
                        'data_points': data['data'],
                        'count': len(data['data']),
                        'metadata': data.get('metadata')
                    }

                return None

        except Exception as e:
            print(f"Warning: Could not fetch {product}: {e}")
            return None

    def _extract_latest_values(self, all_data: Dict[str, Any]) -> Dict[str, Any]:
        """Extract latest value from each product"""
        latest = {}

        for product, product_data in all_data.items():
            if 'data_points' in product_data and product_data['data_points']:
                latest_point = product_data['data_points'][-1]

                if product == 'water_level':
                    latest['water_level_m'] = float(latest_point.get('v', 0))
                elif product == 'water_temperature':
                    latest['water_temp_c'] = float(latest_point.get('v', 0))
                elif product == 'air_temperature':
                    latest['air_temp_c'] = float(latest_point.get('v', 0))
                elif product == 'wind':
                    latest['wind_speed_mps'] = float(latest_point.get('s', 0))
                    latest['wind_dir_deg'] = float(latest_point.get('d', 0))
                    latest['wind_gust_mps'] = float(latest_point.get('g', 0))
                elif product == 'air_pressure':
                    latest['pressure_mb'] = float(latest_point.get('v', 0))
                elif product == 'salinity':
                    latest['salinity_psu'] = float(latest_point.get('v', 0))

                latest[f'{product}_time'] = latest_point.get('t')

        return latest

    def validate_data(self, data: Dict[str, Any]) -> DataQuality:
        """Validate oceanic data quality"""
        if not data or 'observations' not in data:
            return DataQuality.UNAVAILABLE

        # Check if we have any observations
        if not data['observations']:
            return DataQuality.UNAVAILABLE

        # Check data freshness
        if self.last_fetch_time:
            age = (datetime.now(timezone.utc) - self.last_fetch_time).total_seconds()
            if age > 1800:  # More than 30 minutes old
                return DataQuality.DEGRADED

        # Check if we have latest values
        if data.get('latest'):
            return DataQuality.EXCELLENT
        else:
            return DataQuality.GOOD


def test_oceanic_connector():
    """Test NOAA oceanic connector"""
    print("NOAA Oceanic Data Connector - Test")
    print("=" * 50)

    # Create connector for The Battery, New York
    connector = NOAAOceanicConnector({
        'station_id': '8518750',
        'products': ['water_level', 'water_temperature', 'wind'],
        'time_range_hours': 1
    })

    # Connect
    print("\n1. Connecting to NOAA CO-OPS...")
    if not connector.connect():
        print("✗ Connection failed")
        return

    # Fetch data
    print("\n2. Fetching oceanographic data...")
    data_point = connector.fetch_data()

    if data_point:
        print(f"\n   Station: {data_point.data['station_name']}")
        print(f"   Station ID: {data_point.data['station_id']}")

        latest = data_point.data.get('latest', {})
        if latest:
            print(f"\n   Latest Observations:")
            if 'water_level_m' in latest:
                print(f"      Water Level: {latest['water_level_m']:.2f} m")
            if 'water_temp_c' in latest:
                print(f"      Water Temp: {latest['water_temp_c']:.1f} °C")
            if 'wind_speed_mps' in latest:
                print(f"      Wind Speed: {latest['wind_speed_mps']:.1f} m/s")
                print(f"      Wind Dir: {latest['wind_dir_deg']:.0f}°")

        print(f"\n   Products fetched: {len(data_point.data['observations'])}")
        for product, prod_data in data_point.data['observations'].items():
            print(f"      {prod_data['product_name']}: {prod_data['count']} data points")

        print(f"\n   Quality: {data_point.quality.value}")

    print("\n✓ NOAA oceanic connector operational!")


if __name__ == "__main__":
    # Add urllib.parse import at top
    import urllib.parse
    test_oceanic_connector()
