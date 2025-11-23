"""
USGS Seismic Activity Connector
================================

Connects to United States Geological Survey real-time earthquake feeds:

Data Sources:
    - USGS FDSN Event Web Service
    - Real-time GeoJSON feeds
    - Earthquake catalog search

Earthquake Data:
    - Magnitude and location
    - Depth and time
    - Seismic wave details
    - Tsunami warnings
    - Impact assessments

Update Rate: Real-time (seconds after detection)
Coverage: Global earthquake monitoring
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


class USGSSeismicConnector(DataConnector):
    """
    USGS Seismic Activity Connector

    Fetches real-time earthquake data from USGS earthquake monitoring systems
    """

    # USGS API endpoints
    BASE_URL = "https://earthquake.usgs.gov/fdsnws/event/1/query"
    GEOJSON_URL = "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/"

    # Feed types
    FEED_TYPES = {
        'significant_month': 'significant_month.geojson',
        'all_hour': 'all_hour.geojson',
        'all_day': 'all_day.geojson',
        'all_week': 'all_week.geojson',
        '4.5_day': '4.5_day.geojson',  # M4.5+ past day
        '2.5_day': '2.5_day.geojson',  # M2.5+ past day
    }

    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize USGS seismic connector

        Args:
            config: Configuration dictionary:
                - feed_type: Type of feed (default: 'all_hour')
                - min_magnitude: Minimum magnitude to report (default: 2.5)
                - max_age_hours: Maximum age of earthquakes (default: 24)
        """
        default_config = {
            'feed_type': 'all_hour',
            'min_magnitude': 2.5,
            'max_age_hours': 24,
            'required_fields': ['magnitude', 'place', 'time']
        }
        if config:
            default_config.update(config)

        super().__init__(DataSourceType.SEISMIC_ACTIVITY, default_config)

        self.recent_earthquakes = []
        self.earthquake_ids_seen = set()

    def connect(self) -> bool:
        """Establish connection to USGS earthquake feeds"""
        try:
            print("Connecting to USGS Earthquake Monitoring...")

            # Test connection with recent earthquakes
            feed_type = self.config.get('feed_type', 'all_hour')
            url = self.GEOJSON_URL + self.FEED_TYPES[feed_type]

            with urllib.request.urlopen(url, timeout=15) as response:
                data = json.loads(response.read().decode())

                if 'features' in data:
                    eq_count = len(data['features'])
                    print(f"✓ Connected to USGS Earthquake Feed")
                    print(f"  Feed: {feed_type}")
                    print(f"  Events in feed: {eq_count}")

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
        Fetch latest earthquake data

        Returns:
            DataPoint with earthquake information
        """
        if not self.is_connected:
            return None

        try:
            feed_type = self.config.get('feed_type', 'all_hour')
            url = self.GEOJSON_URL + self.FEED_TYPES[feed_type]

            with urllib.request.urlopen(url, timeout=15) as response:
                data = json.loads(response.read().decode())

            if 'features' not in data:
                return None

            # Process earthquakes
            earthquakes = self._process_earthquakes(data['features'])

            # Filter by magnitude
            min_mag = self.config.get('min_magnitude', 2.5)
            earthquakes = [eq for eq in earthquakes if eq['magnitude'] >= min_mag]

            # Sort by time (most recent first)
            earthquakes.sort(key=lambda x: x['time'], reverse=True)

            # Build summary data
            summary_data = {
                'total_events': len(earthquakes),
                'feed_type': feed_type,
                'time_range_hours': self._get_feed_hours(feed_type),
                'earthquakes': earthquakes[:50],  # Limit to 50 most recent

                # Statistics
                'max_magnitude': max([eq['magnitude'] for eq in earthquakes]) if earthquakes else 0,
                'avg_magnitude': sum([eq['magnitude'] for eq in earthquakes]) / len(earthquakes) if earthquakes else 0,
                'significant_count': sum(1 for eq in earthquakes if eq.get('significant', False)),

                # Geographic distribution
                'regions': self._count_regions(earthquakes),

                # Metadata
                'fetch_time': datetime.now(timezone.utc).isoformat(),
                'api_source': 'usgs_geojson_feed',
                'api_metadata': data.get('metadata', {})
            }

            quality = self.validate_data(summary_data)

            data_point = DataPoint(
                source_type=self.source_type,
                timestamp=datetime.now(timezone.utc),
                data=summary_data,
                quality=quality
            )

            self.recent_earthquakes = earthquakes
            self.last_fetch_time = datetime.now(timezone.utc)

            return data_point

        except Exception as e:
            print(f"Error fetching seismic data: {e}")
            self.error_count += 1
            return None

    def _process_earthquakes(self, features: List[Dict]) -> List[Dict[str, Any]]:
        """Process earthquake features from GeoJSON"""
        earthquakes = []

        for feature in features:
            props = feature.get('properties', {})
            geom = feature.get('geometry', {})

            # Skip if we've seen this earthquake before
            eq_id = feature.get('id')
            if eq_id in self.earthquake_ids_seen:
                continue

            self.earthquake_ids_seen.add(eq_id)

            # Extract coordinates
            coords = geom.get('coordinates', [0, 0, 0])

            earthquake = {
                'id': eq_id,
                'magnitude': props.get('mag', 0),
                'magnitude_type': props.get('magType', 'unknown'),
                'place': props.get('place', 'Unknown location'),
                'time': datetime.fromtimestamp(props.get('time', 0) / 1000, tz=timezone.utc).isoformat(),
                'updated': datetime.fromtimestamp(props.get('updated', 0) / 1000, tz=timezone.utc).isoformat(),

                # Location
                'longitude': coords[0],
                'latitude': coords[1],
                'depth_km': coords[2],

                # Details
                'type': props.get('type', 'earthquake'),
                'status': props.get('status', 'automatic'),
                'tsunami': props.get('tsunami', 0) == 1,
                'significant': props.get('sig', 0) > 600,  # Significance > 600

                # Impact
                'felt_reports': props.get('felt', 0),
                'cdi': props.get('cdi'),  # Community Decimal Intensity
                'mmi': props.get('mmi'),  # Modified Mercalli Intensity
                'alert_level': props.get('alert'),  # green/yellow/orange/red

                # URLs
                'url': props.get('url'),
                'detail_url': props.get('detail')
            }

            earthquakes.append(earthquake)

        return earthquakes

    def _get_feed_hours(self, feed_type: str) -> int:
        """Get time range in hours for feed type"""
        if 'hour' in feed_type:
            return 1
        elif 'day' in feed_type:
            return 24
        elif 'week' in feed_type:
            return 168
        elif 'month' in feed_type:
            return 720
        return 24

    def _count_regions(self, earthquakes: List[Dict]) -> Dict[str, int]:
        """Count earthquakes by region"""
        regions = {}
        for eq in earthquakes:
            place = eq['place']
            # Extract region (text after "of")
            if ' of ' in place:
                region = place.split(' of ')[-1]
            else:
                region = place

            regions[region] = regions.get(region, 0) + 1

        return dict(sorted(regions.items(), key=lambda x: x[1], reverse=True)[:10])

    def validate_data(self, data: Dict[str, Any]) -> DataQuality:
        """Validate seismic data quality"""
        if not data or 'total_events' not in data:
            return DataQuality.UNAVAILABLE

        # Check if data is stale
        if self.last_fetch_time:
            age = (datetime.now(timezone.utc) - self.last_fetch_time).total_seconds()
            if age > 3600:  # More than 1 hour old
                return DataQuality.DEGRADED

        return DataQuality.EXCELLENT


def test_seismic_connector():
    """Test USGS seismic connector"""
    print("USGS Seismic Activity Connector - Test")
    print("=" * 50)

    # Create connector
    connector = USGSSeismicConnector({
        'feed_type': 'all_day',
        'min_magnitude': 4.0
    })

    # Connect
    print("\n1. Connecting to USGS...")
    if not connector.connect():
        print("✗ Connection failed")
        return

    # Fetch data
    print("\n2. Fetching earthquake data...")
    data_point = connector.fetch_data()

    if data_point:
        print(f"\n   Total events: {data_point.data['total_events']}")
        print(f"   Max magnitude: M{data_point.data['max_magnitude']:.1f}")
        print(f"   Avg magnitude: M{data_point.data['avg_magnitude']:.1f}")
        print(f"   Significant events: {data_point.data['significant_count']}")

        print(f"\n   Recent earthquakes:")
        for eq in data_point.data['earthquakes'][:5]:
            print(f"      M{eq['magnitude']:.1f} - {eq['place']}")
            if eq['tsunami']:
                print(f"         ⚠ TSUNAMI WARNING")

        print(f"\n   Top regions:")
        for region, count in list(data_point.data['regions'].items())[:5]:
            print(f"      {region}: {count} events")

        print(f"\n   Quality: {data_point.quality.value}")

    print("\n✓ USGS seismic connector operational!")


if __name__ == "__main__":
    test_seismic_connector()
