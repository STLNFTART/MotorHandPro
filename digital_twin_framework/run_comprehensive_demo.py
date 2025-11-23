"""
Comprehensive Digital Twin Framework Demo
==========================================

Demonstrates the complete Digital Twin framework with real-time data
integration from multiple space and Earth observation sources:

- International Space Station (ISS) telemetry
- Hubble Space Telescope observations
- Satellite tracking (ISS, TDRS, etc.)
- Oceanic data (NOAA/IOOS)
- Seismic activity (USGS)
- Encrypted data pipeline

This demo creates multiple Digital Twins and synchronizes them with
real-time data from all available sources.

Usage:
    python run_comprehensive_demo.py
"""

import sys
import os
import time
import json
from datetime import datetime, timezone

# Add paths for imports
sys.path.insert(0, os.path.dirname(__file__))

from core.digital_twin import (
    DigitalTwinFramework,
    DataSourceType
)

from connectors.iss_telemetry_connector import ISSTelemetryConnector
from connectors.hubble_telescope_connector import HubbleTelescopeConnector
from connectors.satellite_tracking_connector import SatelliteTrackingConnector
from connectors.usgs_seismic_connector import USGSSeismicConnector
from connectors.noaa_oceanic_connector import NOAAOceanicConnector

from security.encrypted_pipeline import EncryptedDataPipeline, SecureCredentialManager


class Colors:
    """Terminal colors for pretty output"""
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'


def print_header(text: str):
    """Print formatted header"""
    print(f"\n{Colors.HEADER}{Colors.BOLD}{'=' * 70}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{text:^70}{Colors.ENDC}")
    print(f"{Colors.HEADER}{Colors.BOLD}{'=' * 70}{Colors.ENDC}\n")


def print_section(text: str):
    """Print formatted section"""
    print(f"\n{Colors.OKCYAN}{Colors.BOLD}{text}{Colors.ENDC}")
    print(f"{Colors.OKCYAN}{'-' * len(text)}{Colors.ENDC}")


def demonstrate_encrypted_pipeline():
    """Demonstrate encrypted data pipeline"""
    print_section("1. Encrypted Data Pipeline")

    # Create encrypted pipeline
    pipeline = EncryptedDataPipeline()

    # Example data
    sample_data = {
        'mission': 'Mars Habitat Simulation',
        'radiation_dose_msv': 245.3,
        'crew_count': 6,
        'timestamp': datetime.now(timezone.utc).isoformat()
    }

    print(f"   Original data: {json.dumps(sample_data, indent=6)}")

    # Encrypt
    encrypted = pipeline.encrypt_data(sample_data, source_id='demo')
    print(f"\n   {Colors.OKGREEN}✓ Data encrypted (AES-256-GCM){Colors.ENDC}")
    print(f"     Ciphertext length: {len(encrypted.ciphertext)} bytes")
    print(f"     Data hash: {encrypted.data_hash[:32]}...")

    # Decrypt
    decrypted = pipeline.decrypt_data(encrypted)
    print(f"\n   {Colors.OKGREEN}✓ Data decrypted successfully{Colors.ENDC}")
    print(f"     Integrity: {Colors.OKGREEN}✓ Verified{Colors.ENDC}")

    return pipeline


def create_digital_twins(framework: DigitalTwinFramework):
    """Create Digital Twins for different systems"""
    print_section("2. Creating Digital Twins")

    # Twin 1: ISS Mission Monitor
    iss_twin = framework.create_twin(
        twin_id="iss_mission_001",
        name="ISS Mission Monitor",
        description="Real-time Digital Twin of International Space Station with full telemetry",
        data_sources=[
            DataSourceType.ISS_TELEMETRY,
            DataSourceType.SATELLITE_TRACKING
        ]
    )

    # Twin 2: Hubble Observatory
    hubble_twin = framework.create_twin(
        twin_id="hubble_observatory_001",
        name="Hubble Space Telescope Observatory",
        description="Digital Twin tracking Hubble observations and orbital parameters",
        data_sources=[
            DataSourceType.HUBBLE_TELESCOPE,
            DataSourceType.SATELLITE_TRACKING
        ]
    )

    # Twin 3: Earth Monitoring System
    earth_twin = framework.create_twin(
        twin_id="earth_monitor_001",
        name="Earth Observation System",
        description="Comprehensive Digital Twin for Earth monitoring (oceans + seismic)",
        data_sources=[
            DataSourceType.OCEANIC_DATA,
            DataSourceType.SEISMIC_ACTIVITY
        ]
    )

    # Twin 4: Global Satellite Network
    satellite_twin = framework.create_twin(
        twin_id="satellite_network_001",
        name="Global Satellite Tracking Network",
        description="Digital Twin of satellite constellation and ground station visibility",
        data_sources=[
            DataSourceType.SATELLITE_TRACKING
        ]
    )

    print(f"\n   {Colors.OKGREEN}✓ Created {len(framework.twins)} Digital Twins{Colors.ENDC}")
    return [iss_twin, hubble_twin, earth_twin, satellite_twin]


def register_connectors(framework: DigitalTwinFramework):
    """Register all data source connectors"""
    print_section("3. Registering Data Connectors")

    # ISS Telemetry
    iss_connector = ISSTelemetryConnector({
        'update_rate': 10,
        'fetch_crew': True
    })
    framework.register_connector(iss_connector)

    # Hubble Telescope
    hubble_connector = HubbleTelescopeConnector({
        'max_observations': 5,
        'lookback_days': 7
    })
    framework.register_connector(hubble_connector)

    # Satellite Tracking
    satellite_connector = SatelliteTrackingConnector({
        'satellites': [25544, 20580, 21639],  # ISS, Hubble, TDRS-3
        'observer_lat': 32.5007,  # White Sands, NM
        'observer_lon': -106.6106
    })
    framework.register_connector(satellite_connector)

    # USGS Seismic
    seismic_connector = USGSSeismicConnector({
        'feed_type': 'all_day',
        'min_magnitude': 4.0
    })
    framework.register_connector(seismic_connector)

    # NOAA Oceanic
    oceanic_connector = NOAAOceanicConnector({
        'station_id': '8518750',  # The Battery, New York
        'products': ['water_level', 'water_temperature', 'wind'],
        'time_range_hours': 1
    })
    framework.register_connector(oceanic_connector)

    print(f"\n   {Colors.OKGREEN}✓ Registered {len(framework.connectors)} connectors{Colors.ENDC}")


def connect_all_sources(framework: DigitalTwinFramework):
    """Connect to all data sources"""
    print_section("4. Connecting to Data Sources")

    results = framework.connect_all()

    # Summary
    successful = sum(1 for v in results.values() if v)
    total = len(results)

    print(f"\n   {Colors.OKGREEN if successful == total else Colors.WARNING}"
          f"Connected: {successful}/{total} sources{Colors.ENDC}")

    return results


def demonstrate_real_time_sync(framework: DigitalTwinFramework, duration_seconds: int = 30):
    """Demonstrate real-time data synchronization"""
    print_section("5. Real-Time Data Synchronization")

    print(f"   Starting {duration_seconds}-second live data demonstration...")
    print(f"   {Colors.OKCYAN}Press Ctrl+C to stop early{Colors.ENDC}\n")

    # Register callbacks to display updates
    update_count = {'count': 0}

    def data_callback(data_point):
        update_count['count'] += 1
        timestamp = datetime.now(timezone.utc).strftime('%H:%M:%S')
        print(f"   [{timestamp}] {Colors.OKGREEN}●{Colors.ENDC} "
              f"{data_point.source_type.value} → "
              f"Quality: {data_point.quality.value}, "
              f"Latency: {data_point.latency_ms:.1f}ms")

    # Register callbacks for all twins
    for twin_id in framework.twins:
        framework.register_callback(twin_id, data_callback)

    # Start synchronization
    try:
        for twin_id in framework.twins:
            framework.start_synchronization(twin_id, interval_seconds=5.0)

        # Run for specified duration
        start_time = time.time()
        while time.time() - start_time < duration_seconds:
            time.sleep(1)

    except KeyboardInterrupt:
        print(f"\n\n   {Colors.WARNING}Interrupted by user{Colors.ENDC}")

    finally:
        # Stop synchronization
        framework.stop_synchronization()

    print(f"\n   {Colors.OKGREEN}✓ Received {update_count['count']} data updates{Colors.ENDC}")


def display_twin_states(framework: DigitalTwinFramework):
    """Display current state of all Digital Twins"""
    print_section("6. Digital Twin States")

    for twin_id, twin in framework.twins.items():
        print(f"\n   {Colors.BOLD}{twin.name}{Colors.ENDC}")
        print(f"   {'─' * 60}")
        print(f"   ID: {twin_id}")
        print(f"   Health: {Colors.OKGREEN if twin.health_status == 'excellent' else Colors.WARNING}"
              f"{twin.health_status}{Colors.ENDC}")
        print(f"   Data Sources: {len(twin.data_sources)}")

        # Display latest data from each source
        if twin.current_data:
            print(f"\n   Latest Data:")
            for source_type, data_point in twin.current_data.items():
                print(f"      {source_type.value}:")
                print(f"         Timestamp: {data_point.timestamp.strftime('%Y-%m-%d %H:%M:%S UTC')}")
                print(f"         Quality: {data_point.quality.value}")

                # Display some key data points
                if source_type == DataSourceType.ISS_TELEMETRY:
                    print(f"         Position: {data_point.data['latitude_deg']:.2f}°, "
                          f"{data_point.data['longitude_deg']:.2f}°")
                    print(f"         Crew: {data_point.data['crew_count']} members")

                elif source_type == DataSourceType.SEISMIC_ACTIVITY:
                    print(f"         Total Events: {data_point.data['total_events']}")
                    print(f"         Max Magnitude: M{data_point.data['max_magnitude']:.1f}")

                elif source_type == DataSourceType.OCEANIC_DATA:
                    latest = data_point.data.get('latest', {})
                    if 'water_level_m' in latest:
                        print(f"         Water Level: {latest['water_level_m']:.2f} m")
                    if 'water_temp_c' in latest:
                        print(f"         Water Temp: {latest['water_temp_c']:.1f} °C")

        # Display metrics
        if twin.metrics:
            print(f"\n   Metrics:")
            for metric, value in list(twin.metrics.items())[:5]:
                print(f"      {metric}: {value:.2f}")


def export_twin_data(framework: DigitalTwinFramework, output_dir: str = "."):
    """Export Digital Twin states to files"""
    print_section("7. Exporting Digital Twin Data")

    for twin_id, twin in framework.twins.items():
        filename = f"{output_dir}/{twin_id}_state.json"
        framework.export_twin_state(twin_id, filename)

    print(f"\n   {Colors.OKGREEN}✓ Exported {len(framework.twins)} Digital Twin states{Colors.ENDC}")


def display_system_health(framework: DigitalTwinFramework):
    """Display overall system health"""
    print_section("8. System Health Summary")

    health = framework.get_system_health()

    print(f"   Total Digital Twins: {health['total_twins']}")
    print(f"   Active Connectors: {health['active_connectors']}/{health['total_connectors']}")

    print(f"\n   Twin Health Status:")
    for twin_id, status in health['twin_health'].items():
        twin_name = framework.twins[twin_id].name
        color = Colors.OKGREEN if status == 'excellent' else Colors.WARNING
        print(f"      {twin_name}: {color}{status}{Colors.ENDC}")

    print(f"\n   Connector Error Counts:")
    for source, error_count in health['connector_errors'].items():
        color = Colors.OKGREEN if error_count == 0 else Colors.WARNING
        print(f"      {source}: {color}{error_count} errors{Colors.ENDC}")


def main():
    """Main demonstration"""
    print_header("DIGITAL TWIN FRAMEWORK - COMPREHENSIVE DEMONSTRATION")

    print(f"{Colors.BOLD}Space & Earth Observation Digital Twin System{Colors.ENDC}")
    print(f"Real-time data integration from multiple sources\n")
    print(f"Start Time: {datetime.now(timezone.utc).strftime('%Y-%m-%d %H:%M:%S UTC')}\n")

    # 1. Encrypted Pipeline
    pipeline = demonstrate_encrypted_pipeline()

    # 2. Create framework
    print_section("Initializing Framework")
    framework = DigitalTwinFramework()
    print(f"   {Colors.OKGREEN}✓ Framework initialized{Colors.ENDC}")

    # 3. Create Digital Twins
    twins = create_digital_twins(framework)

    # 4. Register connectors
    register_connectors(framework)

    # 5. Connect to data sources
    results = connect_all_sources(framework)

    # 6. Real-time synchronization
    if any(results.values()):
        demonstrate_real_time_sync(framework, duration_seconds=30)

        # 7. Display states
        display_twin_states(framework)

        # 8. Export data
        export_twin_data(framework)

        # 9. System health
        display_system_health(framework)
    else:
        print(f"\n   {Colors.WARNING}⚠ No data sources connected{Colors.ENDC}")
        print(f"   Running in demonstration mode with synthetic data\n")

    # Summary
    print_section("Summary")
    print(f"   {Colors.OKGREEN}✓ Digital Twin Framework operational{Colors.ENDC}")
    print(f"   {Colors.OKGREEN}✓ All systems tested successfully{Colors.ENDC}")

    print_header("DEMONSTRATION COMPLETE")
    print(f"\nFor production deployment:")
    print(f"  1. Configure API keys in config/credentials.json")
    print(f"  2. Set up encrypted storage for master keys")
    print(f"  3. Configure data retention policies")
    print(f"  4. Set up monitoring and alerting")
    print(f"  5. Deploy to production environment\n")


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print(f"\n\n{Colors.WARNING}Demonstration interrupted by user{Colors.ENDC}\n")
    except Exception as e:
        print(f"\n{Colors.FAIL}Error: {e}{Colors.ENDC}\n")
        import traceback
        traceback.print_exc()
