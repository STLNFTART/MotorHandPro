"""
Digital Twin Framework - Core Architecture
==========================================

A comprehensive framework for creating Digital Twins of space and Earth systems
with real-time data integration from multiple sources:

- International Space Station (ISS) telemetry
- Hubble Space Telescope observations
- Ground satellite relay stations
- Oceanic data (NOAA/IOOS)
- Seismic activity (USGS)
- Satellite tracking and TLE data

Architecture:
    Digital Twin = Virtual replica of physical system
    Connectors = Modular data source interfaces
    Sync Engine = Real-time data synchronization
    Security = Encrypted data pipelines

Author: PRIMAL Logic Integration Team
License: MIT
"""

import json
import time
import threading
from datetime import datetime, timezone
from typing import Dict, List, Any, Optional, Callable
from dataclasses import dataclass, field, asdict
from enum import Enum
import hashlib


class DataSourceType(Enum):
    """Enumeration of supported data sources"""
    ISS_TELEMETRY = "iss_telemetry"
    HUBBLE_TELESCOPE = "hubble_telescope"
    SATELLITE_TRACKING = "satellite_tracking"
    OCEANIC_DATA = "oceanic_data"
    SEISMIC_ACTIVITY = "seismic_activity"
    GROUND_STATIONS = "ground_stations"


class DataQuality(Enum):
    """Data quality indicators"""
    EXCELLENT = "excellent"
    GOOD = "good"
    DEGRADED = "degraded"
    UNAVAILABLE = "unavailable"


@dataclass
class DataPoint:
    """Represents a single data point from any source"""
    source_type: DataSourceType
    timestamp: datetime
    data: Dict[str, Any]
    quality: DataQuality = DataQuality.GOOD
    latency_ms: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization"""
        result = asdict(self)
        result['source_type'] = self.source_type.value
        result['quality'] = self.quality.value
        result['timestamp'] = self.timestamp.isoformat()
        return result

    def compute_hash(self) -> str:
        """Compute SHA-256 hash for data integrity"""
        data_str = json.dumps(self.data, sort_keys=True)
        return hashlib.sha256(data_str.encode()).hexdigest()


@dataclass
class DigitalTwinState:
    """Current state of a Digital Twin"""
    twin_id: str
    name: str
    description: str
    created_at: datetime
    last_updated: datetime
    data_sources: List[DataSourceType]
    current_data: Dict[DataSourceType, DataPoint] = field(default_factory=dict)
    historical_data: List[DataPoint] = field(default_factory=list)
    health_status: str = "initializing"
    metrics: Dict[str, float] = field(default_factory=dict)

    def update_data(self, data_point: DataPoint):
        """Update Digital Twin with new data point"""
        self.current_data[data_point.source_type] = data_point
        self.historical_data.append(data_point)
        self.last_updated = datetime.now(timezone.utc)
        self.update_metrics()

    def update_metrics(self):
        """Compute real-time metrics about data flow"""
        if not self.historical_data:
            return

        # Calculate average latency per source
        latencies = {}
        for dp in self.historical_data[-100:]:  # Last 100 points
            source = dp.source_type
            if source not in latencies:
                latencies[source] = []
            latencies[source].append(dp.latency_ms)

        for source, lats in latencies.items():
            avg_latency = sum(lats) / len(lats) if lats else 0
            self.metrics[f"{source.value}_avg_latency_ms"] = avg_latency

        # Data quality score
        recent_points = self.historical_data[-50:]
        quality_scores = {
            DataQuality.EXCELLENT: 1.0,
            DataQuality.GOOD: 0.8,
            DataQuality.DEGRADED: 0.5,
            DataQuality.UNAVAILABLE: 0.0
        }

        if recent_points:
            avg_quality = sum(quality_scores[dp.quality] for dp in recent_points) / len(recent_points)
            self.metrics['overall_data_quality'] = avg_quality

            if avg_quality >= 0.9:
                self.health_status = "excellent"
            elif avg_quality >= 0.7:
                self.health_status = "good"
            elif avg_quality >= 0.5:
                self.health_status = "degraded"
            else:
                self.health_status = "critical"

    def get_latest_data(self, source_type: DataSourceType) -> Optional[DataPoint]:
        """Get most recent data from a specific source"""
        return self.current_data.get(source_type)

    def to_dict(self) -> Dict[str, Any]:
        """Serialize to dictionary"""
        return {
            'twin_id': self.twin_id,
            'name': self.name,
            'description': self.description,
            'created_at': self.created_at.isoformat(),
            'last_updated': self.last_updated.isoformat(),
            'data_sources': [ds.value for ds in self.data_sources],
            'current_data': {k.value: v.to_dict() for k, v in self.current_data.items()},
            'health_status': self.health_status,
            'metrics': self.metrics
        }


class DataConnector:
    """Base class for all data source connectors"""

    def __init__(self, source_type: DataSourceType, config: Dict[str, Any]):
        self.source_type = source_type
        self.config = config
        self.is_connected = False
        self.last_fetch_time = None
        self.error_count = 0

    def connect(self) -> bool:
        """Establish connection to data source"""
        raise NotImplementedError("Subclasses must implement connect()")

    def disconnect(self):
        """Close connection to data source"""
        self.is_connected = False

    def fetch_data(self) -> Optional[DataPoint]:
        """Fetch latest data from source"""
        raise NotImplementedError("Subclasses must implement fetch_data()")

    def validate_data(self, data: Dict[str, Any]) -> DataQuality:
        """Validate data quality"""
        if not data:
            return DataQuality.UNAVAILABLE

        # Basic validation - can be overridden by subclasses
        required_fields = self.config.get('required_fields', [])
        if all(field in data for field in required_fields):
            return DataQuality.GOOD
        else:
            return DataQuality.DEGRADED


class DigitalTwinFramework:
    """
    Main Digital Twin Framework

    Manages multiple Digital Twins with real-time data synchronization
    from various space and Earth observation data sources.
    """

    def __init__(self, config_file: Optional[str] = None):
        self.config = self._load_config(config_file) if config_file else {}
        self.twins: Dict[str, DigitalTwinState] = {}
        self.connectors: Dict[DataSourceType, DataConnector] = {}
        self.sync_threads: Dict[str, threading.Thread] = {}
        self.is_running = False
        self.callbacks: Dict[str, List[Callable]] = {}

    def _load_config(self, config_file: str) -> Dict[str, Any]:
        """Load configuration from JSON file"""
        try:
            with open(config_file, 'r') as f:
                return json.load(f)
        except Exception as e:
            print(f"Error loading config: {e}")
            return {}

    def create_twin(
        self,
        twin_id: str,
        name: str,
        description: str,
        data_sources: List[DataSourceType]
    ) -> DigitalTwinState:
        """Create a new Digital Twin"""
        now = datetime.now(timezone.utc)
        twin = DigitalTwinState(
            twin_id=twin_id,
            name=name,
            description=description,
            created_at=now,
            last_updated=now,
            data_sources=data_sources
        )
        self.twins[twin_id] = twin
        print(f"✓ Created Digital Twin: {name} (ID: {twin_id})")
        return twin

    def register_connector(self, connector: DataConnector):
        """Register a data source connector"""
        self.connectors[connector.source_type] = connector
        print(f"✓ Registered connector: {connector.source_type.value}")

    def connect_all(self) -> Dict[DataSourceType, bool]:
        """Connect all registered connectors"""
        results = {}
        for source_type, connector in self.connectors.items():
            try:
                success = connector.connect()
                results[source_type] = success
                status = "✓" if success else "✗"
                print(f"{status} {source_type.value}: {'Connected' if success else 'Failed'}")
            except Exception as e:
                results[source_type] = False
                print(f"✗ {source_type.value}: Error - {e}")
        return results

    def start_synchronization(self, twin_id: str, interval_seconds: float = 10.0):
        """Start real-time data synchronization for a Digital Twin"""
        if twin_id not in self.twins:
            raise ValueError(f"Twin {twin_id} not found")

        def sync_worker():
            twin = self.twins[twin_id]
            while self.is_running:
                for source_type in twin.data_sources:
                    connector = self.connectors.get(source_type)
                    if connector and connector.is_connected:
                        try:
                            start_time = time.time()
                            data_point = connector.fetch_data()
                            if data_point:
                                data_point.latency_ms = (time.time() - start_time) * 1000
                                twin.update_data(data_point)
                                self._trigger_callbacks(twin_id, data_point)
                        except Exception as e:
                            print(f"Error fetching {source_type.value}: {e}")
                            connector.error_count += 1

                time.sleep(interval_seconds)

        thread = threading.Thread(target=sync_worker, daemon=True)
        self.sync_threads[twin_id] = thread
        self.is_running = True
        thread.start()
        print(f"✓ Started synchronization for {twin.name}")

    def stop_synchronization(self, twin_id: str = None):
        """Stop synchronization for a specific twin or all twins"""
        self.is_running = False
        if twin_id:
            thread = self.sync_threads.get(twin_id)
            if thread:
                thread.join(timeout=5.0)
                print(f"✓ Stopped synchronization for twin {twin_id}")
        else:
            for thread in self.sync_threads.values():
                thread.join(timeout=5.0)
            print("✓ Stopped all synchronization threads")

    def register_callback(self, twin_id: str, callback: Callable[[DataPoint], None]):
        """Register callback for data updates"""
        if twin_id not in self.callbacks:
            self.callbacks[twin_id] = []
        self.callbacks[twin_id].append(callback)

    def _trigger_callbacks(self, twin_id: str, data_point: DataPoint):
        """Trigger all callbacks for a twin"""
        callbacks = self.callbacks.get(twin_id, [])
        for callback in callbacks:
            try:
                callback(data_point)
            except Exception as e:
                print(f"Error in callback: {e}")

    def get_twin_state(self, twin_id: str) -> Optional[DigitalTwinState]:
        """Get current state of a Digital Twin"""
        return self.twins.get(twin_id)

    def export_twin_state(self, twin_id: str, filepath: str):
        """Export Digital Twin state to JSON file"""
        twin = self.twins.get(twin_id)
        if not twin:
            raise ValueError(f"Twin {twin_id} not found")

        with open(filepath, 'w') as f:
            json.dump(twin.to_dict(), f, indent=2)
        print(f"✓ Exported {twin.name} state to {filepath}")

    def get_system_health(self) -> Dict[str, Any]:
        """Get overall system health metrics"""
        return {
            'total_twins': len(self.twins),
            'active_connectors': sum(1 for c in self.connectors.values() if c.is_connected),
            'total_connectors': len(self.connectors),
            'twin_health': {
                twin_id: twin.health_status
                for twin_id, twin in self.twins.items()
            },
            'connector_errors': {
                source.value: connector.error_count
                for source, connector in self.connectors.items()
            }
        }


if __name__ == "__main__":
    # Example usage
    print("Digital Twin Framework - Core Architecture")
    print("=" * 50)

    framework = DigitalTwinFramework()

    # Create a Digital Twin for ISS mission monitoring
    iss_twin = framework.create_twin(
        twin_id="iss_mission_001",
        name="ISS Mission Monitor",
        description="Real-time Digital Twin of International Space Station",
        data_sources=[
            DataSourceType.ISS_TELEMETRY,
            DataSourceType.SATELLITE_TRACKING
        ]
    )

    print("\nFramework initialized successfully!")
    print(f"System Health: {framework.get_system_health()}")
