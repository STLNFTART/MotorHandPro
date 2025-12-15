"""
Cloud Monitoring Client for MotorHandPro Custom Metrics

Metrics Types:
- experiment_runtime: Experiment execution time
- telemetry_rate: Data ingestion rate
- model_accuracy: ML model performance
- error_rate: Error frequency
- resource_utilization: CPU/memory/GPU usage

Time Series Data:
- Point-in-time measurements
- Distribution (histogram) metrics
- Cumulative metrics
"""

import logging
from typing import Any, Dict, List, Optional
from datetime import datetime
import time

logger = logging.getLogger(__name__)

try:
    from google.cloud import monitoring_v3
    from google.api import metric_pb2
    from google.api import monitored_resource_pb2
    MONITORING_AVAILABLE = True
except ImportError:
    MONITORING_AVAILABLE = False
    logger.warning("google-cloud-monitoring not installed. Install with: pip install google-cloud-monitoring")

from . import config


class MonitoringClient:
    """Cloud Monitoring client for custom metrics"""

    def __init__(self):
        self.client: Optional[monitoring_v3.MetricServiceClient] = None
        self.project_name: Optional[str] = None

        if not config.is_gcp_enabled():
            logger.info("GCP backend disabled, Monitoring not initialized")
            return

        if not MONITORING_AVAILABLE:
            logger.error("Cloud Monitoring library not available")
            return

        try:
            self.client = monitoring_v3.MetricServiceClient()
            self.project_name = f"projects/{config.project}"
            logger.info("Cloud Monitoring client initialized")

        except Exception as e:
            logger.error(f"Failed to initialize Cloud Monitoring: {e}")
            self.client = None

    def is_available(self) -> bool:
        """Check if Cloud Monitoring is available"""
        return self.client is not None

    def write_metric(
        self,
        metric_type: str,
        value: float,
        labels: Optional[Dict[str, str]] = None,
        resource_type: str = "global",
        resource_labels: Optional[Dict[str, str]] = None,
        timestamp: Optional[datetime] = None
    ) -> bool:
        """
        Write custom metric

        Args:
            metric_type: Metric type (e.g., 'experiment_runtime')
            value: Metric value
            labels: Metric labels
            resource_type: Monitored resource type
            resource_labels: Resource labels
            timestamp: Measurement timestamp (defaults to now)

        Returns:
            Success status
        """
        if not self.is_available():
            logger.debug(f"Monitoring not available, skipping metric: {metric_type}={value}")
            return False

        try:
            # Build time series
            series = monitoring_v3.TimeSeries()

            # Metric descriptor
            series.metric.type = f"custom.googleapis.com/motorhandpro/{metric_type}"

            if labels:
                for key, val in labels.items():
                    series.metric.labels[key] = str(val)

            # Resource
            series.resource.type = resource_type
            if resource_labels:
                for key, val in resource_labels.items():
                    series.resource.labels[key] = str(val)
            elif resource_type == "global":
                series.resource.labels['project_id'] = config.project

            # Point
            point = monitoring_v3.Point()
            point.value.double_value = float(value)

            now = timestamp or datetime.utcnow()
            point.interval.end_time.seconds = int(now.timestamp())
            point.interval.end_time.nanos = int((now.timestamp() % 1) * 10**9)

            series.points = [point]

            # Write time series
            self.client.create_time_series(
                name=self.project_name,
                time_series=[series]
            )

            logger.debug(f"Wrote metric: {metric_type}={value}")
            return True

        except Exception as e:
            logger.error(f"Failed to write metric: {e}")
            return False

    def write_experiment_metric(
        self,
        run_id: str,
        metric_name: str,
        value: float,
        epoch: Optional[int] = None
    ) -> bool:
        """
        Write experiment training metric

        Args:
            run_id: Experiment run ID
            metric_name: Metric name (e.g., 'accuracy', 'loss')
            value: Metric value
            epoch: Training epoch number

        Returns:
            Success status
        """
        labels = {
            'run_id': run_id,
            'metric_name': metric_name
        }

        if epoch is not None:
            labels['epoch'] = str(epoch)

        return self.write_metric(
            metric_type=f"experiment/{metric_name}",
            value=value,
            labels=labels
        )

    def write_telemetry_rate(
        self,
        source: str,
        rate: float,
        unit: str = "records_per_second"
    ) -> bool:
        """
        Write telemetry ingestion rate

        Args:
            source: Data source identifier
            rate: Ingestion rate
            unit: Rate unit

        Returns:
            Success status
        """
        labels = {
            'source': source,
            'unit': unit
        }

        return self.write_metric(
            metric_type="telemetry/ingestion_rate",
            value=rate,
            labels=labels
        )

    def write_error_rate(
        self,
        component: str,
        error_count: int,
        time_window_seconds: int = 60
    ) -> bool:
        """
        Write error rate metric

        Args:
            component: Component name
            error_count: Number of errors
            time_window_seconds: Time window for rate calculation

        Returns:
            Success status
        """
        rate = error_count / time_window_seconds

        labels = {
            'component': component,
            'window': f'{time_window_seconds}s'
        }

        return self.write_metric(
            metric_type="error/rate",
            value=rate,
            labels=labels
        )

    def write_resource_utilization(
        self,
        resource_type: str,
        utilization: float,
        resource_id: Optional[str] = None
    ) -> bool:
        """
        Write resource utilization metric

        Args:
            resource_type: Resource type (cpu, memory, gpu, disk)
            utilization: Utilization percentage (0-100)
            resource_id: Resource identifier

        Returns:
            Success status
        """
        labels = {
            'resource_type': resource_type
        }

        if resource_id:
            labels['resource_id'] = resource_id

        return self.write_metric(
            metric_type=f"resource/{resource_type}_utilization",
            value=utilization,
            labels=labels
        )

    def write_batch_metrics(
        self,
        metrics: List[Dict[str, Any]]
    ) -> tuple[int, int]:
        """
        Write multiple metrics in batch

        Args:
            metrics: List of metric dictionaries with keys:
                    - metric_type: str
                    - value: float
                    - labels: Optional[Dict]
                    - timestamp: Optional[datetime]

        Returns:
            (successful_count, failed_count)
        """
        if not self.is_available():
            return 0, len(metrics)

        successful = 0
        failed = 0

        for metric in metrics:
            if self.write_metric(**metric):
                successful += 1
            else:
                failed += 1

        logger.info(f"Batch metrics write: {successful} succeeded, {failed} failed")
        return successful, failed


class MetricsRecorder:
    """Context manager for recording operation metrics"""

    def __init__(
        self,
        metric_type: str,
        labels: Optional[Dict[str, str]] = None,
        client: Optional[MonitoringClient] = None
    ):
        self.metric_type = metric_type
        self.labels = labels or {}
        self.client = client or get_client()
        self.start_time = None

    def __enter__(self):
        self.start_time = time.time()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.start_time is not None:
            duration = time.time() - self.start_time
            self.client.write_metric(
                self.metric_type,
                duration,
                self.labels
            )


# Global client instance
_client = None

def get_client() -> MonitoringClient:
    """Get or create Monitoring client singleton"""
    global _client
    if _client is None:
        _client = MonitoringClient()
    return _client


# Convenience functions
def write_metric(
    metric_type: str,
    value: float,
    labels: Optional[Dict[str, str]] = None,
    resource_type: str = "global",
    resource_labels: Optional[Dict[str, str]] = None,
    timestamp: Optional[datetime] = None
) -> bool:
    """Write custom metric"""
    return get_client().write_metric(
        metric_type, value, labels, resource_type, resource_labels, timestamp
    )


def write_experiment_metric(
    run_id: str,
    metric_name: str,
    value: float,
    epoch: Optional[int] = None
) -> bool:
    """Write experiment metric"""
    return get_client().write_experiment_metric(run_id, metric_name, value, epoch)


def write_telemetry_rate(source: str, rate: float, unit: str = "records_per_second") -> bool:
    """Write telemetry rate"""
    return get_client().write_telemetry_rate(source, rate, unit)


def write_error_rate(component: str, error_count: int, time_window_seconds: int = 60) -> bool:
    """Write error rate"""
    return get_client().write_error_rate(component, error_count, time_window_seconds)


def write_resource_utilization(
    resource_type: str,
    utilization: float,
    resource_id: Optional[str] = None
) -> bool:
    """Write resource utilization"""
    return get_client().write_resource_utilization(resource_type, utilization, resource_id)


def record_operation(metric_type: str, labels: Optional[Dict[str, str]] = None) -> MetricsRecorder:
    """Context manager for recording operation duration"""
    return MetricsRecorder(metric_type, labels)
