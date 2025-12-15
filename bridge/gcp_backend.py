"""
GCP Backend Adapters for MotorHandPro REST API

Optional GCP backend integration for:
- Telemetry ingestion
- Experiment tracking
- Artifact storage

Enable with environment variables:
  MHP_BACKEND=gcp
  GCP_PROJECT=your-project
  BQ_DATASET=motorhandpro
  GCS_BUCKET=motorhandpro-artifacts
"""

import os
import logging
from typing import List, Dict, Any, Optional
from datetime import datetime
from fastapi import APIRouter, HTTPException, BackgroundTasks
from pydantic import BaseModel, Field
import uuid

logger = logging.getLogger(__name__)

# Import GCP clients (optional)
try:
    import sys
    sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from gcp import config, get_bq, get_gcs, get_logging, get_monitoring

    # Check if GCP is enabled
    GCP_ENABLED = config.is_gcp_enabled()

    if GCP_ENABLED:
        logger.info(f"GCP backend enabled: project={config.project}, dataset={config.bq_dataset}")
        bq = get_bq()
        gcs = get_gcs()
        cloud_logging = get_logging()
        monitoring = get_monitoring()
    else:
        logger.info("GCP backend disabled (MHP_BACKEND != 'gcp')")
        bq = gcs = cloud_logging = monitoring = None

except ImportError as e:
    logger.warning(f"GCP libraries not available: {e}")
    GCP_ENABLED = False
    bq = gcs = cloud_logging = monitoring = None


# Create router
router = APIRouter(prefix="/gcp", tags=["GCP Backend"])


# ========== Request/Response Models ==========

class TelemetryEvent(BaseModel):
    """Single telemetry event"""
    timestamp: Optional[str] = Field(None, description="Event timestamp (ISO format, UTC)")
    source: str = Field(..., description="Data source identifier")
    event_type: str = Field(..., description="Event type")
    value: Optional[float] = Field(None, description="Numeric value")
    unit: Optional[str] = Field(None, description="Unit of measurement")
    quality: Optional[str] = Field("good", description="Data quality indicator")
    metadata: Optional[Dict[str, Any]] = Field(None, description="Additional metadata")
    run_id: Optional[str] = Field(None, description="Experiment run ID")
    device_id: Optional[str] = Field(None, description="Device identifier")


class TelemetryIngestRequest(BaseModel):
    """Batch telemetry ingestion request"""
    events: List[TelemetryEvent] = Field(..., description="List of telemetry events")
    table_name: Optional[str] = Field("raw_telemetry", description="Target table name")


class TelemetryIngestResponse(BaseModel):
    """Telemetry ingestion response"""
    success: bool
    events_count: int
    backend: str
    message: Optional[str] = None


class ExperimentRunRequest(BaseModel):
    """Register experiment run"""
    experiment_name: str = Field(..., description="Experiment name")
    parameters: Dict[str, Any] = Field(..., description="Experiment parameters")
    run_id: Optional[str] = Field(None, description="Run ID (auto-generated if not provided)")
    user: Optional[str] = Field(None, description="User identifier")
    git_commit: Optional[str] = Field(None, description="Git commit hash")


class ExperimentRunResponse(BaseModel):
    """Experiment run registration response"""
    run_id: str
    success: bool
    backend: str
    message: Optional[str] = None


class ExperimentMetricsRequest(BaseModel):
    """Update experiment metrics"""
    run_id: str = Field(..., description="Experiment run ID")
    metrics: Dict[str, Any] = Field(..., description="Metrics dictionary")
    status: Optional[str] = Field("running", description="Run status")


class ExperimentMetricsResponse(BaseModel):
    """Experiment metrics update response"""
    success: bool
    run_id: str
    backend: str


class QueryTelemetryRequest(BaseModel):
    """Query telemetry data"""
    start_time: Optional[str] = Field(None, description="Start timestamp (ISO format)")
    end_time: Optional[str] = Field(None, description="End timestamp (ISO format)")
    limit: Optional[int] = Field(1000, description="Maximum records to return")
    table_name: Optional[str] = Field("raw_telemetry", description="Source table")


class QueryExperimentRunsRequest(BaseModel):
    """Query experiment runs"""
    experiment_name: Optional[str] = Field(None, description="Filter by experiment name")
    status: Optional[str] = Field(None, description="Filter by status")
    limit: Optional[int] = Field(100, description="Maximum runs to return")


# ========== Local Backend Fallback ==========

class LocalBackend:
    """Local storage fallback when GCP is disabled"""

    def __init__(self):
        self.telemetry = []
        self.experiment_runs = {}

    def store_telemetry(self, events: List[Dict[str, Any]]) -> bool:
        """Store telemetry locally"""
        self.telemetry.extend(events)
        logger.info(f"Stored {len(events)} telemetry events locally")
        return True

    def store_experiment_run(self, run_id: str, data: Dict[str, Any]) -> bool:
        """Store experiment run locally"""
        self.experiment_runs[run_id] = data
        logger.info(f"Stored experiment run locally: {run_id}")
        return True

    def get_telemetry(self, limit: int = 1000) -> List[Dict[str, Any]]:
        """Get recent telemetry"""
        return self.telemetry[-limit:]

    def get_experiment_runs(self, limit: int = 100) -> List[Dict[str, Any]]:
        """Get recent experiment runs"""
        return list(self.experiment_runs.values())[-limit:]


# Initialize local backend
local_backend = LocalBackend()


# ========== Endpoints ==========

@router.post("/telemetry/ingest", response_model=TelemetryIngestResponse)
async def ingest_telemetry(
    request: TelemetryIngestRequest,
    background_tasks: BackgroundTasks
):
    """
    Ingest telemetry data

    Writes to BigQuery if GCP backend is enabled, otherwise stores locally.

    Usage:
        POST /gcp/telemetry/ingest
        {
            "events": [
                {
                    "source": "emg_sensor_01",
                    "event_type": "sensor_reading",
                    "value": 0.0042,
                    "unit": "volts"
                }
            ]
        }
    """
    # Convert events to dictionaries
    events_dict = [event.dict() for event in request.events]

    # Ensure timestamps
    for event in events_dict:
        if not event.get('timestamp'):
            event['timestamp'] = datetime.utcnow().isoformat()

    # Write to backend
    if GCP_ENABLED and bq:
        success = bq.write_telemetry(events_dict, request.table_name)
        backend = "bigquery"

        # Also write to Cloud Logging (async)
        if success and cloud_logging:
            for event in events_dict:
                background_tasks.add_task(
                    cloud_logging.log_telemetry_event,
                    event.get('event_type', 'unknown'),
                    event
                )
    else:
        success = local_backend.store_telemetry(events_dict)
        backend = "local"

    return TelemetryIngestResponse(
        success=success,
        events_count=len(request.events),
        backend=backend,
        message="Telemetry ingested successfully" if success else "Ingestion failed"
    )


@router.post("/runs/register", response_model=ExperimentRunResponse)
async def register_experiment_run(
    request: ExperimentRunRequest,
    background_tasks: BackgroundTasks
):
    """
    Register new experiment run

    Stores run metadata in BigQuery if GCP backend is enabled.

    Usage:
        POST /gcp/runs/register
        {
            "experiment_name": "orbital_decay",
            "parameters": {"altitude_km": 400}
        }
    """
    # Generate run ID if not provided
    run_id = request.run_id or f"run_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}_{str(uuid.uuid4())[:8]}"

    run_data = {
        'run_id': run_id,
        'experiment_name': request.experiment_name,
        'parameters': request.parameters,
        'timestamp': datetime.utcnow().isoformat(),
        'status': 'running',
        'user': request.user,
        'git_commit': request.git_commit
    }

    # Write to backend
    if GCP_ENABLED and bq:
        success = bq.write_experiment_run(
            run_id=run_id,
            experiment_name=request.experiment_name,
            parameters=request.parameters,
            metrics={},
            status='running'
        )
        backend = "bigquery"

        # Log experiment start (async)
        if success and cloud_logging:
            background_tasks.add_task(
                cloud_logging.log_experiment_event,
                run_id, 'started', {'parameters': request.parameters}
            )
    else:
        success = local_backend.store_experiment_run(run_id, run_data)
        backend = "local"

    return ExperimentRunResponse(
        run_id=run_id,
        success=success,
        backend=backend,
        message=f"Run {run_id} registered" if success else "Registration failed"
    )


@router.post("/runs/metrics", response_model=ExperimentMetricsResponse)
async def update_experiment_metrics(
    request: ExperimentMetricsRequest,
    background_tasks: BackgroundTasks
):
    """
    Update experiment run metrics

    Updates metrics in BigQuery if GCP backend is enabled.

    Usage:
        POST /gcp/runs/metrics
        {
            "run_id": "run_20250115_103045_abc123",
            "metrics": {"accuracy": 0.95, "loss": 0.05},
            "status": "completed"
        }
    """
    if GCP_ENABLED and bq:
        success = bq.write_experiment_run(
            run_id=request.run_id,
            experiment_name="",  # Not updated
            parameters={},  # Not updated
            metrics=request.metrics,
            status=request.status
        )
        backend = "bigquery"

        # Log metrics (async)
        if success and monitoring:
            for metric_name, metric_value in request.metrics.items():
                if isinstance(metric_value, (int, float)):
                    background_tasks.add_task(
                        monitoring.write_experiment_metric,
                        request.run_id, metric_name, float(metric_value)
                    )
    else:
        # Update local backend
        if request.run_id in local_backend.experiment_runs:
            local_backend.experiment_runs[request.run_id]['metrics'] = request.metrics
            local_backend.experiment_runs[request.run_id]['status'] = request.status
            success = True
        else:
            success = False
        backend = "local"

    return ExperimentMetricsResponse(
        success=success,
        run_id=request.run_id,
        backend=backend
    )


@router.get("/runs/{run_id}/metrics")
async def get_run_metrics(run_id: str):
    """
    Get metrics for specific run

    Usage:
        GET /gcp/runs/{run_id}/metrics
    """
    if GCP_ENABLED and bq:
        metrics = bq.get_run_metrics(run_id)
        backend = "bigquery"
    else:
        run = local_backend.experiment_runs.get(run_id)
        metrics = run.get('metrics', {}) if run else None
        backend = "local"

    if metrics is None:
        raise HTTPException(status_code=404, detail=f"Run {run_id} not found")

    return {
        "run_id": run_id,
        "metrics": metrics,
        "backend": backend
    }


@router.post("/telemetry/query")
async def query_telemetry(request: QueryTelemetryRequest):
    """
    Query telemetry data

    Usage:
        POST /gcp/telemetry/query
        {
            "start_time": "2025-01-15T00:00:00Z",
            "end_time": "2025-01-15T23:59:59Z",
            "limit": 1000
        }
    """
    if GCP_ENABLED and bq:
        records = bq.query_telemetry(
            request.start_time,
            request.end_time,
            request.limit,
            request.table_name
        )
        backend = "bigquery"
    else:
        records = local_backend.get_telemetry(request.limit)
        backend = "local"

    if records is None:
        raise HTTPException(status_code=500, detail="Query failed")

    return {
        "records": records,
        "count": len(records),
        "backend": backend
    }


@router.post("/runs/query")
async def query_experiment_runs(request: QueryExperimentRunsRequest):
    """
    Query experiment runs

    Usage:
        POST /gcp/runs/query
        {
            "experiment_name": "orbital_decay",
            "status": "completed",
            "limit": 100
        }
    """
    if GCP_ENABLED and bq:
        runs = bq.query_experiment_runs(
            request.experiment_name,
            request.status,
            request.limit
        )
        backend = "bigquery"
    else:
        runs = local_backend.get_experiment_runs(request.limit)
        backend = "local"

    if runs is None:
        raise HTTPException(status_code=500, detail="Query failed")

    return {
        "runs": runs,
        "count": len(runs),
        "backend": backend
    }


@router.get("/health")
async def health_check():
    """
    Check GCP backend health

    Returns status of GCP services and configuration.
    """
    health = {
        "gcp_enabled": GCP_ENABLED,
        "backend": "gcp" if GCP_ENABLED else "local",
        "services": {}
    }

    if GCP_ENABLED:
        health["config"] = {
            "project": config.project,
            "bq_dataset": config.bq_dataset,
            "gcs_bucket": config.gcs_bucket
        }

        # Check service availability
        if bq:
            health["services"]["bigquery"] = bq.get_client().is_available()
        if gcs:
            health["services"]["gcs"] = gcs.get_client().is_available()
        if cloud_logging:
            health["services"]["logging"] = cloud_logging.get_client().is_available()
        if monitoring:
            health["services"]["monitoring"] = monitoring.get_client().is_available()

    return health


# Export router
__all__ = ['router', 'GCP_ENABLED']
