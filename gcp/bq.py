"""
BigQuery Client for MotorHandPro Telemetry and Dataset Storage

Uses Application Default Credentials (ADC).
Run locally: gcloud auth application-default login
Run in GCP: uses service account automatically

Table Schema:
- raw_telemetry: Real-time sensor/experiment data
- curated_timeseries: Processed time-series data
- experiment_runs: Experiment metadata and results
- metrics_validation: Validation results and anomalies
"""

import json
from datetime import datetime
from typing import Any, Dict, List, Optional
import logging

logger = logging.getLogger(__name__)

try:
    from google.cloud import bigquery
    from google.cloud.exceptions import GoogleCloudError
    BIGQUERY_AVAILABLE = True
except ImportError:
    BIGQUERY_AVAILABLE = False
    logger.warning("google-cloud-bigquery not installed. Install with: pip install google-cloud-bigquery")

from . import config


class BigQueryClient:
    """BigQuery client for telemetry and dataset operations"""

    def __init__(self):
        self.client: Optional[bigquery.Client] = None
        self.dataset_ref = None

        if not config.is_gcp_enabled():
            logger.info("GCP backend disabled, BigQuery client not initialized")
            return

        if not BIGQUERY_AVAILABLE:
            logger.error("BigQuery library not available")
            return

        try:
            self.client = bigquery.Client(project=config.project)
            self.dataset_ref = self.client.dataset(config.bq_dataset)
            logger.info(f"BigQuery client initialized: {config.project}.{config.bq_dataset}")
        except Exception as e:
            logger.error(f"Failed to initialize BigQuery client: {e}")
            self.client = None

    def is_available(self) -> bool:
        """Check if BigQuery is available"""
        return self.client is not None

    def ensure_dataset(self) -> bool:
        """Ensure dataset exists, create if not"""
        if not self.is_available():
            return False

        try:
            self.client.get_dataset(self.dataset_ref)
            logger.info(f"Dataset {config.bq_dataset} exists")
            return True
        except GoogleCloudError:
            # Dataset doesn't exist, create it
            try:
                dataset = bigquery.Dataset(self.dataset_ref)
                dataset.location = "US"
                dataset.description = "MotorHandPro telemetry and experiment data"
                self.client.create_dataset(dataset)
                logger.info(f"Created dataset {config.bq_dataset}")
                return True
            except Exception as e:
                logger.error(f"Failed to create dataset: {e}")
                return False

    def write_telemetry(self, records: List[Dict[str, Any]], table_name: str = "raw_telemetry") -> bool:
        """
        Write telemetry records to BigQuery

        Args:
            records: List of telemetry dictionaries
            table_name: Target table name

        Returns:
            Success status
        """
        if not self.is_available():
            logger.debug("BigQuery not available, skipping write")
            return False

        try:
            table_id = f"{config.project}.{config.bq_dataset}.{table_name}"

            # Ensure timestamp field
            for record in records:
                if 'timestamp' not in record:
                    record['timestamp'] = datetime.utcnow().isoformat()

            errors = self.client.insert_rows_json(table_id, records)

            if errors:
                logger.error(f"BigQuery insert errors: {errors}")
                return False

            logger.debug(f"Wrote {len(records)} records to {table_id}")
            return True

        except Exception as e:
            logger.error(f"Failed to write telemetry: {e}")
            return False

    def write_experiment_run(
        self,
        run_id: str,
        experiment_name: str,
        parameters: Dict[str, Any],
        metrics: Dict[str, Any],
        status: str = "completed"
    ) -> bool:
        """
        Write experiment run metadata

        Args:
            run_id: Unique run identifier
            experiment_name: Name of experiment
            parameters: Experiment parameters
            metrics: Result metrics
            status: Run status (running, completed, failed)

        Returns:
            Success status
        """
        if not self.is_available():
            return False

        try:
            table_id = f"{config.project}.{config.bq_dataset}.experiment_runs"

            record = {
                'run_id': run_id,
                'experiment_name': experiment_name,
                'parameters': json.dumps(parameters),
                'metrics': json.dumps(metrics),
                'status': status,
                'timestamp': datetime.utcnow().isoformat()
            }

            errors = self.client.insert_rows_json(table_id, [record])

            if errors:
                logger.error(f"Failed to write experiment run: {errors}")
                return False

            logger.info(f"Recorded experiment run: {run_id}")
            return True

        except Exception as e:
            logger.error(f"Failed to write experiment run: {e}")
            return False

    def query_telemetry(
        self,
        start_time: Optional[str] = None,
        end_time: Optional[str] = None,
        limit: int = 1000,
        table_name: str = "raw_telemetry"
    ) -> Optional[List[Dict[str, Any]]]:
        """
        Query telemetry data

        Args:
            start_time: Start timestamp (ISO format)
            end_time: End timestamp (ISO format)
            limit: Maximum rows to return
            table_name: Source table

        Returns:
            List of telemetry records or None
        """
        if not self.is_available():
            return None

        try:
            query = f"""
                SELECT *
                FROM `{config.project}.{config.bq_dataset}.{table_name}`
                WHERE 1=1
            """

            if start_time:
                query += f" AND timestamp >= '{start_time}'"
            if end_time:
                query += f" AND timestamp <= '{end_time}'"

            query += f" ORDER BY timestamp DESC LIMIT {limit}"

            query_job = self.client.query(query)
            results = query_job.result()

            records = [dict(row) for row in results]
            logger.info(f"Retrieved {len(records)} telemetry records")
            return records

        except Exception as e:
            logger.error(f"Failed to query telemetry: {e}")
            return None

    def query_experiment_runs(
        self,
        experiment_name: Optional[str] = None,
        status: Optional[str] = None,
        limit: int = 100
    ) -> Optional[List[Dict[str, Any]]]:
        """
        Query experiment runs

        Args:
            experiment_name: Filter by experiment name
            status: Filter by status
            limit: Maximum runs to return

        Returns:
            List of experiment run records or None
        """
        if not self.is_available():
            return None

        try:
            query = f"""
                SELECT *
                FROM `{config.project}.{config.bq_dataset}.experiment_runs`
                WHERE 1=1
            """

            if experiment_name:
                query += f" AND experiment_name = '{experiment_name}'"
            if status:
                query += f" AND status = '{status}'"

            query += f" ORDER BY timestamp DESC LIMIT {limit}"

            query_job = self.client.query(query)
            results = query_job.result()

            records = []
            for row in results:
                record = dict(row)
                # Parse JSON fields
                if 'parameters' in record:
                    record['parameters'] = json.loads(record['parameters'])
                if 'metrics' in record:
                    record['metrics'] = json.loads(record['metrics'])
                records.append(record)

            logger.info(f"Retrieved {len(records)} experiment runs")
            return records

        except Exception as e:
            logger.error(f"Failed to query experiment runs: {e}")
            return None

    def get_run_metrics(self, run_id: str) -> Optional[Dict[str, Any]]:
        """
        Get metrics for a specific run

        Args:
            run_id: Run identifier

        Returns:
            Metrics dictionary or None
        """
        if not self.is_available():
            return None

        try:
            query = f"""
                SELECT metrics
                FROM `{config.project}.{config.bq_dataset}.experiment_runs`
                WHERE run_id = '{run_id}'
                LIMIT 1
            """

            query_job = self.client.query(query)
            results = list(query_job.result())

            if not results:
                logger.warning(f"No run found with id: {run_id}")
                return None

            metrics_json = results[0]['metrics']
            return json.loads(metrics_json)

        except Exception as e:
            logger.error(f"Failed to get run metrics: {e}")
            return None


# Global client instance
_client = None

def get_client() -> BigQueryClient:
    """Get or create BigQuery client singleton"""
    global _client
    if _client is None:
        _client = BigQueryClient()
    return _client


# Convenience functions
def write_telemetry(records: List[Dict[str, Any]], table_name: str = "raw_telemetry") -> bool:
    """Write telemetry records"""
    return get_client().write_telemetry(records, table_name)


def write_experiment_run(
    run_id: str,
    experiment_name: str,
    parameters: Dict[str, Any],
    metrics: Dict[str, Any],
    status: str = "completed"
) -> bool:
    """Write experiment run metadata"""
    return get_client().write_experiment_run(run_id, experiment_name, parameters, metrics, status)


def query_telemetry(
    start_time: Optional[str] = None,
    end_time: Optional[str] = None,
    limit: int = 1000,
    table_name: str = "raw_telemetry"
) -> Optional[List[Dict[str, Any]]]:
    """Query telemetry data"""
    return get_client().query_telemetry(start_time, end_time, limit, table_name)


def query_experiment_runs(
    experiment_name: Optional[str] = None,
    status: Optional[str] = None,
    limit: int = 100
) -> Optional[List[Dict[str, Any]]]:
    """Query experiment runs"""
    return get_client().query_experiment_runs(experiment_name, status, limit)


def get_run_metrics(run_id: str) -> Optional[Dict[str, Any]]:
    """Get metrics for a specific run"""
    return get_client().get_run_metrics(run_id)
