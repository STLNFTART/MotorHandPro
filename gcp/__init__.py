"""
GCP Integration Module for MotorHandPro

Optional GCP backends for telemetry, data storage, and monitoring.
All clients use Application Default Credentials (ADC) - NO KEYS IN REPO.

Environment Variables:
    MHP_BACKEND: local|gcp (default: local)
    GCP_PROJECT: GCP project ID
    BQ_DATASET: BigQuery dataset name
    GCS_BUCKET: GCS bucket name

Usage:
    from gcp import bq, gcs, logging_client, monitoring_client

    # Check if GCP is enabled
    if config.is_gcp_enabled():
        bq.write_telemetry(data)
"""

import os
from typing import Optional

class GCPConfig:
    """GCP backend configuration"""

    def __init__(self):
        self.backend = os.getenv('MHP_BACKEND', 'local')
        self.project = os.getenv('GCP_PROJECT')
        self.bq_dataset = os.getenv('BQ_DATASET', 'motorhandpro')
        self.gcs_bucket = os.getenv('GCS_BUCKET')

    def is_gcp_enabled(self) -> bool:
        """Check if GCP backend is enabled"""
        return self.backend == 'gcp' and self.project is not None

    def validate(self) -> tuple[bool, Optional[str]]:
        """Validate GCP configuration"""
        if not self.is_gcp_enabled():
            return True, None

        if not self.project:
            return False, "GCP_PROJECT not set"
        if not self.gcs_bucket:
            return False, "GCS_BUCKET not set"

        return True, None

config = GCPConfig()

# Lazy imports to avoid import errors when libraries not installed
def _lazy_import(module_name):
    """Lazy import helper"""
    try:
        return __import__(f'gcp.{module_name}', fromlist=[module_name])
    except ImportError:
        return None

# Module-level getters
def get_bq():
    """Get BigQuery module"""
    from . import bq
    return bq

def get_gcs():
    """Get GCS module"""
    from . import gcs
    return gcs

def get_logging():
    """Get Cloud Logging module"""
    from . import logging
    return logging

def get_monitoring():
    """Get Cloud Monitoring module"""
    from . import monitoring
    return monitoring

def get_dataplex():
    """Get Dataplex module (optional)"""
    from . import dataplex
    return dataplex

__all__ = [
    'config',
    'get_bq',
    'get_gcs',
    'get_logging',
    'get_monitoring',
    'get_dataplex'
]
