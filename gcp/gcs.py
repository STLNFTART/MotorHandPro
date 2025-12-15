"""
Google Cloud Storage Client for MotorHandPro Artifacts

Handles:
- Experiment artifacts (models, checkpoints, plots)
- Dataset storage and versioning
- Notebook/code archival
- Large binary data

Storage Layout:
gs://{bucket}/
├── experiments/
│   └── {run_id}/
│       ├── model.pkl
│       ├── checkpoint.pt
│       ├── plots/
│       └── logs/
├── datasets/
│   └── {dataset_name}/
│       └── {version}/
├── notebooks/
│   └── {notebook_name}/
│       └── {timestamp}.ipynb
└── telemetry/
    └── {date}/
        └── {hour}.parquet
"""

import logging
import os
from pathlib import Path
from typing import Any, Dict, List, Optional, Union
from datetime import datetime

logger = logging.getLogger(__name__)

try:
    from google.cloud import storage
    from google.cloud.exceptions import GoogleCloudError, NotFound
    GCS_AVAILABLE = True
except ImportError:
    GCS_AVAILABLE = False
    logger.warning("google-cloud-storage not installed. Install with: pip install google-cloud-storage")

from . import config


class GCSClient:
    """Google Cloud Storage client for artifact management"""

    def __init__(self):
        self.client: Optional[storage.Client] = None
        self.bucket: Optional[storage.Bucket] = None

        if not config.is_gcp_enabled():
            logger.info("GCP backend disabled, GCS client not initialized")
            return

        if not GCS_AVAILABLE:
            logger.error("GCS library not available")
            return

        try:
            self.client = storage.Client(project=config.project)
            self.bucket = self.client.bucket(config.gcs_bucket)
            logger.info(f"GCS client initialized: gs://{config.gcs_bucket}")
        except Exception as e:
            logger.error(f"Failed to initialize GCS client: {e}")
            self.client = None

    def is_available(self) -> bool:
        """Check if GCS is available"""
        return self.client is not None and self.bucket is not None

    def upload_file(
        self,
        local_path: Union[str, Path],
        gcs_path: str,
        metadata: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Upload file to GCS

        Args:
            local_path: Local file path
            gcs_path: GCS object path (relative to bucket)
            metadata: Optional metadata dictionary

        Returns:
            Success status
        """
        if not self.is_available():
            logger.debug("GCS not available")
            return False

        try:
            blob = self.bucket.blob(gcs_path)

            if metadata:
                blob.metadata = metadata

            blob.upload_from_filename(str(local_path))
            logger.info(f"Uploaded {local_path} to gs://{config.gcs_bucket}/{gcs_path}")
            return True

        except Exception as e:
            logger.error(f"Failed to upload file: {e}")
            return False

    def upload_string(
        self,
        content: str,
        gcs_path: str,
        content_type: str = "text/plain",
        metadata: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Upload string content to GCS

        Args:
            content: String content
            gcs_path: GCS object path
            content_type: MIME type
            metadata: Optional metadata

        Returns:
            Success status
        """
        if not self.is_available():
            return False

        try:
            blob = self.bucket.blob(gcs_path)
            blob.content_type = content_type

            if metadata:
                blob.metadata = metadata

            blob.upload_from_string(content)
            logger.info(f"Uploaded string to gs://{config.gcs_bucket}/{gcs_path}")
            return True

        except Exception as e:
            logger.error(f"Failed to upload string: {e}")
            return False

    def download_file(
        self,
        gcs_path: str,
        local_path: Union[str, Path]
    ) -> bool:
        """
        Download file from GCS

        Args:
            gcs_path: GCS object path
            local_path: Local destination path

        Returns:
            Success status
        """
        if not self.is_available():
            return False

        try:
            blob = self.bucket.blob(gcs_path)

            # Create parent directories if needed
            Path(local_path).parent.mkdir(parents=True, exist_ok=True)

            blob.download_to_filename(str(local_path))
            logger.info(f"Downloaded gs://{config.gcs_bucket}/{gcs_path} to {local_path}")
            return True

        except NotFound:
            logger.warning(f"Object not found: gs://{config.gcs_bucket}/{gcs_path}")
            return False
        except Exception as e:
            logger.error(f"Failed to download file: {e}")
            return False

    def download_string(self, gcs_path: str) -> Optional[str]:
        """
        Download object as string

        Args:
            gcs_path: GCS object path

        Returns:
            Content string or None
        """
        if not self.is_available():
            return None

        try:
            blob = self.bucket.blob(gcs_path)
            content = blob.download_as_text()
            logger.info(f"Downloaded gs://{config.gcs_bucket}/{gcs_path}")
            return content

        except NotFound:
            logger.warning(f"Object not found: gs://{config.gcs_bucket}/{gcs_path}")
            return None
        except Exception as e:
            logger.error(f"Failed to download string: {e}")
            return None

    def list_objects(
        self,
        prefix: str,
        delimiter: Optional[str] = None
    ) -> List[str]:
        """
        List objects with prefix

        Args:
            prefix: Object prefix
            delimiter: Delimiter for directory-style listing

        Returns:
            List of object names
        """
        if not self.is_available():
            return []

        try:
            blobs = self.client.list_blobs(
                self.bucket,
                prefix=prefix,
                delimiter=delimiter
            )

            objects = [blob.name for blob in blobs]
            logger.info(f"Listed {len(objects)} objects with prefix '{prefix}'")
            return objects

        except Exception as e:
            logger.error(f"Failed to list objects: {e}")
            return []

    def delete_object(self, gcs_path: str) -> bool:
        """
        Delete object from GCS

        Args:
            gcs_path: GCS object path

        Returns:
            Success status
        """
        if not self.is_available():
            return False

        try:
            blob = self.bucket.blob(gcs_path)
            blob.delete()
            logger.info(f"Deleted gs://{config.gcs_bucket}/{gcs_path}")
            return True

        except NotFound:
            logger.warning(f"Object not found: gs://{config.gcs_bucket}/{gcs_path}")
            return False
        except Exception as e:
            logger.error(f"Failed to delete object: {e}")
            return False

    def upload_experiment_artifacts(
        self,
        run_id: str,
        artifacts_dir: Union[str, Path],
        include_patterns: Optional[List[str]] = None
    ) -> int:
        """
        Upload all artifacts from experiment run

        Args:
            run_id: Unique run identifier
            artifacts_dir: Local artifacts directory
            include_patterns: Glob patterns to include (e.g., ["*.pkl", "*.png"])

        Returns:
            Number of files uploaded
        """
        if not self.is_available():
            return 0

        artifacts_dir = Path(artifacts_dir)
        if not artifacts_dir.exists():
            logger.error(f"Artifacts directory not found: {artifacts_dir}")
            return 0

        uploaded = 0

        # Collect files
        files = []
        if include_patterns:
            for pattern in include_patterns:
                files.extend(artifacts_dir.rglob(pattern))
        else:
            files = list(artifacts_dir.rglob('*'))

        # Upload each file
        for local_file in files:
            if local_file.is_file():
                # Relative path from artifacts_dir
                rel_path = local_file.relative_to(artifacts_dir)
                gcs_path = f"experiments/{run_id}/{rel_path}"

                if self.upload_file(local_file, gcs_path):
                    uploaded += 1

        logger.info(f"Uploaded {uploaded} artifacts for run {run_id}")
        return uploaded

    def download_experiment_artifacts(
        self,
        run_id: str,
        local_dir: Union[str, Path]
    ) -> int:
        """
        Download all artifacts for experiment run

        Args:
            run_id: Unique run identifier
            local_dir: Local destination directory

        Returns:
            Number of files downloaded
        """
        if not self.is_available():
            return 0

        local_dir = Path(local_dir)
        local_dir.mkdir(parents=True, exist_ok=True)

        prefix = f"experiments/{run_id}/"
        objects = self.list_objects(prefix)

        downloaded = 0
        for gcs_path in objects:
            # Remove prefix to get relative path
            rel_path = gcs_path[len(prefix):]
            local_path = local_dir / rel_path

            if self.download_file(gcs_path, local_path):
                downloaded += 1

        logger.info(f"Downloaded {downloaded} artifacts for run {run_id}")
        return downloaded

    def upload_dataset(
        self,
        dataset_name: str,
        local_path: Union[str, Path],
        version: str = "latest",
        metadata: Optional[Dict[str, str]] = None
    ) -> bool:
        """
        Upload dataset to versioned location

        Args:
            dataset_name: Dataset name
            local_path: Local file/directory
            version: Version identifier
            metadata: Optional metadata

        Returns:
            Success status
        """
        local_path = Path(local_path)

        if local_path.is_file():
            gcs_path = f"datasets/{dataset_name}/{version}/{local_path.name}"
            return self.upload_file(local_path, gcs_path, metadata)

        elif local_path.is_dir():
            uploaded = 0
            for local_file in local_path.rglob('*'):
                if local_file.is_file():
                    rel_path = local_file.relative_to(local_path)
                    gcs_path = f"datasets/{dataset_name}/{version}/{rel_path}"
                    if self.upload_file(local_file, gcs_path, metadata):
                        uploaded += 1

            logger.info(f"Uploaded {uploaded} files for dataset {dataset_name}:{version}")
            return uploaded > 0

        else:
            logger.error(f"Invalid path: {local_path}")
            return False

    def get_object_metadata(self, gcs_path: str) -> Optional[Dict[str, Any]]:
        """
        Get object metadata

        Args:
            gcs_path: GCS object path

        Returns:
            Metadata dictionary or None
        """
        if not self.is_available():
            return None

        try:
            blob = self.bucket.blob(gcs_path)
            blob.reload()

            metadata = {
                'name': blob.name,
                'size': blob.size,
                'content_type': blob.content_type,
                'created': blob.time_created.isoformat() if blob.time_created else None,
                'updated': blob.updated.isoformat() if blob.updated else None,
                'metadata': blob.metadata or {}
            }

            return metadata

        except NotFound:
            return None
        except Exception as e:
            logger.error(f"Failed to get metadata: {e}")
            return None


# Global client instance
_client = None

def get_client() -> GCSClient:
    """Get or create GCS client singleton"""
    global _client
    if _client is None:
        _client = GCSClient()
    return _client


# Convenience functions
def upload_file(
    local_path: Union[str, Path],
    gcs_path: str,
    metadata: Optional[Dict[str, str]] = None
) -> bool:
    """Upload file to GCS"""
    return get_client().upload_file(local_path, gcs_path, metadata)


def download_file(gcs_path: str, local_path: Union[str, Path]) -> bool:
    """Download file from GCS"""
    return get_client().download_file(gcs_path, local_path)


def upload_experiment_artifacts(
    run_id: str,
    artifacts_dir: Union[str, Path],
    include_patterns: Optional[List[str]] = None
) -> int:
    """Upload experiment artifacts"""
    return get_client().upload_experiment_artifacts(run_id, artifacts_dir, include_patterns)


def download_experiment_artifacts(run_id: str, local_dir: Union[str, Path]) -> int:
    """Download experiment artifacts"""
    return get_client().download_experiment_artifacts(run_id, local_dir)


def upload_dataset(
    dataset_name: str,
    local_path: Union[str, Path],
    version: str = "latest",
    metadata: Optional[Dict[str, str]] = None
) -> bool:
    """Upload dataset"""
    return get_client().upload_dataset(dataset_name, local_path, version, metadata)
