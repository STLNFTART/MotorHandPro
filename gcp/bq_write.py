"""
BigQuery Storage Write API for High-Throughput Streaming

Uses BigQuery Storage Write API for high-volume, low-latency writes.
Advantages over insert_rows_json:
- Lower latency (milliseconds vs seconds)
- Higher throughput (millions of rows/sec)
- Exactly-once semantics
- Lower cost for bulk data

Use this for:
- Real-time telemetry streams
- High-frequency sensor data
- Large batch uploads

Use regular bq.py for:
- Low-volume writes
- Metadata/configuration
- Simple use cases
"""

import logging
from typing import Any, Dict, List, Optional
from datetime import datetime

logger = logging.getLogger(__name__)

try:
    from google.cloud import bigquery_storage_v1
    from google.cloud.bigquery_storage_v1 import types, writer
    from google.protobuf import descriptor_pb2
    import json
    STORAGE_WRITE_AVAILABLE = True
except ImportError:
    STORAGE_WRITE_AVAILABLE = False
    logger.warning("google-cloud-bigquery-storage not installed. Install with: pip install google-cloud-bigquery-storage")

from . import config


class StorageWriteClient:
    """BigQuery Storage Write API client for high-throughput streaming"""

    def __init__(self):
        self.client: Optional[bigquery_storage_v1.BigQueryWriteClient] = None

        if not config.is_gcp_enabled():
            logger.info("GCP backend disabled, Storage Write client not initialized")
            return

        if not STORAGE_WRITE_AVAILABLE:
            logger.error("BigQuery Storage Write library not available")
            return

        try:
            self.client = bigquery_storage_v1.BigQueryWriteClient()
            logger.info("Storage Write client initialized")
        except Exception as e:
            logger.error(f"Failed to initialize Storage Write client: {e}")
            self.client = None

    def is_available(self) -> bool:
        """Check if Storage Write is available"""
        return self.client is not None

    def stream_telemetry(
        self,
        records: List[Dict[str, Any]],
        table_name: str = "raw_telemetry"
    ) -> bool:
        """
        Stream telemetry using Storage Write API

        Args:
            records: List of telemetry dictionaries
            table_name: Target table name

        Returns:
            Success status
        """
        if not self.is_available():
            logger.debug("Storage Write not available")
            return False

        try:
            # Table path
            parent = f"projects/{config.project}/datasets/{config.bq_dataset}/tables/{table_name}"

            # Create write stream
            write_stream = types.WriteStream()
            write_stream.type_ = types.WriteStream.Type.PENDING

            write_stream = self.client.create_write_stream(
                parent=parent,
                write_stream=write_stream
            )

            # Prepare serialized rows
            serialized_rows = []
            for record in records:
                # Ensure timestamp
                if 'timestamp' not in record:
                    record['timestamp'] = datetime.utcnow().isoformat()

                # Convert to JSON string (protobuf will parse)
                serialized_rows.append(json.dumps(record).encode('utf-8'))

            # Create append request
            request = types.AppendRowsRequest()
            request.write_stream = write_stream.name

            # Append rows
            proto_rows = types.AppendRowsRequest.ProtoData()
            proto_data = types.ProtoRows()
            proto_data.serialized_rows = serialized_rows
            proto_rows.rows = proto_data

            request.proto_rows = proto_rows

            # Send request
            response = self.client.append_rows(iter([request]))

            # Check response
            for resp in response:
                if resp.HasField('error'):
                    logger.error(f"Storage Write error: {resp.error}")
                    return False

            # Finalize stream
            self.client.finalize_write_stream(name=write_stream.name)

            # Commit batch
            batch_commit_request = types.BatchCommitWriteStreamsRequest()
            batch_commit_request.parent = parent
            batch_commit_request.write_streams = [write_stream.name]

            self.client.batch_commit_write_streams(batch_commit_request)

            logger.info(f"Streamed {len(records)} records via Storage Write API")
            return True

        except Exception as e:
            logger.error(f"Failed to stream telemetry: {e}")
            return False

    def stream_batch(
        self,
        records: List[Dict[str, Any]],
        table_name: str,
        batch_size: int = 1000
    ) -> tuple[int, int]:
        """
        Stream large batch of records in chunks

        Args:
            records: List of records
            table_name: Target table
            batch_size: Records per batch

        Returns:
            (successful_count, failed_count)
        """
        if not self.is_available():
            return 0, len(records)

        successful = 0
        failed = 0

        for i in range(0, len(records), batch_size):
            batch = records[i:i + batch_size]
            if self.stream_telemetry(batch, table_name):
                successful += len(batch)
            else:
                failed += len(batch)

        logger.info(f"Batch streaming complete: {successful} succeeded, {failed} failed")
        return successful, failed


# Global client instance
_client = None

def get_client() -> StorageWriteClient:
    """Get or create Storage Write client singleton"""
    global _client
    if _client is None:
        _client = StorageWriteClient()
    return _client


# Convenience functions
def stream_telemetry(records: List[Dict[str, Any]], table_name: str = "raw_telemetry") -> bool:
    """Stream telemetry using Storage Write API"""
    return get_client().stream_telemetry(records, table_name)


def stream_batch(
    records: List[Dict[str, Any]],
    table_name: str,
    batch_size: int = 1000
) -> tuple[int, int]:
    """Stream large batch of records"""
    return get_client().stream_batch(records, table_name, batch_size)
