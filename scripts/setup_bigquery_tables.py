#!/usr/bin/env python3
"""
Setup BigQuery tables for MotorHandPro telemetry and experiment tracking.

Usage:
    python setup_bigquery_tables.py --project PROJECT_ID --dataset DATASET_NAME

Requirements:
    - google-cloud-bigquery
    - Authenticated via ADC: gcloud auth application-default login
    - Permissions: bigquery.datasets.create, bigquery.tables.create
"""

import argparse
import json
import sys
from pathlib import Path
from typing import Dict, List

try:
    from google.cloud import bigquery
    from google.cloud.exceptions import GoogleCloudError
except ImportError:
    print("Error: google-cloud-bigquery not installed")
    print("Install with: pip install google-cloud-bigquery")
    sys.exit(1)


def load_schema(schema_file: Path) -> List[bigquery.SchemaField]:
    """Load BigQuery schema from JSON file"""
    with open(schema_file, 'r') as f:
        schema_json = json.load(f)

    schema_fields = []
    for field in schema_json:
        # Handle nested fields (for RECORD type)
        if 'fields' in field:
            nested_fields = [
                bigquery.SchemaField(
                    f['name'],
                    f['type'],
                    mode=f.get('mode', 'NULLABLE'),
                    description=f.get('description', '')
                )
                for f in field['fields']
            ]
            schema_field = bigquery.SchemaField(
                field['name'],
                field['type'],
                mode=field.get('mode', 'NULLABLE'),
                description=field.get('description', ''),
                fields=nested_fields
            )
        else:
            schema_field = bigquery.SchemaField(
                field['name'],
                field['type'],
                mode=field.get('mode', 'NULLABLE'),
                description=field.get('description', '')
            )

        schema_fields.append(schema_field)

    return schema_fields


def create_dataset(client: bigquery.Client, project_id: str, dataset_id: str) -> bool:
    """Create BigQuery dataset if it doesn't exist"""
    dataset_ref = f"{project_id}.{dataset_id}"

    try:
        client.get_dataset(dataset_ref)
        print(f"✓ Dataset {dataset_ref} already exists")
        return True
    except GoogleCloudError:
        pass

    # Create dataset
    dataset = bigquery.Dataset(dataset_ref)
    dataset.location = "US"
    dataset.description = "MotorHandPro telemetry, experiments, and analytics"

    try:
        dataset = client.create_dataset(dataset, timeout=30)
        print(f"✓ Created dataset {dataset_ref}")
        return True
    except GoogleCloudError as e:
        print(f"✗ Failed to create dataset: {e}")
        return False


def create_table(
    client: bigquery.Client,
    project_id: str,
    dataset_id: str,
    table_id: str,
    schema: List[bigquery.SchemaField],
    description: str = "",
    partitioning_field: str = None,
    clustering_fields: List[str] = None
) -> bool:
    """Create BigQuery table"""
    table_ref = f"{project_id}.{dataset_id}.{table_id}"

    try:
        client.get_table(table_ref)
        print(f"✓ Table {table_ref} already exists")
        return True
    except GoogleCloudError:
        pass

    # Create table
    table = bigquery.Table(table_ref, schema=schema)
    table.description = description

    # Partitioning (for time-series data)
    if partitioning_field:
        table.time_partitioning = bigquery.TimePartitioning(
            type_=bigquery.TimePartitioningType.DAY,
            field=partitioning_field
        )

    # Clustering (for query optimization)
    if clustering_fields:
        table.clustering_fields = clustering_fields

    try:
        table = client.create_table(table, timeout=30)
        print(f"✓ Created table {table_ref}")
        return True
    except GoogleCloudError as e:
        print(f"✗ Failed to create table: {e}")
        return False


def main():
    parser = argparse.ArgumentParser(
        description="Setup BigQuery tables for MotorHandPro"
    )
    parser.add_argument(
        '--project',
        required=True,
        help='GCP project ID'
    )
    parser.add_argument(
        '--dataset',
        default='motorhandpro',
        help='BigQuery dataset name (default: motorhandpro)'
    )
    parser.add_argument(
        '--schemas-dir',
        default='schemas/bigquery',
        help='Path to schemas directory'
    )

    args = parser.parse_args()

    # Find schemas directory
    schemas_dir = Path(args.schemas_dir)
    if not schemas_dir.exists():
        # Try from repo root
        repo_root = Path(__file__).parent.parent
        schemas_dir = repo_root / 'schemas' / 'bigquery'

    if not schemas_dir.exists():
        print(f"Error: Schemas directory not found: {schemas_dir}")
        sys.exit(1)

    print(f"Using schemas from: {schemas_dir}")

    # Initialize BigQuery client
    try:
        client = bigquery.Client(project=args.project)
        print(f"✓ Authenticated as BigQuery client for project: {args.project}")
    except Exception as e:
        print(f"✗ Failed to initialize BigQuery client: {e}")
        print("\nAuthentication required. Run:")
        print("  gcloud auth application-default login")
        sys.exit(1)

    # Create dataset
    print(f"\n{'='*80}")
    print("Creating dataset...")
    print('='*80)
    if not create_dataset(client, args.project, args.dataset):
        sys.exit(1)

    # Define tables to create
    tables = [
        {
            'table_id': 'raw_telemetry',
            'schema_file': 'raw_telemetry.json',
            'description': 'Raw telemetry events from sensors and experiments',
            'partitioning_field': 'timestamp',
            'clustering_fields': ['source', 'event_type', 'run_id']
        },
        {
            'table_id': 'curated_timeseries',
            'schema_file': 'curated_timeseries.json',
            'description': 'Curated time-series data with quality checks',
            'partitioning_field': 'timestamp',
            'clustering_fields': ['series_id', 'metric_name']
        },
        {
            'table_id': 'experiment_runs',
            'schema_file': 'experiment_runs.json',
            'description': 'Experiment run metadata and results',
            'partitioning_field': 'timestamp',
            'clustering_fields': ['experiment_name', 'status']
        },
        {
            'table_id': 'metrics_validation',
            'schema_file': 'metrics_validation.json',
            'description': 'Metrics validation results and anomalies',
            'partitioning_field': 'timestamp',
            'clustering_fields': ['run_id', 'validation_type', 'status']
        }
    ]

    # Create tables
    print(f"\n{'='*80}")
    print("Creating tables...")
    print('='*80)

    success_count = 0
    for table_def in tables:
        schema_file = schemas_dir / table_def['schema_file']

        if not schema_file.exists():
            print(f"✗ Schema file not found: {schema_file}")
            continue

        # Load schema
        try:
            schema = load_schema(schema_file)
        except Exception as e:
            print(f"✗ Failed to load schema {schema_file}: {e}")
            continue

        # Create table
        success = create_table(
            client,
            args.project,
            args.dataset,
            table_def['table_id'],
            schema,
            description=table_def.get('description', ''),
            partitioning_field=table_def.get('partitioning_field'),
            clustering_fields=table_def.get('clustering_fields')
        )

        if success:
            success_count += 1

    # Summary
    print(f"\n{'='*80}")
    print("Summary")
    print('='*80)
    print(f"Created {success_count}/{len(tables)} tables")

    if success_count == len(tables):
        print("\n✓ BigQuery setup complete!")
        print(f"\nView dataset: https://console.cloud.google.com/bigquery?project={args.project}&d={args.dataset}")
        print("\nNext steps:")
        print("  1. Set environment variables:")
        print(f"     export GCP_PROJECT={args.project}")
        print(f"     export BQ_DATASET={args.dataset}")
        print(f"     export MHP_BACKEND=gcp")
        print("  2. Start REST server: python bridge/rest_server.py")
        print("  3. Test: curl http://localhost:8000/gcp/health")
    else:
        print("\n✗ Some tables failed to create. Check errors above.")
        sys.exit(1)


if __name__ == "__main__":
    main()
