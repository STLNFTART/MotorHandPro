# MotorHandPro Data Schemas

This directory contains data contracts and schemas for MotorHandPro's telemetry and storage systems.

## Structure

```
schemas/
├── bigquery/               # BigQuery table schemas
│   ├── raw_telemetry.json
│   ├── curated_timeseries.json
│   ├── experiment_runs.json
│   └── metrics_validation.json
└── telemetry_spec.md      # Telemetry data specification
```

## BigQuery Schemas

### Creating Tables

Use the schemas to create BigQuery tables:

```bash
# Create raw telemetry table
bq mk --table \
  ${GCP_PROJECT}:${BQ_DATASET}.raw_telemetry \
  schemas/bigquery/raw_telemetry.json

# Create curated timeseries table
bq mk --table \
  ${GCP_PROJECT}:${BQ_DATASET}.curated_timeseries \
  schemas/bigquery/curated_timeseries.json

# Create experiment runs table
bq mk --table \
  ${GCP_PROJECT}:${BQ_DATASET}.experiment_runs \
  schemas/bigquery/experiment_runs.json

# Create metrics validation table
bq mk --table \
  ${GCP_PROJECT}:${BQ_DATASET}.metrics_validation \
  schemas/bigquery/metrics_validation.json
```

### Automated Setup

Use the provided setup script:

```bash
python scripts/setup_bigquery_tables.py \
  --project ${GCP_PROJECT} \
  --dataset ${BQ_DATASET}
```

## Telemetry Specification

See [telemetry_spec.md](telemetry_spec.md) for detailed telemetry data contracts including:

- Event types and payloads
- Data quality indicators
- Timestamp handling
- Units and naming conventions
- Retention policies
- Example client code

## Schema Versioning

Schemas follow semantic versioning:
- **Major**: Breaking changes (field removal, type changes)
- **Minor**: Backward-compatible additions
- **Patch**: Documentation updates

Current version: **1.0.0**

## Validation

Validate data against schemas before ingestion:

```python
from gcp import bq
import jsonschema

# Load schema
with open('schemas/bigquery/raw_telemetry.json') as f:
    schema = json.load(f)

# Validate data
telemetry_data = {...}
jsonschema.validate(telemetry_data, schema)

# Write to BigQuery
bq.write_telemetry([telemetry_data])
```

## Migration

When updating schemas:

1. Add new fields as **NULLABLE**
2. Create migration script for backward compatibility
3. Update telemetry_spec.md
4. Increment schema version
5. Test with sample data

## Contact

For schema changes or questions, open an issue or contact the data platform team.
