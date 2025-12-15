# GCP Backend Integration for MotorHandPro

MotorHandPro supports optional Google Cloud Platform backends for telemetry storage, experiment tracking, and artifact management.

**Key Principle:** GCP integration is **optional** and uses **Application Default Credentials (ADC)** - NO KEYS IN REPO.

## Table of Contents

- [Overview](#overview)
- [Architecture](#architecture)
- [Prerequisites](#prerequisites)
- [Local Development Setup](#local-development-setup)
- [Production Deployment](#production-deployment)
- [API Usage](#api-usage)
- [Monitoring & Logging](#monitoring--logging)
- [Troubleshooting](#troubleshooting)

## Overview

### What GCP Backends Provide

- **BigQuery**: Telemetry storage, experiment tracking, time-series analytics
- **Cloud Storage**: Artifact storage (models, datasets, plots, notebooks)
- **Cloud Logging**: Structured application and audit logs
- **Cloud Monitoring**: Custom metrics and dashboards
- **Dataplex** (optional): Data catalog and governance

### When to Use GCP Backends

✅ **Use GCP when you need:**
- Scalable telemetry ingestion (>1M events/day)
- Long-term data retention and analytics
- Multi-user experiment tracking
- Enterprise data governance
- Cloud-based execution environments

❌ **Use local storage when you:**
- Are prototyping/developing locally
- Have small-scale experiments
- Don't need cloud infrastructure
- Want to minimize costs

## Architecture

```
MotorHandPro (Local/Edge)
    ↓
bridge/rest_server.py (FastAPI)
    ↓
bridge/gcp_backend.py (Optional Router)
    ↓
gcp/ (Client Modules)
    ├── bq.py (BigQuery)
    ├── gcs.py (Cloud Storage)
    ├── logging.py (Cloud Logging)
    └── monitoring.py (Cloud Monitoring)
    ↓
Google Cloud Platform
    ├── BigQuery (telemetry, experiments)
    ├── Cloud Storage (artifacts)
    ├── Cloud Logging (logs)
    └── Cloud Monitoring (metrics)
```

## Prerequisites

### Required APIs

Enable the following APIs in your GCP project:

```bash
gcloud services enable bigquery.googleapis.com
gcloud services enable storage.googleapis.com
gcloud services enable logging.googleapis.com
gcloud services enable monitoring.googleapis.com
gcloud services enable dataplex.googleapis.com  # Optional
```

### IAM Roles

Create a service account with these roles:

```bash
# Create service account
gcloud iam service-accounts create motorhandpro-sa \
  --display-name="MotorHandPro Service Account"

# Grant roles
gcloud projects add-iam-policy-binding ${GCP_PROJECT} \
  --member="serviceAccount:motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com" \
  --role="roles/bigquery.dataEditor"

gcloud projects add-iam-policy-binding ${GCP_PROJECT} \
  --member="serviceAccount:motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com" \
  --role="roles/bigquery.jobUser"

gcloud projects add-iam-policy-binding ${GCP_PROJECT} \
  --member="serviceAccount:motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com" \
  --role="roles/storage.objectAdmin"

gcloud projects add-iam-policy-binding ${GCP_PROJECT} \
  --member="serviceAccount:motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com" \
  --role="roles/logging.logWriter"

gcloud projects add-iam-policy-binding ${GCP_PROJECT} \
  --member="serviceAccount:motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com" \
  --role="roles/monitoring.metricWriter"
```

### Python Dependencies

Install GCP client libraries:

```bash
pip install google-cloud-bigquery google-cloud-storage \
            google-cloud-logging google-cloud-monitoring \
            google-cloud-bigquery-storage  # Optional, for high-throughput
```

Or use the requirements file:

```bash
pip install -r requirements-gcp.txt
```

## Local Development Setup

### 1. Authenticate with ADC

```bash
# Authenticate using your Google account
gcloud auth application-default login

# Or use service account key (for testing only)
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account-key.json"
```

**Important:** DO NOT commit service account keys to the repository.

### 2. Set Environment Variables

Create a `.env` file (see `.env.example`):

```bash
# Backend configuration
MHP_BACKEND=gcp  # or 'local'

# GCP project settings
GCP_PROJECT=your-project-id
BQ_DATASET=motorhandpro
GCS_BUCKET=motorhandpro-artifacts

# Optional: region
GCP_REGION=us-central1
```

Load environment variables:

```bash
export $(cat .env | xargs)
```

### 3. Create GCP Resources

Run the setup script:

```bash
python scripts/setup_bigquery_tables.py \
  --project ${GCP_PROJECT} \
  --dataset ${BQ_DATASET}

# Create GCS bucket
gsutil mb -p ${GCP_PROJECT} -l ${GCP_REGION} gs://${GCS_BUCKET}
```

### 4. Start the REST Server

```bash
cd bridge/
python rest_server.py
```

Check GCP backend health:

```bash
curl http://localhost:8000/gcp/health
```

Expected response:

```json
{
  "gcp_enabled": true,
  "backend": "gcp",
  "config": {
    "project": "your-project",
    "bq_dataset": "motorhandpro",
    "gcs_bucket": "motorhandpro-artifacts"
  },
  "services": {
    "bigquery": true,
    "gcs": true,
    "logging": true,
    "monitoring": true
  }
}
```

## Production Deployment

### GKE/Cloud Run Deployment

Use Workload Identity for authentication (no keys needed):

```yaml
# k8s deployment example
apiVersion: v1
kind: Pod
metadata:
  name: motorhandpro
spec:
  serviceAccountName: motorhandpro-ksa
  containers:
  - name: app
    image: gcr.io/your-project/motorhandpro:latest
    env:
    - name: MHP_BACKEND
      value: "gcp"
    - name: GCP_PROJECT
      value: "your-project"
    - name: BQ_DATASET
      value: "motorhandpro"
    - name: GCS_BUCKET
      value: "motorhandpro-artifacts"
```

Bind Kubernetes service account to GCP service account:

```bash
gcloud iam service-accounts add-iam-policy-binding \
  motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com \
  --role roles/iam.workloadIdentityUser \
  --member "serviceAccount:${GCP_PROJECT}.svc.id.goog[default/motorhandpro-ksa]"
```

### Compute Engine / VM Deployment

Attach service account to VM:

```bash
gcloud compute instances create motorhandpro-vm \
  --service-account=motorhandpro-sa@${GCP_PROJECT}.iam.gserviceaccount.com \
  --scopes=cloud-platform
```

## API Usage

### Telemetry Ingestion

```python
import requests

# POST telemetry data
telemetry = {
    "events": [
        {
            "source": "emg_sensor_01",
            "event_type": "sensor_reading",
            "value": 0.0042,
            "unit": "volts",
            "quality": "good"
        }
    ]
}

response = requests.post(
    "http://localhost:8000/gcp/telemetry/ingest",
    json=telemetry
)

print(response.json())
# {"success": true, "events_count": 1, "backend": "bigquery"}
```

### Experiment Tracking

```python
# Register experiment run
run = {
    "experiment_name": "orbital_decay",
    "parameters": {"altitude_km": 400, "duration_days": 365}
}

response = requests.post(
    "http://localhost:8000/gcp/runs/register",
    json=run
)

run_id = response.json()["run_id"]

# Update metrics
metrics = {
    "run_id": run_id,
    "metrics": {"final_altitude_km": 375, "decay_rate_km_per_day": 0.068},
    "status": "completed"
}

requests.post(
    "http://localhost:8000/gcp/runs/metrics",
    json=metrics
)

# Query metrics
response = requests.get(f"http://localhost:8000/gcp/runs/{run_id}/metrics")
print(response.json())
```

### Query Telemetry

```python
# Query recent telemetry
query = {
    "start_time": "2025-01-15T00:00:00Z",
    "end_time": "2025-01-15T23:59:59Z",
    "limit": 1000
}

response = requests.post(
    "http://localhost:8000/gcp/telemetry/query",
    json=query
)

records = response.json()["records"]
```

### Direct Client Usage

```python
from gcp import bq, gcs, logging as cloud_logging, monitoring

# Write telemetry
bq.write_telemetry([{
    "timestamp": "2025-01-15T10:30:45Z",
    "source": "sensor_01",
    "event_type": "reading",
    "value": 42.0
}])

# Upload artifact
gcs.upload_file(
    local_path="model.pkl",
    gcs_path="experiments/run_123/model.pkl"
)

# Write log
cloud_logging.log(
    message="Experiment started",
    severity="INFO",
    payload={"run_id": "run_123"}
)

# Write metric
monitoring.write_metric(
    metric_type="experiment_duration",
    value=3600.0,
    labels={"experiment": "orbital_decay"}
)
```

## Monitoring & Logging

### View Logs

```bash
# Recent logs
gcloud logging read "resource.type=global AND \
  labels.log_type=application" --limit 50

# Experiment logs
gcloud logging read "labels.run_id=run_123" --limit 100
```

### View Metrics

```bash
# Custom metrics
gcloud monitoring time-series list \
  --filter='metric.type="custom.googleapis.com/motorhandpro/experiment_duration"'
```

### BigQuery Queries

```sql
-- Recent telemetry
SELECT *
FROM `project.motorhandpro.raw_telemetry`
WHERE timestamp >= TIMESTAMP_SUB(CURRENT_TIMESTAMP(), INTERVAL 1 HOUR)
ORDER BY timestamp DESC
LIMIT 1000;

-- Experiment results
SELECT
  experiment_name,
  JSON_VALUE(parameters, '$.altitude_km') as altitude,
  JSON_VALUE(metrics, '$.final_altitude_km') as final_altitude,
  duration_seconds
FROM `project.motorhandpro.experiment_runs`
WHERE status = 'completed'
ORDER BY timestamp DESC;
```

## Troubleshooting

### GCP Backend Not Enabled

**Symptom:** API returns `"backend": "local"`

**Solution:**
1. Check environment variable: `echo $MHP_BACKEND` (should be "gcp")
2. Check GCP project: `echo $GCP_PROJECT`
3. Restart server after setting environment variables

### Authentication Errors

**Symptom:** `google.auth.exceptions.DefaultCredentialsError`

**Solution:**
```bash
# Re-authenticate
gcloud auth application-default login

# Or set credentials
export GOOGLE_APPLICATION_CREDENTIALS="/path/to/key.json"
```

### BigQuery Permission Denied

**Symptom:** `403 Forbidden` errors when writing to BigQuery

**Solution:**
1. Verify service account has `bigquery.dataEditor` role
2. Verify dataset exists: `bq ls -p ${GCP_PROJECT}`
3. Create dataset if needed: `bq mk ${GCP_PROJECT}:${BQ_DATASET}`

### GCS Upload Failures

**Symptom:** `403 Forbidden` or `404 Not Found` errors

**Solution:**
1. Verify bucket exists: `gsutil ls gs://${GCS_BUCKET}`
2. Create bucket: `gsutil mb gs://${GCS_BUCKET}`
3. Verify service account has `storage.objectAdmin` role

### High Costs

**Issue:** Unexpected GCP bills

**Solution:**
1. Enable BigQuery table expiration (e.g., 90 days for raw telemetry)
2. Use GCS lifecycle policies for artifact cleanup
3. Monitor usage in Cloud Console → Billing
4. Consider downsampling high-frequency telemetry
5. Use Storage Write API for bulk uploads (lower cost)

### Local Fallback

To temporarily disable GCP backend without changing code:

```bash
export MHP_BACKEND=local
```

All data will be stored in-memory locally.

## Cost Estimation

### Typical Monthly Costs (Estimates)

**Small Scale:**
- 1M telemetry events/day: ~$5
- 100 experiment runs/month: ~$1
- 10GB artifacts storage: ~$0.20
- **Total:** ~$10-15/month

**Medium Scale:**
- 100M events/day: ~$50
- 1000 runs/month: ~$5
- 100GB storage: ~$2
- **Total:** ~$100-150/month

**Large Scale:**
- 1B+ events/day: ~$500+
- 10,000+ runs/month: ~$50
- 1TB+ storage: ~$20
- **Total:** ~$1000+/month

See [GCP Pricing Calculator](https://cloud.google.com/products/calculator) for detailed estimates.

## Security Best Practices

1. ✅ **Never commit credentials** to repository
2. ✅ Use **Application Default Credentials** (ADC)
3. ✅ Use **Workload Identity** in GKE
4. ✅ Rotate service account keys regularly (if using keys)
5. ✅ Use **least privilege** IAM roles
6. ✅ Enable **VPC Service Controls** for production
7. ✅ Use **Customer-Managed Encryption Keys** (CMEK) for sensitive data
8. ✅ Enable **audit logging** for all data access

## Support

For issues or questions:
- Open an issue on GitHub
- Check [GCP Documentation](https://cloud.google.com/docs)
- Review telemetry spec: `schemas/telemetry_spec.md`

## Related Documentation

- [Telemetry Specification](../schemas/telemetry_spec.md)
- [BigQuery Schemas](../schemas/bigquery/)
- [GCP Client API Reference](../gcp/)
