# MotorHandPro Telemetry Specification

## Overview

This document defines the telemetry data contracts for MotorHandPro, ensuring consistent data collection, transmission, and storage across all components.

## Telemetry Event Types

### 1. Sensor Readings

**Event Type:** `sensor_reading`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "emg_sensor_01",
  "event_type": "sensor_reading",
  "value": 0.0042,
  "unit": "volts",
  "quality": "good",
  "metadata": {
    "sensor_type": "EMG",
    "sampling_rate_hz": 1000,
    "electrode_position": "forearm_flexor"
  },
  "device_id": "motorhand_v2_001",
  "run_id": "exp_20250115_103045"
}
```

### 2. Experiment State Changes

**Event Type:** `experiment_state`

**States:** `started`, `running`, `paused`, `resumed`, `completed`, `failed`, `cancelled`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "experiment_controller",
  "event_type": "experiment_state",
  "metadata": {
    "state": "started",
    "experiment_name": "orbital_decay_analysis",
    "run_id": "exp_20250115_103045",
    "parameters": {
      "altitude_km": 400,
      "duration_days": 365
    }
  }
}
```

### 3. System Metrics

**Event Type:** `system_metric`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "system_monitor",
  "event_type": "system_metric",
  "value": 75.3,
  "unit": "percent",
  "metadata": {
    "metric_type": "cpu_utilization",
    "hostname": "compute-node-01",
    "core_count": 16
  }
}
```

### 4. Model Training Metrics

**Event Type:** `training_metric`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "model_trainer",
  "event_type": "training_metric",
  "value": 0.9234,
  "unit": "accuracy",
  "metadata": {
    "run_id": "exp_20250115_103045",
    "epoch": 42,
    "metric_name": "validation_accuracy",
    "loss": 0.0234
  }
}
```

### 5. Error Events

**Event Type:** `error`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "data_pipeline",
  "event_type": "error",
  "quality": "bad",
  "metadata": {
    "error_type": "ConnectionTimeout",
    "error_message": "Failed to connect to BigQuery",
    "stack_trace": "...",
    "severity": "warning",
    "retry_count": 3
  },
  "run_id": "exp_20250115_103045"
}
```

### 6. Data Quality Events

**Event Type:** `data_quality`

**Payload:**
```json
{
  "timestamp": "2025-01-15T10:30:45.123Z",
  "source": "quality_checker",
  "event_type": "data_quality",
  "value": 0.95,
  "unit": "score",
  "quality": "good",
  "metadata": {
    "check_type": "completeness",
    "dataset": "emg_recordings",
    "missing_rate": 0.05,
    "expected_rate": 0.02
  }
}
```

## Data Ingestion Patterns

### Real-Time Streaming

Use for:
- High-frequency sensor data (>10 Hz)
- Real-time monitoring
- Live dashboards

**Method:** BigQuery Storage Write API (`gcp.bq_write.stream_telemetry()`)

### Batch Ingestion

Use for:
- Post-experiment analysis
- Historical data migration
- Bulk uploads

**Method:** BigQuery insert (`gcp.bq.write_telemetry()`)

### Micro-Batching

Use for:
- Medium-frequency data (0.1-10 Hz)
- Balancing latency and cost

**Method:** Buffer locally, flush every 1-10 seconds

## Quality Indicators

### Quality Levels

- **good**: Data passed all quality checks
- **suspect**: Data has minor issues but is usable
- **bad**: Data failed quality checks, should be filtered

### Quality Checks

1. **Range Check**: Value within expected bounds
2. **Rate Check**: Change rate within expected limits
3. **Completeness**: No missing required fields
4. **Freshness**: Data timestamp is recent
5. **Consistency**: Data consistent with related measurements

## Time Handling

### Timestamps

- **Format:** ISO 8601 with timezone: `2025-01-15T10:30:45.123Z`
- **Timezone:** Always UTC
- **Precision:** Milliseconds minimum, microseconds preferred

### Time Synchronization

- Use NTP for device time synchronization
- Include both `timestamp` (event time) and `ingestion_time` (system time)
- Monitor clock skew for distributed systems

## Metadata Standards

### Required Fields

- `timestamp`: Event timestamp (UTC)
- `source`: Data source identifier
- `event_type`: Event type string

### Recommended Fields

- `run_id`: Experiment run identifier
- `device_id`: Physical device identifier
- `quality`: Data quality indicator
- `unit`: Unit of measurement (for numeric values)

### Optional Fields

- `location`: Geographic location (GEOGRAPHY type)
- `user`: User identifier
- `version`: Data schema version

## Units and Naming Conventions

### Units

Use SI units by default:
- Length: `meters`, `kilometers`
- Mass: `kilograms`
- Time: `seconds`
- Temperature: `celsius`, `kelvin`
- Voltage: `volts`
- Current: `amperes`
- Angle: `radians`, `degrees`

### Naming Conventions

- **snake_case** for field names: `sensor_reading`, `run_id`
- **lowercase** for event types: `sensor_reading`, not `SensorReading`
- **descriptive** identifiers: `emg_sensor_01`, not `s1`

## Data Retention

### Raw Telemetry

- **Hot Storage:** 30 days (frequent access)
- **Warm Storage:** 90 days (occasional access)
- **Cold Storage:** 1 year+ (archival)

### Curated Timeseries

- **Hot Storage:** 90 days
- **Warm Storage:** 1 year
- **Cold Storage:** 5 years+

### Experiment Runs

- **Retention:** Indefinite (or per data governance policy)

## Compliance and Privacy

### PII Handling

- **No PII** in telemetry by default
- Use anonymized identifiers
- Hash or tokenize sensitive fields

### HIPAA Compliance (if applicable)

- Encrypt data in transit and at rest
- Audit all access
- Implement data retention policies

## Example Client Code

### Python

```python
from gcp import bq

# Write single telemetry event
telemetry = {
    'timestamp': datetime.utcnow().isoformat(),
    'source': 'emg_sensor_01',
    'event_type': 'sensor_reading',
    'value': 0.0042,
    'unit': 'volts',
    'quality': 'good'
}

bq.write_telemetry([telemetry])

# Write experiment run
bq.write_experiment_run(
    run_id='exp_20250115_103045',
    experiment_name='orbital_decay',
    parameters={'altitude_km': 400},
    metrics={'final_altitude_km': 375},
    status='completed'
)
```

### REST API

```bash
# POST /telemetry/ingest
curl -X POST http://localhost:8000/telemetry/ingest \
  -H "Content-Type: application/json" \
  -d '{
    "timestamp": "2025-01-15T10:30:45.123Z",
    "source": "emg_sensor_01",
    "event_type": "sensor_reading",
    "value": 0.0042,
    "unit": "volts"
  }'
```

## Version History

- **v1.0** (2025-01-15): Initial specification
