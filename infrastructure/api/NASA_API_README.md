# NASA Data API Documentation

## Overview

The MotorHandPro API now includes comprehensive endpoints for fetching, processing, and streaming NASA comet data (3I/ATLAS). This integration connects real NASA data sources with the PRIMAL Logic framework and LAM temporal displacement system.

## Architecture

```
┌─────────────────────┐
│  NASA Data Sources  │
│  - JPL Horizons     │
│  - MPC Astrometry   │
│  - Simulated Feed   │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│   FastAPI Server    │
│  /nasa/* endpoints  │
└──────────┬──────────┘
           │
           ├─────────────────┐
           ▼                 ▼
┌─────────────────┐   ┌──────────────┐
│  TimescaleDB    │   │ MQTT Broker  │
│  nasa_data.*    │   │ motorhand/   │
└─────────────────┘   │ nasa/*       │
                      └──────┬───────┘
                             ▼
                      ┌──────────────┐
                      │  WebSocket   │
                      │  Streaming   │
                      └──────────────┘
```

## API Endpoints

### 1. Check NASA Data Status

**GET** `/nasa/status`

Check if NASA data client is available and ready.

**Response:**
```json
{
  "status": "available",
  "client": "NASACometDataClient",
  "data_sources": ["horizons", "mpc", "simulated"],
  "timestamp": "2025-12-07T12:00:00Z"
}
```

**Authentication:** None (public endpoint)

---

### 2. Fetch NASA Comet Data

**POST** `/nasa/comet/fetch`

Fetch comet observations from NASA data sources.

**Authentication:** Bearer token required

**Request Body:**
```json
{
  "data_source": "horizons",
  "start_time": "2025-12-07T00:00:00",
  "end_time": "2025-12-08T00:00:00",
  "step": "1h",
  "days_back": 7
}
```

**Parameters:**
- `data_source` (required): One of `"horizons"`, `"mpc"`, or `"simulated"`
- `start_time` (optional): ISO 8601 timestamp (for Horizons)
- `end_time` (optional): ISO 8601 timestamp (for Horizons)
- `step` (optional): Time step for Horizons (e.g., "1h", "30m")
- `days_back` (optional): Days back for MPC data (default: 7)

**Response:**
```json
{
  "status": "ok",
  "data_source": "horizons",
  "count": 24,
  "observations": [
    {
      "timestamp": "2025-12-07T00:00:00",
      "ra": 123.456,
      "dec": 45.678,
      "distance_au": 2.345,
      "velocity_km_s": 15.2,
      "magnitude": 8.5,
      "gas_production_rate": 1234.5,
      "tail_length_km": 50000.0
    }
  ]
}
```

**Side Effects:**
- Stores observations in `nasa_data.comet_observations` table
- Publishes MQTT message to `motorhand/nasa/comet/observations`

---

### 3. Get Stored Observations

**GET** `/nasa/comet/observations?limit=100&data_source=horizons`

Retrieve stored NASA comet observations from database.

**Authentication:** Bearer token required

**Query Parameters:**
- `limit` (optional): Maximum number of observations (default: 100)
- `data_source` (optional): Filter by data source

**Response:**
```json
{
  "status": "ok",
  "count": 100,
  "observations": [
    {
      "id": 1,
      "time": "2025-12-07T00:00:00Z",
      "ra": 123.456,
      "dec": 45.678,
      "distance_au": 2.345,
      "velocity_km_s": 15.2,
      "magnitude": 8.5,
      "gas_production_rate": 1234.5,
      "tail_length_km": 50000.0,
      "data_source": "horizons",
      "processed": false,
      "created_at": "2025-12-07T12:00:00Z"
    }
  ]
}
```

---

### 4. Process Observations Through PRIMAL Operator

**POST** `/nasa/comet/process`

Process unprocessed observations through the Recursive Planck Operator.

**Authentication:** Bearer token required

**Response:**
```json
{
  "status": "ok",
  "count": 24,
  "processed_states": [
    {
      "timestamp": "2025-12-07T00:00:00",
      "primal_state": {
        "n": 1,
        "signal": 0.567,
        "memory_integral": 1.234,
        "error": 0.012,
        "anomaly_score": 0.234
      }
    }
  ]
}
```

**Side Effects:**
- Stores processed states in `nasa_data.processed_states` table
- Marks observations as `processed = true`
- Publishes MQTT message to `motorhand/nasa/comet/processed`

---

### 5. Get Processed States

**GET** `/nasa/comet/processed?limit=100`

Retrieve processed PRIMAL states with observation data.

**Authentication:** Bearer token required

**Query Parameters:**
- `limit` (optional): Maximum number of states (default: 100)

**Response:**
```json
{
  "status": "ok",
  "count": 100,
  "processed_states": [
    {
      "id": 1,
      "time": "2025-12-07T00:00:00Z",
      "observation_id": 123,
      "primal_n": 1,
      "signal": 0.567,
      "memory_integral": 1.234,
      "error": 0.012,
      "anomaly_score": 0.234,
      "ra": 123.456,
      "dec": 45.678,
      "distance_au": 2.345
    }
  ]
}
```

---

## WebSocket Streaming

The WebSocket server supports real-time NASA data streaming.

### Connect

```javascript
const ws = new WebSocket('ws://localhost:8765');

// Authenticate
ws.send(JSON.stringify({
  type: 'auth',
  token: 'your_jwt_token_here'
}));
```

### Subscribe to NASA Data Topics

```javascript
// Subscribe to NASA comet observations
ws.send(JSON.stringify({
  type: 'subscribe',
  topics: ['motorhand/nasa/comet/observations', 'motorhand/nasa/comet/processed']
}));
```

### Query NASA Data

```javascript
// Query observations
ws.send(JSON.stringify({
  type: 'query',
  query_type: 'nasa_comet_observations',
  params: {
    limit: 50,
    data_source: 'horizons'
  }
}));

// Query processed states
ws.send(JSON.stringify({
  type: 'query',
  query_type: 'nasa_processed_states',
  params: {
    limit: 50
  }
}));
```

### Receive Real-Time Updates

```javascript
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);

  if (data.type === 'mqtt') {
    console.log(`Topic: ${data.topic}`);
    console.log(`Payload: ${data.payload}`);
  }
};
```

---

## Database Schema

### `nasa_data.comet_observations`

TimescaleDB hypertable for storing comet observations.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL | Primary key |
| `time` | TIMESTAMPTZ | Observation timestamp (unique) |
| `ra` | DOUBLE PRECISION | Right ascension (degrees) |
| `dec` | DOUBLE PRECISION | Declination (degrees) |
| `distance_au` | DOUBLE PRECISION | Distance from Earth (AU) |
| `velocity_km_s` | DOUBLE PRECISION | Velocity (km/s) |
| `magnitude` | DOUBLE PRECISION | Apparent magnitude |
| `gas_production_rate` | DOUBLE PRECISION | Gas production (g/s) |
| `tail_length_km` | DOUBLE PRECISION | Tail length (km) |
| `data_source` | VARCHAR(50) | Source: horizons/mpc/simulated |
| `processed` | BOOLEAN | Processed through operator |
| `created_at` | TIMESTAMPTZ | Record creation time |

### `nasa_data.processed_states`

TimescaleDB hypertable for storing PRIMAL processed states.

| Column | Type | Description |
|--------|------|-------------|
| `id` | SERIAL | Primary key |
| `time` | TIMESTAMPTZ | State timestamp |
| `observation_id` | INTEGER | FK to comet_observations |
| `primal_n` | INTEGER | PRIMAL iteration count |
| `signal` | DOUBLE PRECISION | Signal value |
| `memory_integral` | DOUBLE PRECISION | Memory integral |
| `error` | DOUBLE PRECISION | Error value |
| `anomaly_score` | DOUBLE PRECISION | Anomaly detection score |
| `created_at` | TIMESTAMPTZ | Record creation time |

---

## MQTT Topics

### Published by API

- `motorhand/nasa/comet/observations` - New observations fetched
- `motorhand/nasa/comet/processed` - Observations processed

### Message Format

```json
{
  "count": 24,
  "source": "horizons"
}
```

---

## Example Usage

### Python

```python
import requests

BASE_URL = "http://localhost:8000"
TOKEN = "your_jwt_token"

headers = {"Authorization": f"Bearer {TOKEN}"}

# Check status
response = requests.get(f"{BASE_URL}/nasa/status")
print(response.json())

# Fetch data from JPL Horizons
response = requests.post(
    f"{BASE_URL}/nasa/comet/fetch",
    headers=headers,
    json={
        "data_source": "horizons",
        "start_time": "2025-12-07T00:00:00",
        "end_time": "2025-12-08T00:00:00",
        "step": "1h"
    }
)
print(f"Fetched {response.json()['count']} observations")

# Process observations
response = requests.post(
    f"{BASE_URL}/nasa/comet/process",
    headers=headers
)
print(f"Processed {response.json()['count']} states")

# Get processed states
response = requests.get(
    f"{BASE_URL}/nasa/comet/processed?limit=10",
    headers=headers
)
print(response.json())
```

### cURL

```bash
# Check status
curl http://localhost:8000/nasa/status

# Fetch simulated data
curl -X POST http://localhost:8000/nasa/comet/fetch \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"data_source": "simulated"}'

# Get observations
curl http://localhost:8000/nasa/comet/observations?limit=50 \
  -H "Authorization: Bearer $TOKEN"
```

---

## Integration with Live Pipeline

The `live_nasa_pipeline.py` script can be integrated with the API:

```python
from datetime import datetime
import requests

# Pipeline fetches data
observations = pipeline.fetch_live_data()

# Post to API
for obs in observations:
    requests.post(
        "http://localhost:8000/nasa/comet/fetch",
        headers={"Authorization": f"Bearer {token}"},
        json={
            "data_source": "horizons",
            "start_time": obs.timestamp.isoformat(),
            ...
        }
    )
```

---

## Deployment

### Running the API Server

```bash
cd infrastructure/api
python fastapi_server.py
```

Server runs on `http://0.0.0.0:8000`

### Running the WebSocket Server

```bash
cd infrastructure/websocket
python websocket_server.py
```

WebSocket runs on `ws://0.0.0.0:8765`

### Database Migration

```bash
psql $DATABASE_URL -f infrastructure/database/migrations/003_nasa_data_schema.sql
```

---

## Monitoring

### Prometheus Metrics

The API exposes Prometheus metrics at `/metrics`:

```
# View metrics
curl http://localhost:8000/metrics
```

### API Documentation

Interactive API documentation available at:
- Swagger UI: `http://localhost:8000/docs`
- ReDoc: `http://localhost:8000/redoc`

---

## Security

All NASA data endpoints (except `/nasa/status`) require JWT authentication:

1. Obtain token via `/auth/login`
2. Include in `Authorization: Bearer <token>` header
3. Token valid for 24 hours (configurable via `JWT_EXPIRATION_HOURS`)

---

## Support

For issues or questions:
- Check server logs
- Verify database connection
- Ensure NASA data client dependencies are installed
- Check MQTT broker connectivity
