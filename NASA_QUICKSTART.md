# NASA Data API - Quick Start Guide

Complete setup and testing guide for the NASA Data API integration.

## Prerequisites

- Python 3.8+
- PostgreSQL 12+ with TimescaleDB extension
- Optional: MQTT broker (Mosquitto)
- Optional: jq for JSON formatting

## 1. Database Setup

### Apply Migrations

```bash
cd /home/user/MotorHandPro

# Set database connection (choose one method):

# Method 1: Environment variables
export PGHOST=localhost
export PGPORT=5432
export PGUSER=postgres
export PGPASSWORD=your_password
export PGDATABASE=motorhand

# Method 2: Connection string
export DATABASE_URL="postgresql://user:password@localhost:5432/motorhand"

# Apply migrations
./infrastructure/database/apply_migrations.sh
```

Expected output:
```
✅ Connected to database
✅ All migrations applied successfully
✅ nasa_data schema exists
✅ comet_observations table exists
✅ processed_states table exists
```

### Verify Schema

```bash
psql -c "\dt+ nasa_data.*"
```

You should see:
- `nasa_data.comet_observations` (hypertable)
- `nasa_data.processed_states` (hypertable)

## 2. Install Dependencies

```bash
pip install fastapi uvicorn websockets paho-mqtt asyncpg python-jose passlib prometheus-client
```

Or with requirements file:
```bash
pip install -r infrastructure/api/requirements.txt  # if exists
```

## 3. Start the API Server

### Terminal 1: FastAPI Server

```bash
cd /home/user/MotorHandPro
python -m infrastructure.api
```

Server starts on: **http://localhost:8000**

Endpoints:
- API docs: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc
- Health: http://localhost:8000/health
- Metrics: http://localhost:8000/metrics

### Terminal 2: WebSocket Server (Optional)

```bash
cd /home/user/MotorHandPro
python -m infrastructure.websocket
```

WebSocket server starts on: **ws://localhost:8765**

## 4. Test the API

### Quick Test Script

Run automated tests with curl:

```bash
cd /home/user/MotorHandPro
./scripts/test_nasa_api.sh
```

This tests all NASA endpoints in sequence.

### Manual Testing

#### 1. Check Status (No Auth)

```bash
curl http://localhost:8000/nasa/status | jq
```

Expected:
```json
{
  "status": "available",
  "client": "NASACometDataClient",
  "data_sources": ["horizons", "mpc", "simulated"],
  "timestamp": "2025-12-07T..."
}
```

#### 2. Get Auth Token

First, login to get a JWT token:

```bash
TOKEN=$(curl -s -X POST http://localhost:8000/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "admin", "password": "your_password"}' \
  | jq -r '.access_token')

echo "Token: $TOKEN"
```

Or use a dev token for testing:
```bash
export TOKEN="dev-token"
```

#### 3. Fetch Simulated Data

```bash
curl -X POST http://localhost:8000/nasa/comet/fetch \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "data_source": "simulated",
    "start_time": "2025-01-01T00:00:00",
    "end_time": "2025-01-02T00:00:00",
    "step": "1h"
  }' | jq
```

#### 4. Query Observations

```bash
curl "http://localhost:8000/nasa/comet/observations?limit=10" \
  -H "Authorization: Bearer $TOKEN" | jq
```

#### 5. Process Through PRIMAL Operator

```bash
curl -X POST http://localhost:8000/nasa/comet/process \
  -H "Authorization: Bearer $TOKEN" | jq
```

#### 6. Get Processed States

```bash
curl "http://localhost:8000/nasa/comet/processed?limit=10" \
  -H "Authorization: Bearer $TOKEN" | jq
```

## 5. Run Automated Tests

### pytest

```bash
cd /home/user/MotorHandPro

# Install pytest if needed
pip install pytest requests

# Run tests
pytest tests/test_nasa_api_smoke.py -v

# Run with specific token
NASA_API_TOKEN="your_token" pytest tests/test_nasa_api_smoke.py -v
```

### Expected Output

```
tests/test_nasa_api_smoke.py::TestNASAStatus::test_nasa_status_endpoint_exists PASSED
tests/test_nasa_api_smoke.py::TestNASAStatus::test_nasa_status_returns_valid_data PASSED
tests/test_nasa_api_smoke.py::TestNASACometFetch::test_fetch_simulated_data PASSED
tests/test_nasa_api_smoke.py::TestNASACometObservations::test_get_observations_endpoint PASSED
tests/test_nasa_api_smoke.py::TestNASACometProcessing::test_process_endpoint_exists PASSED
tests/test_nasa_api_smoke.py::TestNASAEndToEnd::test_full_pipeline PASSED

============= 6 passed in 2.34s =============
```

## 6. Monitor MQTT Streams (Optional)

If you have an MQTT broker running:

```bash
# Subscribe to all NASA topics
mosquitto_sub -t 'motorhand/nasa/comet/#' -v

# In another terminal, trigger data fetch to see messages
curl -X POST http://localhost:8000/nasa/comet/fetch \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"data_source": "simulated"}' | jq
```

You should see:
```
motorhand/nasa/comet/observations {"count": 24, "source": "simulated"}
```

## 7. WebSocket Streaming

### JavaScript/Browser Example

```javascript
const ws = new WebSocket("ws://localhost:8765");

// Authenticate
ws.onopen = () => {
  ws.send(JSON.stringify({
    type: "auth",
    token: "your_jwt_token"
  }));
};

// Subscribe to NASA topics
ws.addEventListener("message", (event) => {
  const data = JSON.parse(event.data);

  if (data.type === "welcome") {
    // Subscribe to NASA observations
    ws.send(JSON.stringify({
      type: "subscribe",
      topics: ["motorhand/nasa/comet/observations", "motorhand/nasa/comet/processed"]
    }));

    // Query observations
    ws.send(JSON.stringify({
      type: "query",
      query_type: "nasa_comet_observations",
      params: { limit: 10 }
    }));
  }

  if (data.type === "mqtt") {
    console.log("MQTT message:", data.topic, data.payload);
  }
});
```

### Python WebSocket Client

```python
import asyncio
import websockets
import json

async def nasa_websocket_client():
    uri = "ws://localhost:8765"
    async with websockets.connect(uri) as ws:
        # Authenticate
        await ws.send(json.dumps({
            "type": "auth",
            "token": "your_jwt_token"
        }))

        # Wait for welcome
        response = await ws.recv()
        print(json.loads(response))

        # Subscribe to NASA topics
        await ws.send(json.dumps({
            "type": "subscribe",
            "topics": ["motorhand/nasa/comet/observations"]
        }))

        # Query observations
        await ws.send(json.dumps({
            "type": "query",
            "query_type": "nasa_comet_observations",
            "params": {"limit": 5}
        }))

        # Receive messages
        while True:
            message = await ws.recv()
            print(json.loads(message))

asyncio.run(nasa_websocket_client())
```

## 8. Production Deployment

### Docker Compose

Add to your `docker-compose.yml`:

```yaml
services:
  motorhand-api:
    build:
      context: .
      dockerfile: infrastructure/docker/Dockerfile.fastapi
    ports:
      - "8000:8000"
    environment:
      - DATABASE_URL=postgresql://motorhand:password@timescaledb:5432/motorhand
      - MQTT_BROKER=mqtt:1883
      - JWT_SECRET=change_in_production
    depends_on:
      - timescaledb
      - mqtt

  motorhand-websocket:
    build:
      context: .
      dockerfile: infrastructure/docker/Dockerfile.websocket
    ports:
      - "8765:8765"
    environment:
      - DATABASE_URL=postgresql://motorhand:password@timescaledb:5432/motorhand
      - MQTT_BROKER=mqtt:1883
      - JWT_SECRET=change_in_production
    depends_on:
      - timescaledb
      - mqtt
      - motorhand-api
```

### Environment Variables

```bash
# Required
export DATABASE_URL="postgresql://user:pass@host:5432/db"

# Optional
export MQTT_BROKER="mqtt://mqtt-host:1883"
export JWT_SECRET="your-secret-key"
export JWT_EXPIRATION_HOURS="24"
```

## 9. Troubleshooting

### Server Won't Start

```bash
# Check if port 8000 is already in use
lsof -i :8000

# Check Python path
python -c "import sys; print('\n'.join(sys.path))"
```

### Database Connection Fails

```bash
# Test direct connection
psql postgresql://user:pass@host:5432/db -c "SELECT version();"

# Check if TimescaleDB extension is installed
psql -c "SELECT * FROM pg_extension WHERE extname = 'timescaledb';"
```

### NASA Client Not Available

```bash
# Check if nasa_comet_data.py exists
ls -la network_simulation_cluster/data_sources/nasa_comet_data.py

# Test import
python -c "from network_simulation_cluster.data_sources.nasa_comet_data import NASACometDataClient"
```

### Authentication Fails

```bash
# Bypass auth for testing (dev mode only)
export JWT_SECRET="dev-secret"
TOKEN="dev-token"
```

## 10. Next Steps

- Read full API documentation: `infrastructure/api/NASA_API_README.md`
- Explore Swagger UI: http://localhost:8000/docs
- Monitor with Prometheus: http://localhost:8000/metrics
- Integrate with notebooks: See example notebooks
- Set up continuous data pipeline: `python live_nasa_pipeline.py`

## Support

For issues:
1. Check logs from `python -m infrastructure.api`
2. Verify database schema with `\dt+ nasa_data.*`
3. Test endpoints with `./scripts/test_nasa_api.sh`
4. Run pytest: `pytest tests/test_nasa_api_smoke.py -v`

## Summary of Files Created

```
/home/user/MotorHandPro/
├── infrastructure/
│   ├── api/
│   │   ├── __main__.py                    # FastAPI entrypoint
│   │   ├── fastapi_server.py              # NASA endpoints
│   │   └── NASA_API_README.md             # Full API docs
│   ├── websocket/
│   │   ├── __main__.py                    # WebSocket entrypoint
│   │   └── websocket_server.py            # NASA query support
│   └── database/
│       ├── migrations/
│       │   └── 003_nasa_data_schema.sql   # NASA schema
│       └── apply_migrations.sh            # Migration runner
├── scripts/
│   └── test_nasa_api.sh                   # Manual test script
├── tests/
│   └── test_nasa_api_smoke.py             # pytest tests
└── NASA_QUICKSTART.md                     # This file
```
