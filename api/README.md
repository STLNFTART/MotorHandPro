# MotorHandPro API

FastAPI server providing REST endpoints for MotorHandPro with MongoDB Atlas integration.

## Features

- **LAM Actions**: Store and retrieve LAM (Large Action Model) actions
- **NASA Observations**: Store and retrieve NASA comet tracking data
- **Test Results**: Store and retrieve test execution results
- **Satellite Tracking**: Store and retrieve satellite position data
- **Statistics**: Get aggregated statistics across all data types

## Setup

### 1. Install Dependencies

```bash
cd api
pip3 install -r requirements.txt
```

### 2. Configure Environment

Copy the `.env.example` file to `.env` and fill in your MongoDB credentials:

```bash
cp ../.env.example ../.env
```

Edit `.env` and set:

```bash
MONGODB_URI=mongodb+srv://Primal:YOUR_PASSWORD@cluster0.xinq1u1.mongodb.net/?appName=Cluster0
API_HOST=0.0.0.0
API_PORT=8000
CORS_ORIGINS=http://localhost:3000,https://yourdomain.com
```

**Important**: Never commit `.env` to git! It's already in `.gitignore`.

### 3. Test MongoDB Connection

```bash
cd ../config
python3 mongodb.py
```

You should see:

```
✅ MongoDB connection successful
📊 Database Statistics:
...
```

### 4. Start the API Server

```bash
cd ../api
python3 server.py
```

Or with uvicorn directly:

```bash
uvicorn server:app --host 0.0.0.0 --port 8000 --reload
```

The API will be available at: `http://localhost:8000`

## API Documentation

Once the server is running, visit:

- **Interactive Docs**: http://localhost:8000/docs
- **ReDoc**: http://localhost:8000/redoc
- **OpenAPI JSON**: http://localhost:8000/openapi.json

## API Endpoints

### Health & Stats

- `GET /` - Root endpoint (health check)
- `GET /health` - Health check with database status
- `GET /stats` - Overall database statistics

### LAM Actions

- `POST /lam/actions` - Create new LAM action
- `GET /lam/actions` - Get LAM actions (filterable)
- `GET /lam/actions/stats` - Get LAM action statistics

### NASA Observations

- `POST /nasa/observations` - Create new NASA observation
- `GET /nasa/observations` - Get NASA observations (filterable)
- `GET /nasa/observations/stats` - Get observation statistics

### Test Results

- `POST /test/results` - Create new test result
- `GET /test/results` - Get test results (filterable)
- `GET /test/results/stats` - Get test result statistics

### Satellite Tracking

- `POST /satellite/data` - Create new satellite data point
- `GET /satellite/data` - Get satellite data (filterable)

## Usage Examples

### Using cURL

#### Create LAM Action

```bash
curl -X POST http://localhost:8000/lam/actions \
  -H "Content-Type: application/json" \
  -d '{
    "action_type": "plan_trip",
    "user_id": "user123",
    "primal_state": {
      "n": 0.5,
      "signal": 4.5,
      "memory_integral": 10.2,
      "error": 0.05,
      "anomaly_score": 0.01
    },
    "lam_integration": {
      "enabled": true,
      "E_displaced": 4.52
    }
  }'
```

#### Get LAM Actions

```bash
curl http://localhost:8000/lam/actions?user_id=user123&limit=10
```

#### Get Statistics

```bash
curl http://localhost:8000/stats
```

### Using Python

```python
import requests

# Create LAM action
response = requests.post(
    'http://localhost:8000/lam/actions',
    json={
        'action_type': 'plan_trip',
        'user_id': 'user123',
        'primal_state': {
            'n': 0.5,
            'signal': 4.5,
            'memory_integral': 10.2,
            'error': 0.05,
            'anomaly_score': 0.01
        },
        'lam_integration': {
            'enabled': True,
            'E_displaced': 4.52
        }
    }
)

print(response.json())

# Get LAM actions
response = requests.get(
    'http://localhost:8000/lam/actions',
    params={'user_id': 'user123', 'limit': 10}
)

print(response.json())
```

### Using JavaScript/Fetch

```javascript
// Create LAM action
fetch('http://localhost:8000/lam/actions', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({
    action_type: 'plan_trip',
    user_id: 'user123',
    primal_state: {
      n: 0.5,
      signal: 4.5,
      memory_integral: 10.2,
      error: 0.05,
      anomaly_score: 0.01
    },
    lam_integration: {
      enabled: true,
      E_displaced: 4.52
    }
  })
})
  .then(response => response.json())
  .then(data => console.log(data));

// Get LAM actions
fetch('http://localhost:8000/lam/actions?user_id=user123&limit=10')
  .then(response => response.json())
  .then(data => console.log(data));
```

## Integration with Existing Pipeline

### NASA Pipeline Integration

Modify `live_nasa_pipeline.py` to save observations to MongoDB:

```python
import requests

# In your pipeline after processing observations
for obs, state in zip(observations, processed_states):
    response = requests.post(
        'http://localhost:8000/nasa/observations',
        json={
            'comet_id': '3I/ATLAS',
            'observation': {
                'ra': obs.ra,
                'dec': obs.dec,
                'distance_au': obs.distance_au,
                'magnitude': obs.magnitude,
                'gas_production_rate': obs.gas_production_rate
            },
            'primal_state': state['primal_state'],
            'lam_integration': state.get('lam_integration')
        }
    )
```

### LAM Actions Integration

Modify LAM action handlers to save to MongoDB:

```python
import requests

# When a LAM action is executed
response = requests.post(
    'http://localhost:8000/lam/actions',
    json={
        'action_type': 'make_reservation',
        'user_id': user_id,
        'primal_state': primal_state,
        'lam_integration': lam_integration_data,
        'metadata': {'trip_id': trip_id, 'hotel': hotel_name}
    }
)
```

### Test Results Integration

Modify test runners to save results to MongoDB:

```python
import requests

# After running tests
response = requests.post(
    'http://localhost:8000/test/results',
    json={
        'branch': 'main',
        'test_type': 'LAM Core',
        'success': True,
        'results': {
            'tests_passed': 15,
            'tests_failed': 0,
            'duration': 0.015
        },
        'primal_constants': {
            'lambda': 0.16905,
            'D': 149.9992314,
            'lipschitz': 0.000129932
        }
    }
)
```

## Production Deployment

### Using systemd

Create `/etc/systemd/system/motorhandpro-api.service`:

```ini
[Unit]
Description=MotorHandPro API Server
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/var/www/MotorHandPro/api
Environment="PATH=/usr/local/bin"
ExecStart=/usr/local/bin/uvicorn server:app --host 0.0.0.0 --port 8000
Restart=always

[Install]
WantedBy=multi-user.target
```

Enable and start:

```bash
sudo systemctl enable motorhandpro-api
sudo systemctl start motorhandpro-api
sudo systemctl status motorhandpro-api
```

### Using Docker

Create `Dockerfile`:

```dockerfile
FROM python:3.11-slim

WORKDIR /app

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 8000

CMD ["uvicorn", "server:app", "--host", "0.0.0.0", "--port", "8000"]
```

Build and run:

```bash
docker build -t motorhandpro-api .
docker run -d -p 8000:8000 --env-file .env motorhandpro-api
```

### Nginx Reverse Proxy

Add to Nginx configuration:

```nginx
server {
    listen 80;
    server_name api.yourdomain.com;

    location / {
        proxy_pass http://localhost:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

## Monitoring

### View Logs

```bash
# systemd
sudo journalctl -u motorhandpro-api -f

# Docker
docker logs -f <container_id>
```

### Check Health

```bash
curl http://localhost:8000/health
```

## Security Notes

1. **Never commit `.env` files** - they contain sensitive credentials
2. **Use HTTPS in production** - configure SSL certificates
3. **Restrict CORS origins** - only allow trusted domains
4. **Use environment variables** - never hardcode credentials
5. **Monitor database access** - review MongoDB Atlas access logs
6. **Rotate credentials regularly** - update passwords periodically

## Troubleshooting

### Connection Refused

Check if server is running:

```bash
ps aux | grep uvicorn
```

### MongoDB Connection Failed

Verify connection string and network access:

```bash
cd ../config
python3 mongodb.py
```

Check MongoDB Atlas:
- IP whitelist includes your server IP
- Database user has correct permissions
- Connection string is correct

### CORS Errors

Update `CORS_ORIGINS` in `.env`:

```bash
CORS_ORIGINS=http://localhost:3000,https://yourdomain.com,https://www.yourdomain.com
```

## Support

For issues or questions:
- Review API documentation at `/docs`
- Check MongoDB connection with `config/mongodb.py`
- Review server logs for errors
- Verify environment variables in `.env`
