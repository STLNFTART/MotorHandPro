# Getting Started with Node-RED Integration for MotorHandPro

## Quick Start (5 Minutes)

This guide will get you up and running with Node-RED + MotorHandPro in 5 minutes.

### Prerequisites

- Docker and Docker Compose installed
- MotorHandPro LAM API running (`http://localhost:8000`)

### Step 1: Deploy Node-RED Stack

```bash
cd /home/user/MotorHandPro/node-red
./setup.sh
docker-compose up -d
```

**What this does:**
- Starts Node-RED on `http://localhost:1880`
- Starts Mosquitto MQTT broker on `localhost:1883`
- Starts TimescaleDB for time-series data on `localhost:5433`
- Starts Grafana dashboard on `http://localhost:3000` (optional)

### Step 2: Access Node-RED Editor

Open your browser to:
```
http://localhost:1880
```

**Default credentials:**
- Username: `admin`
- Password: `admin`

### Step 3: Install Custom Nodes

```bash
# From your terminal
cd ~/.node-red
npm install /home/user/MotorHandPro/node-red/custom-nodes/node-red-contrib-motorhand-lam
```

**Restart Node-RED** to load the new nodes:
```bash
docker-compose restart nodered
```

### Step 4: Import Example Flow

1. In Node-RED editor, click menu (☰) → Import
2. Select file: `/home/user/MotorHandPro/node-red/flows/03-system-health-dashboard.json`
3. Click "Import"
4. Click "Deploy" (red button top-right)

### Step 5: Test the Integration

**Option A: Trigger Manually**
- Click the timestamp button on the "Manual Check" inject node
- Watch the debug sidebar for output

**Option B: Wait for Auto-Polling**
- The flow polls LAM status every 10 seconds automatically
- Watch the node status indicators change

### Step 6: View Dashboard (Optional)

Install Node-RED Dashboard:
```bash
cd ~/.node-red
npm install node-red-dashboard
docker-compose restart nodered
```

Access dashboard:
```
http://localhost:1880/ui
```

---

## What You've Built

You now have a complete event-driven orchestration layer that:

✅ Polls LAM system health every 10 seconds
✅ Calculates health scores based on resonance field stability
✅ Routes alerts based on severity (warning vs critical)
✅ Stores data in TimescaleDB for historical analysis
✅ Displays real-time dashboard with gauges and charts

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    External Systems                          │
│   MQTT Sensors | Drones | TAK | Webhooks | Serial Devices  │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                  Node-RED (Port 1880)                        │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐            │
│  │   Custom   │  │   MQTT     │  │  Scheduled │            │
│  │    LAM     │  │  Protocol  │  │   Tasks    │            │
│  │   Nodes    │  │  Bridges   │  │  (Cron)    │            │
│  └─────┬──────┘  └─────┬──────┘  └─────┬──────┘            │
│        │               │               │                    │
│        └───────────────┴───────────────┘                    │
│                        │                                    │
│        ┌───────────────▼───────────────┐                   │
│        │   Decision & Routing Logic    │                   │
│        └───────────────┬───────────────┘                   │
│                        │                                    │
│        ┌───────────────▼───────────────┐                   │
│        │  Outputs (Email, DB, etc.)    │                   │
│        └───────────────────────────────┘                   │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              MotorHandPro LAM System                         │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐            │
│  │  REST API  │  │ WebSocket  │  │ PostgreSQL │            │
│  │ (Port 8000)│  │(8765, 8766)│  │  Database  │            │
│  └────────────┘  └────────────┘  └────────────┘            │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Components

### 1. Custom Node-RED Nodes

Located in: `custom-nodes/node-red-contrib-motorhand-lam/`

**Nodes:**
- **lam-api-call**: General-purpose LAM API caller
- **lam-status**: Specialized status monitoring with auto-polling
- **satellite-query**: Satellite constellation queries
- **websocket-bridge**: Real-time WebSocket communication

**Documentation**: [Custom Nodes README](custom-nodes/node-red-contrib-motorhand-lam/README.md)

### 2. Example Flows

Located in: `flows/`

- **01-hourly-satellite-sweep.json**: Automated satellite config testing
- **02-mqtt-telemetry-analysis.json**: IoT/drone telemetry analysis
- **03-system-health-dashboard.json**: Real-time health monitoring

**Documentation**: [Flows README](flows/README.md)

### 3. Infrastructure (Docker Compose)

- **Node-RED**: Visual flow editor and runtime
- **Mosquitto**: MQTT broker for IoT messaging
- **TimescaleDB**: Time-series database
- **Grafana**: Advanced visualization (optional)
- **Nginx**: Reverse proxy with TLS (production profile)

**Configuration**: [docker-compose.yml](docker-compose.yml)

### 4. LAM API Webhooks

**NEW**: LAM API now supports webhook subscriptions!

**Endpoints:**
- `POST /webhooks/subscribe` - Subscribe to events
- `DELETE /webhooks/subscribe/{id}` - Unsubscribe
- `GET /webhooks/subscribe` - List subscriptions
- `POST /webhooks/test` - Test webhook delivery
- `GET /webhooks/events` - List available event types

**Event Types:**
- `action.started`, `action.completed`, `action.failed`
- `resonance.drift`, `resonance.unstable`, `resonance.stable`
- `trip.planned`, `reservation.made`, `question.answered`
- `satellite.updated`, `satellite.coverage_low`
- `experiment.started`, `experiment.completed`
- `health.warning`, `health.critical`, `health.recovered`

---

## Common Use Cases

### Use Case 1: Scheduled Satellite Analysis

**Goal**: Test 5 satellite configurations every hour

**Flow**: `01-hourly-satellite-sweep.json`

**How it works:**
1. Cron trigger fires every hour
2. Generates 5 satellite configs
3. For each config, asks LAM for stability analysis
4. Aggregates results
5. Checks for anomalies (slow response, parameter drift)
6. Sends email alerts if anomalies detected
7. Stores all results in TimescaleDB

**Customize:**
- Change schedule in inject node
- Add/remove satellite configs
- Adjust anomaly thresholds
- Add Slack/PagerDuty alerts

### Use Case 2: IoT Telemetry Processing

**Goal**: Process drone telemetry, analyze stability, alert on warnings

**Flow**: `02-mqtt-telemetry-analysis.json`

**How it works:**
1. Subscribe to `drone/telemetry` MQTT topic
2. Validate and parse JSON payload
3. Send to LAM for stability analysis
4. Extract warnings from LAM response
5. If warnings found:
   - Publish to `alerts/drone` topic
   - Store in alert database
   - Send notifications
6. Update dashboard with real-time charts

**Customize:**
- Add more MQTT topics (sensors, robots, etc.)
- Change warning detection logic
- Add custom analysis questions
- Route to different alert channels

### Use Case 3: System Health Dashboard

**Goal**: Real-time LAM system monitoring with auto-alerts

**Flow**: `03-system-health-dashboard.json`

**How it works:**
1. Poll LAM status every 10 seconds
2. Calculate health score (0-100) based on:
   - Resonance field stability
   - Parameter bounds (alpha, lambda)
   - Lipschitz constant
   - Action success rate
3. Categorize: Healthy / Warning / Critical
4. If critical:
   - Send email alert (debounced to 5 min)
   - Post to Slack
   - Log to database
5. Update dashboard gauges and charts

**Customize:**
- Change polling interval
- Adjust health score algorithm
- Modify alert thresholds
- Add more gauges/charts

---

## Integration Patterns

### Pattern 1: Node-RED → LAM (Pull/Polling)

**When**: You control the timing (scheduled tasks, manual triggers)

```
[Inject/MQTT/Timer] → [LAM API Call] → [Process Response] → [Output]
```

**Example**: Hourly satellite sweep, on-demand queries

### Pattern 2: LAM → Node-RED (Push/Webhooks)

**When**: LAM events trigger Node-RED flows

```
LAM API → Webhook → [Node-RED HTTP In] → [Process Event] → [Action]
```

**Example**: Real-time health alerts, experiment completion notifications

**Setup**:
```bash
# Subscribe Node-RED to LAM events
curl -X POST http://localhost:8000/webhooks/subscribe \
  -H "Content-Type: application/json" \
  -d '{
    "url": "http://nodered:1880/webhook/lam-events",
    "events": ["health.critical", "experiment.completed"]
  }'
```

### Pattern 3: Bi-Directional (WebSocket)

**When**: Real-time two-way communication needed

```
[WebSocket Bridge] ↔ LAM WebSocket Server (port 8765)
```

**Example**: Live control panel, real-time telemetry streaming

### Pattern 4: Event Hub (MQTT)

**When**: Multiple systems need decoupled messaging

```
System A → MQTT Broker ← Node-RED → LAM API
                  ↑
              System B
```

**Example**: Drone telemetry → MQTT → Node-RED → LAM → MQTT → TAK

---

## Configuration Guide

### Environment Variables

Edit `.env` file in `node-red/` directory:

```bash
# LAM API
LAM_API_URL=http://host.docker.internal:8000

# MQTT
MQTT_BROKER=mqtt://mosquitto:1883
MQTT_USERNAME=nodered
MQTT_PASSWORD=your_secure_password

# Database
TIMESCALE_PASSWORD=your_timescale_password

# Notifications
EMAIL_SMTP_HOST=smtp.gmail.com
EMAIL_SMTP_PORT=587
EMAIL_USERNAME=alerts@example.com
EMAIL_PASSWORD=your_app_password

SLACK_WEBHOOK_URL=https://hooks.slack.com/services/YOUR/WEBHOOK/URL

# GitHub
GITHUB_TOKEN=ghp_your_token_here
GITHUB_REPO=STLNFTART/MotorHandPro
```

### Node-RED Security

Generate password hash:
```bash
node -e "console.log(require('bcryptjs').hashSync('your-password', 8))"
```

Add to `.env`:
```bash
NODE_RED_ADMIN_PASSWORD_HASH=$2b$08$your_hash_here
```

### MQTT Authentication

Create password file:
```bash
docker run --rm -v $(pwd)/mosquitto:/mosquitto/config eclipse-mosquitto:latest \
  mosquitto_passwd -c -b /mosquitto/config/passwd nodered your_password
```

---

## Troubleshooting

### Node-RED Container Won't Start

**Check logs:**
```bash
docker-compose logs nodered
```

**Common issues:**
- Port 1880 already in use
- Permission errors on volumes
- Invalid settings.js syntax

**Solution:**
```bash
docker-compose down
docker-compose up -d
```

### LAM API Unreachable

**Test from Node-RED container:**
```bash
docker exec -it motorhand-nodered curl http://host.docker.internal:8000/health
```

**Expected response:**
```json
{"status":"healthy","lam_available":true,"timestamp":"..."}
```

**If fails:**
- Ensure LAM API is running: `curl http://localhost:8000/health`
- For Linux, you may need to use container name instead: `http://lam-api:8000`

### MQTT Connection Failed

**Test MQTT broker:**
```bash
# Subscribe
mosquitto_sub -h localhost -p 1883 -t test/topic -u nodered -P your_password

# Publish (in another terminal)
mosquitto_pub -h localhost -p 1883 -t test/topic -m "hello" -u nodered -P your_password
```

**If fails:**
- Check broker logs: `docker-compose logs mosquitto`
- Verify credentials in `mosquitto/passwd`
- Ensure port 1883 is exposed

### Custom Nodes Not Appearing

**Reinstall:**
```bash
cd ~/.node-red
npm uninstall node-red-contrib-motorhand-lam
npm install /home/user/MotorHandPro/node-red/custom-nodes/node-red-contrib-motorhand-lam
docker-compose restart nodered
```

**Check Node-RED logs:**
```bash
docker-compose logs nodered | grep motorhand
```

### Dashboard Not Loading

**Install dashboard package:**
```bash
cd ~/.node-red
npm install node-red-dashboard
docker-compose restart nodered
```

**Access:**
```
http://localhost:1880/ui
```

**If still fails:**
- Check Node-RED logs for errors
- Ensure dashboard nodes are configured with ui_group and ui_tab

---

## Performance Tuning

### Polling Intervals

**Conservative (low load):**
- Health monitoring: 30s
- Satellite queries: Every 6 hours
- MQTT: Event-driven (no limit)

**Aggressive (real-time):**
- Health monitoring: 5s
- Satellite queries: Every 30 minutes
- MQTT: Event-driven with rate limiting

### Database Write Batching

For high-frequency telemetry (>100 msg/sec), batch writes:

```javascript
// In function node before database
const buffer = flow.get('write_buffer') || [];
buffer.push(msg.payload);

if (buffer.length >= 100 || Date.now() - flow.get('last_write', 0) > 10000) {
    msg.payload = buffer;
    flow.set('write_buffer', []);
    flow.set('last_write', Date.now());
    return msg;
}

flow.set('write_buffer', buffer);
return null;
```

### Memory Management

**TimescaleDB retention policies:**
```sql
-- Keep only 30 days of raw telemetry
SELECT add_retention_policy('motorhand.satellite_telemetry', INTERVAL '30 days');

-- Continuous aggregates keep hourly summaries forever
```

---

## Production Deployment

### 1. Enable TLS

Generate certificates:
```bash
cd node-red/nginx/certs
openssl req -x509 -nodes -days 365 -newkey rsa:2048 \
  -keyout nodered.key -out nodered.crt
```

Start with production profile:
```bash
docker-compose --profile production up -d
```

Access via HTTPS:
```
https://your-domain.com
```

### 2. Secure Credentials

**Use Docker secrets:**
```yaml
# docker-compose.yml
secrets:
  nodered_password:
    file: ./secrets/nodered_password.txt
```

**Reference in .env:**
```bash
NODE_RED_ADMIN_PASSWORD_HASH_FILE=/run/secrets/nodered_password
```

### 3. Monitoring

**Add health check endpoints:**
- Node-RED: `http://localhost:1880/health` (add custom endpoint)
- MQTT: Port 1883 connectivity
- TimescaleDB: Port 5433 connectivity

**Prometheus metrics:**
```bash
cd ~/.node-red
npm install node-red-contrib-prometheus-exporter
```

### 4. Backup & Recovery

**Backup Node-RED flows:**
```bash
docker cp motorhand-nodered:/data/flows.json ./backup/flows-$(date +%Y%m%d).json
```

**Backup TimescaleDB:**
```bash
docker exec motorhand-timescaledb pg_dump -U nodered motorhand_telemetry > backup.sql
```

**Restore:**
```bash
docker exec -i motorhand-timescaledb psql -U nodered motorhand_telemetry < backup.sql
```

---

## Next Steps

1. **Import all example flows** to see patterns in action
2. **Customize flows** for your specific use cases
3. **Add dashboard nodes** for visualization
4. **Configure webhook subscriptions** for event-driven workflows
5. **Set up alerts** (email, Slack, PagerDuty)
6. **Enable database storage** for historical analysis
7. **Create your own custom nodes** for specialized integrations
8. **Deploy to production** with TLS and authentication

---

## Resources

- [Full Documentation](README.md)
- [Custom Nodes Reference](custom-nodes/node-red-contrib-motorhand-lam/README.md)
- [Example Flows Guide](flows/README.md)
- [LAM API Documentation](../README.md)
- [Node-RED Official Docs](https://nodered.org/docs/)
- [Docker Compose Reference](https://docs.docker.com/compose/)

---

## Support

**Questions?** Open an issue on GitHub:
https://github.com/STLNFTART/MotorHandPro/issues

**Contributing?** Submit a pull request with your custom flows or nodes!

---

## License

MIT License - See LICENSE file for details

---

**Built with ❤️ for the MotorHandPro project**

*Mission Control Canvas for Primal Logic Intelligence*
