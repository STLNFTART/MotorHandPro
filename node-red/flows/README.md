# Node-RED Example Flows for MotorHandPro

This directory contains ready-to-import Node-RED flows demonstrating common integration patterns with the MotorHandPro LAM system.

## Available Flows

### 1. Hourly Satellite Sweep (`01-hourly-satellite-sweep.json`)

**Purpose**: Automated testing of multiple satellite configurations on a schedule.

**Features**:
- Runs every hour (configurable)
- Tests 5 different satellite configurations
- Queries LAM for orbital analysis
- Aggregates results
- Detects anomalies (slow responses, parameter drift)
- Sends alerts on anomalies
- Logs all results

**Use Case**: Continuous validation of satellite constellation configurations, regression testing.

**Configuration Required**:
- None (works out of the box with debug output)
- Optional: Add email node for alerts
- Optional: Add PostgreSQL/TimescaleDB node for persistence

**Import & Test**:
```bash
1. Import flow: Menu → Import → Select file
2. Deploy
3. Click inject node "Every Hour" to trigger manually
4. Watch debug sidebar for results
```

---

### 2. MQTT Telemetry Analysis (`02-mqtt-telemetry-analysis.json`)

**Purpose**: Receive IoT/drone telemetry via MQTT, analyze with LAM, route based on results.

**Features**:
- Subscribes to MQTT topic `drone/telemetry`
- Validates and parses incoming JSON
- Sends telemetry to LAM for stability analysis
- Extracts safety warnings from LAM response
- Publishes alerts to `alerts/drone` topic
- Updates dashboard with real-time charts
- Stores telemetry and alerts to database

**Use Case**: Real-time monitoring of field sensors, drones, or robotic systems with intelligent analysis.

**Configuration Required**:
1. **MQTT Broker**: Edit `mqtt_broker_config` node
   - Set broker hostname (default: `mosquitto`)
   - Set credentials if authentication enabled

2. **Dashboard** (Optional):
   ```bash
   cd ~/.node-red
   npm install node-red-dashboard
   ```
   - Replace dashboard comment nodes with:
     - `ui_chart` for altitude/velocity trends
     - `ui_gauge` for stability score

3. **Database** (Optional):
   - Add PostgreSQL nodes for time-series storage

**Test Data**:
```bash
# Publish test telemetry to MQTT
mosquitto_pub -h localhost -t drone/telemetry -m '{
  "altitude": 150.5,
  "velocity": 12.3,
  "position": {"lat": 40.7128, "lon": -74.0060}
}'
```

---

### 3. System Health Dashboard (`03-system-health-dashboard.json`)

**Purpose**: Real-time LAM system health monitoring with intelligent alerting.

**Features**:
- Polls LAM status every 10 seconds
- Calculates health score (0-100) based on:
  - Resonance field stability
  - Parameter bounds (alpha, lambda)
  - Lipschitz constant
  - Action success rate
- Categorizes status: Healthy / Warning / Critical
- Sends critical alerts (with 5-min debounce)
- Displays gauges (health score, success rate, resonance alpha)
- Trends chart (last 100 readings)
- Manual trigger button

**Use Case**: Operations center monitoring, automated health checks, proactive alerting.

**Configuration Required**:
1. **Dashboard Nodes**:
   ```bash
   cd ~/.node-red
   npm install node-red-dashboard
   ```

2. **Replace Comment Nodes**:
   - `gauge_health` → `ui_gauge` (0-100, red<50, yellow<80, green≥80)
   - `gauge_success` → `ui_gauge` (0-100%, red<90, yellow<95, green≥95)
   - `gauge_alpha` → `ui_gauge` (normalized, target=50)
   - `chart_trends` → `ui_chart` (line chart, last 100 points)

3. **Alert Channels** (Optional):
   - `send_email` → Email node (SMTP configuration)
   - `send_slack` → HTTP Request node (Slack webhook URL)

**Access Dashboard**:
```
http://localhost:1880/ui
```

---

## Import Instructions

### Method 1: Import from File

1. Open Node-RED editor: `http://localhost:1880`
2. Click menu (☰) → Import
3. Select the `.json` file
4. Choose "new flow" or "current flow"
5. Click "Import"
6. Click "Deploy"

### Method 2: Copy-Paste JSON

1. Open the `.json` file in a text editor
2. Copy entire contents
3. Node-RED menu → Import → Clipboard
4. Paste JSON
5. Import → Deploy

### Method 3: Auto-load on Startup

Copy flows to Node-RED user directory:

```bash
cp *.json ~/.node-red/flows/
# Node-RED will auto-load on next restart
```

---

## Prerequisites

### Required

1. **Node-RED** running (via Docker or standalone)
   ```bash
   cd /home/user/MotorHandPro/node-red
   docker-compose up -d nodered
   ```

2. **LAM API** accessible at `http://localhost:8000`
   ```bash
   cd /home/user/MotorHandPro/lam
   python lam_main.py serve
   ```

3. **Custom MotorHandPro Nodes** installed
   ```bash
   cd ~/.node-red
   npm install /home/user/MotorHandPro/node-red/custom-nodes/node-red-contrib-motorhand-lam
   ```

### Optional (for full functionality)

1. **MQTT Broker** (for flow #2)
   ```bash
   docker-compose up -d mosquitto
   ```

2. **Node-RED Dashboard** (for gauges/charts)
   ```bash
   cd ~/.node-red
   npm install node-red-dashboard
   ```

3. **TimescaleDB** (for time-series storage)
   ```bash
   docker-compose up -d timescaledb
   ```

4. **Email Configuration** (for alerts)
   - Add `node-red-node-email` package
   - Configure SMTP settings

---

## Customization Guide

### Changing Schedule

**Hourly Satellite Sweep**:
```javascript
// In inject node "Every Hour"
repeat: "3600"  // Change to seconds (3600 = 1 hour)

// Or use cron
crontab: "0 * * * *"  // Every hour at minute 0
crontab: "*/30 * * * *"  // Every 30 minutes
```

### Adding More Satellite Configs

**In function node "Generate Configs"**:
```javascript
msg.configs = [
    { altitude: 550, inclination: 53.0, shell: 1 },
    // Add more configs here
    { altitude: 600, inclination: 45.0, shell: 6 }
];
```

### Changing Alert Thresholds

**System Health Dashboard**:
```javascript
// In function "Process Status"

// Adjust health score penalties
if (!rf.stable) {
    health_score -= 30;  // Change penalty for instability
}

// Adjust status thresholds
if (health_score < 50) {
    status_level = 'critical';  // Change threshold
} else if (health_score < 80) {
    status_level = 'warning';
}
```

### Adding Custom MQTT Topics

**MQTT Telemetry Analysis**:
```javascript
// Duplicate mqtt_in node, change topic to:
topic: "sensor/temperature"
topic: "robot/arm/position"
topic: "satellite/+/telemetry"  // wildcard
```

---

## Debugging Tips

### 1. Enable Debug Nodes

All flows include debug nodes. Enable them:
- Click debug node
- Toggle "enabled" checkbox
- View output in Debug sidebar (bug icon)

### 2. Check Node Status

Nodes show status indicators:
- 🟢 **Green dot**: Success
- 🟡 **Yellow ring**: Connecting/processing
- 🔴 **Red ring**: Error

### 3. View Error Logs

```bash
# Docker deployment
docker logs motorhand-nodered -f

# Standalone
tail -f ~/.node-red/logs/*.log
```

### 4. Test API Connection

```bash
# From Node-RED container
docker exec -it motorhand-nodered curl http://host.docker.internal:8000/health

# Expected response
{"status":"ok"}
```

### 5. Test MQTT Connection

```bash
# Subscribe to test topic
mosquitto_sub -h localhost -t test/topic -v

# Publish test message
mosquitto_pub -h localhost -t test/topic -m "hello"
```

---

## Performance Considerations

### Polling Intervals

- **Status monitoring**: 10s (adjust if CPU/network limited)
- **Satellite sweeps**: 1 hour (increase for smaller datasets)
- **MQTT processing**: Event-driven (no limit needed)

### Database Write Batching

For high-frequency telemetry, batch writes:

```javascript
// Add function node before database
const buffer = flow.get('write_buffer') || [];
buffer.push(msg.payload);

if (buffer.length >= 100) {
    msg.payload = buffer;
    flow.set('write_buffer', []);
    return msg;
}

flow.set('write_buffer', buffer);
return null;  // Don't write yet
```

### Rate Limiting API Calls

Add delay between API calls:

```javascript
// Add delay node after split
delay: 1000  // 1 second between calls
rate: 10     // Max 10 calls per second
```

---

## Integration with External Systems

### TAK Server

Send satellite visibility to TAK:

```javascript
// After satellite-query node
const cotXml = satellites.map(sat => `
  <event>
    <point lat="${sat.lat}" lon="${sat.lon}"/>
    <detail>
      <satellite id="${sat.id}" altitude="${sat.altitude_km}"/>
    </detail>
  </event>
`).join('\n');

msg.payload = cotXml;
// Send to TAK multicast or HTTP endpoint
```

### GitHub Issues

Log experiments to GitHub:

```javascript
// HTTP Request node
method: POST
url: https://api.github.com/repos/STLNFTART/MotorHandPro/issues
headers: {
  "Authorization": "token ${GITHUB_TOKEN}"
}
payload: {
  title: "Experiment Complete: " + msg.experiment.title,
  body: JSON.stringify(msg.results, null, 2)
}
```

### Slack Notifications

```javascript
// HTTP Request node
method: POST
url: process.env.SLACK_WEBHOOK_URL
payload: {
  text: "🚨 LAM Health Critical",
  attachments: [{
    color: "danger",
    fields: [
      { title: "Health Score", value: msg.health_score + "%" },
      { title: "Status", value: msg.status_level }
    ]
  }]
}
```

---

## Troubleshooting

### Flow Not Executing

1. **Check Deploy**: Ensure you clicked "Deploy" after import
2. **Check Disabled**: Flow tab shouldn't be grayed out
3. **Trigger Manually**: Click inject node timestamp button
4. **Check Errors**: Look for red triangles on nodes

### LAM API Unreachable

```bash
# Inside Node-RED container
docker exec -it motorhand-nodered ping host.docker.internal

# If fails, use container name instead
apiUrl: "http://lam-api:8000"
```

### MQTT Not Receiving Messages

1. **Check broker config**: Ensure hostname/port correct
2. **Test connection**:
   ```bash
   mosquitto_pub -h localhost -t test -m hello
   mosquitto_sub -h localhost -t test
   ```
3. **Check credentials**: If auth enabled, add username/password

### Dashboard Not Loading

1. **Install dashboard**:
   ```bash
   npm install node-red-dashboard
   ```
2. **Restart Node-RED**
3. **Access**: `http://localhost:1880/ui`
4. **Configure**: Add ui_group and ui_tab for nodes

---

## Next Steps

1. **Import all flows** to see examples
2. **Customize** for your use cases
3. **Add dashboard nodes** for visualization
4. **Configure alerts** (email, Slack, PagerDuty)
5. **Enable database storage** for historical analysis
6. **Create your own flows** combining patterns

---

## Resources

- [Node-RED Documentation](https://nodered.org/docs/)
- [Node-RED Dashboard Guide](https://flows.nodered.org/node/node-red-dashboard)
- [MotorHandPro API Docs](../README.md)
- [Custom Nodes Reference](../custom-nodes/node-red-contrib-motorhand-lam/README.md)

---

## Contributing

Have a useful flow? Submit a pull request!

```bash
cd /home/user/MotorHandPro/node-red/flows
# Add your flow
git add 04-your-flow-name.json
git commit -m "Add: Your flow description"
git push origin claude/node-red-integration-01AmUXEAxYnDvktLT2HBLhgY
```

Include:
- Descriptive flow name
- Clear comments in function nodes
- Default to debug output (avoid hard dependencies)
- Document configuration requirements
