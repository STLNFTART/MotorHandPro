# Node-RED Integration for MotorHandPro

## Overview

This Node-RED integration provides a **visual orchestration layer** for the MotorHandPro LAM system, enabling event-driven workflows, protocol bridging, scheduled automation, and rapid integration with external systems without modifying core code.

## Value Propositions

### 1. **Event-Driven Workflow Orchestration**
```
TAK/Sensor Event → Node-RED webhook → LAM API call → Result routing:
  ├─→ Dashboard update
  ├─→ TAK room notification
  ├─→ Database logging
  └─→ Alert channels (email, Slack, etc.)
```

**Use Case**: Drone telemetry triggers satellite visibility check, results pushed to tactical dashboard.

### 2. **Scheduled Simulation Sweeps**
```
Node-RED cron node:
  Every hour → Loop over 5 satellite configs → POST /api/satellite/analyze
  → Aggregate summaries → Store in TimescaleDB
```

**Use Case**: Automated regression testing of orbital mechanics across parameter ranges.

### 3. **Protocol Bridging**
Node-RED excels at handling diverse protocols:
- **MQTT**: IoT sensors, drones, ground stations
- **Serial**: Arduino motor controller telemetry
- **TCP/UDP**: Low-latency telemetry streams
- **WebSocket**: Bi-directional real-time communication
- **HTTP**: REST API integration

**Use Case**: Arduino serial data → parse → normalize → POST to LAM API → WebSocket broadcast to control panel.

### 4. **Rapid Alerting & Automation**
```javascript
Flow:
  [LAM API Result] → [Decision Node]
    If phase_drift_ms > threshold:
      → [Email Alert]
      → [TAK Message]
      → [PagerDuty]
    Else:
      → [DB Storage Only]
```

**Use Case**: Automatic alert escalation when satellite handoff latency exceeds mission requirements.

### 5. **MCP ↔ Physical World Bridge**
```
MCP Tool Call → LAM API → Event Emission (Kafka/MQTT)
  → Node-RED Flow:
    ├─→ Update live dashboard
    ├─→ Trigger secondary sim
    ├─→ Log to GitHub Issues
    └─→ Store in TimescaleDB
```

**Use Case**: When MCP agent plans a trip, Node-RED flow archives the plan, updates a shared calendar, and notifies collaborators.

---

## Architecture

### System Diagram
```
┌─────────────────────────────────────────────────────────────────────┐
│                         External World                               │
│  MQTT Sensors | Drones | TAK Systems | Webhooks | Serial Devices   │
└────────────────────────────┬────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                        Node-RED (Port 1880)                          │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │
│  │ MQTT Broker  │  │ HTTP Webhooks│  │   Cron Jobs  │              │
│  │   Nodes      │  │    Nodes     │  │    Nodes     │              │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘              │
│         │                  │                  │                      │
│         └──────────────────┴──────────────────┘                      │
│                             │                                        │
│         ┌───────────────────▼───────────────────┐                   │
│         │  LAM API Client Node (Custom)         │                   │
│         │  - POST /trip/plan                    │                   │
│         │  - POST /ask                          │                   │
│         │  - GET /status                        │                   │
│         │  - GET /api/satellite/{id}            │                   │
│         └───────────────────┬───────────────────┘                   │
│                             │                                        │
│         ┌───────────────────▼───────────────────┐                   │
│         │  Decision & Routing Nodes             │                   │
│         │  - Switch (if/else logic)             │                   │
│         │  - Function (JavaScript)              │                   │
│         │  - Template (format messages)         │                   │
│         └───────────────────┬───────────────────┘                   │
│                             │                                        │
│         ┌───────────────────▼───────────────────┐                   │
│         │  Output Nodes                         │                   │
│         │  - Dashboard (UI)                     │                   │
│         │  - Email / Slack / PagerDuty          │                   │
│         │  - Database (PostgreSQL/TimescaleDB)  │                   │
│         │  - WebSocket (real-time push)         │                   │
│         │  - TAK Server (tactical feeds)        │                   │
│         └───────────────────────────────────────┘                   │
└─────────────────────────────────────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    MotorHandPro LAM System                           │
│  ┌────────────────┐  ┌────────────────┐  ┌────────────────┐        │
│  │  FastAPI       │  │  WebSocket     │  │  PostgreSQL    │        │
│  │  (Port 8000)   │  │  (8765, 8766)  │  │  Database      │        │
│  └────────────────┘  └────────────────┘  └────────────────┘        │
└─────────────────────────────────────────────────────────────────────┘
```

### Data Flow Examples

#### Example 1: Scheduled Satellite Analysis
```
[Inject: cron 0 * * * *]
  → [Function: Generate Config Array]
  → [Split: Iterate Configs]
  → [HTTP Request: POST /api/satellite/analyze]
  → [Join: Aggregate Results]
  → [Function: Calculate Summary Stats]
  → [PostgreSQL: Store Results]
  → [Dashboard: Update Chart]
```

#### Example 2: MQTT Sensor → Simulation Trigger
```
[MQTT In: sensor/telemetry]
  → [JSON Parse]
  → [Function: Validate & Transform]
  → [HTTP Request: POST /ask {"question": "analyze telemetry"}]
  → [Switch: Check response.stability]
    ├─→ [Stable] → [Debug Log]
    └─→ [Unstable] → [Email Alert]
                   → [TAK Message]
                   → [Database: Alert Log]
```

#### Example 3: Webhook → Multi-Step Workflow
```
[HTTP In: POST /webhook/trip-request]
  → [HTTP Request: POST /trip/plan]
  → [Function: Extract Booking Details]
  → [Parallel Execution]:
      ├─→ [HTTP: Calendar API - Add Event]
      ├─→ [Email: Send Confirmation]
      ├─→ [Slack: Notify Team]
      └─→ [PostgreSQL: Store Booking]
  → [HTTP Response: 200 OK]
```

---

## Installation

### Prerequisites
- Docker & Docker Compose installed
- MotorHandPro LAM API running on `http://localhost:8000`
- PostgreSQL database accessible

### Quick Start

1. **Deploy Node-RED Stack**:
```bash
cd /home/user/MotorHandPro/node-red
docker-compose up -d
```

This starts:
- Node-RED on `http://localhost:1880`
- Mosquitto MQTT broker on `localhost:1883`
- TimescaleDB for time-series data (optional)

2. **Access Node-RED Editor**:
```
http://localhost:1880
```

3. **Import Example Flows**:
- Go to Menu → Import → Clipboard
- Paste content from `node-red/flows/` directory
- Deploy

---

## Custom Nodes

### `node-red-contrib-motorhand-lam`

Custom node package providing LAM API integration.

#### Nodes Included:

1. **`lam-api-call`**
   - **Description**: Make calls to LAM API endpoints
   - **Config**:
     - API URL: `http://localhost:8000`
     - Endpoint: `/trip/plan`, `/ask`, `/status`, etc.
     - Method: GET, POST
     - Authentication: Optional API key
   - **Input**: `msg.payload` (request body)
   - **Output**: `msg.payload` (API response)

2. **`lam-status`**
   - **Description**: Get LAM system status with resonance field
   - **Output**:
     ```json
     {
       "status": "ok",
       "resonance_field": {
         "alpha": 0.54,
         "lambda": 0.115,
         "stable": true
       },
       "action_count": 42
     }
     ```

3. **`satellite-query`**
   - **Description**: Query satellite constellation
   - **Config**:
     - Query Type: status, visible, coverage
     - Satellite ID (optional)
     - Location (lat/lon for visible satellites)
   - **Output**: Satellite data array

4. **`websocket-bridge`**
   - **Description**: Two-way bridge to LAM WebSocket (port 8765)
   - **Input**: Control commands
   - **Output**: Real-time visualization updates

#### Installation:
```bash
cd node-red/custom-nodes/node-red-contrib-motorhand-lam
npm install
npm link
```

In Node-RED:
```bash
cd ~/.node-red
npm link node-red-contrib-motorhand-lam
```

---

## Example Flows

### Flow 1: Hourly Satellite Sweep
**File**: `flows/satellite-hourly-sweep.json`

**Description**: Every hour, test 5 satellite configurations, aggregate results, alert on anomalies.

**Nodes**:
- **Inject** (cron: `0 * * * *`)
- **Function** (generate configs):
  ```javascript
  msg.configs = [
    {altitude: 550, inclination: 53.0},
    {altitude: 540, inclination: 53.2},
    {altitude: 570, inclination: 70.0},
    {altitude: 560, inclination: 97.6},
    {altitude: 340, inclination: 42.0}
  ];
  return msg;
  ```
- **Split** (iterate configs)
- **HTTP Request** (`POST /api/satellite/analyze`)
- **Join** (aggregate)
- **Function** (check for phase_drift > 10ms)
- **Switch** (route alerts vs. logs)
- **Email** (alert on anomaly)
- **PostgreSQL** (store all results)

### Flow 2: MQTT Drone Telemetry → LAM Analysis
**File**: `flows/mqtt-drone-analysis.json`

**Description**: Receive drone telemetry via MQTT, ask LAM for stability analysis, push to dashboard.

**Nodes**:
- **MQTT In** (topic: `drone/telemetry`)
- **JSON** (parse payload)
- **Function** (format question):
  ```javascript
  msg.payload = {
    question: `Analyze drone telemetry: altitude=${msg.payload.altitude}m, velocity=${msg.payload.velocity}m/s. Is this stable flight?`
  };
  return msg;
  ```
- **HTTP Request** (`POST /ask`)
- **Function** (extract answer)
- **Dashboard Chart** (plot stability score)
- **Debug**

### Flow 3: TAK Integration - Satellite Visibility
**File**: `flows/tak-satellite-visibility.json`

**Description**: TAK system requests satellite visibility via webhook, Node-RED queries LAM, returns CoT XML.

**Nodes**:
- **HTTP In** (`POST /webhook/tak/sat-visibility`)
- **Function** (extract lat/lon from TAK CoT XML)
- **HTTP Request** (`GET /api/visible?lat={lat}&lon={lon}`)
- **Function** (format as TAK CoT):
  ```javascript
  // Generate CoT XML for each visible satellite
  let cotXml = satellites.map(sat => `
    <event>
      <point lat="${sat.subsatellite_lat}" lon="${sat.subsatellite_lon}"/>
      <detail>
        <satellite id="${sat.satellite_id}" altitude="${sat.altitude_km}"/>
      </detail>
    </event>
  `).join('\n');
  msg.payload = cotXml;
  return msg;
  ```
- **HTTP Response** (200 OK with CoT XML)
- **TAK Server** (optional: push to TAK multicast)

### Flow 4: Webhook → Trip Planning → Multi-Channel Notification
**File**: `flows/webhook-trip-notification.json`

**Description**: External system posts trip request, Node-RED calls LAM, then notifies via email, Slack, calendar.

**Nodes**:
- **HTTP In** (`POST /webhook/trip`)
- **HTTP Request** (`POST /trip/plan`)
- **Function** (parse booking details)
- **Link Out** (fanout to multiple flows)
  - **Email** (confirmation)
  - **Slack** (team notification)
  - **HTTP Request** (Google Calendar API)
  - **PostgreSQL** (store booking)
- **HTTP Response** (200 OK)

### Flow 5: MCP Event → GitHub Issue Creation
**File**: `flows/mcp-github-issue.json`

**Description**: When LAM completes an experiment (via MCP), log to GitHub Issues.

**Nodes**:
- **HTTP In** (`POST /webhook/mcp/experiment-complete`)
- **Function** (extract experiment details)
- **HTTP Request** (`POST https://api.github.com/repos/{owner}/{repo}/issues`):
  ```javascript
  msg.payload = {
    title: `Experiment Complete: ${msg.experiment.title}`,
    body: `## Results\n\n${JSON.stringify(msg.experiment.results, null, 2)}`,
    labels: ['automation', 'experiment']
  };
  msg.headers = {
    'Authorization': `token ${env.get('GITHUB_TOKEN')}`
  };
  return msg;
  ```
- **Debug**

---

## Dashboard

Node-RED Dashboard provides a real-time operations center for MotorHandPro.

**Access**: `http://localhost:1880/ui`

### Tabs

1. **System Status**
   - LAM resonance field state (gauge)
   - Action count (counter)
   - Success rate (chart)
   - Last 10 actions (table)

2. **Satellite Constellation**
   - Global coverage heatmap
   - Satellite count by shell (bar chart)
   - Active/maneuvering/deorbiting (pie chart)
   - Search satellite by ID (form)

3. **Experiment Monitor**
   - Active experiments (list)
   - Experiment progress (gauge)
   - Results timeline (chart)

4. **Alerts & Logs**
   - Recent alerts (table with severity)
   - Alert rate (spark line)
   - Acknowledge button

### Example Dashboard Flow
```json
[
  {"id": "status-gauge", "type": "ui_gauge", "group": "system", "min": 0, "max": 1, "label": "Stability"},
  {"id": "action-chart", "type": "ui_chart", "group": "system", "label": "Actions/min"},
  {"id": "sat-table", "type": "ui_table", "group": "satellites", "columns": ["id", "altitude", "status"]}
]
```

---

## Integration Patterns

### Pattern 1: API Polling
**When**: Need periodic status updates from LAM
```
[Inject: interval 10s]
  → [HTTP Request: GET /status]
  → [Dashboard: Update Gauge]
```

### Pattern 2: Event Emission from LAM
**When**: LAM needs to notify Node-RED of events
```
LAM API (modified):
  After action execution:
    → POST http://localhost:1880/webhook/lam-event
      {action_id, type, result, resonance_state}

Node-RED:
  [HTTP In: /webhook/lam-event]
    → [Process Event]
    → [Route to Dashboard/Alerts/DB]
```

### Pattern 3: Bi-Directional WebSocket
**When**: Real-time two-way communication needed
```
Node-RED:
  [WebSocket Out: ws://localhost:8765]
    ← [Commands from Dashboard]

  [WebSocket In: ws://localhost:8765]
    → [Visualization Updates]
    → [Dashboard: Update Charts]
```

### Pattern 4: MQTT Pub/Sub Hub
**When**: Multiple systems need decoupled messaging
```
Drone → MQTT Publish: drone/telemetry
LAM (via Node-RED) → MQTT Subscribe: drone/telemetry
                   → Process
                   → MQTT Publish: lam/analysis/result
TAK System → MQTT Subscribe: lam/analysis/result
```

---

## Configuration

### Environment Variables
Create `.env` file in `node-red/` directory:

```bash
# LAM API Configuration
LAM_API_URL=http://localhost:8000
LAM_API_KEY=optional_api_key_here

# MQTT Broker
MQTT_BROKER=mqtt://localhost:1883
MQTT_USERNAME=nodered
MQTT_PASSWORD=secure_password

# Database
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
POSTGRES_DB=motorhand
POSTGRES_USER=lam_user
POSTGRES_PASSWORD=secure_password

# Notifications
EMAIL_SMTP_HOST=smtp.gmail.com
EMAIL_SMTP_PORT=587
EMAIL_USERNAME=alerts@example.com
EMAIL_PASSWORD=app_specific_password

SLACK_WEBHOOK_URL=https://hooks.slack.com/services/YOUR/WEBHOOK/URL

# GitHub Integration
GITHUB_TOKEN=ghp_your_token_here
GITHUB_REPO=STLNFTART/MotorHandPro

# TAK Server (optional)
TAK_SERVER_URL=https://tak.example.com:8443
TAK_CERT_PATH=/certs/tak-client.p12
```

### Node-RED Settings
Edit `node-red/settings.js`:

```javascript
module.exports = {
    uiPort: process.env.PORT || 1880,

    // Enable projects for version control
    editorTheme: {
        projects: {
            enabled: true
        }
    },

    // Context storage (for persistence)
    contextStorage: {
        default: {
            module: "memory"
        },
        file: {
            module: "localfilesystem"
        }
    },

    // Function node external modules
    functionExternalModules: true,

    // Security
    adminAuth: {
        type: "credentials",
        users: [{
            username: "admin",
            password: "$2b$08$hashed_password",
            permissions: "*"
        }]
    }
};
```

---

## Security Considerations

1. **Authentication**:
   - Enable Node-RED admin auth (see settings above)
   - Use API keys for LAM API calls
   - Secure MQTT with username/password

2. **Network Isolation**:
   - Run Node-RED in Docker network with LAM
   - Only expose port 1880 to trusted networks
   - Use reverse proxy (Nginx) with TLS

3. **Secrets Management**:
   - Store credentials in environment variables
   - Use Node-RED credentials encryption
   - Never commit `.env` files to git

4. **Input Validation**:
   - Validate all webhook inputs
   - Sanitize data before SQL queries
   - Rate-limit external webhooks

---

## Performance Tips

1. **Batching**:
   - Use `join` node to batch API calls
   - Aggregate before database writes
   - Debounce high-frequency events

2. **Caching**:
   - Cache satellite positions (update every 10s)
   - Store LAM status in context (poll every 5s)
   - Use context.file for persistence

3. **Error Handling**:
   - Add `catch` nodes to all flows
   - Implement retry logic for API calls
   - Log errors to database

4. **Monitoring**:
   - Use `status` nodes to show flow health
   - Dashboard gauges for throughput
   - Alert on flow failures

---

## Where Node-RED Does NOT Belong

❌ **Not for**:
- Tight simulation loops (ms-level stepping)
- Physics calculations (ODEs, orbital mechanics)
- Core model state management
- High-frequency control (< 100ms)

✅ **Yes for**:
- Orchestrating API calls
- Event routing and transformation
- Scheduled automation
- Protocol bridging
- Dashboards and alerting
- Multi-system integration

---

## Development Workflow

### Creating a New Flow

1. **Design**: Sketch data flow on paper
2. **Implement**: Drag nodes in Node-RED editor
3. **Test**: Use `inject` and `debug` nodes
4. **Export**: Menu → Export → Download
5. **Version Control**: Commit to `node-red/flows/`
6. **Deploy**: Import in production Node-RED

### Custom Node Development

1. **Generate Template**:
```bash
cd node-red/custom-nodes
mkdir node-red-contrib-my-node
cd node-red-contrib-my-node
npm init
```

2. **Create Node** (`my-node.js`):
```javascript
module.exports = function(RED) {
    function MyNode(config) {
        RED.nodes.createNode(this, config);
        var node = this;

        node.on('input', function(msg) {
            // Process msg.payload
            msg.payload = processData(msg.payload);
            node.send(msg);
        });
    }

    RED.nodes.registerType("my-node", MyNode);
}
```

3. **Create HTML** (`my-node.html`):
```html
<script type="text/javascript">
    RED.nodes.registerType('my-node', {
        category: 'MotorHandPro',
        color: '#a6bbcf',
        defaults: {
            name: {value: ""}
        },
        inputs: 1,
        outputs: 1,
        icon: "file.png",
        label: function() {
            return this.name || "my-node";
        }
    });
</script>
```

4. **Install**:
```bash
npm link
cd ~/.node-red
npm link node-red-contrib-my-node
```

---

## Troubleshooting

### Node-RED Won't Start
```bash
docker-compose logs node-red
# Check for port conflicts, permission issues
```

### LAM API Unreachable
```bash
# From inside Node-RED container
docker exec -it nodered curl http://host.docker.internal:8000/health
```

### MQTT Connection Failed
```bash
# Test MQTT broker
mosquitto_pub -h localhost -p 1883 -t test -m "hello"
mosquitto_sub -h localhost -p 1883 -t test
```

### Flow Not Triggering
- Check node status (dot under node)
- Add `debug` nodes to trace data flow
- Verify msg.payload structure
- Check function node logs (sidebar)

---

## Next Steps

1. **Deploy**: Run `docker-compose up -d`
2. **Import Flows**: Load example flows from `flows/` directory
3. **Configure**: Set environment variables in `.env`
4. **Customize**: Modify flows for your use cases
5. **Monitor**: Access dashboard at `http://localhost:1880/ui`
6. **Extend**: Build custom nodes for specialized integrations

---

## Resources

- [Node-RED Documentation](https://nodered.org/docs/)
- [Node-RED Cookbook](https://cookbook.nodered.org/)
- [MotorHandPro API Docs](../README.md)
- [Custom Nodes Guide](./custom-nodes/README.md)
- [Example Flows](./flows/README.md)

---

## Architecture Decision: Node-RED as "Mission Control Canvas"

> MotorHandPro + Orchestrator + LAM: do the math.
> Node.js: expose a clean, stable API for that math.
> MCP: lets LLMs call that API as tools.
> **Node-RED: wires everything together visually (events → API calls → alerts/logs/other systems).**

This integration treats Node-RED as your **mission control canvas**, not the brain. The Primal Logic framework provides the intelligence; Node-RED provides the visual, flexible orchestration layer.
