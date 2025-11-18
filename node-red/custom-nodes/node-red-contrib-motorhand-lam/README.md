# node-red-contrib-motorhand-lam

Custom Node-RED nodes for MotorHandPro LAM API integration.

## Overview

This package provides specialized Node-RED nodes for interacting with the MotorHandPro Large Action Model (LAM) system, including quantum resonance field monitoring, satellite constellation queries, and real-time WebSocket communication.

## Installation

### Option 1: npm link (Development)

```bash
cd /home/user/MotorHandPro/node-red/custom-nodes/node-red-contrib-motorhand-lam
npm install
npm link

cd ~/.node-red
npm link node-red-contrib-motorhand-lam
```

### Option 2: Direct Installation

```bash
cd ~/.node-red
npm install /home/user/MotorHandPro/node-red/custom-nodes/node-red-contrib-motorhand-lam
```

### Option 3: From npm (when published)

```bash
cd ~/.node-red
npm install node-red-contrib-motorhand-lam
```

After installation, restart Node-RED to load the new nodes.

## Nodes Included

### 1. LAM API Call (`lam-api-call`)

General-purpose node for making HTTP requests to any LAM API endpoint.

**Features:**
- Supports GET, POST, PUT, DELETE methods
- Configurable timeout
- Automatic JSON parsing
- Two outputs (success/error)
- Response time tracking

**Common Endpoints:**
- `/health` - Health check
- `/status` - System status with resonance field
- `/trip/plan` - Trip planning
- `/ask` - Ask questions
- `/task` - Execute tasks
- `/metrics` - Performance metrics

**Example:**
```javascript
// Input
msg.payload = {
  question: "Analyze satellite coverage over New York"
};
msg.endpoint = "/ask";

// Output (success)
msg.payload = {
  answer: "...",
  resonance_state: {...}
};
msg.responseTime = 123; // ms
```

### 2. LAM Status (`lam-status`)

Specialized node for retrieving LAM system status and quantum resonance field state.

**Features:**
- Automatic polling (configurable interval)
- Manual trigger support
- Optional metrics inclusion
- Visual status indicator shows stability

**Output:**
```javascript
{
  status: "ok",
  resonance_field: {
    alpha: 0.54,
    lambda: 0.115,
    lightfoot_constant: 0.16905,
    donte_attractor: 149.9992314,
    lipschitz_constant: 0.000129931830,
    stable: true,
    epoch: 42
  },
  action_count: 42,
  timestamp: "2025-11-18T10:30:00Z"
}
```

**Example Flow:**
```
[Inject: every 10s]
  → [LAM Status]
  → [Function: Check stability]
  → [Dashboard Gauge]
```

### 3. Satellite Query (`satellite-query`)

Query the satellite constellation system with multiple query types.

**Query Types:**
1. **System Status**: Overall constellation statistics
2. **Specific Satellite**: Detailed state for one satellite
3. **Batch Query**: Multiple satellites at once
4. **Visible from Location**: Satellites visible from lat/lon
5. **Coverage Analysis**: Global or regional coverage
6. **Constellation Metadata**: Configuration and shells

**Example - Visible Satellites:**
```javascript
// Input
msg.latitude = 40.7128;  // NYC
msg.longitude = -74.0060;

// Output
msg.payload = [
  {
    satellite_id: 12345,
    altitude_km: 550.3,
    elevation: 45.2,
    azimuth: 123.4
  },
  // ... more satellites
];
msg.satelliteCount = 15;
```

### 4. WebSocket Bridge (`websocket-bridge`)

Two-way bridge to LAM WebSocket server for real-time communication.

**Features:**
- Persistent connection with auto-reconnect
- Send control commands
- Receive visualization updates
- Status indicator
- JSON auto-parsing

**Send Control Command:**
```javascript
msg.payload = {
  type: "control_command",
  command: "start_capture",
  timestamp: new Date().toISOString()
};
```

**Receive Updates:**
```javascript
// Incoming message
{
  payload: {
    type: "visualization_update",
    data: {
      source: "PX4-Autopilot",
      primal_logic_analysis: {
        control_energy: 0.1,
        stability_metric: 0.8
      }
    }
  },
  topic: "visualization_update"
}
```

## Configuration

All nodes support configuration via:
1. Node properties (in the editor)
2. Environment variables
3. Message properties (dynamic override)

### Environment Variables

```bash
# LAM API URL
LAM_API_URL=http://localhost:8000

# WebSocket URL
LAM_WS_URL=ws://localhost:8765
```

## Examples

### Example 1: Monitor System Health

```
[Inject: every 30s]
  → [LAM Status]
  → [Function: Extract stability]
  → [Switch: stable?]
      ├─→ [Dashboard: Green]
      └─→ [Email: Alert unstable]
```

### Example 2: Scheduled Satellite Sweep

```
[Inject: cron 0 * * * *]
  → [Function: Generate sat IDs]
  → [Split]
  → [Satellite Query: satellite]
  → [Join]
  → [Function: Aggregate stats]
  → [PostgreSQL: Store]
```

### Example 3: MQTT → LAM → Dashboard

```
[MQTT In: sensor/telemetry]
  → [LAM API Call: /ask]
  → [Function: Parse analysis]
  → [WebSocket Bridge: Broadcast]
  → [Dashboard: Update chart]
```

### Example 4: Two-Way WebSocket Communication

```
[Dashboard Button]
  → [Function: Format command]
  → [WebSocket Bridge] ←→ [LAM Server]
  → [Switch: by type]
      ├─→ success → [Debug]
      └─→ error → [Email]
```

## Error Handling

All nodes implement proper error handling:

- **LAM API Call**: Two outputs (success/error)
- **LAM Status**: Logs warnings, continues on error
- **Satellite Query**: Throws error with details
- **WebSocket Bridge**: Auto-reconnect on disconnect

**Example Error Handler:**
```
[LAM API Call]
  ├─→ Output 1 (success) → [Process data]
  └─→ Output 2 (error) → [Function: Log error]
                        → [Email: Alert]
```

## Development

### Running Tests

```bash
npm test
```

### Adding New Nodes

1. Create `nodes/my-node.js` (Node.js logic)
2. Create `nodes/my-node.html` (Editor UI)
3. Update `package.json` node-red.nodes section
4. Rebuild: `npm install && npm link`

### Debugging

Enable verbose logging in Node-RED settings:

```javascript
// settings.js
logging: {
    console: {
        level: "debug"
    }
}
```

## API Reference

### LAM API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check |
| `/status` | GET | System status + resonance field |
| `/trip/plan` | POST | Plan trips |
| `/reservation/make` | POST | Make reservations |
| `/food/order` | POST | Order food |
| `/ask` | POST | Ask questions |
| `/task` | POST | Execute tasks |
| `/metrics` | GET | Performance metrics |

### Satellite API Endpoints

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/status` | GET | Constellation status |
| `/api/satellite/{id}` | GET | Get satellite by ID |
| `/api/satellites/batch` | POST | Batch query |
| `/api/visible` | GET | Visible satellites |
| `/api/coverage` | GET | Coverage analysis |
| `/api/constellation` | GET | Constellation metadata |

### WebSocket Messages

**Outgoing (Control):**
```json
{
  "type": "control_command",
  "command": "start_capture|stop_capture|reset"
}

{
  "type": "parameter_update",
  "data": {
    "lambda": 0.16905,
    "ke": 0.3
  }
}
```

**Incoming (Updates):**
```json
{
  "type": "visualization_update",
  "data": {
    "source": "PX4-Autopilot",
    "primal_logic_analysis": {...}
  }
}
```

## Troubleshooting

### Node not appearing in palette

```bash
# Reinstall
cd ~/.node-red
npm uninstall node-red-contrib-motorhand-lam
npm install /path/to/node-red-contrib-motorhand-lam
# Restart Node-RED
```

### Connection errors

```bash
# Check LAM API is running
curl http://localhost:8000/health

# Check WebSocket server
wscat -c ws://localhost:8765
```

### API timeout errors

Increase timeout in node configuration:
- LAM API Call: Set "Timeout" to higher value (e.g., 60000 ms)

## License

MIT

## Contributing

Contributions welcome! Please submit issues and pull requests to:
https://github.com/STLNFTART/MotorHandPro

## Support

For questions and support:
- GitHub Issues: https://github.com/STLNFTART/MotorHandPro/issues
- Documentation: /home/user/MotorHandPro/node-red/README.md

## Version History

### 1.0.0 (2025-11-18)
- Initial release
- LAM API Call node
- LAM Status node
- Satellite Query node
- WebSocket Bridge node
