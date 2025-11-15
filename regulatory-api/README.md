# Regulatory API Integration Layer

A Node.js/TypeScript service that provides unified access to U.S. federal regulatory APIs for research, simulation validation, and context enrichment.

**Supported APIs:**
- **FDA** (openFDA): Drug/device recalls, adverse events
- **NHTSA**: Vehicle recalls, VIN decoding, safety ratings, complaints
- **FAA**: Aviation weather (METAR, TAF, SIGMET, AIRMET, PIREPs)

---

## 🚨 **READ FIRST** 🚨

**LEGAL COMPLIANCE IS MANDATORY**

See [`LEGAL_COMPLIANCE.md`](./LEGAL_COMPLIANCE.md) for complete legal, ToS, and liability requirements before deploying this service.

**Key points:**
- ✅ Use for research, analysis, context enrichment
- ❌ Do NOT use for safety-critical decisions without verification
- ❌ Do NOT automate regulatory submissions
- ⚠️  Implement proper disclaimers (see compliance doc)

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Client Applications                       │
│  (Simulations, Dashboards, LLM Agents, Node-RED)            │
└─────────────────┬───────────────────────────┬───────────────┘
                  │                           │
          ┌───────▼────────┐         ┌────────▼──────────┐
          │  REST API      │         │   MCP Server      │
          │  (Express)     │         │   (stdio)         │
          │  Port 3000     │         │   LLM Tools       │
          └───────┬────────┘         └────────┬──────────┘
                  │                           │
          ┌───────▼───────────────────────────▼──────────┐
          │        Regulatory Client Layer               │
          │  ┌──────────┐ ┌──────────┐ ┌──────────┐    │
          │  │ FDA      │ │ NHTSA    │ │ FAA      │    │
          │  │ Client   │ │ Client   │ │ Client   │    │
          │  └────┬─────┘ └────┬─────┘ └────┬─────┘    │
          └───────┼────────────┼────────────┼───────────┘
                  │            │            │
          ┌───────▼────────────▼────────────▼───────────┐
          │      External APIs (Public Internet)        │
          │  • openFDA     • NHTSA vPIC/Safety          │
          │  • AviationWeather.gov                      │
          └─────────────────────────────────────────────┘
```

### Components

1. **Regulatory Client Layer** (`src/reg/`):
   - `fda.ts` - openFDA API wrapper
   - `nhtsa.ts` - NHTSA vPIC & Safety API wrapper
   - `faa.ts` - FAA Weather API wrapper
   - Handles: Rate limiting, retries, error handling, response normalization

2. **REST API Server** (`src/server.ts`):
   - Express server exposing regulatory functions as HTTP endpoints
   - CORS-enabled for web clients
   - Rate limiting (100 req/15min per IP)
   - Runs on port 3000 (configurable)

3. **MCP Server** (`src/mcp-server.ts`):
   - Model Context Protocol server for LLM agents (Claude, etc.)
   - Exposes regulatory functions as MCP tools
   - Runs on stdio (no network port)

4. **Node-RED Flows** (`node-red-flows.json`):
   - Pre-built flows for:
     - Simulation completion hooks → regulatory context enrichment
     - Weekly recall scans for tracked platforms

---

## Installation

### Prerequisites

- **Node.js** >= 18.x
- **npm** >= 9.x

### Setup

```bash
cd regulatory-api

# Install dependencies
npm install

# Copy and configure environment
cp .env.example .env
# Edit .env and add API keys (optional but recommended)

# Build TypeScript
npm run build
```

### Configuration

Edit `.env`:

```bash
# API Keys (optional but increases rate limits)
FDA_API_KEY=your-key-here  # Get from https://open.fda.gov/apis/authentication/
FAA_API_KEY=your-key-here  # May be required for some FAA endpoints

# Server
PORT=3000
```

**Note**: NHTSA does not require an API key.

---

## Usage

### 1. REST API Server

Start the server:

```bash
npm start
```

The API will be available at `http://localhost:3000`.

#### Example Requests

**FDA - Search drug recalls:**
```bash
curl "http://localhost:3000/api/fda/recalls/drug?query=insulin&limit=5"
```

**NHTSA - Decode VIN:**
```bash
curl "http://localhost:3000/api/nhtsa/vin/1HGBH41JXMN109186"
```

**NHTSA - Get recalls:**
```bash
curl "http://localhost:3000/api/nhtsa/recalls?make=Tesla&model=Model%203&year=2023"
```

**FAA - Get weather (METAR):**
```bash
curl "http://localhost:3000/api/faa/weather/metar?station=KJFK"
```

**FAA - Route weather:**
```bash
curl "http://localhost:3000/api/faa/weather/route?departure=KJFK&arrival=KLAX"
```

#### Available Endpoints

**FDA:**
- `GET /api/fda/recalls/drug?query=...`
- `GET /api/fda/recalls/device?query=...`
- `GET /api/fda/events/adverse?drug=...`
- `GET /api/fda/events/device?product=...`
- `GET /api/fda/ratelimit`

**NHTSA:**
- `GET /api/nhtsa/vin/:vin`
- `GET /api/nhtsa/vehicle?make=...&model=...&year=...`
- `GET /api/nhtsa/recalls?make=...&model=...&year=...`
- `GET /api/nhtsa/complaints?make=...&model=...&year=...`
- `GET /api/nhtsa/safety-ratings?make=...&model=...&year=...`
- `GET /api/nhtsa/manufacturer?name=...`

**FAA:**
- `GET /api/faa/weather/metar?station=...`
- `GET /api/faa/weather/taf?station=...`
- `GET /api/faa/weather/route?departure=...&arrival=...`
- `GET /api/faa/weather/sigmet?region=...`
- `GET /api/faa/weather/airmet?region=...`
- `GET /api/faa/weather/pirep?station=...&radius=...`

**Health:**
- `GET /health`

---

### 2. MCP Server (for LLM Agents)

The MCP server exposes regulatory APIs as tools for LLM agents (Claude Code, etc.).

#### Add to Claude Desktop Config

Edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) or equivalent:

```json
{
  "mcpServers": {
    "regulatory": {
      "command": "node",
      "args": ["/absolute/path/to/regulatory-api/dist/mcp-server.js"],
      "env": {
        "FDA_API_KEY": "your-key-here"
      }
    }
  }
}
```

#### Available MCP Tools

- `fda_search_drug_recalls(query, limit?, classification?)`
- `fda_search_device_recalls(query, limit?, classification?)`
- `fda_search_adverse_events(drug, limit?, serious?)`
- `fda_search_device_events(product, limit?)`
- `nhtsa_decode_vin(vin)`
- `nhtsa_get_recalls(make, model, year)`
- `nhtsa_get_complaints(make, model, year)`
- `nhtsa_get_safety_ratings(make, model, year)`
- `faa_get_metar(station, hours?)`
- `faa_get_taf(station, hours?)`
- `faa_get_route_weather(departure, arrival)`
- `faa_get_pireps(station, radius?)`

#### Example (in Claude)

```
User: "Check if there are any recalls for the 2023 Tesla Model 3"

Claude: [Uses nhtsa_get_recalls tool]
        Found 2 recalls for 2023 Tesla Model 3:
        - Campaign 23V123: Front seat belt issue
        - Campaign 23V456: Touchscreen software update
```

---

### 3. Node-RED Integration

Import `node-red-flows.json` into your Node-RED instance.

#### Flows Included

**Simulation Complete Hook:**
- Listens on `/hooks/simulation-complete` (POST)
- Receives simulation metadata
- Queries appropriate regulatory APIs based on platform type
- Enriches simulation with real-world regulatory context
- Returns combined data

**Weekly Recall Scan:**
- Runs every Monday at 8am
- Scans for new recalls on tracked platforms
- Sends alerts if changes detected

#### Example: Trigger Simulation Hook

```bash
curl -X POST http://localhost:1880/hooks/simulation-complete \
  -H "Content-Type: application/json" \
  -d '{
    "id": "sim-12345",
    "type": "motor_control",
    "platform": {
      "type": "autonomous_vehicle",
      "make": "Tesla",
      "model": "Model 3",
      "year": 2023
    },
    "failureMode": "brake_actuator_lag"
  }'
```

Response includes simulation data + NHTSA recalls + complaints for that platform.

---

## Development

### Run in Dev Mode

```bash
npm run dev
```

Uses `nodemon` to watch for file changes and auto-restart.

### TypeScript Compilation

```bash
npm run build
```

Outputs to `dist/`.

### Project Structure

```
regulatory-api/
├── src/
│   ├── reg/
│   │   ├── fda.ts          # FDA API client
│   │   ├── nhtsa.ts        # NHTSA API client
│   │   └── faa.ts          # FAA API client
│   ├── types.ts            # Shared TypeScript types
│   ├── server.ts           # Express REST API server
│   └── mcp-server.ts       # MCP server for LLM agents
├── dist/                   # Compiled JavaScript (generated)
├── node-red-flows.json     # Pre-built Node-RED flows
├── package.json
├── tsconfig.json
├── .env.example
├── .gitignore
├── LEGAL_COMPLIANCE.md     # **READ THIS**
└── README.md
```

---

## Use Cases

### 1. Simulation Validation

**Scenario**: You run a motor control simulation that models a brake actuator failure.

**Integration**:
1. Simulation completes → sends webhook to Node-RED
2. Node-RED queries NHTSA for recalls related to brake actuators on similar platforms
3. Node-RED queries FDA (if medical device) for adverse events
4. Enriched data sent to dashboard: "Your sim failure mode matches 3 real-world recalls"

### 2. LLM-Powered Research

**Scenario**: You ask Claude to research safety issues for a specific vehicle.

**Integration**:
1. Claude uses MCP tool `nhtsa_get_recalls(make, model, year)`
2. Claude uses MCP tool `nhtsa_get_complaints(make, model, year)`
3. Claude synthesizes findings and compares with your simulation outputs

### 3. Automated Monitoring

**Scenario**: Track recalls for platforms you simulate.

**Integration**:
1. Node-RED cron job runs weekly
2. Queries NHTSA/FDA for new recalls on tracked platforms
3. Compares with last week's results
4. Sends Slack/email alert if new recalls found
5. Optionally triggers new sim runs with updated failure modes

### 4. Regulatory Context for Reports

**Scenario**: Generate a report on simulation results.

**Integration**:
1. REST API endpoint queries FDA adverse events for similar devices
2. Pulls NHTSA complaints for similar failure modes
3. Report includes section: "Real-world regulatory context" with relevant cases

---

## Rate Limits & Caching

### API Rate Limits

| API | Without Key | With Key |
|-----|-------------|----------|
| **FDA** | 240 req/min, 120k/day | 240 req/min, unlimited/day |
| **NHTSA** | ~60 req/min (estimated) | N/A |
| **FAA** | ~60 req/min (estimated) | Varies by endpoint |

**Recommendation**: Always use API keys where available (FDA).

### Caching Strategy

Implement caching at your application layer:

- **Recalls/Adverse Events**: 24 hours (data updates daily)
- **VIN Decodes**: Indefinite (static)
- **METAR**: 30 minutes (updates hourly)
- **TAF**: 6 hours (updates every 6 hours)
- **Safety Ratings**: Indefinite (static for model year)

**Example** (using Redis or in-memory cache):

```typescript
const cache = new Map();
const TTL = 24 * 60 * 60 * 1000; // 24 hours

async function getCachedRecalls(make: string, model: string, year: number) {
  const key = `recalls:${make}:${model}:${year}`;
  const cached = cache.get(key);

  if (cached && Date.now() - cached.timestamp < TTL) {
    return cached.data;
  }

  const data = await nhtsaClient.getRecallsByMakeModelYear(make, model, year);
  cache.set(key, { data, timestamp: Date.now() });
  return data;
}
```

---

## Error Handling

All clients implement robust error handling:

- **Rate limits (429)**: Returns error with reset time
- **Not found (404)**: Returns empty result set
- **Server errors (500+)**: Retries with exponential backoff (max 3 attempts)
- **Network errors**: Throws with descriptive message

**Example error response:**

```json
{
  "error": "FDA API: Rate limit exceeded. Resets at 2025-11-15T12:00:00Z"
}
```

---

## Testing

### Manual Testing

Use the provided test script:

```bash
# Test FDA
curl "http://localhost:3000/api/fda/recalls/drug?query=aspirin&limit=2"

# Test NHTSA
curl "http://localhost:3000/api/nhtsa/recalls?make=Toyota&model=Camry&year=2020"

# Test FAA
curl "http://localhost:3000/api/faa/weather/metar?station=KSFO"
```

### Health Check

```bash
curl http://localhost:3000/health
```

Expected response:
```json
{
  "status": "healthy",
  "timestamp": "2025-11-15T12:00:00.000Z",
  "services": {
    "fda": "available",
    "nhtsa": "available",
    "faa": "available"
  }
}
```

---

## Security

### API Keys

- Store in `.env` (never commit to git)
- `.gitignore` includes `.env`
- Use environment variables in production

### HTTPS

- REST API server uses HTTP locally (port 3000)
- **Production**: Use reverse proxy (nginx, Apache) with TLS termination

### Rate Limiting

- Built-in: 100 requests per 15 minutes per IP
- Configurable in `.env`

### CORS

- Enabled by default (for web clients)
- Configure allowed origins in production

---

## Deployment

### Docker (Optional)

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY dist/ ./dist/
COPY .env ./
EXPOSE 3000
CMD ["node", "dist/server.js"]
```

Build and run:
```bash
docker build -t regulatory-api .
docker run -p 3000:3000 --env-file .env regulatory-api
```

### Systemd Service (Linux)

```ini
[Unit]
Description=Regulatory API Service
After=network.target

[Service]
Type=simple
User=www-data
WorkingDirectory=/opt/regulatory-api
ExecStart=/usr/bin/node dist/server.js
Restart=on-failure
Environment=NODE_ENV=production

[Install]
WantedBy=multi-user.target
```

---

## Troubleshooting

### Issue: Rate Limit Exceeded

**Symptom**: `429 Too Many Requests`

**Solution**:
1. Get an FDA API key: https://open.fda.gov/apis/authentication/
2. Add to `.env`: `FDA_API_KEY=your-key`
3. Implement caching (see above)

### Issue: VIN Decode Returns Empty

**Symptom**: `decodeVin()` returns no data

**Causes**:
- Invalid VIN (must be 17 chars, no I/O/Q)
- VIN not in NHTSA database (older or non-US vehicles)

**Solution**: Validate VIN format before calling API.

### Issue: FAA Weather No Results

**Symptom**: METAR/TAF returns empty

**Causes**:
- Invalid ICAO code
- Station doesn't report METAR (small airports)
- Data temporarily unavailable

**Solution**: Use major airport codes (KJFK, KLAX, etc.) for testing.

---

## Contributing

1. Fork the repo
2. Create a feature branch
3. Write tests (if adding new features)
4. Ensure `npm run build` succeeds
5. Submit a pull request

---

## License

ISC (see package.json)

---

## Support & Contact

- **FDA openFDA**: https://open.fda.gov/about/contact/
- **NHTSA**: https://www.nhtsa.gov/about-nhtsa/contact-us
- **FAA**: https://www.faa.gov/contact/

For this project: [Open an issue](https://github.com/your-org/regulatory-api/issues)

---

## Acknowledgments

Data sources:
- U.S. Food and Drug Administration (FDA) via openFDA
- National Highway Traffic Safety Administration (NHTSA)
- Federal Aviation Administration (FAA) via AviationWeather.gov

**Disclaimer**: This project is not affiliated with, endorsed by, or officially connected to the FDA, NHTSA, or FAA.

---

**Last Updated**: 2025-11-15
