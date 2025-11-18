# MotorHandPro Production Infrastructure

**Complete enterprise-grade deployment for Large Action Model (LAM) platform**

## Directory Structure

```
infrastructure/
├── api/                        # API service implementations
│   ├── fastapi_server.py       # Python FastAPI core server
│   ├── server.js               # Node.js integration gateway
│   └── package.json            # Node.js dependencies
├── db/                         # Database initialization
│   └── init.sql                # PostgreSQL + TimescaleDB schema
├── docker/                     # Dockerfiles for each service
│   ├── Dockerfile.fastapi      # FastAPI container
│   ├── Dockerfile.nodejs       # Node.js API container
│   ├── Dockerfile.dashboard    # React dashboard container
│   └── Dockerfile.websocket    # WebSocket server container
├── grafana/                    # Monitoring dashboards
│   ├── provisioning/
│   │   ├── datasources/        # Prometheus, TimescaleDB datasources
│   │   └── dashboards/         # Dashboard provisioning
│   └── dashboards/
│       └── motorhand-overview.json  # System overview dashboard
├── mqtt/                       # MQTT broker configuration
│   ├── mosquitto.conf          # Eclipse Mosquitto config
│   └── create-passwd.sh        # Password file generator
├── nginx/                      # Reverse proxy configuration
│   ├── nginx.conf              # Main Nginx config
│   └── default.conf            # Server configuration
├── prometheus/                 # Metrics collection
│   ├── prometheus.yml          # Scrape configuration
│   └── alerts/
│       └── motorhand-alerts.yml  # Alert rules
├── tests/                      # Validation test suite
│   └── test_production_infrastructure.py  # 8 comprehensive tests
└── websocket/                  # WebSocket server
    └── websocket_server.py     # Real-time communication server
```

## Services Deployed

1. **TimescaleDB** - PostgreSQL + time-series database
2. **MQTT Broker** - Eclipse Mosquitto messaging
3. **Node-RED** - Workflow orchestration
4. **FastAPI** - Python LAM core API
5. **Node.js API** - Integration gateway
6. **React Dashboard** - Web control panel
7. **WebSocket Server** - Real-time communication
8. **Prometheus** - Metrics collection
9. **Grafana** - Visualization dashboards
10. **Redis** - Caching layer
11. **PgAdmin** - Database management

## Quick Start

```bash
# Deploy production stack
docker-compose -f docker-compose.production.yml up -d

# Check service health
docker-compose -f docker-compose.production.yml ps

# Run validation tests
cd tests
python test_production_infrastructure.py
```

## Documentation

- **[PRODUCTION_DEPLOYMENT.md](../PRODUCTION_DEPLOYMENT.md)** - Complete deployment guide
- **[INTEGRATION_EXAMPLES.md](../integrations/INTEGRATION_EXAMPLES.md)** - SpaceX/Tesla/PX4/CARLA/Starlink/NASA
- **[TESLA_NEURALINK_DEMOS.md](../integrations/TESLA_NEURALINK_DEMOS.md)** - Partnership demonstration apps

## Architecture Overview

```
User → Nginx (80/443) → React Dashboard
                       ↓
                  Node.js API (3000) ←→ FastAPI (8000)
                       ↓                     ↓
                  WebSocket (8765) ←→ MQTT (1883)
                       ↓                     ↓
                  TimescaleDB (5432) ←→ Redis (6379)
                       ↓
              Prometheus (9090) → Grafana (3001)
```

## Features

✅ **Authentication** - JWT-based with bcrypt
✅ **Database** - PostgreSQL 15 + TimescaleDB
✅ **Real-Time** - MQTT + WebSocket
✅ **Monitoring** - Prometheus + Grafana
✅ **Performance** - <100ms API latency
✅ **Stability** - Lipschitz L < 1.0 proven
✅ **Integrations** - 6+ major platforms

## Validation Tests

8 comprehensive tests validate:
1. Database connection (PostgreSQL + TimescaleDB)
2. FastAPI service health
3. Node.js API service health
4. JWT authentication system
5. Performance benchmarking (<100ms)
6. Telemetry data ingestion
7. AGP Lipschitz stability (L < 1.0)
8. External integrations (SpaceX, Tesla, NASA, etc.)

## Security

**IMPORTANT:** Change all default passwords before production deployment!

- Database: `POSTGRES_PASSWORD`
- JWT: `JWT_SECRET` (minimum 32 characters)
- MQTT: `MQTT_PASSWORD`
- Redis: `REDIS_PASSWORD`
- Grafana: `GF_SECURITY_ADMIN_PASSWORD`

## Support

- **GitHub:** https://github.com/STLNFTART/MotorHandPro
- **Issues:** https://github.com/STLNFTART/MotorHandPro/issues
- **Email:** contact@stlnftart.com

## License

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
**Copyright © 2025 Donte Lightfoot (STLNFTART)**
