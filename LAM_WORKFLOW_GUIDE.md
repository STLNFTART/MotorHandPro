# MotorHandPro LAM Initialization Workflow

**Version:** 1.0.0
**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

---

## Overview

The MotorHandPro LAM (Large Action Model) Orchestrator is an intelligent system initialization and management tool that acts as the central brain for your entire production infrastructure. It loads first on system startup and provides interactive guidance for:

1. **🔐 Credential Management** - Secure storage and management of all service credentials
2. **🗺️  Credential Mapping** - Automated mapping and application of credentials to services
3. **🔔 Notification Center** - Real-time notification aggregation from all services
4. **🛠️  Service API Integration** - Complete API access to all infrastructure components

---

## Quick Start

### Launch LAM System

```bash
# Make script executable (first time only)
chmod +x start_lam_system.sh

# Start LAM Orchestrator
./start_lam_system.sh
```

The script will:
1. ✅ Check all prerequisites (Docker, Python, etc.)
2. 📦 Install required Python dependencies
3. 🤖 Launch the LAM Orchestrator
4. 🎯 Guide you through first-time setup (if needed)

---

## Features

### 1. Credential Management System

The LAM provides a secure credential vault with encryption for managing all service credentials.

#### Services Managed

- **Infrastructure Services:**
  - TimescaleDB (PostgreSQL)
  - MQTT Broker (Mosquitto)
  - Redis Cache
  - Grafana Dashboards
  - PgAdmin Database UI
  - FastAPI Core
  - Node.js Integration Gateway

- **External Integrations:**
  - GitHub Repository
  - SpaceX API
  - Tesla Integration
  - NASA Open APIs
  - JWT Authentication

#### Credential Operations

```
🔐 CREDENTIAL MANAGEMENT MENU:

1. View all credentials (masked for security)
2. Add/Update credential (interactive input with hidden passwords)
3. Delete credential (with confirmation)
4. Test credentials (verify connectivity)
5. Export credentials template (.env file)
6. Import credentials from file (.env import)
7. Generate secure credentials (random secure generation)
8. Back to main menu
```

#### Security Features

- **Encrypted Storage:** XOR encryption (can be upgraded to AES-256)
- **Masked Display:** Passwords shown as `****` in listings
- **Hidden Input:** `getpass` for sensitive credential entry
- **Secure Generation:** Cryptographically secure random credentials
- **File Location:** `~/.motorhand/credentials.json.enc`

### 2. Credential Mapping Framework

Automatically maps and applies credentials to all services.

#### Mapping Operations

```
🗺️  CREDENTIAL MAPPING MENU:

1. View service → credential mappings
2. Map credentials to Docker containers
3. Map credentials to API endpoints
4. Map credentials to repositories
5. Auto-apply credentials to all services
6. Verify credential mappings
7. Export docker-compose with credentials
8. Back to main menu
```

#### Auto-Apply Workflow

The LAM can automatically:
1. Generate missing credentials securely
2. Create `.env` file with all credentials
3. Update Docker Compose environment variables
4. Configure service connections
5. Restart services with new credentials

#### Credential Template Example

```env
# TimescaleDB (PostgreSQL)
POSTGRES_USER=motorhand
POSTGRES_PASSWORD=generated_8f7a2b3c
POSTGRES_DB=motorhand

# MQTT Broker (Mosquitto)
MQTT_USERNAME=motorhand
MQTT_PASSWORD=generated_9e2d4f1a

# JWT Authentication
JWT_SECRET=xrP8KqW5...  (64 characters)

# Redis Cache
REDIS_PASSWORD=generated_3a7f9b2e
```

### 3. Notification Center

Real-time notification aggregation from all service containers with interactive management.

#### Features

- **Real-Time Monitoring:** Live Docker container log streaming
- **Notification Levels:** DEBUG, INFO, WARNING, ERROR, CRITICAL
- **Sources:** Docker, MQTT, Database, API, System, Integration
- **Action Tracking:** Mark notifications that require action
- **Acknowledgment:** Track which notifications have been addressed
- **Filtering:** View by level, source, or action-required status
- **Export:** Save notifications to JSON for analysis

#### Notification Display

```
🔔 NOTIFICATIONS
================================================================================

🚨 CRITICAL (2)
--------------------------------------------------------------------------------
[○] ⚡ 14:23:45 | motorhand-fastapi    | Database connection failed
[○] ⚡ 14:22:10 | motorhand-timescaledb| Disk space critical: 95% used

❌ ERROR (5)
--------------------------------------------------------------------------------
[✓]   14:20:33 | motorhand-mqtt       | Authentication failed for client
[○]   14:18:55 | motorhand-nodejs-api | Integration timeout: SpaceX API

⚠️  WARNING (12)
--------------------------------------------------------------------------------
[✓]   14:17:22 | motorhand-redis      | Memory usage high: 85%
[○]   14:15:40 | motorhand-grafana    | Dashboard load time exceeded

Total: 19 | Unacknowledged: 8 | Action Required: 3
```

#### Notification Operations

```
🔔 NOTIFICATION CENTER:

1. View all notifications
2. View by level (Critical/Error/Warning/Info)
3. View by source (Docker/MQTT/Database/API)
4. View notifications requiring action
5. Acknowledge notification
6. Acknowledge all
7. Clear acknowledged notifications
8. Start real-time monitoring
9. Export notifications to file
10. Back to main menu
```

### 4. Service API Integration

Complete API access to all infrastructure components.

#### Available APIs

##### Docker API
```python
from lam.core.service_apis import get_api_manager

api_manager = get_api_manager()

# List containers
containers = api_manager.docker.list_containers()

# Get container logs
logs = api_manager.docker.get_container_logs('motorhand-fastapi')

# Restart container
api_manager.docker.restart_container('motorhand-mqtt')

# Get container stats
stats = api_manager.docker.get_container_stats('motorhand-timescaledb')
```

##### TimescaleDB API
```python
# Initialize database connection
db_config = ServiceAPIConfig(
    name="TimescaleDB",
    host="localhost",
    port=5432,
    username="motorhand",
    password="your_password",
    database="motorhand"
)
await api_manager.initialize_database(db_config)

# Get telemetry summary
telemetry = await api_manager.database.get_telemetry_summary()

# Get AGP state summary
agp_state = await api_manager.database.get_agp_state_summary()

# Get active experiments
experiments = await api_manager.database.get_active_experiments()

# Execute custom query
results = await api_manager.database.execute_query(
    "SELECT * FROM telemetry.spacecraft WHERE spacecraft_id = $1",
    "cubesat-001"
)
```

##### MQTT API
```python
# Initialize MQTT connection
mqtt_config = ServiceAPIConfig(
    name="MQTT",
    host="localhost",
    port=1883,
    username="motorhand",
    password="your_password"
)
api_manager.initialize_mqtt(mqtt_config)

# Publish message
api_manager.mqtt.publish("motorhand/telemetry/test", "Hello from LAM!")

# Subscribe to topic
api_manager.mqtt.subscribe("motorhand/#")

# Get recent messages
messages = api_manager.mqtt.get_recent_messages(topic_filter="telemetry")
```

##### Redis API
```python
# Initialize Redis connection
redis_config = ServiceAPIConfig(
    name="Redis",
    host="localhost",
    port=6379,
    password="your_password"
)
api_manager.initialize_redis(redis_config)

# Set value
api_manager.redis.set("key", "value", expire=3600)

# Get value
value = api_manager.redis.get("key")

# Get Redis info
info = api_manager.redis.get_info()
```

##### Prometheus API
```python
# Query metrics
result = await api_manager.prometheus.query("up{job='fastapi'}")

# Query range
result = await api_manager.prometheus.query_range(
    query="http_requests_total",
    start="2025-01-01T00:00:00Z",
    end="2025-01-01T23:59:59Z",
    step="1m"
)

# Get scrape targets
targets = await api_manager.prometheus.get_targets()
```

##### Grafana API
```python
# Get all dashboards
dashboards = await api_manager.grafana.get_dashboards()

# Get specific dashboard
dashboard = await api_manager.grafana.get_dashboard("motorhand-overview")
```

##### External APIs
```python
# SpaceX API
launch_data = await api_manager.external.spacex_latest_launch()

# NASA API
asteroids = await api_manager.external.nasa_asteroids(api_key="your_key")
```

---

## System Workflow

### First-Time Setup

```
1. Run: ./start_lam_system.sh
   ↓
2. LAM Orchestrator launches
   ↓
3. Main Menu appears with LAM recommendation:
   💡 LAM Analysis: Based on system initialization, recommended action: Credential Management
   ↓
4. Select Option 1: 🔐 Credential Management
   ↓
5. Select Option 7: Generate secure credentials
   → LAM generates all missing credentials automatically
   ↓
6. Select Option 5: Export credentials template
   → Creates .env file with all credentials
   ↓
7. Return to Main Menu
   ↓
8. Select Option 2: 🗺️  Credential Mapping
   ↓
9. Select Option 5: Auto-apply credentials to all services
   → LAM configures all services with credentials
   ↓
10. Select Option 7: Deploy Services
    → Docker Compose starts with configured credentials
    ↓
11. ✅ System is running!
```

### Daily Usage

```
1. Run: ./start_lam_system.sh
   ↓
2. Select Option 3: 📊 View System Health
   → Check all services are running
   ↓
3. Select Option 4: 🔔 Notification Center
   → Review any alerts or issues
   → Acknowledge resolved notifications
   ↓
4. Select Option 5: 🛠️  Service API Integration
   → Interact with services programmatically
   ↓
5. Select Option 6: 📈 System Status & Monitoring
   → View real-time metrics
```

---

## File Structure

```
MotorHandPro/
├── lam_orchestrator.py                    # Main LAM orchestrator
├── start_lam_system.sh                    # Startup script
├── lam/
│   └── core/
│       ├── notification_system.py         # Notification aggregation
│       └── service_apis.py                # Service API clients
├── ~/.motorhand/                          # User configuration directory
│   ├── credentials.json.enc               # Encrypted credentials
│   ├── config.json                        # LAM configuration
│   └── notifications.log                  # Notification history
└── LAM_WORKFLOW_GUIDE.md                  # This document
```

---

## Python Dependencies

The LAM system requires the following Python packages:

```bash
pip install asyncio aiohttp asyncpg paho-mqtt redis docker
```

These are automatically installed by `start_lam_system.sh`.

---

## Configuration

### Credential Storage

Credentials are stored in `~/.motorhand/credentials.json.enc` with simple XOR encryption.

**⚠️  Production Recommendation:** Replace XOR encryption with AES-256 or use a dedicated secrets management system (HashiCorp Vault, AWS Secrets Manager, etc.)

### Upgrading Encryption

To upgrade to AES-256 encryption:

1. Install `cryptography` package:
   ```bash
   pip install cryptography
   ```

2. Modify `lam_orchestrator.py`:
   ```python
   from cryptography.fernet import Fernet

   def _encrypt(self, data: str) -> str:
       key = Fernet.generate_key()  # Store this key securely!
       f = Fernet(key)
       return f.encrypt(data.encode()).decode()

   def _decrypt(self, data: str) -> str:
       f = Fernet(self.encryption_key)
       return f.decrypt(data.encode()).decode()
   ```

---

## Troubleshooting

### LAM Won't Start

**Problem:** `ModuleNotFoundError: No module named 'lam'`

**Solution:**
```bash
# Ensure you're in the MotorHandPro directory
cd /path/to/MotorHandPro

# Install dependencies
pip3 install asyncio aiohttp asyncpg paho-mqtt redis docker

# Run startup script
./start_lam_system.sh
```

### Credentials Not Saving

**Problem:** Credentials disappear after restart

**Solution:**
```bash
# Check permissions on config directory
ls -la ~/.motorhand

# Ensure directory is writable
chmod 700 ~/.motorhand
```

### Docker Connection Failed

**Problem:** `Docker client initialization failed`

**Solution:**
```bash
# Ensure Docker is running
docker ps

# Check Docker socket permissions
ls -la /var/run/docker.sock

# Add user to docker group (Linux)
sudo usermod -aG docker $USER
# Log out and back in
```

### Database Connection Failed

**Problem:** `Database connection failed`

**Solution:**
```bash
# Check if TimescaleDB container is running
docker ps | grep timescaledb

# Verify credentials match
docker logs motorhand-timescaledb

# Test connection manually
psql -h localhost -U motorhand -d motorhand
```

---

## Advanced Usage

### Programmatic API Access

You can use the LAM service APIs programmatically in your own Python scripts:

```python
import asyncio
from lam.core.service_apis import get_api_manager, ServiceAPIConfig

async def main():
    # Initialize API manager
    api_manager = get_api_manager()

    # Configure database
    db_config = ServiceAPIConfig(
        name="TimescaleDB",
        host="localhost",
        port=5432,
        username="motorhand",
        password="your_password",
        database="motorhand"
    )
    await api_manager.initialize_database(db_config)

    # Get system overview
    overview = await api_manager.get_system_overview()
    print(f"Running containers: {len(overview['containers'])}")
    print(f"Recent telemetry: {len(overview['telemetry_summary'])} spacecraft")

    # Cleanup
    await api_manager.cleanup()

if __name__ == "__main__":
    asyncio.run(main())
```

### Custom Notification Handlers

You can subscribe to specific notification levels:

```python
from lam.core.notification_system import get_aggregator, NotificationLevel

def handle_critical(notification):
    print(f"🚨 CRITICAL: {notification.message}")
    # Send email, SMS, etc.

# Get aggregator instance
aggregator = get_aggregator()

# Subscribe to critical notifications
aggregator.subscribe(NotificationLevel.CRITICAL, handle_critical)
```

---

## Security Best Practices

1. **Change Default Credentials:** Always change default credentials before production deployment
2. **Use Strong Passwords:** Generate credentials with option 7 (minimum 32 characters)
3. **Restrict Access:** Ensure `~/.motorhand/` directory is only readable by you (`chmod 700`)
4. **Rotate Credentials:** Regularly update credentials (monthly recommended)
5. **Audit Logs:** Review notification logs for unauthorized access attempts
6. **Network Security:** Use firewall rules to restrict service access
7. **TLS/SSL:** Enable SSL for all production services
8. **Backup Credentials:** Securely backup `credentials.json.enc` to encrypted storage

---

## Support & Documentation

- **Main Documentation:** `PRODUCTION_DEPLOYMENT.md`
- **Integration Examples:** `integrations/INTEGRATION_EXAMPLES.md`
- **Infrastructure Guide:** `infrastructure/README.md`
- **GitHub Issues:** https://github.com/STLNFTART/MotorHandPro/issues
- **Email Support:** contact@stlnftart.com

---

## License & Patent

**Copyright © 2025 Donte Lightfoot (STLNFTART)**
**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846

This software is provided for research and evaluation purposes only.
Commercial use requires explicit written permission from the author.
