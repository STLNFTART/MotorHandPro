# MotorHandPro Deployment Guide

Complete guide for deploying MotorHandPro in various environments from development to production.

## Table of Contents

- [Deployment Options](#deployment-options)
- [Development Deployment](#development-deployment)
- [Docker Deployment](#docker-deployment)
- [Kubernetes Deployment](#kubernetes-deployment)
- [Embedded/Hardware Deployment](#embeddedhardware-deployment)
- [Production Considerations](#production-considerations)
- [Monitoring and Operations](#monitoring-and-operations)
- [Scaling](#scaling)
- [Security](#security)
- [Troubleshooting](#troubleshooting)

## Deployment Options

| Environment | Use Case | Complexity | Scalability |
|-------------|----------|------------|-------------|
| Local/Dev | Development, testing | Low | Single machine |
| Docker Compose | Staging, small production | Medium | Single host |
| Kubernetes | Large production | High | Multi-host cluster |
| Embedded | Hardware control | Medium | Single device |
| Serverless | Event-driven tasks | Medium | Auto-scaling |

## Development Deployment

### Prerequisites

```bash
# System requirements
- Python 3.10+
- Git
- 4GB RAM minimum
- 10GB disk space

# Optional
- Docker (for containerized dev)
- Arduino IDE (for hardware)
```

### Setup

```bash
# 1. Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# 2. Create virtual environment
python3 -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate

# 3. Install dependencies
pip install --upgrade pip
pip install -r requirements.txt

# 4. Configure environment
cp .env.example .env
# Edit .env with your settings

# 5. Verify installation
python validate_algorithms.py
```

### Running Development Server

```bash
# Terminal 1: LAM Orchestrator
python lam_orchestrator.py

# Terminal 2: Control Panel (optional)
cd control_panel
python -m http.server 8080

# Terminal 3: MQTT Broker (if using integrations)
cd node-red/mosquitto
mosquitto -c mosquitto.conf
```

### Development Workflow

```bash
# Make changes
vim extras/primal/kernel_v4.py

# Run tests
pytest tests/

# Run simulation
python run_comprehensive_sweep.py

# Commit
git add .
git commit -m "feat: improve kernel performance"
```

## Docker Deployment

### Single Container

**Dockerfile:**
```dockerfile
FROM python:3.11-slim

WORKDIR /app

# Install dependencies
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Copy application
COPY . .

# Expose ports
EXPOSE 8765 8000

# Run LAM orchestrator
CMD ["python", "lam_orchestrator.py"]
```

**Build and run:**
```bash
# Build image
docker build -t motorhandpro:latest .

# Run container
docker run -d \
  --name motorhand \
  -p 8765:8765 \
  -p 8000:8000 \
  -v $(pwd)/data:/app/data \
  motorhandpro:latest
```

### Docker Compose (Recommended for Staging)

**docker-compose.yml:**
```yaml
version: '3.8'

services:
  lam:
    build: .
    ports:
      - "8765:8765"
      - "8000:8000"
    environment:
      - DATABASE_URL=postgresql://user:pass@db:5432/motorhand
      - MQTT_BROKER=mqtt://mosquitto:1883
    volumes:
      - ./data:/app/data
    depends_on:
      - db
      - mosquitto

  db:
    image: timescale/timescaledb:latest-pg14
    environment:
      POSTGRES_DB: motorhand
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
    volumes:
      - timescale_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  mosquitto:
    image: eclipse-mosquitto:2
    volumes:
      - ./node-red/mosquitto/mosquitto.conf:/mosquitto/config/mosquitto.conf
    ports:
      - "1883:1883"
      - "9001:9001"

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - grafana_data:/var/lib/grafana
      - ./infrastructure/grafana:/etc/grafana/provisioning

  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./infrastructure/prometheus/prometheus.yml:/etc/prometheus/prometheus.yml
      - prometheus_data:/prometheus

volumes:
  timescale_data:
  grafana_data:
  prometheus_data:
```

**Deploy:**
```bash
# Start all services
docker-compose up -d

# Check status
docker-compose ps

# View logs
docker-compose logs -f lam

# Stop services
docker-compose down
```

## Kubernetes Deployment

### Prerequisites

```bash
# Install kubectl
curl -LO "https://dl.k8s.io/release/$(curl -L -s https://dl.k8s.io/release/stable.txt)/bin/linux/amd64/kubectl"
chmod +x kubectl
sudo mv kubectl /usr/local/bin/

# Verify cluster access
kubectl cluster-info
```

### Deployment Manifests

**k8s/deployment.yaml:**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: motorhand-lam
  labels:
    app: motorhand
    component: lam
spec:
  replicas: 3
  selector:
    matchLabels:
      app: motorhand
      component: lam
  template:
    metadata:
      labels:
        app: motorhand
        component: lam
    spec:
      containers:
      - name: lam
        image: motorhandpro:latest
        ports:
        - containerPort: 8765
          name: websocket
        - containerPort: 8000
          name: http
        env:
        - name: DATABASE_URL
          valueFrom:
            secretKeyRef:
              name: motorhand-secrets
              key: database-url
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8000
          initialDelaySeconds: 5
          periodSeconds: 5
```

**k8s/service.yaml:**
```yaml
apiVersion: v1
kind: Service
metadata:
  name: motorhand-lam
spec:
  selector:
    app: motorhand
    component: lam
  ports:
  - name: websocket
    port: 8765
    targetPort: 8765
  - name: http
    port: 8000
    targetPort: 8000
  type: LoadBalancer
```

**Deploy to Kubernetes:**
```bash
# Create namespace
kubectl create namespace motorhand

# Apply configurations
kubectl apply -f k8s/ -n motorhand

# Check deployment status
kubectl get pods -n motorhand
kubectl get svc -n motorhand

# View logs
kubectl logs -f deployment/motorhand-lam -n motorhand

# Scale deployment
kubectl scale deployment motorhand-lam --replicas=5 -n motorhand
```

### Helm Chart (Advanced)

```bash
# Install Helm
curl https://raw.githubusercontent.com/helm/helm/main/scripts/get-helm-3 | bash

# Create Helm chart
helm create motorhand

# Install chart
helm install motorhand ./motorhand -n motorhand --create-namespace

# Upgrade
helm upgrade motorhand ./motorhand -n motorhand

# Rollback
helm rollback motorhand -n motorhand
```

## Embedded/Hardware Deployment

### Arduino Deployment

**Upload to Arduino:**
```bash
# Using Arduino CLI
arduino-cli compile --fqbn arduino:avr:uno MotorHandPro.ino
arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:uno MotorHandPro.ino

# Or use Arduino IDE
# File > Open > MotorHandPro.ino
# Tools > Board > Arduino Uno
# Tools > Port > /dev/ttyACM0
# Upload
```

**Verify deployment:**
```bash
# Monitor serial output
arduino-cli monitor -p /dev/ttyACM0 -c baudrate=115200

# Expected output:
# D = 149.9992314000
# λ = 0.16905
# F'(D) = 0.000129931830
```

### Raspberry Pi Deployment

```bash
# SSH into Pi
ssh pi@raspberrypi.local

# Clone repo
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Install dependencies
sudo apt-get update
sudo apt-get install python3-pip python3-venv
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# Install GPIO libraries
pip install RPi.GPIO pigpio

# Run control loop
sudo python rpi_control.py
```

**Autostart on boot:**
```bash
# Create systemd service
sudo nano /etc/systemd/system/motorhand.service
```

**motorhand.service:**
```ini
[Unit]
Description=MotorHandPro Control System
After=network.target

[Service]
Type=simple
User=pi
WorkingDirectory=/home/pi/MotorHandPro
ExecStart=/home/pi/MotorHandPro/venv/bin/python rpi_control.py
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

**Enable service:**
```bash
sudo systemctl enable motorhand
sudo systemctl start motorhand
sudo systemctl status motorhand
```

## Production Considerations

### Configuration Management

**Environment variables:**
```bash
# .env.production
DATABASE_URL=postgresql://prod_user:secure_pass@db.prod:5432/motorhand
MQTT_BROKER=mqtt://mqtt.prod:1883
LOG_LEVEL=INFO
ENABLE_METRICS=true
MAX_CONNECTIONS=100
```

**Secrets management:**
```bash
# Kubernetes secrets
kubectl create secret generic motorhand-secrets \
  --from-literal=database-url=postgresql://... \
  --from-literal=api-key=... \
  -n motorhand
```

### Database Migration

```bash
# Using Alembic (if database schema changes)
alembic upgrade head

# Backup before migration
pg_dump motorhand > backup_$(date +%Y%m%d).sql
```

### Load Balancing

**nginx.conf:**
```nginx
upstream motorhand_lam {
    least_conn;
    server lam1:8765;
    server lam2:8765;
    server lam3:8765;
}

server {
    listen 80;
    server_name motorhand.example.com;

    location /ws {
        proxy_pass http://motorhand_lam;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
        proxy_set_header Host $host;
    }

    location / {
        proxy_pass http://motorhand_lam;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### SSL/TLS

```bash
# Using Let's Encrypt
sudo certbot --nginx -d motorhand.example.com

# Auto-renewal
sudo certbot renew --dry-run
```

## Monitoring and Operations

### Health Checks

**Implement in lam_orchestrator.py:**
```python
from fastapi import FastAPI

app = FastAPI()

@app.get("/health")
async def health():
    return {"status": "healthy", "version": "1.0"}

@app.get("/ready")
async def ready():
    # Check database connection
    # Check MQTT broker connection
    return {"status": "ready"}
```

### Logging

**Structured logging:**
```python
import logging
import json

logger = logging.getLogger(__name__)

def log_event(event_type, data):
    logger.info(json.dumps({
        "event": event_type,
        "timestamp": datetime.now().isoformat(),
        "data": data
    }))
```

**Centralized logging (ELK Stack):**
```bash
# Filebeat configuration
filebeat.inputs:
- type: log
  paths:
    - /var/log/motorhand/*.log
  json.keys_under_root: true

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
```

### Metrics

**Prometheus metrics:**
```python
from prometheus_client import Counter, Histogram, Gauge

# Counters
control_loop_count = Counter('motorhand_control_loops_total', 'Total control loops')

# Histograms
control_latency = Histogram('motorhand_control_latency_seconds', 'Control loop latency')

# Gauges
current_psi = Gauge('motorhand_psi', 'Current control signal')

# Usage
with control_latency.time():
    psi = compute_control_signal()
    current_psi.set(psi)
    control_loop_count.inc()
```

### Alerts

**Prometheus alerting rules:**
```yaml
groups:
- name: motorhand
  rules:
  - alert: HighControlLatency
    expr: motorhand_control_latency_seconds > 0.1
    for: 5m
    annotations:
      summary: "High control loop latency detected"

  - alert: StabilityViolation
    expr: motorhand_lipschitz_constant > 1.0
    annotations:
      summary: "CRITICAL: Stability condition violated!"
```

## Scaling

### Horizontal Scaling

**Auto-scaling in Kubernetes:**
```yaml
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: motorhand-lam-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: motorhand-lam
  minReplicas: 2
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
```

### Database Scaling

**Read replicas:**
```bash
# TimescaleDB replication
# In postgresql.conf on primary:
wal_level = replica
max_wal_senders = 5

# On replica:
pg_basebackup -h primary -D /var/lib/postgresql/data -U replication -v -P
```

## Security

### Authentication

**JWT tokens:**
```python
from fastapi import Depends, HTTPException
from fastapi.security import HTTPBearer

security = HTTPBearer()

async def verify_token(credentials = Depends(security)):
    token = credentials.credentials
    # Verify JWT token
    if not is_valid_token(token):
        raise HTTPException(status_code=401, detail="Invalid token")
```

### Network Security

**Kubernetes Network Policies:**
```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: motorhand-lam-policy
spec:
  podSelector:
    matchLabels:
      app: motorhand
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: motorhand
    ports:
    - protocol: TCP
      port: 8765
```

## Troubleshooting

### Common Deployment Issues

**Container won't start:**
```bash
# Check logs
docker logs motorhand

# Common causes:
# - Missing environment variables
# - Database connection failure
# - Port already in use
```

**Database connection errors:**
```bash
# Test connection
psql -h db.prod -U user -d motorhand

# Check network
nc -zv db.prod 5432
```

**High latency:**
```bash
# Check resource usage
kubectl top pods -n motorhand

# Scale up if needed
kubectl scale deployment motorhand-lam --replicas=5
```

---

**See Also:**
- [User Guide](USER_GUIDE.md) - Getting started
- [Architecture](../ARCHITECTURE.md) - System design
- [Monitoring Setup](../../infrastructure/README.md) - Detailed monitoring

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Application No. 63/842,846
