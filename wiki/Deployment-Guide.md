# Deployment Guide

Comprehensive guide for deploying MotorHandPro in various environments.

## 🎯 Deployment Options

1. [Local Development](#local-development)
2. [Docker Deployment](#docker-deployment)
3. [Kubernetes Production](#kubernetes-production)
4. [Edge Deployment](#edge-deployment)
5. [Hybrid Cloud-Edge](#hybrid-cloud-edge)

---

## Local Development

### Quick Setup

```bash
# Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Create virtual environment
python3 -m venv venv
source venv/bin/activate

# Install dependencies
pip install -r requirements.txt

# Run smoke test
cd lam
python smoke_test.py
```

### Running LAM Server

```bash
# Start LAM server
cd lam
python lam_server.py

# In another terminal, start control panel
cd control_panel
python -m http.server 8080

# Open browser: http://localhost:8080
```

---

## Docker Deployment

### Single Container

```bash
# Build image
docker build -t motorhandpro:latest .

# Run container
docker run -d \
  --name motorhandpro \
  -p 8000:8000 \
  -p 8765:8765 \
  -v $(pwd)/data:/app/data \
  motorhandpro:latest
```

### Docker Compose

**File**: `docker-compose.yml`

```yaml
version: '3.8'

services:
  lam:
    build: .
    ports:
      - "8000:8000"    # REST API
      - "8765:8765"    # WebSocket
    environment:
      - LAMBDA=0.16905
      - KE=0.5
      - DISPLACEMENT_SEC=2.0
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    restart: unless-stopped

  mqtt:
    image: eclipse-mosquitto:2
    ports:
      - "1883:1883"    # MQTT
      - "9001:9001"    # WebSocket
    volumes:
      - ./mosquitto.conf:/mosquitto/config/mosquitto.conf
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data
    restart: unless-stopped

  control-panel:
    image: nginx:alpine
    ports:
      - "8080:80"
    volumes:
      - ./control_panel:/usr/share/nginx/html:ro
    restart: unless-stopped

volumes:
  redis-data:
```

**Start Stack**:

```bash
# Start all services
docker-compose up -d

# View logs
docker-compose logs -f lam

# Stop all services
docker-compose down
```

---

## Kubernetes Production

### Prerequisites

- Kubernetes cluster (v1.20+)
- kubectl configured
- Helm 3 (optional)

### Quick Deploy

```bash
# Apply manifests
kubectl apply -f k8s/namespace.yaml
kubectl apply -f k8s/deployment.yaml
kubectl apply -f k8s/service.yaml
kubectl apply -f k8s/ingress.yaml

# Check status
kubectl get pods -n motorhandpro
kubectl get svc -n motorhandpro
```

### Namespace

**File**: `k8s/namespace.yaml`

```yaml
apiVersion: v1
kind: Namespace
metadata:
  name: motorhandpro
```

### Deployment

**File**: `k8s/deployment.yaml`

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: lam-server
  namespace: motorhandpro
spec:
  replicas: 3
  selector:
    matchLabels:
      app: lam-server
  template:
    metadata:
      labels:
        app: lam-server
    spec:
      containers:
      - name: lam
        image: motorhandpro/lam:latest
        ports:
        - containerPort: 8000
          name: http
        - containerPort: 8765
          name: websocket
        env:
        - name: LAMBDA
          value: "0.16905"
        - name: KE
          value: "0.5"
        - name: REDIS_HOST
          value: "redis-service"
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

### Service

**File**: `k8s/service.yaml`

```yaml
apiVersion: v1
kind: Service
metadata:
  name: lam-service
  namespace: motorhandpro
spec:
  selector:
    app: lam-server
  ports:
  - name: http
    port: 80
    targetPort: 8000
  - name: websocket
    port: 8765
    targetPort: 8765
  type: LoadBalancer
```

### Ingress

**File**: `k8s/ingress.yaml`

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: motorhandpro-ingress
  namespace: motorhandpro
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
spec:
  tls:
  - hosts:
    - motorhandpro.example.com
    secretName: motorhandpro-tls
  rules:
  - host: motorhandpro.example.com
    http:
      paths:
      - path: /api
        pathType: Prefix
        backend:
          service:
            name: lam-service
            port:
              number: 80
      - path: /ws
        pathType: Prefix
        backend:
          service:
            name: lam-service
            port:
              number: 8765
```

### ConfigMap

**File**: `k8s/configmap.yaml`

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: lam-config
  namespace: motorhandpro
data:
  lambda: "0.16905"
  ke: "0.5"
  displacement_sec: "2.0"
  d0: "149.9992314000"
  i3: "6.4939394023"
```

### Secrets

```bash
# Create secret for credentials
kubectl create secret generic lam-secrets \
  --from-literal=jwt-secret='your-secret-key' \
  --from-literal=mqtt-password='mqtt-password' \
  -n motorhandpro
```

### Monitoring

**Prometheus ServiceMonitor**:

```yaml
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: lam-metrics
  namespace: motorhandpro
spec:
  selector:
    matchLabels:
      app: lam-server
  endpoints:
  - port: http
    path: /metrics
```

---

## Edge Deployment

### Raspberry Pi Setup

#### 1. Install OS

```bash
# Flash Raspberry Pi OS Lite
# SSH into Pi
ssh pi@raspberrypi.local
```

#### 2. Install Dependencies

```bash
# Update system
sudo apt-get update
sudo apt-get upgrade -y

# Install Python
sudo apt-get install -y python3 python3-pip python3-venv

# Install system dependencies
sudo apt-get install -y git mosquitto mosquitto-clients
```

#### 3. Deploy MotorHandPro

```bash
# Clone repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Setup virtual environment
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt

# Configure as systemd service
sudo cp systemd/motorhandpro.service /etc/systemd/system/
sudo systemctl enable motorhandpro
sudo systemctl start motorhandpro
```

#### 4. Configure MQTT Bridge

**File**: `/etc/mosquitto/conf.d/bridge.conf`

```
connection cloudbridge
address mqtt.example.com:1883
topic motorhand/# both 0
remote_username user
remote_password password
```

#### 5. Arduino Connection

```bash
# Install Arduino CLI
curl -fsSL https://raw.githubusercontent.com/arduino/arduino-cli/master/install.sh | sh

# Upload sketch
arduino-cli compile --fqbn arduino:avr:uno MotorHandPro.ino
arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:uno MotorHandPro.ino
```

---

## Hybrid Cloud-Edge

### Architecture

```
┌──────────────────────────────────────┐
│         Cloud (AWS/Azure)             │
│  ┌────────────────────────────────┐  │
│  │   LAM Coordination Service     │  │
│  │   (Kubernetes Cluster)         │  │
│  └────────────┬───────────────────┘  │
└───────────────┼──────────────────────┘
                │ MQTT/TLS
┌───────────────▼──────────────────────┐
│      Edge Gateway (Raspberry Pi)     │
│  ┌────────────────────────────────┐  │
│  │   Local LAM Instance           │  │
│  │   MQTT Bridge                  │  │
│  └────────────┬───────────────────┘  │
└───────────────┼──────────────────────┘
                │ Serial/GPIO
┌───────────────▼──────────────────────┐
│         Arduino Controllers          │
│         Servo Motors / Hardware      │
└──────────────────────────────────────┘
```

### Cloud Setup (AWS)

#### 1. Deploy LAM to EKS

```bash
# Create EKS cluster
eksctl create cluster --name motorhandpro --region us-east-1

# Deploy application
kubectl apply -f k8s/
```

#### 2. Configure IoT Core

```bash
# Create thing
aws iot create-thing --thing-name edge-gateway-01

# Create certificate
aws iot create-keys-and-certificate \
  --set-as-active \
  --certificate-pem-outfile cert.pem \
  --public-key-outfile public.key \
  --private-key-outfile private.key
```

### Edge Configuration

**File**: `edge-config.yaml`

```yaml
cloud:
  mqtt_broker: "mqtt.iot.us-east-1.amazonaws.com"
  port: 8883
  client_id: "edge-gateway-01"
  cert_file: "/etc/motorhand/cert.pem"
  key_file: "/etc/motorhand/private.key"
  ca_file: "/etc/motorhand/AmazonRootCA1.pem"

local:
  mqtt_broker: "localhost"
  port: 1883
  serial_port: "/dev/ttyACM0"
  baud_rate: 115200

control:
  lambda: 0.16905
  ke: 0.5
  displacement_sec: 2.0
```

---

## Production Checklist

### Pre-Deployment

- [ ] Security audit completed
- [ ] Load testing performed
- [ ] Backup strategy defined
- [ ] Monitoring configured
- [ ] Logging centralized
- [ ] Secrets management configured
- [ ] SSL/TLS certificates obtained
- [ ] DNS records configured

### Post-Deployment

- [ ] Health checks passing
- [ ] Metrics being collected
- [ ] Logs flowing to aggregator
- [ ] Alerts configured
- [ ] Backup tested
- [ ] Rollback procedure documented
- [ ] Team trained on operations

---

## Monitoring & Observability

### Prometheus Metrics

```python
# In LAM server
from prometheus_client import Counter, Histogram, Gauge

control_commands = Counter('control_commands_total', 'Total control commands')
displacement_latency = Histogram('displacement_latency_seconds', 'Displacement computation time')
active_fields = Gauge('active_fields', 'Number of active control fields')
```

### Grafana Dashboard

Import dashboard from `/infrastructure/grafana/dashboards/motorhandpro.json`

### Log Aggregation

**File**: `filebeat.yml`

```yaml
filebeat.inputs:
- type: log
  paths:
    - /app/logs/*.log
  fields:
    app: motorhandpro

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
```

---

## Scaling

### Horizontal Scaling

```bash
# Scale LAM replicas
kubectl scale deployment lam-server --replicas=5 -n motorhandpro

# Autoscaling
kubectl autoscale deployment lam-server \
  --cpu-percent=70 \
  --min=3 --max=10 \
  -n motorhandpro
```

### Load Balancing

- Use Kubernetes service for internal load balancing
- Use cloud load balancer (ALB/NLB) for external traffic
- Configure session affinity for WebSocket connections

---

## Security

### Best Practices

1. **Use TLS/SSL**: Encrypt all network traffic
2. **JWT Authentication**: Secure API endpoints
3. **RBAC**: Kubernetes role-based access control
4. **Secrets Management**: Use Kubernetes secrets or Vault
5. **Network Policies**: Restrict pod-to-pod communication
6. **Image Scanning**: Scan Docker images for vulnerabilities

### Example Network Policy

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: lam-network-policy
  namespace: motorhandpro
spec:
  podSelector:
    matchLabels:
      app: lam-server
  policyTypes:
  - Ingress
  - Egress
  ingress:
  - from:
    - podSelector:
        matchLabels:
          app: control-panel
    ports:
    - protocol: TCP
      port: 8000
```

---

## Troubleshooting

### Pod Not Starting

```bash
# Check events
kubectl describe pod <pod-name> -n motorhandpro

# Check logs
kubectl logs <pod-name> -n motorhandpro
```

### Connection Issues

```bash
# Test connectivity
kubectl run -it --rm debug --image=nicolaka/netshoot --restart=Never -- bash
# Inside pod:
curl http://lam-service/health
```

---

## Related Documentation

- [Architecture](Architecture) - System design
- [Kubernetes](Kubernetes-Deployment) - Detailed K8s guide
- [Docker Setup](Docker-Setup) - Container configuration
- [Monitoring Setup](Monitoring-Setup) - Observability

---

For detailed deployment documentation, see also:
- `/docs/guides/DEPLOYMENT.md` in repository
- `/infrastructure/README.md`
- `/k8s/README.md`
