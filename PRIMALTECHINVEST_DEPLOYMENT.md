# MotorHandPro Deployment Guide for www.primaltechinvest.com

**Complete production deployment guide for hosting all simulations and services**

---

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [DNS Configuration](#dns-configuration)
4. [Server Setup](#server-setup)
5. [Deployment Steps](#deployment-steps)
6. [SSL/TLS Configuration](#ssltls-configuration)
7. [Post-Deployment](#post-deployment)
8. [Monitoring & Maintenance](#monitoring--maintenance)
9. [Troubleshooting](#troubleshooting)

---

## Overview

This deployment hosts the complete MotorHandPro platform at **www.primaltechinvest.com**, including:

- **LAM (Large Action Model)** - Core AI orchestration system
- **React Dashboard** - Web control panel
- **FastAPI** - Python backend for simulations
- **Node.js API** - Integration gateway
- **TimescaleDB** - Time-series database
- **MQTT Broker** - Real-time messaging
- **Node-RED** - Workflow orchestration
- **Grafana** - Monitoring dashboards
- **Prometheus** - Metrics collection
- **WebSocket Server** - Real-time bidirectional communication

### Architecture

```
Internet → Nginx (443) → React Dashboard
                        ↓
                    /api → FastAPI (LAM simulations)
                    /integration → Node.js API
                    /ws → WebSocket
                    /monitoring → Grafana
                    /nodered → Node-RED
```

---

## Prerequisites

### Server Requirements

- **OS**: Ubuntu 22.04 LTS (recommended) or similar Linux distro
- **RAM**: Minimum 8GB, 16GB+ recommended
- **CPU**: 4+ cores recommended
- **Storage**: 50GB+ SSD
- **Network**: Static IP address with ports 80, 443 open

### Software Requirements

- Docker Engine 24.0+
- Docker Compose 2.20+
- Git
- curl

### Domain Requirements

- Domain registered: **primaltechinvest.com** ✓
- DNS access to create A records
- Email for SSL certificate notifications

---

## DNS Configuration

### Required DNS Records

Point your domain to your server's IP address:

```
Type    Name                    Value              TTL
A       primaltechinvest.com    <YOUR_SERVER_IP>   300
A       www.primaltechinvest.com <YOUR_SERVER_IP>  300
```

### Verification

After configuring DNS (allow 5-60 minutes for propagation):

```bash
# Check DNS resolution
dig primaltechinvest.com +short
dig www.primaltechinvest.com +short

# Should both return your server IP
```

---

## Server Setup

### 1. Install Docker

```bash
# Update packages
sudo apt update && sudo apt upgrade -y

# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Add user to docker group
sudo usermod -aG docker $USER

# Logout and login for group changes to take effect
```

### 2. Install Docker Compose

```bash
# Install Docker Compose
sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Verify installation
docker --version
docker-compose --version
```

### 3. Clone Repository

```bash
# Clone the repository
git clone https://github.com/STLNFTART/MotorHandPro.git
cd MotorHandPro

# Checkout deployment branch
git checkout claude/unified-results-framework-01AqMEy8x8ruuHXhChvVa9Xz
```

### 4. Configure Firewall

```bash
# Allow SSH, HTTP, HTTPS
sudo ufw allow 22/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw enable
```

---

## Deployment Steps

### Step 1: Update Security Secrets

**CRITICAL**: Update these secrets before deploying!

Edit `docker-compose.production.yml` and change:

1. **PostgreSQL Password** (line 17):
   ```yaml
   - POSTGRES_PASSWORD=YOUR_STRONG_PASSWORD_HERE
   ```

2. **JWT Secret** (line 93):
   ```yaml
   - JWT_SECRET=YOUR_RANDOM_SECRET_HERE_32_CHARS_MIN
   ```
   Generate with: `openssl rand -base64 32`

3. **Redis Password** (line 260):
   ```yaml
   command: redis-server --appendonly yes --requirepass YOUR_REDIS_PASSWORD
   ```

4. **Grafana Admin Password** (line 278):
   ```yaml
   - GF_SECURITY_ADMIN_PASSWORD=YOUR_GRAFANA_PASSWORD
   ```

5. **Update email in SSL script**:
   Edit `infrastructure/ssl/init-letsencrypt.sh` line 9:
   ```bash
   email="your-email@primaltechinvest.com"
   ```

### Step 2: Run Deployment Script

```bash
# Make script executable (if not already)
chmod +x deploy-primaltechinvest.sh

# Run deployment
./deploy-primaltechinvest.sh
```

The script will:
1. ✓ Check prerequisites
2. ✓ Create directories
3. ✓ Build Docker images
4. ✓ Obtain SSL certificates
5. ✓ Start all services
6. ✓ Run health checks

**Note**: First deployment takes 5-10 minutes for image builds.

### Step 3: Verify Deployment

```bash
# Check all containers are running
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml ps

# Expected output: All services "Up" and "healthy"
```

---

## SSL/TLS Configuration

SSL certificates are automatically obtained from Let's Encrypt during deployment.

### Manual SSL Renewal (if needed)

```bash
# Force certificate renewal
./infrastructure/ssl/init-letsencrypt.sh
```

### Auto-Renewal

Certificates automatically renew every 12 hours via the certbot container.

### Verify SSL

```bash
# Test SSL configuration
curl -I https://www.primaltechinvest.com

# Check certificate expiration
echo | openssl s_client -servername www.primaltechinvest.com -connect www.primaltechinvest.com:443 2>/dev/null | openssl x509 -noout -dates
```

---

## Post-Deployment

### 1. Test Endpoints

```bash
# Main site
curl -I https://www.primaltechinvest.com

# API health
curl https://www.primaltechinvest.com/api/health

# WebSocket (requires wscat)
wscat -c wss://www.primaltechinvest.com/ws
```

### 2. Access Services

- **Main Dashboard**: https://www.primaltechinvest.com
- **API Documentation**: https://www.primaltechinvest.com/api/docs
- **Grafana Monitoring**: https://www.primaltechinvest.com/monitoring
- **Node-RED**: https://www.primaltechinvest.com/nodered

### 3. Change Default Passwords

**Grafana**:
1. Go to https://www.primaltechinvest.com/monitoring
2. Login: `admin` / `<your_password_from_compose_file>`
3. Change password immediately

**PgAdmin** (local only):
1. Go to http://localhost:5050
2. Login: `admin@motorhand.local` / `<your_password>`
3. Change password

### 4. Configure Database Backups

```bash
# Create backup script
cat > backup-db.sh << 'EOF'
#!/bin/bash
BACKUP_DIR="/backups/motorhand"
DATE=$(date +%Y%m%d_%H%M%S)
mkdir -p $BACKUP_DIR

docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml \
  exec -T timescaledb pg_dump -U motorhand motorhand | \
  gzip > $BACKUP_DIR/motorhand_$DATE.sql.gz

# Keep only last 7 days
find $BACKUP_DIR -name "motorhand_*.sql.gz" -mtime +7 -delete
EOF

chmod +x backup-db.sh

# Add to crontab (daily at 2 AM)
(crontab -l 2>/dev/null; echo "0 2 * * * /home/$USER/MotorHandPro/backup-db.sh") | crontab -
```

---

## Monitoring & Maintenance

### View Logs

```bash
# All services
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs -f

# Specific service
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs -f fastapi

# Last 100 lines
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs --tail=100
```

### Restart Services

```bash
# Restart specific service
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart fastapi

# Restart all services
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart
```

### Update Application

```bash
# Pull latest changes
git pull origin claude/unified-results-framework-01AqMEy8x8ruuHXhChvVa9Xz

# Rebuild and restart
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml up -d --build
```

### Monitor Resources

```bash
# Docker stats
docker stats

# Disk usage
df -h
docker system df
```

### Grafana Dashboards

Access monitoring at: https://www.primaltechinvest.com/monitoring

Pre-configured dashboards:
- System overview
- API latency metrics
- Database performance
- Container resource usage

---

## Troubleshooting

### Issue: SSL Certificate Failed

**Solution**:
```bash
# Check DNS is pointing to correct IP
dig www.primaltechinvest.com +short

# Ensure ports 80 and 443 are open
sudo netstat -tlnp | grep -E ':(80|443)'

# Try staging mode first (edit init-letsencrypt.sh, set staging=1)
./infrastructure/ssl/init-letsencrypt.sh
```

### Issue: Service Won't Start

**Solution**:
```bash
# Check logs
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs <service-name>

# Check container status
docker ps -a

# Restart service
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart <service-name>
```

### Issue: Database Connection Errors

**Solution**:
```bash
# Check TimescaleDB is running
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml ps timescaledb

# Test connection
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml exec timescaledb psql -U motorhand -d motorhand -c "SELECT version();"
```

### Issue: High Memory Usage

**Solution**:
```bash
# Check memory usage by container
docker stats --no-stream

# Restart memory-heavy services
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart grafana prometheus
```

### Issue: 502 Bad Gateway

**Solution**:
```bash
# Check nginx logs
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs dashboard

# Check backend services are running
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml ps fastapi nodejs-api

# Restart nginx
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart dashboard
```

---

## Maintenance Schedule

### Daily
- Monitor service health via Grafana
- Check disk space: `df -h`

### Weekly
- Review logs for errors
- Check SSL certificate expiration
- Review backup files

### Monthly
- Update Docker images: `docker-compose pull`
- Clean up old images: `docker system prune -a`
- Review security updates: `sudo apt update && sudo apt upgrade`

---

## Support & Documentation

- **Repository**: https://github.com/STLNFTART/MotorHandPro
- **Issues**: https://github.com/STLNFTART/MotorHandPro/issues
- **Full Infrastructure Docs**: [PRODUCTION_INFRASTRUCTURE_SUMMARY.md](./PRODUCTION_INFRASTRUCTURE_SUMMARY.md)
- **Integration Examples**: [integrations/INTEGRATION_EXAMPLES.md](./integrations/INTEGRATION_EXAMPLES.md)

---

## Security Checklist

- [ ] All default passwords changed
- [ ] SSL certificate obtained and valid
- [ ] Firewall configured (ports 22, 80, 443 only)
- [ ] Database backups automated
- [ ] Monitoring alerts configured in Grafana
- [ ] Server OS updated
- [ ] Docker images updated
- [ ] Access logs reviewed

---

## Quick Reference

```bash
# Start all services
./deploy-primaltechinvest.sh

# Stop all services
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml down

# View logs
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml logs -f

# Restart service
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml restart <service>

# Check status
docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml ps

# Update code
git pull && docker-compose -f docker-compose.production.yml -f docker-compose.primaltechinvest.yml up -d --build
```

---

**Patent Pending**: U.S. Provisional Patent Application No. 63/842,846
**Copyright © 2025 Donte Lightfoot (STLNFTART)**
