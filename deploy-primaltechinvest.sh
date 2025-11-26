#!/bin/bash
# MotorHandPro Deployment Script for www.primaltechinvest.com
# Deploys full production stack with all simulations and services

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DOMAIN="www.primaltechinvest.com"
EMAIL="contact@primaltechinvest.com"
COMPOSE_FILES="-f docker-compose.production.yml -f docker-compose.primaltechinvest.yml"

# Function to print colored output
print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

print_header() {
    echo -e "${BLUE}======================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}======================================${NC}"
}

# Check prerequisites
print_header "Checking Prerequisites"

if ! command -v docker &> /dev/null; then
    print_error "Docker not found. Please install Docker first."
    exit 1
fi
print_success "Docker found"

if ! command -v docker-compose &> /dev/null; then
    print_error "Docker Compose not found. Please install Docker Compose first."
    exit 1
fi
print_success "Docker Compose found"

# Check if running as root or with sudo
if [ "$EUID" -ne 0 ]; then
    print_warning "Not running as root. You may need sudo for some operations."
fi

# Create necessary directories
print_header "Setting Up Directories"
mkdir -p certbot/conf certbot/www
mkdir -p infrastructure/nginx/ssl
print_success "Directories created"

# Update secrets (prompt user)
print_header "Security Configuration"
print_warning "IMPORTANT: Update these secrets in docker-compose.production.yml before deploying:"
echo "  1. POSTGRES_PASSWORD"
echo "  2. JWT_SECRET"
echo "  3. REDIS_PASSWORD (motorhand_redis_password)"
echo "  4. GF_SECURITY_ADMIN_PASSWORD"
read -p "Have you updated all secrets? (y/N) " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    print_error "Please update secrets before deploying. Exiting."
    exit 1
fi
print_success "Secrets confirmed"

# Build Docker images
print_header "Building Docker Images"
print_info "This may take several minutes..."
docker-compose $COMPOSE_FILES build
print_success "Docker images built"

# Initialize SSL certificates (first time only)
if [ ! -d "certbot/conf/live/$DOMAIN" ]; then
    print_header "Initializing SSL Certificates"
    print_info "Running Let's Encrypt certificate initialization..."
    ./infrastructure/ssl/init-letsencrypt.sh
    print_success "SSL certificates obtained"
else
    print_info "SSL certificates already exist. Skipping initialization."
fi

# Start services
print_header "Starting Services"
print_info "Launching all containers..."
docker-compose $COMPOSE_FILES up -d
print_success "Services started"

# Wait for services to be ready
print_header "Waiting for Services to Initialize"
print_info "This may take 30-60 seconds..."
sleep 15

# Health checks
print_header "Running Health Checks"

# Check TimescaleDB
if docker-compose $COMPOSE_FILES exec -T timescaledb pg_isready -U motorhand > /dev/null 2>&1; then
    print_success "TimescaleDB: Healthy"
else
    print_error "TimescaleDB: Not responding"
fi

# Check MQTT
if docker-compose $COMPOSE_FILES ps mqtt | grep -q "Up"; then
    print_success "MQTT Broker: Running"
else
    print_error "MQTT Broker: Not running"
fi

# Check FastAPI
if curl -sf http://localhost:8000/health > /dev/null 2>&1; then
    print_success "FastAPI: Healthy"
else
    print_warning "FastAPI: Not responding yet (may need more time)"
fi

# Check Node.js API
if curl -sf http://localhost:3000/health > /dev/null 2>&1; then
    print_success "Node.js API: Healthy"
else
    print_warning "Node.js API: Not responding yet (may need more time)"
fi

# Check Dashboard/Nginx
if curl -sf http://localhost/health > /dev/null 2>&1; then
    print_success "Dashboard: Healthy"
else
    print_warning "Dashboard: Not responding yet (may need more time)"
fi

# Check Grafana
if curl -sf http://localhost:3001/api/health > /dev/null 2>&1; then
    print_success "Grafana: Healthy"
else
    print_warning "Grafana: Not responding yet (may need more time)"
fi

# Display service URLs
print_header "Deployment Complete!"
echo ""
echo -e "${GREEN}🎉 MotorHandPro is now deployed!${NC}"
echo ""
echo "Access your services:"
echo ""
echo -e "${BLUE}Public (Internet):${NC}"
echo "  🌐 Main Site:       https://$DOMAIN"
echo "  🔌 API:             https://$DOMAIN/api"
echo "  🔗 WebSocket:       wss://$DOMAIN/ws"
echo "  📊 Monitoring:      https://$DOMAIN/monitoring"
echo "  🔄 Node-RED:        https://$DOMAIN/nodered"
echo ""
echo -e "${BLUE}Local (Server only):${NC}"
echo "  🗄️  TimescaleDB:     localhost:5432"
echo "  📡 MQTT:            localhost:1883"
echo "  📈 Prometheus:      http://localhost:9090"
echo "  🖥️  PgAdmin:         http://localhost:5050"
echo "  💾 Redis:           localhost:6379"
echo ""
echo -e "${YELLOW}Default Credentials (CHANGE THESE):${NC}"
echo "  Grafana:  admin / admin_change_in_production"
echo "  PgAdmin:  admin@motorhand.local / admin_change_in_production"
echo ""
echo -e "${BLUE}Useful Commands:${NC}"
echo "  View logs:          docker-compose $COMPOSE_FILES logs -f"
echo "  View specific log:  docker-compose $COMPOSE_FILES logs -f <service>"
echo "  Restart service:    docker-compose $COMPOSE_FILES restart <service>"
echo "  Stop all:           docker-compose $COMPOSE_FILES down"
echo "  View status:        docker-compose $COMPOSE_FILES ps"
echo ""
echo -e "${YELLOW}Next Steps:${NC}"
echo "  1. Point your domain DNS A record to this server's IP"
echo "  2. Test all endpoints"
echo "  3. Configure monitoring alerts in Grafana"
echo "  4. Set up database backups"
echo "  5. Review logs for any issues"
echo ""
print_success "Deployment script complete"
