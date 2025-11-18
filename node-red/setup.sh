#!/bin/bash

# MotorHandPro Node-RED Setup Script
# This script initializes the Node-RED integration environment

set -e

echo "========================================="
echo "MotorHandPro Node-RED Setup"
echo "========================================="
echo ""

# Check if Docker is installed
if ! command -v docker &> /dev/null; then
    echo "ERROR: Docker is not installed. Please install Docker first."
    exit 1
fi

# Check if Docker Compose is installed
if ! command -v docker-compose &> /dev/null; then
    echo "ERROR: Docker Compose is not installed. Please install Docker Compose first."
    exit 1
fi

# Create .env file from template if it doesn't exist
if [ ! -f .env ]; then
    echo "Creating .env file from template..."
    cp .env.example .env
    echo "✓ .env file created. Please edit it with your credentials."
    echo ""
fi

# Create MQTT password file
echo "Setting up MQTT authentication..."
mkdir -p mosquitto
if [ ! -f mosquitto/passwd ]; then
    # Create password file (user: nodered, password: motorhand)
    # In production, change this password!
    docker run --rm -v $(pwd)/mosquitto:/mosquitto/config eclipse-mosquitto:latest mosquitto_passwd -c -b /mosquitto/config/passwd nodered motorhand
    echo "✓ MQTT password file created (user: nodered, password: motorhand)"
    echo "  IMPORTANT: Change this password in production!"
    echo ""
else
    echo "✓ MQTT password file already exists"
    echo ""
fi

# Create directories for persistent data
echo "Creating data directories..."
mkdir -p data/logs
mkdir -p data/context
mkdir -p data/static
mkdir -p flows
mkdir -p custom-nodes
mkdir -p grafana/provisioning/datasources
mkdir -p grafana/provisioning/dashboards
mkdir -p nginx/certs
echo "✓ Data directories created"
echo ""

# Create Grafana datasource configuration
echo "Configuring Grafana..."
cat > grafana/provisioning/datasources/timescaledb.yml <<EOF
apiVersion: 1

datasources:
  - name: TimescaleDB
    type: postgres
    access: proxy
    url: timescaledb:5432
    database: motorhand_telemetry
    user: nodered
    secureJsonData:
      password: \${TIMESCALE_PASSWORD}
    jsonData:
      sslmode: disable
      postgresVersion: 1400
      timescaledb: true
EOF
echo "✓ Grafana datasource configured"
echo ""

# Create initial flows directory structure
echo "Creating example flows structure..."
mkdir -p flows/examples
echo "✓ Flows directory structure created"
echo ""

# Pull Docker images
echo "Pulling Docker images..."
docker-compose pull
echo "✓ Docker images pulled"
echo ""

echo "========================================="
echo "Setup Complete!"
echo "========================================="
echo ""
echo "Next steps:"
echo ""
echo "1. Edit .env file with your credentials:"
echo "   nano .env"
echo ""
echo "2. Start the services:"
echo "   docker-compose up -d"
echo ""
echo "3. Access Node-RED:"
echo "   http://localhost:1880"
echo "   (default credentials: admin/admin)"
echo ""
echo "4. Access Grafana (optional):"
echo "   http://localhost:3000"
echo "   (default credentials: admin/admin)"
echo ""
echo "5. MQTT broker is available at:"
echo "   mqtt://localhost:1883"
echo "   (credentials: nodered/motorhand)"
echo ""
echo "To view logs:"
echo "   docker-compose logs -f nodered"
echo ""
echo "To stop services:"
echo "   docker-compose down"
echo ""
echo "========================================="
