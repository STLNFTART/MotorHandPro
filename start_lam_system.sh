#!/bin/bash
# MotorHandPro LAM System Startup Script
# This script initializes the LAM as the first action and guides system setup
# Patent Pending: U.S. Provisional Patent Application No. 63/842,846

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print banner
echo -e "${BLUE}"
cat << 'EOF'
================================================================================
██╗      █████╗ ███╗   ███╗    ███████╗██╗   ██╗███████╗████████╗███████╗███╗   ███╗
██║     ██╔══██╗████╗ ████║    ██╔════╝╚██╗ ██╔╝██╔════╝╚══██╔══╝██╔════╝████╗ ████║
██║     ███████║██╔████╔██║    ███████╗ ╚████╔╝ ███████╗   ██║   █████╗  ██╔████╔██║
██║     ██╔══██║██║╚██╔╝██║    ╚════██║  ╚██╔╝  ╚════██║   ██║   ██╔══╝  ██║╚██╔╝██║
███████╗██║  ██║██║ ╚═╝ ██║    ███████║   ██║   ███████║   ██║   ███████╗██║ ╚═╝ ██║
╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    ╚══════╝   ╚═╝   ╚══════╝   ╚═╝   ╚══════╝╚═╝     ╚═╝

MotorHandPro - Large Action Model System Initialization
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
================================================================================
EOF
echo -e "${NC}"

echo -e "${GREEN}🚀 Starting LAM System Initialization...${NC}\n"

# Check prerequisites
echo -e "${YELLOW}📋 Checking prerequisites...${NC}"

# Check Docker
if command -v docker &> /dev/null; then
    echo -e "${GREEN}✅ Docker found${NC}"
else
    echo -e "${RED}❌ Docker not found. Please install Docker first.${NC}"
    exit 1
fi

# Check Docker Compose
if command -v docker-compose &> /dev/null; then
    echo -e "${GREEN}✅ Docker Compose found${NC}"
else
    echo -e "${RED}❌ Docker Compose not found. Please install Docker Compose first.${NC}"
    exit 1
fi

# Check Python
if command -v python3 &> /dev/null; then
    PYTHON_VERSION=$(python3 --version | cut -d' ' -f2)
    echo -e "${GREEN}✅ Python $PYTHON_VERSION found${NC}"
else
    echo -e "${RED}❌ Python 3 not found. Please install Python 3.8+ first.${NC}"
    exit 1
fi

echo ""

# Check if running containers
RUNNING_CONTAINERS=$(docker ps -q | wc -l)
if [ "$RUNNING_CONTAINERS" -gt 0 ]; then
    echo -e "${YELLOW}⚠️  Found $RUNNING_CONTAINERS running container(s)${NC}"
    echo "Do you want to stop them before initializing? (recommended for fresh start)"
    read -p "Stop running containers? (y/n): " STOP_CONTAINERS

    if [ "$STOP_CONTAINERS" = "y" ]; then
        echo -e "${YELLOW}🛑 Stopping running containers...${NC}"
        docker-compose -f docker-compose.production.yml down 2>/dev/null || true
        echo -e "${GREEN}✅ Containers stopped${NC}"
    fi
fi

echo ""

# Install Python dependencies
echo -e "${YELLOW}📦 Installing Python dependencies...${NC}"
pip3 install -q --upgrade pip
pip3 install -q asyncio aiohttp asyncpg paho-mqtt redis docker 2>/dev/null || true
echo -e "${GREEN}✅ Dependencies installed${NC}"

echo ""

# Create necessary directories
echo -e "${YELLOW}📁 Creating configuration directories...${NC}"
mkdir -p ~/.motorhand
mkdir -p ./logs
mkdir -p ./data
echo -e "${GREEN}✅ Directories created${NC}"

echo ""

# Check for existing credentials
if [ -f ~/.motorhand/credentials.json.enc ]; then
    echo -e "${GREEN}✅ Found existing credentials${NC}"
    FIRST_RUN=false
else
    echo -e "${YELLOW}📝 No credentials found - first-time setup${NC}"
    FIRST_RUN=true
fi

echo ""

# Launch LAM Orchestrator
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}🤖 Launching LAM Orchestrator...${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"

echo ""

if [ "$FIRST_RUN" = true ]; then
    echo -e "${YELLOW}🎯 FIRST-TIME SETUP WORKFLOW:${NC}"
    echo ""
    echo "The LAM will guide you through:"
    echo "  1. 🔐 Credential Management - Set up service credentials"
    echo "  2. 🗺️  Credential Mapping - Map credentials to services"
    echo "  3. 🚀 Service Deployment - Deploy production infrastructure"
    echo ""
    echo "Press Enter to continue..."
    read
fi

# Run LAM Orchestrator
python3 lam_orchestrator.py

# Check exit status
if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✅ LAM Orchestrator completed successfully${NC}"

    # Check if services were deployed
    DEPLOYED_CONTAINERS=$(docker ps --filter "name=motorhand-" | wc -l)
    if [ "$DEPLOYED_CONTAINERS" -gt 1 ]; then
        echo ""
        echo -e "${GREEN}🎉 System is running with $((DEPLOYED_CONTAINERS-1)) services${NC}"
        echo ""
        echo -e "${BLUE}📊 Access Points:${NC}"
        echo "  • Dashboard:     http://localhost"
        echo "  • FastAPI Docs:  http://localhost:8000/docs"
        echo "  • Grafana:       http://localhost:3001"
        echo "  • Prometheus:    http://localhost:9090"
        echo "  • PgAdmin:       http://localhost:5050"
        echo ""
        echo -e "${YELLOW}💡 Tip: Run './start_lam_system.sh' anytime to manage your system${NC}"
    fi
else
    echo ""
    echo -e "${RED}❌ LAM Orchestrator exited with errors${NC}"
    exit 1
fi

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}🎊 MotorHandPro LAM System Ready!${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo ""
