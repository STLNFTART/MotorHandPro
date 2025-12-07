#!/bin/bash
# Start all NASA API services in development mode
#
# Usage:
#   ./start_all.sh
#
# This script starts:
#   1. FastAPI server (port 8000)
#   2. WebSocket server (port 8765)
#
# Prerequisites:
#   - Database running and migrated
#   - Python dependencies installed

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

cd "$REPO_ROOT"

echo "=============================================================================="
echo "Starting MotorHandPro NASA API Services"
echo "=============================================================================="
echo ""

# Check if database is accessible
echo "Checking database connection..."
if psql "$DATABASE_URL" -c "SELECT 1" > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Database accessible${NC}"
else
    echo -e "${YELLOW}⚠️  Database not accessible - services may fail to start${NC}"
    echo "Set DATABASE_URL or check your PostgreSQL connection"
fi

echo ""
echo "Starting services in background..."
echo ""

# Create logs directory
mkdir -p logs

# Start FastAPI server
echo -e "${BLUE}Starting FastAPI server on port 8000...${NC}"
python -m infrastructure.api > logs/fastapi.log 2>&1 &
FASTAPI_PID=$!
echo "  PID: $FASTAPI_PID"
echo "  Logs: logs/fastapi.log"

# Wait for FastAPI to start
sleep 2

# Check if FastAPI is running
if curl -sf http://localhost:8000/health > /dev/null 2>&1; then
    echo -e "${GREEN}✅ FastAPI server running${NC}"
else
    echo -e "${YELLOW}⚠️  FastAPI may not have started - check logs/fastapi.log${NC}"
fi

echo ""

# Start WebSocket server
echo -e "${BLUE}Starting WebSocket server on port 8765...${NC}"
python -m infrastructure.websocket > logs/websocket.log 2>&1 &
WEBSOCKET_PID=$!
echo "  PID: $WEBSOCKET_PID"
echo "  Logs: logs/websocket.log"

echo ""
echo "=============================================================================="
echo "Services Started"
echo "=============================================================================="
echo ""
echo "FastAPI Server:"
echo "  URL: http://localhost:8000"
echo "  Docs: http://localhost:8000/docs"
echo "  PID: $FASTAPI_PID"
echo ""
echo "WebSocket Server:"
echo "  URL: ws://localhost:8765"
echo "  PID: $WEBSOCKET_PID"
echo ""
echo "Logs:"
echo "  tail -f logs/fastapi.log"
echo "  tail -f logs/websocket.log"
echo ""
echo "Stop services:"
echo "  kill $FASTAPI_PID $WEBSOCKET_PID"
echo "  or: pkill -f 'infrastructure.api'"
echo "  or: pkill -f 'infrastructure.websocket'"
echo ""
echo "Test endpoints:"
echo "  ./scripts/test_nasa_api.sh"
echo "  pytest tests/test_nasa_api_smoke.py -v"
echo ""
echo "=============================================================================="
echo ""

# Save PIDs to file
echo "$FASTAPI_PID" > logs/fastapi.pid
echo "$WEBSOCKET_PID" > logs/websocket.pid

echo "PIDs saved to logs/*.pid"
echo ""
echo "Press Ctrl+C to stop monitoring logs (servers will keep running)"
echo ""

# Monitor logs
tail -f logs/fastapi.log logs/websocket.log
