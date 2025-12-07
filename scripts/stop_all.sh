#!/bin/bash
# Stop all NASA API services
#
# Usage:
#   ./stop_all.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

cd "$REPO_ROOT"

echo "=============================================================================="
echo "Stopping MotorHandPro NASA API Services"
echo "=============================================================================="
echo ""

# Stop by PID files
if [ -f logs/fastapi.pid ]; then
    FASTAPI_PID=$(cat logs/fastapi.pid)
    if kill -0 $FASTAPI_PID 2>/dev/null; then
        echo "Stopping FastAPI server (PID: $FASTAPI_PID)..."
        kill $FASTAPI_PID
        echo -e "${GREEN}✅ FastAPI stopped${NC}"
    else
        echo "FastAPI server not running (PID: $FASTAPI_PID)"
    fi
    rm logs/fastapi.pid
fi

if [ -f logs/websocket.pid ]; then
    WEBSOCKET_PID=$(cat logs/websocket.pid)
    if kill -0 $WEBSOCKET_PID 2>/dev/null; then
        echo "Stopping WebSocket server (PID: $WEBSOCKET_PID)..."
        kill $WEBSOCKET_PID
        echo -e "${GREEN}✅ WebSocket stopped${NC}"
    else
        echo "WebSocket server not running (PID: $WEBSOCKET_PID)"
    fi
    rm logs/websocket.pid
fi

# Fallback: kill by process name
echo ""
echo "Checking for any remaining processes..."

if pkill -f 'infrastructure.api' 2>/dev/null; then
    echo "Killed remaining FastAPI processes"
fi

if pkill -f 'infrastructure.websocket' 2>/dev/null; then
    echo "Killed remaining WebSocket processes"
fi

echo ""
echo -e "${GREEN}All services stopped${NC}"
echo ""
