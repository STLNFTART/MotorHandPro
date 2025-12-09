#!/bin/bash
# Manual test script for NASA API endpoints
#
# Usage:
#   ./test_nasa_api.sh
#
# Environment variables:
#   NASA_API_BASE - Base URL for API (default: http://localhost:8000)
#   NASA_API_TOKEN - JWT token for authentication

set -e

BASE="${NASA_API_BASE:-http://localhost:8000}"
TOKEN="${NASA_API_TOKEN:-dev-token}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=============================================================================="
echo "NASA API Manual Test Script"
echo "=============================================================================="
echo ""
echo "Base URL: $BASE"
echo "Token: ${TOKEN:0:20}..."
echo ""

# Check if server is running
echo "Checking if server is running..."
if ! curl -sf "$BASE/health" > /dev/null 2>&1; then
    echo -e "${RED}❌ Server not running at $BASE${NC}"
    echo ""
    echo "Start the server first:"
    echo "  python -m infrastructure.api"
    exit 1
fi
echo -e "${GREEN}✅ Server is running${NC}"
echo ""

# Test 1: Status endpoint (no auth required)
echo "=============================================================================="
echo "Test 1: GET /nasa/status"
echo "=============================================================================="
echo ""
curl -s -X GET "$BASE/nasa/status" | jq '.'
echo ""

# Test 2: Fetch simulated observations
echo "=============================================================================="
echo "Test 2: POST /nasa/comet/fetch (simulated data)"
echo "=============================================================================="
echo ""

START_TIME=$(date -u -d "2025-01-01" +"%Y-%m-%dT%H:%M:%S" 2>/dev/null || date -u -j -f "%Y-%m-%d" "2025-01-01" +"%Y-%m-%dT%H:%M:%S")
END_TIME=$(date -u -d "2025-01-02" +"%Y-%m-%dT%H:%M:%S" 2>/dev/null || date -u -j -f "%Y-%m-%d" "2025-01-02" +"%Y-%m-%dT%H:%M:%S")

echo "Request:"
cat <<EOF | jq '.'
{
  "data_source": "simulated",
  "start_time": "$START_TIME",
  "end_time": "$END_TIME",
  "step": "2h"
}
EOF
echo ""

FETCH_RESPONSE=$(curl -s -X POST "$BASE/nasa/comet/fetch" \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "data_source": "simulated",
    "start_time": "'"$START_TIME"'",
    "end_time": "'"$END_TIME"'",
    "step": "2h"
  }')

echo "Response:"
echo "$FETCH_RESPONSE" | jq '.'
echo ""

# Check if fetch was successful
FETCH_STATUS=$(echo "$FETCH_RESPONSE" | jq -r '.status')
if [ "$FETCH_STATUS" = "ok" ]; then
    echo -e "${GREEN}✅ Fetch successful${NC}"
    OBS_COUNT=$(echo "$FETCH_RESPONSE" | jq -r '.count')
    echo "Observations fetched: $OBS_COUNT"
else
    echo -e "${RED}❌ Fetch failed${NC}"
fi
echo ""

# Test 3: Get observations
echo "=============================================================================="
echo "Test 3: GET /nasa/comet/observations"
echo "=============================================================================="
echo ""

OBS_RESPONSE=$(curl -s -X GET "$BASE/nasa/comet/observations?limit=5" \
  -H "Authorization: Bearer $TOKEN")

echo "Response (first 5 observations):"
echo "$OBS_RESPONSE" | jq '.'
echo ""

OBS_RETRIEVED=$(echo "$OBS_RESPONSE" | jq -r '.observations | length')
echo -e "${GREEN}✅ Retrieved $OBS_RETRIEVED observations${NC}"
echo ""

# Test 4: Process observations
echo "=============================================================================="
echo "Test 4: POST /nasa/comet/process"
echo "=============================================================================="
echo ""

PROCESS_RESPONSE=$(curl -s -X POST "$BASE/nasa/comet/process" \
  -H "Authorization: Bearer $TOKEN")

echo "Response:"
echo "$PROCESS_RESPONSE" | jq '.'
echo ""

PROCESS_STATUS=$(echo "$PROCESS_RESPONSE" | jq -r '.status')
if [ "$PROCESS_STATUS" = "ok" ]; then
    echo -e "${GREEN}✅ Processing successful${NC}"
    PROCESSED_COUNT=$(echo "$PROCESS_RESPONSE" | jq -r '.count')
    echo "States processed: $PROCESSED_COUNT"
else
    echo -e "${YELLOW}⚠️  No data to process or processing skipped${NC}"
fi
echo ""

# Test 5: Get processed states
echo "=============================================================================="
echo "Test 5: GET /nasa/comet/processed"
echo "=============================================================================="
echo ""

PROCESSED_RESPONSE=$(curl -s -X GET "$BASE/nasa/comet/processed?limit=3" \
  -H "Authorization: Bearer $TOKEN")

echo "Response (first 3 processed states):"
echo "$PROCESSED_RESPONSE" | jq '.'
echo ""

STATES_RETRIEVED=$(echo "$PROCESSED_RESPONSE" | jq -r '.processed_states | length')
echo -e "${GREEN}✅ Retrieved $STATES_RETRIEVED processed states${NC}"
echo ""

# Summary
echo "=============================================================================="
echo "Test Summary"
echo "=============================================================================="
echo ""
echo -e "${GREEN}✅ All tests completed${NC}"
echo ""
echo "Endpoints tested:"
echo "  1. GET  /nasa/status"
echo "  2. POST /nasa/comet/fetch"
echo "  3. GET  /nasa/comet/observations"
echo "  4. POST /nasa/comet/process"
echo "  5. GET  /nasa/comet/processed"
echo ""
echo "Next steps:"
echo "  - View API docs: $BASE/docs"
echo "  - Run pytest tests: pytest tests/test_nasa_api_smoke.py -v"
echo "  - Monitor MQTT: mosquitto_sub -t 'motorhand/nasa/comet/#'"
echo ""
echo "=============================================================================="
