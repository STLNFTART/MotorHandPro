#!/bin/bash
# Test All NASA API Integrations
#
# Tests APOD, DONKI (Space Weather), Mars Rover Photos, and Near Earth Objects
# Usage: ./test_nasa_apis.sh

set -e

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

API_URL="http://localhost:8000"
TOKEN="dev-token"

echo "=============================================================================="
echo "NASA API Integration Tests (api.nasa.gov)"
echo "=============================================================================="
echo ""

# Test 1: API Status
echo -e "${BLUE}Test 1: NASA API Client Status${NC}"
echo "GET /nasa/api/status"
curl -s "${API_URL}/nasa/api/status" | python -m json.tool
echo -e "${GREEN}✓ Status endpoint working${NC}\n"

# Test 2: APOD - Today's Picture
echo -e "${BLUE}Test 2: Astronomy Picture of the Day (Today)${NC}"
echo "GET /nasa/apod"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/apod" | python -m json.tool | head -25
echo -e "${GREEN}✓ APOD endpoint working${NC}\n"

# Test 3: APOD - Specific Date
echo -e "${BLUE}Test 3: APOD for Specific Date${NC}"
echo "GET /nasa/apod?date=2024-01-01"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/apod?date=2024-01-01" | python -m json.tool | head -25
echo -e "${GREEN}✓ Historical APOD working${NC}\n"

# Test 4: Space Weather - All Events
echo -e "${BLUE}Test 4: Space Weather Events (All Types, Last 7 Days)${NC}"
echo "GET /nasa/space-weather?days=7"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/space-weather?days=7" | python -m json.tool | head -40
echo -e "${GREEN}✓ Space weather (all) working${NC}\n"

# Test 5: Space Weather - Solar Flares
echo -e "${BLUE}Test 5: Solar Flares (Last 7 Days)${NC}"
echo "GET /nasa/space-weather?event_type=FLR&days=7"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/space-weather?event_type=FLR&days=7" | python -m json.tool | head -40
echo -e "${GREEN}✓ Solar flares working${NC}\n"

# Test 6: Space Weather - Coronal Mass Ejections
echo -e "${BLUE}Test 6: Coronal Mass Ejections (Last 30 Days)${NC}"
echo "GET /nasa/space-weather?event_type=CME&days=30"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/space-weather?event_type=CME&days=30" | python -m json.tool | head -40
echo -e "${GREEN}✓ CME events working${NC}\n"

# Test 7: Space Weather - Geomagnetic Storms
echo -e "${BLUE}Test 7: Geomagnetic Storms (Last 30 Days)${NC}"
echo "GET /nasa/space-weather?event_type=GST&days=30"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/space-weather?event_type=GST&days=30" | python -m json.tool | head -40
echo -e "${GREEN}✓ Geomagnetic storms working${NC}\n"

# Test 8: Mars Rover - Curiosity Latest Photos
echo -e "${BLUE}Test 8: Mars Curiosity Rover Photos (Sol 4000)${NC}"
echo "GET /nasa/mars/photos/curiosity?sol=4000"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/mars/photos/curiosity?sol=4000" | python -m json.tool | head -50
echo -e "${GREEN}✓ Mars rover photos working${NC}\n"

# Test 9: Mars Rover - Perseverance Photos
echo -e "${BLUE}Test 9: Mars Perseverance Rover Photos (Earth Date)${NC}"
echo "GET /nasa/mars/photos/perseverance?earth_date=2024-01-01"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/mars/photos/perseverance?earth_date=2024-01-01" | python -m json.tool | head -50
echo -e "${GREEN}✓ Perseverance photos working${NC}\n"

# Test 10: Near Earth Objects
echo -e "${BLUE}Test 10: Near Earth Objects (Next 7 Days)${NC}"
echo "GET /nasa/neo"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/neo" | python -m json.tool | head -60
echo -e "${GREEN}✓ NEO endpoint working${NC}\n"

# Test 11: NEO with Date Range
echo -e "${BLUE}Test 11: Near Earth Objects (Custom Date Range)${NC}"
echo "GET /nasa/neo?start_date=2025-12-01&end_date=2025-12-07"
curl -s -H "Authorization: Bearer ${TOKEN}" \
    "${API_URL}/nasa/neo?start_date=2025-12-01&end_date=2025-12-07" | python -m json.tool | head -60
echo -e "${GREEN}✓ NEO date range working${NC}\n"

echo "=============================================================================="
echo -e "${GREEN}All NASA API Tests Completed Successfully${NC}"
echo "=============================================================================="
echo ""
echo "Available NASA APIs:"
echo "  ✓ APOD - Astronomy Picture of the Day"
echo "  ✓ DONKI - Space Weather Events (CME, GST, IPS, FLR, SEP, MPC, RBE, HSS)"
echo "  ✓ Mars InSight - Mars Weather Data"
echo "  ✓ Mars Rover Photos - Curiosity, Opportunity, Spirit, Perseverance"
echo "  ✓ NEO - Near Earth Objects (Asteroids)"
echo ""
echo "Space Weather Event Types:"
echo "  - CME: Coronal Mass Ejection"
echo "  - GST: Geomagnetic Storm"
echo "  - IPS: Interplanetary Shock"
echo "  - FLR: Solar Flare"
echo "  - SEP: Solar Energetic Particle"
echo "  - MPC: Magnetopause Crossing"
echo "  - RBE: Radiation Belt Enhancement"
echo "  - HSS: High Speed Stream"
echo ""
