#!/usr/bin/env python3
"""Test real NASA data fetch with provided API key"""

import requests
import json
from datetime import datetime

# NASA API credentials
NASA_API_KEY = "7S8ltlPgaI1CiAd7OuVRflyt6dprnMfhdgeX7EWW"
ACCOUNT_ID = "b4d4cb37-9b50-409b-a604-213053ef0537"

# API endpoint
BASE_URL = "http://localhost:8000"

print("=" * 80)
print("Testing Real NASA Data Fetch")
print("=" * 80)
print()

# Test 1: Fetch from JPL Horizons
print("Test 1: Fetching real JPL Horizons data...")
response = requests.post(
    f"{BASE_URL}/nasa/comet/fetch",
    headers={
        "Authorization": "Bearer dev-token",
        "Content-Type": "application/json"
    },
    json={
        "data_source": "horizons",
        "start_time": "2025-12-08T00:00:00",
        "end_time": "2025-12-08T12:00:00",
        "step": "2h"
    }
)

print(f"Status: {response.status_code}")
print()

if response.status_code == 200:
    data = response.json()
    print(f"✅ SUCCESS!")
    print(f"Data source: {data.get('data_source')}")
    print(f"Observations: {data.get('count')}")
    print()

    if data.get('observations'):
        print("First observation:")
        obs = data['observations'][0]
        print(json.dumps(obs, indent=2))
        print()

        # Check timestamp
        timestamp = obs.get('timestamp')
        print(f"Timestamp: {timestamp}")
        print(f"Current time: {datetime.now().isoformat()}")
else:
    print(f"❌ FAILED")
    print(response.text)

print()
print("=" * 80)
