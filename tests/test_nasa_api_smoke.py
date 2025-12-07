#!/usr/bin/env python3
"""
NASA API Smoke Tests

Run these tests to verify NASA data API endpoints are working.

Prerequisites:
    - FastAPI server running on http://localhost:8000
    - Database with nasa_data schema created
    - Valid JWT token (or dev token)

Run:
    pytest tests/test_nasa_api_smoke.py -v

Or with specific token:
    NASA_API_TOKEN="your_token" pytest tests/test_nasa_api_smoke.py -v
"""

import os
import requests
import pytest
from datetime import datetime, timedelta

# Configuration
BASE_URL = os.getenv("NASA_API_BASE", "http://localhost:8000")
TOKEN = os.getenv("NASA_API_TOKEN", "dev-token")
HEADERS = {"Authorization": f"Bearer {TOKEN}"}

# Skip tests if server is not running
def check_server():
    """Check if API server is running"""
    try:
        r = requests.get(f"{BASE_URL}/health", timeout=2)
        return r.status_code == 200
    except:
        return False

pytestmark = pytest.mark.skipif(
    not check_server(),
    reason="API server not running at " + BASE_URL
)


class TestNASAStatus:
    """Test NASA status endpoint"""

    def test_nasa_status_endpoint_exists(self):
        """Verify /nasa/status endpoint is accessible"""
        r = requests.get(f"{BASE_URL}/nasa/status", timeout=10)
        assert r.status_code == 200

    def test_nasa_status_returns_valid_data(self):
        """Verify status endpoint returns expected structure"""
        r = requests.get(f"{BASE_URL}/nasa/status", timeout=10)
        r.raise_for_status()
        data = r.json()

        assert "status" in data
        assert data["status"] in ["available", "unavailable"]
        assert "timestamp" in data


class TestNASACometFetch:
    """Test NASA comet data fetching"""

    def test_fetch_simulated_data(self):
        """Test fetching simulated comet observations"""
        start = datetime.now().isoformat()
        end = (datetime.now() + timedelta(hours=24)).isoformat()

        r = requests.post(
            f"{BASE_URL}/nasa/comet/fetch",
            headers={**HEADERS, "Content-Type": "application/json"},
            json={
                "data_source": "simulated",
                "start_time": start,
                "end_time": end,
                "step": "1h"
            },
            timeout=30,
        )

        # If authentication fails, skip this test
        if r.status_code == 401:
            pytest.skip("Authentication required - set NASA_API_TOKEN environment variable")

        r.raise_for_status()
        data = r.json()

        assert data["status"] == "ok"
        assert data["data_source"] == "simulated"
        assert "observations" in data
        assert len(data["observations"]) > 0

    def test_fetch_invalid_source_returns_400(self):
        """Test that invalid data source returns 400"""
        r = requests.post(
            f"{BASE_URL}/nasa/comet/fetch",
            headers={**HEADERS, "Content-Type": "application/json"},
            json={
                "data_source": "invalid_source",
            },
            timeout=10,
        )

        if r.status_code == 401:
            pytest.skip("Authentication required")

        assert r.status_code == 400


class TestNASACometObservations:
    """Test NASA comet observation queries"""

    def test_get_observations_endpoint(self):
        """Test getting stored observations"""
        r = requests.get(
            f"{BASE_URL}/nasa/comet/observations",
            params={"limit": 10},
            headers=HEADERS,
            timeout=10,
        )

        if r.status_code == 401:
            pytest.skip("Authentication required")

        r.raise_for_status()
        data = r.json()

        assert "status" in data
        assert "observations" in data
        assert isinstance(data["observations"], list)

    def test_get_observations_with_filter(self):
        """Test filtering observations by data source"""
        r = requests.get(
            f"{BASE_URL}/nasa/comet/observations",
            params={"limit": 5, "data_source": "simulated"},
            headers=HEADERS,
            timeout=10,
        )

        if r.status_code == 401:
            pytest.skip("Authentication required")

        r.raise_for_status()
        data = r.json()

        assert "observations" in data
        # If there are observations, they should all be from simulated source
        for obs in data["observations"]:
            if "data_source" in obs:
                assert obs["data_source"] == "simulated"


class TestNASACometProcessing:
    """Test NASA comet data processing through PRIMAL operator"""

    def test_process_endpoint_exists(self):
        """Test that process endpoint is accessible"""
        r = requests.post(
            f"{BASE_URL}/nasa/comet/process",
            headers=HEADERS,
            timeout=10,
        )

        if r.status_code == 401:
            pytest.skip("Authentication required")

        # Should return 200 even if no data to process
        assert r.status_code == 200

    def test_get_processed_states(self):
        """Test getting processed states"""
        r = requests.get(
            f"{BASE_URL}/nasa/comet/processed",
            params={"limit": 10},
            headers=HEADERS,
            timeout=10,
        )

        if r.status_code == 401:
            pytest.skip("Authentication required")

        r.raise_for_status()
        data = r.json()

        assert "status" in data
        assert "processed_states" in data
        assert isinstance(data["processed_states"], list)


class TestNASAEndToEnd:
    """End-to-end test of NASA data pipeline"""

    def test_full_pipeline(self):
        """Test complete pipeline: fetch -> store -> process -> retrieve"""

        # 1. Fetch simulated data
        start = datetime.now().isoformat()
        end = (datetime.now() + timedelta(hours=6)).isoformat()

        r1 = requests.post(
            f"{BASE_URL}/nasa/comet/fetch",
            headers={**HEADERS, "Content-Type": "application/json"},
            json={
                "data_source": "simulated",
                "start_time": start,
                "end_time": end,
                "step": "2h"
            },
            timeout=30,
        )

        if r1.status_code == 401:
            pytest.skip("Authentication required")

        r1.raise_for_status()
        fetch_data = r1.json()
        assert fetch_data["status"] == "ok"
        obs_count = fetch_data["count"]
        assert obs_count > 0

        # 2. Verify observations were stored
        r2 = requests.get(
            f"{BASE_URL}/nasa/comet/observations",
            params={"limit": 100, "data_source": "simulated"},
            headers=HEADERS,
            timeout=10,
        )
        r2.raise_for_status()
        obs_data = r2.json()
        assert len(obs_data["observations"]) >= obs_count

        # 3. Process observations
        r3 = requests.post(
            f"{BASE_URL}/nasa/comet/process",
            headers=HEADERS,
            timeout=30,
        )
        r3.raise_for_status()
        process_data = r3.json()
        assert process_data["status"] == "ok"

        # 4. Retrieve processed states
        r4 = requests.get(
            f"{BASE_URL}/nasa/comet/processed",
            params={"limit": 10},
            headers=HEADERS,
            timeout=10,
        )
        r4.raise_for_status()
        processed_data = r4.json()
        assert "processed_states" in processed_data


if __name__ == "__main__":
    """Run tests directly"""
    import sys

    print("=" * 80)
    print("NASA API Smoke Tests")
    print("=" * 80)
    print(f"Base URL: {BASE_URL}")
    print(f"Token: {'Set' if TOKEN != 'dev-token' else 'Using default dev-token'}")
    print()

    # Check if server is running
    if not check_server():
        print(f"❌ Server not running at {BASE_URL}")
        print()
        print("Start the server first:")
        print("  python -m infrastructure.api")
        sys.exit(1)

    print(f"✅ Server is running at {BASE_URL}")
    print()
    print("Running tests with pytest...")
    print("=" * 80)
    print()

    pytest.main([__file__, "-v", "--tb=short"])
