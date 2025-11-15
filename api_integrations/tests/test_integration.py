"""
Integration tests for API clients.

These tests make actual API calls and should be run sparingly.
Use pytest markers: pytest -m integration
"""

import pytest
from datetime import date
from api_integrations import APIManager
from api_integrations.base_client import APIError


@pytest.fixture
def manager():
    """Create API manager for tests."""
    return APIManager()


@pytest.mark.integration
def test_nasa_apod_get_today(manager):
    """Test getting today's APOD."""
    try:
        apod = manager.nasa_apod.get_today()
        assert 'title' in apod
        assert 'url' in apod
        assert 'date' in apod
    except APIError as e:
        pytest.skip(f"API error: {e}")


@pytest.mark.integration
def test_nasa_neows_statistics(manager):
    """Test getting NEO statistics."""
    try:
        stats = manager.nasa_neows.get_statistics()
        assert 'near_earth_object_count' in stats
        assert 'close_approach_count' in stats
    except APIError as e:
        pytest.skip(f"API error: {e}")


@pytest.mark.integration
def test_spacex_company_info(manager):
    """Test getting SpaceX company info."""
    try:
        company = manager.spacex.get_company_info()
        assert 'name' in company
        assert 'founded' in company
        assert 'employees' in company
    except APIError as e:
        pytest.skip(f"API error: {e}")


@pytest.mark.integration
def test_spacex_latest_launch(manager):
    """Test getting latest SpaceX launch."""
    try:
        launch = manager.spacex.get_latest_launch()
        assert 'name' in launch
        assert 'date_utc' in launch
    except APIError as e:
        pytest.skip(f"API error: {e}")


@pytest.mark.integration
def test_health_check(manager):
    """Test API health check."""
    status = manager.health_check()
    assert isinstance(status, dict)
    assert len(status) > 0
