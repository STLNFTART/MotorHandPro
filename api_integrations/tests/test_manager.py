"""Tests for the unified API manager."""

import pytest
from api_integrations import APIManager
from api_integrations.config import APIConfig


def test_manager_initialization():
    """Test that the manager initializes correctly."""
    manager = APIManager()
    assert manager is not None
    assert manager.config is not None


def test_manager_with_custom_config():
    """Test manager with custom configuration."""
    config = APIConfig(nasa_api_key="test_key")
    manager = APIManager(config=config)
    assert manager.config.nasa_api_key == "test_key"


def test_nasa_client_properties():
    """Test that NASA client properties are accessible."""
    manager = APIManager()

    # Test lazy loading
    assert manager._nasa_apod is None
    apod = manager.nasa_apod
    assert apod is not None
    assert manager._nasa_apod is not None

    # Test all NASA clients
    assert manager.nasa_apod is not None
    assert manager.nasa_neows is not None
    assert manager.nasa_epic is not None
    assert manager.nasa_power is not None
    assert manager.nasa_images is not None
    assert manager.nasa_ssd is not None


def test_spacex_client_properties():
    """Test that SpaceX client properties are accessible."""
    manager = APIManager()

    # Test lazy loading
    assert manager._spacex is None
    spacex = manager.spacex
    assert spacex is not None
    assert manager._spacex is not None

    # Test all SpaceX clients
    assert manager.spacex is not None
    assert manager.starlink is not None


def test_get_all_nasa_clients():
    """Test getting all NASA clients."""
    manager = APIManager()
    clients = manager.get_all_nasa_clients()

    assert "apod" in clients
    assert "neows" in clients
    assert "epic" in clients
    assert "power" in clients
    assert "images" in clients
    assert "ssd" in clients


def test_get_all_spacex_clients():
    """Test getting all SpaceX clients."""
    manager = APIManager()
    clients = manager.get_all_spacex_clients()

    assert "spacex" in clients
    assert "starlink" in clients


def test_manager_repr():
    """Test manager string representation."""
    manager = APIManager()
    repr_str = repr(manager)
    assert "APIManager" in repr_str
    assert "NASA" in repr_str
    assert "SpaceX" in repr_str
