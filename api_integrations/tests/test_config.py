"""Tests for API configuration."""

import os
import pytest
from api_integrations.config import APIConfig


def test_default_config():
    """Test default configuration values."""
    config = APIConfig()

    assert config.nasa_api_key == "DEMO_KEY"
    assert config.nasa_base_url == "https://api.nasa.gov"
    assert config.spacex_base_url == "https://api.spacexdata.com/v4"
    assert config.timeout == 30
    assert config.retry_attempts == 3


def test_custom_config():
    """Test custom configuration values."""
    config = APIConfig(
        nasa_api_key="custom_key",
        timeout=60,
        retry_attempts=5
    )

    assert config.nasa_api_key == "custom_key"
    assert config.timeout == 60
    assert config.retry_attempts == 5


def test_config_from_env(monkeypatch):
    """Test configuration from environment variables."""
    monkeypatch.setenv("NASA_API_KEY", "env_key")
    monkeypatch.setenv("API_TIMEOUT", "45")
    monkeypatch.setenv("API_RETRY_ATTEMPTS", "4")

    config = APIConfig.from_env()

    assert config.nasa_api_key == "env_key"
    assert config.timeout == 45
    assert config.retry_attempts == 4
