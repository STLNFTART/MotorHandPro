"""Tests for base API client."""

import pytest
from api_integrations.base_client import BaseAPIClient, APIError, RateLimitError
from api_integrations.config import APIConfig


def test_base_client_initialization():
    """Test base client initialization."""
    client = BaseAPIClient()
    assert client is not None
    assert client.config is not None
    assert client.session is not None


def test_base_client_custom_config():
    """Test base client with custom config."""
    config = APIConfig(timeout=45)
    client = BaseAPIClient(config)
    assert client.config.timeout == 45


def test_session_creation():
    """Test that session is created with retry logic."""
    client = BaseAPIClient()
    assert client.session is not None
    assert hasattr(client.session, 'request')


def test_rate_limit_wait():
    """Test rate limiting logic."""
    config = APIConfig(rate_limit_enabled=True, requests_per_hour=3600)
    client = BaseAPIClient(config)

    # Should not raise exception
    client._rate_limit_wait()


def test_rate_limit_disabled():
    """Test with rate limiting disabled."""
    config = APIConfig(rate_limit_enabled=False)
    client = BaseAPIClient(config)

    # Should not raise exception and should be instant
    client._rate_limit_wait()
