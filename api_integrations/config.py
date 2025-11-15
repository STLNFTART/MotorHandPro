"""
Configuration module for NASA and SpaceX API integrations.

Handles API keys, endpoints, and client settings.
"""

import os
from typing import Optional
from dataclasses import dataclass


@dataclass
class APIConfig:
    """Configuration for API integrations."""

    # NASA API Configuration
    nasa_api_key: str = "DEMO_KEY"  # Get your key from https://api.nasa.gov
    nasa_base_url: str = "https://api.nasa.gov"

    # SpaceX API Configuration
    spacex_base_url: str = "https://api.spacexdata.com/v4"
    starlink_metrics_url: str = "https://api.starlink.com/public-files"

    # Request settings
    timeout: int = 30  # seconds
    retry_attempts: int = 3
    retry_delay: float = 1.0  # seconds

    # Rate limiting
    rate_limit_enabled: bool = True
    requests_per_hour: int = 1000

    @classmethod
    def from_env(cls) -> "APIConfig":
        """
        Create configuration from environment variables.

        Environment variables:
            NASA_API_KEY: Your NASA API key
            NASA_BASE_URL: Override default NASA base URL
            SPACEX_BASE_URL: Override default SpaceX base URL
            API_TIMEOUT: Request timeout in seconds
            API_RETRY_ATTEMPTS: Number of retry attempts

        Returns:
            APIConfig instance with values from environment
        """
        return cls(
            nasa_api_key=os.getenv("NASA_API_KEY", "DEMO_KEY"),
            nasa_base_url=os.getenv("NASA_BASE_URL", "https://api.nasa.gov"),
            spacex_base_url=os.getenv("SPACEX_BASE_URL", "https://api.spacexdata.com/v4"),
            timeout=int(os.getenv("API_TIMEOUT", "30")),
            retry_attempts=int(os.getenv("API_RETRY_ATTEMPTS", "3"))
        )


# Global configuration instance
config = APIConfig.from_env()
