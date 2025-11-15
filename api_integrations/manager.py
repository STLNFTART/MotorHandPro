"""
Unified API Manager for all NASA and SpaceX/Starlink integrations.

Provides a single entry point to access all API clients.
"""

from typing import Optional
from .config import APIConfig
from .nasa import (
    APODClient,
    NeoWsClient,
    EPICClient,
    POWERClient,
    ImageLibraryClient,
    SSDClient
)
from .spacex import SpaceXClient, StarlinkMetricsClient


class APIManager:
    """
    Unified manager for all NASA and SpaceX API clients.

    Example usage:
        >>> manager = APIManager()
        >>> apod = manager.nasa.apod.get_today()
        >>> launches = manager.spacex.get_latest_launch()
        >>> starlink = manager.starlink.get_residential_metrics()
    """

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize API manager with all clients.

        Args:
            config: API configuration (uses global config if None)
        """
        from .config import config as default_config
        self.config = config or default_config

        # Initialize NASA clients
        self._nasa_apod = None
        self._nasa_neows = None
        self._nasa_epic = None
        self._nasa_power = None
        self._nasa_image_library = None
        self._nasa_ssd = None

        # Initialize SpaceX clients
        self._spacex = None
        self._starlink_metrics = None

    # ========== NASA Clients (Lazy Loading) ==========

    @property
    def nasa_apod(self) -> APODClient:
        """Get NASA APOD (Astronomy Picture of the Day) client."""
        if self._nasa_apod is None:
            self._nasa_apod = APODClient(self.config)
        return self._nasa_apod

    @property
    def nasa_neows(self) -> NeoWsClient:
        """Get NASA NeoWs (Near Earth Object Web Service) client."""
        if self._nasa_neows is None:
            self._nasa_neows = NeoWsClient(self.config)
        return self._nasa_neows

    @property
    def nasa_epic(self) -> EPICClient:
        """Get NASA EPIC (Earth Polychromatic Imaging Camera) client."""
        if self._nasa_epic is None:
            self._nasa_epic = EPICClient(self.config)
        return self._nasa_epic

    @property
    def nasa_power(self) -> POWERClient:
        """Get NASA POWER (weather and climate data) client."""
        if self._nasa_power is None:
            self._nasa_power = POWERClient(self.config)
        return self._nasa_power

    @property
    def nasa_images(self) -> ImageLibraryClient:
        """Get NASA Image and Video Library client."""
        if self._nasa_image_library is None:
            self._nasa_image_library = ImageLibraryClient(self.config)
        return self._nasa_image_library

    @property
    def nasa_ssd(self) -> SSDClient:
        """Get NASA Solar System Dynamics client."""
        if self._nasa_ssd is None:
            self._nasa_ssd = SSDClient(self.config)
        return self._nasa_ssd

    # ========== SpaceX Clients (Lazy Loading) ==========

    @property
    def spacex(self) -> SpaceXClient:
        """Get SpaceX API client."""
        if self._spacex is None:
            self._spacex = SpaceXClient(self.config)
        return self._spacex

    @property
    def starlink(self) -> StarlinkMetricsClient:
        """Get Starlink public metrics client."""
        if self._starlink_metrics is None:
            self._starlink_metrics = StarlinkMetricsClient(self.config)
        return self._starlink_metrics

    # ========== Convenience Methods ==========

    def get_all_nasa_clients(self) -> dict:
        """
        Get dictionary of all NASA clients.

        Returns:
            Dict with client names as keys and client instances as values
        """
        return {
            "apod": self.nasa_apod,
            "neows": self.nasa_neows,
            "epic": self.nasa_epic,
            "power": self.nasa_power,
            "images": self.nasa_images,
            "ssd": self.nasa_ssd
        }

    def get_all_spacex_clients(self) -> dict:
        """
        Get dictionary of all SpaceX/Starlink clients.

        Returns:
            Dict with client names as keys and client instances as values
        """
        return {
            "spacex": self.spacex,
            "starlink": self.starlink
        }

    def health_check(self) -> dict:
        """
        Check availability of all APIs.

        Returns:
            Dict with API status for each service
        """
        status = {}

        # Check NASA APIs
        try:
            self.nasa_apod.get_today()
            status["nasa_apod"] = "OK"
        except Exception as e:
            status["nasa_apod"] = f"ERROR: {e}"

        try:
            self.nasa_neows.get_statistics()
            status["nasa_neows"] = "OK"
        except Exception as e:
            status["nasa_neows"] = f"ERROR: {e}"

        # Check SpaceX API
        try:
            self.spacex.get_company_info()
            status["spacex"] = "OK"
        except Exception as e:
            status["spacex"] = f"ERROR: {e}"

        # Check Starlink metrics
        try:
            self.starlink.get_residential_metrics()
            status["starlink"] = "OK"
        except Exception as e:
            status["starlink"] = f"ERROR: {e}"

        return status

    def __repr__(self) -> str:
        """String representation of API manager."""
        return (
            f"APIManager(\n"
            f"  NASA APIs: APOD, NeoWs, EPIC, POWER, Images, SSD\n"
            f"  SpaceX APIs: SpaceX, Starlink Metrics\n"
            f"  Config: {self.config}\n"
            f")"
        )
