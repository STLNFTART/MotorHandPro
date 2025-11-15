"""
NASA Near Earth Object Web Service (NeoWs) API Client.

Provides access to near Earth asteroid information.
API Documentation: https://api.nasa.gov/#neows
"""

from typing import Optional, Dict, Any
from datetime import date, timedelta
from ..base_client import BaseAPIClient, APIConfig


class NeoWsClient(BaseAPIClient):
    """Client for NASA NeoWs API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize NeoWs client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = f"{self.config.nasa_base_url}/neo/rest/v1"

    def get_feed(
        self,
        start_date: Optional[date] = None,
        end_date: Optional[date] = None
    ) -> Dict[str, Any]:
        """
        Get near Earth objects feed for a date range.

        Args:
            start_date: Start date (defaults to today)
            end_date: End date (defaults to 7 days from start_date)

        Returns:
            Dict containing:
                - element_count: Total number of NEOs
                - near_earth_objects: Dict of NEOs by date
                - links: Navigation links
        """
        if start_date is None:
            start_date = date.today()

        if end_date is None:
            end_date = start_date + timedelta(days=7)

        params = {
            "api_key": self.config.nasa_api_key,
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d")
        }

        return self.get(f"{self.base_endpoint}/feed", params=params)

    def get_by_id(self, asteroid_id: str) -> Dict[str, Any]:
        """
        Get detailed information about a specific asteroid.

        Args:
            asteroid_id: NASA JPL small body database ID

        Returns:
            Dict with detailed asteroid information including:
                - name: Asteroid name
                - nasa_jpl_url: Link to JPL database
                - is_potentially_hazardous_asteroid: Hazard flag
                - close_approach_data: Historical close approaches
                - orbital_data: Orbital parameters
        """
        params = {"api_key": self.config.nasa_api_key}
        return self.get(f"{self.base_endpoint}/neo/{asteroid_id}", params=params)

    def browse(self, page: int = 0, size: int = 20) -> Dict[str, Any]:
        """
        Browse all near Earth objects.

        Args:
            page: Page number (0-indexed)
            size: Number of results per page (max 100)

        Returns:
            Dict containing paginated NEO data
        """
        params = {
            "api_key": self.config.nasa_api_key,
            "page": page,
            "size": min(size, 100)
        }

        return self.get(f"{self.base_endpoint}/neo/browse", params=params)

    def get_statistics(self) -> Dict[str, Any]:
        """
        Get Near Earth Object statistics.

        Returns:
            Dict containing:
                - near_earth_object_count: Total count
                - close_approach_count: Total close approaches
                - last_updated: Last update timestamp
                - nasa_jpl_url: JPL database URL
        """
        params = {"api_key": self.config.nasa_api_key}
        return self.get(f"{self.base_endpoint}/stats", params=params)
