"""
NASA Astronomy Picture of the Day (APOD) API Client.

Provides access to NASA's daily astronomy images and videos.
API Documentation: https://api.nasa.gov/#apod
"""

from typing import Optional, Dict, Any, List
from datetime import date, datetime
from ..base_client import BaseAPIClient, APIConfig


class APODClient(BaseAPIClient):
    """Client for NASA APOD API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize APOD client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.endpoint = f"{self.config.nasa_base_url}/planetary/apod"

    def get_today(self) -> Dict[str, Any]:
        """
        Get today's astronomy picture.

        Returns:
            Dict containing:
                - date: Date of the image
                - explanation: Description of the image
                - title: Image title
                - url: URL of the image
                - media_type: Type of media (image/video)
                - hdurl: High-resolution image URL (if available)
                - copyright: Copyright information (if applicable)
        """
        params = {"api_key": self.config.nasa_api_key}
        return self.get(self.endpoint, params=params)

    def get_by_date(self, target_date: date) -> Dict[str, Any]:
        """
        Get astronomy picture for a specific date.

        Args:
            target_date: Date to retrieve (between 1995-06-16 and today)

        Returns:
            Dict with APOD data for the specified date
        """
        params = {
            "api_key": self.config.nasa_api_key,
            "date": target_date.strftime("%Y-%m-%d")
        }
        return self.get(self.endpoint, params=params)

    def get_range(
        self,
        start_date: date,
        end_date: Optional[date] = None
    ) -> List[Dict[str, Any]]:
        """
        Get astronomy pictures for a date range.

        Args:
            start_date: Start date for range
            end_date: End date for range (defaults to today)

        Returns:
            List of APOD dictionaries for the date range

        Note:
            Maximum range is 100 dates
        """
        params = {
            "api_key": self.config.nasa_api_key,
            "start_date": start_date.strftime("%Y-%m-%d")
        }

        if end_date:
            params["end_date"] = end_date.strftime("%Y-%m-%d")

        return self.get(self.endpoint, params=params)

    def get_random(self, count: int = 1) -> List[Dict[str, Any]]:
        """
        Get random astronomy pictures.

        Args:
            count: Number of random images to retrieve (max 100)

        Returns:
            List of random APOD dictionaries
        """
        params = {
            "api_key": self.config.nasa_api_key,
            "count": min(count, 100)  # API limit
        }
        result = self.get(self.endpoint, params=params)

        # API returns single dict if count=1, otherwise list
        return result if isinstance(result, list) else [result]
