"""
NASA Earth Polychromatic Imaging Camera (EPIC) API Client.

Provides access to full disc imagery of Earth from the DSCOVR satellite.
API Documentation: https://epic.gsfc.nasa.gov/about/api
"""

from typing import Optional, Dict, Any, List
from datetime import date
from ..base_client import BaseAPIClient, APIConfig


class EPICClient(BaseAPIClient):
    """Client for NASA EPIC API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize EPIC client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = f"{self.config.nasa_base_url}/EPIC/api"

    def get_natural_images(
        self,
        target_date: Optional[date] = None,
        available: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Get natural color Earth images.

        Args:
            target_date: Specific date to retrieve (None for most recent)
            available: If True, return list of available dates instead

        Returns:
            List of image metadata dicts or list of available dates
        """
        if available:
            endpoint = f"{self.base_endpoint}/natural/available"
        elif target_date:
            date_str = target_date.strftime("%Y-%m-%d")
            endpoint = f"{self.base_endpoint}/natural/date/{date_str}"
        else:
            endpoint = f"{self.base_endpoint}/natural"

        params = {"api_key": self.config.nasa_api_key}
        return self.get(endpoint, params=params)

    def get_enhanced_images(
        self,
        target_date: Optional[date] = None,
        available: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Get enhanced color Earth images.

        Args:
            target_date: Specific date to retrieve (None for most recent)
            available: If True, return list of available dates instead

        Returns:
            List of enhanced image metadata dicts or list of available dates
        """
        if available:
            endpoint = f"{self.base_endpoint}/enhanced/available"
        elif target_date:
            date_str = target_date.strftime("%Y-%m-%d")
            endpoint = f"{self.base_endpoint}/enhanced/date/{date_str}"
        else:
            endpoint = f"{self.base_endpoint}/enhanced"

        params = {"api_key": self.config.nasa_api_key}
        return self.get(endpoint, params=params)

    def get_aerosol_images(
        self,
        target_date: Optional[date] = None,
        available: bool = False
    ) -> List[Dict[str, Any]]:
        """
        Get aerosol measurement images.

        Args:
            target_date: Specific date to retrieve (None for most recent)
            available: If True, return list of available dates instead

        Returns:
            List of aerosol image metadata dicts or list of available dates
        """
        if available:
            endpoint = f"{self.base_endpoint}/aerosol/available"
        elif target_date:
            date_str = target_date.strftime("%Y-%m-%d")
            endpoint = f"{self.base_endpoint}/aerosol/date/{date_str}"
        else:
            endpoint = f"{self.base_endpoint}/aerosol"

        params = {"api_key": self.config.nasa_api_key}
        return self.get(endpoint, params=params)

    def build_image_url(
        self,
        image_data: Dict[str, Any],
        image_type: str = "natural",
        size: str = "png"
    ) -> str:
        """
        Build the full URL for downloading an EPIC image.

        Args:
            image_data: Image metadata dict from API
            image_type: Type of image (natural/enhanced/aerosol)
            size: Image format (png/jpg/thumbs)

        Returns:
            Full URL to download the image
        """
        image_name = image_data["image"]
        date_obj = image_data["date"].split()[0].replace("-", "/")

        base_url = "https://epic.gsfc.nasa.gov/archive"
        return f"{base_url}/{image_type}/{date_obj}/{size}/{image_name}.{size}"
