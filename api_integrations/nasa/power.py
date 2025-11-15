"""
NASA POWER (Prediction Of Worldwide Energy Resources) API Client.

Provides access to solar and meteorological data.
API Documentation: https://power.larc.nasa.gov/docs/services/api/
"""

from typing import Optional, Dict, Any, List
from ..base_client import BaseAPIClient, APIConfig


class POWERClient(BaseAPIClient):
    """Client for NASA POWER API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize POWER client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = "https://power.larc.nasa.gov/api/temporal"

    def get_point_data(
        self,
        latitude: float,
        longitude: float,
        parameters: List[str],
        start_date: str,
        end_date: str,
        community: str = "RE",
        temporal_api: str = "daily"
    ) -> Dict[str, Any]:
        """
        Get meteorological data for a specific point.

        Args:
            latitude: Latitude (-90 to 90)
            longitude: Longitude (-180 to 180)
            parameters: List of parameters (e.g., ["T2M", "PRECTOTCORR"])
            start_date: Start date (YYYYMMDD format)
            end_date: End date (YYYYMMDD format)
            community: Community (RE=Renewable Energy, AG=Agriculture, SB=Sustainable Buildings)
            temporal_api: Temporal resolution (daily/monthly/climatology)

        Returns:
            Dict with parameter data and metadata

        Available parameters (examples):
            - T2M: Temperature at 2 Meters
            - T2M_MAX: Maximum Temperature at 2 Meters
            - T2M_MIN: Minimum Temperature at 2 Meters
            - PRECTOTCORR: Precipitation Corrected
            - WS2M: Wind Speed at 2 Meters
            - RH2M: Relative Humidity at 2 Meters
            - ALLSKY_SFC_SW_DWN: All Sky Surface Shortwave Downward Irradiance
        """
        params = {
            "parameters": ",".join(parameters),
            "community": community,
            "longitude": longitude,
            "latitude": latitude,
            "start": start_date,
            "end": end_date,
            "format": "JSON"
        }

        endpoint = f"{self.base_endpoint}/{temporal_api}/point"
        return self.get(endpoint, params=params)

    def get_regional_data(
        self,
        bbox: List[float],
        parameters: List[str],
        start_date: str,
        end_date: str,
        community: str = "RE",
        temporal_api: str = "daily"
    ) -> Dict[str, Any]:
        """
        Get meteorological data for a regional bounding box.

        Args:
            bbox: [lon_min, lat_min, lon_max, lat_max]
            parameters: List of parameters
            start_date: Start date (YYYYMMDD format)
            end_date: End date (YYYYMMDD format)
            community: Community code
            temporal_api: Temporal resolution

        Returns:
            Dict with regional parameter data
        """
        lon_min, lat_min, lon_max, lat_max = bbox

        params = {
            "parameters": ",".join(parameters),
            "community": community,
            "bbox": f"{lon_min},{lat_min},{lon_max},{lat_max}",
            "start": start_date,
            "end": end_date,
            "format": "JSON"
        }

        endpoint = f"{self.base_endpoint}/{temporal_api}/regional"
        return self.get(endpoint, params=params)

    def get_climatology(
        self,
        latitude: float,
        longitude: float,
        parameters: List[str],
        community: str = "RE"
    ) -> Dict[str, Any]:
        """
        Get long-term climatology data for a point.

        Args:
            latitude: Latitude
            longitude: Longitude
            parameters: List of parameters
            community: Community code

        Returns:
            Dict with climatological averages
        """
        params = {
            "parameters": ",".join(parameters),
            "community": community,
            "longitude": longitude,
            "latitude": latitude,
            "format": "JSON"
        }

        endpoint = f"{self.base_endpoint}/climatology/point"
        return self.get(endpoint, params=params)
