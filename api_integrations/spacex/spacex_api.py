"""
SpaceX API Client (r-spacex/SpaceX-API).

Provides access to SpaceX launch, rocket, capsule, and Starlink satellite data.
API Documentation: https://github.com/r-spacex/SpaceX-API/tree/master/docs
"""

from typing import Optional, Dict, Any, List
from ..base_client import BaseAPIClient, APIConfig


class SpaceXClient(BaseAPIClient):
    """Client for unofficial SpaceX API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize SpaceX API client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = self.config.spacex_base_url

    # ========== Launches ==========

    def get_launches(
        self,
        query: Optional[Dict[str, Any]] = None,
        options: Optional[Dict[str, Any]] = None
    ) -> List[Dict[str, Any]]:
        """
        Query launches with advanced filtering.

        Args:
            query: MongoDB-style query (e.g., {"upcoming": True})
            options: Query options (limit, sort, select, populate)

        Returns:
            List of launch dictionaries
        """
        payload = {
            "query": query or {},
            "options": options or {}
        }
        return self.post(f"{self.base_endpoint}/launches/query", data=payload)

    def get_latest_launch(self) -> Dict[str, Any]:
        """Get the latest launch."""
        return self.get(f"{self.base_endpoint}/launches/latest")

    def get_next_launch(self) -> Dict[str, Any]:
        """Get the next upcoming launch."""
        return self.get(f"{self.base_endpoint}/launches/next")

    def get_past_launches(self, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Get past launches.

        Args:
            limit: Number of launches to return

        Returns:
            List of past launch dictionaries
        """
        options = {"limit": limit, "sort": {"date_unix": "desc"}}
        return self.get_launches(query={"upcoming": False}, options=options)

    def get_upcoming_launches(self, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Get upcoming launches.

        Args:
            limit: Number of launches to return

        Returns:
            List of upcoming launch dictionaries
        """
        options = {"limit": limit, "sort": {"date_unix": "asc"}}
        return self.get_launches(query={"upcoming": True}, options=options)

    # ========== Rockets ==========

    def get_rockets(self) -> List[Dict[str, Any]]:
        """Get all rockets."""
        return self.get(f"{self.base_endpoint}/rockets")

    def get_rocket(self, rocket_id: str) -> Dict[str, Any]:
        """
        Get specific rocket by ID.

        Args:
            rocket_id: Rocket ID (e.g., "5e9d0d95eda69973a809d1ec" for Falcon 9)
        """
        return self.get(f"{self.base_endpoint}/rockets/{rocket_id}")

    # ========== Capsules ==========

    def get_capsules(self) -> List[Dict[str, Any]]:
        """Get all capsules."""
        return self.get(f"{self.base_endpoint}/capsules")

    def get_capsule(self, capsule_id: str) -> Dict[str, Any]:
        """
        Get specific capsule by serial number.

        Args:
            capsule_id: Capsule serial (e.g., "C101")
        """
        return self.get(f"{self.base_endpoint}/capsules/{capsule_id}")

    # ========== Crew ==========

    def get_crew(self) -> List[Dict[str, Any]]:
        """Get all crew members."""
        return self.get(f"{self.base_endpoint}/crew")

    def get_crew_member(self, crew_id: str) -> Dict[str, Any]:
        """
        Get specific crew member.

        Args:
            crew_id: Crew member ID
        """
        return self.get(f"{self.base_endpoint}/crew/{crew_id}")

    # ========== Starlink ==========

    def get_starlink_satellites(
        self,
        query: Optional[Dict[str, Any]] = None,
        options: Optional[Dict[str, Any]] = None
    ) -> List[Dict[str, Any]]:
        """
        Query Starlink satellites.

        Args:
            query: MongoDB-style query
            options: Query options (limit, sort, select)

        Returns:
            List of Starlink satellite dictionaries with:
                - spaceTrack: Orbital data
                - launch: Launch ID
                - version: Starlink version
                - longitude: Current longitude
                - latitude: Current latitude
                - height_km: Altitude in km
                - velocity_kms: Velocity in km/s
        """
        payload = {
            "query": query or {},
            "options": options or {}
        }
        return self.post(f"{self.base_endpoint}/starlink/query", data=payload)

    def get_starlink_by_id(self, starlink_id: str) -> Dict[str, Any]:
        """
        Get specific Starlink satellite.

        Args:
            starlink_id: Starlink satellite ID
        """
        return self.get(f"{self.base_endpoint}/starlink/{starlink_id}")

    # ========== Launchpads ==========

    def get_launchpads(self) -> List[Dict[str, Any]]:
        """Get all launchpads."""
        return self.get(f"{self.base_endpoint}/launchpads")

    def get_launchpad(self, launchpad_id: str) -> Dict[str, Any]:
        """
        Get specific launchpad.

        Args:
            launchpad_id: Launchpad ID
        """
        return self.get(f"{self.base_endpoint}/launchpads/{launchpad_id}")

    # ========== Landpads ==========

    def get_landpads(self) -> List[Dict[str, Any]]:
        """Get all landing pads."""
        return self.get(f"{self.base_endpoint}/landpads")

    def get_landpad(self, landpad_id: str) -> Dict[str, Any]:
        """
        Get specific landing pad.

        Args:
            landpad_id: Landing pad ID
        """
        return self.get(f"{self.base_endpoint}/landpads/{landpad_id}")

    # ========== Ships ==========

    def get_ships(self) -> List[Dict[str, Any]]:
        """Get all ships."""
        return self.get(f"{self.base_endpoint}/ships")

    def get_ship(self, ship_id: str) -> Dict[str, Any]:
        """
        Get specific ship.

        Args:
            ship_id: Ship ID
        """
        return self.get(f"{self.base_endpoint}/ships/{ship_id}")

    # ========== Company Info ==========

    def get_company_info(self) -> Dict[str, Any]:
        """Get SpaceX company information."""
        return self.get(f"{self.base_endpoint}/company")

    # ========== Roadster ==========

    def get_roadster(self) -> Dict[str, Any]:
        """Get Elon's Roadster orbital data."""
        return self.get(f"{self.base_endpoint}/roadster")
