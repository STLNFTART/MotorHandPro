"""
NASA Solar System Dynamics (SSD) API Client.

Provides access to ephemeris data, orbital elements, and mission design information.
API Documentation: https://ssd-api.jpl.nasa.gov/doc/
"""

from typing import Optional, Dict, Any, List
from ..base_client import BaseAPIClient, APIConfig


class SSDClient(BaseAPIClient):
    """Client for NASA Solar System Dynamics API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize SSD client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = "https://ssd-api.jpl.nasa.gov"

    def get_close_approach_data(
        self,
        date_min: Optional[str] = None,
        date_max: Optional[str] = None,
        dist_max: Optional[float] = None,
        h_max: Optional[float] = None,
        body: str = "Earth"
    ) -> Dict[str, Any]:
        """
        Get close approach data for small bodies.

        Args:
            date_min: Start date (YYYY-MM-DD or YYYY-MM-DD HH:MM)
            date_max: End date (YYYY-MM-DD or YYYY-MM-DD HH:MM)
            dist_max: Maximum distance in AU (default: 0.05)
            h_max: Maximum absolute magnitude
            body: Target body (Earth, Moon, etc.)

        Returns:
            Dict containing:
                - count: Number of results
                - data: List of close approach records
                - fields: Field descriptions
        """
        params = {}

        if date_min:
            params["date-min"] = date_min
        if date_max:
            params["date-max"] = date_max
        if dist_max:
            params["dist-max"] = str(dist_max)
        if h_max:
            params["h-max"] = str(h_max)
        if body:
            params["body"] = body

        return self.get(f"{self.base_endpoint}/cad.api", params=params)

    def get_fireball_data(
        self,
        date_min: Optional[str] = None,
        date_max: Optional[str] = None,
        energy_min: Optional[float] = None,
        impact_energy_min: Optional[float] = None
    ) -> Dict[str, Any]:
        """
        Get fireball (meteor) entry data.

        Args:
            date_min: Minimum date (YYYY-MM-DD)
            date_max: Maximum date (YYYY-MM-DD)
            energy_min: Minimum total radiated energy (joules ×10¹⁰)
            impact_energy_min: Minimum impact energy (kt)

        Returns:
            Dict containing:
                - count: Number of fireballs
                - data: List of fireball events
                - fields: Field descriptions
        """
        params = {}

        if date_min:
            params["date-min"] = date_min
        if date_max:
            params["date-max"] = date_max
        if energy_min:
            params["energy-min"] = str(energy_min)
        if impact_energy_min:
            params["req-impact-energy-min"] = str(impact_energy_min)

        return self.get(f"{self.base_endpoint}/fireball.api", params=params)

    def get_mission_design(
        self,
        des: str,
        orbit_class: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        Get mission design data for a small body.

        Args:
            des: Object designation (e.g., "433" for Eros)
            orbit_class: Orbit class filter

        Returns:
            Dict with mission design parameters
        """
        params = {"des": des}

        if orbit_class:
            params["class"] = orbit_class

        return self.get(f"{self.base_endpoint}/nhats.api", params=params)

    def get_sentry_data(
        self,
        des: Optional[str] = None,
        removed: int = 0
    ) -> Dict[str, Any]:
        """
        Get Sentry impact risk data for potentially hazardous objects.

        Args:
            des: Specific object designation (None for all)
            removed: Include removed objects (0=no, 1=yes)

        Returns:
            Dict containing:
                - count: Number of objects
                - data: List of Sentry objects with impact probabilities
        """
        params = {"removed": removed}

        if des:
            params["des"] = des

        return self.get(f"{self.base_endpoint}/sentry.api", params=params)

    def get_sbdb_data(
        self,
        sstr: str,
        full_precision: bool = False
    ) -> Dict[str, Any]:
        """
        Get Small-Body Database (SBDB) information.

        Args:
            sstr: Search string (name, designation, or SPK-ID)
            full_precision: Return full precision values

        Returns:
            Dict with comprehensive orbital and physical parameters
        """
        params = {
            "sstr": sstr,
            "full-prec": "true" if full_precision else "false"
        }

        return self.get(f"{self.base_endpoint}/sbdb.api", params=params)
