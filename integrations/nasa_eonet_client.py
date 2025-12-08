#!/usr/bin/env python3
"""
NASA EONET (Earth Observatory Natural Event Tracker) Integration

Provides real-time natural event data from NASA's EONET system:
- Wildfires
- Severe storms
- Volcanoes
- Floods
- Droughts
- Landslides
- Sea and lake ice
- Snow
- Water color
- Dust and haze
- Temperature extremes

API Documentation: https://eonet.gsfc.nasa.gov/docs/v3

Author: Claude
Date: December 8, 2025
"""

import requests
import json
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta
from dataclasses import dataclass, field
import warnings


@dataclass
class EONETEvent:
    """Natural event from EONET"""
    id: str
    title: str
    description: Optional[str]
    link: str
    categories: List[Dict[str, Any]]
    sources: List[Dict[str, Any]]
    geometry: List[Dict[str, Any]]
    closed: Optional[str] = None

    # Derived fields
    event_date: Optional[datetime] = None
    latitude: Optional[float] = None
    longitude: Optional[float] = None
    magnitude: Optional[float] = None


@dataclass
class EONETCategory:
    """Event category"""
    id: str
    title: str
    description: str
    link: str
    layers: Optional[str] = None


class NASAEONETClient:
    """Client for NASA Earth Observatory Natural Event Tracker (EONET)"""

    # EONET API endpoints
    BASE_URL = "https://eonet.gsfc.nasa.gov/api/v3"
    EVENTS_URL = f"{BASE_URL}/events"
    CATEGORIES_URL = f"{BASE_URL}/categories"
    LAYERS_URL = f"{BASE_URL}/layers"
    SOURCES_URL = f"{BASE_URL}/sources"

    # Category IDs
    CATEGORY_IDS = {
        "wildfires": 8,
        "severe_storms": 10,
        "volcanoes": 12,
        "floods": 5,
        "droughts": 6,
        "landslides": 14,
        "sea_lake_ice": 15,
        "snow": 16,
        "water_color": 17,
        "dust_haze": 18,
        "manmade": 9,
        "earthquakes": 16,
        "temperature_extremes": 19
    }

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize EONET client

        Args:
            api_key: Optional NASA API key (EONET is public, no key required)
        """
        self.api_key = api_key
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'MotorHandPro-EONET-Client/1.0'
        })

        self.cache = {}
        self.cache_duration = 300  # 5 minutes

    def get_events(
        self,
        status: str = "all",
        limit: int = 100,
        days: Optional[int] = None,
        category: Optional[str] = None,
        source: Optional[str] = None,
        bbox: Optional[str] = None
    ) -> List[EONETEvent]:
        """
        Get natural events from EONET

        Args:
            status: Event status - "open", "closed", or "all" (default)
            limit: Maximum number of events to return (default: 100)
            days: Only events in last N days
            category: Filter by category ID or name
            source: Filter by data source
            bbox: Bounding box "west,south,east,north" in degrees

        Returns:
            List of EONET events
        """
        cache_key = f"events_{status}_{limit}_{days}_{category}_{source}_{bbox}"

        # Check cache
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if (datetime.now() - cached_time).seconds < self.cache_duration:
                return cached_data

        # Build query parameters
        params = {
            "status": status,
            "limit": limit
        }

        if days:
            params["days"] = days

        if category:
            # Map category name to ID if needed
            if category in self.CATEGORY_IDS:
                params["category"] = self.CATEGORY_IDS[category]
            else:
                params["category"] = category

        if source:
            params["source"] = source

        if bbox:
            params["bbox"] = bbox

        try:
            response = self.session.get(
                self.EVENTS_URL,
                params=params,
                timeout=30
            )

            if response.status_code == 503:
                warnings.warn("EONET API temporarily unavailable (503)")
                return []

            if response.status_code != 200:
                warnings.warn(f"EONET API returned {response.status_code}")
                return []

            data = response.json()
            events = self._parse_events(data)

            # Cache results
            self.cache[cache_key] = (datetime.now(), events)

            return events

        except requests.RequestException as e:
            warnings.warn(f"EONET API error: {e}")
            return []
        except json.JSONDecodeError as e:
            warnings.warn(f"Failed to parse EONET response: {e}")
            return []

    def get_categories(self) -> List[EONETCategory]:
        """Get all event categories"""
        cache_key = "categories"

        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if (datetime.now() - cached_time).seconds < 3600:  # 1 hour cache
                return cached_data

        try:
            response = self.session.get(self.CATEGORIES_URL, timeout=10)

            if response.status_code != 200:
                return []

            data = response.json()
            categories = []

            if "categories" in data:
                for cat in data["categories"]:
                    categories.append(EONETCategory(
                        id=cat.get("id"),
                        title=cat.get("title"),
                        description=cat.get("description", ""),
                        link=cat.get("link"),
                        layers=cat.get("layers")
                    ))

            self.cache[cache_key] = (datetime.now(), categories)
            return categories

        except Exception as e:
            warnings.warn(f"Failed to get EONET categories: {e}")
            return []

    def get_event_by_id(self, event_id: str) -> Optional[EONETEvent]:
        """Get specific event by ID"""
        try:
            response = self.session.get(
                f"{self.EVENTS_URL}/{event_id}",
                timeout=10
            )

            if response.status_code != 200:
                return None

            data = response.json()
            events = self._parse_events(data)
            return events[0] if events else None

        except Exception as e:
            warnings.warn(f"Failed to get event {event_id}: {e}")
            return None

    def _parse_events(self, data: Dict[str, Any]) -> List[EONETEvent]:
        """Parse EONET events from API response"""
        events = []

        try:
            # Handle both single event and event list responses
            event_list = data.get("events", [data] if "id" in data else [])

            for event_data in event_list:
                # Extract geometry (coordinates)
                geometry = event_data.get("geometry", [])
                latitude = None
                longitude = None
                magnitude = None
                event_date = None

                if geometry:
                    # Get most recent geometry point
                    latest_geo = geometry[-1]
                    coords = latest_geo.get("coordinates", [])
                    if len(coords) >= 2:
                        longitude = coords[0]
                        latitude = coords[1]
                        if len(coords) >= 3:
                            magnitude = coords[2]

                    # Parse date
                    date_str = latest_geo.get("date")
                    if date_str:
                        try:
                            event_date = datetime.fromisoformat(date_str.replace('Z', '+00:00'))
                        except:
                            pass

                event = EONETEvent(
                    id=event_data.get("id"),
                    title=event_data.get("title"),
                    description=event_data.get("description"),
                    link=event_data.get("link"),
                    categories=event_data.get("categories", []),
                    sources=event_data.get("sources", []),
                    geometry=geometry,
                    closed=event_data.get("closed"),
                    event_date=event_date,
                    latitude=latitude,
                    longitude=longitude,
                    magnitude=magnitude
                )

                events.append(event)

            return events

        except Exception as e:
            warnings.warn(f"Failed to parse EONET events: {e}")
            return []


# Example usage
if __name__ == "__main__":
    client = NASAEONETClient()

    print("=" * 80)
    print("NASA EONET - Natural Event Tracker")
    print("=" * 80)
    print()

    # Get categories
    print("Event Categories:")
    categories = client.get_categories()
    for cat in categories:
        print(f"  {cat.id}: {cat.title}")
    print()

    # Get recent wildfires
    print("Recent Wildfires:")
    wildfires = client.get_events(category="wildfires", limit=5, days=30)
    for event in wildfires:
        print(f"  {event.title}")
        if event.event_date:
            print(f"    Date: {event.event_date}")
        if event.latitude and event.longitude:
            print(f"    Location: {event.latitude:.2f}, {event.longitude:.2f}")
    print()

    # Get recent severe storms
    print("Recent Severe Storms:")
    storms = client.get_events(category="severe_storms", limit=5, days=30)
    for event in storms:
        print(f"  {event.title}")
        if event.event_date:
            print(f"    Date: {event.event_date}")
    print()
