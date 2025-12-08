"""
NASA API Client - Multi-API Integration
Supports multiple NASA APIs from api.nasa.gov using API key authentication

Available APIs:
- APOD: Astronomy Picture of the Day
- DONKI: Space Weather Database (CME, Solar Flares, etc.)
- Mars InSight: Mars Weather Data
- Mars Rover Photos: Curiosity, Opportunity, Spirit
- NEO: Near Earth Objects (Asteroids)
- Earth: Earth Imagery and Assets

Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import requests
from datetime import datetime, timedelta, timezone
from typing import List, Optional, Dict, Any
from dataclasses import dataclass
import warnings
import time


@dataclass
class APODImage:
    """Astronomy Picture of the Day"""
    date: datetime
    title: str
    explanation: str
    url: str
    media_type: str  # 'image' or 'video'
    hdurl: Optional[str] = None
    copyright: Optional[str] = None


@dataclass
class SpaceWeatherEvent:
    """DONKI Space Weather Event"""
    event_id: str
    event_type: str  # 'CME', 'GST', 'IPS', 'FLR', 'SEP', 'MPC', 'RBE'
    event_time: datetime
    instruments: List[str]
    link: str
    data: Dict[str, Any]


@dataclass
class MarsWeather:
    """Mars InSight Weather Data"""
    sol: int  # Martian day
    earth_date: datetime
    season: str
    min_temp: Optional[float]
    max_temp: Optional[float]
    pressure: Optional[float]
    wind_speed: Optional[float]
    wind_direction: Optional[str]


@dataclass
class MarsRoverPhoto:
    """Mars Rover Photo"""
    photo_id: int
    sol: int
    camera_name: str
    camera_full_name: str
    earth_date: datetime
    img_src: str
    rover_name: str


@dataclass
class NearEarthObject:
    """Near Earth Object (Asteroid)"""
    neo_id: str
    name: str
    nasa_jpl_url: str
    absolute_magnitude: float
    estimated_diameter_km: Dict[str, float]
    is_potentially_hazardous: bool
    close_approach_date: datetime
    miss_distance_km: float
    relative_velocity_kmh: float


class NASAAPIClient:
    """
    Client for NASA's api.nasa.gov APIs

    Supports:
    - APOD (Astronomy Picture of the Day)
    - DONKI (Space Weather)
    - Mars InSight Weather
    - Mars Rover Photos
    - Near Earth Objects
    - Earth Imagery
    """

    BASE_URL = "https://api.nasa.gov"

    # API Endpoints
    APOD_URL = f"{BASE_URL}/planetary/apod"
    DONKI_BASE_URL = f"{BASE_URL}/DONKI"
    MARS_WEATHER_URL = f"{BASE_URL}/insight_weather/"
    MARS_ROVER_URL = f"{BASE_URL}/mars-photos/api/v1/rovers"
    NEO_URL = f"{BASE_URL}/neo/rest/v1/feed"
    EARTH_IMAGERY_URL = f"{BASE_URL}/planetary/earth/imagery"

    # DONKI Event Types
    DONKI_EVENTS = {
        "CME": "Coronal Mass Ejection",
        "GST": "Geomagnetic Storm",
        "IPS": "Interplanetary Shock",
        "FLR": "Solar Flare",
        "SEP": "Solar Energetic Particle",
        "MPC": "Magnetopause Crossing",
        "RBE": "Radiation Belt Enhancement",
        "HSS": "High Speed Stream"
    }

    # Mars Rovers
    MARS_ROVERS = ["curiosity", "opportunity", "spirit", "perseverance"]

    def __init__(self, api_key: str = "DEMO_KEY"):
        """
        Initialize NASA API client

        Args:
            api_key: Your NASA API key from api.nasa.gov
                    Default is DEMO_KEY (limited to 30 requests/hour)
        """
        self.api_key = api_key
        self.session = requests.Session()
        self.cache = {}
        self.cache_duration = 900  # 15 minutes

    def _get_cached(self, key: str) -> Optional[Any]:
        """Get cached response if still valid"""
        if key in self.cache:
            data, timestamp = self.cache[key]
            if time.time() - timestamp < self.cache_duration:
                return data
        return None

    def _set_cache(self, key: str, data: Any):
        """Cache response"""
        self.cache[key] = (data, time.time())

    def _make_request(self, url: str, params: Optional[Dict] = None) -> Optional[Dict]:
        """Make API request with error handling"""
        if params is None:
            params = {}

        # Add API key to all requests
        params["api_key"] = self.api_key

        # Check cache
        cache_key = f"{url}:{str(params)}"
        cached = self._get_cached(cache_key)
        if cached:
            return cached

        try:
            response = self.session.get(url, params=params, timeout=10)

            if response.status_code == 503:
                warnings.warn("NASA API temporarily unavailable (503)")
                return None

            if response.status_code == 429:
                warnings.warn("NASA API rate limit exceeded (429)")
                return None

            if response.status_code == 403:
                warnings.warn("Invalid API key or permission denied (403)")
                return None

            response.raise_for_status()
            data = response.json()

            # Cache successful response
            self._set_cache(cache_key, data)
            return data

        except requests.exceptions.RequestException as e:
            warnings.warn(f"NASA API request failed: {e}")
            return None

    # ========================================================================
    # APOD - Astronomy Picture of the Day
    # ========================================================================

    def get_apod(self, date: Optional[datetime] = None, hd: bool = True) -> Optional[APODImage]:
        """
        Get Astronomy Picture of the Day

        Args:
            date: Date to get APOD for (default: today)
            hd: Include HD URL if available

        Returns:
            APODImage or None if unavailable
        """
        params = {}
        if date:
            params["date"] = date.strftime("%Y-%m-%d")
        if hd:
            params["hd"] = "true"

        data = self._make_request(self.APOD_URL, params)
        if not data:
            return None

        return APODImage(
            date=datetime.strptime(data["date"], "%Y-%m-%d").replace(tzinfo=timezone.utc),
            title=data.get("title", ""),
            explanation=data.get("explanation", ""),
            url=data.get("url", ""),
            media_type=data.get("media_type", "image"),
            hdurl=data.get("hdurl"),
            copyright=data.get("copyright")
        )

    def get_apod_range(self, start_date: datetime, end_date: datetime) -> List[APODImage]:
        """Get APOD for a date range"""
        params = {
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d")
        }

        data = self._make_request(self.APOD_URL, params)
        if not data or not isinstance(data, list):
            return []

        return [
            APODImage(
                date=datetime.strptime(item["date"], "%Y-%m-%d").replace(tzinfo=timezone.utc),
                title=item.get("title", ""),
                explanation=item.get("explanation", ""),
                url=item.get("url", ""),
                media_type=item.get("media_type", "image"),
                hdurl=item.get("hdurl"),
                copyright=item.get("copyright")
            )
            for item in data
        ]

    # ========================================================================
    # DONKI - Space Weather Events
    # ========================================================================

    def get_space_weather_events(
        self,
        event_type: str = "all",
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None
    ) -> List[SpaceWeatherEvent]:
        """
        Get space weather events from DONKI

        Args:
            event_type: Event type (CME, GST, IPS, FLR, SEP, MPC, RBE, HSS, or 'all')
            start_date: Start date (default: 30 days ago)
            end_date: End date (default: today)

        Returns:
            List of space weather events
        """
        if not start_date:
            start_date = datetime.now(timezone.utc) - timedelta(days=30)
        if not end_date:
            end_date = datetime.now(timezone.utc)

        events = []
        event_types = list(self.DONKI_EVENTS.keys()) if event_type == "all" else [event_type.upper()]

        for evt_type in event_types:
            url = f"{self.DONKI_BASE_URL}/{evt_type}"
            params = {
                "startDate": start_date.strftime("%Y-%m-%d"),
                "endDate": end_date.strftime("%Y-%m-%d")
            }

            data = self._make_request(url, params)
            if data and isinstance(data, list):
                for item in data:
                    # Parse event time (varies by event type)
                    time_field = self._get_event_time_field(evt_type, item)
                    if time_field:
                        try:
                            # Try standard format first
                            event_time = datetime.strptime(
                                time_field[:19], "%Y-%m-%dT%H:%M:%S"
                            ).replace(tzinfo=timezone.utc)
                        except ValueError:
                            # Try alternate format with Z suffix
                            try:
                                event_time = datetime.strptime(
                                    time_field.replace("Z", ""), "%Y-%m-%dT%H:%M"
                                ).replace(tzinfo=timezone.utc)
                            except ValueError:
                                # Fallback to current time
                                event_time = datetime.now(timezone.utc)
                    else:
                        event_time = datetime.now(timezone.utc)

                    events.append(SpaceWeatherEvent(
                        event_id=item.get("activityID", item.get("flrID", item.get("gstID", "unknown"))),
                        event_type=evt_type,
                        event_time=event_time,
                        instruments=item.get("instruments", []),
                        link=item.get("link", ""),
                        data=item
                    ))

        return sorted(events, key=lambda e: e.event_time, reverse=True)

    def _get_event_time_field(self, event_type: str, data: Dict) -> Optional[str]:
        """Extract time field from event data based on type"""
        time_fields = {
            "CME": "activityTime",
            "GST": "startTime",
            "IPS": "eventTime",
            "FLR": "beginTime",
            "SEP": "eventTime",
            "MPC": "eventTime",
            "RBE": "eventTime",
            "HSS": "eventTime"
        }
        field = time_fields.get(event_type)
        return data.get(field) if field else None

    # ========================================================================
    # Mars InSight Weather
    # ========================================================================

    def get_mars_weather(self) -> List[MarsWeather]:
        """
        Get recent Mars weather data from InSight lander

        Returns:
            List of MarsWeather for recent sols
        """
        data = self._make_request(self.MARS_WEATHER_URL)
        if not data or "sol_keys" not in data:
            return []

        weather_data = []
        for sol in data["sol_keys"]:
            sol_data = data[sol]

            # Parse temperature if available
            min_temp = sol_data.get("AT", {}).get("mn")
            max_temp = sol_data.get("AT", {}).get("mx")

            # Parse pressure if available
            pressure = sol_data.get("PRE", {}).get("av")

            # Parse wind if available
            wind_speed = sol_data.get("HWS", {}).get("av")
            wind_direction = sol_data.get("WD", {}).get("most_common", {}).get("compass_point")

            weather_data.append(MarsWeather(
                sol=int(sol),
                earth_date=datetime.strptime(
                    sol_data["First_UTC"][:10], "%Y-%m-%d"
                ).replace(tzinfo=timezone.utc),
                season=sol_data.get("Season", "unknown"),
                min_temp=min_temp,
                max_temp=max_temp,
                pressure=pressure,
                wind_speed=wind_speed,
                wind_direction=wind_direction
            ))

        return sorted(weather_data, key=lambda w: w.sol, reverse=True)

    # ========================================================================
    # Mars Rover Photos
    # ========================================================================

    def get_mars_rover_photos(
        self,
        rover: str = "curiosity",
        sol: Optional[int] = None,
        earth_date: Optional[datetime] = None,
        camera: Optional[str] = None,
        page: int = 1
    ) -> List[MarsRoverPhoto]:
        """
        Get Mars Rover photos

        Args:
            rover: Rover name (curiosity, opportunity, spirit, perseverance)
            sol: Martian sol (takes precedence over earth_date)
            earth_date: Earth date
            camera: Camera name (FHAZ, RHAZ, MAST, CHEMCAM, etc.)
            page: Page number

        Returns:
            List of MarsRoverPhoto
        """
        rover = rover.lower()
        if rover not in self.MARS_ROVERS:
            warnings.warn(f"Unknown rover: {rover}")
            return []

        url = f"{self.MARS_ROVER_URL}/{rover}/photos"
        params = {"page": page}

        if sol is not None:
            params["sol"] = sol
        elif earth_date:
            params["earth_date"] = earth_date.strftime("%Y-%m-%d")
        else:
            # Default to latest sol
            params["sol"] = 1000  # Curiosity is past sol 3000, but this is a safe default

        if camera:
            params["camera"] = camera.upper()

        data = self._make_request(url, params)
        if not data or "photos" not in data:
            return []

        return [
            MarsRoverPhoto(
                photo_id=photo["id"],
                sol=photo["sol"],
                camera_name=photo["camera"]["name"],
                camera_full_name=photo["camera"]["full_name"],
                earth_date=datetime.strptime(
                    photo["earth_date"], "%Y-%m-%d"
                ).replace(tzinfo=timezone.utc),
                img_src=photo["img_src"],
                rover_name=photo["rover"]["name"]
            )
            for photo in data["photos"]
        ]

    # ========================================================================
    # Near Earth Objects (Asteroids)
    # ========================================================================

    def get_near_earth_objects(
        self,
        start_date: Optional[datetime] = None,
        end_date: Optional[datetime] = None
    ) -> List[NearEarthObject]:
        """
        Get Near Earth Objects (asteroids) for date range

        Args:
            start_date: Start date (default: today)
            end_date: End date (default: 7 days from start)

        Returns:
            List of NearEarthObject
        """
        if not start_date:
            start_date = datetime.now(timezone.utc)
        if not end_date:
            end_date = start_date + timedelta(days=7)

        params = {
            "start_date": start_date.strftime("%Y-%m-%d"),
            "end_date": end_date.strftime("%Y-%m-%d")
        }

        data = self._make_request(self.NEO_URL, params)
        if not data or "near_earth_objects" not in data:
            return []

        neos = []
        for date_str, objects in data["near_earth_objects"].items():
            for neo in objects:
                # Get first close approach
                if neo["close_approach_data"]:
                    approach = neo["close_approach_data"][0]
                    neos.append(NearEarthObject(
                        neo_id=neo["id"],
                        name=neo["name"],
                        nasa_jpl_url=neo["nasa_jpl_url"],
                        absolute_magnitude=neo["absolute_magnitude_h"],
                        estimated_diameter_km=neo["estimated_diameter"]["kilometers"],
                        is_potentially_hazardous=neo["is_potentially_hazardous_asteroid"],
                        close_approach_date=datetime.strptime(
                            approach["close_approach_date"], "%Y-%m-%d"
                        ).replace(tzinfo=timezone.utc),
                        miss_distance_km=float(approach["miss_distance"]["kilometers"]),
                        relative_velocity_kmh=float(approach["relative_velocity"]["kilometers_per_hour"])
                    ))

        return sorted(neos, key=lambda n: n.close_approach_date)


if __name__ == "__main__":
    # Test with demo key
    client = NASAAPIClient()

    print("Testing NASA API Client...")
    print("=" * 80)

    # Test APOD
    print("\n1. Astronomy Picture of the Day:")
    apod = client.get_apod()
    if apod:
        print(f"   {apod.title}")
        print(f"   {apod.url}")

    # Test Space Weather
    print("\n2. Recent Solar Flares:")
    flares = client.get_space_weather_events("FLR", end_date=datetime.now(timezone.utc))
    print(f"   Found {len(flares)} solar flares in last 30 days")

    # Test Mars Rover
    print("\n3. Recent Mars Rover Photos:")
    photos = client.get_mars_rover_photos("curiosity", sol=3000)
    print(f"   Found {len(photos)} photos from Curiosity sol 3000")

    # Test NEO
    print("\n4. Near Earth Objects:")
    neos = client.get_near_earth_objects()
    print(f"   Found {len(neos)} asteroids approaching Earth in next 7 days")

    print("\n" + "=" * 80)
    print("NASA API Client test complete!")
