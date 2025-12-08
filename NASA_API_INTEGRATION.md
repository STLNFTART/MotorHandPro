# NASA API Integration (api.nasa.gov)

## Overview

MotorHandPro now integrates with NASA's official API portal (api.nasa.gov) to access multiple real-time NASA data sources using your NASA API credentials.

**Your API Key:** `7S8ltlPgaI1CiAd7OuVRflyt6dprnMfhdgeX7EWW`

## Available APIs

### 1. APOD - Astronomy Picture of the Day
Daily astronomical imagery with detailed descriptions from professional astronomers.

### 2. DONKI - Space Weather Database
Real-time space weather events including:
- Solar Flares
- Coronal Mass Ejections (CME)
- Geomagnetic Storms
- Interplanetary Shocks
- Solar Energetic Particles

### 3. Mars Rover Photos
Thousands of images from NASA's Mars rovers:
- Curiosity
- Perseverance
- Opportunity (inactive)
- Spirit (inactive)

### 4. Near Earth Objects (NEO)
Asteroid tracking and close approach data for objects passing near Earth.

### 5. Mars InSight Weather
Real-time weather data from Mars (temperature, pressure, wind).

## API Endpoints

### Status Check

```bash
GET /nasa/api/status
```

**Response:**
```json
{
  "status": "available",
  "client": "NASAAPIClient",
  "api_key_set": true,
  "available_apis": [
    "APOD - Astronomy Picture of the Day",
    "DONKI - Space Weather Events",
    "Mars InSight - Mars Weather",
    "Mars Rover Photos",
    "NEO - Near Earth Objects"
  ]
}
```

---

### APOD - Astronomy Picture of the Day

#### Get Today's APOD

```bash
GET /nasa/apod
Authorization: Bearer <token>
```

**Response:**
```json
{
  "status": "ok",
  "data": {
    "date": "2025-12-07",
    "title": "The Sun and Its Missing Colors",
    "explanation": "It is still not known why the Sun's light is missing some colors...",
    "url": "https://apod.nasa.gov/apod/image/2512/sunspectrum_mpso_1080.jpg",
    "hdurl": "https://apod.nasa.gov/apod/image/2512/sunspectrum_mpso_3071.jpg",
    "media_type": "image",
    "copyright": null
  }
}
```

#### Get APOD for Specific Date

```bash
GET /nasa/apod?date=2024-01-01
Authorization: Bearer <token>
```

#### Get APOD Range

```bash
GET /nasa/apod/range?start_date=2024-12-01&end_date=2024-12-07
Authorization: Bearer <token>
```

**Parameters:**
- `date`: Date in YYYY-MM-DD format (optional)
- `hd`: Include HD URL (default: true)
- `start_date`: Range start (YYYY-MM-DD)
- `end_date`: Range end (max 7 days from start)

---

### DONKI - Space Weather Events

#### Get All Space Weather Events

```bash
GET /nasa/space-weather?days=30
Authorization: Bearer <token>
```

**Response:**
```json
{
  "status": "ok",
  "count": 4,
  "events": [
    {
      "event_id": "2025-12-06T20:29:00-FLR-001",
      "event_type": "FLR",
      "event_time": "2025-12-06T20:29:00+00:00",
      "instruments": [{"displayName": "GOES-P: EXIS 1.0-8.0"}],
      "link": "https://webtools.ccmc.gsfc.nasa.gov/DONKI/view/FLR/43300/-1"
    }
  ]
}
```

#### Get Solar Flares

```bash
GET /nasa/space-weather?event_type=FLR&days=7
Authorization: Bearer <token>
```

#### Get Coronal Mass Ejections

```bash
GET /nasa/space-weather?event_type=CME&days=30
Authorization: Bearer <token>
```

#### Get Geomagnetic Storms

```bash
GET /nasa/space-weather?event_type=GST&days=30
Authorization: Bearer <token>
```

**Parameters:**
- `event_type`: Event type filter (default: "all")
  - `all` - All events
  - `FLR` - Solar Flares
  - `CME` - Coronal Mass Ejections
  - `GST` - Geomagnetic Storms
  - `IPS` - Interplanetary Shocks
  - `SEP` - Solar Energetic Particles
  - `MPC` - Magnetopause Crossing
  - `RBE` - Radiation Belt Enhancement
  - `HSS` - High Speed Stream
- `days`: Number of days to look back (default: 30)

---

### Mars Rover Photos

#### Get Photos by Sol (Martian Day)

```bash
GET /nasa/mars/photos/{rover}?sol=4000
Authorization: Bearer <token>
```

**Rovers:** `curiosity`, `perseverance`, `opportunity`, `spirit`

**Response:**
```json
{
  "status": "ok",
  "count": 125,
  "photos": [
    {
      "photo_id": 1234567,
      "sol": 4000,
      "camera": "FHAZ",
      "camera_full_name": "Front Hazard Avoidance Camera",
      "earth_date": "2024-03-15",
      "img_src": "https://mars.nasa.gov/msl-raw-images/...",
      "rover": "Curiosity"
    }
  ]
}
```

#### Get Photos by Earth Date

```bash
GET /nasa/mars/photos/perseverance?earth_date=2024-01-01
Authorization: Bearer <token>
```

#### Filter by Camera

```bash
GET /nasa/mars/photos/curiosity?sol=3000&camera=NAVCAM
Authorization: Bearer <token>
```

**Parameters:**
- `rover`: Rover name (curiosity, perseverance, opportunity, spirit)
- `sol`: Martian sol number (optional)
- `earth_date`: Earth date YYYY-MM-DD (optional)
- `camera`: Camera name (optional)
  - `FHAZ` - Front Hazard Avoidance Camera
  - `RHAZ` - Rear Hazard Avoidance Camera
  - `MAST` - Mast Camera
  - `CHEMCAM` - Chemistry and Camera Complex
  - `NAVCAM` - Navigation Camera
  - `PANCAM` - Panoramic Camera
- `page`: Page number (default: 1)

---

### Mars InSight Weather

```bash
GET /nasa/mars/weather
Authorization: Bearer <token>
```

**Response:**
```json
{
  "status": "ok",
  "count": 7,
  "data": [
    {
      "sol": 1234,
      "earth_date": "2024-03-15",
      "season": "summer",
      "min_temp_c": -95.2,
      "max_temp_c": -15.8,
      "pressure_pa": 720.5,
      "wind_speed_ms": 5.2,
      "wind_direction": "SW"
    }
  ]
}
```

---

### Near Earth Objects (Asteroids)

#### Get Upcoming Asteroids (Next 7 Days)

```bash
GET /nasa/neo
Authorization: Bearer <token>
```

#### Get Asteroids for Date Range

```bash
GET /nasa/neo?start_date=2025-12-01&end_date=2025-12-07
Authorization: Bearer <token>
```

**Response:**
```json
{
  "status": "ok",
  "count": 15,
  "objects": [
    {
      "neo_id": "2021277",
      "name": "(2021 PH27)",
      "nasa_jpl_url": "https://ssd.jpl.nasa.gov/sbdb.cgi?sstr=2021277",
      "absolute_magnitude": 18.5,
      "estimated_diameter_km": {"min": 0.15, "max": 0.34},
      "is_potentially_hazardous": false,
      "close_approach_date": "2025-12-05",
      "miss_distance_km": 42582100.5,
      "relative_velocity_kmh": 54321.87
    }
  ]
}
```

**Parameters:**
- `start_date`: Start date YYYY-MM-DD (default: today)
- `end_date`: End date YYYY-MM-DD (default: start + 7 days)

---

## Configuration

### Setting Your API Key

The NASA API key is configured via environment variable:

```bash
export NASA_API_KEY="7S8ltlPgaI1CiAd7OuVRflyt6dprnMfhdgeX7EWW"
```

Or in the server startup:

```bash
NASA_API_KEY="your-key-here" python -m infrastructure.api
```

### Rate Limits

With your NASA API key, you have access to:
- **1,000 requests per hour**
- **No daily limit**

The DEMO_KEY has limits of:
- 30 requests per hour
- 50 requests per day per IP

### Response Caching

All NASA API responses are cached for 15 minutes to:
- Reduce API load
- Improve response times
- Stay within rate limits

## Testing

### Quick Test

```bash
# Test APOD
curl -H "Authorization: Bearer dev-token" \
  http://localhost:8000/nasa/apod

# Test Space Weather
curl -H "Authorization: Bearer dev-token" \
  "http://localhost:8000/nasa/space-weather?event_type=FLR&days=7"

# Test Mars Rover
curl -H "Authorization: Bearer dev-token" \
  "http://localhost:8000/nasa/mars/photos/curiosity?sol=4000"

# Test Near Earth Objects
curl -H "Authorization: Bearer dev-token" \
  http://localhost:8000/nasa/neo
```

### Comprehensive Test Suite

```bash
./scripts/test_nasa_apis.sh
```

## Integration with MotorHandPro

### Real-Time Data Streaming

All NASA data can be integrated with:

1. **PRIMAL Operator**: Process time-series data from space weather events
2. **Database Storage**: Store historical data for trend analysis
3. **MQTT Publishing**: Stream events to subscribers
4. **WebSocket**: Real-time updates to dashboards

### Example: Space Weather Monitoring

```python
from nasa_api_client import NASAAPIClient
from datetime import datetime, timedelta, timezone

client = NASAAPIClient(api_key="your-key")

# Monitor solar flares
flares = client.get_space_weather_events("FLR", days=7)
for flare in flares:
    print(f"Solar Flare at {flare.event_time}")
    print(f"  ID: {flare.event_id}")
    print(f"  Link: {flare.link}")
```

### Example: Mars Rover Daily Photos

```python
# Get latest Perseverance photos
photos = client.get_mars_rover_photos("perseverance", sol=1000)
for photo in photos[:10]:
    print(f"{photo.camera_name}: {photo.img_src}")
```

### Example: Asteroid Tracking

```python
# Track potentially hazardous asteroids
neos = client.get_near_earth_objects()
hazardous = [neo for neo in neos if neo.is_potentially_hazardous]
for asteroid in hazardous:
    print(f"⚠️  {asteroid.name}")
    print(f"   Miss distance: {asteroid.miss_distance_km:,.0f} km")
    print(f"   Date: {asteroid.close_approach_date}")
```

## Data Sources

All data comes from official NASA sources:

- **APOD**: [apod.nasa.gov](https://apod.nasa.gov)
- **DONKI**: [ccmc.gsfc.nasa.gov/donki](https://ccmc.gsfc.nasa.gov/donki/)
- **Mars Rovers**: [mars.nasa.gov](https://mars.nasa.gov)
- **NEO**: [cneos.jpl.nasa.gov](https://cneos.jpl.nasa.gov)
- **InSight**: [mars.nasa.gov/insight](https://mars.nasa.gov/insight/)

## Error Handling

The integration handles various error conditions:

1. **503 Service Unavailable**: NASA API temporarily down
   - Returns empty results with warning
   - Cached data may be available

2. **429 Rate Limit Exceeded**: Too many requests
   - Warning message in logs
   - Caching helps prevent this

3. **403 Forbidden**: Invalid API key
   - Check API_KEY environment variable
   - Verify key at [api.nasa.gov](https://api.nasa.gov)

4. **Network Errors**: Connection failures
   - Graceful degradation
   - Retry logic in client

## Future Enhancements

- [ ] Automatic polling for new space weather events
- [ ] MQTT streaming of solar flares and CMEs
- [ ] Database storage of asteroid close approaches
- [ ] Alert system for potentially hazardous NEOs
- [ ] Mars weather trending and anomaly detection
- [ ] Integration with LAM for autonomous space weather response
- [ ] Dashboard visualization of all NASA data streams

## Support

- **NASA API Documentation**: [api.nasa.gov](https://api.nasa.gov)
- **Get Your Own API Key**: [api.nasa.gov - Generate API Key](https://api.nasa.gov)
- **NASA Data Policy**: Free and open to public use
- **MotorHandPro Issues**: Report integration issues via GitHub

## License

This integration uses NASA's public APIs. NASA data is available free of charge and may be used without restriction.

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
