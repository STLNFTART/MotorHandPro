# NASA EONET (Earth Observatory Natural Event Tracker) Integration

## Overview

The MotorHandPro system now integrates with NASA's Earth Observatory Natural Event Tracker (EONET) API to access real-time data about natural events occurring on Earth.

## What is EONET?

EONET is a NASA service that tracks natural events around the world as they occur. It provides data on:

- 🔥 **Wildfires**: Active fires detected by satellite
- ⛈️ **Severe Storms**: Hurricanes, cyclones, typhoons
- 🌋 **Volcanoes**: Volcanic eruptions and activity
- 🌊 **Floods**: Major flooding events
- 🏜️ **Droughts**: Significant drought conditions
- 🌫️ **Dust and Haze**: Atmospheric dust storms
- ❄️ **Snow**: Extreme snow events
- 🌊 **Water Color**: Ocean phenomena
- 🧊 **Sea and Lake Ice**: Ice extent changes
- 🌍 **Earthquakes**: Significant seismic activity
- 🏔️ **Landslides**: Major landslide events
- 🏭 **Manmade**: Industrial or human-caused events
- 🌡️ **Temperature Extremes**: Heat waves, cold snaps

## API Endpoints

### 1. Check EONET Status

```bash
GET /eonet/status
```

**Response:**
```json
{
  "status": "available",
  "client": "NASAEONETClient",
  "base_url": "https://eonet.gsfc.nasa.gov/api/v3",
  "categories_available": 13,
  "timestamp": "2025-12-08T00:27:33.336994+00:00"
}
```

### 2. Get Event Categories

```bash
GET /eonet/categories
Authorization: Bearer <token>
```

**Response:**
```json
{
  "status": "ok",
  "count": 13,
  "categories": [
    {
      "id": 8,
      "title": "Wildfires",
      "description": "Wildfires includes all nature of fire, including forest and plains fires, as well as urban and industrial fire events."
    },
    ...
  ]
}
```

### 3. Get Events

```bash
GET /eonet/events?status_filter=all&limit=100&days=30&category=wildfires
Authorization: Bearer <token>
```

**Parameters:**
- `status_filter`: Filter by event status (`all`, `open`, `closed`)
- `limit`: Maximum number of events to return (default: 100)
- `days`: Number of days to look back (optional)
- `category`: Filter by category (e.g., `wildfires`, `volcanoes`, etc.)

**Response:**
```json
{
  "status": "ok",
  "count": 42,
  "query": {
    "status": "open",
    "limit": 100,
    "days": 30,
    "category": "wildfires"
  },
  "events": [
    {
      "id": "EONET_6789",
      "title": "Wildfire - California, United States",
      "description": "Large wildfire in Northern California",
      "categories": ["Wildfires"],
      "event_date": "2025-12-01T00:00:00",
      "latitude": 40.7128,
      "longitude": -122.4194,
      "link": "https://eonet.gsfc.nasa.gov/api/v3/events/EONET_6789",
      "closed_date": null
    },
    ...
  ]
}
```

### 4. Get Specific Event by ID

```bash
GET /eonet/event/{event_id}
Authorization: Bearer <token>
```

**Example:**
```bash
curl -H "Authorization: Bearer dev-token" \
  http://localhost:8000/eonet/event/EONET_6789
```

## Implementation Details

### Client Library

**Location:** `integrations/nasa_eonet_client.py`

**Key Features:**
- 15-minute response caching to reduce API load
- Graceful error handling for API unavailability
- Support for all 13 event categories
- Filtering by status, category, time range, and bounding box
- Detailed event information including coordinates and timestamps

**Class: NASAEONETClient**

```python
from nasa_eonet_client import NASAEONETClient

client = NASAEONETClient()

# Get recent wildfires
events = client.get_events(category="wildfires", days=7)

# Get all categories
categories = client.get_categories()

# Get specific event
event = client.get_event_by_id("EONET_6789")
```

### FastAPI Integration

**Location:** `infrastructure/api/fastapi_server.py`

The EONET client is automatically initialized on server startup and provides four REST endpoints for accessing natural event data.

## Testing

### Quick Test

```bash
# Test EONET status
curl http://localhost:8000/eonet/status

# Get recent events
curl -H "Authorization: Bearer dev-token" \
  "http://localhost:8000/eonet/events?limit=10"

# Get wildfire events
curl -H "Authorization: Bearer dev-token" \
  "http://localhost:8000/eonet/events?category=wildfires&days=30"
```

### Comprehensive Test Suite

```bash
# Run full EONET test suite
./scripts/test_eonet_api.sh
```

## Available Event Categories

| Category | ID | Description |
|----------|----|----|
| Wildfires | 8 | Forest fires, plains fires, urban fires |
| Severe Storms | 10 | Hurricanes, cyclones, typhoons |
| Volcanoes | 12 | Volcanic eruptions and activity |
| Floods | 5 | Major flooding events |
| Droughts | 6 | Significant drought conditions |
| Dust and Haze | 7 | Atmospheric dust storms |
| Snow | 15 | Extreme snow events |
| Water Color | 16 | Ocean color phenomena |
| Sea and Lake Ice | 9 | Ice extent changes |
| Earthquakes | 16 | Significant seismic activity |
| Landslides | 14 | Major landslide events |
| Manmade | 13 | Industrial or human-caused events |
| Temperature Extremes | 17 | Heat waves, cold snaps |

## Error Handling

The EONET client handles various error conditions:

1. **503 Service Unavailable**: NASA EONET API is temporarily down
   - Returns empty list with warning
   - Cached data may be available

2. **Network Errors**: Connection failures
   - Graceful degradation
   - Warning messages in logs

3. **Invalid Parameters**: Bad requests
   - Validation errors
   - Clear error messages

## API Availability

**Note:** NASA's EONET API is a public service and occasionally experiences downtime. When the API returns 503 Service Unavailable, the endpoints will return empty results but continue functioning. The system will automatically resume fetching data when the service becomes available.

## Integration with MotorHandPro

The EONET data can be integrated with MotorHandPro's:

1. **Recursive Planck Operator**: Process natural event time-series data
2. **Database Storage**: Store events for trend analysis
3. **Real-time Streaming**: Push events via MQTT/WebSocket
4. **Visualization**: Display events on dashboards

## Data Streaming

For real-time event streaming:

1. **Polling**: Periodically check for new events
2. **WebSocket**: Push updates to connected clients
3. **MQTT**: Publish events to topics by category

## Future Enhancements

- [ ] Automatic event ingestion into TimescaleDB
- [ ] Real-time event alerts via MQTT
- [ ] Geographic filtering by bounding box
- [ ] Integration with PRIMAL operator for anomaly detection
- [ ] Dashboard visualization of active events
- [ ] Historical trend analysis

## Support

- **EONET API Documentation**: https://eonet.gsfc.nasa.gov/docs/v3
- **NASA Earthdata**: https://earthdata.nasa.gov/
- **MotorHandPro Issues**: Report integration issues via GitHub

## License

This integration uses NASA's public EONET API. NASA data is available free of charge and may be used without restriction.

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
