# NASA and SpaceX API Integration Suite

Comprehensive Python clients for NASA and SpaceX/Starlink APIs, integrated with the MotorHandPro Primal Logic framework.

## Features

### NASA APIs
- **APOD** - Astronomy Picture of the Day
- **NeoWs** - Near Earth Object Web Service
- **EPIC** - Earth Polychromatic Imaging Camera
- **POWER** - Weather and Climate Data
- **Image Library** - NASA Media Library (140,000+ assets)
- **SSD** - Solar System Dynamics

### SpaceX/Starlink APIs
- **SpaceX API** - Launch, rocket, capsule, and Starlink satellite data
- **Starlink Metrics** - Public network performance data

## Installation

```bash
cd api_integrations
pip install -r requirements.txt
```

## Configuration

1. Copy the example environment file:
```bash
cp .env.example .env
```

2. Get your NASA API key from https://api.nasa.gov and add it to `.env`:
```env
NASA_API_KEY=your_key_here
```

Note: The `DEMO_KEY` is rate-limited. For production use, register for a free API key.

## Quick Start

### Using the Unified Manager

```python
from api_integrations import APIManager

# Initialize the manager
manager = APIManager()

# NASA APOD - Get today's astronomy picture
apod = manager.nasa_apod.get_today()
print(f"Today's APOD: {apod['title']}")

# NASA NeoWs - Get near Earth asteroids
asteroids = manager.nasa_neows.get_feed()
print(f"Found {asteroids['element_count']} asteroids")

# SpaceX - Get latest launch
launch = manager.spacex.get_latest_launch()
print(f"Latest launch: {launch['name']}")

# Starlink - Get network metrics
metrics = manager.starlink.get_residential_metrics()
print(f"Starlink metrics: {metrics}")
```

### Using Individual Clients

```python
from api_integrations.nasa import APODClient
from api_integrations.spacex import SpaceXClient

# NASA APOD client
apod_client = APODClient()
picture = apod_client.get_today()

# SpaceX client
spacex_client = SpaceXClient()
starlink_sats = spacex_client.get_starlink_satellites()
```

## Examples

See the `examples/` directory for detailed usage examples:

- `examples/nasa_apod_demo.py` - APOD API examples
- `examples/nasa_neows_demo.py` - Near Earth Object tracking
- `examples/spacex_demo.py` - SpaceX launch and rocket data
- `examples/starlink_demo.py` - Starlink satellite tracking
- `examples/comprehensive_demo.py` - Combined usage examples

## API Documentation

### NASA APOD Client

```python
# Get today's picture
today = manager.nasa_apod.get_today()

# Get picture by date
from datetime import date
picture = manager.nasa_apod.get_by_date(date(2024, 1, 1))

# Get date range
pictures = manager.nasa_apod.get_range(
    start_date=date(2024, 1, 1),
    end_date=date(2024, 1, 7)
)

# Get random pictures
random = manager.nasa_apod.get_random(count=5)
```

### NASA NeoWs Client

```python
# Get asteroid feed
feed = manager.nasa_neows.get_feed()

# Get specific asteroid
asteroid = manager.nasa_neows.get_by_id("3542519")

# Browse all asteroids
browse = manager.nasa_neows.browse(page=0, size=20)

# Get statistics
stats = manager.nasa_neows.get_statistics()
```

### SpaceX Client

```python
# Get latest launch
launch = manager.spacex.get_latest_launch()

# Get next launch
next_launch = manager.spacex.get_next_launch()

# Get Starlink satellites
starlink = manager.spacex.get_starlink_satellites(
    options={"limit": 100}
)

# Get company info
company = manager.spacex.get_company_info()
```

### Starlink Metrics Client

```python
# Get residential metrics
residential = manager.starlink.get_residential_metrics()

# Get maritime metrics
maritime = manager.starlink.get_maritime_metrics()

# Get all metrics
all_metrics = manager.starlink.get_all_metrics()
```

## Error Handling

All clients include built-in error handling and retry logic:

```python
from api_integrations.base_client import APIError, RateLimitError

try:
    data = manager.nasa_apod.get_today()
except RateLimitError:
    print("Rate limit exceeded, try again later")
except APIError as e:
    print(f"API error: {e}")
```

## Rate Limiting

- Built-in rate limiting to respect API quotas
- Configurable via `APIConfig`
- Automatic retry with exponential backoff

## Testing

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=api_integrations

# Run specific test file
pytest tests/test_nasa_apod.py
```

## Health Check

Check the status of all APIs:

```python
status = manager.health_check()
for api, state in status.items():
    print(f"{api}: {state}")
```

## Advanced Configuration

```python
from api_integrations import APIManager
from api_integrations.config import APIConfig

# Custom configuration
config = APIConfig(
    nasa_api_key="your_key",
    timeout=60,
    retry_attempts=5,
    requests_per_hour=500
)

manager = APIManager(config=config)
```

## Integration with MotorHandPro

These API clients are designed to complement the MotorHandPro Primal Logic framework:

- Fetch real-time space weather data for autonomous control systems
- Track near Earth objects for trajectory planning
- Access SpaceX/Starlink orbital data for satellite communication
- Integrate NASA climate data with robotic environmental sensors

## License

See main project NOTICE file. This integration suite follows the same license as MotorHandPro.

## Resources

- [NASA API Portal](https://api.nasa.gov)
- [SpaceX API Docs](https://github.com/r-spacex/SpaceX-API)
- [NASA POWER API](https://power.larc.nasa.gov/docs/)
- [NASA Image Library](https://images.nasa.gov)

## Support

For issues or questions:
1. Check the examples directory
2. Review API documentation links above
3. Contact the MotorHandPro team

---

**Part of the MotorHandPro Project**
High-precision robotic control meets space exploration data.
