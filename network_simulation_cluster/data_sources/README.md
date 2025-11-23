# Real-Time Data Source Integration

Production-grade data integration for PRIMAL Network Simulations from authoritative sources in St. Louis and beyond.

## 🎯 Overview

This module integrates three major real-time data sources:

1. **🏦 FRED** - Federal Reserve Economic Data (St. Louis Fed)
2. **🗺️ USGS** - United States Geological Survey (Terrain/Elevation)
3. **🛰️ Space-Track.org** - Satellite Two-Line Element (TLE) Data

---

## 📊 Data Sources

### 1. Federal Reserve Economic Data (FRED)

**Provider:** Federal Reserve Bank of St. Louis
**Location:** 1 Federal Reserve Bank Plaza, St. Louis, MO
**Website:** https://fred.stlouisfed.org

#### Features:
- **820,000+ economic time series** from 109 sources
- **Real-time updates** (some series updated hourly)
- **Free API** with simple registration
- **St. Louis regional data** included

#### Key Indicators:
- Financial Stress Index (STLFSI4)
- Unemployment Rate (UNRATE)
- VIX Volatility Index (VIXCLS)
- Credit Spreads (BAMLH0A0HYM2)
- TED Spread (TEDRATE)
- St. Louis GDP, unemployment, housing prices

#### Setup:
```bash
# 1. Register for free API key
# Visit: https://fred.stlouisfed.org/docs/api/api_key.html

# 2. Set environment variable
export FRED_API_KEY='your_api_key_here'

# 3. Test integration
python -m network_simulation_cluster.data_sources.fred_integration
```

#### Usage:
```python
from network_simulation_cluster.data_sources import FREDClient

# Initialize client
fred = FREDClient(api_key='your_key')

# Get economic threat score
threat = fred.get_economic_threat_score()
print(f"Economic Threat: {threat['composite_threat']:.3f}")
print(f"Classification: {threat['threat_level']}")

# Get St. Louis regional data
stl_data = fred.get_stl_regional_data()
print(f"STL Data: {stl_data}")

# Simulate financial contagion
contagion = fred.simulate_financial_contagion(initial_shock=0.5)
print(f"Contagion amplification: {contagion['amplification_factor']:.2f}x")
```

#### PRIMAL Logic Enhancement:
```python
from network_simulation_cluster.data_sources import FREDEnhancedPrimalLogic

primal_fred = FREDEnhancedPrimalLogic(api_key='your_key')

# Calculate enhanced threat combining network + economic
enhanced = primal_fred.calculate_enhanced_threat(network_threat=0.4)
print(f"Combined Threat: {enhanced['combined_threat']:.3f}")

# Adjust network parameters based on economic conditions
params = primal_fred.adjust_network_parameters(
    base_failure_rate=0.01,
    base_recovery_rate=0.15
)
print(f"Adjusted Failure Rate: {params['failure_rate']:.4f}")
```

---

### 2. USGS Elevation/Terrain Data

**Provider:** United States Geological Survey
**Website:** https://www.usgs.gov
**API:** https://apps.nationalmap.gov/epqs/

#### Features:
- **No API key required** - Free public service
- **Multiple resolutions** (1m, 3m, 10m, 30m)
- **Nationwide coverage** (United States)
- **Real-time elevation queries**

#### Datasets:
- 3DEP (3D Elevation Program) - Modern high-resolution
- NED (National Elevation Dataset) - Legacy coverage

#### Setup:
```bash
# No setup required - public API
python -m network_simulation_cluster.data_sources.usgs_terrain
```

#### Usage:
```python
from network_simulation_cluster.data_sources import USGSElevationClient

# Initialize client
usgs = USGSElevationClient()

# Get single point elevation (St. Louis Gateway Arch)
elevation = usgs.get_elevation(38.6247, -90.1848)
print(f"Gateway Arch elevation: {elevation.elevation_meters:.1f}m")

# Get elevation grid (for UAV swarm planning)
grid = usgs.get_elevation_grid(
    center_lat=38.6270,
    center_lon=-90.1994,
    radius_km=10,
    grid_points=10
)
print(f"Retrieved {len(grid)} elevation points")

# Get elevation profile along a path
profile = usgs.get_elevation_profile(
    start_lat=38.6270, start_lon=-90.1994,  # Downtown STL
    end_lat=38.7487, end_lon=-90.3700,      # Lambert Airport
    num_samples=20
)

# Calculate terrain roughness
roughness = usgs.calculate_terrain_roughness(38.6270, -90.1994, radius_km=5)
print(f"Terrain roughness: {roughness:.3f}")

# Get minimum safe UAV altitude
safe_alt = usgs.get_min_safe_altitude_agl(38.6270, -90.1994, clearance_meters=100)
print(f"Minimum safe altitude: {safe_alt:.1f}m MSL")
```

#### Terrain-Aware Network Planning:
```python
from network_simulation_cluster.data_sources import TerrainAwareNetworkPlanner

planner = TerrainAwareNetworkPlanner()

# Plan node placement considering terrain
nodes = planner.plan_node_placement(
    region_lat=38.6270,
    region_lon=-90.1994,
    num_nodes=50,
    radius_km=10
)

# Check line-of-sight between nodes
los = planner.calculate_line_of_sight(
    node1={'latitude': 38.6, 'longitude': -90.2, 'elevation_m': 150},
    node2={'latitude': 38.7, 'longitude': -90.3, 'elevation_m': 200},
    antenna_height_m=10
)
print(f"Line-of-sight: {'✅ CLEAR' if los else '❌ BLOCKED'}")
```

---

### 3. Space-Track.org Satellite Data

**Provider:** U.S. Space Force (18th Space Defense Squadron)
**Website:** https://www.space-track.org

#### Features:
- **Real satellite TLE data** (Two-Line Elements)
- **50,000+ active satellites** tracked
- **Free registration** required
- **Major constellations** (Starlink, OneWeb, GPS, etc.)

#### Constellations Available:
- **Starlink** (~5,000 satellites)
- **OneWeb** (~600 satellites)
- **GPS** (32 satellites)
- **Galileo** (30 satellites)
- **Glonass** (24 satellites)
- **Beidou** (35 satellites)
- **Iridium** (75 satellites)

#### Setup:
```bash
# 1. Register for free account
# Visit: https://www.space-track.org/auth/createAccount

# 2. Set environment variables
export SPACETRACK_USER='your_username'
export SPACETRACK_PASS='your_password'

# 3. Test integration
python -m network_simulation_cluster.data_sources.spacetrack_satellites
```

#### Usage:
```python
from network_simulation_cluster.data_sources import SpaceTrackClient

# Initialize client
spacetrack = SpaceTrackClient(username='user', password='pass')

# Get single satellite TLE (ISS)
iss = spacetrack.get_tle_by_norad_id(25544)
print(f"ISS: {iss.name}")
print(f"Altitude: {iss.perigee:.1f} - {iss.apogee:.1f} km")
print(f"Period: {iss.period:.2f} minutes")

# Get constellation
starlink = spacetrack.get_constellation('STARLINK', limit=100)
print(f"Retrieved {len(starlink)} Starlink satellites")

# Get statistics
stats = spacetrack.get_satellite_statistics(starlink)
print(f"Average altitude: {stats['avg_perigee_km']:.1f} km")
print(f"Orbital shells: {stats['orbital_shells']}")

# Get active satellites
active = spacetrack.get_active_satellites(limit=1000)
print(f"Active satellites: {len(active)}")
```

#### Satellite Network Simulation:
```python
from network_simulation_cluster.data_sources import SatelliteNetworkSimulator

simulator = SatelliteNetworkSimulator(spacetrack)

# Create satellite network from real constellation
network = simulator.create_satellite_network('STARLINK', num_satellites=50)
print(f"Created network with {len(network['nodes'])} satellite nodes")

# Estimate global coverage
coverage = simulator.estimate_global_coverage(starlink[:50])
print(f"Global coverage: {coverage['coverage_percent']:.1f}%")

# Calculate coverage area for single satellite
area = simulator.calculate_coverage_area(altitude_km=550, min_elevation_deg=10)
print(f"Coverage area: {area:,.0f} km²")
```

---

## 🚀 Integrated Usage

### Real-Time Enhanced Network Simulator

Combine all data sources for production-grade network simulation:

```python
from network_simulation_cluster import RealTimeEnhancedNetwork

# Create enhanced simulator
network = RealTimeEnhancedNetwork(
    num_nodes=10000,
    enable_economic_data=True,      # FRED integration
    enable_terrain_data=True,        # USGS integration
    enable_satellite_data=True,      # Space-Track integration
    fred_api_key='your_fred_key'
)

# Print data source status
network.print_data_source_status()

# Get real-time threat assessment (network + economic)
threat = network.get_real_time_threat_assessment()
print(f"Combined Threat: {threat['combined_threat']:.3f}")
print(f"Classification: {threat['threat_classification']}")

# Apply terrain-aware node placement
network.apply_terrain_aware_node_placement()

# Integrate real satellite constellation
network.integrate_satellite_network('STARLINK', num_satellites=100)

# Run simulation with real-time data
network.simulate_network_activity(
    duration_seconds=1200,  # 20 minutes
    variant_name="REAL_TIME_INTEGRATED"
)
```

---

## 📈 Use Cases

### 1. Economic Network Resilience
```python
# Test network behavior under economic stress
fred = FREDClient(api_key='key')
primal_fred = FREDEnhancedPrimalLogic(api_key='key')

# Adjust network parameters based on current economic conditions
params = primal_fred.adjust_network_parameters(
    base_failure_rate=0.01,
    base_recovery_rate=0.15
)

# Run simulation with economically-adjusted parameters
network = EnhancedDistributedNetwork(
    num_nodes=10000,
    failure_rate=params['failure_rate'],
    recovery_rate=params['recovery_rate']
)
```

### 2. Terrain-Aware UAV Swarm
```python
# Plan UAV swarm with real terrain awareness
usgs = USGSElevationClient()
planner = TerrainAwareNetworkPlanner(usgs)

# Get terrain for St. Louis region
nodes = planner.plan_node_placement(38.6270, -90.1994, num_nodes=500, radius_km=50)

# Ensure minimum safe altitude for each UAV
for node in nodes:
    node['safe_altitude'] = node['elevation_m'] + 100  # 100m clearance
```

### 3. Satellite-Ground Hybrid Network
```python
# Create hybrid network with satellites and ground nodes
spacetrack = SpaceTrackClient(username='user', password='pass')
simulator = SatelliteNetworkSimulator(spacetrack)

# Add Starlink constellation
starlink_network = simulator.create_satellite_network('STARLINK', 100)

# Add GPS constellation for positioning
gps_network = simulator.create_satellite_network('GPS', 32)

# Combine with ground network for resilience testing
```

### 4. Financial Contagion Modeling
```python
# Model financial network contagion with real economic data
fred = FREDClient(api_key='key')

# Simulate contagion under current conditions
contagion = fred.simulate_financial_contagion(initial_shock=0.5)
print(f"Initial shock: 50%")
print(f"Final impact: {contagion['final_impact']*100:.1f}%")
print(f"Amplification: {contagion['amplification_factor']:.2f}x")
```

---

## 🔒 Security & API Keys

### Environment Variables
```bash
# FRED API Key (free registration)
export FRED_API_KEY='your_fred_key'

# Space-Track credentials (free registration)
export SPACETRACK_USER='your_username'
export SPACETRACK_PASS='your_password'

# USGS - no credentials needed (public API)
```

### Configuration File (Optional)
```python
# config.py
import os

FRED_API_KEY = os.getenv('FRED_API_KEY', '')
SPACETRACK_USER = os.getenv('SPACETRACK_USER', '')
SPACETRACK_PASS = os.getenv('SPACETRACK_PASS', '')
```

---

## 📊 Data Freshness

| Source | Update Frequency | Cache Duration |
|--------|-----------------|----------------|
| FRED Economic | Varies (hourly to monthly) | 1 hour |
| USGS Terrain | Static (updated periodically) | 24 hours |
| Space-Track TLE | Daily | 1 hour |

---

## 🎖️ St. Louis Partnerships

### Potential Collaborations

#### Federal Reserve Bank of St. Louis
- **Research Partnership:** Economic network resilience modeling
- **Data Access:** Full FRED database with API
- **Publication:** Joint research on financial contagion using PRIMAL Logic
- **Contact:** FRED Support - https://fred.stlouisfed.org/contactus/

#### National Geospatial-Intelligence Agency (NGA West)
- **Location:** 3200 South 2nd Street, St. Louis, MO
- **Unclassified Research:** GEOINT Services partnership
- **Innovation Program:** https://www.nga.mil/innovation.html
- **Academic Partnerships:** Research agreements for unclassified work

---

## 🚧 Future Enhancements

### Additional Data Sources (Planned)

1. **NOAA Weather Data**
   - Real-time weather conditions
   - Space weather (solar activity)
   - Integration with satellite/UAV operations

2. **OpenStreetMap**
   - Road networks for ground-based simulations
   - Building heights for urban propagation
   - Geographic features

3. **FAA NOTAM System**
   - No-fly zones
   - Temporary flight restrictions
   - Airspace status

4. **Census Bureau Data**
   - Population density
   - Demographic data
   - Regional statistics

---

## 📝 Testing

Run all integration tests:

```bash
# Test FRED integration
python -m network_simulation_cluster.data_sources.fred_integration

# Test USGS integration
python -m network_simulation_cluster.data_sources.usgs_terrain

# Test Space-Track integration
python -m network_simulation_cluster.data_sources.spacetrack_satellites

# Test real-time enhanced simulator
python -m network_simulation_cluster.enhanced_realtime_simulator
```

---

## 📖 References

### FRED
- API Documentation: https://fred.stlouisfed.org/docs/api/
- Data Sources: https://fred.stlouisfed.org/sources
- Research Papers: https://research.stlouisfed.org/

### USGS
- Elevation API: https://apps.nationalmap.gov/epqs/
- 3DEP Program: https://www.usgs.gov/3d-elevation-program
- Data Access: https://www.usgs.gov/products/data-and-tools/data-and-tools-topics

### Space-Track.org
- Documentation: https://www.space-track.org/documentation
- API Guide: https://www.space-track.org/documentation#api
- Registration: https://www.space-track.org/auth/createAccount

---

## 📄 License

Data source APIs are subject to their respective terms of service:
- FRED: Free for non-commercial use
- USGS: Public domain (U.S. Government)
- Space-Track: Free registration, terms apply

This integration code: MIT License

---

## 🤝 Contributing

Contributions welcome! Please submit pull requests with:
- New data source integrations
- Performance improvements
- Additional use cases
- Documentation enhancements

---

**Built with ❤️ in St. Louis, MO for the PRIMAL Logic Framework**
