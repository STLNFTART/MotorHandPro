#!/usr/bin/env python3
"""
Enhanced Real-Time Network Simulator with Live Data Integration

Extends the PRIMAL ATAK Network Simulator with real-time data from:
- Federal Reserve Economic Data (FRED) - St. Louis
- USGS Terrain/Elevation Data
- Space-Track.org Satellite TLE Data

This provides production-grade intelligence for network simulations.
"""

import os
from typing import Optional, Dict, Any
from datetime import datetime

# Import base simulator
from .primal_atak_network_simulator import EnhancedDistributedNetwork, PrimalLogicCore

# Import data sources
try:
    from .data_sources import (
        FREDClient,
        FREDEnhancedPrimalLogic,
        USGSElevationClient,
        TerrainAwareNetworkPlanner,
        SpaceTrackClient,
        SatelliteNetworkSimulator
    )
    DATA_SOURCES_AVAILABLE = True
except ImportError:
    DATA_SOURCES_AVAILABLE = False
    print("⚠️ Data sources not available. Install with: pip install requests")


class RealTimeEnhancedNetwork(EnhancedDistributedNetwork):
    """Enhanced network simulator with real-time data integration"""

    def __init__(self, num_nodes=10000, failure_rate=0.01, recovery_rate=0.15,
                 redundancy_factor=20,
                 fred_api_key: Optional[str] = None,
                 spacetrack_user: Optional[str] = None,
                 spacetrack_pass: Optional[str] = None,
                 enable_economic_data: bool = True,
                 enable_terrain_data: bool = True,
                 enable_satellite_data: bool = False):
        """
        Initialize real-time enhanced network simulator

        Args:
            num_nodes: Number of network nodes
            failure_rate: Base failure rate
            recovery_rate: Base recovery rate
            redundancy_factor: Redundancy factor for routing
            fred_api_key: FRED API key (or from FRED_API_KEY env var)
            spacetrack_user: Space-Track username (or from SPACETRACK_USER env var)
            spacetrack_pass: Space-Track password (or from SPACETRACK_PASS env var)
            enable_economic_data: Enable FRED economic data integration
            enable_terrain_data: Enable USGS terrain integration
            enable_satellite_data: Enable Space-Track satellite data
        """
        # Initialize base simulator
        super().__init__(num_nodes, failure_rate, recovery_rate, redundancy_factor)

        # Real-time data sources
        self.fred_client = None
        self.fred_primal = None
        self.usgs_client = None
        self.terrain_planner = None
        self.spacetrack_client = None
        self.satellite_sim = None

        # Data integration flags
        self.economic_data_enabled = False
        self.terrain_data_enabled = False
        self.satellite_data_enabled = False

        if not DATA_SOURCES_AVAILABLE:
            print("⚠️ Real-time data sources unavailable. Running in offline mode.")
            return

        # Initialize FRED economic data
        if enable_economic_data:
            try:
                print("🏦 Initializing FRED economic data integration...")
                self.fred_client = FREDClient(fred_api_key)
                self.fred_primal = FREDEnhancedPrimalLogic(fred_api_key)
                self.economic_data_enabled = True
                print("✅ FRED economic data enabled")
            except Exception as e:
                print(f"⚠️ FRED initialization failed: {e}")
                print("   Get API key at: https://fred.stlouisfed.org/docs/api/api_key.html")

        # Initialize USGS terrain data
        if enable_terrain_data:
            try:
                print("🗺️  Initializing USGS terrain data integration...")
                self.usgs_client = USGSElevationClient()
                self.terrain_planner = TerrainAwareNetworkPlanner(self.usgs_client)
                self.terrain_data_enabled = True
                print("✅ USGS terrain data enabled")
            except Exception as e:
                print(f"⚠️ USGS initialization failed: {e}")

        # Initialize Space-Track satellite data
        if enable_satellite_data:
            try:
                print("🛰️  Initializing Space-Track satellite data integration...")
                self.spacetrack_client = SpaceTrackClient(spacetrack_user, spacetrack_pass)
                self.satellite_sim = SatelliteNetworkSimulator(self.spacetrack_client)
                self.satellite_data_enabled = True
                print("✅ Space-Track satellite data enabled")
            except Exception as e:
                print(f"⚠️ Space-Track initialization failed: {e}")
                print("   Register at: https://www.space-track.org/auth/createAccount")

    def _update_node_states_with_primal_logic(self):
        """Override to include real-time economic conditions"""
        # Get economic stress if available
        if self.economic_data_enabled and self.fred_primal:
            try:
                # Adjust network parameters based on economic conditions
                adjusted_params = self.fred_primal.adjust_network_parameters(
                    self.failure_rate,
                    self.recovery_rate
                )

                # Temporarily adjust rates
                original_failure = self.failure_rate
                original_recovery = self.recovery_rate

                self.failure_rate = adjusted_params['failure_rate']
                self.recovery_rate = adjusted_params['recovery_rate']

                # Log economic influence
                if hasattr(self, '_economic_log_counter'):
                    self._economic_log_counter += 1
                else:
                    self._economic_log_counter = 0

                if self._economic_log_counter % 500 == 0:
                    print(f"📊 Economic Adjustment: "
                          f"Failure={adjusted_params['failure_rate']:.4f}, "
                          f"Recovery={adjusted_params['recovery_rate']:.4f}, "
                          f"Stress={adjusted_params['economic_stress']:.3f}")

            except Exception as e:
                print(f"⚠️ Economic data update error: {e}")

        # Call parent method
        super()._update_node_states_with_primal_logic()

        # Restore original rates if modified
        if self.economic_data_enabled:
            self.failure_rate = original_failure
            self.recovery_rate = original_recovery

    def get_real_time_threat_assessment(self) -> Dict[str, Any]:
        """Get comprehensive threat assessment combining network and real-world data"""
        assessment = {
            'network_threat': self.primal_logic.threat_level,
            'network_consciousness': self.primal_logic.network_consciousness,
            'network_health': self.network_stats['network_health'],
            'timestamp': datetime.now().isoformat()
        }

        # Add economic threat if available
        if self.economic_data_enabled and self.fred_client:
            try:
                economic_threat = self.fred_client.get_economic_threat_score()
                assessment['economic_threat'] = economic_threat['composite_threat']
                assessment['economic_indicators'] = economic_threat['indicators']
                assessment['economic_classification'] = economic_threat['threat_level']

                # Combined threat using PRIMAL Logic
                enhanced = self.fred_primal.calculate_enhanced_threat(
                    self.primal_logic.threat_level
                )
                assessment['combined_threat'] = enhanced['combined_threat']
                assessment['threat_classification'] = enhanced['threat_classification']

            except Exception as e:
                print(f"⚠️ Economic threat assessment error: {e}")

        return assessment

    def get_stl_regional_context(self) -> Dict[str, Any]:
        """Get St. Louis regional economic and geographic context"""
        context = {
            'region': 'St. Louis, MO',
            'timestamp': datetime.now().isoformat()
        }

        # Economic data
        if self.economic_data_enabled and self.fred_client:
            try:
                stl_data = self.fred_client.get_stl_regional_data()
                context['economic_data'] = stl_data['data']
            except Exception as e:
                print(f"⚠️ STL economic data error: {e}")

        # Terrain data (Gateway Arch area)
        if self.terrain_data_enabled and self.usgs_client:
            try:
                gateway_arch = self.usgs_client.get_elevation(38.6247, -90.1848)
                if gateway_arch:
                    context['elevation_gateway_arch'] = {
                        'meters': gateway_arch.elevation_meters,
                        'feet': gateway_arch.elevation_feet,
                        'data_source': gateway_arch.data_source
                    }

                # Terrain roughness
                roughness = self.usgs_client.calculate_terrain_roughness(
                    38.6270, -90.1994, radius_km=10
                )
                context['terrain_roughness'] = roughness

            except Exception as e:
                print(f"⚠️ STL terrain data error: {e}")

        return context

    def simulate_financial_contagion(self, initial_shock: float = 0.5) -> Dict[str, Any]:
        """
        Simulate financial contagion using real economic data

        Args:
            initial_shock: Initial shock magnitude (0-1)

        Returns:
            Contagion simulation results
        """
        if not self.economic_data_enabled or not self.fred_client:
            return {'error': 'Economic data not enabled'}

        try:
            return self.fred_client.simulate_financial_contagion(initial_shock)
        except Exception as e:
            return {'error': str(e)}

    def apply_terrain_aware_node_placement(self):
        """Update node positions with real terrain data"""
        if not self.terrain_data_enabled or not self.terrain_planner:
            print("⚠️ Terrain data not available")
            return

        print("🗺️  Applying terrain-aware node placement...")

        # St. Louis region center
        stl_lat = 38.6270
        stl_lon = -90.1994

        try:
            # Get terrain-optimized placements
            placements = self.terrain_planner.plan_node_placement(
                stl_lat, stl_lon,
                num_nodes=min(100, self.num_nodes),  # Sample subset
                radius_km=20
            )

            # Update node data with terrain information
            for placement in placements:
                node_id = placement['node_id']
                if node_id in self.nodes:
                    self.nodes[node_id].update({
                        'latitude': placement['latitude'],
                        'longitude': placement['longitude'],
                        'elevation_m': placement['elevation_m'],
                        'elevation_ft': placement['elevation_ft'],
                        'terrain_aware': True
                    })

            print(f"✅ Updated {len(placements)} nodes with terrain data")

        except Exception as e:
            print(f"⚠️ Terrain placement error: {e}")

    def integrate_satellite_network(self, constellation: str = 'STARLINK',
                                   num_satellites: int = 50):
        """
        Integrate real satellite constellation into simulation

        Args:
            constellation: Constellation name (STARLINK, GPS, etc.)
            num_satellites: Number of satellites to include
        """
        if not self.satellite_data_enabled or not self.satellite_sim:
            print("⚠️ Satellite data not available")
            return

        print(f"🛰️  Integrating {constellation} constellation...")

        try:
            network = self.satellite_sim.create_satellite_network(
                constellation, num_satellites
            )

            if 'error' not in network:
                # Add satellite nodes to network
                for sat_node in network['nodes']:
                    node_id = sat_node['node_id']
                    self.nodes[node_id] = {
                        'health': 1.0,
                        'latency': sat_node['period'] * 10,  # Period-based latency
                        'last_seen': datetime.now().isoformat(),
                        'connections': 20,
                        'load': 0.1,
                        'region': f"satellite-{constellation}",
                        'status': 'active',
                        'recovery_cooldown': 0,
                        'failure_count': 0,
                        'last_recovery': None,
                        'neighbors': [],
                        'backup_routes': [],
                        'processing_power': 0.8,
                        'bandwidth': 100,
                        'uptime': 0.99,
                        'satellite_data': sat_node
                    }

                print(f"✅ Integrated {len(network['nodes'])} satellite nodes")
                print(f"   Statistics: {network['statistics']}")

                return network
            else:
                print(f"❌ Satellite integration error: {network['error']}")

        except Exception as e:
            print(f"⚠️ Satellite integration error: {e}")

    def print_data_source_status(self):
        """Print status of all data source integrations"""
        print("\n📡 Real-Time Data Source Status:")
        print("=" * 80)
        print(f"🏦 FRED Economic Data: {'✅ ENABLED' if self.economic_data_enabled else '❌ DISABLED'}")
        print(f"🗺️  USGS Terrain Data: {'✅ ENABLED' if self.terrain_data_enabled else '❌ DISABLED'}")
        print(f"🛰️  Space-Track Satellites: {'✅ ENABLED' if self.satellite_data_enabled else '❌ DISABLED'}")

        if self.economic_data_enabled:
            try:
                threat = self.get_real_time_threat_assessment()
                print(f"\n💰 Current Economic Conditions:")
                if 'economic_threat' in threat:
                    print(f"   Threat Level: {threat['economic_classification']}")
                    print(f"   Threat Score: {threat['economic_threat']:.3f}")
                if 'combined_threat' in threat:
                    print(f"   Combined (Network + Economic): {threat['combined_threat']:.3f}")
            except Exception as e:
                print(f"   ⚠️ Error fetching economic data: {e}")

        if self.terrain_data_enabled:
            try:
                context = self.get_stl_regional_context()
                if 'elevation_gateway_arch' in context:
                    arch_elev = context['elevation_gateway_arch']
                    print(f"\n🗺️  St. Louis Terrain:")
                    print(f"   Gateway Arch elevation: {arch_elev['meters']:.1f}m")
                    print(f"   Terrain roughness: {context.get('terrain_roughness', 0):.3f}")
            except Exception as e:
                print(f"   ⚠️ Error fetching terrain data: {e}")

        print("=" * 80)


# Example usage
if __name__ == "__main__":
    print("🌐 Real-Time Enhanced Network Simulator")
    print("=" * 80)

    # Create enhanced simulator with real-time data
    network = RealTimeEnhancedNetwork(
        num_nodes=1000,  # Smaller for demo
        enable_economic_data=True,
        enable_terrain_data=True,
        enable_satellite_data=False  # Requires credentials
    )

    # Print data source status
    network.print_data_source_status()

    # Get real-time threat assessment
    print("\n🎯 Real-Time Threat Assessment:")
    print("-" * 80)
    threat = network.get_real_time_threat_assessment()
    for key, value in threat.items():
        if isinstance(value, dict):
            print(f"{key}:")
            for k, v in value.items():
                print(f"  {k}: {v}")
        else:
            print(f"{key}: {value}")

    # Get St. Louis context
    print("\n🏙️  St. Louis Regional Context:")
    print("-" * 80)
    context = network.get_stl_regional_context()
    for key, value in context.items():
        if isinstance(value, dict):
            print(f"{key}:")
            for k, v in value.items():
                print(f"  {k}: {v}")
        else:
            print(f"{key}: {value}")

    # Optionally run simulation
    run_sim = input("\n▶️  Run 2-minute simulation? (y/n): ").lower()
    if run_sim == 'y':
        network.simulate_network_activity(duration_seconds=120, variant_name="REAL_TIME_DEMO")
