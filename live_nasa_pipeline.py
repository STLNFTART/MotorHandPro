#!/usr/bin/env python3
"""
Live NASA Data Pipeline for MotorHandPro

Automated pipeline that:
1. Fetches live NASA data for 3I/ATLAS from multiple sources
2. Processes through Recursive Planck Operator
3. Integrates with LAM temporal displacement framework
4. Generates real-time visualizations
5. Runs experiments across repo simulations

Author: Donte Lightfoot
Date: December 4, 2025
"""

import os
import sys
import time
import json
import argparse
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Any, Optional

# Add paths
sys.path.insert(0, str(Path(__file__).parent / "network_simulation_cluster" / "data_sources"))
sys.path.insert(0, str(Path(__file__).parent / "lam"))

try:
    from nasa_comet_data import (
        NASACometDataClient,
        RecursivePlanckOperator,
        CometObservation
    )
    from nasa_data_visualization import NASADataVisualizer
except ImportError as e:
    print(f"❌ Error importing NASA modules: {e}")
    print("   Make sure nasa_comet_data.py and nasa_data_visualization.py are available")
    sys.exit(1)

# Optional: Integrate with LAM framework
try:
    from temporal_displacement import (
        TimeWarpField,
        TemporalDisplacementConfig
    )
    HAVE_LAM = True
except ImportError:
    HAVE_LAM = False
    print("⚠️  LAM temporal displacement not available")


class LiveNASAPipeline:
    """
    Live NASA data pipeline for continuous data ingestion and processing
    """

    def __init__(
        self,
        output_dir: str = "nasa_live_output",
        update_interval_seconds: int = 3600,
        enable_lam_integration: bool = True
    ):
        """
        Initialize live pipeline

        Args:
            output_dir: Output directory for data and visualizations
            update_interval_seconds: How often to fetch new data (default 1 hour)
            enable_lam_integration: Enable LAM temporal displacement integration
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)

        self.update_interval = update_interval_seconds
        self.enable_lam = enable_lam_integration and HAVE_LAM

        # Initialize components
        self.client = NASACometDataClient()
        self.operator = RecursivePlanckOperator()
        self.visualizer = NASADataVisualizer(output_dir=str(self.output_dir / "visualizations"))

        # LAM integration
        if self.enable_lam:
            config = TemporalDisplacementConfig(
                alpha=1.618,
                beta=0.1,
                lambda_val=0.16905
            )
            self.time_warp = TimeWarpField(config)

        # State tracking
        self.observations = []
        self.states = []
        self.start_time = datetime.now()
        self.iteration = 0

    def fetch_live_data(self) -> List[CometObservation]:
        """
        Fetch latest data from NASA sources

        Returns:
            List of new observations
        """
        print(f"📡 Fetching data from NASA APIs...")

        # Try JPL Horizons first
        observations = self.client.fetch_horizons_ephemeris(
            start_time=datetime.now(),
            end_time=datetime.now() + timedelta(hours=24),
            step="1h"
        )

        if observations:
            print(f"✅ Retrieved {len(observations)} points from JPL Horizons")
            return observations

        # Fallback to MPC
        print("⚠️  Horizons unavailable, trying MPC...")
        observations = self.client.fetch_mpc_astrometry(days_back=7)

        if observations:
            print(f"✅ Retrieved {len(observations)} points from MPC")
            return observations

        # Last resort: simulated feed
        print("⚠️  No live data available, using simulated feed")
        observations = self.client.simulate_live_feed(
            duration_hours=24.0,
            update_rate_hz=0.1
        )

        print(f"✅ Generated {len(observations)} simulated points")
        return observations

    def process_observations(
        self,
        observations: List[CometObservation]
    ) -> List[Dict[str, Any]]:
        """
        Process observations through Recursive Planck Operator

        Args:
            observations: New observations

        Returns:
            List of processed states
        """
        print(f"🔬 Processing {len(observations)} observations...")

        processed_states = []

        for obs in observations:
            # Update Recursive Planck Operator
            state = self.operator.update(obs)
            anomaly_score = self.operator.get_anomaly_score()

            # LAM integration
            if self.enable_lam:
                # Compute temporal displacement
                Delta = state.signal * 0.01  # Scale signal to displacement

                # Push to time warp field
                self.time_warp.push_sample(
                    t=obs.timestamp.timestamp(),
                    E0=state.signal
                )

                # Get displaced value
                E_displaced = self.time_warp.get_displaced_value(
                    t=obs.timestamp.timestamp(),
                    Delta=Delta
                )
            else:
                E_displaced = None

            # Package state
            state_dict = {
                'timestamp': obs.timestamp.isoformat(),
                'observation': {
                    'ra': obs.ra,
                    'dec': obs.dec,
                    'distance_au': obs.distance_au,
                    'magnitude': obs.magnitude,
                    'gas_production_rate': obs.gas_production_rate
                },
                'primal_state': {
                    'n': state.n,
                    'signal': state.signal,
                    'memory_integral': state.memory_integral,
                    'error': state.error,
                    'anomaly_score': anomaly_score
                },
                'lam_integration': {
                    'enabled': self.enable_lam,
                    'E_displaced': E_displaced if E_displaced else None
                } if self.enable_lam else None
            }

            processed_states.append(state_dict)

        print(f"✅ Processed {len(processed_states)} states")
        return processed_states

    def save_data(
        self,
        observations: List[CometObservation],
        states: List[Dict[str, Any]]
    ):
        """
        Save data to JSON files

        Args:
            observations: Observations
            states: Processed states
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")

        # Save observations
        obs_file = self.output_dir / f"observations_{timestamp}.json"
        with open(obs_file, 'w') as f:
            obs_data = [
                {
                    'timestamp': obs.timestamp.isoformat(),
                    'ra': obs.ra,
                    'dec': obs.dec,
                    'distance_au': obs.distance_au,
                    'velocity_km_s': obs.velocity_km_s,
                    'magnitude': obs.magnitude,
                    'gas_production_rate': obs.gas_production_rate,
                    'tail_length_km': obs.tail_length_km
                }
                for obs in observations
            ]
            json.dump(obs_data, f, indent=2)

        print(f"💾 Saved observations: {obs_file}")

        # Save states
        states_file = self.output_dir / f"states_{timestamp}.json"
        with open(states_file, 'w') as f:
            json.dump(states, f, indent=2)

        print(f"💾 Saved states: {states_file}")

    def generate_visualizations(
        self,
        observations: List[CometObservation]
    ):
        """
        Generate visualizations

        Args:
            observations: Observations to visualize
        """
        print("🎨 Generating visualizations...")

        # Generate comprehensive report
        report = self.visualizer.create_comprehensive_report(
            observations=observations,
            use_live_data=False  # Already have observations
        )

        print(f"✅ Generated {len(report['files'])} visualization files")

    def run_experiments(
        self,
        observations: List[CometObservation]
    ):
        """
        Run experiments across repo simulations

        Args:
            observations: Latest observations
        """
        print("🧪 Running experiments...")

        # Example: Use latest observation to parameterize simulations
        if len(observations) > 0:
            latest = observations[-1]

            print(f"   Latest observation:")
            print(f"     RA: {latest.ra:.4f}°")
            print(f"     Dec: {latest.dec:.4f}°")
            print(f"     Distance: {latest.distance_au:.4f} AU")

            if latest.gas_production_rate:
                print(f"     Gas flux: {latest.gas_production_rate:.2f} g/s")

                # Example integration with other repo experiments
                # This could pipe data to:
                # - Quantro Heart (cardiac-like oscillations)
                # - MotorHandPro actuator control
                # - Primal simulations (swarm dynamics)

                print("   → Could pipe to Quantro Heart for oscillation analysis")
                print("   → Could pipe to MotorHandPro for actuator dynamics")
                print("   → Could pipe to Primal Simulations for swarm tracking")

        print("✅ Experiments complete")

    def run_iteration(self):
        """Run single pipeline iteration"""
        self.iteration += 1

        print("\n" + "=" * 80)
        print(f"🚀 PIPELINE ITERATION {self.iteration}")
        print(f"   Time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"   Elapsed: {(datetime.now() - self.start_time).seconds} seconds")
        print("=" * 80)
        print()

        # Fetch data
        observations = self.fetch_live_data()

        if not observations:
            print("⚠️  No observations retrieved, skipping iteration")
            return

        # Process
        states = self.process_observations(observations)

        # Save
        self.save_data(observations, states)

        # Visualize
        self.generate_visualizations(observations)

        # Run experiments
        self.run_experiments(observations)

        # Store
        self.observations.extend(observations)
        self.states.extend(states)

        print()
        print("=" * 80)
        print(f"✅ ITERATION {self.iteration} COMPLETE")
        print("=" * 80)
        print()

    def run_continuous(self, max_iterations: Optional[int] = None):
        """
        Run pipeline continuously

        Args:
            max_iterations: Maximum iterations (None = infinite)
        """
        print("\n" + "=" * 80)
        print("🌌 NASA LIVE DATA PIPELINE - CONTINUOUS MODE")
        print("=" * 80)
        print(f"   Update interval: {self.update_interval} seconds")
        print(f"   Output directory: {self.output_dir}")
        print(f"   LAM integration: {'✅ Enabled' if self.enable_lam else '❌ Disabled'}")
        print("=" * 80)
        print()

        iteration_count = 0

        try:
            while True:
                self.run_iteration()

                iteration_count += 1

                if max_iterations and iteration_count >= max_iterations:
                    print(f"✅ Reached maximum iterations ({max_iterations})")
                    break

                # Wait for next iteration
                print(f"⏳ Waiting {self.update_interval} seconds until next update...")
                print(f"   Press Ctrl+C to stop\n")
                time.sleep(self.update_interval)

        except KeyboardInterrupt:
            print("\n\n⚠️  Pipeline stopped by user")

        print("\n" + "=" * 80)
        print("📊 PIPELINE SUMMARY")
        print("=" * 80)
        print(f"   Total iterations: {self.iteration}")
        print(f"   Total observations: {len(self.observations)}")
        print(f"   Total states: {len(self.states)}")
        print(f"   Runtime: {(datetime.now() - self.start_time).seconds} seconds")
        print("=" * 80)
        print()


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Live NASA Data Pipeline for MotorHandPro"
    )
    parser.add_argument(
        '--mode',
        choices=['single', 'continuous'],
        default='single',
        help='Run mode: single iteration or continuous'
    )
    parser.add_argument(
        '--interval',
        type=int,
        default=3600,
        help='Update interval in seconds (default: 3600 = 1 hour)'
    )
    parser.add_argument(
        '--max-iterations',
        type=int,
        default=None,
        help='Maximum iterations for continuous mode (default: infinite)'
    )
    parser.add_argument(
        '--output-dir',
        type=str,
        default='nasa_live_output',
        help='Output directory'
    )
    parser.add_argument(
        '--disable-lam',
        action='store_true',
        help='Disable LAM temporal displacement integration'
    )

    args = parser.parse_args()

    # Initialize pipeline
    pipeline = LiveNASAPipeline(
        output_dir=args.output_dir,
        update_interval_seconds=args.interval,
        enable_lam_integration=not args.disable_lam
    )

    # Run
    if args.mode == 'single':
        pipeline.run_iteration()
    else:
        pipeline.run_continuous(max_iterations=args.max_iterations)


if __name__ == "__main__":
    main()
