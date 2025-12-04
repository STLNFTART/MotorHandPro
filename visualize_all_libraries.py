#!/usr/bin/env python3
"""
Master NASA Visualization Orchestrator
Generates visualizations using ALL 8 libraries

Usage:
    python3 visualize_all_libraries.py

Author: Donte Lightfoot
Date: December 4, 2025
"""

import sys
import json
import warnings
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Any

# Add paths
sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent / "network_simulation_cluster" / "data_sources"))

# Import NASA data structures
try:
    from nasa_comet_data import (
        NASACometDataClient,
        RecursivePlanckOperator,
        CometObservation,
        RecursivePlanckState
    )
    HAVE_NASA_DATA = True
except ImportError as e:
    HAVE_NASA_DATA = False
    warnings.warn(f"NASA data module not available: {e}")

# Import visualization libraries
from nasa_visualizations import AVAILABLE_LIBRARIES, print_library_status


class MasterVisualizationOrchestrator:
    """Orchestrates all visualization libraries"""

    def __init__(self, output_dir: str = "all_visualizations"):
        """Initialize orchestrator"""
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)

        self.observations = []
        self.states = []

        print("=" * 80)
        print("🎨 MASTER VISUALIZATION ORCHESTRATOR")
        print("=" * 80)
        print()
        print_library_status()
        print()

    def load_data_from_pipeline(self):
        """Load data from existing NASA pipeline output"""
        pipeline_dir = Path('nasa_live_output')

        if not pipeline_dir.exists():
            print("⚠️  No pipeline data found, generating simulated data...")
            return self.generate_simulated_data()

        # Get latest files
        obs_files = list(pipeline_dir.glob('observations_*.json'))
        state_files = list(pipeline_dir.glob('states_*.json'))

        if not obs_files or not state_files:
            print("⚠️  No data files found, generating simulated data...")
            return self.generate_simulated_data()

        latest_obs_file = max(obs_files, key=lambda f: f.stat().st_mtime)
        latest_state_file = max(state_files, key=lambda f: f.stat().st_mtime)

        print(f"📂 Loading data from pipeline...")
        print(f"   Observations: {latest_obs_file.name}")
        print(f"   States: {latest_state_file.name}")

        with open(latest_obs_file, 'r') as f:
            obs_data = json.load(f)

        with open(latest_state_file, 'r') as f:
            state_data = json.load(f)

        # Convert to objects
        from datetime import datetime

        for obs_dict in obs_data:
            obs = type('Observation', (), {})()
            obs.timestamp = datetime.fromisoformat(obs_dict['timestamp'])
            obs.ra = obs_dict['ra']
            obs.dec = obs_dict['dec']
            obs.distance_au = obs_dict['distance_au']
            obs.magnitude = obs_dict['magnitude']
            obs.velocity_km_s = obs_dict.get('velocity_km_s', 0)
            obs.elongation = obs_dict.get('elongation', 0)
            obs.phase_angle = obs_dict.get('phase_angle', 0)
            obs.gas_production_rate = obs_dict.get('gas_flux', 0) or obs_dict.get('gas_production_rate', 0)
            obs.tail_length_km = obs_dict.get('tail_length_km', 0)
            self.observations.append(obs)

        for state_dict in state_data:
            state = type('State', (), {})()
            ps = state_dict['primal_state']
            state.n = ps['n']
            state.signal = ps['signal']
            state.memory_integral = ps['memory_integral']
            state.error = ps['error']
            state.mu = 0.16905
            state.D = 149.9992314
            self.states.append(state)

        print(f"✅ Loaded {len(self.observations)} observations and {len(self.states)} states")
        return True

    def generate_simulated_data(self):
        """Generate simulated data for testing"""
        print("🔬 Generating simulated data...")

        if not HAVE_NASA_DATA:
            print("❌ NASA data module not available")
            return False

        client = NASACometDataClient()
        self.observations = client.simulate_live_feed(duration_hours=12.0, update_rate_hz=0.2)

        operator = RecursivePlanckOperator()
        self.states = []

        for obs in self.observations:
            state = operator.update(obs)
            # Create state object
            state_obj = type('State', (), {})()
            state_obj.n = state.n
            state_obj.signal = state.signal
            state_obj.memory_integral = state.memory_integral
            state_obj.error = state.error
            state_obj.mu = state.mu
            state_obj.D = state.D
            self.states.append(state_obj)

        print(f"✅ Generated {len(self.observations)} observations")
        return True

    def visualize_with_plotly(self):
        """Generate Plotly visualizations"""
        print("\n" + "=" * 80)
        print("📊 PLOTLY - Interactive 3D Dashboards")
        print("=" * 80)

        if not AVAILABLE_LIBRARIES['plotly']:
            print("❌ Plotly not available")
            return

        from nasa_data_visualization import NASADataVisualizer

        visualizer = NASADataVisualizer(output_dir=str(self.output_dir / "plotly"))
        report = visualizer.create_comprehensive_report(self.observations, use_live_data=False)

        print(f"✅ Generated {len(report['files'])} Plotly visualizations")

    def visualize_with_bokeh(self):
        """Generate Bokeh visualizations"""
        print("\n" + "=" * 80)
        print("📊 BOKEH - Real-Time 2D Streaming")
        print("=" * 80)

        if not AVAILABLE_LIBRARIES['bokeh']:
            print("❌ Bokeh not available - install with: pip install bokeh")
            return

        from nasa_visualizations.bokeh_realtime import BokehRealtimeVisualizer

        try:
            visualizer = BokehRealtimeVisualizer()
            output_path = self.output_dir / "bokeh" / "realtime_dashboard.html"
            output_path.parent.mkdir(exist_ok=True, parents=True)

            visualizer.create_realtime_dashboard(
                self.observations,
                self.states,
                str(output_path)
            )
            print(f"✅ Generated Bokeh dashboard: {output_path}")
        except Exception as e:
            print(f"⚠️  Bokeh visualization failed: {e}")

    def visualize_with_altair(self):
        """Generate Altair visualizations"""
        print("\n" + "=" * 80)
        print("📊 ALTAIR - Declarative Exploration")
        print("=" * 80)

        if not AVAILABLE_LIBRARIES['altair']:
            print("❌ Altair not available - install with: pip install altair vega_datasets")
            return

        from nasa_visualizations.altair_exploration import AltairExplorationVisualizer

        try:
            visualizer = AltairExplorationVisualizer()
            output_dir = self.output_dir / "altair"
            output_dir.mkdir(exist_ok=True, parents=True)

            # Exploration dashboard
            visualizer.create_exploration_dashboard(
                self.observations,
                self.states,
                str(output_dir / "exploration_dashboard.html")
            )

            # Interactive selection
            visualizer.create_interactive_selection(
                self.observations,
                self.states,
                str(output_dir / "interactive_selection.html")
            )

            print(f"✅ Generated 2 Altair visualizations")
        except Exception as e:
            print(f"⚠️  Altair visualization failed: {e}")

    def visualize_with_pyvista(self):
        """Generate PyVista visualizations"""
        print("\n" + "=" * 80)
        print("📊 PYVISTA - 3D Meshes & Celestial Bodies")
        print("=" * 80)

        if not AVAILABLE_LIBRARIES['pyvista']:
            print("❌ PyVista not available - install with: pip install pyvista")
            return

        from nasa_visualizations.pyvista_meshes import PyVistaComet3DVisualizer

        try:
            visualizer = PyVistaComet3DVisualizer()
            output_dir = self.output_dir / "pyvista"
            output_dir.mkdir(exist_ok=True, parents=True)

            # 3D scene
            visualizer.create_3d_scene(
                self.observations,
                self.states,
                str(output_dir / "3d_scene.png"),
                interactive=False
            )

            print(f"✅ Generated PyVista 3D visualization")
        except Exception as e:
            print(f"⚠️  PyVista visualization failed: {e}")

    def print_webapp_instructions(self):
        """Print instructions for web apps"""
        print("\n" + "=" * 80)
        print("🌐 WEB APPLICATIONS")
        print("=" * 80)
        print()

        if AVAILABLE_LIBRARIES['dash']:
            print("📊 DASH - Production Web Dashboard")
            print("   To run:")
            print("   python3 -c \"from nasa_visualizations.dash_webapp import *; app = DashNASAWebApp(); app.load_data(observations, states); app.run()\"")
            print("   Or create a script and run it")
            print()
        else:
            print("❌ Dash not available - install with: pip install dash")
            print()

        if AVAILABLE_LIBRARIES['streamlit']:
            print("📊 STREAMLIT - Rapid Prototyping")
            print("   To run:")
            print("   streamlit run nasa_visualizations/streamlit_app.py")
            print()
        else:
            print("❌ Streamlit not available - install with: pip install streamlit")
            print()

    def generate_all(self):
        """Generate visualizations using all available libraries"""
        print("=" * 80)
        print("🚀 GENERATING ALL VISUALIZATIONS")
        print("=" * 80)
        print()

        # Load data
        if not self.load_data_from_pipeline():
            print("❌ Failed to load data")
            return

        # Sample data for faster processing
        sample_size = min(1000, len(self.observations))
        step = max(1, len(self.observations) // sample_size)
        self.observations = self.observations[::step]
        self.states = self.states[::step]
        print(f"📊 Using {len(self.observations)} sampled points for faster generation")
        print()

        # Generate with each library
        self.visualize_with_plotly()
        self.visualize_with_bokeh()
        self.visualize_with_altair()
        self.visualize_with_pyvista()

        # Web app instructions
        self.print_webapp_instructions()

        # Summary
        print("\n" + "=" * 80)
        print("✅ VISUALIZATION GENERATION COMPLETE")
        print("=" * 80)
        print()
        print(f"📁 Output directory: {self.output_dir}")
        print()

        # List all generated files
        all_files = list(self.output_dir.rglob('*.*'))
        if all_files:
            total_size = sum(f.stat().st_size for f in all_files if f.is_file())
            total_size_mb = total_size / (1024 * 1024)

            print(f"📊 Generated {len(all_files)} files ({total_size_mb:.1f} MB)")
            print()

            # Group by library
            by_library = {}
            for f in all_files:
                lib = f.parent.name
                if lib not in by_library:
                    by_library[lib] = []
                by_library[lib].append(f)

            for lib, files in sorted(by_library.items()):
                lib_size = sum(f.stat().st_size for f in files) / (1024 * 1024)
                print(f"   {lib:12s}: {len(files):2d} files ({lib_size:>6.1f} MB)")
                for f in files[:3]:  # Show first 3 files
                    print(f"      - {f.name}")
                if len(files) > 3:
                    print(f"      ... and {len(files)-3} more")
                print()

        print("=" * 80)


def main():
    """Main entry point"""
    orchestrator = MasterVisualizationOrchestrator()
    orchestrator.generate_all()


if __name__ == "__main__":
    main()
