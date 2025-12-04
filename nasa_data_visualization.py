#!/usr/bin/env python3
"""
Enhanced NASA Data Visualization for 3I/ATLAS
Using Plotly, Vispy, and PyVista for publication-quality interactive visualizations

Integrates with MotorHandPro's Primal Logic framework and live NASA data streams

Author: Donte Lightfoot
Date: December 4, 2025
"""

import numpy as np
import warnings
from typing import List, Dict, Optional, Tuple, Any
from datetime import datetime, timedelta
from pathlib import Path

# Core dependencies
try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    HAVE_PLOTLY = True
except ImportError:
    HAVE_PLOTLY = False
    warnings.warn("Plotly not available - install with: pip install plotly")

# Optional advanced visualization
try:
    import pyvista as pv
    HAVE_PYVISTA = True
except ImportError:
    HAVE_PYVISTA = False

try:
    from vispy import scene, app
    from vispy.visuals import markers
    HAVE_VISPY = False  # Disable for now - requires OpenGL
except ImportError:
    HAVE_VISPY = False

# Fallback to matplotlib
try:
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    from matplotlib.animation import FuncAnimation
    HAVE_MPL = True
except ImportError:
    HAVE_MPL = False

# Import our NASA data client
import sys
sys.path.insert(0, str(Path(__file__).parent / "network_simulation_cluster" / "data_sources"))
from nasa_comet_data import (
    CometObservation,
    NASACometDataClient,
    RecursivePlanckOperator,
    RecursivePlanckState
)


class NASADataVisualizer:
    """
    Advanced visualization for NASA comet data and Primal Logic simulations
    """

    def __init__(self, output_dir: str = "nasa_visualizations"):
        """
        Initialize visualizer

        Args:
            output_dir: Directory for saving visualizations
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)

        self.client = NASACometDataClient()
        self.operator = RecursivePlanckOperator()

    def create_interactive_sky_chart(
        self,
        observations: List[CometObservation],
        save_path: Optional[str] = None
    ) -> Optional[go.Figure]:
        """
        Create interactive 3D sky chart with comet trajectory

        Args:
            observations: List of comet observations
            save_path: Path to save HTML file (None = show only)

        Returns:
            Plotly figure object
        """
        if not HAVE_PLOTLY:
            warnings.warn("Plotly not available - skipping interactive sky chart")
            return None

        if len(observations) == 0:
            warnings.warn("No observations to plot")
            return None

        # Extract data
        ra_vals = [obs.ra for obs in observations]
        dec_vals = [obs.dec for obs in observations]
        distances = [obs.distance_au for obs in observations]
        magnitudes = [obs.magnitude for obs in observations]
        timestamps = [obs.timestamp.strftime("%Y-%m-%d %H:%M") for obs in observations]

        # Convert RA/Dec to Cartesian coordinates
        ra_rad = np.deg2rad(ra_vals)
        dec_rad = np.deg2rad(dec_vals)
        distances_arr = np.array(distances)

        # Spherical to Cartesian
        x = distances_arr * np.cos(dec_rad) * np.cos(ra_rad)
        y = distances_arr * np.cos(dec_rad) * np.sin(ra_rad)
        z = distances_arr * np.sin(dec_rad)

        # Create 3D scatter plot
        fig = go.Figure()

        # Comet trajectory
        fig.add_trace(go.Scatter3d(
            x=x,
            y=y,
            z=z,
            mode='lines+markers',
            name='3I/ATLAS Trajectory',
            marker=dict(
                size=8,
                color=magnitudes,
                colorscale='Viridis',
                colorbar=dict(title="Magnitude"),
                showscale=True,
                line=dict(width=0.5, color='white')
            ),
            line=dict(color='cyan', width=3),
            text=[
                f"Time: {t}<br>RA: {ra:.4f}°<br>Dec: {dec:.4f}°<br>"
                f"Distance: {d:.4f} AU<br>Mag: {m:.2f}"
                for t, ra, dec, d, m in zip(timestamps, ra_vals, dec_vals, distances, magnitudes)
            ],
            hovertemplate='%{text}<extra></extra>'
        ))

        # Add Earth at origin
        fig.add_trace(go.Scatter3d(
            x=[0],
            y=[0],
            z=[0],
            mode='markers',
            name='Earth',
            marker=dict(
                size=15,
                color='blue',
                symbol='circle'
            ),
            text=['Earth'],
            hovertemplate='Earth<extra></extra>'
        ))

        # Add Sun (approximate position)
        sun_distance = 1.0  # 1 AU
        fig.add_trace(go.Scatter3d(
            x=[sun_distance],
            y=[0],
            z=[0],
            mode='markers',
            name='Sun',
            marker=dict(
                size=20,
                color='yellow',
                symbol='circle'
            ),
            text=['Sun'],
            hovertemplate='Sun<extra></extra>'
        ))

        # Layout
        fig.update_layout(
            title={
                'text': '3I/ATLAS (C/2025 N1) - Interactive Sky Chart<br>'
                        '<sub>MotorHandPro Primal Logic Framework</sub>',
                'x': 0.5,
                'xanchor': 'center'
            },
            scene=dict(
                xaxis_title='X (AU)',
                yaxis_title='Y (AU)',
                zaxis_title='Z (AU)',
                bgcolor='black',
                xaxis=dict(backgroundcolor="black", gridcolor="gray"),
                yaxis=dict(backgroundcolor="black", gridcolor="gray"),
                zaxis=dict(backgroundcolor="black", gridcolor="gray"),
            ),
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='#0a0a0a',
            font=dict(color='white', size=12),
            showlegend=True,
            legend=dict(
                bgcolor='rgba(0,0,0,0.5)',
                bordercolor='cyan',
                borderwidth=1
            ),
            width=1200,
            height=800
        )

        # Save if requested
        if save_path:
            fig.write_html(save_path)
            print(f"✅ Saved interactive sky chart: {save_path}")

        return fig

    def create_primal_logic_dashboard(
        self,
        observations: List[CometObservation],
        states: List[RecursivePlanckState],
        save_path: Optional[str] = None
    ) -> Optional[go.Figure]:
        """
        Create comprehensive dashboard for Primal Logic analysis

        Args:
            observations: Comet observations
            states: Recursive Planck Operator states
            save_path: Path to save HTML file

        Returns:
            Plotly figure
        """
        if not HAVE_PLOTLY:
            warnings.warn("Plotly not available")
            return None

        if len(observations) == 0 or len(states) == 0:
            warnings.warn("No data to plot")
            return None

        # Extract data
        timestamps = [obs.timestamp for obs in observations]
        signals = [state.signal for state in states]
        n_values = [state.n for state in states]
        errors = [state.error for state in states]
        memory_integrals = [state.memory_integral for state in states]

        # Compute anomaly scores
        anomaly_scores = [min(1.0, err / 10.0) for err in errors]

        # Create subplots
        fig = make_subplots(
            rows=3,
            cols=2,
            subplot_titles=(
                'Signal S(t) - Ni Flux',
                'State n(t) - Anomaly Metric',
                'Error γ(t)',
                'Memory Integral ∫K(τ)n(τ)dτ',
                'Anomaly Score',
                'Phase Space (n vs S)'
            ),
            specs=[
                [{'type': 'scatter'}, {'type': 'scatter'}],
                [{'type': 'scatter'}, {'type': 'scatter'}],
                [{'type': 'scatter'}, {'type': 'scatter'}]
            ],
            vertical_spacing=0.12,
            horizontal_spacing=0.1
        )

        # Signal S(t)
        fig.add_trace(
            go.Scatter(
                x=timestamps,
                y=signals,
                mode='lines+markers',
                name='Signal S(t)',
                line=dict(color='cyan', width=2),
                marker=dict(size=5),
            ),
            row=1, col=1
        )

        # State n(t)
        fig.add_trace(
            go.Scatter(
                x=timestamps,
                y=n_values,
                mode='lines',
                name='State n(t)',
                line=dict(color='lime', width=3),
                fill='tozeroy',
                fillcolor='rgba(0, 255, 0, 0.1)'
            ),
            row=1, col=2
        )

        # Add bounds ±D
        D = states[0].D if states else 149.9992314
        fig.add_hline(y=D, line_dash="dash", line_color="red", row=1, col=2,
                     annotation_text="Upper Bound +D")
        fig.add_hline(y=-D, line_dash="dash", line_color="red", row=1, col=2,
                     annotation_text="Lower Bound -D")

        # Error γ(t)
        fig.add_trace(
            go.Scatter(
                x=timestamps,
                y=errors,
                mode='lines+markers',
                name='Error γ(t)',
                line=dict(color='orange', width=2),
                marker=dict(size=4)
            ),
            row=2, col=1
        )

        # Memory integral
        fig.add_trace(
            go.Scatter(
                x=timestamps,
                y=memory_integrals,
                mode='lines',
                name='Memory ∫K(τ)n(τ)dτ',
                line=dict(color='purple', width=2),
                fill='tozeroy',
                fillcolor='rgba(128, 0, 128, 0.1)'
            ),
            row=2, col=2
        )

        # Anomaly score
        fig.add_trace(
            go.Scatter(
                x=timestamps,
                y=anomaly_scores,
                mode='lines+markers',
                name='Anomaly Score',
                line=dict(color='red', width=3),
                marker=dict(
                    size=8,
                    color=anomaly_scores,
                    colorscale='RdYlGn_r',
                    showscale=True,
                    colorbar=dict(title="Anomaly", x=1.15)
                )
            ),
            row=3, col=1
        )

        # Phase space plot
        fig.add_trace(
            go.Scatter(
                x=n_values,
                y=signals,
                mode='markers+lines',
                name='Phase Space',
                marker=dict(
                    size=8,
                    color=list(range(len(n_values))),
                    colorscale='Plasma',
                    showscale=True,
                    colorbar=dict(title="Time Step", x=1.3)
                ),
                line=dict(color='white', width=1, dash='dot')
            ),
            row=3, col=2
        )

        # Update layout
        fig.update_layout(
            title={
                'text': '3I/ATLAS Primal Logic Analysis Dashboard<br>'
                        '<sub>Recursive Planck Operator - μ=0.16905</sub>',
                'x': 0.5,
                'xanchor': 'center'
            },
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='#1a1a1a',
            font=dict(color='white', size=11),
            showlegend=True,
            legend=dict(
                bgcolor='rgba(0,0,0,0.5)',
                bordercolor='cyan',
                borderwidth=1
            ),
            height=1200,
            width=1600
        )

        # Update axes
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="Time", row=1, col=1)
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="Time", row=1, col=2)
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="Time", row=2, col=1)
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="Time", row=2, col=2)
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="Time", row=3, col=1)
        fig.update_xaxes(showgrid=True, gridcolor='#333', title_text="State n(t)", row=3, col=2)

        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="S(t) [g/s]", row=1, col=1)
        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="n(t)", row=1, col=2)
        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="γ(t)", row=2, col=1)
        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="Memory", row=2, col=2)
        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="Anomaly", row=3, col=1)
        fig.update_yaxes(showgrid=True, gridcolor='#333', title_text="Signal S(t)", row=3, col=2)

        # Save if requested
        if save_path:
            fig.write_html(save_path)
            print(f"✅ Saved Primal Logic dashboard: {save_path}")

        return fig

    def create_realtime_animation(
        self,
        observations: List[CometObservation],
        save_path: Optional[str] = None,
        frame_duration_ms: int = 100
    ) -> Optional[go.Figure]:
        """
        Create animated visualization of comet motion

        Args:
            observations: Comet observations
            save_path: Path to save HTML
            frame_duration_ms: Animation frame duration

        Returns:
            Plotly figure with animation
        """
        if not HAVE_PLOTLY:
            return None

        if len(observations) < 2:
            warnings.warn("Need at least 2 observations for animation")
            return None

        # Extract data
        ra_vals = [obs.ra for obs in observations]
        dec_vals = [obs.dec for obs in observations]
        timestamps = [obs.timestamp.strftime("%Y-%m-%d %H:%M") for obs in observations]

        # Create frames
        frames = []

        for i in range(len(observations)):
            # Trajectory up to current point
            frame_data = go.Scatter(
                x=ra_vals[:i+1],
                y=dec_vals[:i+1],
                mode='lines+markers',
                line=dict(color='cyan', width=2),
                marker=dict(
                    size=[5] * i + [15],  # Larger marker for current position
                    color='cyan'
                ),
                name='3I/ATLAS'
            )

            frames.append(go.Frame(
                data=[frame_data],
                name=str(i),
                layout=go.Layout(
                    title_text=f"3I/ATLAS Motion - {timestamps[i]}"
                )
            ))

        # Initial figure
        fig = go.Figure(
            data=[go.Scatter(
                x=[ra_vals[0]],
                y=[dec_vals[0]],
                mode='markers',
                marker=dict(size=15, color='cyan')
            )],
            frames=frames
        )

        # Layout with animation controls
        fig.update_layout(
            title="3I/ATLAS Real-Time Animation",
            xaxis=dict(
                title="Right Ascension (degrees)",
                range=[min(ra_vals)-1, max(ra_vals)+1],
                showgrid=True,
                gridcolor='gray'
            ),
            yaxis=dict(
                title="Declination (degrees)",
                range=[min(dec_vals)-1, max(dec_vals)+1],
                showgrid=True,
                gridcolor='gray'
            ),
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='#1a1a1a',
            font=dict(color='white'),
            updatemenus=[dict(
                type='buttons',
                showactive=False,
                buttons=[
                    dict(label='Play',
                         method='animate',
                         args=[None, dict(
                             frame=dict(duration=frame_duration_ms, redraw=True),
                             fromcurrent=True
                         )]),
                    dict(label='Pause',
                         method='animate',
                         args=[[None], dict(
                             frame=dict(duration=0, redraw=False),
                             mode='immediate'
                         )])
                ]
            )],
            sliders=[dict(
                active=0,
                steps=[dict(
                    args=[[f.name], dict(
                        frame=dict(duration=0, redraw=True),
                        mode='immediate'
                    )],
                    label=timestamps[k],
                    method='animate'
                ) for k, f in enumerate(frames)],
                transition=dict(duration=0),
                x=0.1,
                y=0,
                currentvalue=dict(
                    font=dict(size=12),
                    prefix='Time: ',
                    visible=True,
                    xanchor='right'
                ),
                len=0.9
            )]
        )

        if save_path:
            fig.write_html(save_path)
            print(f"✅ Saved animation: {save_path}")

        return fig

    def create_comprehensive_report(
        self,
        observations: List[CometObservation],
        use_live_data: bool = True
    ) -> Dict[str, Any]:
        """
        Generate comprehensive visualization report

        Args:
            observations: Comet observations (if None, fetch live data)
            use_live_data: Whether to fetch live data from NASA

        Returns:
            Dictionary with paths to generated files
        """
        print("=" * 80)
        print("🌌 NASA DATA VISUALIZATION PIPELINE")
        print("=" * 80)
        print()

        report = {
            'timestamp': datetime.now().isoformat(),
            'num_observations': 0,
            'files': []
        }

        # Fetch data if needed
        if use_live_data or len(observations) == 0:
            print("📡 Fetching live data from NASA APIs...")
            observations = self.client.fetch_horizons_ephemeris()

            if not observations:
                print("⚠️  No live data available, using simulated feed")
                observations = self.client.simulate_live_feed(duration_hours=24.0)

        report['num_observations'] = len(observations)
        print(f"✅ Loaded {len(observations)} observations")
        print()

        # Process with Recursive Planck Operator
        print("🔬 Processing with Recursive Planck Operator...")
        states = []

        for obs in observations:
            state = self.operator.update(obs)
            states.append(RecursivePlanckState(
                n=state.n,
                signal=state.signal,
                memory_integral=state.memory_integral,
                error=state.error,
                mu=state.mu,
                D=state.D
            ))

        print(f"✅ Computed {len(states)} states")
        print()

        # Generate visualizations
        print("🎨 Generating visualizations...")

        # 1. Interactive sky chart
        if HAVE_PLOTLY:
            sky_chart_path = self.output_dir / "interactive_sky_chart.html"
            self.create_interactive_sky_chart(observations, str(sky_chart_path))
            report['files'].append(str(sky_chart_path))

        # 2. Primal Logic dashboard
        if HAVE_PLOTLY:
            dashboard_path = self.output_dir / "primal_logic_dashboard.html"
            self.create_primal_logic_dashboard(observations, states, str(dashboard_path))
            report['files'].append(str(dashboard_path))

        # 3. Animation
        if HAVE_PLOTLY and len(observations) >= 10:
            animation_path = self.output_dir / "comet_animation.html"
            # Use subset for animation (every 5th point for performance)
            anim_obs = observations[::5]
            self.create_realtime_animation(anim_obs, str(animation_path))
            report['files'].append(str(animation_path))

        print()
        print("=" * 80)
        print("✅ VISUALIZATION PIPELINE COMPLETE")
        print("=" * 80)
        print()
        print(f"📁 Output directory: {self.output_dir}")
        print(f"📊 Generated {len(report['files'])} visualizations")
        for filepath in report['files']:
            print(f"   - {filepath}")
        print()

        return report


# Main execution
if __name__ == "__main__":
    print("\n" + "=" * 80)
    print("🚀 MOTORHANDPRO - NASA 3I/ATLAS DATA VISUALIZATION")
    print("=" * 80)
    print()

    if not HAVE_PLOTLY:
        print("❌ ERROR: Plotly is required for visualization")
        print("   Install with: pip install plotly")
        exit(1)

    # Initialize visualizer
    visualizer = NASADataVisualizer()

    # Generate comprehensive report with live data
    report = visualizer.create_comprehensive_report(
        observations=[],
        use_live_data=False  # Set to True to fetch real NASA data
    )

    print("✅ NASA data visualization pipeline ready!")
    print()
