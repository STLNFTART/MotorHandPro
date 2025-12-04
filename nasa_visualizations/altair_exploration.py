#!/usr/bin/env python3
"""
Altair Declarative Visualization for NASA Data
Specializes in: Declarative grammar, data exploration, publication quality

Features:
- Declarative grammar of graphics (Vega-Lite)
- Easy data exploration and transformation
- Publication-quality output
- Concise and readable code

Author: Donte Lightfoot
Date: December 4, 2025
"""

import pandas as pd
import warnings
from typing import List

try:
    import altair as alt
    HAVE_ALTAIR = True
except ImportError:
    HAVE_ALTAIR = False
    warnings.warn("Altair not available. Install with: pip install altair vega_datasets")


class AltairExplorationVisualizer:
    """Declarative data exploration using Altair"""

    def __init__(self):
        """Initialize Altair visualizer"""
        if not HAVE_ALTAIR:
            raise ImportError("Altair is required")

        # Set theme
        alt.themes.enable('dark')

    def create_exploration_dashboard(
        self,
        observations: List,
        states: List,
        output_path: str = "altair_exploration.html"
    ):
        """
        Create declarative exploration dashboard

        Args:
            observations: Comet observations
            states: Recursive Planck states
            output_path: Output HTML file
        """
        if not observations or not states:
            warnings.warn("No data provided")
            return

        # Convert to DataFrame for Altair
        obs_data = pd.DataFrame([
            {
                'timestamp': obs.timestamp,
                'ra': obs.ra,
                'dec': obs.dec,
                'distance_au': obs.distance_au,
                'magnitude': obs.magnitude,
                'gas_flux': obs.gas_production_rate if hasattr(obs, 'gas_production_rate') and obs.gas_production_rate else 0
            }
            for obs in observations
        ])

        state_data = pd.DataFrame([
            {
                'timestamp': obs.timestamp,
                'signal': s.signal,
                'n': s.n,
                'error': s.error,
                'memory': s.memory_integral,
                'anomaly': min(1.0, s.error / 10.0)
            }
            for obs, s in zip(observations, states)
        ])

        # Merge datasets
        combined_data = pd.merge(obs_data, state_data, on='timestamp')

        # Chart 1: Interactive scatter - RA vs Dec with size by magnitude
        chart1 = alt.Chart(combined_data).mark_circle(opacity=0.6).encode(
            x=alt.X('ra:Q', title='Right Ascension (deg)', scale=alt.Scale(zero=False)),
            y=alt.Y('dec:Q', title='Declination (deg)', scale=alt.Scale(zero=False)),
            size=alt.Size('magnitude:Q', title='Magnitude', scale=alt.Scale(range=[50, 500])),
            color=alt.Color('distance_au:Q', title='Distance (AU)', scale=alt.Scale(scheme='viridis')),
            tooltip=[
                alt.Tooltip('timestamp:T', format='%Y-%m-%d %H:%M'),
                alt.Tooltip('ra:Q', format='.4f', title='RA'),
                alt.Tooltip('dec:Q', format='.4f', title='Dec'),
                alt.Tooltip('distance_au:Q', format='.4f', title='Distance'),
                alt.Tooltip('magnitude:Q', format='.2f')
            ]
        ).properties(
            width=600,
            height=400,
            title='Comet Position: RA vs Dec (Interactive)'
        ).interactive()

        # Chart 2: Time series - Signal and State
        base_time = alt.Chart(combined_data).encode(
            x=alt.X('timestamp:T', title='Time')
        )

        signal_line = base_time.mark_line(color='cyan', strokeWidth=2).encode(
            y=alt.Y('signal:Q', title='Signal S(t) [g/s]'),
            tooltip=[
                alt.Tooltip('timestamp:T', format='%Y-%m-%d %H:%M'),
                alt.Tooltip('signal:Q', format='.2f', title='Signal')
            ]
        )

        signal_points = base_time.mark_circle(size=30, color='cyan', opacity=0.5).encode(
            y='signal:Q'
        )

        chart2 = (signal_line + signal_points).properties(
            width=600,
            height=250,
            title='Signal S(t) - Ni Gas Flux'
        ).interactive()

        # Chart 3: State n(t) with bounds
        state_line = base_time.mark_line(color='orange', strokeWidth=2).encode(
            y=alt.Y('n:Q', title='State n(t)', scale=alt.Scale(domain=[-150, 150])),
            tooltip=[
                alt.Tooltip('timestamp:T', format='%Y-%m-%d %H:%M'),
                alt.Tooltip('n:Q', format='.4f', title='State n')
            ]
        )

        # Add bound lines
        bounds_data = pd.DataFrame({
            'timestamp': [combined_data['timestamp'].min(), combined_data['timestamp'].max()],
            'upper': [149.9992314, 149.9992314],
            'lower': [-149.9992314, -149.9992314]
        })

        upper_bound = alt.Chart(bounds_data).mark_line(
            color='red', strokeDash=[5, 5], strokeWidth=1
        ).encode(
            x='timestamp:T',
            y='upper:Q'
        )

        lower_bound = alt.Chart(bounds_data).mark_line(
            color='red', strokeDash=[5, 5], strokeWidth=1
        ).encode(
            x='timestamp:T',
            y='lower:Q'
        )

        chart3 = (state_line + upper_bound + lower_bound).properties(
            width=600,
            height=250,
            title='State n(t) with ±D Bounds'
        ).interactive()

        # Chart 4: Anomaly heatmap
        chart4 = alt.Chart(combined_data).mark_rect().encode(
            x=alt.X('hours(timestamp):O', title='Hour'),
            y=alt.Y('date(timestamp):O', title='Date'),
            color=alt.Color('anomaly:Q', title='Anomaly Score',
                          scale=alt.Scale(scheme='redyellowgreen', reverse=True, domain=[0, 1])),
            tooltip=[
                alt.Tooltip('timestamp:T', format='%Y-%m-%d %H:%M'),
                alt.Tooltip('anomaly:Q', format='.4f', title='Anomaly Score'),
                alt.Tooltip('error:Q', format='.4f', title='Error')
            ]
        ).properties(
            width=600,
            height=200,
            title='Anomaly Heatmap (Time Grid)'
        )

        # Chart 5: Correlation matrix as scatter plot matrix
        scatter_matrix = alt.Chart(combined_data.sample(min(500, len(combined_data)))).mark_circle(
            size=20, opacity=0.5
        ).encode(
            alt.X(alt.repeat("column"), type='quantitative'),
            alt.Y(alt.repeat("row"), type='quantitative'),
            color=alt.Color('anomaly:Q', scale=alt.Scale(scheme='redyellowgreen', reverse=True))
        ).properties(
            width=180,
            height=180
        ).repeat(
            row=['signal', 'n', 'error'],
            column=['distance_au', 'gas_flux', 'anomaly']
        ).properties(
            title='Correlation Matrix (Sample)'
        )

        # Chart 6: Distribution histograms
        hist_signal = alt.Chart(combined_data).mark_bar(color='cyan', opacity=0.7).encode(
            alt.X('signal:Q', bin=alt.Bin(maxbins=30), title='Signal S(t)'),
            alt.Y('count()', title='Frequency')
        ).properties(
            width=280,
            height=200,
            title='Signal Distribution'
        )

        hist_anomaly = alt.Chart(combined_data).mark_bar(color='red', opacity=0.7).encode(
            alt.X('anomaly:Q', bin=alt.Bin(maxbins=30), title='Anomaly Score'),
            alt.Y('count()', title='Frequency')
        ).properties(
            width=280,
            height=200,
            title='Anomaly Distribution'
        )

        # Compose dashboard
        dashboard = alt.vconcat(
            chart1,
            alt.hconcat(chart2, chart3),
            chart4,
            alt.hconcat(hist_signal, hist_anomaly),
            scatter_matrix
        ).properties(
            title={
                "text": "3I/ATLAS Data Exploration Dashboard (Altair)",
                "subtitle": "Declarative Grammar of Graphics"
            }
        ).configure_view(
            strokeWidth=0
        ).configure_axis(
            gridColor='#333'
        )

        # Save
        dashboard.save(output_path)
        print(f"✅ Altair exploration dashboard saved: {output_path}")

        return dashboard

    def create_interactive_selection(
        self,
        observations: List,
        states: List,
        output_path: str = "altair_interactive_selection.html"
    ):
        """
        Create dashboard with linked brushing and selection

        Args:
            observations: Comet observations
            states: Recursive Planck states
            output_path: Output HTML file
        """
        # Convert to DataFrame
        data = pd.DataFrame([
            {
                'timestamp': obs.timestamp,
                'ra': obs.ra,
                'dec': obs.dec,
                'signal': s.signal,
                'n': s.n,
                'anomaly': min(1.0, s.error / 10.0)
            }
            for obs, s in zip(observations, states)
        ])

        # Create selection
        brush = alt.selection_interval()

        # Base chart with selection
        base = alt.Chart(data).properties(
            width=400,
            height=300
        ).add_params(brush)

        # Scatter plot with brushing
        scatter = base.mark_circle(size=60).encode(
            x='ra:Q',
            y='dec:Q',
            color=alt.condition(brush, 'anomaly:Q', alt.value('lightgray'),
                              scale=alt.Scale(scheme='redyellowgreen', reverse=True)),
            tooltip=['timestamp:T', 'ra:Q', 'dec:Q', 'anomaly:Q']
        ).properties(title='Select Region (Brush)')

        # Time series filtered by selection
        filtered_time = base.mark_line(strokeWidth=3).encode(
            x='timestamp:T',
            y='signal:Q',
            color=alt.condition(brush, alt.value('cyan'), alt.value('gray')),
            tooltip=['timestamp:T', 'signal:Q']
        ).properties(title='Signal (Filtered by Selection)')

        # State filtered by selection
        filtered_state = base.mark_area(opacity=0.7).encode(
            x='timestamp:T',
            y='n:Q',
            color=alt.condition(brush, alt.value('orange'), alt.value('lightgray')),
            tooltip=['timestamp:T', 'n:Q']
        ).properties(title='State n(t) (Filtered)')

        # Compose
        dashboard = alt.vconcat(
            scatter,
            alt.hconcat(filtered_time, filtered_state)
        ).properties(
            title="Interactive Selection Dashboard (Linked Brushing)"
        )

        dashboard.save(output_path)
        print(f"✅ Altair interactive selection saved: {output_path}")

        return dashboard


# Example usage
if __name__ == "__main__":
    print("=" * 80)
    print("📊 ALTAIR DECLARATIVE VISUALIZATION")
    print("=" * 80)
    print()

    if not HAVE_ALTAIR:
        print("❌ Altair not installed")
        print("   Install with: pip install altair vega_datasets")
        exit(1)

    print("✅ Altair available")
    print()
    print("Features:")
    print("  - Declarative grammar of graphics (Vega-Lite)")
    print("  - Concise and readable code")
    print("  - Publication-quality output")
    print("  - Easy data exploration")
    print("  - Linked brushing and selection")
    print()
    print("Usage:")
    print("  visualizer = AltairExplorationVisualizer()")
    print("  visualizer.create_exploration_dashboard(observations, states)")
    print("  visualizer.create_interactive_selection(observations, states)")
    print()
    print("=" * 80)
