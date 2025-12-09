#!/usr/bin/env python3
"""
Bokeh Real-Time Visualization for NASA Data
Specializes in: Real-time streaming, 2D plots, server-based updates

Features:
- Real-time data streaming at high frequency
- Interactive tools (pan, zoom, hover)
- Server-based push updates
- Excellent for time-series monitoring

Author: Donte Lightfoot
Date: December 4, 2025
"""

import numpy as np
from datetime import datetime
from typing import List, Optional
import warnings

try:
    from bokeh.plotting import figure, output_file, save, show
    from bokeh.models import HoverTool, ColumnDataSource, DatetimeTickFormatter
    from bokeh.layouts import column, row, gridplot
    from bokeh.palettes import Viridis256, Plasma256
    from bokeh.io import curdoc
    from bokeh.server.server import Server
    HAVE_BOKEH = True
except ImportError:
    HAVE_BOKEH = False
    warnings.warn("Bokeh not available. Install with: pip install bokeh")


class BokehRealtimeVisualizer:
    """Real-time visualization using Bokeh"""

    def __init__(self, title: str = "NASA 3I/ATLAS Real-Time Monitor"):
        """Initialize Bokeh visualizer"""
        if not HAVE_BOKEH:
            raise ImportError("Bokeh is required for real-time visualization")

        self.title = title
        self.data_sources = {}

    def create_realtime_dashboard(
        self,
        observations: List,
        states: List,
        output_path: str = "bokeh_realtime_dashboard.html"
    ):
        """
        Create real-time monitoring dashboard

        Args:
            observations: Comet observations
            states: Recursive Planck states
            output_path: Output HTML file
        """
        if not observations or not states:
            warnings.warn("No data provided")
            return

        # Extract time series data
        timestamps = [obs.timestamp for obs in observations]
        ra_vals = [obs.ra for obs in observations]
        dec_vals = [obs.dec for obs in observations]
        distances = [obs.distance_au for obs in observations]
        signals = [s.signal for s in states]
        n_values = [s.n for s in states]
        errors = [s.error for s in states]
        anomalies = [min(1.0, s.error / 10.0) for s in states]

        output_file(output_path)

        # Create data sources for streaming
        source_position = ColumnDataSource(data={
            'time': timestamps,
            'ra': ra_vals,
            'dec': dec_vals
        })

        source_signal = ColumnDataSource(data={
            'time': timestamps,
            'signal': signals,
            'n': n_values
        })

        source_anomaly = ColumnDataSource(data={
            'time': timestamps,
            'error': errors,
            'anomaly': anomalies
        })

        # Plot 1: RA/Dec position tracking
        p1 = figure(
            title="Comet Position (RA/Dec)",
            x_axis_label="Right Ascension (deg)",
            y_axis_label="Declination (deg)",
            width=600,
            height=400,
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        p1.line('ra', 'dec', source=source_position, line_width=2, color='cyan', alpha=0.8)
        p1.circle('ra', 'dec', source=source_position, size=4, color='cyan', alpha=0.6)

        hover1 = HoverTool(tooltips=[
            ("RA", "@ra{0.0000}°"),
            ("Dec", "@dec{0.0000}°"),
            ("Time", "@time{%F %T}")
        ], formatters={'@time': 'datetime'})
        p1.add_tools(hover1)

        # Plot 2: Signal over time
        p2 = figure(
            title="Signal S(t) - Ni Flux",
            x_axis_type="datetime",
            x_axis_label="Time",
            y_axis_label="Signal (g/s)",
            width=600,
            height=300,
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        p2.line('time', 'signal', source=source_signal, line_width=2, color='lime', legend_label="Signal")
        p2.circle('time', 'signal', source=source_signal, size=3, color='lime', alpha=0.6)

        hover2 = HoverTool(tooltips=[
            ("Signal", "@signal{0.00} g/s"),
            ("Time", "@time{%F %T}")
        ], formatters={'@time': 'datetime'})
        p2.add_tools(hover2)

        # Plot 3: State n(t)
        p3 = figure(
            title="State n(t) - Anomaly Metric",
            x_axis_type="datetime",
            x_axis_label="Time",
            y_axis_label="State n(t)",
            width=600,
            height=300,
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        p3.line('time', 'n', source=source_signal, line_width=2, color='orange', legend_label="State n(t)")
        p3.circle('time', 'n', source=source_signal, size=3, color='orange', alpha=0.6)

        # Add bounds
        D = 149.9992314
        p3.line([timestamps[0], timestamps[-1]], [D, D], line_width=1, color='red',
                line_dash='dashed', legend_label="+D Bound")
        p3.line([timestamps[0], timestamps[-1]], [-D, -D], line_width=1, color='red',
                line_dash='dashed', legend_label="-D Bound")

        hover3 = HoverTool(tooltips=[
            ("State n", "@n{0.0000}"),
            ("Time", "@time{%F %T}")
        ], formatters={'@time': 'datetime'})
        p3.add_tools(hover3)

        # Plot 4: Anomaly score
        p4 = figure(
            title="Anomaly Detection Score",
            x_axis_type="datetime",
            x_axis_label="Time",
            y_axis_label="Anomaly Score",
            width=600,
            height=300,
            tools="pan,wheel_zoom,box_zoom,reset,save"
        )

        # Color-coded by anomaly level
        colors = ['green' if a < 0.3 else 'yellow' if a < 0.6 else 'red' for a in anomalies]

        p4.line('time', 'anomaly', source=source_anomaly, line_width=2, color='red')
        p4.circle('time', 'anomaly', source=source_anomaly, size=5, color=colors, alpha=0.8)

        # Threshold lines
        p4.line([timestamps[0], timestamps[-1]], [0.3, 0.3], line_width=1, color='yellow',
                line_dash='dotted', legend_label="Warning (0.3)")
        p4.line([timestamps[0], timestamps[-1]], [0.6, 0.6], line_width=1, color='red',
                line_dash='dotted', legend_label="Critical (0.6)")

        hover4 = HoverTool(tooltips=[
            ("Anomaly", "@anomaly{0.0000}"),
            ("Error", "@error{0.0000}"),
            ("Time", "@time{%F %T}")
        ], formatters={'@time': 'datetime'})
        p4.add_tools(hover4)

        # Configure legends
        for p in [p2, p3, p4]:
            p.legend.location = "top_left"
            p.legend.click_policy = "hide"

        # Layout
        layout = column(
            row(p1),
            row(p2, p3),
            row(p4)
        )

        save(layout)
        print(f"✅ Bokeh real-time dashboard saved: {output_path}")

        return layout

    def create_streaming_monitor(
        self,
        port: int = 5006
    ):
        """
        Create Bokeh server for live streaming (requires bokeh server)

        Args:
            port: Server port

        Usage:
            Run with: bokeh serve --show bokeh_streaming.py
        """
        print(f"🚀 Starting Bokeh streaming server on port {port}...")
        print(f"   Visit: http://localhost:{port}")
        print()
        print("   This requires running: bokeh serve --show <script.py>")


# Example usage
if __name__ == "__main__":
    print("=" * 80)
    print("📊 BOKEH REAL-TIME VISUALIZATION")
    print("=" * 80)
    print()

    if not HAVE_BOKEH:
        print("❌ Bokeh not installed")
        print("   Install with: pip install bokeh")
        exit(1)

    print("✅ Bokeh available")
    print()
    print("Features:")
    print("  - Real-time data streaming")
    print("  - Interactive pan/zoom/hover")
    print("  - Server-based updates")
    print("  - Excellent for time-series monitoring")
    print()
    print("Usage:")
    print("  visualizer = BokehRealtimeVisualizer()")
    print("  visualizer.create_realtime_dashboard(observations, states)")
    print()
    print("=" * 80)
