#!/usr/bin/env python3
"""
Dash Production Web Application for NASA Data
Specializes in: Production web dashboards, multi-user deployment

Features:
- Production-ready web dashboards
- Real-time updates with callbacks
- Multi-user support
- Scalable deployment

Author: Donte Lightfoot
Date: December 4, 2025
"""

import warnings
from typing import List
from datetime import datetime

try:
    import dash
    from dash import dcc, html, Input, Output, callback
    import plotly.graph_objects as go
    import plotly.express as px
    from dash.dash_table import DataTable
    HAVE_DASH = True
except ImportError:
    HAVE_DASH = False
    warnings.warn("Dash not available. Install with: pip install dash")

try:
    import pandas as pd
    HAVE_PANDAS = True
except ImportError:
    HAVE_PANDAS = False


class DashNASAWebApp:
    """Production web dashboard using Dash"""

    def __init__(self, title: str = "NASA 3I/ATLAS Live Monitor"):
        """Initialize Dash app"""
        if not HAVE_DASH:
            raise ImportError("Dash is required for web dashboard")

        self.app = dash.Dash(__name__, title=title)
        self.observations = []
        self.states = []

    def load_data(self, observations: List, states: List):
        """Load data into app"""
        self.observations = observations
        self.states = states

    def build_layout(self):
        """Build dashboard layout"""

        self.app.layout = html.Div([
            # Header
            html.Div([
                html.H1("🌌 NASA 3I/ATLAS Live Data Monitor",
                       style={'textAlign': 'center', 'color': '#00FFFF'}),
                html.H3("Recursive Planck Operator Analysis",
                       style={'textAlign': 'center', 'color': '#888'})
            ], style={'backgroundColor': '#0a0a0a', 'padding': '20px'}),

            # Auto-refresh interval
            dcc.Interval(
                id='interval-component',
                interval=60*1000,  # Update every 60 seconds
                n_intervals=0
            ),

            # Status indicators
            html.Div([
                html.Div([
                    html.H4("Latest Data", style={'color': '#00FFFF'}),
                    html.Div(id='latest-data-display', style={'fontSize': '14px'})
                ], className='status-card', style={
                    'backgroundColor': '#1a1a1a',
                    'padding': '20px',
                    'margin': '10px',
                    'borderRadius': '10px',
                    'border': '1px solid #333'
                })
            ]),

            # Main plots
            html.Div([
                # 3D Trajectory
                html.Div([
                    dcc.Graph(id='trajectory-3d')
                ], style={'width': '50%', 'display': 'inline-block'}),

                # Signal time series
                html.Div([
                    dcc.Graph(id='signal-timeseries')
                ], style={'width': '50%', 'display': 'inline-block'})
            ]),

            html.Div([
                # State n(t)
                html.Div([
                    dcc.Graph(id='state-plot')
                ], style={'width': '50%', 'display': 'inline-block'}),

                # Anomaly gauge
                html.Div([
                    dcc.Graph(id='anomaly-gauge')
                ], style={'width': '50%', 'display': 'inline-block'})
            ]),

            # Data table
            html.Div([
                html.H3("Latest Observations", style={'color': '#00FFFF'}),
                html.Div(id='data-table')
            ], style={'margin': '20px', 'backgroundColor': '#1a1a1a',
                     'padding': '20px', 'borderRadius': '10px'}),

            # Footer
            html.Div([
                html.P(f"MotorHandPro NASA Data Pipeline | "
                      f"Updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}",
                      style={'textAlign': 'center', 'color': '#666'})
            ], style={'padding': '20px'})

        ], style={'backgroundColor': '#0a0a0a', 'color': 'white',
                 'fontFamily': 'Arial, sans-serif'})

    def setup_callbacks(self):
        """Setup dashboard callbacks"""

        @self.app.callback(
            Output('latest-data-display', 'children'),
            Input('interval-component', 'n_intervals')
        )
        def update_latest_data(n):
            if not self.observations:
                return "No data available"

            latest_obs = self.observations[-1]
            latest_state = self.states[-1] if self.states else None

            data_display = [
                html.P(f"⏱️  Time: {latest_obs.timestamp.strftime('%Y-%m-%d %H:%M:%S')}", style={'margin': '5px'}),
                html.P(f"📍 Position: RA {latest_obs.ra:.4f}°, Dec {latest_obs.dec:.4f}°", style={'margin': '5px'}),
                html.P(f"📏 Distance: {latest_obs.distance_au:.4f} AU", style={'margin': '5px'}),
            ]

            if hasattr(latest_obs, 'gas_production_rate') and latest_obs.gas_production_rate:
                data_display.append(
                    html.P(f"💨 Ni Flux: {latest_obs.gas_production_rate:.2f} g/s",
                          style={'margin': '5px', 'color': '#00FF00'})
                )

            if latest_state:
                data_display.append(
                    html.P(f"🎯 State n(t): {latest_state.n:.4f}",
                          style={'margin': '5px', 'color': '#FFA500'})
                )
                anomaly = min(1.0, latest_state.error / 10.0)
                color = '#00FF00' if anomaly < 0.3 else '#FFFF00' if anomaly < 0.6 else '#FF0000'
                data_display.append(
                    html.P(f"⚠️  Anomaly: {anomaly:.4f}",
                          style={'margin': '5px', 'color': color})
                )

            return data_display

        @self.app.callback(
            Output('trajectory-3d', 'figure'),
            Input('interval-component', 'n_intervals')
        )
        def update_trajectory_3d(n):
            if not self.observations:
                return go.Figure()

            # Convert to Cartesian
            ra_rad = [np.deg2rad(obs.ra) for obs in self.observations]
            dec_rad = [np.deg2rad(obs.dec) for obs in self.observations]
            distances = [obs.distance_au for obs in self.observations]

            x = [d * np.cos(dec) * np.cos(ra) for d, dec, ra in zip(distances, dec_rad, ra_rad)]
            y = [d * np.cos(dec) * np.sin(ra) for d, dec, ra in zip(distances, dec_rad, ra_rad)]
            z = [d * np.sin(dec) for d, dec in zip(distances, dec_rad)]

            fig = go.Figure(data=[
                go.Scatter3d(
                    x=x, y=y, z=z,
                    mode='lines+markers',
                    marker=dict(size=4, color='cyan'),
                    line=dict(color='cyan', width=3),
                    name='Comet Path'
                ),
                go.Scatter3d(
                    x=[0], y=[0], z=[0],
                    mode='markers',
                    marker=dict(size=10, color='blue'),
                    name='Earth'
                )
            ])

            fig.update_layout(
                title="3D Trajectory",
                scene=dict(
                    xaxis_title='X (AU)', yaxis_title='Y (AU)', zaxis_title='Z (AU)',
                    bgcolor='black'
                ),
                paper_bgcolor='#1a1a1a',
                plot_bgcolor='black',
                font=dict(color='white'),
                showlegend=True
            )

            return fig

        @self.app.callback(
            Output('signal-timeseries', 'figure'),
            Input('interval-component', 'n_intervals')
        )
        def update_signal_timeseries(n):
            if not self.observations or not self.states:
                return go.Figure()

            timestamps = [obs.timestamp for obs in self.observations]
            signals = [s.signal for s in self.states]

            fig = go.Figure(data=[
                go.Scatter(
                    x=timestamps, y=signals,
                    mode='lines+markers',
                    line=dict(color='lime', width=2),
                    marker=dict(size=3),
                    name='Signal S(t)'
                )
            ])

            fig.update_layout(
                title="Signal S(t) - Ni Gas Flux",
                xaxis_title="Time",
                yaxis_title="Signal (g/s)",
                paper_bgcolor='#1a1a1a',
                plot_bgcolor='#0a0a0a',
                font=dict(color='white'),
                xaxis=dict(gridcolor='#333'),
                yaxis=dict(gridcolor='#333')
            )

            return fig

        @self.app.callback(
            Output('state-plot', 'figure'),
            Input('interval-component', 'n_intervals')
        )
        def update_state_plot(n):
            if not self.observations or not self.states:
                return go.Figure()

            timestamps = [obs.timestamp for obs in self.observations]
            n_values = [s.n for s in self.states]

            D = 149.9992314

            fig = go.Figure(data=[
                go.Scatter(
                    x=timestamps, y=n_values,
                    mode='lines',
                    line=dict(color='orange', width=3),
                    fill='tozeroy',
                    name='State n(t)'
                ),
                go.Scatter(
                    x=[timestamps[0], timestamps[-1]], y=[D, D],
                    mode='lines',
                    line=dict(color='red', width=1, dash='dash'),
                    name='+D Bound'
                ),
                go.Scatter(
                    x=[timestamps[0], timestamps[-1]], y=[-D, -D],
                    mode='lines',
                    line=dict(color='red', width=1, dash='dash'),
                    name='-D Bound'
                )
            ])

            fig.update_layout(
                title="State n(t) with Bounds",
                xaxis_title="Time",
                yaxis_title="State n(t)",
                paper_bgcolor='#1a1a1a',
                plot_bgcolor='#0a0a0a',
                font=dict(color='white'),
                xaxis=dict(gridcolor='#333'),
                yaxis=dict(gridcolor='#333')
            )

            return fig

        @self.app.callback(
            Output('anomaly-gauge', 'figure'),
            Input('interval-component', 'n_intervals')
        )
        def update_anomaly_gauge(n):
            if not self.states:
                return go.Figure()

            latest_state = self.states[-1]
            anomaly = min(1.0, latest_state.error / 10.0)

            fig = go.Figure(go.Indicator(
                mode="gauge+number+delta",
                value=anomaly,
                domain={'x': [0, 1], 'y': [0, 1]},
                title={'text': "Anomaly Score", 'font': {'color': 'white'}},
                delta={'reference': 0.5},
                gauge={
                    'axis': {'range': [None, 1], 'tickcolor': 'white'},
                    'bar': {'color': "darkblue"},
                    'steps': [
                        {'range': [0, 0.3], 'color': "green"},
                        {'range': [0.3, 0.6], 'color': "yellow"},
                        {'range': [0.6, 1], 'color': "red"}
                    ],
                    'threshold': {
                        'line': {'color': "red", 'width': 4},
                        'thickness': 0.75,
                        'value': 0.6
                    }
                }
            ))

            fig.update_layout(
                paper_bgcolor='#1a1a1a',
                font={'color': 'white'}
            )

            return fig

        @self.app.callback(
            Output('data-table', 'children'),
            Input('interval-component', 'n_intervals')
        )
        def update_data_table(n):
            if not self.observations or not HAVE_PANDAS:
                return html.P("No data available")

            # Get last 10 observations
            recent_obs = self.observations[-10:]
            recent_states = self.states[-10:] if len(self.states) >= 10 else self.states

            data = []
            for obs, state in zip(recent_obs, recent_states):
                data.append({
                    'Time': obs.timestamp.strftime('%H:%M:%S'),
                    'RA': f"{obs.ra:.4f}°",
                    'Dec': f"{obs.dec:.4f}°",
                    'Distance': f"{obs.distance_au:.4f} AU",
                    'Signal': f"{state.signal:.2f} g/s",
                    'State n': f"{state.n:.4f}",
                    'Anomaly': f"{min(1.0, state.error/10.0):.4f}"
                })

            df = pd.DataFrame(data)

            return DataTable(
                data=df.to_dict('records'),
                columns=[{'name': i, 'id': i} for i in df.columns],
                style_cell={
                    'backgroundColor': '#0a0a0a',
                    'color': 'white',
                    'border': '1px solid #333'
                },
                style_header={
                    'backgroundColor': '#1a1a1a',
                    'fontWeight': 'bold',
                    'color': '#00FFFF'
                },
                style_data_conditional=[
                    {
                        'if': {'row_index': 'odd'},
                        'backgroundColor': '#1a1a1a'
                    }
                ]
            )

    def run(self, host: str = '0.0.0.0', port: int = 8050, debug: bool = False):
        """
        Run Dash web application

        Args:
            host: Host address
            port: Port number
            debug: Debug mode
        """
        self.build_layout()
        self.setup_callbacks()

        print("=" * 80)
        print("🚀 DASH WEB APPLICATION STARTING")
        print("=" * 80)
        print(f"   Host: {host}")
        print(f"   Port: {port}")
        print(f"   URL: http://localhost:{port}")
        print("=" * 80)
        print()

        self.app.run(host=host, port=port, debug=debug)


# Example usage
if __name__ == "__main__":
    import numpy as np

    print("=" * 80)
    print("📊 DASH PRODUCTION WEB DASHBOARD")
    print("=" * 80)
    print()

    if not HAVE_DASH:
        print("❌ Dash not installed")
        print("   Install with: pip install dash")
        exit(1)

    print("✅ Dash available")
    print()
    print("Features:")
    print("  - Production-ready web dashboards")
    print("  - Real-time updates with callbacks")
    print("  - Multi-user support")
    print("  - Scalable deployment")
    print("  - Auto-refresh every 60 seconds")
    print()
    print("Usage:")
    print("  app = DashNASAWebApp()")
    print("  app.load_data(observations, states)")
    print("  app.run(host='0.0.0.0', port=8050)")
    print()
    print("=" * 80)
