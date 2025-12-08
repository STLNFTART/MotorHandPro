#!/usr/bin/env python3
"""
Streamlit Rapid Prototyping App for NASA Data
Specializes in: Rapid prototyping, easy development, quick iterations

Features:
- Ultra-fast development
- Auto-reload on code changes
- Simple Python scripts
- Built-in widgets

Usage:
    streamlit run streamlit_app.py

Author: Donte Lightfoot
Date: December 4, 2025
"""

import warnings
from pathlib import Path

try:
    import streamlit as st
    import plotly.graph_objects as go
    import plotly.express as px
    HAVE_STREAMLIT = True
except ImportError:
    HAVE_STREAMLIT = False
    warnings.warn("Streamlit not available. Install with: pip install streamlit")

try:
    import pandas as pd
    import numpy as np
    HAVE_DEPS = True
except ImportError:
    HAVE_DEPS = False


def load_nasa_data():
    """Load NASA data from JSON files"""
    import json
    from datetime import datetime

    output_dir = Path('nasa_live_output')

    if not output_dir.exists():
        return None, None

    # Get latest observation file
    obs_files = list(output_dir.glob('observations_*.json'))
    state_files = list(output_dir.glob('states_*.json'))

    if not obs_files or not state_files:
        return None, None

    latest_obs_file = max(obs_files, key=lambda f: f.stat().st_mtime)
    latest_state_file = max(state_files, key=lambda f: f.stat().st_mtime)

    with open(latest_obs_file, 'r') as f:
        observations = json.load(f)

    with open(latest_state_file, 'r') as f:
        states = json.load(f)

    return observations, states


def main():
    """Main Streamlit app"""

    # Page config
    st.set_page_config(
        page_title="NASA 3I/ATLAS Monitor",
        page_icon="🌌",
        layout="wide",
        initial_sidebar_state="expanded"
    )

    # Title
    st.title("🌌 NASA 3I/ATLAS Live Data Monitor")
    st.markdown("**Recursive Planck Operator Analysis** | MotorHandPro")

    # Sidebar
    st.sidebar.header("⚙️ Controls")

    # Data selection
    data_source = st.sidebar.selectbox(
        "Data Source",
        ["Live Data", "Simulated", "Historical"]
    )

    # Time range
    time_range = st.sidebar.slider(
        "Time Range (hours)",
        min_value=1,
        max_value=24,
        value=12
    )

    # Anomaly threshold
    anomaly_threshold = st.sidebar.slider(
        "Anomaly Threshold",
        min_value=0.0,
        max_value=1.0,
        value=0.5,
        step=0.05
    )

    # Refresh button
    if st.sidebar.button("🔄 Refresh Data"):
        st.cache_data.clear()
        st.rerun()

    # Load data
    with st.spinner("Loading NASA data..."):
        observations, states = load_nasa_data()

    if observations is None or states is None:
        st.error("❌ No data available. Run the NASA pipeline first:")
        st.code("python3 live_nasa_pipeline.py --mode single")
        return

    # Convert to DataFrames
    obs_df = pd.DataFrame(observations)
    state_df = pd.DataFrame([s['primal_state'] for s in states])

    # Compute anomaly flags
    state_df['anomaly_flag'] = state_df['anomaly_score'] > anomaly_threshold

    # Status metrics
    st.header("📊 Live Status")

    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.metric(
            "Latest RA",
            f"{obs_df.iloc[-1]['ra']:.4f}°",
            delta=f"{obs_df.iloc[-1]['ra'] - obs_df.iloc[-2]['ra']:.6f}°"
        )

    with col2:
        st.metric(
            "Latest Dec",
            f"{obs_df.iloc[-1]['dec']:.4f}°",
            delta=f"{obs_df.iloc[-1]['dec'] - obs_df.iloc[-2]['dec']:.6f}°"
        )

    with col3:
        st.metric(
            "Distance",
            f"{obs_df.iloc[-1]['distance_au']:.4f} AU",
            delta=f"{obs_df.iloc[-1]['distance_au'] - obs_df.iloc[-2]['distance_au']:.4f} AU"
        )

    with col4:
        latest_anomaly = state_df.iloc[-1]['anomaly_score']
        anomaly_status = "🟢 NORMAL" if latest_anomaly < 0.3 else "🟡 WARNING" if latest_anomaly < 0.6 else "🔴 CRITICAL"
        st.metric(
            "Anomaly Status",
            anomaly_status,
            delta=f"{latest_anomaly:.4f}"
        )

    # Visualizations
    st.header("📈 Visualizations")

    tab1, tab2, tab3, tab4 = st.tabs(["3D Trajectory", "Time Series", "Anomaly Analysis", "Data Table"])

    with tab1:
        st.subheader("3D Comet Trajectory")

        # Convert RA/Dec to Cartesian
        ra_rad = np.deg2rad(obs_df['ra'])
        dec_rad = np.deg2rad(obs_df['dec'])
        distances = obs_df['distance_au']

        x = distances * np.cos(dec_rad) * np.cos(ra_rad)
        y = distances * np.cos(dec_rad) * np.sin(ra_rad)
        z = distances * np.sin(dec_rad)

        fig_3d = go.Figure(data=[
            go.Scatter3d(
                x=x, y=y, z=z,
                mode='lines+markers',
                marker=dict(
                    size=4,
                    color=state_df['anomaly_score'],
                    colorscale='Viridis',
                    colorbar=dict(title="Anomaly"),
                    showscale=True
                ),
                line=dict(color='cyan', width=2),
                name='Comet Path'
            ),
            go.Scatter3d(
                x=[0], y=[0], z=[0],
                mode='markers',
                marker=dict(size=10, color='blue'),
                name='Earth'
            )
        ])

        fig_3d.update_layout(
            scene=dict(
                xaxis_title='X (AU)',
                yaxis_title='Y (AU)',
                zaxis_title='Z (AU)',
                bgcolor='black'
            ),
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='black',
            font=dict(color='white'),
            height=600
        )

        st.plotly_chart(fig_3d, use_container_width=True)

    with tab2:
        st.subheader("Time Series Analysis")

        # Signal plot
        fig_signal = px.line(
            state_df,
            y='signal',
            title='Signal S(t) - Ni Gas Flux',
            labels={'signal': 'Signal (g/s)', 'index': 'Time Step'}
        )
        fig_signal.update_traces(line_color='lime')
        fig_signal.update_layout(
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='#1a1a1a',
            font=dict(color='white')
        )
        st.plotly_chart(fig_signal, use_container_width=True)

        # State plot
        fig_state = px.line(
            state_df,
            y='n',
            title='State n(t) with Bounds',
            labels={'n': 'State n(t)', 'index': 'Time Step'}
        )
        fig_state.update_traces(line_color='orange')

        # Add bounds
        D = 149.9992314
        fig_state.add_hline(y=D, line_dash="dash", line_color="red", annotation_text="+D")
        fig_state.add_hline(y=-D, line_dash="dash", line_color="red", annotation_text="-D")

        fig_state.update_layout(
            paper_bgcolor='#0a0a0a',
            plot_bgcolor='#1a1a1a',
            font=dict(color='white')
        )
        st.plotly_chart(fig_state, use_container_width=True)

    with tab3:
        st.subheader("Anomaly Detection Analysis")

        col1, col2 = st.columns(2)

        with col1:
            # Anomaly score over time
            fig_anomaly = px.line(
                state_df,
                y='anomaly_score',
                title='Anomaly Score Over Time',
                labels={'anomaly_score': 'Anomaly Score', 'index': 'Time Step'}
            )
            fig_anomaly.update_traces(line_color='red')
            fig_anomaly.add_hline(y=anomaly_threshold, line_dash="dot",
                                 line_color="yellow", annotation_text="Threshold")
            fig_anomaly.update_layout(
                paper_bgcolor='#0a0a0a',
                plot_bgcolor='#1a1a1a',
                font=dict(color='white')
            )
            st.plotly_chart(fig_anomaly, use_container_width=True)

        with col2:
            # Anomaly distribution
            fig_hist = px.histogram(
                state_df,
                x='anomaly_score',
                title='Anomaly Score Distribution',
                nbins=50,
                labels={'anomaly_score': 'Anomaly Score'}
            )
            fig_hist.update_traces(marker_color='red', opacity=0.7)
            fig_hist.update_layout(
                paper_bgcolor='#0a0a0a',
                plot_bgcolor='#1a1a1a',
                font=dict(color='white')
            )
            st.plotly_chart(fig_hist, use_container_width=True)

        # Anomaly events
        anomaly_events = state_df[state_df['anomaly_flag']]
        if len(anomaly_events) > 0:
            st.warning(f"⚠️ {len(anomaly_events)} anomaly events detected above threshold {anomaly_threshold}")
            st.dataframe(anomaly_events[['signal', 'n', 'error', 'anomaly_score']].head(10))
        else:
            st.success(f"✅ No anomalies detected above threshold {anomaly_threshold}")

    with tab4:
        st.subheader("Raw Data Table")

        # Combine data
        combined_df = pd.DataFrame({
            'RA': obs_df['ra'],
            'Dec': obs_df['dec'],
            'Distance (AU)': obs_df['distance_au'],
            'Signal (g/s)': state_df['signal'],
            'State n': state_df['n'],
            'Error': state_df['error'],
            'Anomaly': state_df['anomaly_score']
        })

        # Show last 100 rows
        st.dataframe(
            combined_df.tail(100).style.background_gradient(
                subset=['Anomaly'],
                cmap='RdYlGn_r'
            ),
            use_container_width=True,
            height=400
        )

        # Download button
        csv = combined_df.to_csv(index=False)
        st.download_button(
            label="📥 Download CSV",
            data=csv,
            file_name="nasa_data_export.csv",
            mime="text/csv"
        )

    # Footer
    st.markdown("---")
    st.markdown(
        """
        <div style='text-align: center; color: #666;'>
            MotorHandPro NASA Data Pipeline | Recursive Planck Operator (μ=0.16905)<br>
            Powered by Streamlit | Real-time Comet Monitoring
        </div>
        """,
        unsafe_allow_html=True
    )


if __name__ == "__main__":
    if not HAVE_STREAMLIT:
        print("❌ Streamlit not installed")
        print("   Install with: pip install streamlit")
        print()
        print("Run with: streamlit run streamlit_app.py")
        exit(1)

    main()
