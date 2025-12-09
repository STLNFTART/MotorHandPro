"""
NASA Data Visualization Suite - Multi-Library Implementation

Implements 8 visualization libraries for comprehensive data exploration:
- Plotly: Interactive 3D dashboards
- Bokeh: Real-time 2D streaming
- Altair: Declarative exploration
- Vispy: GPU-accelerated big data
- PyVista: 3D mesh celestial bodies
- HoloViews: Multi-plot coordination
- Dash: Production web deployment
- Streamlit: Rapid prototyping

Author: Donte Lightfoot
Date: December 4, 2025
"""

__version__ = "1.0.0"
__author__ = "Donte Lightfoot"

# Import availability checks
AVAILABLE_LIBRARIES = {}

try:
    import plotly
    AVAILABLE_LIBRARIES['plotly'] = True
except ImportError:
    AVAILABLE_LIBRARIES['plotly'] = False

try:
    import bokeh
    AVAILABLE_LIBRARIES['bokeh'] = True
except ImportError:
    AVAILABLE_LIBRARIES['bokeh'] = False

try:
    import altair
    AVAILABLE_LIBRARIES['altair'] = True
except ImportError:
    AVAILABLE_LIBRARIES['altair'] = False

try:
    import vispy
    AVAILABLE_LIBRARIES['vispy'] = True
except ImportError:
    AVAILABLE_LIBRARIES['vispy'] = False

try:
    import pyvista
    AVAILABLE_LIBRARIES['pyvista'] = True
except ImportError:
    AVAILABLE_LIBRARIES['pyvista'] = False

try:
    import holoviews
    AVAILABLE_LIBRARIES['holoviews'] = True
except ImportError:
    AVAILABLE_LIBRARIES['holoviews'] = False

try:
    import dash
    AVAILABLE_LIBRARIES['dash'] = True
except ImportError:
    AVAILABLE_LIBRARIES['dash'] = False

try:
    import streamlit
    AVAILABLE_LIBRARIES['streamlit'] = True
except ImportError:
    AVAILABLE_LIBRARIES['streamlit'] = False


def print_library_status():
    """Print status of all visualization libraries"""
    print("=" * 80)
    print("📊 VISUALIZATION LIBRARY STATUS")
    print("=" * 80)
    print()

    for lib, available in AVAILABLE_LIBRARIES.items():
        status = "✅ Available" if available else "❌ Not Installed"
        print(f"  {lib:12s}: {status}")

    print()
    print(f"Total Available: {sum(AVAILABLE_LIBRARIES.values())}/8")
    print("=" * 80)


def get_recommended_library(use_case: str) -> str:
    """
    Get recommended library for specific use case

    Args:
        use_case: One of 'interactive_3d', 'realtime', 'exploration',
                  'big_data', 'meshes', 'multi_plot', 'web', 'prototype'

    Returns:
        Recommended library name
    """
    recommendations = {
        'interactive_3d': 'plotly',
        'realtime': 'bokeh',
        'exploration': 'altair',
        'big_data': 'vispy',
        'meshes': 'pyvista',
        'multi_plot': 'holoviews',
        'web': 'dash',
        'prototype': 'streamlit'
    }

    return recommendations.get(use_case, 'plotly')
