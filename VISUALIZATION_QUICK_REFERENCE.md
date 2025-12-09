# Visualization Libraries: Quick Reference Chart
## At-a-Glance Comparison for Scientific/Astronomical Data

---

## Feature Matrix

| Feature | Plotly | Bokeh | Altair | Vispy | PyVista | HoloViews | Dash | Streamlit |
|---------|--------|-------|--------|-------|---------|-----------|------|-----------|
| **3D Native Support** | ✓✓ Good | ✗ None | ✗ None | ✓✓ Excellent | ✓✓ Excellent | ✓ Good | ✓✓ Good (via Plotly) | ✓ Fair |
| **Real-Time Streaming** | ✓ Good | ✓✓ Excellent | ✗ Poor | ✓✓ Excellent | ✗ Limited | ✓ Good | ✓ Good | ✓ Fair |
| **Large Datasets (1M+)** | ✓ Good | ✓ Good | ✗ Limited | ✓✓ Excellent | ~ Medium | ✓ Good | ✓ Medium | ✓ Medium |
| **Publication Quality** | ✓✓ Excellent | ✓ Good | ✓✓ Excellent | ✓ Good | ✓✓ Excellent | ✓ Good | ✓ Very Good | ✓ Good |
| **Interactive Features** | ✓✓ Excellent | ✓✓ Excellent | ✓✓ Excellent | ✓ Good | ✓ Good | ✓✓ Excellent | ✓✓ Excellent | ✓ Good |
| **Animation Support** | ✓✓ Built-in | ✗ Limited | ✗ Limited | ✓ Possible | ✓ Possible | ✓ Good | ✓ Good (via callbacks) | ✓ Possible |
| **Browser-Based** | ✓ Yes | ✓ Yes | ✓ Yes | ✗ Limited | ✗ Limited | ✓ Yes | ✓ Yes | ✓ Yes |
| **Desktop Apps** | ✓ Possible | ✓ Yes | ✗ Limited | ✓✓ Excellent | ✓✓ Excellent | ✓ Yes | ✗ Not ideal | ✗ Not ideal |
| **Production Deploy** | ✓ Yes | ✓ Yes | ✗ Limited | ✓ Yes | ✓ Via web | ✓ Yes | ✓✓ Excellent | ✓ Easy |
| **Learning Curve** | ✓ Moderate | ✓ Moderate | ✓ Easy | ✓ Steep | ✓ Easy | ✓ Moderate | ✓ Moderate | ✓ Very Easy |

---

## Performance Ratings

### Maximum Dataset Size (Interactive Performance)

```
Vispy:      ████████████████████ 100M+ points
Bokeh:      ███████████ 10M points
Plotly:     █████████ 5M points
PyVista:    █████████ 5M points (mesh-dependent)
HoloViews:  ███████ 2M points
Dash:       ███████ 2M points
Streamlit:  ████ 500K points
Altair:     ███ 50K points
```

### Real-Time Update Rate (Points/Second)

```
Vispy:      ████████████████████ 100K+/sec
Bokeh:      ██████████████████ 50K/sec
Plotly:     ████████████ 10K/sec
HoloViews:  ████████████ 10K/sec
Dash:       ████████ 5K/sec
PyVista:    ████ 2K/sec
Streamlit:  ███ 1K/sec
Altair:     ██ 100/sec
```

### Publication Quality (1-5 scale)

```
Plotly:     ★★★★★ (5.0)
PyVista:    ★★★★★ (5.0)
Altair:     ★★★★☆ (4.5)
Bokeh:      ★★★★☆ (4.0)
Dash:       ★★★★☆ (4.0)
HoloViews:  ★★★☆☆ (3.5)
Vispy:      ★★★☆☆ (3.5)
Streamlit:  ★★★☆☆ (3.5)
```

---

## Integration Effort (Matplotlib → New Library)

### Code Rewrite Required

```
Streamlit:  ▓░░░░░░░░░░ 10-20%
Altair:     ▓▓▓░░░░░░░░ 30-50%
HoloViews:  ▓▓░░░░░░░░░ 20-40%
Plotly:     ▓▓▓▓░░░░░░░ 40-60%
PyVista:    ▓▓▓▓░░░░░░░ 40-60%
Dash:       ▓▓▓▓▓░░░░░░ 50-80%
Bokeh:      ▓▓▓▓▓░░░░░░ 50-70%
Vispy:      ▓▓▓▓▓▓▓░░░░ 70-90%
```

### Time to Proficiency

```
Streamlit:  █░░░░░░░░░░░░░ 1-3 days
Altair:     ██░░░░░░░░░░░░ 1 week
Plotly:     ███░░░░░░░░░░░ 1-2 weeks
PyVista:    ███░░░░░░░░░░░ 1-2 weeks
HoloViews:  ███░░░░░░░░░░░ 1-2 weeks
Dash:       ████░░░░░░░░░░ 2-3 weeks
Bokeh:      █████░░░░░░░░░ 2-3 weeks
Vispy:      ██████░░░░░░░░ 3-4 weeks
```

---

## Best Use Cases by Domain

### Astronomy/NASA
```
🥇 Gold:   Plotly (3D, interactive, NASA-integrated)
🥈 Silver: PyVista (celestial bodies, 3D mesh)
🥉 Bronze: Bokeh (real-time observation data)
```

### High-Frequency Real-Time Data
```
🥇 Gold:   Vispy (100K+ points/sec)
🥈 Silver: Bokeh (50K+ points/sec)
🥉 Bronze: Plotly (10K+ points/sec)
```

### Large Survey Data (Millions of Objects)
```
🥇 Gold:   Vispy (100M+ efficient)
🥈 Silver: Bokeh (10M efficient)
🥉 Bronze: Plotly (5M reasonable)
```

### Web Dashboards
```
🥇 Gold:   Dash (production-ready)
🥈 Silver: Streamlit (rapid development)
🥉 Bronze: Plotly (standalone, needs Dash for scalability)
```

### Publication-Quality 3D
```
🥇 Gold:   PyVista (purpose-built)
🥈 Silver: Plotly (excellent interactivity)
🥉 Bronze: Bokeh (2D only)
```

### Quick Prototyping
```
🥇 Gold:   Streamlit (minimal code)
🥈 Silver: Altair (declarative, simple)
🥉 Bronze: Plotly (high-level, easy)
```

---

## Installation & Dependencies

### Minimal Installation

```bash
# Plotly: Simplest, fewest dependencies
pip install plotly pandas

# Streamlit: Includes visualization support
pip install streamlit

# Altair: Pure Python, minimal deps
pip install altair pandas

# Bokeh: Requires server for real-time
pip install bokeh

# PyVista: Based on VTK, larger install
pip install pyvista

# Vispy: GPU libraries required
pip install vispy numpy PyQt5

# Dash: Full web framework
pip install dash plotly

# HoloViews: Part of Holoviz ecosystem
pip install holoviews bokeh
```

---

## Decision Guide: Which Library?

### If you answer YES to these questions:

**Q1: Do you need a web-based dashboard for multiple users?**
- YES → Use **DASH**
- NO  → Continue to Q2

**Q2: Do you have real-time data streaming at >1000 Hz?**
- YES → Use **VISPY**
- NO  → Continue to Q3

**Q3: Are you visualizing 3D meshes or celestial bodies?**
- YES → Use **PYVISTA**
- NO  → Continue to Q4

**Q4: Do you have >10M points to visualize?**
- YES → Use **VISPY**
- NO  → Continue to Q5

**Q5: Is this a quick prototype or exploratory analysis?**
- YES → Use **STREAMLIT** or **ALTAIR**
- NO  → Continue to Q6

**Q6: Do you need interactive 3D visualization?**
- YES → Use **PLOTLY**
- NO  → Use **ALTAIR**

---

## Strengths Summary

### PLOTLY ✓
- Interactive 3D plots
- Beautiful automatic styling
- NASA-recommended
- Animations easy to create
- Browser-based interactivity
- **Best for**: Scientific dashboards with 3D

### VISPY ✓
- Extreme performance
- Real-time streaming
- Handles 100M+ points
- GPU-accelerated
- Custom visualizations
- **Best for**: High-frequency real-time data

### PYVISTA ✓
- 3D meshes and volumes
- Celestial body support
- Slicing and clipping tools
- Professional 3D output
- VTK integration
- **Best for**: Complex 3D scientific data

### BOKEH ✓
- Real-time streaming
- Server-based architecture
- Large 2D datasets
- Customizable interactions
- Python integration
- **Best for**: Real-time 2D monitoring

### ALTAIR ✓
- Declarative, simple syntax
- Quick iteration
- Interactive filtering
- Beautiful defaults
- Publication-ready
- **Best for**: Exploratory analysis

### DASH ✓
- Production web framework
- Multi-user deployment
- Complete solution
- Professional appearance
- Scalable architecture
- **Best for**: Production dashboards

### STREAMLIT ✓
- Easiest to learn
- Minimal code required
- Quick prototyping
- Easy sharing
- Built-in deployment
- **Best for**: Rapid development

### HOLOVIEWS ✓
- Multi-backend support
- Streaming data
- Composable visualizations
- Flexible
- Scientific stack integration
- **Best for**: Complex multi-plot dashboards

---

## Weaknesses Summary

### PLOTLY ✗
- Struggles with >10M points
- Browser rendering limitations
- Limited customization
- Requires HTML/JavaScript knowledge for advanced

### VISPY ✗
- Steep learning curve
- Limited browser support
- Smaller community
- GPU hardware dependent

### PYVISTA ✗
- Not ideal for pure point clouds
- Limited real-time streaming
- Desktop-focused
- Larger memory footprint

### BOKEH ✗
- No 3D support
- More complex than Plotly for simple plots
- Server overhead

### ALTAIR ✗
- Limited to ~50K points
- No 3D support
- Less customizable

### DASH ✗
- More complex than Streamlit
- Requires server infrastructure
- Steeper learning curve

### STREAMLIT ✗
- Not ideal for real-time (full rerun)
- Limited customization
- Performance degrades with large apps
- Not production-grade scaling

### HOLOVIEWS ✗
- Added abstraction complexity
- Depends on backend choice
- Steeper learning curve

---

## Cost & Licensing

| Library | License | Cost | Commercial Support |
|---------|---------|------|-------------------|
| Plotly | MIT | Free (commercial options) | ✓ Yes (Dash Enterprise) |
| Bokeh | BSD 3-Clause | Free | ✓ Limited |
| Altair | BSD 3-Clause | Free | ✓ Community |
| Vispy | BSD 3-Clause | Free | ✗ Community |
| PyVista | MIT | Free | ✓ Community |
| HoloViews | BSD 3-Clause | Free | ✓ Community |
| Dash | MIT | Free (commercial options) | ✓ Yes (Dash Enterprise) |
| Streamlit | Apache 2.0 | Free (Streamlit Cloud) | ✓ Limited |

---

## Community Size & Support

```
Plotly:     ████████████████████ Huge community, many tutorials
Streamlit:  ██████████████████ Large growing community
Bokeh:      ███████████████ Active, good documentation
Altair:     ███████████ Growing, academic focus
Vispy:      ████████ Smaller, specialized
PyVista:    ████████ Growing, scientific
HoloViews:  ████████ Moderate, research-focused
Dash:       ███████████████████ Large through Plotly
```

---

## Recommendations Summary

### For NASA/Astronomical Data:

**Tier 1 (Recommended):**
1. **Plotly** - Best overall, NASA-integrated, 3D, interactive
2. **Vispy** - When you need extreme performance
3. **PyVista** - For 3D meshes and celestial bodies

**Tier 2 (Good alternatives):**
4. **Bokeh** - Real-time observing dashboards
5. **Dash** - Production web dashboards

**Tier 3 (Specific use cases):**
6. **Streamlit** - Quick prototyping and sharing
7. **Altair** - Exploratory analysis

---

## Quick Start Templates

### Template 1: 3D Astronomical Plot (Plotly)
```python
import plotly.express as px
import pandas as pd

data = pd.DataFrame({...})  # Your astronomical data
fig = px.scatter_3d(data, x='ra', y='dec', z='redshift',
                    color='magnitude', size='flux')
fig.show()
```

### Template 2: Real-Time Dashboard (Dash)
```python
import dash
from dash import dcc, html, Input, Output

app = dash.Dash()
app.layout = html.Div([
    dcc.Graph(id='graph'),
    dcc.Interval(id='interval', interval=1000)
])

@app.callback(Output('graph', 'figure'),
              Input('interval', 'n_intervals'))
def update(n):
    return create_figure()

if __name__ == '__main__':
    app.run_server()
```

### Template 3: Real-Time Streaming (Vispy)
```python
import vispy.plot as vp

canvas = vp.Plot()
scatter = canvas.scatter(x, y, z, size=2)
canvas.show()
```

### Template 4: Celestial Visualization (PyVista)
```python
import pyvista as pv

plotter = pv.Plotter()
earth = pv.Sphere(radius=1.0)
plotter.add_mesh(earth, color='blue')
plotter.show()
```

