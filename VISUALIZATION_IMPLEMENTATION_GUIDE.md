# Visualization Libraries: Quick Implementation Guide
## Python Scientific Data Visualization Code Examples

This guide provides ready-to-use code examples for the top 3 recommended visualization libraries.

---

## 1. PLOTLY: Interactive 3D Astronomical Data

### Installation
```bash
pip install plotly pandas
```

### Example 1: 3D Scatter Plot of Astronomical Survey
```python
import plotly.graph_objects as go
import plotly.express as px
import pandas as pd
import numpy as np

# Generate sample astronomical data
n_objects = 5000
data = pd.DataFrame({
    'ra': np.random.uniform(0, 360, n_objects),
    'dec': np.random.uniform(-90, 90, n_objects),
    'redshift': np.random.exponential(0.5, n_objects),
    'magnitude': np.random.normal(20, 2, n_objects),
    'spectral_type': np.random.choice(['O', 'B', 'A', 'F', 'G', 'K', 'M'], n_objects),
    'luminosity': np.random.lognormal(0, 1, n_objects)
})

# Simple 3D visualization
fig = px.scatter_3d(
    data,
    x='ra',
    y='dec',
    z='redshift',
    color='magnitude',
    size='luminosity',
    hover_data=['spectral_type', 'magnitude'],
    title='Astronomical Survey - Interactive 3D View',
    labels={
        'ra': 'Right Ascension (°)',
        'dec': 'Declination (°)',
        'redshift': 'Redshift',
        'magnitude': 'Apparent Magnitude'
    }
)

# Customize layout
fig.update_layout(
    width=1000,
    height=800,
    scene=dict(
        xaxis_title='RA',
        yaxis_title='Dec',
        zaxis_title='Redshift',
        camera=dict(
            eye=dict(x=1.5, y=1.5, z=1.2)
        )
    ),
    hovermode='closest'
)

fig.show()
```

### Example 2: Time-Series Animation (Variable Stars)
```python
# Create time-series data for variable stars
time_steps = 100
stars = 50

animated_data = []
for star_id in range(stars):
    for t in range(time_steps):
        animated_data.append({
            'time': t,
            'star_id': f'Star_{star_id}',
            'magnitude': 20 + 2 * np.sin(2 * np.pi * t / 20 + star_id),
            'flux': np.exp(-(20 + 2 * np.sin(2 * np.pi * t / 20 + star_id))/2.5),
            'period': 20 + 5 * np.random.random()
        })

data_time = pd.DataFrame(animated_data)

fig = px.scatter(
    data_time,
    x='time',
    y='magnitude',
    animation_frame='time',
    animation_group='star_id',
    size='flux',
    color='period',
    range_y=[15, 25],
    title='Variable Star Magnitude Over Time',
    labels={'magnitude': 'Apparent Magnitude', 'time': 'Time (days)'}
)

fig.show()
```

### Example 3: Hierarchical Clustering Visualization
```python
# 3D visualization of high-dimensional data reduction
from sklearn.preprocessing import StandardScaler
from sklearn.manifold import TSNE

# Project high-dimensional data to 3D
scaler = StandardScaler()
data_scaled = scaler.fit_transform(data[['ra', 'dec', 'redshift', 'magnitude']])

# Use t-SNE for visualization
tsne = TSNE(n_components=3, random_state=42)
coords_3d = tsne.fit_transform(data_scaled)

fig = go.Figure(data=[go.Scatter3d(
    x=coords_3d[:, 0],
    y=coords_3d[:, 1],
    z=coords_3d[:, 2],
    mode='markers',
    marker=dict(
        size=5,
        color=data['magnitude'],
        colorscale='Viridis',
        showscale=True,
        colorbar=dict(title='Magnitude')
    ),
    text=data['spectral_type'],
    hovertemplate='<b>%{text}</b><br>Magnitude: %{marker.color:.2f}'
)])

fig.update_layout(
    title='3D Astronomical Data Space',
    scene=dict(xaxis_title='t-SNE 1', yaxis_title='t-SNE 2', zaxis_title='t-SNE 3'),
    width=1000, height=800
)

fig.show()
```

### Example 4: Dash Web Dashboard
```python
import dash
from dash import dcc, html, Input, Output
import plotly.express as px

app = dash.Dash(__name__)

app.layout = html.Div([
    html.H1("Astronomical Data Dashboard"),

    html.Div([
        html.Label("Select Magnitude Range:"),
        dcc.RangeSlider(
            id='mag-slider',
            min=15, max=25, step=0.5,
            marks={i: str(i) for i in range(15, 26)},
            value=[18, 22]
        ),
    ], style={'width': '80%', 'margin': '20px auto'}),

    dcc.Graph(id='3d-scatter'),
    dcc.Graph(id='magnitude-hist'),

    dcc.Interval(id='interval-component', interval=5000)  # Update every 5s
])

@app.callback(
    [Output('3d-scatter', 'figure'), Output('magnitude-hist', 'figure')],
    Input('mag-slider', 'value')
)
def update_graphs(mag_range):
    filtered_data = data[
        (data['magnitude'] >= mag_range[0]) &
        (data['magnitude'] <= mag_range[1])
    ]

    fig_3d = px.scatter_3d(
        filtered_data,
        x='ra', y='dec', z='redshift',
        color='magnitude', size='luminosity'
    )

    fig_hist = px.histogram(
        filtered_data,
        x='magnitude',
        nbins=30,
        title='Magnitude Distribution'
    )

    return fig_3d, fig_hist

if __name__ == '__main__':
    app.run_server(debug=True)
```

---

## 2. VISPY: High-Performance Real-Time Visualization

### Installation
```bash
pip install vispy numpy
```

### Example 1: Real-Time Point Cloud Visualization
```python
import numpy as np
import vispy.plot as vp
from vispy import app

# Create interactive 3D scatter plot
canvas = vp.Plot(title='Real-Time Point Cloud')

# Generate large dataset
n_points = 1_000_000
x = np.random.randn(n_points)
y = np.random.randn(n_points)
z = np.random.randn(n_points)

# Create scatter plot with size and color
color = np.sqrt(x**2 + y**2 + z**2)
scatter = canvas.scatter(
    x, y, z,
    color=color,
    size=2,
    edge_width=0
)

# Add colorbar
canvas.colorbar()
canvas.show()
```

### Example 2: Streaming Real-Time Data
```python
from vispy import scene, app
from vispy.geometry import create_sphere
import numpy as np

class RealTimeVisualizer:
    def __init__(self):
        self.canvas = scene.SceneCanvas(title='Real-Time Streaming Data')
        self.view = self.canvas.central_widget.add_view()
        self.view.camera = scene.TurntableCamera(up='z')

        # Initialize with empty data
        self.scatter = scene.visuals.Markers()
        self.view.add(self.scatter)

        # Update timer
        self.timer = app.Timer(0.033)  # ~30 FPS
        self.timer.connect(self.on_timer)
        self.timer.start()

    def on_timer(self, event):
        # Simulate streaming data
        new_data = np.random.randn(100, 3)
        self.scatter.set_data(new_data, size=5)
        self.canvas.update()

# Create and run visualizer
viz = RealTimeVisualizer()
app.run()
```

### Example 3: High-Performance Line Plotting
```python
from vispy.plot import PlotCanvas
import numpy as np

# Create canvas
plot = PlotCanvas(title='High-Performance Time Series')

# Generate large time series dataset
n_points = 10_000_000
t = np.arange(n_points) / 1000  # 10k points
y = np.cumsum(np.random.randn(n_points)) / 100

# Plot line (efficient rendering)
line = plot.plot(t, y, title='Sensor Data Stream')
plot.show()
```

### Example 4: 3D Mesh Visualization
```python
from vispy import scene
from vispy.geometry import create_sphere
import numpy as np

canvas = scene.SceneCanvas(title='3D Mesh Visualization')
view = canvas.central_widget.add_view()
view.camera = scene.TurntableCamera()

# Create sphere mesh
sphere_data = create_sphere()
mesh = scene.visuals.Mesh(
    vertices=sphere_data.get_vertices(),
    faces=sphere_data.get_faces(),
    color=(0.5, 0.5, 1.0, 1.0)
)
view.add(mesh)

canvas.show()
```

---

## 3. PYVISTA: 3D Mesh and Celestial Visualization

### Installation
```bash
pip install pyvista numpy
```

### Example 1: Celestial Body Visualization
```python
import pyvista as pv
import numpy as np

# Create a sphere representing a planet
plotter = pv.Plotter(window_size=[800, 800])

# Earth
earth = pv.Sphere(radius=1.0, center=(0, 0, 0))
plotter.add_mesh(earth, color='blue', label='Earth')

# Moon
moon = pv.Sphere(radius=0.27, center=(4, 0, 0))
plotter.add_mesh(moon, color='gray', label='Moon')

# Sun
sun = pv.Sphere(radius=0.1, center=(-20, 0, 0))
plotter.add_mesh(sun, color='yellow', emissive=True, label='Sun')

# Add labels and camera settings
plotter.set_background('black')
plotter.show()
```

### Example 2: Orbital Mechanics Visualization
```python
import pyvista as pv
import numpy as np

# Create orbital visualization
plotter = pv.Plotter()

# Sun at origin
sun = pv.Sphere(radius=0.5)
plotter.add_mesh(sun, color='yellow')

# Create orbital paths
for i, (semi_major, color) in enumerate(
    [(5, 'blue'), (8, 'red'), (12, 'gray')]
):
    # Parametric orbit
    t = np.linspace(0, 2*np.pi, 100)
    x = semi_major * np.cos(t)
    y = semi_major * np.sin(t)
    z = np.zeros_like(t)

    # Plot orbit
    plotter.plot(
        x, y, z,
        color=color,
        line_width=2,
        label=f'Orbit {i+1}'
    )

    # Add planet on orbit
    planet = pv.Sphere(radius=0.3, center=(semi_major, 0, 0))
    plotter.add_mesh(planet, color=color)

plotter.camera_position = 'xy'
plotter.show()
```

### Example 3: Volumetric Data Visualization
```python
import pyvista as pv
import numpy as np

# Create volumetric data (e.g., atmospheric density)
x = np.arange(-10, 10, 1)
y = np.arange(-10, 10, 1)
z = np.arange(0, 20, 1)
X, Y, Z = np.meshgrid(x, y, z)

# Create density field
density = np.exp(-(X**2 + Y**2) / 10) * np.exp(-Z/5)

# Create structured grid
grid = pv.StructuredGrid(X, Y, Z)
grid['density'] = density.ravel()

# Visualize
plotter = pv.Plotter()
plotter.add_volume(
    grid,
    scalars='density',
    cmap='hot',
    scalar_bar_args={'title': 'Density'}
)
plotter.show()
```

### Example 4: Advanced Slicing and Clipping
```python
import pyvista as pv
import numpy as np

# Create a complex dataset
mesh = pv.Sphere(radius=2, theta_resolution=30, phi_resolution=30)

# Add color data
mesh['scalars'] = np.random.random(mesh.n_points)

# Create plotter with slicing capability
plotter = pv.Plotter()
plotter.add_mesh(mesh, cmap='viridis', show_edges=False)

# Add slice plane
origin = [0, 0, 0]
normal = [0, 0, 1]
sliced = mesh.slice(origin=origin, normal=normal)
plotter.add_mesh(sliced, color='red', line_width=3)

plotter.show()
```

---

## Performance Comparison: Example Benchmarking

```python
import time
import numpy as np
import matplotlib.pyplot as plt

def benchmark_visualization(library_name, create_viz_func, n_points_list):
    """Benchmark visualization library performance"""
    times = []

    for n_points in n_points_list:
        data = np.random.randn(n_points, 3)

        start = time.time()
        create_viz_func(data)
        elapsed = time.time() - start

        times.append(elapsed)
        print(f"{library_name} - {n_points:,} points: {elapsed:.3f}s")

    return times

# Example benchmarking
n_points_list = [10_000, 100_000, 1_000_000]

# Plotly benchmark
def plotly_viz(data):
    import plotly.graph_objects as go
    fig = go.Figure(data=[go.Scatter3d(x=data[:,0], y=data[:,1], z=data[:,2], mode='markers')])
    # Note: Plotly rendering is browser-based, so actual time includes rendering

# Vispy benchmark
def vispy_viz(data):
    import vispy.plot as vp
    canvas = vp.Plot()
    canvas.scatter(data[:, 0], data[:, 1], data[:, 2], size=2)

# PyVista benchmark
def pyvista_viz(data):
    import pyvista as pv
    cloud = pv.PolyData(data)
    plotter = pv.Plotter()
    plotter.add_mesh(cloud)

print("Visualization Library Performance Benchmark")
print("=" * 50)
# plotly_times = benchmark_visualization("Plotly", plotly_viz, n_points_list)
vispy_times = benchmark_visualization("Vispy", vispy_viz, n_points_list)
pyvista_times = benchmark_visualization("PyVista", pyvista_viz, n_points_list)
```

---

## Choosing Between Libraries: Decision Tree

```
Start here: "What do I need to visualize?"

├─ "Interactive web dashboard for multiple users"
│  └─→ Use DASH (built on Plotly)
│
├─ "3D interactive plots, moderate data size (<1M points)"
│  └─→ Use PLOTLY
│
├─ "Real-time streaming with millions of points"
│  └─→ Use VISPY
│
├─ "3D meshes, volumetric data, celestial bodies"
│  └─→ Use PYVISTA
│
├─ "Quick prototype with minimal code changes"
│  └─→ Use STREAMLIT (embed existing matplotlib)
│
└─ "Exploratory data analysis with lots of filtering"
   └─→ Use ALTAIR
```

---

## Integration with Existing Code

### Minimal-Effort Migration: Streamlit
```python
# Your existing matplotlib code
import matplotlib.pyplot as plt
import numpy as np

data = np.random.randn(1000, 3)

plt.figure(figsize=(8, 6))
plt.scatter(data[:, 0], data[:, 1], c=data[:, 2], cmap='viridis')
plt.colorbar()
plt.show()

# Convert to Streamlit (just wrap it):
import streamlit as st

st.title("My Visualization")

fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(data[:, 0], data[:, 1], c=data[:, 2], cmap='viridis')
st.pyplot(fig)
```

### Progressive Migration: Keep Matplotlib, Add Interactivity
```python
# Step 1: Keep existing matplotlib
import matplotlib.pyplot as plt

fig = create_matplotlib_plot()
plt.savefig('temp.png')

# Step 2: Add Plotly alongside
import plotly.graph_objects as go

fig = go.Figure()
fig.add_trace(go.Scatter(x=x, y=y, mode='markers'))
fig.show()

# Step 3: Eventually replace with pure Plotly
# (no matplotlib dependency)
```

---

## Production Deployment Checklist

### For Plotly + Dash:
- [ ] Install: `pip install dash plotly`
- [ ] Test locally: `python app.py`
- [ ] Deploy to Heroku/AWS/DigitalOcean
- [ ] Monitor performance for 100+ concurrent users
- [ ] Set up logging for errors

### For Vispy Desktop App:
- [ ] Install: `pip install vispy PyQt5 numpy`
- [ ] Test on multiple GPUs (NVIDIA, AMD, Intel)
- [ ] Package with PyInstaller for distribution
- [ ] Optimize shader code for your GPU hardware

### For PyVista Web (via Streamlit):
- [ ] Install: `pip install streamlit pyvista`
- [ ] Deploy to Streamlit Cloud (free tier available)
- [ ] Monitor memory usage (3D rendering intensive)
- [ ] Cache data appropriately with @st.cache_data

---

## Summary Table: When to Use Each

| Need | Use | Why |
|------|-----|-----|
| Quick 3D exploration | Plotly | Works out-of-box, interactive |
| Web dashboard | Dash | Production-ready, scalable |
| Real-time streaming | Vispy | Best performance for large data |
| Celestial visualization | PyVista | Purpose-built for 3D objects |
| Quick prototype | Streamlit | Minimal code changes |
| Publication plot | Plotly or PyVista | Professional appearance |
| Huge dataset (>100M) | Vispy | GPU acceleration essential |
| Exploratory analysis | Altair | Quick iteration, filtering |

