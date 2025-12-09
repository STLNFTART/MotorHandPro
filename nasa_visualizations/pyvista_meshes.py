#!/usr/bin/env python3
"""
PyVista 3D Mesh Visualization for NASA Data
Specializes in: 3D meshes, celestial bodies, volumetric data

Features:
- 3D mesh visualization for celestial objects
- Volumetric rendering
- Planetary surfaces and orbits
- Publication-quality 3D output

Author: Donte Lightfoot
Date: December 4, 2025
"""

import numpy as np
import warnings
from typing import List, Optional, Tuple

try:
    import pyvista as pv
    HAVE_PYVISTA = True
except ImportError:
    HAVE_PYVISTA = False
    warnings.warn("PyVista not available. Install with: pip install pyvista")


class PyVistaComet3DVisualizer:
    """3D mesh visualization using PyVista"""

    def __init__(self):
        """Initialize PyVista visualizer"""
        if not HAVE_PYVISTA:
            raise ImportError("PyVista is required")

        # Set theme
        pv.set_plot_theme("dark")

    def create_comet_nucleus_mesh(
        self,
        radius: float = 5.0,
        resolution: int = 50
    ) -> pv.PolyData:
        """
        Create 3D mesh of comet nucleus

        Args:
            radius: Nucleus radius (km)
            resolution: Mesh resolution

        Returns:
            PyVista mesh object
        """
        # Create irregular sphere for nucleus
        sphere = pv.Sphere(radius=radius, theta_resolution=resolution,
                          phi_resolution=resolution)

        # Add surface roughness
        points = sphere.points
        noise = np.random.normal(0, radius * 0.1, points.shape[0])
        points_radii = np.linalg.norm(points, axis=1)
        points = points * ((points_radii + noise) / points_radii)[:, np.newaxis]
        sphere.points = points

        return sphere

    def create_coma_volume(
        self,
        center: Tuple[float, float, float],
        max_radius: float = 100.0,
        density_decay: float = 0.1
    ) -> pv.ImageData:
        """
        Create volumetric coma (gas cloud)

        Args:
            center: Coma center position
            max_radius: Maximum coma extent (km)
            density_decay: Density decay rate

        Returns:
            PyVista volume data
        """
        # Create 3D grid
        grid_size = 64
        x = np.linspace(-max_radius, max_radius, grid_size)
        y = np.linspace(-max_radius, max_radius, grid_size)
        z = np.linspace(-max_radius, max_radius, grid_size)

        xx, yy, zz = np.meshgrid(x, y, z, indexing='ij')

        # Compute distance from center
        dist = np.sqrt((xx - center[0])**2 + (yy - center[1])**2 + (zz - center[2])**2)

        # Exponential density decay
        density = np.exp(-density_decay * dist / max_radius)

        # Create volume
        volume = pv.ImageData(dimensions=(grid_size, grid_size, grid_size))
        volume.origin = (-max_radius, -max_radius, -max_radius)
        volume.spacing = (2 * max_radius / grid_size,) * 3
        volume['density'] = density.flatten(order='F')

        return volume

    def create_trajectory_tube(
        self,
        observations: List,
        radius: float = 1.0
    ) -> pv.PolyData:
        """
        Create tube following comet trajectory

        Args:
            observations: Comet observations
            radius: Tube radius

        Returns:
            PyVista tube mesh
        """
        # Extract positions (simplified - RA/Dec to Cartesian)
        points = []
        for obs in observations:
            # Convert RA/Dec to approximate Cartesian
            ra_rad = np.deg2rad(obs.ra)
            dec_rad = np.deg2rad(obs.dec)
            r = obs.distance_au * 149597870.7  # AU to km

            x = r * np.cos(dec_rad) * np.cos(ra_rad)
            y = r * np.cos(dec_rad) * np.sin(ra_rad)
            z = r * np.sin(dec_rad)

            points.append([x, y, z])

        points = np.array(points)

        # Create spline
        spline = pv.Spline(points, n_points=len(points) * 2)

        # Create tube
        tube = spline.tube(radius=radius * 1e6)  # Convert to km

        return tube

    def create_3d_scene(
        self,
        observations: List,
        states: List,
        output_path: str = "pyvista_3d_scene.png",
        interactive: bool = False
    ):
        """
        Create comprehensive 3D scene

        Args:
            observations: Comet observations
            states: Recursive Planck states
            output_path: Output image file
            interactive: Show interactive window
        """
        if not observations:
            warnings.warn("No data provided")
            return

        # Create plotter
        plotter = pv.Plotter(window_size=[1600, 1200])

        # Add comet nucleus
        nucleus = self.create_comet_nucleus_mesh(radius=5.0)

        # Position nucleus at latest observation
        latest = observations[-1]
        ra_rad = np.deg2rad(latest.ra)
        dec_rad = np.deg2rad(latest.dec)
        r = latest.distance_au * 149597870.7

        nucleus_center = [
            r * np.cos(dec_rad) * np.cos(ra_rad),
            r * np.cos(dec_rad) * np.sin(ra_rad),
            r * np.sin(dec_rad)
        ]

        nucleus.translate(nucleus_center, inplace=True)
        plotter.add_mesh(nucleus, color='gray', show_edges=True, opacity=1.0,
                        label='Comet Nucleus')

        # Add coma (gas cloud)
        coma = self.create_coma_volume(nucleus_center, max_radius=2e6)
        plotter.add_volume(coma, cmap='plasma', opacity='sigmoid',
                          scalar_bar_args={'title': 'Gas Density'})

        # Add trajectory
        if len(observations) > 1:
            trajectory = self.create_trajectory_tube(observations, radius=0.5)

            # Color by anomaly score
            anomaly_scores = [min(1.0, s.error / 10.0) for s in states]
            # Interpolate anomaly scores for trajectory points
            traj_anomalies = np.interp(
                np.linspace(0, len(anomaly_scores)-1, trajectory.n_points),
                np.arange(len(anomaly_scores)),
                anomaly_scores
            )
            trajectory['anomaly'] = traj_anomalies

            plotter.add_mesh(trajectory, scalars='anomaly', cmap='coolwarm',
                            scalar_bar_args={'title': 'Anomaly Score'},
                            label='Trajectory')

        # Add Earth at origin
        earth = pv.Sphere(radius=6371, center=(0, 0, 0))
        plotter.add_mesh(earth, color='blue', opacity=0.8, label='Earth')

        # Add Sun (approximate)
        sun_distance = 1.496e8  # 1 AU in km
        sun = pv.Sphere(radius=696000, center=(sun_distance, 0, 0))
        plotter.add_mesh(sun, color='yellow', emissive=True, opacity=0.9, label='Sun')

        # Add grid planes
        plotter.show_grid(color='white', opacity=0.1)

        # Add axes
        plotter.add_axes(interactive=True)

        # Add title
        title_text = (
            f"3I/ATLAS 3D Scene (PyVista)\n"
            f"Latest: RA {latest.ra:.2f}°, Dec {latest.dec:.2f}°, "
            f"Distance {latest.distance_au:.3f} AU"
        )
        plotter.add_text(title_text, position='upper_edge', font_size=12, color='white')

        # Add legend
        plotter.add_legend(bcolor='black', border=True, size=[0.2, 0.2])

        # Camera position
        plotter.camera_position = 'xz'
        plotter.camera.zoom(0.8)

        # Save or show
        if interactive:
            plotter.show()
        else:
            plotter.screenshot(output_path)
            print(f"✅ PyVista 3D scene saved: {output_path}")

        return plotter

    def create_animated_rotation(
        self,
        observations: List,
        states: List,
        output_path: str = "pyvista_rotation.gif",
        n_frames: int = 36
    ):
        """
        Create animated rotation of 3D scene

        Args:
            observations: Comet observations
            states: Recursive Planck states
            output_path: Output GIF file
            n_frames: Number of frames
        """
        print(f"🎬 Creating animated rotation ({n_frames} frames)...")

        plotter = pv.Plotter(window_size=[800, 600], off_screen=True)

        # Build scene (simplified)
        if not observations:
            return

        latest = observations[-1]
        nucleus = self.create_comet_nucleus_mesh(radius=5.0)

        ra_rad = np.deg2rad(latest.ra)
        dec_rad = np.deg2rad(latest.dec)
        r = latest.distance_au * 149597870.7

        nucleus_center = [
            r * np.cos(dec_rad) * np.cos(ra_rad),
            r * np.cos(dec_rad) * np.sin(ra_rad),
            r * np.sin(dec_rad)
        ]

        nucleus.translate(nucleus_center, inplace=True)
        plotter.add_mesh(nucleus, color='cyan', show_edges=True)

        # Add Earth
        earth = pv.Sphere(radius=6371, center=(0, 0, 0))
        plotter.add_mesh(earth, color='blue', opacity=0.8)

        # Setup camera path
        plotter.open_gif(output_path)

        angles = np.linspace(0, 360, n_frames)
        for angle in angles:
            plotter.camera.azimuth = angle
            plotter.write_frame()

        plotter.close()
        print(f"✅ PyVista animation saved: {output_path}")


# Example usage
if __name__ == "__main__":
    print("=" * 80)
    print("📊 PYVISTA 3D MESH VISUALIZATION")
    print("=" * 80)
    print()

    if not HAVE_PYVISTA:
        print("❌ PyVista not installed")
        print("   Install with: pip install pyvista")
        exit(1)

    print("✅ PyVista available")
    print()
    print("Features:")
    print("  - 3D mesh visualization for celestial objects")
    print("  - Volumetric rendering (coma gas cloud)")
    print("  - Planetary surfaces and orbits")
    print("  - Publication-quality 3D output")
    print("  - Animation support")
    print()
    print("Usage:")
    print("  visualizer = PyVistaComet3DVisualizer()")
    print("  visualizer.create_3d_scene(observations, states)")
    print("  visualizer.create_animated_rotation(observations, states)")
    print()
    print("=" * 80)
