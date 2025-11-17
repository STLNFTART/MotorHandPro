#!/usr/bin/env python3
"""
LAM Integration with Satellite Constellation System
Allows LAM to monitor, query, and manage satellite operations
"""
import sys
import json
from pathlib import Path
from typing import Dict, Any, List, Optional
from datetime import datetime

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    from integrations.satellite_orbital_mechanics import SatelliteOrbitSimulator
    from integrations.data_capture import IntegrationDataCapture
    SATELLITE_AVAILABLE = True
except ImportError:
    SATELLITE_AVAILABLE = False
    print("Warning: Satellite integration modules not found")


class LAMSatelliteInterface:
    """
    Interface between LAM and Satellite Constellation System
    Enables LAM to monitor and manage satellite operations
    """

    def __init__(self):
        if not SATELLITE_AVAILABLE:
            raise ImportError("Satellite integration modules not available")

        # Initialize satellite simulator
        self.simulator = None
        self.data_capture = IntegrationDataCapture()

    def initialize_constellation(self, num_satellites: int = 50000,
                                altitude_km: float = 550.0) -> Dict[str, Any]:
        """
        Initialize satellite constellation
        """
        print(f"Initializing {num_satellites} satellite constellation at {altitude_km}km...")

        self.simulator = SatelliteOrbitSimulator(
            num_satellites=num_satellites,
            altitude_km=altitude_km
        )

        result = {
            "success": True,
            "constellation": {
                "num_satellites": num_satellites,
                "altitude_km": altitude_km,
                "initialized_at": datetime.now().isoformat()
            }
        }

        self.data_capture.record_event("constellation_initialized", result)
        return result

    def get_satellite_status(self, satellite_id: Optional[int] = None) -> Dict[str, Any]:
        """
        Get status of all satellites or specific satellite
        """
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        if satellite_id is not None:
            # Get specific satellite
            if 0 <= satellite_id < self.simulator.num_satellites:
                position = self.simulator.satellite_positions[satellite_id]
                velocity = self.simulator.satellite_velocities[satellite_id]

                return {
                    "satellite_id": satellite_id,
                    "position": {
                        "x": float(position[0]),
                        "y": float(position[1]),
                        "z": float(position[2])
                    },
                    "velocity": {
                        "vx": float(velocity[0]),
                        "vy": float(velocity[1]),
                        "vz": float(velocity[2])
                    },
                    "altitude_km": self.simulator.altitude_km,
                    "orbital_period_min": self.simulator.orbital_period / 60.0
                }
            else:
                return {"error": f"Satellite ID {satellite_id} out of range"}

        # Get constellation summary
        return {
            "total_satellites": self.simulator.num_satellites,
            "altitude_km": self.simulator.altitude_km,
            "orbital_period_min": self.simulator.orbital_period / 60.0,
            "status": "operational"
        }

    def propagate_orbits(self, duration_seconds: float = 60.0) -> Dict[str, Any]:
        """
        Propagate satellite orbits forward in time
        """
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        print(f"Propagating orbits for {duration_seconds} seconds...")

        initial_positions = self.simulator.satellite_positions.copy()

        # Propagate
        self.simulator.propagate(duration_seconds)

        final_positions = self.simulator.satellite_positions

        # Calculate some metrics
        max_displacement = 0.0
        for i in range(self.simulator.num_satellites):
            dx = final_positions[i] - initial_positions[i]
            displacement = (dx[0]**2 + dx[1]**2 + dx[2]**2)**0.5
            max_displacement = max(max_displacement, displacement)

        result = {
            "success": True,
            "duration_seconds": duration_seconds,
            "max_displacement_km": float(max_displacement),
            "timestamp": datetime.now().isoformat()
        }

        self.data_capture.record_event("orbit_propagation", result)
        return result

    def detect_collisions(self, threshold_km: float = 5.0) -> Dict[str, Any]:
        """
        Detect potential satellite collisions
        """
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        collisions = []
        positions = self.simulator.satellite_positions

        # Simple pairwise distance check (optimized for small samples)
        num_check = min(1000, self.simulator.num_satellites)

        for i in range(num_check):
            for j in range(i + 1, num_check):
                dx = positions[i] - positions[j]
                distance = (dx[0]**2 + dx[1]**2 + dx[2]**2)**0.5

                if distance < threshold_km:
                    collisions.append({
                        "satellite_1": i,
                        "satellite_2": j,
                        "distance_km": float(distance)
                    })

        result = {
            "total_checked": num_check,
            "collisions_detected": len(collisions),
            "threshold_km": threshold_km,
            "collisions": collisions[:10]  # Limit to first 10
        }

        if len(collisions) > 0:
            self.data_capture.record_event("collision_alert", result)

        return result

    def get_coverage_statistics(self) -> Dict[str, Any]:
        """
        Calculate global coverage statistics
        """
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        # Basic coverage metrics
        total_satellites = self.simulator.num_satellites
        altitude_km = self.simulator.altitude_km

        # Simplified coverage calculation
        earth_radius = 6371.0  # km
        satellite_radius = earth_radius + altitude_km

        # Visible area per satellite (simplified)
        horizon_angle = (earth_radius / satellite_radius)
        visible_area_percent = (1 - horizon_angle) * 100

        # Constellation coverage (very simplified)
        total_coverage_percent = min(100.0, visible_area_percent * (total_satellites / 1000.0))

        return {
            "total_satellites": total_satellites,
            "altitude_km": altitude_km,
            "satellites_per_1000": total_satellites / 1000.0,
            "estimated_coverage_percent": round(total_coverage_percent, 2),
            "orbital_period_min": self.simulator.orbital_period / 60.0
        }

    def analyze_constellation_health(self) -> Dict[str, Any]:
        """
        Comprehensive constellation health analysis
        """
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        # Check various health metrics
        status = self.get_satellite_status()
        collisions = self.detect_collisions()
        coverage = self.get_coverage_statistics()

        # Health score
        health_score = 100.0

        # Deduct for collisions
        if collisions['collisions_detected'] > 0:
            health_score -= min(50, collisions['collisions_detected'] * 10)

        # Coverage check
        if coverage['estimated_coverage_percent'] < 50:
            health_score -= 20

        health_status = "EXCELLENT" if health_score >= 90 else \
                       "GOOD" if health_score >= 70 else \
                       "FAIR" if health_score >= 50 else "POOR"

        result = {
            "health_score": round(health_score, 2),
            "health_status": health_status,
            "metrics": {
                "total_satellites": status['total_satellites'],
                "collisions": collisions['collisions_detected'],
                "coverage_percent": coverage['estimated_coverage_percent']
            },
            "recommendations": self._generate_recommendations(health_score, collisions, coverage)
        }

        self.data_capture.record_event("health_analysis", result)
        return result

    def _generate_recommendations(self, health_score: float,
                                 collisions: Dict, coverage: Dict) -> List[str]:
        """Generate operational recommendations"""
        recommendations = []

        if collisions['collisions_detected'] > 0:
            recommendations.append(f"⚠️  {collisions['collisions_detected']} collision risks detected - perform avoidance maneuvers")

        if coverage['estimated_coverage_percent'] < 75:
            recommendations.append(f"📡 Coverage at {coverage['estimated_coverage_percent']}% - consider deploying additional satellites")

        if health_score >= 90:
            recommendations.append("✓ All systems nominal - maintain current operations")
        elif health_score < 50:
            recommendations.append("🚨 Critical issues detected - immediate attention required")

        return recommendations

    def export_telemetry(self, output_file: Path) -> Dict[str, Any]:
        """Export constellation telemetry data"""
        if self.simulator is None:
            return {"error": "Constellation not initialized"}

        telemetry = {
            "timestamp": datetime.now().isoformat(),
            "constellation": {
                "num_satellites": self.simulator.num_satellites,
                "altitude_km": self.simulator.altitude_km,
                "orbital_period_min": self.simulator.orbital_period / 60.0
            },
            "positions": [],
            "velocities": []
        }

        # Export subset of satellite data
        num_export = min(100, self.simulator.num_satellites)
        for i in range(num_export):
            pos = self.simulator.satellite_positions[i]
            vel = self.simulator.satellite_velocities[i]

            telemetry["positions"].append({
                "id": i,
                "x": float(pos[0]),
                "y": float(pos[1]),
                "z": float(pos[2])
            })

            telemetry["velocities"].append({
                "id": i,
                "vx": float(vel[0]),
                "vy": float(vel[1]),
                "vz": float(vel[2])
            })

        with open(output_file, 'w') as f:
            json.dump(telemetry, f, indent=2)

        return {
            "success": True,
            "file": str(output_file),
            "satellites_exported": num_export
        }


def main():
    """Test satellite integration"""
    print("=== LAM Satellite Integration Test ===\n")

    if not SATELLITE_AVAILABLE:
        print("ERROR: Satellite modules not available")
        return

    interface = LAMSatelliteInterface()

    # Initialize constellation
    print("1. Initializing constellation...")
    init_result = interface.initialize_constellation(num_satellites=1000, altitude_km=550)
    print(json.dumps(init_result, indent=2))

    # Get status
    print("\n2. Getting constellation status...")
    status = interface.get_satellite_status()
    print(json.dumps(status, indent=2))

    # Propagate orbits
    print("\n3. Propagating orbits...")
    prop_result = interface.propagate_orbits(duration_seconds=300)
    print(json.dumps(prop_result, indent=2))

    # Check for collisions
    print("\n4. Detecting collisions...")
    collisions = interface.detect_collisions(threshold_km=10.0)
    print(json.dumps(collisions, indent=2))

    # Coverage stats
    print("\n5. Coverage statistics...")
    coverage = interface.get_coverage_statistics()
    print(json.dumps(coverage, indent=2))

    # Health analysis
    print("\n6. Health analysis...")
    health = interface.analyze_constellation_health()
    print(json.dumps(health, indent=2))


if __name__ == "__main__":
    main()
