#!/usr/bin/env python3
"""
Primal Swarm Parameter Sweep
Uses unified experiment framework to test swarm configurations
"""
import sys
import math
import random
from pathlib import Path

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent / "extras" / "primal"))

from experiments.framework import ParamGrid, run_parameter_sweep

try:
    from primal_constants import COMM_RANGE_M, LAMBDA, TAU, R_EARTH as R
except ImportError:
    COMM_RANGE_M = 13500.0  # 13.5 km
    LAMBDA = 0.0893
    TAU = 0.0997
    R = 6371000.0  # Earth radius in meters


def haversine_m(lat1, lon1, lat2, lon2):
    """Calculate distance between two points on Earth in meters"""
    φ1, λ1, φ2, λ2 = map(math.radians, [lat1, lon1, lat2, lon2])
    dφ = φ2 - φ1
    dλ = λ2 - λ1
    a = math.sin(dφ/2)**2 + math.cos(φ1)*math.cos(φ2)*math.sin(dλ/2)**2
    return 2 * R * math.asin(math.sqrt(a))


def move(lat, lon, brg_deg, dist_m):
    """Move from lat/lon by distance in given bearing"""
    br = math.radians(brg_deg)
    lat1, lon1 = map(math.radians, [lat, lon])
    dR = dist_m / R
    lat2 = math.asin(math.sin(lat1)*math.cos(dR) + math.cos(lat1)*math.sin(dR)*math.cos(br))
    lon2 = lon1 + math.atan2(
        math.sin(br)*math.sin(dR)*math.cos(lat1),
        math.cos(dR) - math.sin(lat1)*math.sin(lat2)
    )
    return math.degrees(lat2), ((math.degrees(lon2)+540) % 360) - 180


def trust(dt, q, lambda_val, tau_val):
    """Calculate trust value"""
    return tau_val + (1.0 - tau_val) * math.exp(-lambda_val * dt) * max(0.0, min(1.0, q))


def simulate_swarm(config):
    """
    Run a single swarm configuration

    Args:
        config: dict with keys:
            - n_drones: number of drones
            - duration_s: simulation duration in seconds
            - dt: time step in seconds
            - lambda_val: trust decay rate
            - tau_val: minimum trust level

    Returns:
        (metrics, time_series) tuple
    """
    n_drones = config['n_drones']
    duration_s = config['duration_s']
    dt = config['dt']
    lambda_val = config.get('lambda_val', LAMBDA)
    tau_val = config.get('tau_val', TAU)

    # Initialize drones
    rng = random.Random(42)  # Fixed seed for reproducibility
    base_lat, base_lon = 37.7749, -122.4194
    speeds = [6, 8, 10, 12]  # m/s

    drones = []
    for i in range(n_drones):
        sp = rng.choice(speeds)
        drones.append({
            "id": f"uav-{i:05d}",
            "lat": base_lat + rng.uniform(-0.002, 0.002),
            "lon": base_lon + rng.uniform(-0.002, 0.002),
            "alt": rng.uniform(120, 240),
            "course": rng.uniform(0, 360),
            "speed": sp,
            "last_seen_ts": 0.0,
            "q": 0.9,
            "role": "WING"
        })

    # Run simulation
    time_series = {
        "t": [],
        "avg_neighbors": [],
        "avg_trust": [],
        "leaders": [],
        "relays": [],
        "wings": []
    }

    t = 0.0
    max_neighbors = 0
    min_trust = 1.0
    total_neighbor_counts = []

    while t < duration_s:
        # Move drones
        for d in drones:
            d["course"] = (d["course"] + rng.uniform(-3, 3)) % 360
            step = d["speed"] * dt
            d["lat"], d["lon"] = move(d["lat"], d["lon"], d["course"], step)

        # Calculate metrics
        neighbor_counts = []
        trust_values = []
        role_counts = {"LEADER": 0, "RELAY": 0, "WING": 0}

        for d in drones:
            # Count neighbors
            nbrs = 0
            for o in drones:
                if o is d:
                    continue
                if haversine_m(d["lat"], d["lon"], o["lat"], o["lon"]) <= COMM_RANGE_M:
                    nbrs += 1
            neighbor_counts.append(nbrs)
            max_neighbors = max(max_neighbors, nbrs)

            # Calculate trust
            dt_trust = dt  # Time since last update
            T = trust(dt_trust, d["q"], lambda_val, tau_val)
            trust_values.append(T)
            min_trust = min(min_trust, T)

            # Assign role
            if T >= 0.85 and nbrs >= 8:
                d["role"] = "LEADER"
            elif T >= 0.65 and nbrs >= 4:
                d["role"] = "RELAY"
            else:
                d["role"] = "WING"

            role_counts[d["role"]] += 1

        # Record time series
        time_series["t"].append(t)
        time_series["avg_neighbors"].append(sum(neighbor_counts) / len(neighbor_counts) if neighbor_counts else 0)
        time_series["avg_trust"].append(sum(trust_values) / len(trust_values) if trust_values else 0)
        time_series["leaders"].append(role_counts["LEADER"])
        time_series["relays"].append(role_counts["RELAY"])
        time_series["wings"].append(role_counts["WING"])

        total_neighbor_counts.extend(neighbor_counts)

        t += dt

    # Calculate final metrics
    avg_neighbors = sum(total_neighbor_counts) / len(total_neighbor_counts) if total_neighbor_counts else 0
    final_avg_trust = sum(trust_values) / len(trust_values) if trust_values else 0

    # Stability: swarm is stable if average trust > 0.7 and avg neighbors > 2
    stable = final_avg_trust > 0.7 and avg_neighbors > 2

    metrics = {
        "avg_neighbors": round(avg_neighbors, 2),
        "max_neighbors": max_neighbors,
        "final_avg_trust": round(final_avg_trust, 4),
        "min_trust": round(min_trust, 4),
        "final_leaders": role_counts["LEADER"],
        "final_relays": role_counts["RELAY"],
        "final_wings": role_counts["WING"],
        "stable": stable
    }

    return metrics, time_series


def main():
    """Run parameter sweep on swarm configuration"""

    # Define parameter grid
    grid = ParamGrid({
        "n_drones": [10, 25, 50, 100],
        "duration_s": [30.0],
        "dt": [0.5],
        "lambda_val": [0.05, 0.0893, 0.15],
        "tau_val": [0.05, 0.0997, 0.15]
    })

    # Run sweep
    output_dir = run_parameter_sweep(
        sim_name="primal_swarm",
        param_grid=grid,
        simulate_fn=simulate_swarm,
        tag="full_sweep"
    )

    print(f"\n✓ Sweep complete! Results in: {output_dir}")
    print(f"\n  View summary: {output_dir}/summary/summary.csv")
    print(f"  View stats:   {output_dir}/summary/stats.json")
    print(f"  View report:  {output_dir}/REPORT.md")


if __name__ == "__main__":
    main()
