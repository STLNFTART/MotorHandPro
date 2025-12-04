#!/usr/bin/env python3
"""
NASA Pipeline Monitor - Real-time status viewer
Usage: python3 monitor_pipeline.py
"""

import os
import json
import subprocess
from pathlib import Path
from datetime import datetime

def check_pipeline_status():
    """Check if pipeline is running"""
    try:
        result = subprocess.run(
            ['pgrep', '-f', 'live_nasa_pipeline.py'],
            capture_output=True,
            text=True
        )
        return result.returncode == 0
    except:
        return False

def get_output_stats():
    """Get statistics on output files"""
    output_dir = Path('nasa_live_output')

    if not output_dir.exists():
        return None

    obs_files = list(output_dir.glob('observations_*.json'))
    state_files = list(output_dir.glob('states_*.json'))
    viz_dir = output_dir / 'visualizations'
    viz_files = list(viz_dir.glob('*.html')) if viz_dir.exists() else []

    # Get total size
    total_size = sum(f.stat().st_size for f in output_dir.rglob('*') if f.is_file())
    total_size_mb = total_size / (1024 * 1024)

    return {
        'num_obs': len(obs_files),
        'num_states': len(state_files),
        'num_viz': len(viz_files),
        'total_size_mb': total_size_mb,
        'latest_obs': max(obs_files, key=lambda f: f.stat().st_mtime) if obs_files else None
    }

def get_latest_comet_data(obs_file):
    """Get latest comet observation data"""
    try:
        with open(obs_file, 'r') as f:
            observations = json.load(f)
            if observations:
                return observations[-1]
    except:
        pass
    return None

def main():
    print("=" * 80)
    print("🌌 NASA PIPELINE MONITOR")
    print("=" * 80)
    print()

    # Check if running
    is_running = check_pipeline_status()

    if is_running:
        print("✅ Pipeline Status: RUNNING")
    else:
        print("⚠️  Pipeline Status: NOT RUNNING")

    print()

    # Get output statistics
    stats = get_output_stats()

    if stats:
        print("📊 Output Files:")
        print(f"   Observations: {stats['num_obs']}")
        print(f"   States: {stats['num_states']}")
        print(f"   Visualizations: {stats['num_viz']}")
        print(f"   Total Size: {stats['total_size_mb']:.1f} MB")
        print()

        # Get latest comet data
        if stats['latest_obs']:
            latest_data = get_latest_comet_data(stats['latest_obs'])
            if latest_data:
                print("🌌 Latest Comet Data:")
                print(f"   Time: {latest_data['timestamp']}")
                print(f"   RA: {latest_data['ra']:.4f}° | Dec: {latest_data['dec']:.4f}°")
                print(f"   Distance: {latest_data['distance_au']:.4f} AU")
                if 'gas_production_rate' in latest_data:
                    print(f"   Ni Flux: {latest_data['gas_production_rate']:.2f} g/s")
                print()
    else:
        print("📊 No output files found yet")
        print()

    # Show visualizations
    if stats and stats['num_viz'] > 0:
        print("🎨 Available Visualizations:")
        viz_dir = Path('nasa_live_output/visualizations')
        for viz_file in sorted(viz_dir.glob('*.html')):
            size_mb = viz_file.stat().st_size / (1024 * 1024)
            print(f"   - {viz_file.name} ({size_mb:.1f} MB)")
        print()

    # Show how to view
    print("=" * 80)
    print("🚀 QUICK ACTIONS")
    print("=" * 80)
    print()

    if is_running:
        print("To stop pipeline:")
        print("  pkill -f live_nasa_pipeline.py")
        print()
    else:
        print("To start pipeline:")
        print("  python3 live_nasa_pipeline.py --mode continuous --interval 3600")
        print()

    if stats and stats['num_viz'] > 0:
        print("To view visualizations:")
        print("  cd nasa_live_output/visualizations")
        print("  python3 -m http.server 8000")
        print("  # Then visit: http://localhost:8000")

    print()
    print("=" * 80)
    print(f"Monitor updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("=" * 80)

if __name__ == "__main__":
    main()
