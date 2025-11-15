#!/usr/bin/env python3
"""
Quick start example for NASA and SpaceX APIs.

A simple demonstration to get you started quickly.
"""

import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api_integrations import APIManager


def main():
    # Initialize the API manager
    manager = APIManager()

    print("="*60)
    print("NASA & SpaceX API - Quick Start")
    print("="*60)

    # 1. NASA APOD - Today's astronomy picture
    print("\n1. Today's Astronomy Picture:")
    apod = manager.nasa_apod.get_today()
    print(f"   Title: {apod['title']}")
    print(f"   URL: {apod['url']}")

    # 2. Near Earth Objects
    print("\n2. Near Earth Object Statistics:")
    stats = manager.nasa_neows.get_statistics()
    print(f"   Total NEOs: {stats['near_earth_object_count']}")

    # 3. SpaceX Latest Launch
    print("\n3. Latest SpaceX Launch:")
    launch = manager.spacex.get_latest_launch()
    print(f"   Name: {launch['name']}")
    print(f"   Date: {launch.get('date_utc')}")
    print(f"   Success: {launch.get('success')}")

    # 4. SpaceX Company Info
    print("\n4. SpaceX Company Info:")
    company = manager.spacex.get_company_info()
    print(f"   Founded: {company['founded']}")
    print(f"   Employees: {company['employees']}")
    print(f"   Valuation: ${company['valuation']:,}")

    # 5. Starlink Metrics
    print("\n5. Starlink Metrics:")
    try:
        metrics = manager.starlink.get_residential_metrics()
        print(f"   Data retrieved successfully!")
    except Exception as e:
        print(f"   Error: {e}")

    print("\n" + "="*60)
    print("Quick Start Complete!")
    print("="*60)
    print("\nNext steps:")
    print("  - See examples/comprehensive_demo.py for more features")
    print("  - Check api_integrations/README.md for full documentation")
    print("  - Get your NASA API key at https://api.nasa.gov")


if __name__ == "__main__":
    main()
