#!/usr/bin/env python3
"""
Comprehensive demonstration of all NASA and SpaceX API integrations.

This example shows how to use all available API clients.
"""

import sys
import os
from datetime import date, timedelta

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from api_integrations import APIManager
from api_integrations.base_client import APIError


def demo_nasa_apod(manager):
    """Demonstrate NASA APOD API."""
    print("\n" + "="*60)
    print("NASA APOD (Astronomy Picture of the Day)")
    print("="*60)

    try:
        # Get today's picture
        apod = manager.nasa_apod.get_today()
        print(f"\nToday's APOD:")
        print(f"  Title: {apod['title']}")
        print(f"  Date: {apod['date']}")
        print(f"  Type: {apod['media_type']}")
        print(f"  URL: {apod['url']}")
        print(f"  Explanation: {apod['explanation'][:200]}...")

        # Get random picture
        random_pics = manager.nasa_apod.get_random(count=1)
        print(f"\nRandom APOD:")
        print(f"  Title: {random_pics[0]['title']}")
        print(f"  Date: {random_pics[0]['date']}")

    except APIError as e:
        print(f"Error: {e}")


def demo_nasa_neows(manager):
    """Demonstrate NASA NeoWs API."""
    print("\n" + "="*60)
    print("NASA NeoWs (Near Earth Objects)")
    print("="*60)

    try:
        # Get statistics
        stats = manager.nasa_neows.get_statistics()
        print(f"\nNEO Statistics:")
        print(f"  Total NEOs: {stats['near_earth_object_count']}")
        print(f"  Close Approaches: {stats['close_approach_count']}")

        # Get feed
        feed = manager.nasa_neows.get_feed()
        print(f"\nCurrent Feed:")
        print(f"  Total Elements: {feed['element_count']}")

        # Display first NEO
        first_date = list(feed['near_earth_objects'].keys())[0]
        first_neo = feed['near_earth_objects'][first_date][0]
        print(f"\n  Example NEO:")
        print(f"    Name: {first_neo['name']}")
        print(f"    Hazardous: {first_neo['is_potentially_hazardous_asteroid']}")
        print(f"    Diameter (m): {first_neo['estimated_diameter']['meters']['estimated_diameter_max']:.2f}")

    except APIError as e:
        print(f"Error: {e}")


def demo_nasa_epic(manager):
    """Demonstrate NASA EPIC API."""
    print("\n" + "="*60)
    print("NASA EPIC (Earth Images)")
    print("="*60)

    try:
        # Get natural color images
        images = manager.nasa_epic.get_natural_images()
        print(f"\nNatural Color Images Available: {len(images)}")

        if images:
            first_image = images[0]
            print(f"\n  Latest Image:")
            print(f"    Date: {first_image['date']}")
            print(f"    Image: {first_image['image']}")

            # Build download URL
            url = manager.nasa_epic.build_image_url(first_image)
            print(f"    URL: {url}")

    except APIError as e:
        print(f"Error: {e}")


def demo_nasa_power(manager):
    """Demonstrate NASA POWER API."""
    print("\n" + "="*60)
    print("NASA POWER (Climate Data)")
    print("="*60)

    try:
        # Get point data for a location (e.g., St. Louis, MO)
        latitude = 38.6270
        longitude = -90.1994
        parameters = ["T2M", "PRECTOTCORR"]

        # Get recent data
        end_date = date.today()
        start_date = end_date - timedelta(days=7)

        print(f"\nFetching climate data for:")
        print(f"  Location: ({latitude}, {longitude})")
        print(f"  Date Range: {start_date} to {end_date}")
        print(f"  Parameters: {parameters}")

        data = manager.nasa_power.get_point_data(
            latitude=latitude,
            longitude=longitude,
            parameters=parameters,
            start_date=start_date.strftime("%Y%m%d"),
            end_date=end_date.strftime("%Y%m%d")
        )

        print(f"\n  Data retrieved successfully!")
        print(f"  Parameters: {list(data.get('parameters', {}).keys())}")

    except APIError as e:
        print(f"Error: {e}")


def demo_nasa_images(manager):
    """Demonstrate NASA Image Library API."""
    print("\n" + "="*60)
    print("NASA Image and Video Library")
    print("="*60)

    try:
        # Search for Mars images
        results = manager.nasa_images.search(
            query="Mars",
            media_type="image",
            year_start=2020,
            page_size=5
        )

        collection = results.get('collection', {})
        items = collection.get('items', [])

        print(f"\nSearch Results for 'Mars':")
        print(f"  Found: {len(items)} items (showing first 5)")

        for i, item in enumerate(items[:3], 1):
            data = item.get('data', [{}])[0]
            print(f"\n  {i}. {data.get('title', 'N/A')}")
            print(f"     Date: {data.get('date_created', 'N/A')}")
            print(f"     NASA ID: {data.get('nasa_id', 'N/A')}")

    except APIError as e:
        print(f"Error: {e}")


def demo_nasa_ssd(manager):
    """Demonstrate NASA SSD API."""
    print("\n" + "="*60)
    print("NASA Solar System Dynamics")
    print("="*60)

    try:
        # Get close approach data
        cad = manager.nasa_ssd.get_close_approach_data(
            dist_max=0.05,  # Within 0.05 AU
            body="Earth"
        )

        print(f"\nClose Approach Data:")
        print(f"  Total approaches: {cad.get('count', 0)}")

        if cad.get('data'):
            print(f"\n  Recent/Upcoming approaches:")
            for approach in cad['data'][:3]:
                print(f"    Object: {approach[0]}")
                print(f"    Date: {approach[3]}")
                print(f"    Distance: {approach[4]} AU")

        # Get fireball data
        fireballs = manager.nasa_ssd.get_fireball_data()
        print(f"\n  Fireball events: {fireballs.get('count', 0)}")

    except APIError as e:
        print(f"Error: {e}")


def demo_spacex(manager):
    """Demonstrate SpaceX API."""
    print("\n" + "="*60)
    print("SpaceX API")
    print("="*60)

    try:
        # Company info
        company = manager.spacex.get_company_info()
        print(f"\nSpaceX Company Info:")
        print(f"  Founded: {company.get('founded')}")
        print(f"  Employees: {company.get('employees')}")
        print(f"  Valuation: ${company.get('valuation'):,}")
        print(f"  Launch Sites: {company.get('launch_sites')}")

        # Latest launch
        latest = manager.spacex.get_latest_launch()
        print(f"\nLatest Launch:")
        print(f"  Name: {latest['name']}")
        print(f"  Date: {latest.get('date_utc')}")
        print(f"  Success: {latest.get('success')}")
        print(f"  Details: {latest.get('details', 'N/A')[:100]}...")

        # Next launch
        try:
            next_launch = manager.spacex.get_next_launch()
            print(f"\nNext Launch:")
            print(f"  Name: {next_launch['name']}")
            print(f"  Date: {next_launch.get('date_utc')}")
        except:
            print(f"\nNo upcoming launches scheduled")

        # Starlink satellites
        starlink = manager.spacex.get_starlink_satellites(
            options={"limit": 5}
        )
        print(f"\nStarlink Satellites: {len(starlink)} (showing 5)")

    except APIError as e:
        print(f"Error: {e}")


def demo_starlink(manager):
    """Demonstrate Starlink Metrics API."""
    print("\n" + "="*60)
    print("Starlink Public Metrics")
    print("="*60)

    try:
        # Get residential metrics
        metrics = manager.starlink.get_residential_metrics()
        print(f"\nResidential Metrics:")
        print(f"  Data retrieved: {type(metrics)}")

        # Get all metrics
        all_metrics = manager.starlink.get_all_metrics()
        print(f"\nAll Metrics:")
        for service, data in all_metrics.items():
            if 'error' in data:
                print(f"  {service}: {data['error']}")
            else:
                print(f"  {service}: OK")

    except APIError as e:
        print(f"Error: {e}")


def main():
    """Run all demonstrations."""
    print("\n" + "="*60)
    print("NASA & SpaceX API Integration - Comprehensive Demo")
    print("="*60)

    # Initialize manager
    manager = APIManager()

    # Run all demos
    demo_nasa_apod(manager)
    demo_nasa_neows(manager)
    demo_nasa_epic(manager)
    demo_nasa_power(manager)
    demo_nasa_images(manager)
    demo_nasa_ssd(manager)
    demo_spacex(manager)
    demo_starlink(manager)

    # Health check
    print("\n" + "="*60)
    print("API Health Check")
    print("="*60)
    status = manager.health_check()
    for api, state in status.items():
        print(f"  {api}: {state}")

    print("\n" + "="*60)
    print("Demo Complete!")
    print("="*60)


if __name__ == "__main__":
    main()
