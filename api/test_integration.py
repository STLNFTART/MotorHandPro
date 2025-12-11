#!/usr/bin/env python3
"""
MongoDB Integration Test Script
Tests database connection and demonstrates API usage

Author: Donte Lightfoot
Date: December 11, 2025
"""

import os
import sys
from pathlib import Path
from datetime import datetime
from dotenv import load_dotenv

# Load environment variables
load_dotenv(Path(__file__).parent.parent / ".env")

# Add config path
sys.path.insert(0, str(Path(__file__).parent.parent / "config"))

try:
    from mongodb import MotorHandProDB
except ImportError as e:
    print(f"❌ Error importing MongoDB module: {e}")
    sys.exit(1)


def test_connection():
    """Test MongoDB connection"""
    print("=" * 80)
    print("🧪 Testing MongoDB Connection")
    print("=" * 80)
    print()

    try:
        db = MotorHandProDB()

        if not db.verify_connection():
            print("❌ Connection failed")
            return False

        print()
        return db
    except Exception as e:
        print(f"❌ Error: {e}")
        return False


def test_lam_actions(db):
    """Test LAM actions"""
    print("=" * 80)
    print("🧪 Testing LAM Actions")
    print("=" * 80)
    print()

    # Save test action
    print("📝 Saving test LAM action...")
    doc_id = db.save_lam_action(
        action_type="plan_trip",
        user_id="test_user_123",
        primal_state={
            'n': 0.5,
            'signal': 4.5,
            'memory_integral': 10.2,
            'error': 0.05,
            'anomaly_score': 0.01
        },
        lam_integration={
            'enabled': True,
            'E_displaced': 4.52
        },
        metadata={
            'test': True,
            'timestamp': datetime.utcnow().isoformat()
        }
    )
    print(f"✅ Saved with ID: {doc_id}")
    print()

    # Retrieve actions
    print("📖 Retrieving LAM actions...")
    actions = db.get_lam_actions(user_id="test_user_123", limit=5)
    print(f"✅ Retrieved {len(actions)} actions")

    if actions:
        latest = actions[0]
        print(f"\n   Latest action:")
        print(f"     Type: {latest['action_type']}")
        print(f"     User: {latest['user_id']}")
        print(f"     Signal: {latest['primal_state']['signal']}")
    print()

    # Get statistics
    print("📊 Getting LAM action statistics...")
    stats = db.get_lam_action_stats()
    print(f"✅ Statistics:")
    print(f"   Total actions: {stats['total_actions']}")
    print(f"   Action types: {stats['action_types']}")
    print()


def test_nasa_observations(db):
    """Test NASA observations"""
    print("=" * 80)
    print("🧪 Testing NASA Observations")
    print("=" * 80)
    print()

    # Save test observation
    print("📝 Saving test NASA observation...")
    doc_id = db.save_nasa_observation(
        comet_id="3I/ATLAS",
        observation={
            'ra': 188.7396,
            'dec': -56.0809,
            'distance_au': 1.8240,
            'magnitude': 18.5,
            'gas_production_rate': 4.59
        },
        primal_state={
            'n': 0.04684,
            'signal': 4.6844,
            'memory_integral': 0.0,
            'error': 0.0844,
            'anomaly_score': 0.0084
        },
        lam_integration={
            'enabled': True,
            'E_displaced': 4.6844
        }
    )
    print(f"✅ Saved with ID: {doc_id}")
    print()

    # Retrieve observations
    print("📖 Retrieving NASA observations...")
    observations = db.get_nasa_observations(comet_id="3I/ATLAS", limit=5)
    print(f"✅ Retrieved {len(observations)} observations")

    if observations:
        latest = observations[0]
        print(f"\n   Latest observation:")
        print(f"     Comet: {latest['comet_id']}")
        print(f"     RA: {latest['observation']['ra']:.4f}°")
        print(f"     Dec: {latest['observation']['dec']:.4f}°")
        print(f"     Distance: {latest['observation']['distance_au']:.4f} AU")
    print()

    # Get statistics
    print("📊 Getting NASA observation statistics...")
    stats = db.get_nasa_observation_stats()
    print(f"✅ Statistics:")
    print(f"   Total observations: {stats['total_observations']}")
    print(f"   Comets: {stats['comets']}")
    print()


def test_test_results(db):
    """Test test results"""
    print("=" * 80)
    print("🧪 Testing Test Results")
    print("=" * 80)
    print()

    # Save test result
    print("📝 Saving test result...")
    doc_id = db.save_test_result(
        branch="claude/fix-skip-content-link-014TkokNNffPp3Sf69qctXHM",
        test_type="LAM Core",
        success=True,
        results={
            'tests_passed': 15,
            'tests_failed': 0,
            'duration': 0.015
        },
        primal_constants={
            'lambda': 0.16905,
            'D': 149.9992314,
            'lipschitz': 0.000129932
        }
    )
    print(f"✅ Saved with ID: {doc_id}")
    print()

    # Retrieve test results
    print("📖 Retrieving test results...")
    results = db.get_test_results(success=True, limit=5)
    print(f"✅ Retrieved {len(results)} results")

    if results:
        latest = results[0]
        print(f"\n   Latest result:")
        print(f"     Branch: {latest['branch']}")
        print(f"     Type: {latest['test_type']}")
        print(f"     Success: {latest['success']}")
        print(f"     Passed: {latest['results'].get('tests_passed', 'N/A')}")
    print()

    # Get statistics
    print("📊 Getting test result statistics...")
    stats = db.get_test_result_stats()
    print(f"✅ Statistics:")
    print(f"   Total tests: {stats['total_tests']}")
    print(f"   Passed: {stats['passed_tests']}")
    print(f"   Failed: {stats['failed_tests']}")
    print(f"   Pass rate: {stats['pass_rate']:.1f}%")
    print()


def test_satellites(db):
    """Test satellite data"""
    print("=" * 80)
    print("🧪 Testing Satellite Data")
    print("=" * 80)
    print()

    # Save satellite data
    print("📝 Saving satellite data...")
    doc_id = db.save_satellite_data(
        satellite_id="SAT-001",
        position={'x': 7000.0, 'y': 0.0, 'z': 0.0},
        velocity={'vx': 0.0, 'vy': 7.5, 'vz': 0.0},
        metadata={'constellation': 'test', 'active': True}
    )
    print(f"✅ Saved with ID: {doc_id}")
    print()

    # Retrieve satellite data
    print("📖 Retrieving satellite data...")
    data = db.get_satellite_data(satellite_id="SAT-001", limit=5)
    print(f"✅ Retrieved {len(data)} data points")

    if data:
        latest = data[0]
        print(f"\n   Latest data:")
        print(f"     Satellite: {latest['satellite_id']}")
        print(f"     Position: ({latest['position']['x']}, {latest['position']['y']}, {latest['position']['z']})")
        print(f"     Velocity: ({latest['velocity']['vx']}, {latest['velocity']['vy']}, {latest['velocity']['vz']})")
    print()


def main():
    """Main test runner"""
    print("\n")
    print("🌌 MotorHandPro MongoDB Integration Test")
    print()

    # Check if .env file exists
    env_file = Path(__file__).parent.parent / ".env"
    if not env_file.exists():
        print("⚠️  Warning: .env file not found")
        print("   Please create .env from .env.example and configure MongoDB credentials")
        print()

        # Check if MONGODB_URI is set in environment
        if not os.getenv('MONGODB_URI'):
            print("❌ MONGODB_URI not set in environment variables")
            print("   Please set MONGODB_URI or create .env file")
            return

    # Test connection
    db = test_connection()
    if not db:
        return

    try:
        # Run all tests
        test_lam_actions(db)
        test_nasa_observations(db)
        test_test_results(db)
        test_satellites(db)

        # Overall statistics
        print("=" * 80)
        print("📊 Overall Database Statistics")
        print("=" * 80)
        print()

        stats = db.get_database_stats()
        print(f"LAM Actions:        {stats['lam_actions']['count']:>6}")
        print(f"NASA Observations:  {stats['nasa_observations']['count']:>6}")
        print(f"Test Results:       {stats['test_results']['count']:>6}")
        print(f"Satellites:         {stats['satellites']['count']:>6}")
        print()

        print("=" * 80)
        print("✅ All Tests Completed Successfully!")
        print("=" * 80)
        print()

    finally:
        # Close connection
        db.close()


if __name__ == "__main__":
    main()
