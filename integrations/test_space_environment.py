"""
Test Suite for Space Environment Effects
=========================================
Comprehensive tests for Van Allen radiation belt and EMP weapon system.

Author: MotorHandPro Integration Team
License: MIT
"""

import sys
import numpy as np
from datetime import datetime, timedelta
import time

# Import modules to test
from space_environment_effects import (
    VanAllenRadiationBelt,
    EMPWeaponSystem,
    SpaceEnvironmentSimulator,
    SatelliteHealth,
    RadiationBeltRegion,
    EARTH_RADIUS,
    INNER_BELT_PEAK_ALT,
    OUTER_BELT_PEAK_ALT
)


def test_radiation_belt_classification():
    """Test radiation belt region classification"""
    print("\n" + "="*60)
    print("TEST 1: Radiation Belt Region Classification")
    print("="*60)

    belt = VanAllenRadiationBelt()

    test_cases = [
        (500, RadiationBeltRegion.LEO_SAFE, "Low Earth Orbit"),
        (3500, RadiationBeltRegion.INNER_BELT, "Inner Belt Peak"),
        (10000, RadiationBeltRegion.SLOT_REGION, "Slot Region"),
        (22000, RadiationBeltRegion.OUTER_BELT, "Outer Belt Peak"),
        (35786, RadiationBeltRegion.OUTER_BELT, "GEO (still in outer belt)"),
        (65000, RadiationBeltRegion.GEO, "Above outer belt")
    ]

    passed = 0
    for altitude, expected_region, description in test_cases:
        region = belt.classify_radiation_region(altitude)
        status = "✓ PASS" if region == expected_region else "✗ FAIL"
        print(f"{status} | {altitude:6.0f} km → {region.value:15s} ({description})")
        if region == expected_region:
            passed += 1

    print(f"\nResult: {passed}/{len(test_cases)} tests passed")
    return passed == len(test_cases)


def test_radiation_intensity_calculation():
    """Test radiation intensity calculations"""
    print("\n" + "="*60)
    print("TEST 2: Radiation Intensity Calculation")
    print("="*60)

    belt = VanAllenRadiationBelt()

    # Test at inner belt peak
    dose_rate, proton_flux, electron_flux = belt.calculate_radiation_intensity(
        INNER_BELT_PEAK_ALT, 0.0
    )

    print(f"\nInner Belt Peak ({INNER_BELT_PEAK_ALT} km, equator):")
    print(f"  Dose rate: {dose_rate:.4f} Sv/day")
    print(f"  Proton flux: {proton_flux:.2e} particles/cm²/s")
    print(f"  Electron flux: {electron_flux:.2e} particles/cm²/s")

    inner_belt_pass = dose_rate > 5.0  # Should be high radiation

    # Test at outer belt peak
    dose_rate, proton_flux, electron_flux = belt.calculate_radiation_intensity(
        OUTER_BELT_PEAK_ALT, 0.0
    )

    print(f"\nOuter Belt Peak ({OUTER_BELT_PEAK_ALT} km, equator):")
    print(f"  Dose rate: {dose_rate:.4f} Sv/day")
    print(f"  Proton flux: {proton_flux:.2e} particles/cm²/s")
    print(f"  Electron flux: {electron_flux:.2e} particles/cm²/s")

    outer_belt_pass = dose_rate > 2.0  # Should be moderate radiation

    # Test at slot region (low radiation)
    dose_rate, proton_flux, electron_flux = belt.calculate_radiation_intensity(
        10000, 0.0
    )

    print(f"\nSlot Region (10000 km, equator):")
    print(f"  Dose rate: {dose_rate:.4f} Sv/day")
    print(f"  Proton flux: {proton_flux:.2e} particles/cm²/s")
    print(f"  Electron flux: {electron_flux:.2e} particles/cm²/s")

    slot_pass = dose_rate < 0.5  # Should be low radiation

    # Test latitude effect
    dose_equator, _, _ = belt.calculate_radiation_intensity(INNER_BELT_PEAK_ALT, 0.0)
    dose_pole, _, _ = belt.calculate_radiation_intensity(INNER_BELT_PEAK_ALT, 90.0)

    print(f"\nLatitude Effect (Inner Belt Peak):")
    print(f"  Equator (0°): {dose_equator:.4f} Sv/day")
    print(f"  Pole (90°):   {dose_pole:.4f} Sv/day")
    print(f"  Ratio:        {dose_equator/dose_pole:.2f}x")

    latitude_pass = dose_equator > dose_pole  # Equator should have higher radiation

    passed = sum([inner_belt_pass, outer_belt_pass, slot_pass, latitude_pass])
    print(f"\nResult: {passed}/4 tests passed")

    return passed == 4


def test_magnetic_field_calculation():
    """Test magnetic field strength calculation"""
    print("\n" + "="*60)
    print("TEST 3: Magnetic Field Strength Calculation")
    print("="*60)

    belt = VanAllenRadiationBelt()

    test_altitudes = [0, 500, 3500, 22000, 35786]

    for altitude in test_altitudes:
        field_equator = belt.calculate_magnetic_field_strength(altitude, 0.0)
        field_pole = belt.calculate_magnetic_field_strength(altitude, 90.0)

        print(f"\nAltitude {altitude:6.0f} km:")
        print(f"  Equator: {field_equator:8.1f} nT")
        print(f"  Pole:    {field_pole:8.1f} nT")
        print(f"  Ratio:   {field_pole/field_equator:.2f}x")

    # Field should decrease with altitude
    field_leo = belt.calculate_magnetic_field_strength(500, 0.0)
    field_geo = belt.calculate_magnetic_field_strength(35786, 0.0)

    field_decrease_pass = field_leo > field_geo

    print(f"\n✓ Magnetic field decreases with altitude: {field_decrease_pass}")
    print(f"  LEO (500 km): {field_leo:.1f} nT")
    print(f"  GEO (35786 km): {field_geo:.1f} nT")

    return field_decrease_pass


def test_radiation_damage_accumulation():
    """Test cumulative radiation damage over time"""
    print("\n" + "="*60)
    print("TEST 4: Radiation Damage Accumulation")
    print("="*60)

    belt = VanAllenRadiationBelt()
    health = SatelliteHealth(satellite_id=1, radiation_shielding=0.5)

    print(f"\nInitial satellite health:")
    print(f"  Health: {health.health:.4f}")
    print(f"  Solar panel efficiency: {health.solar_panel_efficiency:.4f}")
    print(f"  Electronics health: {health.electronics_health:.4f}")
    print(f"  Cumulative dose: {health.cumulative_radiation_dose:.6f} Sv")

    # Simulate 30 days in inner radiation belt
    days = 30
    time_step_seconds = 86400  # 1 day

    print(f"\nSimulating {days} days at inner belt peak altitude...")

    for day in range(days):
        exposure = belt.apply_radiation_effects(
            health, INNER_BELT_PEAK_ALT, 0.0, time_step_seconds
        )

    print(f"\nAfter {days} days:")
    print(f"  Health: {health.health:.4f}")
    print(f"  Solar panel efficiency: {health.solar_panel_efficiency:.4f}")
    print(f"  Electronics health: {health.electronics_health:.4f}")
    print(f"  Cumulative dose: {health.cumulative_radiation_dose:.6f} Sv")

    # Satellite should have accumulated damage
    damage_pass = (
        health.health < 1.0 and
        health.solar_panel_efficiency < 1.0 and
        health.cumulative_radiation_dose > 0
    )

    print(f"\n✓ Radiation damage accumulated: {damage_pass}")

    return damage_pass


def test_emp_weapon_system():
    """Test EMP weapon effects"""
    print("\n" + "="*60)
    print("TEST 5: EMP Weapon System")
    print("="*60)

    emp_system = EMPWeaponSystem()

    # Create EMP event
    emp_position = np.array([0, 0, EARTH_RADIUS + 1000])
    emp_event = emp_system.create_emp_event(emp_position, "NUCLEAR", 0.9)

    print(f"\nCreated EMP event:")
    print(f"  ID: {emp_event.event_id}")
    print(f"  Position: {emp_event.position}")
    print(f"  Intensity: {emp_event.intensity:.2f}")
    print(f"  Range: {emp_event.range:.1f} km")
    print(f"  Source: {emp_event.source_type}")

    # Test satellite at various distances
    health = SatelliteHealth(satellite_id=1, emp_shielding=0.3)

    test_distances = [100, 250, 400, 600]

    print(f"\nEMP damage vs distance (satellite with 30% shielding):")

    for distance in test_distances:
        sat_position = emp_position + np.array([distance, 0, 0])
        damage = emp_system.calculate_emp_damage(sat_position, health, emp_event)
        print(f"  {distance:3.0f} km: {damage:.4f} damage")

    # Apply EMP effects
    sat_close = emp_position + np.array([100, 0, 0])  # Close to EMP
    initial_health = health.health

    emp_status = emp_system.apply_emp_effects(sat_close, health, datetime.utcnow())

    print(f"\nEMP effects on close satellite (100 km):")
    print(f"  Damage: {emp_status['total_damage']:.4f}")
    print(f"  EMP affected: {emp_status['emp_affected']}")
    print(f"  Health before: {initial_health:.4f}")
    print(f"  Health after: {health.health:.4f}")
    print(f"  Electronics health: {health.electronics_health:.4f}")

    emp_pass = (
        emp_status['total_damage'] > 0 and
        health.emp_affected and
        health.health < initial_health
    )

    print(f"\n✓ EMP damage applied correctly: {emp_pass}")

    return emp_pass


def test_emp_recovery():
    """Test EMP recovery mechanics"""
    print("\n" + "="*60)
    print("TEST 6: EMP Recovery")
    print("="*60)

    emp_system = EMPWeaponSystem()
    health = SatelliteHealth(satellite_id=1, emp_shielding=0.5)

    # Create EMP and damage satellite
    emp_position = np.array([0, 0, EARTH_RADIUS + 1000])
    emp_event = emp_system.create_emp_event(emp_position, "NUCLEAR", 0.8)

    sat_position = emp_position + np.array([50, 0, 0])  # Very close

    current_time = datetime.utcnow()
    emp_status = emp_system.apply_emp_effects(sat_position, health, current_time)

    print(f"EMP damage applied:")
    print(f"  EMP affected: {health.emp_affected}")
    print(f"  Recovery time: {health.emp_recovery_time}")

    # Try recovery before time elapses
    emp_system.check_emp_recovery(health, current_time)
    still_affected = health.emp_affected

    # Simulate time passing
    future_time = current_time + timedelta(seconds=400)
    emp_system.check_emp_recovery(health, future_time)
    recovered = not health.emp_affected

    print(f"\nRecovery check:")
    print(f"  Still affected immediately: {still_affected}")
    print(f"  Recovered after 400s: {recovered}")

    recovery_pass = still_affected and recovered

    print(f"\n✓ EMP recovery works correctly: {recovery_pass}")

    return recovery_pass


def test_integrated_simulation():
    """Test integrated space environment simulator"""
    print("\n" + "="*60)
    print("TEST 7: Integrated Space Environment Simulation")
    print("="*60)

    sim = SpaceEnvironmentSimulator()

    # Initialize multiple satellites at different altitudes
    satellite_data = [
        {'id': 1, 'alt': 550, 'lat': 0.0, 'region': 'LEO'},
        {'id': 2, 'alt': 3500, 'lat': 0.0, 'region': 'Inner Belt'},
        {'id': 3, 'alt': 10000, 'lat': 0.0, 'region': 'Slot'},
        {'id': 4, 'alt': 22000, 'lat': 0.0, 'region': 'Outer Belt'},
        {'id': 5, 'alt': 35786, 'lat': 0.0, 'region': 'GEO'}
    ]

    print("\nInitializing satellites...")
    for sat in satellite_data:
        sim.initialize_satellite(
            sat['id'],
            radiation_shielding=0.6,
            emp_shielding=0.5,
            radiation_hardening=0.7
        )
        print(f"  ✓ Satellite {sat['id']} at {sat['alt']} km ({sat['region']})")

    # Update all satellites
    print("\nUpdating satellite states...")
    updates = []
    for sat in satellite_data:
        position = np.array([0, 0, EARTH_RADIUS + sat['alt']])
        result = sim.update_satellite(
            satellite_id=sat['id'],
            position_eci=position,
            latitude=sat['lat'],
            altitude=sat['alt']
        )
        updates.append(result)

        print(f"\nSatellite {sat['id']} ({sat['region']}):")
        print(f"  Radiation region: {result['radiation']['region']}")
        print(f"  Dose rate: {result['radiation']['dose_rate']:.6f} Sv/day")
        print(f"  Health: {result['health']['health']:.4f}")

    # Create EMP event
    print("\n\nCreating EMP event...")
    emp_pos = np.array([0, 0, EARTH_RADIUS + 550])
    sim.emp_system.create_emp_event(emp_pos, "NUCLEAR", 0.85)
    print(f"  ✓ EMP created at LEO altitude")

    # Update satellites again with EMP
    print("\nUpdating satellites with EMP effects...")
    for sat in satellite_data[:2]:  # Only LEO and inner belt affected
        position = np.array([0, 0, EARTH_RADIUS + sat['alt']])
        result = sim.update_satellite(sat['id'], position, sat['lat'], sat['alt'])

        emp_damage = result['emp']['total_damage']
        print(f"  Satellite {sat['id']}: EMP damage = {emp_damage:.4f}")

    # Get statistics
    print("\n\nSystem Statistics:")
    stats = sim.get_statistics()
    for key, value in stats.items():
        print(f"  {key}: {value}")

    simulation_pass = (
        len(sim.satellite_health_db) == 5 and
        stats['total_satellites'] == 5 and
        stats['active_emp_events'] >= 1
    )

    print(f"\n✓ Integrated simulation works: {simulation_pass}")

    return simulation_pass


def test_performance():
    """Test performance with many satellites"""
    print("\n" + "="*60)
    print("TEST 8: Performance Test")
    print("="*60)

    sim = SpaceEnvironmentSimulator()

    num_satellites = 1000
    print(f"\nInitializing {num_satellites} satellites...")

    start_time = time.time()

    # Batch initialization
    satellites_data = []
    for i in range(num_satellites):
        altitude = np.random.uniform(500, 35000)
        latitude = np.random.uniform(-90, 90)
        position = np.array([
            np.random.uniform(-10000, 10000),
            np.random.uniform(-10000, 10000),
            EARTH_RADIUS + altitude
        ])

        sim.initialize_satellite(i)

        satellites_data.append({
            'satellite_id': i,
            'position_eci': position,
            'latitude': latitude,
            'altitude': altitude
        })

    init_time = time.time() - start_time

    # Batch update
    print(f"Updating {num_satellites} satellites...")
    start_time = time.time()

    results = sim.update_all_satellites(satellites_data)

    update_time = time.time() - start_time

    print(f"\nPerformance Results:")
    print(f"  Initialization: {init_time:.3f}s ({num_satellites/init_time:.1f} sats/sec)")
    print(f"  Update: {update_time:.3f}s ({num_satellites/update_time:.1f} sats/sec)")
    print(f"  Total satellites: {len(results)}")

    performance_pass = (
        len(results) == num_satellites and
        update_time < 5.0  # Should process 1000 satellites in under 5 seconds
    )

    print(f"\n✓ Performance acceptable: {performance_pass}")

    return performance_pass


def run_all_tests():
    """Run all tests and generate report"""
    print("\n" + "="*60)
    print("SPACE ENVIRONMENT EFFECTS - COMPREHENSIVE TEST SUITE")
    print("="*60)
    print(f"Date: {datetime.utcnow().strftime('%Y-%m-%d %H:%M:%S UTC')}")

    tests = [
        ("Radiation Belt Classification", test_radiation_belt_classification),
        ("Radiation Intensity Calculation", test_radiation_intensity_calculation),
        ("Magnetic Field Calculation", test_magnetic_field_calculation),
        ("Radiation Damage Accumulation", test_radiation_damage_accumulation),
        ("EMP Weapon System", test_emp_weapon_system),
        ("EMP Recovery", test_emp_recovery),
        ("Integrated Simulation", test_integrated_simulation),
        ("Performance Test", test_performance)
    ]

    results = []
    for test_name, test_func in tests:
        try:
            passed = test_func()
            results.append((test_name, passed))
        except Exception as e:
            print(f"\n✗ ERROR in {test_name}: {e}")
            import traceback
            traceback.print_exc()
            results.append((test_name, False))

    # Summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)

    passed_count = sum(1 for _, passed in results if passed)
    total_count = len(results)

    for test_name, passed in results:
        status = "✓ PASS" if passed else "✗ FAIL"
        print(f"{status} | {test_name}")

    print(f"\n{'='*60}")
    print(f"OVERALL: {passed_count}/{total_count} tests passed")

    if passed_count == total_count:
        print("🎉 ALL TESTS PASSED!")
    else:
        print(f"⚠️  {total_count - passed_count} test(s) failed")

    print("="*60)

    return passed_count == total_count


if __name__ == "__main__":
    success = run_all_tests()
    sys.exit(0 if success else 1)
