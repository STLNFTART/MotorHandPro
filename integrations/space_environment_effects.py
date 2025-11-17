"""
Space Environment Effects Module
==================================
Implements Van Allen radiation belt physics, EMP effects, and space environment
damage modeling for satellite systems.

Features:
- Van Allen inner/outer radiation belt modeling
- Electromagnetic pulse (EMP) weapon effects
- Cumulative radiation dose tracking
- Solar particle event simulation
- Magnetic field interaction
- Electronics degradation modeling

Author: MotorHandPro Integration Team
License: MIT
"""

import numpy as np
from datetime import datetime, timedelta
from typing import Dict, List, Tuple, Optional, Any
from dataclasses import dataclass, field
from enum import Enum
import random
import math


# === CONSTANTS ===
EARTH_RADIUS = 6378.137  # km (WGS84 equatorial radius)
MAGNETIC_FIELD_STRENGTH_SURFACE = 31000.0  # nT (nanotesla)

# Van Allen Belt boundaries (km above Earth surface)
INNER_BELT_MIN_ALT = 1000.0
INNER_BELT_MAX_ALT = 6000.0
INNER_BELT_PEAK_ALT = 3500.0  # Maximum radiation

OUTER_BELT_MIN_ALT = 13000.0
OUTER_BELT_MAX_ALT = 60000.0
OUTER_BELT_PEAK_ALT = 22000.0  # Maximum radiation

SLOT_REGION_MIN_ALT = 6000.0
SLOT_REGION_MAX_ALT = 13000.0

# Radiation intensity multipliers
INNER_BELT_MAX_INTENSITY = 10.0  # Sv/day (Sieverts per day - protons)
OUTER_BELT_MAX_INTENSITY = 5.0   # Sv/day (electrons)
SLOT_REGION_INTENSITY = 0.1      # Sv/day (minimal)

# EMP Parameters
EMP_WEAPON_RANGE = 500.0  # km effective range
EMP_PULSE_DURATION = 30  # seconds
EMP_RECOVERY_TIME_BASE = 300  # seconds


class RadiationBeltRegion(Enum):
    """Radiation environment classification"""
    LEO_SAFE = "LEO_SAFE"  # Below inner belt
    INNER_BELT = "INNER_BELT"  # Proton radiation
    SLOT_REGION = "SLOT_REGION"  # Low radiation between belts
    OUTER_BELT = "OUTER_BELT"  # Electron radiation
    GEO = "GEO"  # Geostationary orbit altitude


@dataclass
class RadiationExposure:
    """Radiation exposure data for a satellite"""
    satellite_id: int
    timestamp: datetime
    altitude: float  # km
    latitude: float  # degrees
    region: RadiationBeltRegion
    dose_rate: float  # Sv/day (Sieverts per day)
    cumulative_dose: float  # Sv (total accumulated)
    magnetic_field_strength: float  # nT
    proton_flux: float  # particles/cm²/s
    electron_flux: float  # particles/cm²/s

    def to_dict(self) -> Dict:
        """Convert to JSON-serializable dictionary"""
        return {
            'satellite_id': self.satellite_id,
            'timestamp': self.timestamp.isoformat(),
            'altitude': self.altitude,
            'latitude': self.latitude,
            'region': self.region.value,
            'dose_rate': self.dose_rate,
            'cumulative_dose': self.cumulative_dose,
            'magnetic_field_strength': self.magnetic_field_strength,
            'proton_flux': self.proton_flux,
            'electron_flux': self.electron_flux
        }


@dataclass
class EMPEvent:
    """Electromagnetic pulse event"""
    event_id: str
    position: np.ndarray  # [x, y, z] in km (ECI frame)
    start_time: datetime
    duration: float  # seconds
    intensity: float  # 0.0 to 1.0
    range: float  # km
    source_type: str  # NUCLEAR, HERF, SOLAR_FLARE, CYBER
    active: bool = True

    def is_expired(self, current_time: datetime) -> bool:
        """Check if EMP event has expired"""
        elapsed = (current_time - self.start_time).total_seconds()
        return elapsed > self.duration


@dataclass
class SatelliteHealth:
    """Satellite health and degradation tracking"""
    satellite_id: int
    health: float = 1.0  # 0.0 to 1.0
    radiation_shielding: float = 0.5  # 0.0 to 1.0
    radiation_hardening: float = 0.5  # 0.0 to 1.0
    emp_shielding: float = 0.5  # 0.0 to 1.0
    cumulative_radiation_dose: float = 0.0  # Sv
    emp_affected: bool = False
    emp_recovery_time: Optional[datetime] = None
    solar_panel_efficiency: float = 1.0  # Degrades with radiation
    electronics_health: float = 1.0  # Degrades with radiation/EMP

    def to_dict(self) -> Dict:
        """Convert to JSON-serializable dictionary"""
        return {
            'satellite_id': self.satellite_id,
            'health': round(self.health, 4),
            'radiation_shielding': round(self.radiation_shielding, 4),
            'cumulative_radiation_dose': round(self.cumulative_radiation_dose, 6),
            'emp_affected': self.emp_affected,
            'solar_panel_efficiency': round(self.solar_panel_efficiency, 4),
            'electronics_health': round(self.electronics_health, 4)
        }


class VanAllenRadiationBelt:
    """
    Van Allen radiation belt physics simulation.

    Models the Earth's radiation belts with realistic particle flux
    and magnetic field interactions.
    """

    def __init__(self):
        self.radiation_events = []
        self.solar_particle_events = []  # SPEs
        self.geomagnetic_index_kp = 3.0  # 0-9 scale (3 = quiet conditions)

    def classify_radiation_region(self, altitude: float) -> RadiationBeltRegion:
        """Classify which radiation region a satellite is in"""
        if altitude < INNER_BELT_MIN_ALT:
            return RadiationBeltRegion.LEO_SAFE
        elif altitude <= INNER_BELT_MAX_ALT:
            return RadiationBeltRegion.INNER_BELT
        elif altitude <= SLOT_REGION_MAX_ALT:
            return RadiationBeltRegion.SLOT_REGION
        elif altitude <= OUTER_BELT_MAX_ALT:
            return RadiationBeltRegion.OUTER_BELT
        else:
            return RadiationBeltRegion.GEO

    def calculate_radiation_intensity(self, altitude: float, latitude: float) -> Tuple[float, float, float]:
        """
        Calculate radiation intensity at given position.

        Returns:
            (dose_rate_sv_per_day, proton_flux, electron_flux)
        """

        # Latitude effect: radiation concentrated near magnetic equator
        # Dipole magnetic field is weakest at equator (particles trapped)
        latitude_factor = np.cos(np.radians(latitude)) ** 2

        # Initialize intensity variables
        inner_intensity = 0.0
        outer_intensity = 0.0

        # Inner belt (protons)
        if INNER_BELT_MIN_ALT <= altitude <= INNER_BELT_MAX_ALT:
            # Gaussian peak centered at INNER_BELT_PEAK_ALT
            distance_from_peak = abs(altitude - INNER_BELT_PEAK_ALT)
            sigma = (INNER_BELT_MAX_ALT - INNER_BELT_MIN_ALT) / 4.0
            inner_intensity = INNER_BELT_MAX_INTENSITY * np.exp(-(distance_from_peak**2) / (2 * sigma**2))
            proton_flux = inner_intensity * 1e8  # particles/cm²/s
            electron_flux = inner_intensity * 1e6
        # Outer belt (electrons)
        elif OUTER_BELT_MIN_ALT <= altitude <= OUTER_BELT_MAX_ALT:
            distance_from_peak = abs(altitude - OUTER_BELT_PEAK_ALT)
            sigma = (OUTER_BELT_MAX_ALT - OUTER_BELT_MIN_ALT) / 4.0
            outer_intensity = OUTER_BELT_MAX_INTENSITY * np.exp(-(distance_from_peak**2) / (2 * sigma**2))
            proton_flux = outer_intensity * 1e6
            electron_flux = outer_intensity * 1e9  # Higher electron flux
            inner_intensity = 0.0
        # Slot region (minimal radiation)
        elif SLOT_REGION_MIN_ALT <= altitude <= SLOT_REGION_MAX_ALT:
            inner_intensity = SLOT_REGION_INTENSITY
            outer_intensity = SLOT_REGION_INTENSITY
            proton_flux = 1e5
            electron_flux = 1e5
        else:
            inner_intensity = 0.0
            outer_intensity = 0.0
            proton_flux = 1e4  # Background cosmic rays
            electron_flux = 1e4

        # Total dose rate (Sv/day)
        total_dose_rate = (inner_intensity + outer_intensity) * latitude_factor

        # Apply geomagnetic activity multiplier
        # Kp > 5 = storm conditions = increased radiation
        geomagnetic_multiplier = 1.0 + (self.geomagnetic_index_kp - 3.0) * 0.2
        total_dose_rate *= max(0.5, geomagnetic_multiplier)

        return total_dose_rate, proton_flux * latitude_factor, electron_flux * latitude_factor

    def calculate_magnetic_field_strength(self, altitude: float, latitude: float) -> float:
        """
        Calculate magnetic field strength at position using dipole model.

        Returns field strength in nT (nanotesla)
        """
        radius = EARTH_RADIUS + altitude
        lat_rad = np.radians(latitude)

        # Dipole magnetic field: B = B₀(R_E/r)³√(1 + 3sin²λ)
        field_strength = MAGNETIC_FIELD_STRENGTH_SURFACE * \
                        (EARTH_RADIUS / radius) ** 3 * \
                        np.sqrt(1 + 3 * np.sin(lat_rad) ** 2)

        return field_strength

    def apply_radiation_effects(self, satellite_health: SatelliteHealth,
                                altitude: float, latitude: float,
                                time_delta_seconds: float) -> RadiationExposure:
        """
        Apply radiation effects to satellite and return exposure data.

        Args:
            satellite_health: Satellite health data to modify
            altitude: Current altitude (km)
            latitude: Current latitude (degrees)
            time_delta_seconds: Time since last update (seconds)
        """

        # Get radiation environment
        dose_rate, proton_flux, electron_flux = self.calculate_radiation_intensity(altitude, latitude)
        region = self.classify_radiation_region(altitude)
        mag_field = self.calculate_magnetic_field_strength(altitude, latitude)

        # Calculate dose received during time delta
        time_delta_days = time_delta_seconds / 86400.0
        dose_received = dose_rate * time_delta_days

        # Apply shielding protection
        effective_dose = dose_received * (1.0 - satellite_health.radiation_shielding)

        # Accumulate radiation dose
        satellite_health.cumulative_radiation_dose += effective_dose

        # Apply radiation damage effects
        if effective_dose > 0:
            # Solar panel degradation (1% per 1000 Sv total dose)
            solar_degradation = effective_dose * 0.00001
            satellite_health.solar_panel_efficiency = max(0.1,
                satellite_health.solar_panel_efficiency - solar_degradation)

            # Electronics degradation (depends on hardening)
            electronics_degradation = effective_dose * 0.0001 * (1.0 - satellite_health.radiation_hardening)
            satellite_health.electronics_health = max(0.1,
                satellite_health.electronics_health - electronics_degradation)

            # Overall health degradation
            health_degradation = effective_dose * 0.00005
            satellite_health.health = max(0.1, satellite_health.health - health_degradation)

        # Create exposure record
        exposure = RadiationExposure(
            satellite_id=satellite_health.satellite_id,
            timestamp=datetime.utcnow(),
            altitude=altitude,
            latitude=latitude,
            region=region,
            dose_rate=dose_rate,
            cumulative_dose=satellite_health.cumulative_radiation_dose,
            magnetic_field_strength=mag_field,
            proton_flux=proton_flux,
            electron_flux=electron_flux
        )

        return exposure

    def simulate_solar_particle_event(self, intensity: float = 1.0, duration_hours: float = 24.0):
        """
        Simulate a solar particle event (SPE) - coronal mass ejection.

        Increases radiation levels globally for duration.
        """
        event = {
            'start_time': datetime.utcnow(),
            'intensity': intensity,  # 1.0 = moderate, 5.0 = extreme
            'duration': duration_hours,
            'geomagnetic_kp': min(9.0, 3.0 + intensity * 2.0)
        }

        self.solar_particle_events.append(event)
        self.geomagnetic_index_kp = event['geomagnetic_kp']

        return event

    def get_belt_statistics(self, satellite_healths: List[SatelliteHealth],
                           altitudes: List[float]) -> Dict:
        """Calculate statistics across radiation belt regions"""

        region_counts = {region: 0 for region in RadiationBeltRegion}
        region_avg_dose = {region: [] for region in RadiationBeltRegion}

        for health, altitude in zip(satellite_healths, altitudes):
            region = self.classify_radiation_region(altitude)
            region_counts[region] += 1
            region_avg_dose[region].append(health.cumulative_radiation_dose)

        stats = {}
        for region in RadiationBeltRegion:
            doses = region_avg_dose[region]
            stats[region.value] = {
                'count': region_counts[region],
                'avg_dose': float(np.mean(doses)) if doses else 0.0,
                'max_dose': float(np.max(doses)) if doses else 0.0
            }

        return stats


class EMPWeaponSystem:
    """
    Electromagnetic Pulse (EMP) weapon system simulation.

    Models various EMP sources and their effects on satellite electronics.
    """

    def __init__(self):
        self.active_emp_events: List[EMPEvent] = []
        self.total_emp_events = 0

    def create_emp_event(self, position: np.ndarray, source_type: str = "NUCLEAR",
                        intensity: float = 0.8) -> EMPEvent:
        """
        Create a new EMP event.

        Args:
            position: [x, y, z] position in ECI frame (km)
            source_type: NUCLEAR, HERF, SOLAR_FLARE, CYBER
            intensity: 0.0 to 1.0
        """

        self.total_emp_events += 1

        event = EMPEvent(
            event_id=f"EMP_{datetime.utcnow().strftime('%Y%m%d_%H%M%S')}_{self.total_emp_events}",
            position=position,
            start_time=datetime.utcnow(),
            duration=EMP_PULSE_DURATION,
            intensity=intensity,
            range=EMP_WEAPON_RANGE,
            source_type=source_type
        )

        self.active_emp_events.append(event)
        return event

    def calculate_emp_damage(self, satellite_position: np.ndarray,
                           satellite_health: SatelliteHealth,
                           emp_event: EMPEvent) -> float:
        """
        Calculate EMP damage to satellite.

        Returns damage factor (0.0 to 1.0)
        """

        # Calculate distance from EMP source
        distance = np.linalg.norm(satellite_position - emp_event.position)

        if distance > emp_event.range:
            return 0.0  # Out of range

        # Damage decreases with distance (inverse square law)
        distance_factor = 1.0 - (distance / emp_event.range)
        distance_factor = distance_factor ** 2  # Inverse square

        # EMP shielding reduces damage
        shielding_protection = satellite_health.emp_shielding

        # Calculate effective damage
        damage = emp_event.intensity * distance_factor * (1.0 - shielding_protection)

        return damage

    def apply_emp_effects(self, satellite_position: np.ndarray,
                         satellite_health: SatelliteHealth,
                         current_time: datetime) -> Dict:
        """
        Apply EMP effects from all active events.

        Returns dictionary with damage info.
        """

        total_damage = 0.0
        affected_by_events = []

        # Check all active EMP events
        for emp_event in self.active_emp_events:
            if not emp_event.active:
                continue

            damage = self.calculate_emp_damage(satellite_position, satellite_health, emp_event)

            if damage > 0:
                total_damage += damage
                affected_by_events.append({
                    'event_id': emp_event.event_id,
                    'damage': damage,
                    'source_type': emp_event.source_type
                })

        # Apply damage to satellite
        if total_damage > 0:
            satellite_health.emp_affected = True
            satellite_health.emp_recovery_time = current_time + timedelta(
                seconds=EMP_RECOVERY_TIME_BASE * total_damage
            )

            # Electronics damage
            satellite_health.electronics_health *= (1.0 - total_damage * 0.5)
            satellite_health.electronics_health = max(0.05, satellite_health.electronics_health)

            # Overall health impact
            satellite_health.health *= (1.0 - total_damage * 0.3)
            satellite_health.health = max(0.05, satellite_health.health)

        return {
            'total_damage': total_damage,
            'events': affected_by_events,
            'emp_affected': satellite_health.emp_affected,
            'recovery_time': satellite_health.emp_recovery_time.isoformat()
                           if satellite_health.emp_recovery_time else None
        }

    def update_emp_events(self, current_time: datetime):
        """Remove expired EMP events"""
        self.active_emp_events = [
            event for event in self.active_emp_events
            if not event.is_expired(current_time)
        ]

    def check_emp_recovery(self, satellite_health: SatelliteHealth, current_time: datetime):
        """Check if satellite has recovered from EMP"""
        if satellite_health.emp_affected and satellite_health.emp_recovery_time:
            if current_time >= satellite_health.emp_recovery_time:
                satellite_health.emp_affected = False
                satellite_health.emp_recovery_time = None
                # Partial recovery of electronics
                satellite_health.electronics_health = min(1.0,
                    satellite_health.electronics_health + 0.2)


class SpaceEnvironmentSimulator:
    """
    Complete space environment simulation combining radiation and EMP effects.
    """

    def __init__(self):
        self.radiation_belt = VanAllenRadiationBelt()
        self.emp_system = EMPWeaponSystem()
        self.satellite_health_db: Dict[int, SatelliteHealth] = {}
        self.last_update_time = datetime.utcnow()

    def initialize_satellite(self, satellite_id: int,
                           radiation_shielding: float = 0.5,
                           emp_shielding: float = 0.5,
                           radiation_hardening: float = 0.5) -> SatelliteHealth:
        """Initialize satellite health tracking"""

        health = SatelliteHealth(
            satellite_id=satellite_id,
            radiation_shielding=radiation_shielding,
            emp_shielding=emp_shielding,
            radiation_hardening=radiation_hardening
        )

        self.satellite_health_db[satellite_id] = health
        return health

    def update_satellite(self, satellite_id: int, position_eci: np.ndarray,
                        latitude: float, altitude: float) -> Dict:
        """
        Update satellite with space environment effects.

        Returns comprehensive status dictionary.
        """

        current_time = datetime.utcnow()
        time_delta = (current_time - self.last_update_time).total_seconds()

        # Get or create satellite health
        if satellite_id not in self.satellite_health_db:
            self.initialize_satellite(satellite_id)

        health = self.satellite_health_db[satellite_id]

        # Apply radiation effects
        radiation_exposure = self.radiation_belt.apply_radiation_effects(
            health, altitude, latitude, time_delta
        )

        # Apply EMP effects
        emp_status = self.emp_system.apply_emp_effects(
            position_eci, health, current_time
        )

        # Check for EMP recovery
        self.emp_system.check_emp_recovery(health, current_time)

        return {
            'satellite_id': satellite_id,
            'health': health.to_dict(),
            'radiation': radiation_exposure.to_dict(),
            'emp': emp_status,
            'timestamp': current_time.isoformat()
        }

    def update_all_satellites(self, satellites_data: List[Dict]) -> List[Dict]:
        """
        Batch update multiple satellites.

        Args:
            satellites_data: List of dicts with keys: satellite_id, position_eci, latitude, altitude
        """

        results = []
        for sat_data in satellites_data:
            result = self.update_satellite(
                satellite_id=sat_data['satellite_id'],
                position_eci=sat_data['position_eci'],
                latitude=sat_data['latitude'],
                altitude=sat_data['altitude']
            )
            results.append(result)

        self.last_update_time = datetime.utcnow()

        # Update EMP events
        self.emp_system.update_emp_events(self.last_update_time)

        return results

    def get_statistics(self) -> Dict:
        """Get overall system statistics"""

        healths = list(self.satellite_health_db.values())
        altitudes = []  # Would need to track this separately

        return {
            'total_satellites': len(healths),
            'avg_health': float(np.mean([h.health for h in healths])) if healths else 0.0,
            'avg_radiation_dose': float(np.mean([h.cumulative_radiation_dose for h in healths])) if healths else 0.0,
            'emp_affected_count': sum(1 for h in healths if h.emp_affected),
            'active_emp_events': len(self.emp_system.active_emp_events),
            'total_emp_events': self.emp_system.total_emp_events,
            'geomagnetic_kp': self.radiation_belt.geomagnetic_index_kp
        }


if __name__ == "__main__":
    # Quick test
    print("Space Environment Effects Module - Test")
    print("=" * 60)

    # Create simulator
    print("\n1. Initializing space environment simulator...")
    sim = SpaceEnvironmentSimulator()

    # Test satellite in inner radiation belt
    print("\n2. Testing satellite in inner Van Allen belt...")
    test_position = np.array([0, 0, EARTH_RADIUS + 3500])  # Inner belt peak
    result = sim.update_satellite(
        satellite_id=1,
        position_eci=test_position,
        latitude=0.0,
        altitude=3500.0
    )

    print(f"   Radiation region: {result['radiation']['region']}")
    print(f"   Dose rate: {result['radiation']['dose_rate']:.4f} Sv/day")
    print(f"   Proton flux: {result['radiation']['proton_flux']:.2e} particles/cm²/s")
    print(f"   Health: {result['health']['health']:.4f}")

    # Test EMP event
    print("\n3. Creating EMP event...")
    emp_pos = np.array([100, 100, EARTH_RADIUS + 3500])
    emp_event = sim.emp_system.create_emp_event(emp_pos, "NUCLEAR", 0.9)
    print(f"   EMP Event ID: {emp_event.event_id}")
    print(f"   Range: {emp_event.range} km")

    # Update satellite with EMP
    result = sim.update_satellite(1, test_position, 0.0, 3500.0)
    print(f"   EMP damage: {result['emp']['total_damage']:.4f}")
    print(f"   EMP affected: {result['emp']['emp_affected']}")

    print("\n" + "=" * 60)
    print("Module test complete!")
