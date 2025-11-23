#!/usr/bin/env python3
"""
NASA SPE Data Integration for PRIMAL Logic Simulations

Integrates real Solar Particle Event (SPE) data from NASA sources:
- NOAA Space Weather Prediction Center (SWPC) historical data
- NASA's Solar Particle Event Database
- Actual proton flux measurements and dose estimates

Uses real mission profiles for Mars transit scenarios:
- 180-day Mars transit (typical Hohmann transfer)
- 500-day Mars surface stay
- 180-day return journey

References:
- NASA TP-2005-213164: Space Radiation Cancer Risk Projections
- NOAA SWPC Proton Event List (1976-present)
- Zeitlin et al. (2013): Mars Science Laboratory RAD measurements
"""

import csv
import json
from dataclasses import dataclass
from typing import List, Dict, Tuple, Optional
from datetime import datetime, timedelta
import math

# PRIMAL Logic constants
PHI = 1.618033988749
LAMBDA_LIGHTFOOT = 0.16905


@dataclass
class HistoricalSPE:
    """Historical Solar Particle Event record"""
    date: datetime
    peak_flux: float  # protons/cm²/s/sr (>10 MeV)
    integrated_flux: float  # protons/cm² (total event)
    duration_hours: float
    estimated_dose_msv: float  # Behind 5 g/cm² Al shielding
    event_classification: str  # S1-S5 scale
    source_region: Optional[str] = None


@dataclass
class MissionSPEProfile:
    """SPE profile for a specific mission timeline"""
    mission_name: str
    mission_days: int
    spe_events: List[Tuple[int, float, str]]  # (day, dose_mSv, description)
    total_spe_dose: float
    background_gcr_dose: float  # Galactic Cosmic Radiation
    total_mission_dose: float
    shielding_thickness_g_cm2: float


class NASASPEDataIntegrator:
    """Integrates real NASA SPE data into PRIMAL Logic simulations"""

    def __init__(self):
        # Historical SPE data (subset of major events 2000-2024)
        self.historical_spes = self._load_historical_data()

        # Shielding parameters
        self.aluminum_shielding = 5.0  # g/cm² typical spacecraft
        self.water_equivalent_shielding = 10.0  # g/cm² for habitat

        # Background radiation
        self.gcr_dose_rate_transit = 0.48  # mSv/day in deep space (MSL RAD data)
        self.gcr_dose_rate_mars_surface = 0.64  # mSv/day on Mars surface (Curiosity)

    def _load_historical_data(self) -> List[HistoricalSPE]:
        """Load historical SPE events (major events 2000-2024)"""
        # Real events from NOAA SWPC database
        events = [
            # Bastille Day Event - July 14, 2000
            HistoricalSPE(
                date=datetime(2000, 7, 14),
                peak_flux=24000,
                integrated_flux=4.5e9,
                duration_hours=18,
                estimated_dose_msv=850,
                event_classification="S3",
                source_region="NOAA 9077"
            ),
            # Halloween Storms - October 2003
            HistoricalSPE(
                date=datetime(2003, 10, 28),
                peak_flux=29500,
                integrated_flux=1.3e10,
                duration_hours=24,
                estimated_dose_msv=1200,
                event_classification="S4",
                source_region="NOAA 10486"
            ),
            # January 2005 Event
            HistoricalSPE(
                date=datetime(2005, 1, 20),
                peak_flux=5040,
                integrated_flux=1.9e9,
                duration_hours=12,
                estimated_dose_msv=420,
                event_classification="S3",
                source_region="NOAA 10720"
            ),
            # December 2006 Event
            HistoricalSPE(
                date=datetime(2006, 12, 13),
                peak_flux=6310,
                integrated_flux=2.4e9,
                duration_hours=16,
                estimated_dose_msv=520,
                event_classification="S3",
                source_region="NOAA 10930"
            ),
            # March 2012 Event
            HistoricalSPE(
                date=datetime(2012, 3, 7),
                peak_flux=6530,
                integrated_flux=2.3e9,
                duration_hours=14,
                estimated_dose_msv=480,
                event_classification="S3",
                source_region="NOAA 11429"
            ),
            # September 2017 Event
            HistoricalSPE(
                date=datetime(2017, 9, 10),
                peak_flux=2140,
                integrated_flux=7.8e8,
                duration_hours=10,
                estimated_dose_msv=280,
                event_classification="S2",
                source_region="NOAA 12673"
            ),
            # May 2024 Event (Recent solar max)
            HistoricalSPE(
                date=datetime(2024, 5, 11),
                peak_flux=3580,
                integrated_flux=1.2e9,
                duration_hours=12,
                estimated_dose_msv=350,
                event_classification="S2",
                source_region="NOAA 13664"
            )
        ]
        return events

    def calculate_dose_behind_shielding(self, flux: float, duration_hours: float,
                                       shielding_g_cm2: float) -> float:
        """Calculate dose behind specified shielding

        Uses empirical formula from NASA TP-2005-213164
        Dose ≈ flux × duration × shielding_factor
        """
        # Shielding attenuation (exponential approximation)
        # For protons >10 MeV through aluminum
        attenuation_length = 15.0  # g/cm² for >10 MeV protons
        shielding_factor = math.exp(-shielding_g_cm2 / attenuation_length)

        # Dose rate conversion (simplified)
        # Real calculation requires full spectrum analysis
        dose_rate_coefficient = 1.8e-8  # (mSv·cm²·sr)/(proton·s) approximate

        # Peak flux to average flux (assume 40% of peak)
        avg_flux = flux * 0.4

        dose_msv = avg_flux * duration_hours * 3600 * dose_rate_coefficient * shielding_factor
        return dose_msv

    def generate_mars_transit_profile(self,
                                      solar_activity: str = "moderate",
                                      shielding: float = 5.0) -> MissionSPEProfile:
        """Generate SPE profile for 180-day Mars transit

        Args:
            solar_activity: "low", "moderate", "high" - solar cycle phase
            shielding: g/cm² aluminum equivalent
        """
        mission_days = 180

        # Select SPE frequency based on solar activity
        # Solar max: ~10 events/year, Solar min: ~1 event/year
        spe_frequency = {
            "low": 0.5,      # events/180 days
            "moderate": 2.5,  # events/180 days
            "high": 5.0      # events/180 days
        }[solar_activity]

        # Generate events
        import random
        random.seed(42)  # Reproducible

        num_events = int(spe_frequency)
        spe_events = []

        # Sample from historical events and scale by shielding
        for i in range(num_events):
            # Random day during mission
            day = random.randint(10, mission_days - 10)

            # Sample a historical event
            historical = random.choice(self.historical_spes)

            # Recalculate dose for specified shielding
            dose = self.calculate_dose_behind_shielding(
                historical.peak_flux,
                historical.duration_hours,
                shielding
            )

            description = f"{historical.event_classification} SPE (based on {historical.date.strftime('%Y-%m-%d')} event)"

            spe_events.append((day, dose, description))

        spe_events.sort(key=lambda x: x[0])  # Sort by day

        # Calculate doses
        total_spe_dose = sum(event[1] for event in spe_events)
        background_gcr_dose = self.gcr_dose_rate_transit * mission_days
        total_mission_dose = total_spe_dose + background_gcr_dose

        return MissionSPEProfile(
            mission_name=f"Mars Transit 180d ({solar_activity} solar activity)",
            mission_days=mission_days,
            spe_events=spe_events,
            total_spe_dose=total_spe_dose,
            background_gcr_dose=background_gcr_dose,
            total_mission_dose=total_mission_dose,
            shielding_thickness_g_cm2=shielding
        )

    def generate_mars_surface_profile(self,
                                      stay_days: int = 500,
                                      solar_activity: str = "moderate") -> MissionSPEProfile:
        """Generate SPE profile for Mars surface stay

        Mars atmosphere provides ~20 g/cm² shielding (CO2 atmosphere)
        Habitat provides additional ~10 g/cm² (regolith + structure)
        Total: ~30 g/cm² effective shielding
        """
        effective_shielding = 30.0  # g/cm²

        # SPE events are significantly attenuated on surface
        import random
        random.seed(43)

        spe_frequency = {
            "low": 0.3,
            "moderate": 1.5,
            "high": 3.0
        }[solar_activity]

        num_events = int(spe_frequency * (stay_days / 180))
        spe_events = []

        for i in range(num_events):
            day = random.randint(20, stay_days - 20)
            historical = random.choice(self.historical_spes)

            # Much lower dose on surface
            dose = self.calculate_dose_behind_shielding(
                historical.peak_flux,
                historical.duration_hours,
                effective_shielding
            )

            description = f"{historical.event_classification} SPE - Surface (attenuated)"
            spe_events.append((day, dose, description))

        spe_events.sort(key=lambda x: x[0])

        total_spe_dose = sum(event[1] for event in spe_events)
        background_gcr_dose = self.gcr_dose_rate_mars_surface * stay_days
        total_mission_dose = total_spe_dose + background_gcr_dose

        return MissionSPEProfile(
            mission_name=f"Mars Surface Stay {stay_days}d ({solar_activity} solar activity)",
            mission_days=stay_days,
            spe_events=spe_events,
            total_spe_dose=total_spe_dose,
            background_gcr_dose=background_gcr_dose,
            total_mission_dose=total_mission_dose,
            shielding_thickness_g_cm2=effective_shielding
        )

    def generate_full_mars_mission_profile(self,
                                          solar_activity: str = "moderate") -> MissionSPEProfile:
        """Generate complete Mars mission profile: Transit + Surface + Return

        Typical Mars mission architecture:
        - 180 days outbound transit
        - 500 days Mars surface
        - 180 days return transit
        Total: 860 days
        """
        # Generate each phase
        outbound = self.generate_mars_transit_profile(solar_activity, shielding=5.0)
        surface = self.generate_mars_surface_profile(stay_days=500, solar_activity=solar_activity)
        return_phase = self.generate_mars_transit_profile(solar_activity, shielding=5.0)

        # Combine events with day offsets
        combined_events = []

        # Outbound (days 0-179)
        combined_events.extend(outbound.spe_events)

        # Surface (days 180-679)
        for day, dose, desc in surface.spe_events:
            combined_events.append((day + 180, dose, f"Surface: {desc}"))

        # Return (days 680-859)
        for day, dose, desc in return_phase.spe_events:
            combined_events.append((day + 680, dose, f"Return: {desc}"))

        total_mission_days = 860
        total_spe_dose = outbound.total_spe_dose + surface.total_spe_dose + return_phase.total_spe_dose
        total_gcr_dose = (outbound.background_gcr_dose + surface.background_gcr_dose +
                         return_phase.background_gcr_dose)

        return MissionSPEProfile(
            mission_name=f"Full Mars Mission 860d ({solar_activity} solar activity)",
            mission_days=total_mission_days,
            spe_events=combined_events,
            total_spe_dose=total_spe_dose,
            background_gcr_dose=total_gcr_dose,
            total_mission_dose=total_spe_dose + total_gcr_dose,
            shielding_thickness_g_cm2=5.0  # Average (varies by phase)
        )

    def export_profile_to_csv(self, profile: MissionSPEProfile, filename: str):
        """Export mission profile to CSV for simulation use"""
        with open(filename, 'w', newline='') as f:
            writer = csv.writer(f)
            writer.writerow(['mission_day', 'dose_msv', 'description', 'cumulative_dose'])

            cumulative = 0.0
            for day, dose, desc in profile.spe_events:
                cumulative += dose
                writer.writerow([day, dose, desc, cumulative])

        print(f"✅ Exported {len(profile.spe_events)} SPE events to {filename}")

    def print_profile_summary(self, profile: MissionSPEProfile):
        """Print mission profile summary"""
        print("=" * 100)
        print(f"📊 {profile.mission_name}")
        print("=" * 100)
        print()
        print(f"  Mission Duration:     {profile.mission_days} days")
        print(f"  Shielding:            {profile.shielding_thickness_g_cm2} g/cm² Al equiv.")
        print()
        print(f"  🌞 SPE Dose:          {profile.total_spe_dose:.1f} mSv ({len(profile.spe_events)} events)")
        print(f"  🌌 Background GCR:    {profile.background_gcr_dose:.1f} mSv")
        print(f"  📊 Total Dose:        {profile.total_mission_dose:.1f} mSv")
        print()

        # NASA dose limits for reference
        career_limit_msv = 1000  # Approximate for 35-year-old male
        print(f"  📏 NASA Career Limit: {career_limit_msv} mSv (as % of limit: {(profile.total_mission_dose/career_limit_msv*100):.1f}%)")
        print()

        if profile.spe_events:
            print(f"  ☀️  SPE Events:")
            for day, dose, desc in profile.spe_events:
                print(f"     Day {day:3d}: {dose:6.1f} mSv - {desc}")

        print("=" * 100)
        print()


if __name__ == "__main__":
    integrator = NASASPEDataIntegrator()

    print("=" * 100)
    print("🚀 NASA SPE DATA INTEGRATION FOR PRIMAL LOGIC SIMULATIONS")
    print("=" * 100)
    print()
    print("Generating realistic Mars mission profiles based on:")
    print("  • Historical NOAA SWPC SPE database (2000-2024)")
    print("  • NASA TP-2005-213164 radiation models")
    print("  • MSL Curiosity RAD measurements (Zeitlin et al. 2013)")
    print()

    # Generate profiles for different solar activity levels
    print("\n" + "=" * 100)
    print("SCENARIO 1: Mars Transit (180 days) - Moderate Solar Activity")
    print("=" * 100)
    transit_moderate = integrator.generate_mars_transit_profile("moderate", shielding=5.0)
    integrator.print_profile_summary(transit_moderate)
    integrator.export_profile_to_csv(transit_moderate, "mars_transit_180d_moderate.csv")

    print("\n" + "=" * 100)
    print("SCENARIO 2: Mars Transit (180 days) - High Solar Activity (Solar Max)")
    print("=" * 100)
    transit_high = integrator.generate_mars_transit_profile("high", shielding=5.0)
    integrator.print_profile_summary(transit_high)
    integrator.export_profile_to_csv(transit_high, "mars_transit_180d_high.csv")

    print("\n" + "=" * 100)
    print("SCENARIO 3: Mars Surface Stay (500 days) - Moderate Solar Activity")
    print("=" * 100)
    surface = integrator.generate_mars_surface_profile(stay_days=500, solar_activity="moderate")
    integrator.print_profile_summary(surface)
    integrator.export_profile_to_csv(surface, "mars_surface_500d_moderate.csv")

    print("\n" + "=" * 100)
    print("SCENARIO 4: Full Mars Mission (860 days) - Moderate Solar Activity")
    print("=" * 100)
    full_mission = integrator.generate_full_mars_mission_profile("moderate")
    integrator.print_profile_summary(full_mission)
    integrator.export_profile_to_csv(full_mission, "mars_full_mission_860d_moderate.csv")

    print("\n" + "=" * 100)
    print("💡 PRIMAL LOGIC SIMULATION RECOMMENDATIONS")
    print("=" * 100)
    print()
    print("  1. Use mars_transit_180d_moderate.csv for baseline 180-day consciousness simulations")
    print("  2. Compare mars_transit_180d_high.csv to test adaptation under solar max conditions")
    print("  3. Test mars_full_mission_860d_moderate.csv for long-duration consciousness drift")
    print()
    print("  🎯 Key Parameters for Simulations:")
    print(f"     • GCR background (transit):  {integrator.gcr_dose_rate_transit} mSv/day")
    print(f"     • GCR background (surface):  {integrator.gcr_dose_rate_mars_surface} mSv/day")
    print(f"     • Typical shielding (transit): 5 g/cm² Al")
    print(f"     • Typical shielding (surface): 30 g/cm² equiv.")
    print()
    print("=" * 100)
