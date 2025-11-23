#!/usr/bin/env python3
"""
Federal Reserve Economic Data (FRED) Integration for PRIMAL Logic Network Simulations

Provides real-time economic indicators from the Federal Reserve Bank of St. Louis
to enhance network threat assessment, financial contagion modeling, and adaptive
consciousness algorithms.

API Key: Register at https://fred.stlouisfed.org/docs/api/api_key.html
"""

import os
import requests
import time
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
import json


class FREDClient:
    """Federal Reserve Economic Data API Client"""

    # FRED API endpoints
    BASE_URL = "https://api.stlouisfed.org/fred"

    # Key economic indicators for PRIMAL Logic threat assessment
    THREAT_INDICATORS = {
        'financial_stress': 'STLFSI4',      # St. Louis Fed Financial Stress Index
        'unemployment': 'UNRATE',            # Unemployment Rate
        'inflation': 'CPIAUCSL',             # Consumer Price Index
        'vix': 'VIXCLS',                     # CBOE Volatility Index
        'credit_spread': 'BAMLH0A0HYM2',     # High Yield Credit Spread
        'ted_spread': 'TEDRATE',             # TED Spread (bank counterparty risk)
        'industrial_production': 'INDPRO',   # Industrial Production Index
        'housing_starts': 'HOUST',           # Housing Starts
    }

    # Regional data for St. Louis
    STL_REGIONAL = {
        'stl_unemployment': 'SAIN729URN',    # St. Louis Unemployment
        'stl_gdp': 'NGMP41180',              # St. Louis GDP
        'stl_house_price': 'ATNHPIUS41180Q', # St. Louis House Price Index
    }

    # Banking/Financial Network indicators
    BANKING_INDICATORS = {
        'fed_funds_rate': 'FEDFUNDS',        # Federal Funds Rate
        'reserves': 'TOTRESNS',              # Total Reserves
        'money_supply_m1': 'M1SL',           # M1 Money Supply
        'money_supply_m2': 'M2SL',           # M2 Money Supply
        'bank_credit': 'TOTBKCR',            # Total Bank Credit
        'commercial_paper': 'COMPAPER',      # Commercial Paper Outstanding
    }

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize FRED client

        Args:
            api_key: FRED API key. If not provided, reads from FRED_API_KEY env var
        """
        self.api_key = api_key or os.getenv('FRED_API_KEY')
        if not self.api_key:
            raise ValueError(
                "FRED API key required. Get one at: "
                "https://fred.stlouisfed.org/docs/api/api_key.html"
            )

        self.session = requests.Session()
        self.cache = {}
        self.cache_duration = 3600  # 1 hour cache

    def get_series(self, series_id: str, limit: int = 100) -> List[Dict[str, Any]]:
        """
        Get time series data from FRED

        Args:
            series_id: FRED series ID (e.g., 'UNRATE', 'GDP')
            limit: Maximum number of observations to return

        Returns:
            List of observations with date and value
        """
        # Check cache
        cache_key = f"{series_id}_{limit}"
        if cache_key in self.cache:
            cached_time, cached_data = self.cache[cache_key]
            if time.time() - cached_time < self.cache_duration:
                return cached_data

        # Fetch from API
        url = f"{self.BASE_URL}/series/observations"
        params = {
            'series_id': series_id,
            'api_key': self.api_key,
            'file_type': 'json',
            'limit': limit,
            'sort_order': 'desc'  # Most recent first
        }

        try:
            response = self.session.get(url, params=params, timeout=10)
            response.raise_for_status()
            data = response.json()

            observations = data.get('observations', [])

            # Cache result
            self.cache[cache_key] = (time.time(), observations)

            return observations

        except requests.RequestException as e:
            print(f"⚠️ FRED API error for {series_id}: {e}")
            return []

    def get_latest_value(self, series_id: str) -> Optional[float]:
        """Get the most recent value for a series"""
        observations = self.get_series(series_id, limit=1)
        if observations:
            try:
                value = observations[0].get('value')
                if value and value != '.':  # FRED uses '.' for missing data
                    return float(value)
            except (ValueError, IndexError):
                pass
        return None

    def get_economic_threat_score(self) -> Dict[str, Any]:
        """
        Calculate composite economic threat score for PRIMAL Logic

        Returns:
            Dictionary with individual indicators and composite threat score (0-1)
        """
        indicators = {}
        threat_components = []

        # Financial Stress Index (already normalized around 0)
        stress_index = self.get_latest_value('STLFSI4')
        if stress_index is not None:
            indicators['financial_stress'] = stress_index
            # Normalize: -1.5 to 5 range -> 0 to 1
            normalized_stress = max(0, min(1, (stress_index + 1.5) / 6.5))
            threat_components.append(normalized_stress)

        # Unemployment Rate (normal ~4%, crisis >10%)
        unemployment = self.get_latest_value('UNRATE')
        if unemployment is not None:
            indicators['unemployment'] = unemployment
            # Normalize: 0-15% -> 0-1
            normalized_unemp = min(1, unemployment / 15.0)
            threat_components.append(normalized_unemp)

        # VIX Volatility (normal ~15, crisis >40)
        vix = self.get_latest_value('VIXCLS')
        if vix is not None:
            indicators['vix'] = vix
            # Normalize: 10-80 -> 0-1
            normalized_vix = max(0, min(1, (vix - 10) / 70))
            threat_components.append(normalized_vix)

        # Credit Spread (normal ~4%, crisis >10%)
        credit_spread = self.get_latest_value('BAMLH0A0HYM2')
        if credit_spread is not None:
            indicators['credit_spread'] = credit_spread
            # Normalize: 2-15% -> 0-1
            normalized_spread = max(0, min(1, (credit_spread - 2) / 13))
            threat_components.append(normalized_spread)

        # TED Spread (normal ~0.2%, crisis >2%)
        ted_spread = self.get_latest_value('TEDRATE')
        if ted_spread is not None:
            indicators['ted_spread'] = ted_spread
            # Normalize: 0-4% -> 0-1
            normalized_ted = min(1, ted_spread / 4.0)
            threat_components.append(normalized_ted)

        # Calculate composite threat score
        if threat_components:
            composite_threat = sum(threat_components) / len(threat_components)
        else:
            composite_threat = 0.0

        return {
            'composite_threat': composite_threat,
            'indicators': indicators,
            'timestamp': datetime.now().isoformat(),
            'threat_level': self._classify_threat(composite_threat)
        }

    def _classify_threat(self, threat_score: float) -> str:
        """Classify threat level"""
        if threat_score < 0.3:
            return "LOW"
        elif threat_score < 0.6:
            return "MODERATE"
        elif threat_score < 0.8:
            return "HIGH"
        else:
            return "CRITICAL"

    def get_stl_regional_data(self) -> Dict[str, Any]:
        """Get St. Louis regional economic indicators"""
        regional = {}

        for name, series_id in self.STL_REGIONAL.items():
            value = self.get_latest_value(series_id)
            if value is not None:
                regional[name] = value

        return {
            'region': 'St. Louis, MO',
            'data': regional,
            'timestamp': datetime.now().isoformat()
        }

    def get_banking_network_state(self) -> Dict[str, Any]:
        """Get banking/financial network indicators"""
        banking = {}

        for name, series_id in self.BANKING_INDICATORS.items():
            value = self.get_latest_value(series_id)
            if value is not None:
                banking[name] = value

        # Calculate network stress indicators
        fed_funds = banking.get('fed_funds_rate', 0)

        return {
            'banking_indicators': banking,
            'network_stress': self._calculate_banking_stress(banking),
            'timestamp': datetime.now().isoformat()
        }

    def _calculate_banking_stress(self, indicators: Dict[str, float]) -> float:
        """Calculate banking system stress (0-1)"""
        stress_signals = []

        # High fed funds rate = tightening = stress
        fed_funds = indicators.get('fed_funds_rate', 0)
        if fed_funds > 0:
            stress_signals.append(min(1, fed_funds / 10.0))

        # Low reserves = stress
        reserves = indicators.get('reserves', 0)
        if reserves > 0:
            # Inverse: lower reserves = higher stress
            # Normalize assuming 3 trillion is comfortable, 1 trillion is stressed
            stress_signals.append(max(0, min(1, 1 - (reserves / 3000000))))

        return sum(stress_signals) / len(stress_signals) if stress_signals else 0.0

    def simulate_financial_contagion(self, initial_shock: float = 0.5) -> Dict[str, Any]:
        """
        Simulate financial contagion using current economic conditions

        Args:
            initial_shock: Initial shock magnitude (0-1)

        Returns:
            Contagion simulation results
        """
        # Get current economic state
        threat = self.get_economic_threat_score()
        banking = self.get_banking_network_state()

        # Contagion parameters based on real data
        base_contagion = threat['composite_threat']
        banking_stress = banking['network_stress']

        # Simulate spread through financial network
        timesteps = 10
        contagion_history = [initial_shock]

        for t in range(timesteps):
            current = contagion_history[-1]

            # Spread factor influenced by economic conditions
            spread_factor = 1.2 + (base_contagion * 0.5) + (banking_stress * 0.3)

            # Recovery factor (lower during high stress)
            recovery_factor = 0.95 - (base_contagion * 0.2)

            # Next timestep
            next_value = current * spread_factor * recovery_factor
            next_value = min(1.0, next_value)  # Cap at 1.0

            contagion_history.append(next_value)

        return {
            'initial_shock': initial_shock,
            'final_impact': contagion_history[-1],
            'peak_impact': max(contagion_history),
            'contagion_history': contagion_history,
            'economic_threat': base_contagion,
            'banking_stress': banking_stress,
            'amplification_factor': contagion_history[-1] / initial_shock
        }

    def get_all_indicators(self) -> Dict[str, Any]:
        """Get all available economic indicators"""
        return {
            'economic_threat': self.get_economic_threat_score(),
            'stl_regional': self.get_stl_regional_data(),
            'banking_network': self.get_banking_network_state(),
            'timestamp': datetime.now().isoformat()
        }


class FREDEnhancedPrimalLogic:
    """
    PRIMAL Logic enhancement with real-time FRED economic data
    """

    def __init__(self, fred_api_key: Optional[str] = None):
        self.fred = FREDClient(fred_api_key)
        self.economic_weight = 0.3  # Weight of economic factors in threat assessment

    def calculate_enhanced_threat(self, network_threat: float) -> Dict[str, Any]:
        """
        Calculate enhanced threat level combining network conditions and economic data

        Args:
            network_threat: Base network threat level (0-1)

        Returns:
            Enhanced threat assessment
        """
        economic_data = self.fred.get_economic_threat_score()
        economic_threat = economic_data['composite_threat']

        # Combine network and economic threats with PRIMAL Logic weighting
        # Golden ratio weighting
        phi = 1.618033988749
        network_weight = 1.0 / phi  # ~0.618
        economic_weight = 1.0 - network_weight  # ~0.382

        combined_threat = (
            network_threat * network_weight +
            economic_threat * economic_weight
        )

        return {
            'combined_threat': combined_threat,
            'network_threat': network_threat,
            'economic_threat': economic_threat,
            'economic_indicators': economic_data['indicators'],
            'threat_classification': self._classify_combined_threat(combined_threat),
            'timestamp': datetime.now().isoformat()
        }

    def _classify_combined_threat(self, threat: float) -> str:
        """Classify combined threat level"""
        if threat < 0.25:
            return "MINIMAL"
        elif threat < 0.5:
            return "MODERATE"
        elif threat < 0.75:
            return "ELEVATED"
        else:
            return "SEVERE"

    def adjust_network_parameters(self, base_failure_rate: float,
                                  base_recovery_rate: float) -> Dict[str, float]:
        """
        Adjust network simulation parameters based on economic conditions

        Args:
            base_failure_rate: Base network failure rate
            base_recovery_rate: Base network recovery rate

        Returns:
            Adjusted parameters
        """
        economic_data = self.fred.get_economic_threat_score()
        economic_stress = economic_data['composite_threat']

        # Higher economic stress = higher failure rate, lower recovery rate
        adjusted_failure = base_failure_rate * (1.0 + economic_stress)
        adjusted_recovery = base_recovery_rate * (1.0 - economic_stress * 0.5)

        return {
            'failure_rate': adjusted_failure,
            'recovery_rate': adjusted_recovery,
            'economic_stress': economic_stress,
            'stress_level': economic_data['threat_level']
        }


# Example usage and testing
if __name__ == "__main__":
    print("🏦 Federal Reserve Economic Data (FRED) Integration Test")
    print("=" * 80)

    # Check for API key
    api_key = os.getenv('FRED_API_KEY')
    if not api_key:
        print("⚠️  FRED_API_KEY not set!")
        print("📝 Register for free API key at: https://fred.stlouisfed.org/docs/api/api_key.html")
        print("💡 Set with: export FRED_API_KEY='your_key_here'")
        exit(1)

    try:
        # Initialize client
        fred = FREDClient(api_key)

        # Test 1: Economic Threat Score
        print("\n📊 Economic Threat Assessment:")
        print("-" * 80)
        threat_data = fred.get_economic_threat_score()
        print(f"Composite Threat Score: {threat_data['composite_threat']:.3f}")
        print(f"Threat Level: {threat_data['threat_level']}")
        print("\nIndicators:")
        for name, value in threat_data['indicators'].items():
            print(f"  - {name}: {value:.2f}")

        # Test 2: St. Louis Regional Data
        print("\n🏙️  St. Louis Regional Data:")
        print("-" * 80)
        stl_data = fred.get_stl_regional_data()
        for name, value in stl_data['data'].items():
            print(f"  - {name}: {value:.2f}")

        # Test 3: Banking Network State
        print("\n🏦 Banking Network State:")
        print("-" * 80)
        banking = fred.get_banking_network_state()
        print(f"Network Stress: {banking['network_stress']:.3f}")
        print("Indicators:")
        for name, value in banking['banking_indicators'].items():
            print(f"  - {name}: {value:.2f}")

        # Test 4: Financial Contagion Simulation
        print("\n🔥 Financial Contagion Simulation:")
        print("-" * 80)
        contagion = fred.simulate_financial_contagion(initial_shock=0.5)
        print(f"Initial Shock: {contagion['initial_shock']:.3f}")
        print(f"Final Impact: {contagion['final_impact']:.3f}")
        print(f"Peak Impact: {contagion['peak_impact']:.3f}")
        print(f"Amplification Factor: {contagion['amplification_factor']:.2f}x")

        # Test 5: PRIMAL Logic Enhancement
        print("\n🧠 PRIMAL Logic + FRED Enhancement:")
        print("-" * 80)
        primal_fred = FREDEnhancedPrimalLogic(api_key)
        enhanced = primal_fred.calculate_enhanced_threat(network_threat=0.4)
        print(f"Network Threat: {enhanced['network_threat']:.3f}")
        print(f"Economic Threat: {enhanced['economic_threat']:.3f}")
        print(f"Combined Threat: {enhanced['combined_threat']:.3f}")
        print(f"Classification: {enhanced['threat_classification']}")

        # Test 6: Network Parameter Adjustment
        print("\n⚙️  Network Parameter Adjustment:")
        print("-" * 80)
        params = primal_fred.adjust_network_parameters(
            base_failure_rate=0.01,
            base_recovery_rate=0.15
        )
        print(f"Adjusted Failure Rate: {params['failure_rate']:.4f}")
        print(f"Adjusted Recovery Rate: {params['recovery_rate']:.4f}")
        print(f"Economic Stress: {params['economic_stress']:.3f}")
        print(f"Stress Level: {params['stress_level']}")

        print("\n✅ All FRED integration tests passed!")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
