#!/usr/bin/env python3
"""
Real API Connectors - Production-ready integrations
Uses credentials from setup wizard for actual third-party API calls
"""
import sys
from pathlib import Path
from typing import Dict, Any, Optional, List

sys.path.insert(0, str(Path(__file__).parent.parent))

from wizards.setup_wizard import SetupWizard
from examples.api_templates import (
    AmadeusFlightAPI,
    GoogleMapsAPI,
    OpenAIIntegration,
    StripeAPI
)


class RealAPIManager:
    """
    Manages real API connections using stored credentials
    """

    def __init__(self):
        self.wizard = SetupWizard()
        self.credentials = self.wizard.load_credentials()

        # Initialize API clients
        self.amadeus = None
        self.google_maps = None
        self.openai = None
        self.stripe = None

        self._initialize_clients()

    def _initialize_clients(self):
        """Initialize API clients if credentials are available"""

        # Amadeus (flights)
        if "travel" in self.credentials:
            travel_creds = self.credentials["travel"]
            if "amadeus_key" in travel_creds and "amadeus_secret" in travel_creds:
                try:
                    self.amadeus = AmadeusFlightAPI(
                        travel_creds["amadeus_key"],
                        travel_creds["amadeus_secret"]
                    )
                    print("✓ Amadeus API initialized")
                except Exception as e:
                    print(f"⚠ Amadeus initialization failed: {e}")

        # Google Maps
        if "api_server" in self.credentials:
            api_creds = self.credentials["api_server"]
            if "google_maps_key" in api_creds:
                try:
                    self.google_maps = GoogleMapsAPI(api_creds["google_maps_key"])
                    print("✓ Google Maps API initialized")
                except Exception as e:
                    print(f"⚠ Google Maps initialization failed: {e}")

        # OpenAI
        if "api_server" in self.credentials:
            api_creds = self.credentials["api_server"]
            if "openai_key" in api_creds:
                try:
                    self.openai = OpenAIIntegration(api_creds["openai_key"])
                    print("✓ OpenAI API initialized")
                except Exception as e:
                    print(f"⚠ OpenAI initialization failed: {e}")

        # Stripe (payments/subscriptions)
        if "subscription" in self.credentials:
            sub_creds = self.credentials["subscription"]
            if "stripe_key" in sub_creds:
                try:
                    self.stripe = StripeAPI(sub_creds["stripe_key"])
                    print("✓ Stripe API initialized")
                except Exception as e:
                    print(f"⚠ Stripe initialization failed: {e}")

    def search_flights(self, origin: str, destination: str,
                      departure_date: str, adults: int = 1) -> Dict[str, Any]:
        """
        Search for real flights using Amadeus API
        """
        if not self.amadeus:
            return {
                "success": False,
                "error": "Amadeus API not configured",
                "instructions": "Run: python lam/lam_main.py wizard\nAdd Amadeus credentials under 'travel'"
            }

        try:
            results = self.amadeus.search_flights(origin, destination, departure_date, adults)
            return {
                "success": True,
                "provider": "Amadeus",
                "results": results
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "provider": "Amadeus"
            }

    def get_directions(self, origin: str, destination: str,
                      mode: str = "driving") -> Dict[str, Any]:
        """
        Get real directions using Google Maps API
        """
        if not self.google_maps:
            return {
                "success": False,
                "error": "Google Maps API not configured",
                "instructions": "Run: python lam/lam_main.py wizard\nAdd Google Maps key under 'api_server'"
            }

        try:
            results = self.google_maps.get_directions(origin, destination, mode)
            return {
                "success": True,
                "provider": "Google Maps",
                "results": results
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "provider": "Google Maps"
            }

    def ai_assistant(self, prompt: str, model: str = "gpt-4") -> Dict[str, Any]:
        """
        Use OpenAI for AI assistance
        """
        if not self.openai:
            return {
                "success": False,
                "error": "OpenAI API not configured",
                "instructions": "Run: python lam/lam_main.py wizard\nAdd OpenAI key under 'api_server'"
            }

        try:
            results = self.openai.chat_completion(prompt, model)
            return {
                "success": True,
                "provider": "OpenAI",
                "results": results
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "provider": "OpenAI"
            }

    def cancel_subscription(self, subscription_id: str) -> Dict[str, Any]:
        """
        Cancel a real subscription using Stripe
        """
        if not self.stripe:
            return {
                "success": False,
                "error": "Stripe API not configured",
                "instructions": "Run: python lam/lam_main.py wizard\nAdd Stripe key under 'subscription'"
            }

        try:
            results = self.stripe.cancel_subscription(subscription_id)
            return {
                "success": True,
                "provider": "Stripe",
                "results": results
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "provider": "Stripe"
            }

    def get_status(self) -> Dict[str, Any]:
        """Get status of all API connections"""
        return {
            "amadeus": "configured" if self.amadeus else "not configured",
            "google_maps": "configured" if self.google_maps else "not configured",
            "openai": "configured" if self.openai else "not configured",
            "stripe": "configured" if self.stripe else "not configured",
            "total_apis": sum([
                1 if self.amadeus else 0,
                1 if self.google_maps else 0,
                1 if self.openai else 0,
                1 if self.stripe else 0
            ])
        }


if __name__ == "__main__":
    print("=== Real API Connector Test ===\n")

    manager = RealAPIManager()

    print("\n=== API Status ===")
    status = manager.get_status()
    for api, state in status.items():
        print(f"  {api}: {state}")

    print("\n=== Configuration Instructions ===")
    print("To configure real APIs:")
    print("1. Run: python lam/lam_main.py wizard")
    print("2. Select the service type (travel, api_server, subscription)")
    print("3. Enter your API credentials")
    print("\nSupported APIs:")
    print("  • Amadeus (flights): Get key at https://developers.amadeus.com/")
    print("  • Google Maps: Get key at https://console.cloud.google.com/")
    print("  • OpenAI: Get key at https://platform.openai.com/api-keys")
    print("  • Stripe: Get key at https://dashboard.stripe.com/apikeys")
