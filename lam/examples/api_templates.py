#!/usr/bin/env python3
"""
Real API Integration Templates for LAM
Examples showing how to integrate actual third-party services
"""
import requests
from typing import Dict, Any, List, Optional


class OpenAIIntegration:
    """
    Template for integrating OpenAI API
    https://platform.openai.com/docs/api-reference
    """

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.base_url = "https://api.openai.com/v1"

    def chat_completion(self, prompt: str, model: str = "gpt-4") -> Dict[str, Any]:
        """Generate chat completion"""
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }

        data = {
            "model": model,
            "messages": [{"role": "user", "content": prompt}]
        }

        response = requests.post(
            f"{self.base_url}/chat/completions",
            headers=headers,
            json=data
        )

        return response.json()


class AmadeusFlightAPI:
    """
    Template for Amadeus Flight Search API
    https://developers.amadeus.com/self-service/category/air
    """

    def __init__(self, api_key: str, api_secret: str):
        self.api_key = api_key
        self.api_secret = api_secret
        self.base_url = "https://test.api.amadeus.com/v2"
        self.access_token = None

    def get_access_token(self) -> str:
        """Get OAuth2 access token"""
        response = requests.post(
            "https://test.api.amadeus.com/v1/security/oauth2/token",
            data={
                "grant_type": "client_credentials",
                "client_id": self.api_key,
                "client_secret": self.api_secret
            }
        )

        if response.status_code == 200:
            self.access_token = response.json()["access_token"]
            return self.access_token
        else:
            raise Exception(f"Failed to get token: {response.text}")

    def search_flights(self, origin: str, destination: str,
                      departure_date: str, adults: int = 1) -> Dict[str, Any]:
        """
        Search for flights

        Args:
            origin: IATA code (e.g., "NYC")
            destination: IATA code (e.g., "PAR")
            departure_date: YYYY-MM-DD
            adults: Number of adult passengers
        """
        if not self.access_token:
            self.get_access_token()

        headers = {
            "Authorization": f"Bearer {self.access_token}"
        }

        params = {
            "originLocationCode": origin,
            "destinationLocationCode": destination,
            "departureDate": departure_date,
            "adults": adults,
            "max": 10
        }

        response = requests.get(
            f"{self.base_url}/shopping/flight-offers",
            headers=headers,
            params=params
        )

        return response.json()


class GoogleMapsAPI:
    """
    Template for Google Maps API
    https://developers.google.com/maps/documentation
    """

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.base_url = "https://maps.googleapis.com/maps/api"

    def geocode(self, address: str) -> Dict[str, Any]:
        """Convert address to coordinates"""
        params = {
            "address": address,
            "key": self.api_key
        }

        response = requests.get(
            f"{self.base_url}/geocode/json",
            params=params
        )

        return response.json()

    def get_directions(self, origin: str, destination: str,
                      mode: str = "driving") -> Dict[str, Any]:
        """Get directions between two points"""
        params = {
            "origin": origin,
            "destination": destination,
            "mode": mode,
            "key": self.api_key
        }

        response = requests.get(
            f"{self.base_url}/directions/json",
            params=params
        )

        return response.json()


class OpenTableAPI:
    """
    Template for OpenTable (or similar) restaurant reservation API
    Note: OpenTable requires partnership; this is a template structure
    """

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.base_url = "https://platform.opentable.com/v2"

    def search_restaurants(self, location: str, date: str, time: str,
                          party_size: int = 2) -> Dict[str, Any]:
        """Search for available restaurants"""
        headers = {
            "Authorization": f"Bearer {self.api_key}"
        }

        params = {
            "location": location,
            "date": date,
            "time": time,
            "party_size": party_size
        }

        # Note: Actual endpoint varies by API provider
        response = requests.get(
            f"{self.base_url}/restaurants/search",
            headers=headers,
            params=params
        )

        return response.json()

    def make_reservation(self, restaurant_id: str, date: str, time: str,
                        party_size: int, customer_info: Dict) -> Dict[str, Any]:
        """Make a restaurant reservation"""
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json"
        }

        data = {
            "restaurant_id": restaurant_id,
            "date": date,
            "time": time,
            "party_size": party_size,
            "customer": customer_info
        }

        response = requests.post(
            f"{self.base_url}/reservations",
            headers=headers,
            json=data
        )

        return response.json()


class DoorDashAPI:
    """
    Template for DoorDash Drive API (for delivery)
    https://developer.doordash.com/en-US/docs/drive/reference/
    """

    def __init__(self, developer_id: str, key_id: str, signing_secret: str):
        self.developer_id = developer_id
        self.key_id = key_id
        self.signing_secret = signing_secret
        self.base_url = "https://openapi.doordash.com"

    def create_delivery(self, pickup_address: str, dropoff_address: str,
                       order_value: float, items: List[Dict]) -> Dict[str, Any]:
        """Create a delivery request"""
        # Note: Requires proper HMAC signing - see DoorDash docs
        headers = {
            "Content-Type": "application/json",
            "Developer-Id": self.developer_id,
            # Add proper authentication headers
        }

        data = {
            "external_delivery_id": f"delivery_{int(datetime.now().timestamp())}",
            "pickup_address": pickup_address,
            "dropoff_address": dropoff_address,
            "order_value": order_value * 100,  # In cents
            "items": items
        }

        response = requests.post(
            f"{self.base_url}/drive/v2/deliveries",
            headers=headers,
            json=data
        )

        return response.json()


class StripeAPI:
    """
    Template for Stripe payment API
    https://stripe.com/docs/api
    """

    def __init__(self, api_key: str):
        self.api_key = api_key
        self.base_url = "https://api.stripe.com/v1"

    def create_payment_intent(self, amount: int, currency: str = "usd") -> Dict[str, Any]:
        """Create a payment intent"""
        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/x-www-form-urlencoded"
        }

        data = {
            "amount": amount,  # In cents
            "currency": currency
        }

        response = requests.post(
            f"{self.base_url}/payment_intents",
            headers=headers,
            data=data
        )

        return response.json()

    def cancel_subscription(self, subscription_id: str) -> Dict[str, Any]:
        """Cancel a subscription"""
        headers = {
            "Authorization": f"Bearer {self.api_key}"
        }

        response = requests.delete(
            f"{self.base_url}/subscriptions/{subscription_id}",
            headers=headers
        )

        return response.json()


# Example usage template
def example_usage():
    """
    Example of how to integrate these APIs into LAM action executors
    """

    # Example 1: Flight search with Amadeus
    def search_flights_real(origin, destination, date):
        # Get credentials from LAM setup wizard
        from lam.wizards.setup_wizard import SetupWizard
        wizard = SetupWizard()
        creds = wizard.load_credentials("travel")

        if "amadeus_key" in creds and "amadeus_secret" in creds:
            api = AmadeusFlightAPI(creds["amadeus_key"], creds["amadeus_secret"])
            results = api.search_flights(origin, destination, date)
            return results
        else:
            return {"error": "Amadeus credentials not configured"}

    # Example 2: Restaurant reservation
    def make_reservation_real(location, date, time, party_size):
        wizard = SetupWizard()
        creds = wizard.load_credentials("reservations")

        if "opentable_key" in creds:
            api = OpenTableAPI(creds["opentable_key"])
            # First search
            restaurants = api.search_restaurants(location, date, time, party_size)
            # Then make reservation at first available
            # ... implementation
            return restaurants
        else:
            return {"error": "OpenTable credentials not configured"}

    # Example 3: Food delivery
    def order_food_real(restaurant, items, address):
        wizard = SetupWizard()
        creds = wizard.load_credentials("food_delivery")

        if "doordash_credentials" in creds:
            # Create DoorDash delivery
            # ... implementation
            pass

    return {
        "flight_search": search_flights_real,
        "reservations": make_reservation_real,
        "food_delivery": order_food_real
    }


# Integration guide
INTEGRATION_GUIDE = """
# How to Integrate Real APIs into LAM

## Step 1: Get API Credentials

1. OpenAI: https://platform.openai.com/api-keys
2. Amadeus (flights): https://developers.amadeus.com/get-started/get-started-with-self-service-apis-335
3. Google Maps: https://developers.google.com/maps/documentation/javascript/get-api-key
4. OpenTable: https://platform.opentable.com/ (requires partnership)
5. DoorDash: https://developer.doordash.com/
6. Stripe: https://stripe.com/docs/keys

## Step 2: Configure in LAM

```bash
python lam/lam_main.py wizard
```

Add credentials for each service you want to use.

## Step 3: Update Action Executors

Replace simulated actions in `lam/actions/action_executors.py` with real API calls:

```python
from lam.examples.api_templates import AmadeusFlightAPI

class TripPlanner(ActionExecutor):
    def _search_flights(self, destination, departure, return_date):
        # Load credentials
        wizard = SetupWizard()
        creds = wizard.load_credentials("travel")

        # Use real API
        api = AmadeusFlightAPI(creds["key"], creds["secret"])
        return api.search_flights("NYC", destination, departure)
```

## Step 4: Handle API Errors

Always wrap API calls in try-except blocks:

```python
try:
    result = api.search_flights(...)
    return {"success": True, "data": result}
except Exception as e:
    return {"success": False, "error": str(e)}
```

## Step 5: Rate Limiting

Implement rate limiting to avoid API quota issues:

```python
import time
from functools import wraps

def rate_limit(calls_per_minute=10):
    def decorator(func):
        last_called = [0.0]
        @wraps(func)
        def wrapper(*args, **kwargs):
            elapsed = time.time() - last_called[0]
            if elapsed < 60.0 / calls_per_minute:
                time.sleep((60.0 / calls_per_minute) - elapsed)
            result = func(*args, **kwargs)
            last_called[0] = time.time()
            return result
        return wrapper
    return decorator

@rate_limit(calls_per_minute=30)
def api_call():
    # Your API call
    pass
```

## Security Best Practices

1. Never hardcode API keys
2. Store credentials encrypted (LAM setup wizard does this)
3. Use environment variables for production
4. Rotate keys regularly
5. Monitor API usage
6. Implement request signing when available
7. Use HTTPS only
"""


if __name__ == "__main__":
    print(INTEGRATION_GUIDE)
