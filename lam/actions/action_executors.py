#!/usr/bin/env python3
"""
LAM Action Executors - Real-world task execution
Implements trip planning, reservations, subscriptions, etc.
"""
import json
import requests
from typing import Dict, Any, List, Optional
from datetime import datetime, timedelta
from pathlib import Path
import sys

# Add parent directory to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))


class ActionExecutor:
    """Base class for action executors"""
    def __init__(self, credentials: Optional[Dict[str, str]] = None):
        self.credentials = credentials or {}
        self.execution_history = []

    def execute(self, *args, **kwargs) -> Dict[str, Any]:
        """Execute the action - to be implemented by subclasses"""
        raise NotImplementedError

    def _record_execution(self, action_type: str, params: Dict[str, Any], result: Dict[str, Any]):
        """Record action execution"""
        self.execution_history.append({
            "timestamp": datetime.now().isoformat(),
            "action_type": action_type,
            "params": params,
            "result": result
        })


class TripPlanner(ActionExecutor):
    """
    Trip planning and booking
    """
    def plan_trip(self, destination: str, departure_date: str, return_date: str,
                  budget: Optional[float] = None, preferences: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Plan a trip with optimal routing and pricing
        """
        params = {
            "destination": destination,
            "departure_date": departure_date,
            "return_date": return_date,
            "budget": budget,
            "preferences": preferences or {}
        }

        # Calculate trip duration
        try:
            dep = datetime.fromisoformat(departure_date)
            ret = datetime.fromisoformat(return_date)
            duration = (ret - dep).days
        except:
            duration = "unknown"

        result = {
            "success": True,
            "trip_plan": {
                "destination": destination,
                "dates": {
                    "departure": departure_date,
                    "return": return_date,
                    "duration_days": duration
                },
                "budget_analysis": self._analyze_budget(destination, duration, budget),
                "flight_options": self._search_flights(destination, departure_date, return_date),
                "accommodation": self._search_accommodation(destination, departure_date, return_date, budget),
                "itinerary": self._generate_itinerary(destination, duration, preferences)
            },
            "estimated_total_cost": self._estimate_total_cost(destination, duration, budget)
        }

        self._record_execution("trip_planning", params, result)
        return result

    def _analyze_budget(self, destination: str, duration: int, budget: Optional[float]) -> Dict[str, Any]:
        """Analyze budget for trip"""
        # Simplified budget analysis
        daily_cost_estimates = {
            "budget": 50,
            "moderate": 150,
            "luxury": 500
        }

        if budget:
            daily_budget = budget / duration if isinstance(duration, int) and duration > 0 else budget
            category = "luxury" if daily_budget > 300 else "moderate" if daily_budget > 100 else "budget"
        else:
            category = "moderate"
            daily_budget = daily_cost_estimates[category]

        return {
            "budget_category": category,
            "daily_budget": daily_budget,
            "total_budget": budget or daily_budget * (duration if isinstance(duration, int) else 7),
            "recommendations": [
                f"Target {category} accommodations",
                f"Allocate ${daily_budget:.2f} per day",
                "Book flights at least 3 weeks in advance for best prices"
            ]
        }

    def _search_flights(self, destination: str, departure: str, return_date: str) -> List[Dict[str, Any]]:
        """Search for flight options (simulated)"""
        # In production, this would call real flight APIs
        return [
            {
                "airline": "Major Carrier",
                "departure_time": f"{departure}T08:00:00",
                "arrival_time": f"{departure}T12:00:00",
                "return_time": f"{return_date}T14:00:00",
                "price": 450.00,
                "class": "economy",
                "stops": 0
            },
            {
                "airline": "Budget Airline",
                "departure_time": f"{departure}T06:00:00",
                "arrival_time": f"{departure}T14:00:00",
                "return_time": f"{return_date}T16:00:00",
                "price": 280.00,
                "class": "economy",
                "stops": 1
            }
        ]

    def _search_accommodation(self, destination: str, check_in: str, check_out: str,
                            budget: Optional[float]) -> List[Dict[str, Any]]:
        """Search for accommodation options (simulated)"""
        return [
            {
                "name": f"{destination} Grand Hotel",
                "type": "hotel",
                "price_per_night": 180.00,
                "rating": 4.5,
                "amenities": ["wifi", "pool", "gym", "breakfast"]
            },
            {
                "name": f"Budget Inn {destination}",
                "type": "hotel",
                "price_per_night": 75.00,
                "rating": 3.5,
                "amenities": ["wifi", "parking"]
            }
        ]

    def _generate_itinerary(self, destination: str, duration: int,
                          preferences: Optional[Dict[str, Any]]) -> List[Dict[str, str]]:
        """Generate suggested itinerary"""
        if not isinstance(duration, int):
            duration = 3

        itinerary = []
        activities = ["City tour", "Museum visit", "Local cuisine experience", "Nature excursion", "Cultural site"]

        for day in range(min(duration, 5)):
            itinerary.append({
                "day": day + 1,
                "activity": activities[day % len(activities)],
                "time": "Full day",
                "notes": f"Explore {destination}"
            })

        return itinerary

    def _estimate_total_cost(self, destination: str, duration: int, budget: Optional[float]) -> Dict[str, float]:
        """Estimate total trip cost"""
        if not isinstance(duration, int):
            duration = 3

        return {
            "flights": 450.00,
            "accommodation": 150.00 * duration,
            "meals": 50.00 * duration,
            "activities": 100.00 * duration,
            "transportation": 30.00 * duration,
            "total": 450.00 + (330.00 * duration)
        }


class ReservationManager(ActionExecutor):
    """
    Make and manage reservations (restaurants, events, etc.)
    """
    def make_reservation(self, venue_type: str, venue_name: str, date: str, time: str,
                        party_size: int, special_requests: Optional[str] = None) -> Dict[str, Any]:
        """
        Make a reservation
        """
        params = {
            "venue_type": venue_type,
            "venue_name": venue_name,
            "date": date,
            "time": time,
            "party_size": party_size,
            "special_requests": special_requests
        }

        # Simulate reservation
        confirmation_code = f"RES{datetime.now().strftime('%Y%m%d%H%M%S')}"

        result = {
            "success": True,
            "confirmation": {
                "code": confirmation_code,
                "venue": venue_name,
                "type": venue_type,
                "date": date,
                "time": time,
                "party_size": party_size,
                "special_requests": special_requests,
                "status": "confirmed"
            },
            "instructions": [
                f"Arrive 10 minutes early at {time}",
                f"Reference confirmation code: {confirmation_code}",
                "Contact venue for changes or cancellations"
            ]
        }

        self._record_execution("make_reservation", params, result)
        return result

    def cancel_reservation(self, confirmation_code: str, reason: Optional[str] = None) -> Dict[str, Any]:
        """Cancel a reservation"""
        result = {
            "success": True,
            "confirmation_code": confirmation_code,
            "status": "cancelled",
            "reason": reason,
            "refund_status": "processing",
            "message": "Reservation cancelled successfully"
        }

        self._record_execution("cancel_reservation", {"code": confirmation_code, "reason": reason}, result)
        return result


class FoodOrderer(ActionExecutor):
    """
    Order food (pizza, delivery, etc.)
    """
    def order_food(self, restaurant: str, items: List[Dict[str, Any]],
                   delivery_address: str, special_instructions: Optional[str] = None) -> Dict[str, Any]:
        """
        Order food for delivery
        """
        params = {
            "restaurant": restaurant,
            "items": items,
            "delivery_address": delivery_address,
            "special_instructions": special_instructions
        }

        # Calculate total
        total = sum(item.get('price', 0) * item.get('quantity', 1) for item in items)
        delivery_fee = 5.00
        tax = total * 0.08
        final_total = total + delivery_fee + tax

        order_id = f"ORD{datetime.now().strftime('%Y%m%d%H%M%S')}"

        result = {
            "success": True,
            "order": {
                "order_id": order_id,
                "restaurant": restaurant,
                "items": items,
                "delivery_address": delivery_address,
                "special_instructions": special_instructions,
                "pricing": {
                    "subtotal": total,
                    "delivery_fee": delivery_fee,
                    "tax": tax,
                    "total": final_total
                },
                "estimated_delivery": self._estimate_delivery_time(),
                "status": "confirmed"
            }
        }

        self._record_execution("order_food", params, result)
        return result

    def _estimate_delivery_time(self) -> str:
        """Estimate delivery time"""
        delivery_time = datetime.now() + timedelta(minutes=45)
        return delivery_time.strftime("%H:%M")


class SubscriptionManager(ActionExecutor):
    """
    Manage subscriptions (cancel, modify, etc.)
    """
    def cancel_subscription(self, service_name: str, subscription_id: str,
                           reason: Optional[str] = None, immediate: bool = False) -> Dict[str, Any]:
        """
        Cancel a subscription
        """
        params = {
            "service_name": service_name,
            "subscription_id": subscription_id,
            "reason": reason,
            "immediate": immediate
        }

        cancellation_date = datetime.now() if immediate else datetime.now() + timedelta(days=30)

        result = {
            "success": True,
            "cancellation": {
                "service": service_name,
                "subscription_id": subscription_id,
                "status": "cancelled" if immediate else "pending_cancellation",
                "effective_date": cancellation_date.strftime("%Y-%m-%d"),
                "reason": reason,
                "refund_eligible": immediate,
                "access_until": cancellation_date.strftime("%Y-%m-%d")
            },
            "message": f"Subscription will be cancelled on {cancellation_date.strftime('%Y-%m-%d')}"
        }

        self._record_execution("cancel_subscription", params, result)
        return result

    def modify_subscription(self, service_name: str, subscription_id: str,
                           new_plan: str, effective_date: Optional[str] = None) -> Dict[str, Any]:
        """
        Modify subscription plan
        """
        if not effective_date:
            effective_date = datetime.now().strftime("%Y-%m-%d")

        result = {
            "success": True,
            "modification": {
                "service": service_name,
                "subscription_id": subscription_id,
                "old_plan": "current",
                "new_plan": new_plan,
                "effective_date": effective_date,
                "status": "confirmed"
            },
            "message": f"Subscription updated to {new_plan} plan"
        }

        self._record_execution("modify_subscription",
                             {"service": service_name, "id": subscription_id, "plan": new_plan},
                             result)
        return result

    def list_subscriptions(self) -> List[Dict[str, Any]]:
        """List all active subscriptions (simulated)"""
        return [
            {
                "service": "Streaming Service A",
                "subscription_id": "SUB001",
                "plan": "Premium",
                "monthly_cost": 15.99,
                "renewal_date": "2025-12-01",
                "status": "active"
            },
            {
                "service": "Cloud Storage",
                "subscription_id": "SUB002",
                "plan": "Pro",
                "monthly_cost": 9.99,
                "renewal_date": "2025-11-25",
                "status": "active"
            }
        ]


class ActionOrchestrator:
    """
    Orchestrates multiple action executors
    """
    def __init__(self, credentials: Optional[Dict[str, Dict[str, str]]] = None):
        self.credentials = credentials or {}

        # Initialize executors
        self.trip_planner = TripPlanner(self.credentials.get('travel'))
        self.reservation_manager = ReservationManager(self.credentials.get('reservations'))
        self.food_orderer = FoodOrderer(self.credentials.get('food_delivery'))
        self.subscription_manager = SubscriptionManager(self.credentials.get('subscriptions'))

    def execute_action(self, action_type: str, **kwargs) -> Dict[str, Any]:
        """
        Execute an action by type
        """
        action_map = {
            "plan_trip": self.trip_planner.plan_trip,
            "make_reservation": self.reservation_manager.make_reservation,
            "cancel_reservation": self.reservation_manager.cancel_reservation,
            "order_food": self.food_orderer.order_food,
            "cancel_subscription": self.subscription_manager.cancel_subscription,
            "modify_subscription": self.subscription_manager.modify_subscription,
            "list_subscriptions": self.subscription_manager.list_subscriptions
        }

        if action_type not in action_map:
            return {"success": False, "error": f"Unknown action type: {action_type}"}

        try:
            return action_map[action_type](**kwargs)
        except Exception as e:
            return {"success": False, "error": str(e)}


def main():
    """Test action executors"""
    orchestrator = ActionOrchestrator()

    print("=== LAM Action Executors Demo ===\n")

    # Trip planning
    print("1. Planning a trip...")
    trip = orchestrator.execute_action(
        "plan_trip",
        destination="Paris",
        departure_date="2025-12-15",
        return_date="2025-12-22",
        budget=2000.00
    )
    print(json.dumps(trip, indent=2, default=str))

    print("\n2. Making a reservation...")
    reservation = orchestrator.execute_action(
        "make_reservation",
        venue_type="restaurant",
        venue_name="The French Bistro",
        date="2025-12-16",
        time="19:00",
        party_size=2,
        special_requests="Window seat preferred"
    )
    print(json.dumps(reservation, indent=2))

    print("\n3. Ordering pizza...")
    order = orchestrator.execute_action(
        "order_food",
        restaurant="Joe's Pizza",
        items=[
            {"name": "Large Pepperoni", "price": 18.99, "quantity": 1},
            {"name": "Caesar Salad", "price": 8.99, "quantity": 1},
            {"name": "Garlic Bread", "price": 5.99, "quantity": 2}
        ],
        delivery_address="123 Main St, Apt 4B",
        special_instructions="Ring doorbell twice"
    )
    print(json.dumps(order, indent=2))

    print("\n4. Listing subscriptions...")
    subs = orchestrator.execute_action("list_subscriptions")
    print(json.dumps(subs, indent=2))


if __name__ == "__main__":
    main()
