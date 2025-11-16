#!/usr/bin/env python3
"""
Test suite for LAM action executors
"""
import unittest
import sys
from pathlib import Path

# Add parent directory to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from actions.action_executors import (
    TripPlanner, ReservationManager, FoodOrderer,
    SubscriptionManager, ActionOrchestrator
)


class TestTripPlanner(unittest.TestCase):
    """Test trip planning functionality"""

    def setUp(self):
        self.planner = TripPlanner()

    def test_plan_trip(self):
        """Test basic trip planning"""
        result = self.planner.plan_trip(
            destination="Paris",
            departure_date="2025-12-15",
            return_date="2025-12-22",
            budget=2000.00
        )

        self.assertTrue(result['success'])
        self.assertIn('trip_plan', result)
        self.assertEqual(result['trip_plan']['destination'], "Paris")

    def test_budget_analysis(self):
        """Test budget analysis"""
        result = self.planner.plan_trip(
            destination="Paris",
            departure_date="2025-12-15",
            return_date="2025-12-22",
            budget=1500.00
        )

        budget_analysis = result['trip_plan']['budget_analysis']
        self.assertIn('budget_category', budget_analysis)
        self.assertIn('daily_budget', budget_analysis)

    def test_flight_search(self):
        """Test flight search"""
        result = self.planner.plan_trip(
            destination="Tokyo",
            departure_date="2026-01-01",
            return_date="2026-01-10"
        )

        flight_options = result['trip_plan']['flight_options']
        self.assertIsInstance(flight_options, list)
        self.assertGreater(len(flight_options), 0)

    def test_execution_history(self):
        """Test action recording"""
        self.planner.plan_trip("Paris", "2025-12-15", "2025-12-22")
        self.assertEqual(len(self.planner.execution_history), 1)


class TestReservationManager(unittest.TestCase):
    """Test reservation management"""

    def setUp(self):
        self.manager = ReservationManager()

    def test_make_reservation(self):
        """Test making a reservation"""
        result = self.manager.make_reservation(
            venue_type="restaurant",
            venue_name="Test Restaurant",
            date="2025-12-20",
            time="19:00",
            party_size=2
        )

        self.assertTrue(result['success'])
        self.assertIn('confirmation', result)
        self.assertIn('code', result['confirmation'])

    def test_confirmation_code_format(self):
        """Test confirmation code is generated"""
        result = self.manager.make_reservation(
            venue_type="restaurant",
            venue_name="Test",
            date="2025-12-20",
            time="19:00",
            party_size=2
        )

        code = result['confirmation']['code']
        self.assertTrue(code.startswith('RES'))

    def test_cancel_reservation(self):
        """Test canceling a reservation"""
        # First make a reservation
        make_result = self.manager.make_reservation(
            venue_type="restaurant",
            venue_name="Test",
            date="2025-12-20",
            time="19:00",
            party_size=2
        )

        code = make_result['confirmation']['code']

        # Then cancel it
        cancel_result = self.manager.cancel_reservation(code, reason="Plans changed")

        self.assertTrue(cancel_result['success'])
        self.assertEqual(cancel_result['status'], 'cancelled')


class TestFoodOrderer(unittest.TestCase):
    """Test food ordering"""

    def setUp(self):
        self.orderer = FoodOrderer()

    def test_order_food(self):
        """Test ordering food"""
        result = self.orderer.order_food(
            restaurant="Test Pizza",
            items=[
                {"name": "Large Pepperoni", "price": 18.99, "quantity": 1},
                {"name": "Garlic Bread", "price": 5.99, "quantity": 1}
            ],
            delivery_address="123 Main St"
        )

        self.assertTrue(result['success'])
        self.assertIn('order', result)
        self.assertIn('order_id', result['order'])

    def test_price_calculation(self):
        """Test pricing calculation"""
        result = self.orderer.order_food(
            restaurant="Test",
            items=[
                {"name": "Pizza", "price": 20.00, "quantity": 1}
            ],
            delivery_address="123 Main St"
        )

        pricing = result['order']['pricing']
        self.assertEqual(pricing['subtotal'], 20.00)
        self.assertEqual(pricing['delivery_fee'], 5.00)
        self.assertGreater(pricing['tax'], 0)
        self.assertGreater(pricing['total'], 20.00)

    def test_delivery_time_estimation(self):
        """Test delivery time is estimated"""
        result = self.orderer.order_food(
            restaurant="Test",
            items=[{"name": "Pizza", "price": 15.00, "quantity": 1}],
            delivery_address="123 Main St"
        )

        self.assertIn('estimated_delivery', result['order'])


class TestSubscriptionManager(unittest.TestCase):
    """Test subscription management"""

    def setUp(self):
        self.manager = SubscriptionManager()

    def test_cancel_subscription(self):
        """Test canceling a subscription"""
        result = self.manager.cancel_subscription(
            service_name="Test Service",
            subscription_id="SUB123",
            reason="No longer needed"
        )

        self.assertTrue(result['success'])
        self.assertIn('cancellation', result)

    def test_immediate_cancellation(self):
        """Test immediate cancellation"""
        result = self.manager.cancel_subscription(
            service_name="Test",
            subscription_id="SUB123",
            immediate=True
        )

        self.assertEqual(result['cancellation']['status'], 'cancelled')

    def test_pending_cancellation(self):
        """Test pending cancellation"""
        result = self.manager.cancel_subscription(
            service_name="Test",
            subscription_id="SUB123",
            immediate=False
        )

        self.assertEqual(result['cancellation']['status'], 'pending_cancellation')

    def test_modify_subscription(self):
        """Test modifying subscription"""
        result = self.manager.modify_subscription(
            service_name="Test",
            subscription_id="SUB123",
            new_plan="Premium"
        )

        self.assertTrue(result['success'])
        self.assertEqual(result['modification']['new_plan'], "Premium")

    def test_list_subscriptions(self):
        """Test listing subscriptions"""
        subs = self.manager.list_subscriptions()
        self.assertIsInstance(subs, list)


class TestActionOrchestrator(unittest.TestCase):
    """Test action orchestration"""

    def setUp(self):
        self.orchestrator = ActionOrchestrator()

    def test_plan_trip_action(self):
        """Test trip planning through orchestrator"""
        result = self.orchestrator.execute_action(
            "plan_trip",
            destination="Paris",
            departure_date="2025-12-15",
            return_date="2025-12-22"
        )

        self.assertTrue(result['success'])

    def test_make_reservation_action(self):
        """Test reservation through orchestrator"""
        result = self.orchestrator.execute_action(
            "make_reservation",
            venue_type="restaurant",
            venue_name="Test",
            date="2025-12-20",
            time="19:00",
            party_size=2
        )

        self.assertTrue(result['success'])

    def test_order_food_action(self):
        """Test food ordering through orchestrator"""
        result = self.orchestrator.execute_action(
            "order_food",
            restaurant="Test",
            items=[{"name": "Pizza", "price": 15.00, "quantity": 1}],
            delivery_address="123 Main St"
        )

        self.assertTrue(result['success'])

    def test_unknown_action(self):
        """Test unknown action type"""
        result = self.orchestrator.execute_action("unknown_action")

        self.assertFalse(result['success'])
        self.assertIn('error', result)


def run_tests():
    """Run all action tests"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    suite.addTests(loader.loadTestsFromTestCase(TestTripPlanner))
    suite.addTests(loader.loadTestsFromTestCase(TestReservationManager))
    suite.addTests(loader.loadTestsFromTestCase(TestFoodOrderer))
    suite.addTests(loader.loadTestsFromTestCase(TestSubscriptionManager))
    suite.addTests(loader.loadTestsFromTestCase(TestActionOrchestrator))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()


if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)
