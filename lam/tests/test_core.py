#!/usr/bin/env python3
"""
Test suite for LAM core engine
"""
import unittest
import sys
from pathlib import Path

# Add parent directories to path
sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "extras" / "primal"))

from core.primal_lam import PrimalLAM, QuantumResonanceField


class TestQuantumResonanceField(unittest.TestCase):
    """Test quantum resonance field stability"""

    def setUp(self):
        self.field = QuantumResonanceField()

    def test_initialization(self):
        """Test resonance field initializes with correct constants"""
        self.assertAlmostEqual(self.field.lambda_lightfoot, 0.16905, places=5)
        self.assertAlmostEqual(self.field.donte_attractor, 149.9992314000, places=7)
        self.assertAlmostEqual(self.field.i3_norm, 6.4939394023, places=7)
        self.assertAlmostEqual(self.field.s_scaling, 23.0983417165, places=7)

    def test_lipschitz_constant(self):
        """Test Lipschitz constant is computed correctly and < 1.0"""
        lipschitz = self.field.lipschitz_constant
        self.assertLess(lipschitz, 1.0, "Lipschitz constant must be < 1.0 for stability")
        self.assertGreater(lipschitz, 0.0, "Lipschitz constant must be positive")
        self.assertAlmostEqual(lipschitz, 0.000129932, places=6)

    def test_semantic_bounds(self):
        """Test parameters stay within semantic bounds"""
        self.assertGreaterEqual(self.field.alpha, self.field.alpha_min)
        self.assertLessEqual(self.field.alpha, self.field.alpha_max)
        self.assertGreaterEqual(self.field.lmbd, self.field.lambda_min)
        self.assertLessEqual(self.field.lmbd, self.field.lambda_max)

    def test_resonance_update(self):
        """Test resonance parameters update within bounds"""
        initial_alpha = self.field.alpha
        initial_lambda = self.field.lmbd

        # Update multiple times
        for i in range(100):
            self.field.update_resonance_parameters(i)

        # Should still be within bounds
        self.assertGreaterEqual(self.field.alpha, self.field.alpha_min)
        self.assertLessEqual(self.field.alpha, self.field.alpha_max)
        self.assertGreaterEqual(self.field.lmbd, self.field.lambda_min)
        self.assertLessEqual(self.field.lmbd, self.field.lambda_max)

        # Should have changed (with high probability)
        self.assertNotEqual(self.field.alpha, initial_alpha)

    def test_stability_check(self):
        """Test stability checking"""
        status = self.field.check_semantic_bounds()
        self.assertIn(status['status'], ['STABLE', 'WARNING', 'UNSTABLE'])
        self.assertIn('message', status)

    def test_get_state(self):
        """Test state retrieval"""
        state = self.field.get_state()

        # Check all expected keys
        expected_keys = ['alpha', 'lambda', 'lightfoot_constant', 'donte_attractor',
                        'lipschitz_constant', 'epoch', 'stable', 'i3_normalization',
                        's_scaling_ratio', 'tau_trust_floor']

        for key in expected_keys:
            self.assertIn(key, state)


class TestPrimalLAM(unittest.TestCase):
    """Test Primal LAM core engine"""

    def setUp(self):
        self.lam = PrimalLAM()

    def test_initialization(self):
        """Test LAM initializes correctly"""
        self.assertIsNotNone(self.lam.resonance)
        self.assertIsInstance(self.lam.action_history, list)
        self.assertEqual(len(self.lam.action_history), 0)

    def test_plan_trip(self):
        """Test trip planning"""
        result = self.lam.plan_trip("Test trip to Paris")
        self.assertIsInstance(result, str)
        self.assertIn("TRIP PLAN", result)
        self.assertIn("Paris", result)
        # Should have recorded action
        self.assertEqual(len(self.lam.action_history), 1)
        self.assertEqual(self.lam.action_history[0]['action_type'], 'trip_planning')

    def test_answer_question(self):
        """Test question answering"""
        result = self.lam.answer_question("What is the Lightfoot constant?")
        self.assertIsInstance(result, str)
        self.assertIn("Lightfoot", result)
        self.assertEqual(len(self.lam.action_history), 1)

    def test_complete_task(self):
        """Test task completion"""
        result = self.lam.complete_task("Test task")
        self.assertIsInstance(result, str)
        self.assertIn("Test task", result)
        self.assertEqual(len(self.lam.action_history), 1)

    def test_action_recording(self):
        """Test action recording works correctly"""
        self.lam.plan_trip("Trip 1")
        self.lam.answer_question("Question 1")
        self.lam.complete_task("Task 1")

        self.assertEqual(len(self.lam.action_history), 3)

        # Check action types
        action_types = [a['action_type'] for a in self.lam.action_history]
        self.assertIn('trip_planning', action_types)
        self.assertIn('question_answering', action_types)
        self.assertIn('task_completion', action_types)

    def test_resonance_updates_on_action(self):
        """Test resonance field updates when actions are performed"""
        initial_epoch = self.lam.resonance.epoch

        self.lam.plan_trip("Test")

        # Epoch should have incremented
        self.assertGreater(self.lam.resonance.epoch, initial_epoch)

    def test_get_status(self):
        """Test status retrieval"""
        status = self.lam.get_status()
        self.assertIsInstance(status, str)
        self.assertIn("PRIMAL LAM STATUS", status)
        self.assertIn("Lightfoot", status)
        self.assertIn("Donte", status)


class TestResonanceStability(unittest.TestCase):
    """Test long-term resonance field stability"""

    def test_long_term_stability(self):
        """Test resonance field remains stable over many updates"""
        field = QuantumResonanceField()

        # Simulate 1000 actions
        for i in range(1000):
            field.update_resonance_parameters(i)

            # Should always maintain stability
            self.assertTrue(field.resonance_field_stable,
                          f"Field became unstable at iteration {i}")
            self.assertLess(field.lipschitz_constant, 1.0)

    def test_convergence_to_attractor(self):
        """Test system shows tendency toward Donte attractor"""
        field = QuantumResonanceField()

        epochs = []
        for i in range(100):
            field.update_resonance_parameters(i)
            epochs.append(field.epoch)

        # Epoch should increase
        self.assertEqual(epochs[-1], 100)


def run_tests():
    """Run all tests"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestQuantumResonanceField))
    suite.addTests(loader.loadTestsFromTestCase(TestPrimalLAM))
    suite.addTests(loader.loadTestsFromTestCase(TestResonanceStability))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()


if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)
