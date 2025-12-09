#!/usr/bin/env python3
"""
Test suite for GoTrax Edge Hoverboard integration
Tests token burn mechanism and actuation control
"""
import unittest
import asyncio
import sys
from pathlib import Path

# Add parent directories to path
sys.path.insert(0, str(Path(__file__).parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "extras" / "primal"))

from integrations.gotrax_hoverboard_integration import (
    GoTraxHoverboardController,
    HederaSmartContractInterface,
    LAMHoverboardInterface,
    HoverboardMotorSpec,
    TokenBurnConfig,
    ActuationRequest,
    ActuationMode
)


class TestHoverboardMotorSpec(unittest.TestCase):
    """Test motor specifications"""

    def test_default_spec(self):
        """Test default motor spec values"""
        spec = HoverboardMotorSpec()
        self.assertEqual(spec.voltage_nominal, 36.0)
        self.assertEqual(spec.power_rating, 250.0)
        self.assertAlmostEqual(spec.wheel_diameter_mm, 165.1, places=1)
        self.assertEqual(spec.pwm_frequency_hz, 20000.0)

    def test_custom_spec(self):
        """Test custom motor spec"""
        spec = HoverboardMotorSpec(
            voltage_nominal=42.0,
            power_rating=350.0,
            max_rpm=300.0
        )
        self.assertEqual(spec.voltage_nominal, 42.0)
        self.assertEqual(spec.power_rating, 350.0)
        self.assertEqual(spec.max_rpm, 300.0)


class TestTokenBurnConfig(unittest.TestCase):
    """Test token burn configuration"""

    def test_default_config(self):
        """Test default token config (1 token = 1 second)"""
        config = TokenBurnConfig()
        self.assertEqual(config.token_rate, 1.0)
        self.assertEqual(config.min_tokens_required, 0.1)

    def test_custom_rate(self):
        """Test custom token rate"""
        config = TokenBurnConfig(token_rate=2.0)
        self.assertEqual(config.token_rate, 2.0)


class TestGoTraxHoverboardController(unittest.TestCase):
    """Test hoverboard controller"""

    def setUp(self):
        self.controller = GoTraxHoverboardController()

    def test_initialization(self):
        """Test controller initializes with correct constants"""
        self.assertAlmostEqual(self.controller.lightfoot_constant, 0.16905, places=5)
        self.assertAlmostEqual(self.controller.donte_attractor, 149.9992314, places=5)
        self.assertEqual(self.controller.token_balance, 0.0)

    def test_lipschitz_constant(self):
        """Test Lipschitz constant is < 1 for stability"""
        lipschitz = self.controller.lipschitz_constant
        self.assertLess(lipschitz, 1.0)
        self.assertGreater(lipschitz, 0.0)

    def test_deposit_tokens(self):
        """Test token deposit"""
        result = self.controller.deposit_tokens(10.0)
        self.assertTrue(result['success'])
        self.assertEqual(result['tokens_deposited'], 10.0)
        self.assertEqual(result['new_balance'], 10.0)
        self.assertEqual(result['max_actuation_time'], 10.0)

    def test_set_token_balance(self):
        """Test setting token balance"""
        result = self.controller.set_token_balance(50.0)
        self.assertTrue(result['success'])
        self.assertEqual(result['token_balance'], 50.0)

    def test_negative_token_balance(self):
        """Test negative token balance is clamped to 0"""
        result = self.controller.set_token_balance(-10.0)
        self.assertTrue(result['success'])
        self.assertEqual(result['token_balance'], 0.0)

    def test_get_motor_state(self):
        """Test motor state retrieval"""
        state = self.controller.get_motor_state()
        self.assertIn('left_motor', state)
        self.assertIn('right_motor', state)
        self.assertIn('token_balance', state)
        self.assertEqual(state['left_motor']['duty_cycle'], 0.0)
        self.assertEqual(state['right_motor']['duty_cycle'], 0.0)

    def test_get_status(self):
        """Test comprehensive status retrieval"""
        status = self.controller.get_status()
        self.assertEqual(status['controller'], 'GoTrax Edge Hoverboard')
        self.assertIn('motor_spec', status)
        self.assertIn('token_config', status)
        self.assertIn('primal_logic', status)


class TestActuationExecution(unittest.TestCase):
    """Test actuation execution with token burn"""

    def setUp(self):
        self.controller = GoTraxHoverboardController()
        self.controller.deposit_tokens(100.0)

    def test_successful_actuation(self):
        """Test successful actuation burns correct tokens"""
        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=2.0,
            power_level=0.5,
            tokens_allocated=2.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        self.assertTrue(result.success)
        self.assertEqual(result.tokens_burned, 2.0)
        self.assertEqual(result.actuation_duration_actual, 2.0)
        self.assertGreater(result.smoothness_score, 0.0)
        self.assertLessEqual(result.smoothness_score, 1.0)

    def test_insufficient_tokens(self):
        """Test actuation fails with insufficient tokens"""
        self.controller.set_token_balance(1.0)

        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=5.0,
            power_level=0.5,
            tokens_allocated=5.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        self.assertFalse(result.success)
        self.assertEqual(result.tokens_burned, 0.0)
        self.assertIn('Insufficient tokens', result.error_message)

    def test_below_minimum_tokens(self):
        """Test actuation fails below minimum token requirement"""
        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=0.05,  # Below minimum 0.1 tokens
            power_level=0.5,
            tokens_allocated=0.05
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        self.assertFalse(result.success)
        self.assertIn('minimum', result.error_message.lower())

    def test_token_rate_1_second(self):
        """Test 1 token = 1 second of actuation"""
        initial_balance = 10.0
        self.controller.set_token_balance(initial_balance)

        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=3.0,
            power_level=0.5,
            tokens_allocated=3.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        self.assertTrue(result.success)
        self.assertEqual(result.tokens_burned, 3.0)  # 3 seconds = 3 tokens
        self.assertEqual(self.controller.token_balance, 7.0)  # 10 - 3 = 7

    def test_different_actuation_modes(self):
        """Test all actuation modes work correctly"""
        modes = [
            ActuationMode.FORWARD,
            ActuationMode.REVERSE,
            ActuationMode.TURN_LEFT,
            ActuationMode.TURN_RIGHT,
            ActuationMode.SPIN,
            ActuationMode.STOP
        ]

        for mode in modes:
            request = ActuationRequest(
                mode=mode,
                duration_seconds=0.5,
                power_level=0.3,
                tokens_allocated=0.5
            )

            result = asyncio.run(self.controller.execute_actuation(request))
            self.assertTrue(result.success, f"Mode {mode.value} failed")

    def test_primal_logic_metrics(self):
        """Test Primal Logic metrics are included in result"""
        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=1.0,
            power_level=0.5,
            tokens_allocated=1.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        metrics = result.primal_logic_metrics
        self.assertIn('lightfoot_constant', metrics)
        self.assertIn('donte_attractor', metrics)
        self.assertIn('lipschitz_bound', metrics)
        self.assertIn('stability_guaranteed', metrics)
        self.assertTrue(metrics['stability_guaranteed'])


class TestSmartContractInterface(unittest.TestCase):
    """Test Hedera smart contract interface (mock mode)"""

    def setUp(self):
        # Create contract in mock mode (no credentials provided)
        self.contract = HederaSmartContractInterface()

    def test_connect(self):
        """Test connection (mock mode)"""
        result = self.contract.connect()
        self.assertTrue(result)
        self.assertTrue(self.contract.is_connected)
        self.assertTrue(self.contract.mock_mode)  # Should be in mock mode

    def test_get_balance(self):
        """Test balance retrieval"""
        balance = self.contract.get_balance()
        self.assertEqual(balance, 100.0)  # Default mock balance

    def test_burn_tokens(self):
        """Test token burn (mock mode)"""
        result = self.contract.burn_tokens(10.0, 10.0)
        self.assertTrue(result['success'])
        self.assertEqual(result['tokens_burned'], 10.0)
        self.assertEqual(result['actuation_seconds'], 10.0)
        self.assertEqual(result['remaining_balance'], 90.0)
        self.assertTrue(result['mock_mode'])  # Verify it's in mock mode
        self.assertIn('transaction_id', result)

    def test_burn_insufficient_balance(self):
        """Test burn fails with insufficient balance"""
        result = self.contract.burn_tokens(200.0, 200.0)
        self.assertFalse(result['success'])
        self.assertIn('Insufficient', result['error'])

    def test_get_contract_info(self):
        """Test contract info retrieval"""
        info = self.contract.get_contract_info()
        self.assertEqual(info['token_symbol'], 'HAT')
        self.assertEqual(info['token_rate'], '1 HAT = 1 second of actuation')
        self.assertEqual(info['network'], 'testnet')  # Default network
        self.assertTrue(info['mock_mode'])  # Should be in mock mode


class TestLAMHoverboardInterface(unittest.TestCase):
    """Test LAM interface for hoverboard control"""

    def setUp(self):
        self.interface = LAMHoverboardInterface()
        self.interface.deposit_tokens(50.0)

    def test_execute_move(self):
        """Test move execution through LAM interface"""
        result = asyncio.run(
            self.interface.execute_move("forward", 2.0, power=0.5)
        )

        self.assertTrue(result['success'])
        self.assertEqual(result['mode'], "forward")
        self.assertEqual(result['duration_requested'], 2.0)
        self.assertEqual(result['tokens_burned'], 2.0)

    def test_deposit_tokens(self):
        """Test token deposit through interface"""
        result = self.interface.deposit_tokens(25.0)
        self.assertTrue(result['success'])
        self.assertEqual(result['tokens_deposited'], 25.0)

    def test_get_status(self):
        """Test status retrieval"""
        status = self.interface.get_status()
        self.assertIn('hoverboard', status)
        self.assertIn('smart_contract', status)

    def test_invalid_mode_defaults_to_stop(self):
        """Test invalid mode defaults to stop"""
        result = asyncio.run(
            self.interface.execute_move("invalid_mode", 1.0, power=0.5)
        )

        self.assertTrue(result['success'])  # Stop should succeed


class TestSmoothnessCalculation(unittest.TestCase):
    """Test smoothness score calculation"""

    def setUp(self):
        self.controller = GoTraxHoverboardController()
        self.controller.deposit_tokens(100.0)

    def test_smoothness_score_range(self):
        """Test smoothness score is between 0 and 1"""
        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=2.0,
            power_level=0.7,
            tokens_allocated=2.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        self.assertGreaterEqual(result.smoothness_score, 0.0)
        self.assertLessEqual(result.smoothness_score, 1.0)

    def test_higher_power_still_smooth(self):
        """Test high power maintains reasonable smoothness"""
        request = ActuationRequest(
            mode=ActuationMode.FORWARD,
            duration_seconds=3.0,
            power_level=1.0,
            tokens_allocated=3.0
        )

        result = asyncio.run(self.controller.execute_actuation(request))

        # Should still have positive smoothness (indicating trajectory generation worked)
        self.assertGreater(result.smoothness_score, 0.0)
        # Smoothness is bounded by 1.0
        self.assertLessEqual(result.smoothness_score, 1.0)


def run_tests():
    """Run all tests"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestHoverboardMotorSpec))
    suite.addTests(loader.loadTestsFromTestCase(TestTokenBurnConfig))
    suite.addTests(loader.loadTestsFromTestCase(TestGoTraxHoverboardController))
    suite.addTests(loader.loadTestsFromTestCase(TestActuationExecution))
    suite.addTests(loader.loadTestsFromTestCase(TestSmartContractInterface))
    suite.addTests(loader.loadTestsFromTestCase(TestLAMHoverboardInterface))
    suite.addTests(loader.loadTestsFromTestCase(TestSmoothnessCalculation))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result.wasSuccessful()


if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)
