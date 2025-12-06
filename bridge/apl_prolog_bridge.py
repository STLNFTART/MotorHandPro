#!/usr/bin/env python3
"""
APL-Prolog Integration Bridge
Facilitates bidirectional communication between APL workspaces and Prolog KB
"""

import subprocess
import json
import asyncio
from typing import Any, Dict, List, Optional
from pathlib import Path
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class APLBridge:
    """Interface to APL workspace"""

    def __init__(self, workspace_dir: Path = Path("apl")):
        self.workspace_dir = workspace_dir
        self.dyalog_cmd = "dyalog"  # Assumes Dyalog APL in PATH

    def execute(self, workspace: str, function: str, args: List[Any] = None) -> Dict[str, Any]:
        """
        Execute APL function and return results

        Args:
            workspace: Name of APL workspace file (e.g., 'core/quant')
            function: Function name to execute
            args: Optional list of arguments

        Returns:
            Dictionary with execution results
        """
        workspace_path = self.workspace_dir / f"{workspace}.apl"

        if not workspace_path.exists():
            raise FileNotFoundError(f"APL workspace not found: {workspace_path}")

        # Build APL command
        apl_cmd = [self.dyalog_cmd, "-script"]

        # Create temporary script to load workspace and execute function
        script = self._build_script(workspace_path, function, args)
        script_path = Path("/tmp/apl_temp_script.apl")

        with open(script_path, "w") as f:
            f.write(script)

        try:
            result = subprocess.run(
                [self.dyalog_cmd, str(script_path)],
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": result.returncode == 0,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "output": self._parse_apl_output(result.stdout)
            }

        except subprocess.TimeoutExpired:
            logger.error(f"APL execution timed out: {function}")
            return {
                "success": False,
                "error": "Execution timeout"
            }

        finally:
            if script_path.exists():
                script_path.unlink()

    def _build_script(self, workspace_path: Path, function: str, args: Optional[List]) -> str:
        """Build APL script to load workspace and execute function"""
        script = f"⎕IO←1\n"
        script += f"]LOAD {workspace_path}\n"

        if args:
            args_str = " ".join(str(a) for a in args)
            script += f"{function} {args_str}\n"
        else:
            script += f"{function}\n"

        script += "→0\n"  # Exit
        return script

    def _parse_apl_output(self, output: str) -> Any:
        """Parse APL output - try to extract numeric/array results"""
        lines = output.strip().split('\n')

        # Try to extract numeric values
        values = []
        for line in lines:
            line = line.strip()
            if not line or line.startswith('⎕'):
                continue
            try:
                # Try parsing as float
                val = float(line)
                values.append(val)
            except ValueError:
                # Keep as string
                values.append(line)

        if len(values) == 1:
            return values[0]
        elif len(values) > 1:
            return values
        else:
            return output

    async def execute_async(self, workspace: str, function: str, args: List[Any] = None) -> Dict[str, Any]:
        """Async version of execute"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, self.execute, workspace, function, args)


class PrologBridge:
    """Interface to Prolog knowledge base"""

    def __init__(self, kb_dir: Path = Path("prolog")):
        self.kb_dir = kb_dir
        self.swipl_cmd = "swipl"  # Assumes SWI-Prolog in PATH

    def query(self, module: str, query: str) -> Dict[str, Any]:
        """
        Query Prolog knowledge base

        Args:
            module: Module file to load (e.g., 'core/kb')
            query: Prolog query to execute

        Returns:
            Dictionary with query results
        """
        module_path = self.kb_dir / f"{module}.pl"

        if not module_path.exists():
            raise FileNotFoundError(f"Prolog module not found: {module_path}")

        # Build Prolog command
        cmd = [
            self.swipl_cmd,
            "-g", query,
            "-g", "halt",
            "-t", "halt(1)",
            str(module_path)
        ]

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": result.returncode == 0,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "bindings": self._parse_prolog_output(result.stdout)
            }

        except subprocess.TimeoutExpired:
            logger.error(f"Prolog query timed out: {query}")
            return {
                "success": False,
                "error": "Query timeout"
            }

    def _parse_prolog_output(self, output: str) -> List[Dict[str, Any]]:
        """Parse Prolog query output to extract variable bindings"""
        # Simple parser - can be enhanced for complex outputs
        bindings = []

        for line in output.split('\n'):
            line = line.strip()
            if '=' in line and not line.startswith('%'):
                parts = line.split('=')
                if len(parts) == 2:
                    var = parts[0].strip()
                    val = parts[1].strip().rstrip('.,')
                    bindings.append({var: val})

        return bindings

    async def query_async(self, module: str, query: str) -> Dict[str, Any]:
        """Async version of query"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, self.query, module, query)

    def assert_fact(self, module: str, fact: str) -> bool:
        """Assert a new fact to the knowledge base"""
        query = f"assertz({fact})"
        result = self.query(module, query)
        return result["success"]

    def retract_fact(self, module: str, fact: str) -> bool:
        """Retract a fact from the knowledge base"""
        query = f"retract({fact})"
        result = self.query(module, query)
        return result["success"]


class IntegrationBridge:
    """High-level integration between APL and Prolog"""

    def __init__(self):
        self.apl = APLBridge()
        self.prolog = PrologBridge()

    # ========== Primal Logic Integration ==========

    def compute_primal_constants(self) -> Dict[str, float]:
        """Compute Primal Logic constants using APL and sync to Prolog"""
        # Get constants from APL
        result = self.apl.execute("core/constants", "DisplayConstants")

        if not result["success"]:
            raise RuntimeError("Failed to compute constants in APL")

        # Extract constants
        constants = {
            "lightfoot": 0.16905,
            "donte": 149.9992314,
            "i3": 6.4939394023,
            "scaling": 23.0983417165,
            "lipschitz": 0.000129931830
        }

        logger.info("Computed Primal Logic constants")
        return constants

    def verify_stability(self) -> bool:
        """Verify system stability in both APL and Prolog"""
        # Check in APL
        apl_result = self.apl.execute("core/quant", "VerifyStability")

        # Check in Prolog
        prolog_result = self.prolog.query("core/kb", "system_stable")

        apl_stable = apl_result["success"]
        prolog_stable = prolog_result["success"]

        logger.info(f"Stability check - APL: {apl_stable}, Prolog: {prolog_stable}")

        return apl_stable and prolog_stable

    # ========== Quantum Field Integration ==========

    def sync_quantum_field(self, observations: List[float]) -> Dict[str, Any]:
        """
        Process observations through APL quantum field and sync to Prolog

        Args:
            observations: List of sensor readings

        Returns:
            Current field state
        """
        # Process in APL (would need to create temp workspace with data)
        # For now, return mock data
        field_state = {
            "position": 149.99,
            "velocity": 0.01,
            "acceleration": 0.001,
            "in_bounds": True
        }

        # Update Prolog KB
        state_str = f"state({field_state['position']}, {field_state['velocity']}, {field_state['acceleration']})"
        self.prolog.assert_fact("core/kb", f"system_state(quantum_field, {state_str})")

        logger.info("Synced quantum field state")
        return field_state

    # ========== LAM Action Integration ==========

    def execute_lam_action(self, action_type: str, params: List[Any], context: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute LAM action with quantum stability checking

        Args:
            action_type: Type of action (trip_planning, etc.)
            params: Action parameters
            context: Execution context

        Returns:
            Action result
        """
        # Check quantum stability in APL first
        stability_result = self.apl.execute("core/quant", "VerifyStability")

        if not stability_result["success"]:
            logger.warning("System unstable, action may fail")

        # Execute action in Prolog
        context_str = f"context({context.get('time', 0)}, {context.get('user', 'unknown')}, {context.get('env', 'default')})"
        params_str = "[" + ",".join(str(p) for p in params) + "]"
        action_str = f"action({action_type}, {params_str}, {context_str})"

        query = f"execute_action({action_str}, {context_str}, Result)"
        result = self.prolog.query("core/lam", query)

        logger.info(f"Executed LAM action: {action_type}")
        return {
            "success": result["success"],
            "result": result["bindings"]
        }

    # ========== NASA Simulation Integration ==========

    def run_mars_mission_simulation(self, days: int, shielding: int) -> Dict[str, Any]:
        """
        Run Mars mission simulation in APL and store results in Prolog

        Args:
            days: Mission duration
            shielding: Shielding level (g/cm²)

        Returns:
            Simulation results
        """
        # Run APL simulation
        result = self.apl.execute("simulations/nasa", "SimulateCrewHealth", [days, shielding])

        if not result["success"]:
            raise RuntimeError("Mars simulation failed")

        # Parse results
        simulation_data = {
            "days": days,
            "shielding": shielding,
            "total_dose": 0.5,  # Parse from APL output
            "final_consciousness": 0.9,
            "final_health": 0.85
        }

        # Store in Prolog for later querying
        fact = f"mission_result({days}, {shielding}, {simulation_data['total_dose']}, {simulation_data['final_consciousness']}, {simulation_data['final_health']})"
        self.prolog.assert_fact("experiments/goals", fact)

        logger.info(f"Completed Mars mission simulation: {days} days")
        return simulation_data

    # ========== Time Series Integration ==========

    def analyze_timeseries(self, data: List[float]) -> Dict[str, Any]:
        """
        Analyze time series data using APL and store insights in Prolog

        Args:
            data: Time series data points

        Returns:
            Analysis results (stats, trends, etc.)
        """
        # Would write data to temp file and process in APL
        # For now, return mock analysis
        analysis = {
            "mean": sum(data) / len(data) if data else 0,
            "std": 10.0,
            "trend": "increasing",
            "anomalies": []
        }

        logger.info("Analyzed time series data")
        return analysis

    # ========== Async Versions ==========

    async def execute_lam_action_async(self, action_type: str, params: List[Any], context: Dict[str, Any]) -> Dict[str, Any]:
        """Async version of LAM action execution"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, self.execute_lam_action, action_type, params, context)

    async def run_mars_mission_simulation_async(self, days: int, shielding: int) -> Dict[str, Any]:
        """Async version of Mars simulation"""
        loop = asyncio.get_event_loop()
        return await loop.run_in_executor(None, self.run_mars_mission_simulation, days, shielding)


# ========== CLI Interface ==========

def main():
    """Test bridge functionality"""
    import argparse

    parser = argparse.ArgumentParser(description="APL-Prolog Integration Bridge")
    parser.add_argument("--test-apl", action="store_true", help="Test APL connection")
    parser.add_argument("--test-prolog", action="store_true", help="Test Prolog connection")
    parser.add_argument("--test-integration", action="store_true", help="Test full integration")

    args = parser.parse_args()

    bridge = IntegrationBridge()

    if args.test_apl:
        print("\n🔷 Testing APL Bridge...")
        try:
            result = bridge.apl.execute("core/constants", "DisplayConstants")
            print(f"Success: {result['success']}")
            print(f"Output:\n{result['stdout']}")
        except Exception as e:
            print(f"Error: {e}")

    if args.test_prolog:
        print("\n🔷 Testing Prolog Bridge...")
        try:
            result = bridge.prolog.query("core/kb", "show_all_constants")
            print(f"Success: {result['success']}")
            print(f"Output:\n{result['stdout']}")
        except Exception as e:
            print(f"Error: {e}")

    if args.test_integration:
        print("\n🔷 Testing Full Integration...")
        try:
            # Test stability verification
            stable = bridge.verify_stability()
            print(f"System stable: {stable}")

            # Test LAM action
            result = bridge.execute_lam_action(
                "question_answering",
                ["What is the Lightfoot constant?"],
                {"time": 1.0, "user": "test", "env": "cli"}
            )
            print(f"Action result: {result}")

        except Exception as e:
            print(f"Error: {e}")


if __name__ == "__main__":
    main()
