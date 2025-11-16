#!/usr/bin/env python3
"""
LAM Integration with Drug Safety Modeling System
Allows LAM to monitor and manage drug safety simulations
"""
import sys
import json
import subprocess
from pathlib import Path
from typing import Dict, Any, List, Optional
from datetime import datetime

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))


class LAMDrugSafetyInterface:
    """
    Interface between LAM and Drug Safety Modeling System
    """

    def __init__(self):
        self.drug_safety_dir = Path(__file__).parent.parent.parent / "drug_safety"
        self.available = self.drug_safety_dir.exists()

        if not self.available:
            print("Warning: Drug safety directory not found")

    def check_system_status(self) -> Dict[str, Any]:
        """Check if drug safety system is available"""
        if not self.available:
            return {
                "available": False,
                "message": "Drug safety system not found"
            }

        # Check for required files
        required_files = ["dub.json", "source/app.d"]
        files_exist = [
            (self.drug_safety_dir / f).exists() for f in required_files
        ]

        return {
            "available": all(files_exist),
            "directory": str(self.drug_safety_dir),
            "d_compiler_required": True,
            "files_checked": dict(zip(required_files, files_exist))
        }

    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the drug safety model"""
        status = self.check_system_status()

        if not status["available"]:
            return status

        return {
            "model_type": "Quantum-Inspired Drug Safety",
            "features": [
                "Quantum-inspired memory lattice",
                "Convergence detection with pattern analysis",
                "Algorithm integration framework",
                "Meta-learning controller",
                "Native complex number support"
            ],
            "language": "D Programming Language",
            "location": str(self.drug_safety_dir)
        }

    def build_model(self) -> Dict[str, Any]:
        """
        Build the drug safety model
        Requires D compiler (dmd, ldc2, or gdc)
        """
        status = self.check_system_status()

        if not status["available"]:
            return status

        build_script = self.drug_safety_dir / "build.sh"

        if not build_script.exists():
            return {
                "success": False,
                "error": "build.sh not found"
            }

        try:
            # Run build script
            result = subprocess.run(
                ["bash", str(build_script)],
                cwd=self.drug_safety_dir,
                capture_output=True,
                text=True,
                timeout=60
            )

            return {
                "success": result.returncode == 0,
                "returncode": result.returncode,
                "stdout": result.stdout[-500:] if len(result.stdout) > 500 else result.stdout,
                "stderr": result.stderr[-500:] if len(result.stderr) > 500 else result.stderr
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Build timed out after 60 seconds"
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    def run_simulation(self, parameters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        Run drug safety simulation
        """
        status = self.check_system_status()

        if not status["available"]:
            return status

        executable = self.drug_safety_dir / "drug_safety"

        if not executable.exists():
            return {
                "success": False,
                "error": "Executable not found. Run build_model() first."
            }

        try:
            # Run simulation
            result = subprocess.run(
                [str(executable)],
                cwd=self.drug_safety_dir,
                capture_output=True,
                text=True,
                timeout=30
            )

            return {
                "success": result.returncode == 0,
                "returncode": result.returncode,
                "output": result.stdout,
                "errors": result.stderr,
                "timestamp": datetime.now().isoformat()
            }

        except subprocess.TimeoutExpired:
            return {
                "success": False,
                "error": "Simulation timed out after 30 seconds"
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e)
            }

    def analyze_results(self) -> Dict[str, Any]:
        """Analyze simulation results"""
        # Look for output files
        output_files = list(self.drug_safety_dir.glob("*.json"))

        if not output_files:
            return {
                "results_found": False,
                "message": "No simulation results found"
            }

        # Read latest result file
        latest_file = max(output_files, key=lambda p: p.stat().st_mtime)

        try:
            with open(latest_file, 'r') as f:
                data = json.load(f)

            return {
                "results_found": True,
                "latest_file": str(latest_file),
                "data": data,
                "timestamp": datetime.fromtimestamp(latest_file.stat().st_mtime).isoformat()
            }

        except Exception as e:
            return {
                "results_found": True,
                "latest_file": str(latest_file),
                "error": f"Failed to parse: {e}"
            }

    def get_recommendations(self) -> List[str]:
        """Get recommendations for drug safety modeling"""
        status = self.check_system_status()

        recommendations = []

        if not status["available"]:
            recommendations.append("🔧 Install D compiler (dmd, ldc2, or gdc)")
            recommendations.append("📁 Ensure drug_safety directory is present")
        else:
            recommendations.append("✓ System available")
            recommendations.append("1. Build model with build_model()")
            recommendations.append("2. Run simulation with run_simulation()")
            recommendations.append("3. Analyze results with analyze_results()")

        return recommendations


def main():
    """Test drug safety integration"""
    print("=== LAM Drug Safety Integration Test ===\n")

    interface = LAMDrugSafetyInterface()

    # Check status
    print("1. Checking system status...")
    status = interface.check_system_status()
    print(json.dumps(status, indent=2))

    # Get model info
    print("\n2. Getting model info...")
    info = interface.get_model_info()
    print(json.dumps(info, indent=2))

    # Get recommendations
    print("\n3. Recommendations...")
    recs = interface.get_recommendations()
    for rec in recs:
        print(f"  {rec}")


if __name__ == "__main__":
    main()
