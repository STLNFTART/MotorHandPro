#!/usr/bin/env python3
"""
MotorHandPro Integration System - Main Entry Point

This script launches the complete integration system including:
- Bi-directional data capture
- Framework validation
- Visualization integration
- Web control panel server

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import asyncio
import subprocess
import sys
from pathlib import Path


def print_banner():
    """Print system banner"""
    banner = """
╔══════════════════════════════════════════════════════════════════╗
║                                                                  ║
║              MotorHandPro Integration System                     ║
║                  Primal Logic Framework                          ║
║                                                                  ║
║  Patent Pending: U.S. Provisional Patent Application 63/842,846 ║
║  © 2025 Donte Lightfoot - The Phoney Express LLC                ║
║                                                                  ║
╚══════════════════════════════════════════════════════════════════╝

Integrated Repositories:
  • SpaceX (r-spacex/SpaceX-API)
  • Tesla (teslamotors/* - Top 5 repos)
  • Firestorm Drones (PX4, QGroundControl)
  • CARLA Simulator (carla-simulator/carla)
  • Visualization Tools (matplotlib, VTK, three.js, LaTeX)

System Features:
  ✓ Bi-directional data capture
  ✓ Real-time control panel
  ✓ Advanced 3D visualization
  ✓ Framework validation
  ✓ LaTeX report generation
"""
    print(banner)


def check_dependencies():
    """Check if required dependencies are installed"""
    print("\n[1/5] Checking dependencies...")

    required = ['numpy', 'matplotlib', 'websockets', 'requests']
    missing = []

    for package in required:
        try:
            __import__(package)
            print(f"  ✓ {package}")
        except ImportError:
            print(f"  ✗ {package} (missing)")
            missing.append(package)

    if missing:
        print(f"\nMissing dependencies: {', '.join(missing)}")
        print("Install with: pip install -r requirements.txt")
        return False

    print("  All core dependencies installed!")
    return True


def run_validation():
    """Run framework validation"""
    print("\n[2/5] Running framework validation...")
    print("  Validating against SpaceX, Tesla, Firestorm, CARLA...")

    try:
        from integrations.framework_validation import PrimalLogicValidator

        validator = PrimalLogicValidator()
        results = validator.run_all_validations()

        # Export results
        validator.export_results_to_json("integrations/validation_results.json")
        validator.generate_latex_report("integrations/validation_report.tex")

        passed = sum(1 for r in results if r.passed)
        print(f"\n  Validation complete: {passed}/{len(results)} tests passed")
        return True

    except Exception as e:
        print(f"  ✗ Validation failed: {e}")
        return False


def start_control_panel_server():
    """Start HTTP server for control panel"""
    print("\n[3/5] Starting control panel HTTP server...")

    control_panel_dir = Path(__file__).parent / "control_panel"

    if not control_panel_dir.exists():
        print(f"  ✗ Control panel directory not found: {control_panel_dir}")
        return None

    try:
        # Start simple HTTP server for control panel
        process = subprocess.Popen(
            [sys.executable, "-m", "http.server", "8080"],
            cwd=control_panel_dir,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )

        print("  ✓ Control panel server started on http://localhost:8080")
        return process

    except Exception as e:
        print(f"  ✗ Failed to start control panel server: {e}")
        return None


async def start_data_capture():
    """Start bi-directional data capture system"""
    print("\n[4/5] Starting data capture system...")

    try:
        from integrations.data_capture import BiDirectionalDataCapture

        capture_system = BiDirectionalDataCapture()
        print("  ✓ Data capture system initialized")
        print("  ✓ WebSocket server starting on ws://localhost:8765")
        print("\n  Data flow:")
        print("    SpaceX ↔ Tesla ↔ Firestorm ↔ CARLA ↔ Primal Logic ↔ Visualization")

        await capture_system.run()

    except Exception as e:
        print(f"  ✗ Data capture failed: {e}")
        raise


def print_usage_instructions():
    """Print usage instructions"""
    print("\n[5/5] System Ready!")
    print("\n" + "="*70)
    print("USAGE INSTRUCTIONS")
    print("="*70)

    print("\n1. Open Control Panel:")
    print("   → Navigate to: http://localhost:8080")

    print("\n2. Control Panel Features:")
    print("   • Adjust Primal Logic parameters (λ, KE)")
    print("   • Toggle data sources (SpaceX, Tesla, Firestorm, CARLA)")
    print("   • View real-time visualizations")
    print("   • Export LaTeX reports")
    print("   • Run validation tests")

    print("\n3. WebSocket Connection:")
    print("   → Control panel connects to: ws://localhost:8765")
    print("   → Bi-directional data flow enabled")

    print("\n4. Visualization Tabs:")
    print("   • Real-time Data: matplotlib/Chart.js plots")
    print("   • 3D Visualization: three.js interactive scene")
    print("   • Stability Analysis: Lipschitz tracking")
    print("   • Trajectory: Vehicle path visualization")

    print("\n5. Reports & Logs:")
    print("   • Validation results: integrations/validation_results.json")
    print("   • LaTeX report: integrations/validation_report.tex")
    print("   • Control panel: http://localhost:8080")

    print("\n6. Repository Links (accessible in control panel):")
    print("   • SpaceX: https://github.com/r-spacex/SpaceX-API")
    print("   • Tesla: https://github.com/teslamotors")
    print("   • Firestorm: https://www.launchfirestorm.com")
    print("   • PX4: https://github.com/PX4/PX4-Autopilot")
    print("   • CARLA: https://github.com/carla-simulator/carla")

    print("\n" + "="*70)
    print("Press Ctrl+C to stop the system")
    print("="*70 + "\n")


async def main():
    """Main entry point"""
    print_banner()

    # Check dependencies
    if not check_dependencies():
        print("\nPlease install dependencies and try again.")
        return 1

    # Run validation
    run_validation()

    # Start control panel server
    http_server = start_control_panel_server()

    try:
        # Print instructions
        print_usage_instructions()

        # Start data capture (runs forever)
        await start_data_capture()

    except KeyboardInterrupt:
        print("\n\nShutting down system...")

    finally:
        if http_server:
            http_server.terminate()
            print("✓ Control panel server stopped")

    print("\nSystem shutdown complete.")
    return 0


if __name__ == "__main__":
    try:
        exit_code = asyncio.run(main())
        sys.exit(exit_code)
    except Exception as e:
        print(f"\n✗ Fatal error: {e}")
        sys.exit(1)
