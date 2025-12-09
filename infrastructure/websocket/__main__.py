#!/usr/bin/env python3
"""
WebSocket Server Entrypoint

Run with:
    python -m infrastructure.websocket

Or from repo root:
    cd /home/user/MotorHandPro
    python -m infrastructure.websocket
"""

import asyncio
import sys
import os

# Ensure we're in the right directory
repo_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
if os.getcwd() != repo_root:
    print(f"Changing directory to repo root: {repo_root}")
    os.chdir(repo_root)

# Import after path setup
from infrastructure.websocket.websocket_server import main as ws_main

def main():
    """Main entry point for WebSocket server"""
    print("=" * 80)
    print("MotorHandPro WebSocket Server")
    print("=" * 80)
    print(f"Current directory: {os.getcwd()}")
    print()
    print("Starting WebSocket server on ws://0.0.0.0:8765")
    print()
    print("Supported query types:")
    print("  - telemetry")
    print("  - agp_state")
    print("  - nasa_comet_observations")
    print("  - nasa_processed_states")
    print()
    print("MQTT topics:")
    print("  - motorhand/nasa/comet/observations")
    print("  - motorhand/nasa/comet/processed")
    print("=" * 80)
    print()

    asyncio.run(ws_main())

if __name__ == "__main__":
    main()
