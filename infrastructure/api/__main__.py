#!/usr/bin/env python3
"""
FastAPI Server Entrypoint

Run with:
    python -m infrastructure.api

Or from repo root:
    cd /home/user/MotorHandPro
    python -m infrastructure.api
"""

import uvicorn
import sys
import os

# Ensure we're in the right directory
repo_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
if os.getcwd() != repo_root:
    print(f"Changing directory to repo root: {repo_root}")
    os.chdir(repo_root)

def main():
    """Main entry point for FastAPI server"""
    print("=" * 80)
    print("MotorHandPro FastAPI Server")
    print("=" * 80)
    print(f"Current directory: {os.getcwd()}")
    print(f"Python path: {sys.path}")
    print()
    print("Starting server on http://0.0.0.0:8000")
    print("API docs: http://localhost:8000/docs")
    print("ReDoc: http://localhost:8000/redoc")
    print()
    print("NASA endpoints:")
    print("  GET  /nasa/status")
    print("  POST /nasa/comet/fetch")
    print("  GET  /nasa/comet/observations")
    print("  POST /nasa/comet/process")
    print("  GET  /nasa/comet/processed")
    print("=" * 80)
    print()

    uvicorn.run(
        "infrastructure.api.fastapi_server:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info",
    )

if __name__ == "__main__":
    main()
