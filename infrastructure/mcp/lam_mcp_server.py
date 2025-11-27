#!/usr/bin/env python3
"""
LAM MCP Server - Model Context Protocol Server
Exposes LAM Curator capabilities through standardized MCP interface
"""
import sys
import json
import asyncio
from pathlib import Path
from typing import Dict, Any, List
from datetime import datetime
import jwt
import secrets

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from lam_curator import LAMCurator
from experiments.framework import ParamGrid


class LAMMCPServer:
    """MCP Server with LAM Curator backend"""

    def __init__(self, secret_key: str, port: int = 8765):
        """
        Initialize MCP server

        Args:
            secret_key: JWT secret for authentication
            port: Server port
        """
        self.secret_key = secret_key
        self.port = port
        self.curator = LAMCurator(api_base="http://localhost:8000")
        self.sessions = {}  # Active authenticated sessions

        print(f"🚀 LAM MCP Server initialized on port {port}")

    def generate_token(self, client_id: str, permissions: List[str]) -> str:
        """Generate JWT token for client"""
        payload = {
            "client_id": client_id,
            "permissions": permissions,
            "issued_at": datetime.utcnow().isoformat(),
            "exp": datetime.utcnow().timestamp() + 3600  # 1 hour expiry
        }
        token = jwt.encode(payload, self.secret_key, algorithm='HS256')
        return token

    def verify_token(self, token: str) -> Dict[str, Any]:
        """Verify JWT token"""
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=['HS256'])
            return {"valid": True, "payload": payload}
        except jwt.ExpiredSignatureError:
            return {"valid": False, "error": "Token expired"}
        except jwt.InvalidTokenError:
            return {"valid": False, "error": "Invalid token"}

    async def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handle MCP request

        Request format:
        {
            "method": "suggest_parameters" | "curate_sweep" | "analyze_stability",
            "token": "jwt_token",
            "params": {...}
        }
        """
        # Verify authentication
        token = request.get("token")
        if not token:
            return {"error": "Authentication required", "code": 401}

        auth_result = self.verify_token(token)
        if not auth_result["valid"]:
            return {"error": auth_result["error"], "code": 401}

        # Route to appropriate handler
        method = request.get("method")
        params = request.get("params", {})

        if method == "suggest_parameters":
            return await self.suggest_parameters(params)
        elif method == "curate_sweep":
            return await self.curate_sweep(params)
        elif method == "analyze_stability":
            return await self.analyze_stability(params)
        elif method == "get_insights":
            return await self.get_insights(params)
        else:
            return {"error": f"Unknown method: {method}", "code": 400}

    async def suggest_parameters(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """MCP handler: suggest parameter ranges"""
        try:
            sim_name = params["sim_name"]
            base_params = params["base_params"]
            target_metric = params.get("target_metric", "stable")

            grid = self.curator.suggest_parameter_ranges(
                sim_name=sim_name,
                base_params=base_params,
                target_metric=target_metric
            )

            return {
                "success": True,
                "parameter_grid": grid.params,
                "lam_state": {
                    "lipschitz": self.curator.resonance.lipschitz_constant,
                    "alpha": self.curator.resonance.alpha,
                    "lambda": self.curator.resonance.lmbd
                }
            }
        except Exception as e:
            return {"error": str(e), "code": 500}

    async def curate_sweep(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """MCP handler: run curated sweep"""
        # This would be implemented with actual simulation function
        return {"error": "Not implemented - requires simulation function", "code": 501}

    async def analyze_stability(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """MCP handler: analyze stability boundary"""
        try:
            sweep_path = Path(params["sweep_path"])
            analysis = self.curator.analyze_stability_boundary(sweep_path)

            return {
                "success": True,
                "analysis": analysis
            }
        except Exception as e:
            return {"error": str(e), "code": 500}

    async def get_insights(self, params: Dict[str, Any]) -> Dict[str, Any]:
        """MCP handler: get LAM insights"""
        try:
            insights = self.curator.generate_insights()
            return {
                "success": True,
                "insights": insights,
                "lam_state": {
                    "lipschitz": self.curator.resonance.lipschitz_constant,
                    "donte_attractor": self.curator.resonance.donte_attractor,
                    "epoch": self.curator.resonance.epoch
                }
            }
        except Exception as e:
            return {"error": str(e), "code": 500}

    async def start(self):
        """Start MCP server"""
        print(f"LAM MCP Server listening on port {self.port}")
        print(f"LAM Lipschitz: {self.curator.resonance.lipschitz_constant:.6f}")
        print(f"Ready for Model Context Protocol requests")
        # In production, this would start an actual async server
        # For now, this is a framework


def create_mcp_credentials() -> Dict[str, str]:
    """Generate secure credentials for MCP server"""
    return {
        "secret_key": secrets.token_hex(32),
        "client_id": f"lam_client_{secrets.token_hex(8)}",
        "client_secret": secrets.token_hex(32)
    }


def main():
    """Run MCP server"""
    import argparse

    parser = argparse.ArgumentParser(description='LAM MCP Server')
    parser.add_argument('--port', type=int, default=8765, help='Server port')
    parser.add_argument('--secret-key', type=str, help='JWT secret key')
    args = parser.parse_args()

    # Load or generate secret key
    secret_key = args.secret_key
    if not secret_key:
        creds_path = Path(__file__).parent / "credentials.json"
        if creds_path.exists():
            with open(creds_path) as f:
                creds = json.load(f)
                secret_key = creds.get("mcp_secret_key")
        else:
            print("⚠️  No credentials found. Run: python init_credentials.py")
            sys.exit(1)

    # Start server
    server = LAMMCPServer(secret_key=secret_key, port=args.port)

    print("\n" + "="*70)
    print("LAM MCP SERVER - MODEL CONTEXT PROTOCOL")
    print("="*70)
    print()
    print("Endpoints:")
    print("  • suggest_parameters - Get LAM-optimized parameter ranges")
    print("  • curate_sweep       - Run LAM-curated parameter sweep")
    print("  • analyze_stability  - Analyze stability boundaries")
    print("  • get_insights       - Get LAM-generated insights")
    print()
    print("Authentication: JWT tokens required")
    print("="*70)
    print()

    asyncio.run(server.start())


if __name__ == "__main__":
    main()
