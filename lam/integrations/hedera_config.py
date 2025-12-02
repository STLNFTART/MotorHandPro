#!/usr/bin/env python3
"""
Hedera Configuration Module
Loads Hedera network configuration from environment variables

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""
import os
from typing import Dict, Optional
from pathlib import Path


def load_hedera_config(env_file: Optional[str] = None) -> Dict[str, str]:
    """
    Load Hedera configuration from environment variables or .env file

    Args:
        env_file: Optional path to .env file (defaults to lam/.env)

    Returns:
        Dictionary with Hedera configuration
    """
    # Try to load python-dotenv if available
    try:
        from dotenv import load_dotenv

        if env_file:
            load_dotenv(env_file)
        else:
            # Look for .env in lam directory
            lam_dir = Path(__file__).parent.parent
            env_path = lam_dir / ".env"
            if env_path.exists():
                load_dotenv(env_path)
    except ImportError:
        pass  # python-dotenv not available, use system env vars only

    # Load configuration from environment variables
    config = {
        "contract_id": os.getenv("HEDERA_CONTRACT_ID", ""),
        "network": os.getenv("HEDERA_NETWORK", "testnet"),
        "account_id": os.getenv("HEDERA_ACCOUNT_ID", ""),
        "private_key": os.getenv("HEDERA_PRIVATE_KEY", ""),
        "mock_mode": os.getenv("HEDERA_MOCK_MODE", "false").lower() == "true",
        "token_burn_rate": float(os.getenv("TOKEN_BURN_RATE", "1.0")),
        "min_tokens_required": float(os.getenv("MIN_TOKENS_REQUIRED", "0.1")),
        "query_gas": int(os.getenv("HEDERA_QUERY_GAS", "100000")),
        "execute_gas": int(os.getenv("HEDERA_EXECUTE_GAS", "200000")),
    }

    return config


def validate_hedera_config(config: Dict[str, str]) -> tuple[bool, str]:
    """
    Validate Hedera configuration

    Args:
        config: Configuration dictionary

    Returns:
        Tuple of (is_valid, error_message)
    """
    # If mock mode is enabled, skip validation
    if config.get("mock_mode", False):
        return True, "Mock mode enabled - skipping credential validation"

    # Check required fields
    required_fields = ["contract_id", "account_id", "private_key"]
    missing_fields = [f for f in required_fields if not config.get(f)]

    if missing_fields:
        return False, f"Missing required configuration: {', '.join(missing_fields)}"

    # Validate network
    network = config.get("network", "").lower()
    if network not in ["testnet", "mainnet"]:
        return False, f"Invalid network: {network}. Must be 'testnet' or 'mainnet'"

    # Validate Hedera ID format (basic check)
    for field in ["contract_id", "account_id"]:
        value = config.get(field, "")
        if value and not value.startswith("0.0."):
            return False, f"Invalid {field} format: {value}. Expected format: 0.0.12345"

    return True, "Configuration valid"


def print_hedera_config_status(config: Dict[str, str]):
    """
    Print Hedera configuration status (without exposing sensitive data)

    Args:
        config: Configuration dictionary
    """
    print("\n" + "=" * 60)
    print("HEDERA CONFIGURATION STATUS")
    print("=" * 60)

    is_valid, message = validate_hedera_config(config)

    print(f"Network:      {config.get('network', 'Not set')}")
    print(f"Contract ID:  {config.get('contract_id', 'Not set')}")
    print(f"Account ID:   {config.get('account_id', 'Not set')}")
    print(f"Private Key:  {'***configured***' if config.get('private_key') else 'Not set'}")
    print(f"Mock Mode:    {config.get('mock_mode', False)}")
    print(f"Token Rate:   {config.get('token_burn_rate', 1.0)} token/second")
    print(f"Min Tokens:   {config.get('min_tokens_required', 0.1)}")
    print()
    print(f"Status:       {'✅ VALID' if is_valid else '❌ INVALID'}")
    print(f"Message:      {message}")
    print("=" * 60 + "\n")


if __name__ == "__main__":
    # Test configuration loading
    config = load_hedera_config()
    print_hedera_config_status(config)
