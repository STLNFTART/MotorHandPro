#!/usr/bin/env python3
"""
Verify $RPO contract is live and accessible
"""
import os
from web3 import Web3

# Contract details
CONTRACT_ADDRESS = "0xa2e2b88620944085D390fdbe86CBa10A2f06a033"
RPC_URL = "https://testnet.hashio.io/api"

# Minimal ABI to check contract
ABI = [
    {
        "inputs": [],
        "name": "symbol",
        "outputs": [{"internalType": "string", "name": "", "type": "string"}],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "name",
        "outputs": [{"internalType": "string", "name": "", "type": "string"}],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "totalSupply",
        "outputs": [{"internalType": "uint256", "name": "", "type": "uint256"}],
        "stateMutability": "view",
        "type": "function"
    },
    {
        "inputs": [],
        "name": "decimals",
        "outputs": [{"internalType": "uint8", "name": "", "type": "uint8"}],
        "stateMutability": "view",
        "type": "function"
    }
]

print("=" * 70)
print("$RPO CONTRACT VERIFICATION")
print("=" * 70)

# Connect to Hedera
w3 = Web3(Web3.HTTPProvider(RPC_URL))

if not w3.is_connected():
    print("❌ Failed to connect to Hedera RPC")
    exit(1)

print(f"✅ Connected to Hedera Testnet")
print(f"   RPC: {RPC_URL}")
print(f"   Chain ID: {w3.eth.chain_id}")

# Load contract
contract = w3.eth.contract(
    address=Web3.to_checksum_address(CONTRACT_ADDRESS),
    abi=ABI
)

print(f"\n📄 Contract: {CONTRACT_ADDRESS}")

try:
    # Get token info
    name = contract.functions.name().call()
    symbol = contract.functions.symbol().call()
    decimals = contract.functions.decimals().call()
    total_supply = contract.functions.totalSupply().call()

    # Convert total supply
    total_supply_tokens = total_supply / (10 ** decimals)

    print(f"\n✅ CONTRACT IS LIVE!")
    print(f"   Name: {name}")
    print(f"   Symbol: {symbol}")
    print(f"   Decimals: {decimals}")
    print(f"   Total Supply: {total_supply_tokens:,.0f} {symbol}")

    print(f"\n🔗 View on HashScan:")
    print(f"   https://hashscan.io/testnet/contract/{CONTRACT_ADDRESS}")

    print(f"\n✅ Ready for burns!")
    print(f"   1 {symbol} = 1 second of actuation")

except Exception as e:
    print(f"\n❌ Error reading contract: {e}")
    print("   Contract may not be deployed or ABI mismatch")

print("\n" + "=" * 70)
