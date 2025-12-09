#!/usr/bin/env python3
"""
Hedera $RPO Token Burn Integration
Real on-chain token burning when actuators are used.

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""
import os
import json
from typing import Dict, Any, Optional
from web3 import Web3
from eth_account import Account
from dotenv import load_dotenv

# Load environment
load_dotenv()
load_dotenv('.env.production')


class RPOTokenBurner:
    """
    Real Hedera $RPO token burn integration.
    Burns tokens on-chain when actuators are activated.
    """

    # $RPO Token Contract ABI (minimal - just what we need for burning)
    RPO_ABI = [
        {
            "inputs": [
                {"internalType": "address", "name": "account", "type": "address"},
                {"internalType": "uint256", "name": "amount", "type": "uint256"}
            ],
            "name": "burn",
            "outputs": [],
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "inputs": [
                {"internalType": "address", "name": "account", "type": "address"}
            ],
            "name": "balanceOf",
            "outputs": [
                {"internalType": "uint256", "name": "", "type": "uint256"}
            ],
            "stateMutability": "view",
            "type": "function"
        },
        {
            "inputs": [],
            "name": "symbol",
            "outputs": [
                {"internalType": "string", "name": "", "type": "string"}
            ],
            "stateMutability": "view",
            "type": "function"
        },
        {
            "inputs": [],
            "name": "decimals",
            "outputs": [
                {"internalType": "uint8", "name": "", "type": "uint8"}
            ],
            "stateMutability": "view",
            "type": "function"
        }
    ]

    def __init__(self,
                 rpc_url: str = None,
                 contract_address: str = None,
                 private_key: str = None):
        """
        Initialize RPO token burner.

        Args:
            rpc_url: Hedera RPC endpoint (default from env)
            contract_address: $RPO contract address (from env)
            private_key: Private key for signing (from env or secure source)
        """
        self.rpc_url = rpc_url or os.getenv('HEDERA_RPC_URL', 'https://testnet.hashio.io/api')
        self.contract_address = contract_address or os.getenv('HEDERA_CONTRACT_ID')
        self.private_key = private_key or os.getenv('PRIVATE_KEY')

        # Validate configuration
        if not self.contract_address:
            raise ValueError("HEDERA_CONTRACT_ID must be set (deployed contract address)")

        if not self.private_key:
            raise ValueError("PRIVATE_KEY must be set for signing transactions")

        # Remove 0x prefix if present for private key
        if self.private_key.startswith('0x'):
            self.private_key = self.private_key[2:]

        # Initialize Web3
        self.w3 = Web3(Web3.HTTPProvider(self.rpc_url))

        if not self.w3.is_connected():
            raise ConnectionError(f"Failed to connect to Hedera RPC: {self.rpc_url}")

        # Get account from private key
        self.account = Account.from_key(self.private_key)
        self.address = self.account.address

        # Initialize contract
        self.contract = self.w3.eth.contract(
            address=Web3.to_checksum_address(self.contract_address),
            abi=self.RPO_ABI
        )

        # Get token info
        try:
            self.decimals = self.contract.functions.decimals().call()
            self.symbol = self.contract.functions.symbol().call()
        except Exception as e:
            print(f"Warning: Could not fetch token info: {e}")
            self.decimals = 18
            self.symbol = "RPO"

        print(f"🔥 RPO Token Burner initialized")
        print(f"   Network: Hedera Testnet")
        print(f"   RPC: {self.rpc_url}")
        print(f"   Contract: {self.contract_address}")
        print(f"   Burner Address: {self.address}")
        print(f"   Token: ${self.symbol} (decimals: {self.decimals})")

    def get_balance(self) -> float:
        """Get $RPO balance for burner account"""
        try:
            balance_wei = self.contract.functions.balanceOf(self.address).call()
            balance = balance_wei / (10 ** self.decimals)
            return balance
        except Exception as e:
            print(f"Error getting balance: {e}")
            return 0.0

    def burn_tokens(self, amount: float, actuation_seconds: float) -> Dict[str, Any]:
        """
        Burn $RPO tokens on-chain.

        Args:
            amount: Number of $RPO tokens to burn
            actuation_seconds: Duration of actuation this burn enables

        Returns:
            Transaction result with hash and details
        """
        try:
            # Convert to wei (token units)
            amount_wei = int(amount * (10 ** self.decimals))

            # Check balance
            balance = self.get_balance()
            if balance < amount:
                return {
                    'success': False,
                    'error': f'Insufficient balance. Have {balance} $RPO, need {amount} $RPO',
                    'balance': balance
                }

            # Build transaction
            nonce = self.w3.eth.get_transaction_count(self.address)

            # Get current gas price
            gas_price = self.w3.eth.gas_price

            # Build burn transaction
            txn = self.contract.functions.burn(
                self.address,
                amount_wei
            ).build_transaction({
                'from': self.address,
                'nonce': nonce,
                'gas': 100000,  # Estimate, will be refined
                'gasPrice': gas_price,
                'chainId': self.w3.eth.chain_id
            })

            # Estimate gas
            try:
                gas_estimate = self.w3.eth.estimate_gas(txn)
                txn['gas'] = int(gas_estimate * 1.2)  # Add 20% buffer
            except Exception as e:
                print(f"Gas estimation failed, using default: {e}")

            # Sign transaction
            signed_txn = self.w3.eth.account.sign_transaction(txn, self.private_key)

            # Send transaction
            tx_hash = self.w3.eth.send_raw_transaction(signed_txn.raw_transaction)
            tx_hash_hex = tx_hash.hex()

            print(f"🔥 Burning {amount} $RPO...")
            print(f"   Transaction: {tx_hash_hex}")

            # Wait for receipt
            receipt = self.w3.eth.wait_for_transaction_receipt(tx_hash, timeout=120)

            success = receipt['status'] == 1

            if success:
                new_balance = self.get_balance()
                print(f"✅ Burn successful!")
                print(f"   Burned: {amount} $RPO")
                print(f"   For: {actuation_seconds} seconds of actuation")
                print(f"   Gas used: {receipt['gasUsed']}")
                print(f"   New balance: {new_balance} $RPO")
            else:
                print(f"❌ Burn failed - transaction reverted")

            return {
                'success': success,
                'transaction_hash': tx_hash_hex,
                'tokens_burned': amount if success else 0,
                'actuation_seconds': actuation_seconds,
                'gas_used': receipt['gasUsed'],
                'block_number': receipt['blockNumber'],
                'new_balance': self.get_balance() if success else balance,
                'explorer_url': f"https://hashscan.io/testnet/transaction/{tx_hash_hex}"
            }

        except Exception as e:
            print(f"❌ Burn error: {e}")
            return {
                'success': False,
                'error': str(e),
                'tokens_burned': 0
            }

    def get_status(self) -> Dict[str, Any]:
        """Get burner status"""
        try:
            balance = self.get_balance()
            max_actuation = balance  # 1 token = 1 second

            return {
                'connected': self.w3.is_connected(),
                'network': 'Hedera Testnet',
                'rpc_url': self.rpc_url,
                'contract_address': self.contract_address,
                'burner_address': self.address,
                'token_symbol': self.symbol,
                'token_decimals': self.decimals,
                'balance': balance,
                'max_actuation_seconds': max_actuation,
                'token_rate': '1 $RPO = 1 second of actuation'
            }
        except Exception as e:
            return {
                'connected': False,
                'error': str(e)
            }


# Callback function for GoTrax integration
_burner_instance: Optional[RPOTokenBurner] = None

def initialize_rpo_burner(contract_address: str = None, private_key: str = None) -> RPOTokenBurner:
    """Initialize the global RPO burner instance"""
    global _burner_instance
    _burner_instance = RPOTokenBurner(
        contract_address=contract_address,
        private_key=private_key
    )
    return _burner_instance

def rpo_burn_callback(amount: float, actuation_seconds: float = None) -> Dict[str, Any]:
    """
    Callback function for GoTrax integration.
    Burns $RPO tokens on-chain.
    """
    if not _burner_instance:
        return {
            'success': False,
            'error': 'RPO burner not initialized. Call initialize_rpo_burner() first.'
        }

    if actuation_seconds is None:
        actuation_seconds = amount  # 1 token = 1 second

    return _burner_instance.burn_tokens(amount, actuation_seconds)


if __name__ == "__main__":
    """Test RPO burning"""
    import sys

    print("=" * 70)
    print("$RPO TOKEN BURN TEST")
    print("=" * 70)

    # Check if contract is deployed
    contract_id = os.getenv('HEDERA_CONTRACT_ID')
    if not contract_id:
        print("\n❌ HEDERA_CONTRACT_ID not set")
        print("\nDeploy the contract first:")
        print("  npm run compile")
        print("  npm run deploy:testnet")
        print("\nThen set HEDERA_CONTRACT_ID in .env.production")
        sys.exit(1)

    # Check for private key
    private_key = os.getenv('PRIVATE_KEY')
    if not private_key:
        print("\n❌ PRIVATE_KEY not set")
        print("\nSet PRIVATE_KEY environment variable:")
        print("  export PRIVATE_KEY=0x...")
        sys.exit(1)

    try:
        # Initialize burner
        burner = RPOTokenBurner()

        # Get status
        print("\n📊 Burner Status:")
        status = burner.get_status()
        print(json.dumps(status, indent=2))

        # Check balance
        balance = burner.get_balance()
        print(f"\n💰 Current Balance: {balance} $RPO")

        if balance < 0.1:
            print("\n⚠️  Insufficient balance for test burn")
            print("   Need at least 0.1 $RPO")
            sys.exit(0)

        # Test burn
        print("\n🔥 Test burning 0.1 $RPO (0.1 seconds of actuation)...")
        result = burner.burn_tokens(0.1, 0.1)
        print("\n📋 Burn Result:")
        print(json.dumps(result, indent=2))

        if result['success']:
            print(f"\n✅ SUCCESS!")
            print(f"   View on HashScan: {result['explorer_url']}")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        sys.exit(1)
