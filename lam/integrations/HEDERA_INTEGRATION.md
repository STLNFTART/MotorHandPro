# Hedera Blockchain Integration for MotorHandPro

## Overview

This integration connects MotorHandPro's actuator control system with the Hedera Hashgraph network, enabling blockchain-based token burn mechanisms for robot actuation.

**Token Economy:** 1 Token = 1 Second of perfectly smooth robotic actuation

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                  MotorHandPro LAM System                    │
│                                                             │
│  ┌──────────────────────────────────────────────┐          │
│  │    GoTrax Hoverboard Controller              │          │
│  │    (Primal Logic Control Framework)          │          │
│  └──────────────┬───────────────────────────────┘          │
│                 │                                           │
│                 ▼                                           │
│  ┌──────────────────────────────────────────────┐          │
│  │    HederaSmartContractInterface              │          │
│  │    - Token burn operations                   │          │
│  │    - Balance queries                         │          │
│  │    - Transaction management                  │          │
│  └──────────────┬───────────────────────────────┘          │
└─────────────────┼───────────────────────────────────────────┘
                  │
                  │ Hedera SDK
                  ▼
┌─────────────────────────────────────────────────────────────┐
│              Hedera Hashgraph Network                       │
│                                                             │
│  ┌──────────────────────────────────────────────┐          │
│  │    Smart Contract (Solidity)                 │          │
│  │    - balanceOf(address) → uint256            │          │
│  │    - burnTokens(uint256, uint256)            │          │
│  └──────────────────────────────────────────────┘          │
│                                                             │
│  Network: Testnet (testing) / Mainnet (production)         │
└─────────────────────────────────────────────────────────────┘
```

## Features

### ✅ Implemented

- **Real Hedera Network Integration**: Direct connection to Hedera testnet/mainnet
- **Smart Contract Execution**: Execute token burn transactions on-chain
- **Balance Queries**: Query token balances from smart contract
- **Dual Mode Operation**:
  - Production mode: Real blockchain transactions
  - Mock mode: Simulated transactions for testing
- **Environment Configuration**: Secure credential management via .env
- **Automatic Fallback**: Gracefully degrades to mock mode if SDK not installed
- **Multi-network Support**: Testnet and mainnet compatibility

### 🔧 Smart Contract Interface

The integration expects a Hedera smart contract with the following interface:

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract ActuationTokenBurn {

    /**
     * @dev Get token balance for an account
     * @param account The account address
     * @return Token balance in smallest unit (8 decimals)
     */
    function balanceOf(address account) external view returns (uint256);

    /**
     * @dev Burn tokens for actuation
     * @param amount Amount of tokens to burn (in smallest unit)
     * @param duration Duration of actuation in milliseconds
     * @return Success status
     */
    function burnTokens(uint256 amount, uint256 duration) external returns (bool);
}
```

## Setup Instructions

### 1. Install Dependencies

```bash
# From the repository root
pip install -r lam/requirements.txt
```

This will install the Hedera SDK:
```
hedera-sdk-py>=2.0.0
```

### 2. Create Hedera Account

#### For Testing (Testnet):
1. Visit [Hedera Portal](https://portal.hedera.com/register)
2. Create a testnet account
3. Note your Account ID (format: `0.0.12345`)
4. Save your private key securely

#### For Production (Mainnet):
1. Visit [Hedera.com](https://hedera.com/)
2. Follow mainnet account creation process
3. Fund your account with HBAR for transaction fees

### 3. Deploy Smart Contract

You need to deploy your token burn smart contract to Hedera:

```bash
# Using Hedera CLI or SDK
# Example contract deployment (pseudocode)
hedera contract create \
  --bytecode ./build/ActuationTokenBurn.bin \
  --gas 100000
```

Note the Contract ID (format: `0.0.67890`)

### 4. Configure Environment Variables

Copy the example environment file:
```bash
cp lam/.env.example lam/.env
```

Edit `lam/.env` with your credentials:
```bash
# Network configuration
HEDERA_NETWORK=testnet  # or "mainnet"

# Your Hedera account credentials
HEDERA_ACCOUNT_ID=0.0.12345
HEDERA_PRIVATE_KEY=302e020100300506032b65700422042012345...

# Your deployed contract
HEDERA_CONTRACT_ID=0.0.67890

# Token configuration
TOKEN_BURN_RATE=1.0
MIN_TOKENS_REQUIRED=0.1

# Set to false for production
HEDERA_MOCK_MODE=false
```

### 5. Test Connection

Run the test script:
```bash
python -m lam.integrations.gotrax_hoverboard_integration
```

Or test configuration:
```bash
python -m lam.integrations.hedera_config
```

## Usage Examples

### Basic Usage (with environment config)

```python
from lam.integrations import LAMHoverboardInterface
from lam.integrations.hedera_config import load_hedera_config

# Load configuration from environment
hedera_config = load_hedera_config()

# Initialize interface
interface = LAMHoverboardInterface(hedera_config=hedera_config)

# Execute movement (will burn tokens on Hedera)
result = await interface.execute_move(
    mode="forward",
    duration=2.0,  # 2 seconds
    power=0.6
)

print(f"Success: {result['success']}")
print(f"Tokens burned: {result['tokens_burned']}")
print(f"Transaction ID: {result.get('transaction_id')}")
```

### Manual Configuration

```python
from lam.integrations import LAMHoverboardInterface

# Manual configuration
hedera_config = {
    "contract_id": "0.0.67890",
    "network": "testnet",
    "account_id": "0.0.12345",
    "private_key": "302e020100300506032b65700422042012345..."
}

interface = LAMHoverboardInterface(hedera_config=hedera_config)

# Execute actuation
result = await interface.execute_move("forward", 2.0, power=0.6)
```

### Mock Mode (for testing)

```python
# Run without real blockchain transactions
interface = LAMHoverboardInterface()  # No config = mock mode

# Deposit mock tokens
interface.deposit_tokens(100.0)

# Execute movement (simulated)
result = await interface.execute_move("forward", 2.0)
# Result will have mock_mode: True
```

## Configuration Reference

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `HEDERA_NETWORK` | No | `testnet` | Network: `testnet` or `mainnet` |
| `HEDERA_ACCOUNT_ID` | Yes* | - | Your Hedera account ID (e.g., `0.0.12345`) |
| `HEDERA_PRIVATE_KEY` | Yes* | - | Your account private key (DER hex) |
| `HEDERA_CONTRACT_ID` | Yes* | - | Smart contract ID (e.g., `0.0.67890`) |
| `HEDERA_MOCK_MODE` | No | `false` | Enable mock mode (`true`/`false`) |
| `TOKEN_BURN_RATE` | No | `1.0` | Tokens per second of actuation |
| `MIN_TOKENS_REQUIRED` | No | `0.1` | Minimum tokens for operation |
| `HEDERA_QUERY_GAS` | No | `100000` | Gas for contract queries |
| `HEDERA_EXECUTE_GAS` | No | `200000` | Gas for contract executions |

\* Required unless `HEDERA_MOCK_MODE=true`

## Transaction Flow

### Token Burn Transaction

1. **Request Actuation**: User requests movement (e.g., 2 seconds forward)
2. **Calculate Required Tokens**: `tokens = duration × token_rate` (e.g., 2 × 1.0 = 2 tokens)
3. **Check Balance**: Query smart contract for available balance
4. **Execute Contract**: Call `burnTokens(amount, duration)` on Hedera
5. **Wait for Receipt**: Confirm transaction success on-chain
6. **Execute Movement**: Perform robotic actuation
7. **Return Result**: Transaction ID, tokens burned, new balance

### Balance Query

1. **Create Query**: `ContractCallQuery` with `balanceOf(address)`
2. **Execute Query**: Read-only operation (no transaction fee)
3. **Parse Result**: Convert from smallest unit to tokens (8 decimals)
4. **Return Balance**: Available tokens

## Error Handling

The integration handles various error scenarios:

### Insufficient Balance
```python
{
    "success": False,
    "error": "Insufficient balance",
    "requested": 5.0,
    "available": 2.0
}
```

### Transaction Failure
```python
{
    "success": False,
    "error": "Hedera transaction failed: <details>",
    "timestamp": "2025-11-28T12:34:56"
}
```

### SDK Not Installed
```
⚠️  Hedera SDK not installed. Running in MOCK MODE
   Install with: pip install hedera-sdk-py
```

## Security Considerations

### 🔐 Private Key Security

- **NEVER** commit `.env` file to version control
- Store private keys securely (use key management service in production)
- Rotate keys periodically
- Use separate accounts for testnet and mainnet

### 🛡️ Transaction Safety

- All transactions require valid account signatures
- Smart contract validates token ownership
- Gas limits prevent runaway transactions
- Receipts confirm on-chain execution

### 🔍 Audit Trail

Every token burn creates an immutable record on Hedera:
- Transaction ID
- Timestamp
- Amount burned
- Actuation duration
- Account address

## Troubleshooting

### "Mock mode" despite configuration

**Cause**: Missing or invalid credentials

**Solution**:
1. Check `.env` file exists in `lam/` directory
2. Verify all required variables are set
3. Validate Hedera ID format (`0.0.12345`)
4. Run: `python -m lam.integrations.hedera_config`

### "Transaction failed" errors

**Possible causes**:
- Insufficient HBAR balance for gas fees
- Invalid contract ID
- Contract function signature mismatch
- Network connectivity issues

**Solution**:
1. Check HBAR balance: Account needs HBAR for transaction fees
2. Verify contract ID is correct
3. Ensure contract implements expected interface
4. Test network connectivity

### "Import Error" for hedera module

**Cause**: Hedera SDK not installed

**Solution**:
```bash
pip install hedera-sdk-py>=2.0.0
```

## Performance Metrics

### Transaction Latency
- **Testnet**: ~3-5 seconds for transaction confirmation
- **Mainnet**: ~3-5 seconds for transaction confirmation
- **Query**: <1 second (read-only)

### Gas Costs
- **Balance Query**: Free (read-only)
- **Token Burn**: ~0.001-0.01 HBAR per transaction (depends on contract complexity)

### Throughput
- Hedera TPS: 10,000+ transactions per second network-wide
- Single account limit: ~1000 TPS

## Development Roadmap

### Future Enhancements

- [ ] Batch token burns for efficiency
- [ ] Event logging from smart contract
- [ ] Token purchase/deposit integration
- [ ] Multi-token support
- [ ] Advanced gas estimation
- [ ] Webhook notifications for transactions
- [ ] Dashboard for token usage analytics

## Support and Resources

### Hedera Resources
- [Hedera Documentation](https://docs.hedera.com/)
- [Hedera Python SDK](https://github.com/hashgraph/hedera-sdk-py)
- [Smart Contract Guide](https://docs.hedera.com/guides/smart-contracts)
- [Testnet Portal](https://portal.hedera.com/)

### MotorHandPro
- Integration Code: `lam/integrations/gotrax_hoverboard_integration.py`
- Configuration: `lam/integrations/hedera_config.py`
- Tests: `lam/tests/test_hoverboard_integration.py`

## License

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
