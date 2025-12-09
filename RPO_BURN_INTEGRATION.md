# $RPO Token Burn Integration

## Real On-Chain Token Burns When Actuators Are Used

This integration enables **real Hedera blockchain token burns** when MotorHandPro actuators are activated.

**1 $RPO Token = 1 Second of Perfectly Smooth Robotic Actuation** 🔥

---

## How It Works

When you activate a GoTrax hoverboard motor (or any actuator):

```
Actuator Activation → Token Burn Transaction → Hedera Blockchain
```

1. **Request actuation** (e.g., 2 seconds forward)
2. **Calculate tokens needed** (2 seconds × 1 $RPO/sec = 2 $RPO)
3. **Burn tokens on-chain** via Hedera smart contract
4. **Execute smooth motion** using Primal Logic control
5. **Confirm burn** with transaction hash

---

## Setup

### 1. Deploy $RPO Contract (If Not Already Deployed)

```bash
# Install dependencies
npm install

# Compile contract
npm run compile

# Deploy to Hedera Testnet
npm run deploy:testnet
```

**Save the deployed contract address!**

### 2. Configure Environment

Create `.env` file (NEVER commit this):

```bash
# Hedera Configuration
HEDERA_RPC_URL=https://testnet.hashio.io/api
HEDERA_CONTRACT_ID=0xYOUR_DEPLOYED_CONTRACT_ADDRESS

# Your Private Key (KEEP SECRET!)
PRIVATE_KEY=0xYOUR_PRIVATE_KEY_HERE

# Token Allocation Addresses
FOUNDER=0x536f51e53111755f9d1327d41fe6b21a9b2b2ba1
TREASURY=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed
COMMUNITY=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed

# Burn Configuration
TOKEN_RATE=1.0  # 1 $RPO = 1 second
MIN_TOKENS_REQUIRED=0.1
USE_REAL_BURNS=true  # Set to true for real burns!
```

### 3. Test the Integration

```bash
# Set environment variable
export USE_REAL_BURNS=true

# Run test
python3 lam/integrations/gotrax_hoverboard_integration.py
```

**Expected output**:
```
🔥 REAL ON-CHAIN $RPO BURNS ENABLED
   Tokens will be burned on Hedera blockchain

🔥 Burned 2.0 $RPO on-chain!
   TX: 0x1234567890abcdef...
```

---

## Code Integration

### Python Example

```python
from lam.integrations.gotrax_hoverboard_integration import LAMHoverboardInterface
import asyncio

async def control_robot():
    # Initialize with real burns enabled
    interface = LAMHoverboardInterface(use_real_burns=True)

    # Deposit tokens (simulated balance for testing)
    interface.deposit_tokens(10.0)

    # Execute movement - THIS WILL BURN TOKENS ON-CHAIN!
    result = await interface.execute_move(
        mode="forward",
        duration=2.0,  # 2 seconds
        power=0.6
    )

    if result['success']:
        print(f"✅ Burned {result['tokens_burned']} $RPO")
        print(f"   Smoothness: {result['smoothness_score']:.2f}")

asyncio.run(control_robot())
```

### Direct RPO Burner

```python
from lam.integrations.hedera_rpo_burn import RPOTokenBurner

# Initialize
burner = RPOTokenBurner(
    contract_address="0xYOUR_CONTRACT",
    private_key="0xYOUR_KEY"
)

# Check balance
balance = burner.get_balance()
print(f"Balance: {balance} $RPO")

# Burn tokens
result = burner.burn_tokens(
    amount=1.0,  # 1 $RPO
    actuation_seconds=1.0  # For 1 second of actuation
)

if result['success']:
    print(f"Transaction: {result['transaction_hash']}")
    print(f"Explorer: {result['explorer_url']}")
```

---

## Security

### ⚠️ CRITICAL SECURITY NOTES

1. **NEVER commit private keys** to git
2. **NEVER share your `.env` file**
3. **Use environment variables** in production
4. **Rotate keys regularly**
5. **Use hardware wallets** for mainnet

### Secure Key Management

**Development/Testing**:
```bash
export PRIVATE_KEY=0x...
python3 your_script.py
```

**Production**:
- Use AWS Secrets Manager
- Use HashiCorp Vault
- Use Hedera native key management
- Use hardware security modules (HSM)

---

## File Structure

```
lam/integrations/
├── hedera_rpo_burn.py              # Real on-chain burning
├── gotrax_hoverboard_integration.py # Actuator control with burns
└── __init__.py

.env.production                      # Template (safe to commit)
.env                                 # YOUR CONFIG (NEVER COMMIT!)
```

---

## Testing Checklist

- [ ] Deploy $RPO contract to Hedera testnet
- [ ] Save contract address to `HEDERA_CONTRACT_ID`
- [ ] Set `PRIVATE_KEY` environment variable
- [ ] Run: `python3 lam/integrations/hedera_rpo_burn.py`
- [ ] Verify balance shows correctly
- [ ] Execute test burn (0.1 $RPO)
- [ ] Verify transaction on HashScan
- [ ] Test GoTrax integration
- [ ] Verify smooth trajectory with Primal Logic
- [ ] Check tokens burned on-chain

---

## Transaction Flow

```
┌─────────────────────────────────┐
│  Actuator Request (2 sec)       │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Calculate: 2 sec × 1 $RPO/sec  │
│  = 2 $RPO to burn               │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Build Burn Transaction         │
│  - Contract: RecursivePlanckOp  │
│  - Function: burn(address, amt) │
│  - Amount: 2 * 10^18 (wei)      │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Sign with Private Key          │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Send to Hedera Network         │
│  RPC: testnet.hashio.io/api     │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Wait for Receipt               │
│  - Status: 1 (success)          │
│  - TX Hash: 0x1234...           │
│  - Gas Used: ~50,000            │
└────────────┬────────────────────┘
             │
             ▼
┌─────────────────────────────────┐
│  Execute Smooth Motion          │
│  - Primal Logic trajectory      │
│  - Exponential memory weight    │
│  - Lipschitz bound < 1          │
└─────────────────────────────────┘
```

---

## Burn Verification

View your burns on **HashScan**:

**Testnet**: https://hashscan.io/testnet/transaction/YOUR_TX_HASH

**Mainnet**: https://hashscan.io/mainnet/transaction/YOUR_TX_HASH

---

## Troubleshooting

### Error: "Insufficient balance"

```bash
# Check your balance
python3 -c "
from lam.integrations.hedera_rpo_burn import RPOTokenBurner
burner = RPOTokenBurner()
print(f'Balance: {burner.get_balance()} $RPO')
"
```

### Error: "Failed to connect to Hedera RPC"

Check your RPC URL:
```bash
curl https://testnet.hashio.io/api \
  -X POST \
  -H "Content-Type: application/json" \
  --data '{"method":"eth_blockNumber","params":[],"id":1,"jsonrpc":"2.0"}'
```

### Error: "HEDERA_CONTRACT_ID not set"

Deploy the contract first:
```bash
npm run deploy:testnet
```

---

## Performance

**Burn Transaction**:
- Gas: ~50,000 (approx)
- Time: 2-5 seconds
- Cost: Minimal HBAR for gas

**Primal Logic Control**:
- Update rate: 50 Hz (20ms intervals)
- Smoothness: Exponentially weighted
- Stability: Mathematically proven (Lipschitz < 1)

---

## Production Deployment

### Mainnet Deployment

```bash
# Deploy to mainnet
npm run deploy:mainnet

# Update environment
export HEDERA_RPC_URL=https://mainnet.hashio.io/api
export HEDERA_CONTRACT_ID=0xMAINNET_CONTRACT_ADDRESS
export USE_REAL_BURNS=true

# Run production
python3 your_production_script.py
```

### Monitoring

Monitor burns in real-time:

```python
from lam.integrations.hedera_rpo_burn import RPOTokenBurner

burner = RPOTokenBurner()

# Get status
status = burner.get_status()
print(f"Balance: {status['balance']} $RPO")
print(f"Max actuation: {status['max_actuation_seconds']} seconds")
```

---

## The Recursion Has Begun ⚡

As per deployment instructions:

> **4. Run your GoTrax. Burn the first 3,600 $RPO.**

**3,600 $RPO = 1 hour of smooth robotic actuation**

This is your initialization ritual. 🔥

---

## License

Patent Pending: U.S. Provisional Patent Application No. 63/842,846

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety

---

**Questions?** Contact: Donte Lightfoot (STLNFTART)
