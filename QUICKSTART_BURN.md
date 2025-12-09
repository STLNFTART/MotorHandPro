# 🔥 Quick Start: Real $RPO Burns

Your $RPO contract is **LIVE** on Hedera Testnet!

**Contract Address**: `0xa2e2b88620944085D390fdbe86CBa10A2f06a033`

---

## ⚡ Quick Start (3 Steps)

### Step 1: Set Your Private Key

```bash
# Set your private key as environment variable
export PRIVATE_KEY=0xYOUR_PRIVATE_KEY_HERE
```

**⚠️ Security**: NEVER commit your private key to git!

### Step 2: Enable Real Burns

```bash
export USE_REAL_BURNS=true
```

### Step 3: Run Actuators

```bash
# Test with GoTrax integration
python3 lam/integrations/gotrax_hoverboard_integration.py
```

**What happens**:
```
🔥 REAL ON-CHAIN $RPO BURNS ENABLED
   Tokens will be burned on Hedera blockchain

Executing forward movement (2 seconds)...
🔥 Burned 2.0 $RPO on-chain!
   TX: 0x1234567890abcdef...

✅ View: https://hashscan.io/testnet/transaction/0x...
```

---

## 📊 View Your Contract

**HashScan**: https://hashscan.io/testnet/contract/0xa2e2b88620944085D390fdbe86CBa10A2f06a033

Here you can see:
- Total supply (100 Billion $RPO)
- All burn transactions
- Token holders
- Contract verified status

---

## 🧪 Test Without Real Burns

To test the logic without spending $RPO:

```bash
# Don't set USE_REAL_BURNS (or set to false)
python3 lam/integrations/gotrax_hoverboard_integration.py
```

Output:
```
⚙️  SIMULATION MODE (set USE_REAL_BURNS=true for real burns)
```

---

## 🔥 Direct Burn Test

Test burning directly (outside of actuators):

```bash
# Set your private key
export PRIVATE_KEY=0xYOUR_KEY
export HEDERA_CONTRACT_ID=0xa2e2b88620944085D390fdbe86CBa10A2f06a033

# Run direct burn test
python3 lam/integrations/hedera_rpo_burn.py
```

This will:
1. Connect to the contract
2. Check your $RPO balance
3. Burn 0.1 $RPO (test amount)
4. Show transaction receipt

---

## 💰 Check Your Balance

```python
from lam.integrations.hedera_rpo_burn import RPOTokenBurner

# Initialize (loads contract address from .env.production)
burner = RPOTokenBurner()

# Get your balance
balance = burner.get_balance()
print(f"You have {balance} $RPO")
print(f"This enables {balance} seconds of actuation")
```

---

## 🚀 Full Example: Actuate with Real Burns

```python
import asyncio
import os
from lam.integrations.gotrax_hoverboard_integration import LAMHoverboardInterface

async def main():
    # Enable real burns
    os.environ['USE_REAL_BURNS'] = 'true'
    os.environ['PRIVATE_KEY'] = '0xYOUR_KEY'

    # Initialize (will connect to live contract)
    interface = LAMHoverboardInterface(use_real_burns=True)

    # Deposit tokens (for testing - sets local balance)
    interface.deposit_tokens(10.0)

    # Execute 2 seconds of movement - BURNS 2 $RPO ON-CHAIN!
    result = await interface.execute_move("forward", 2.0, power=0.6)

    if result['success']:
        print(f"✅ Burned {result['tokens_burned']} $RPO")
        print(f"   Smoothness: {result['smoothness_score']:.2%}")
        print(f"   Primal Logic: Lipschitz={result['primal_logic']['lipschitz_bound']}")

asyncio.run(main())
```

---

## 📋 Configuration Summary

Your setup:

```bash
# Network
HEDERA_NETWORK=testnet
HEDERA_RPC_URL=https://testnet.hashio.io/api

# Live Contract ✅
HEDERA_CONTRACT_ID=0xa2e2b88620944085D390fdbe86CBa10A2f06a033

# Your addresses (from deployment)
FOUNDER=0x536f51e53111755f9d1327d41fe6b21a9b2b2ba1
TREASURY=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed

# Burn rate
TOKEN_RATE=1.0  # 1 $RPO = 1 second
```

---

## 🔍 Verify It's Working

After a burn, check:

1. **HashScan Transaction**:
   - URL provided in output
   - Shows: from address, to address, amount

2. **Balance Decrease**:
   ```bash
   # Before: 10.0 $RPO
   # Burned: 2.0 $RPO
   # After: 8.0 $RPO
   ```

3. **Gas Paid**:
   - Small amount of HBAR for gas
   - Typically < 0.01 HBAR per burn

---

## ⚡ The Initialization Ritual

Per deployment instructions:

> **Burn the first 3,600 $RPO**

```python
# Burn 1 hour of actuation time
result = burner.burn_tokens(
    amount=3600.0,  # 3,600 $RPO
    actuation_seconds=3600.0  # 1 hour
)
```

**3,600 $RPO = 1 hour of perfectly smooth robotic actuation**

The recursion has begun. 🔥

---

## 🆘 Troubleshooting

**"Insufficient balance"**:
- Check: `burner.get_balance()`
- You need $RPO tokens in your wallet first

**"PRIVATE_KEY not set"**:
```bash
export PRIVATE_KEY=0xYOUR_KEY
```

**"Connection failed"**:
- Check internet connection
- Verify Hedera RPC is accessible
- Try different network/VPN

---

## 📚 More Info

- **Full Guide**: `RPO_BURN_INTEGRATION.md`
- **Contract Details**: `contracts/README.md`
- **Deployment**: `scripts/deploy-rpo.ts`

---

**Contract Live**: ✅
**Burns Ready**: ✅
**Primal Logic**: ✅
**Let's Go**: 🚀
