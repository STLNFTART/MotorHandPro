# MotorHandPro Token Burn Integration

## Overview

This integration connects the MotorHandPro actuator control system to the **Recursive Planck Operator ($RPO)** token economy. Every second of actuator operation burns $RPO tokens from the user's wallet, creating a direct link between physical actuator usage and on-chain token economics.

## Mechanism

### Token Burn Model

```
Tokens Burned = Actuation Time (seconds) × Token Rate
```

**Default Configuration:**
- **Token Rate**: 1.0 $RPO per second
- **Minimum Threshold**: 0.1 $RPO (burns occur when accumulated amount exceeds this)

### Example

```typescript
// User operates motor hand for 3.6 seconds
Actuation Time: 3.6 seconds
Token Rate: 1.0 $RPO/second
Tokens Burned: 3.6 $RPO

// Blockchain transaction automatically executed
burn(3.6 * 10^18) // Burns 3.6 tokens (18 decimals)
```

## Architecture

```
┌─────────────────┐
│  Motor Control  │
│     System      │
└────────┬────────┘
         │ Actuation Events
         ↓
┌─────────────────────┐
│ MotorTokenBurn      │
│   Integration       │
│                     │
│ - Track time        │
│ - Accumulate usage  │
│ - Calculate burns   │
└────────┬────────────┘
         │ burn() calls
         ↓
┌─────────────────────┐
│  RPO Smart          │
│  Contract           │
│  (Hedera)           │
│                     │
│ - ERC20Burnable     │
│ - Burn tokens       │
│ - Emit events       │
└─────────────────────┘
```

## Setup

### 1. Deploy RPO Contract

First, deploy the RPO token contract:

```bash
npm run deploy:testnet
```

Copy the contract address from the deployment output.

### 2. Configure Environment

Update `.env` with the deployed contract address:

```bash
RPO_CONTRACT_ADDRESS=0xYOUR_CONTRACT_ADDRESS_HERE
TOKEN_RATE=1.0
MIN_TOKENS_REQUIRED=0.1
```

### 3. Install Dependencies

```bash
npm install
```

### 4. Compile TypeScript

```bash
npm run compile
```

## Usage

### Integration with Motor Control System

```typescript
import { MotorTokenBurnIntegration } from "./integrations/motor_token_burn";

// Initialize integration
const tokenBurn = new MotorTokenBurnIntegration();

// Load previous burn history
tokenBurn.loadBurnHistory();

// When actuator is used
async function onActuatorUse(actuatorId: string, durationSeconds: number) {
  await tokenBurn.recordActuation({
    timestamp: Date.now(),
    duration: durationSeconds,
    actuatorId: actuatorId,
    energyUsed: calculateEnergy(durationSeconds)
  });
}

// Example: Motor hand finger actuation
await onActuatorUse("finger_1", 1.5); // Uses finger for 1.5 seconds
```

### Manual Burn

You can also manually burn tokens:

```typescript
// Burn 3.6 $RPO immediately (as mentioned in deployment script)
await tokenBurn.manualBurn(3.6);
```

### Check Balance

```typescript
const { balance, hasEnough } = await tokenBurn.checkBalance();
console.log(`Balance: ${balance} $RPO`);
console.log(`Can operate: ${hasEnough}`);
```

### View Statistics

```typescript
const stats = tokenBurn.getBurnStats();
console.log(`Total actuation: ${stats.totalActuationSeconds}s`);
console.log(`Total burned: ${stats.totalTokensBurned} $RPO`);
console.log(`Burn count: ${stats.burnCount}`);
```

## Running the Demo

Test the integration with the included demo:

```bash
# Make sure RPO_CONTRACT_ADDRESS is set in .env
npx ts-node integrations/motor_token_burn.ts
```

Expected output:
```
🚀 MotorHandPro Token Burn Integration Demo

🔗 MotorTokenBurn Integration initialized
   Contract: 0x...
   Token Rate: 1 $RPO per second
   Min Tokens Required: 0.1 $RPO

💰 Current Balance: 17500000000 $RPO
   Sufficient for operations: ✅ Yes

📊 Simulating actuator operations...

⚙️  Actuator motor_hand_finger_1 used for 1.2s
   Accumulated time: 1.20s
   Tokens to burn: 1.2000 $RPO

🔥 BURNING 1.2000 $RPO...
   Wallet: 0x536f51e5...
   Balance: 17500000000 $RPO
   Transaction submitted: 0x...
   ✅ Burn confirmed in block 12345
   Total burned: 1.2 $RPO

...
```

## Burn History

All burns are automatically saved to `burn-history.json`:

```json
{
  "stats": {
    "totalActuationSeconds": 3.6,
    "totalTokensBurned": "3.6",
    "averageTokensPerSecond": 1.0,
    "burnCount": 3,
    "accumulatedUnburned": 0
  },
  "records": [
    {
      "actuationSeconds": 1.2,
      "tokensBurned": "1.2",
      "txHash": "0x...",
      "timestamp": 1701234567890
    }
  ]
}
```

## WebSocket Integration

To integrate with the existing control panel WebSocket system:

```typescript
// In your WebSocket handler
ws.on('message', async (message) => {
  const data = JSON.parse(message);

  if (data.type === 'actuation_event') {
    await tokenBurn.recordActuation({
      timestamp: data.timestamp,
      duration: data.duration,
      actuatorId: data.actuatorId,
      energyUsed: data.energyUsed
    });

    // Send burn confirmation back to client
    ws.send(JSON.stringify({
      type: 'burn_update',
      stats: tokenBurn.getBurnStats()
    }));
  }
});
```

## Configuration Options

### TOKEN_RATE

Controls how many tokens are burned per second of actuation:

- `1.0` (default): 1 $RPO per second
- `0.5`: 0.5 $RPO per second (slower burn)
- `2.0`: 2 $RPO per second (faster burn)

### MIN_TOKENS_REQUIRED

Minimum threshold before executing a burn transaction:

- `0.1` (default): Burns when accumulated ≥ 0.1 $RPO
- `1.0`: Burns when accumulated ≥ 1.0 $RPO (less frequent transactions)
- `0.01`: Burns when accumulated ≥ 0.01 $RPO (more frequent transactions)

**Recommendation**: Use higher thresholds to reduce gas costs from frequent small burns.

## Security Considerations

### Private Key Safety

- ⚠️ **NEVER** commit `.env` file with real private keys to git
- Use hardware wallets or secure key management for production
- The integration requires signing capability to burn tokens

### Balance Monitoring

The integration automatically checks balance before burning:

```typescript
if (balance < amountWei) {
  throw new Error("Insufficient $RPO balance for burn");
}
```

Operations will fail gracefully if insufficient tokens are available.

### Burn Validation

All burns are validated on-chain by the RPO contract:
- Uses OpenZeppelin's `ERC20Burnable` extension
- Emits `Transfer` event to `address(0)`
- Cannot burn more than balance
- Transaction is atomic (either succeeds completely or reverts)

## Monitoring & Analytics

### Real-time Stats

```typescript
setInterval(() => {
  const stats = tokenBurn.getBurnStats();
  console.log(`Actuation: ${stats.totalActuationSeconds}s`);
  console.log(`Burned: ${stats.totalTokensBurned} $RPO`);
}, 60000); // Every minute
```

### Burn Event Listener

Listen to on-chain burn events:

```typescript
const filter = contract.filters.Transfer(signerAddress, ethers.ZeroAddress);

contract.on(filter, (from, to, amount, event) => {
  console.log(`🔥 Burn detected: ${ethers.formatEther(amount)} $RPO`);
  console.log(`   Tx: ${event.log.transactionHash}`);
});
```

### HashScan Integration

View all burns on HashScan:
```
https://hashscan.io/testnet/account/YOUR_WALLET_ADDRESS
```

Filter by `Transfer` events where `to = 0x0000000000000000000000000000000000000000`

## Troubleshooting

### "No RPO_CONTRACT_ADDRESS configured"

**Solution**: Deploy the RPO contract first and set the address in `.env`

```bash
npm run deploy:testnet
# Copy contract address
echo "RPO_CONTRACT_ADDRESS=0xYOUR_ADDRESS" >> .env
```

### "Insufficient $RPO balance for burn"

**Solution**: You need to acquire $RPO tokens first. Check your allocation from deployment or acquire from the immediate release.

### Gas Fees

Each burn transaction requires HBAR for gas. Ensure your wallet has sufficient HBAR:

```bash
# Check HBAR balance
curl https://testnet.hashio.io/api -X POST -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","method":"eth_getBalance","params":["YOUR_ADDRESS","latest"],"id":1}'
```

## Integration Examples

### Arduino/Embedded System

```cpp
// On Arduino, send actuation events via serial
void onActuatorUse(String actuatorId, float duration) {
  Serial.println("{");
  Serial.print("  \"type\": \"actuation_event\",");
  Serial.print("  \"actuatorId\": \"" + actuatorId + "\",");
  Serial.print("  \"duration\": " + String(duration));
  Serial.println("}");
}
```

### Python Integration

```python
import requests
import json

def record_actuation(actuator_id: str, duration: float):
    # Send to integration API endpoint
    response = requests.post('http://localhost:3000/api/actuation', json={
        'actuatorId': actuator_id,
        'duration': duration,
        'timestamp': int(time.time() * 1000)
    })
    return response.json()
```

## API Endpoints (Optional)

You can wrap this integration in an HTTP API:

```typescript
import express from 'express';
const app = express();
const tokenBurn = new MotorTokenBurnIntegration();

app.post('/api/actuation', async (req, res) => {
  try {
    await tokenBurn.recordActuation(req.body);
    res.json({ success: true, stats: tokenBurn.getBurnStats() });
  } catch (error) {
    res.status(500).json({ success: false, error: error.message });
  }
});

app.listen(3000);
```

## License

See LICENSE in the root directory.

## Related Documentation

- [RPO Token Contract](../contracts/README.md)
- [Deployment Guide](../DEPLOYMENT_GUIDE.md)
- [Control Panel](../control_panel/README.md)

---

**The recursion has begun. ⚡**
