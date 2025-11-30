# RPO Token Deployment Guide for Chromebook (Linux/Crostini)

## Prerequisites

### 1. Install Node.js and npm

```bash
# Update package list
sudo apt update

# Install Node.js (v18 or higher recommended)
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt install -y nodejs

# Verify installation
node --version  # Should show v20.x.x or higher
npm --version   # Should show 10.x.x or higher
```

### 2. Install Git (if not already installed)

```bash
sudo apt install -y git

# Verify installation
git --version
```

## Clone the Repository

```bash
# Clone the repository
git clone https://github.com/STLNFTART/MotorHandPro.git

# Navigate to the project directory
cd MotorHandPro

# Checkout the deployment branch
git checkout claude/deploy-rpo-token-01SdxJTDSkwoG4b4eLeGDe9x
```

## Setup and Configuration

### 1. Install Dependencies

```bash
# Install all npm packages
npm install
```

This will install:
- Hardhat
- OpenZeppelin contracts
- Ethers.js
- TypeScript dependencies

### 2. Configure Environment Variables

The `.env` file has already been created with your wallet addresses. Verify it contains:

```bash
cat .env
```

You should see:
```
PRIVATE_KEY=0x084e30a41a0e5fc01586d0f93f612bc5e44b6b3e99ec5786befc8eb0dc10fbb9
HEDERA_TESTNET_RPC=https://testnet.hashio.io/api
FOUNDER=0x536f51e53111755f9d1327d41fe6b21a9b2b2ba1
TREASURY=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed
LEGAL=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed
COMMUNITY=0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed
```

**⚠️ SECURITY WARNING:** Never commit your `.env` file with real private keys to GitHub!

## Compile the Smart Contracts

```bash
npm run compile
```

Expected output:
```
Compiled 1 Solidity file successfully
```

This creates:
- `artifacts/` - Compiled contract artifacts
- `cache/` - Hardhat cache files
- `typechain-types/` - TypeScript type definitions

## Deploy to Hedera Testnet

### Deploy the RPO Token

```bash
npm run deploy:testnet
```

Expected output:
```
🚀 DEPLOYING RECURSIVE PLANCK OPERATOR ($RPO)...
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Deploying from: 0x...
Balance: X.XX HBAR
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

📍 TOKEN ALLOCATION ADDRESSES:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Immediate Release (35% split):
  ├─ Founder (17.5B): 0x536f51e531...
  └─ Specified (17.5B): 0x5303AfC8ef...
Founder/Team Vesting (25%): 0x536f51e531...
Treasury (50%): 0x27aa333e75...
...

⏳ Deploying 100,000,000,000 $RPO with allocation structure...

✅ $RPO IS LIVE ON HEDERA TESTNET
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Contract: 0x...
HashScan → https://hashscan.io/testnet/contract/0x...

📊 TOKEN ALLOCATION (100 Billion $RPO)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
💰 Immediate Release: 35B (35%) - SPLIT:
   ├─ Founder: 17.5B → 0x536f51e5...
   └─ Specified: 17.5B → 0x5303AfC8...

⚡ Executing immediate release of 35B tokens (split)...
✅ Immediate release executed!
   ├─ Founder (0x536f51e5...) received 17,500,000,000 $RPO
   └─ Specified (0x5303AfC8...) received 17,500,000,000 $RPO
```

### What Happens During Deployment

1. **Contract Deployment**: The RecursivePlanckOperator contract is deployed to Hedera testnet
2. **Token Minting**: 100 billion $RPO tokens are minted to the contract
3. **Vesting Setup**: All vesting schedules are configured for the 5 allocation wallets
4. **Immediate Release**: Automatically executes, sending:
   - 17.5B $RPO → Founder wallet
   - 17.5B $RPO → Specified address (0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed)

## Verify Deployment

### Check Contract on HashScan

1. Copy the contract address from the deployment output
2. Visit: `https://hashscan.io/testnet/contract/[CONTRACT_ADDRESS]`
3. Verify:
   - Contract is deployed
   - Token name: "Recursive Planck Operator"
   - Symbol: "RPO"
   - Total supply: 100,000,000,000

### Check Token Balances

Visit HashScan and search for the wallet addresses to verify:

**Founder Wallet** (`0x536f51e53111755f9d1327d41fe6b21a9b2b2ba1`):
- Should have: 17,500,000,000 $RPO (immediate release)
- Plus: 25B $RPO locked in vesting (1yr cliff + 1.5yr vest)

**Specified Address** (`0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed`):
- Should have: 17,500,000,000 $RPO (immediate release)

## Post-Deployment Actions

### 1. Distribute Treasury (Manual)

The treasury allocation (50B tokens) requires a manual transaction:

```bash
# Connect to the deployed contract and call distributeTreasury()
# This requires using Hardhat console or a script
npx hardhat console --network hederaTestnet
```

Then in the console:
```javascript
const RPO = await ethers.getContractFactory("RecursivePlanckOperator");
const rpo = await RPO.attach("YOUR_CONTRACT_ADDRESS");
await rpo.distributeTreasury();
```

### 2. Claim Vested Tokens (Over Time)

Beneficiaries can claim their vested tokens after the cliff period using:

- `releaseFounderTokens(address)` - After 1 year
- `releaseCommunityTokens(address)` - Immediately (20%) then linear over 1 year
- `releaseLegalTokens(address)` - After 6 months
- `releaseDevelopmentTokens(address)` - Linear over 2 years

## Deploy to Mainnet

**⚠️ WARNING:** Only deploy to mainnet when you're ready for production!

### Before Mainnet Deployment

1. **Test thoroughly on testnet**
2. **Verify all wallet addresses are correct**
3. **Ensure deployer wallet has sufficient HBAR for gas fees**
4. **Double-check the PRIVATE_KEY in .env is correct**

### Mainnet Deployment

```bash
npm run deploy:mainnet
```

The deployment process is identical to testnet, but uses Hedera mainnet.

## Troubleshooting

### Error: "insufficient funds for gas"

**Solution:** Add more HBAR to your deployer wallet. You need at least 5-10 HBAR for deployment.

### Error: "network not found"

**Solution:** Make sure your `.env` file has the correct RPC URL:
```
HEDERA_TESTNET_RPC=https://testnet.hashio.io/api
```

### Error: "invalid private key"

**Solution:** Verify your PRIVATE_KEY in `.env` starts with `0x` and is 64 characters (excluding the 0x prefix).

### Compilation Errors

**Solution:**
```bash
# Clear cache and recompile
rm -rf cache/ artifacts/
npm run compile
```

## Additional Resources

- **Hedera Documentation**: https://docs.hedera.com
- **HashScan Explorer**: https://hashscan.io
- **Hardhat Documentation**: https://hardhat.org/docs
- **OpenZeppelin Contracts**: https://docs.openzeppelin.com/contracts

## Support

For issues or questions, open an issue at:
https://github.com/STLNFTART/MotorHandPro/issues
