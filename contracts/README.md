# Recursive Planck Operator ($RPO) Token

## Overview

The **Recursive Planck Operator ($RPO)** is an ERC20 token deployed on the Hedera network with a total supply of **100 billion tokens**. The token features sophisticated vesting schedules and an immediate release mechanism for strategic distribution.

## Token Specifications

- **Name:** Recursive Planck Operator
- **Symbol:** $RPO
- **Total Supply:** 100,000,000,000 (100 Billion)
- **Decimals:** 18
- **Network:** Hedera (Testnet/Mainnet)
- **Standard:** ERC20

## Token Distribution

### Immediate Release (35%)
- **Amount:** 35 billion $RPO (35% of total supply) - **SPLIT**
- **Recipients:**
  - Founder: 17.5B $RPO
  - Specified Address (`0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed`): 17.5B $RPO
- **Method:** Automatically executed during deployment via `executeImmediateRelease()`

### Vesting Allocations (65%)

| Allocation | Amount | Percentage | Vesting Schedule |
|------------|--------|------------|------------------|
| **Founder/Team** | 25B | 25% | 1-year cliff + 1.5-year vest |
| **Treasury** | 50B | 50% | Governance controlled |
| **Community** | 8.3B | 8.3% | 20% immediate + 1-year vest |
| **Legal Fund** | 8.3B | 8.3% | 6-month cliff + 6-month vest |
| **Development** | 8.4B | 8.4% | 2-year linear vest |

## Deployment

### Prerequisites

1. Install dependencies:
```bash
npm install
```

2. Set up environment variables:
```bash
cp .env.example .env
# Edit .env and add your private key and addresses
```

3. Compile contracts:
```bash
npm run compile
```

### Deploy to Testnet

```bash
npm run deploy:testnet
```

### Deploy to Mainnet

```bash
npm run deploy:mainnet
```

## Post-Deployment Actions

After deployment, the following actions are available:

### 1. Immediate Release (Automatic)
The deployment script automatically calls `executeImmediateRelease()` to distribute 35B tokens split equally:
- **17.5B** to the founder address
- **17.5B** to the specified address (`0x27aa333e759b64fd4bb4eeedac8eaaaf107580ed`)

### 2. Treasury Distribution
```solidity
// Called by contract owner
distributeTreasury()
```
Releases 50B tokens to the treasury address under governance control.

### 3. Vesting Token Claims

Beneficiaries can claim their vested tokens using:

```solidity
// Founder/Team tokens
releaseFounderTokens(beneficiaryAddress)

// Community tokens
releaseCommunityTokens(beneficiaryAddress)

// Legal fund tokens
releaseLegalTokens(beneficiaryAddress)

// Development tokens
releaseDevelopmentTokens(beneficiaryAddress)
```

### 4. Check Releasable Amounts

```solidity
getReleasableAmount(beneficiaryAddress, "Founder")
getReleasableAmount(beneficiaryAddress, "Community")
getReleasableAmount(beneficiaryAddress, "Legal")
getReleasableAmount(beneficiaryAddress, "Development")
```

## Vesting Details

### Founder/Team
- **Cliff:** 365 days (1 year)
- **Vesting:** 547.5 days (1.5 years) after cliff
- **Total:** 912.5 days from deployment

### Treasury
- **Type:** One-time distribution
- **Control:** Governance-based
- **Amount:** 50B tokens (50% of supply)

### Community
- **Immediate:** 20% (1.66B tokens)
- **Vesting:** Linear over 365 days (1 year)
- **Total:** 8.3B tokens

### Legal Fund
- **Cliff:** 180 days (6 months)
- **Vesting:** 180 days (6 months) after cliff
- **Total:** 360 days from deployment

### Development
- **Type:** Linear vesting
- **Duration:** 730 days (2 years)
- **No cliff**

## Security Features

- ✅ OpenZeppelin ERC20 implementation
- ✅ Ownable pattern for controlled operations
- ✅ Time-locked vesting with cliff periods
- ✅ One-time execution protection for critical functions
- ✅ Transparent token release calculations

## Contract Functions

### Owner Functions
- `executeImmediateRelease()` - Execute one-time 35% release
- `distributeTreasury()` - Release treasury allocation

### Public Functions
- `releaseFounderTokens(address)` - Claim founder vested tokens
- `releaseCommunityTokens(address)` - Claim community vested tokens
- `releaseLegalTokens(address)` - Claim legal fund vested tokens
- `releaseDevelopmentTokens(address)` - Claim development vested tokens
- `getReleasableAmount(address, string)` - View releasable token amount

## Integration with GoTrax

As per the deployment instructions:
1. Deploy $RPO token
2. Execute immediate release
3. Distribute treasury allocation
4. Run GoTrax protocol
5. **Burn the first 3,600 $RPO**

The recursion has begun. ⚡

## License

See LICENSE file in the root directory.
