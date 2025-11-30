# 🔥 EASIEST WAY TO RUN $RPO BURNS

## One Simple Command:

```bash
./run_rpo_burns.sh
```

That's it! The script will:
1. ✅ Ask for your private key (securely, won't show on screen)
2. ✅ Set up all configuration automatically
3. ✅ Give you options to test

---

## What You'll See:

```
🔥 $RPO TOKEN BURN SETUP
========================================

Please enter your private key (it will not be displayed):
[type your key - it won't show]

✅ Private key set (hidden)

📋 Configuration:
   Contract: 0xa2e2b88620944085D390fdbe86CBa10A2f06a033
   Network: Hedera Testnet
   Real Burns: ENABLED 🔥

What would you like to do?

1) Test GoTrax integration (activate actuators)
2) Check your $RPO balance
3) Run direct burn test (0.1 $RPO)
4) Exit

Choose [1-4]:
```

---

## Options Explained:

### Option 1: GoTrax Integration
- Activates hoverboard actuators
- Burns $RPO for each second of actuation
- Shows real-time burns with transaction hashes

### Option 2: Check Balance
- Shows your current $RPO balance
- Calculates how much actuation time you have
- No burning, just informational

### Option 3: Direct Burn Test
- Burns exactly 0.1 $RPO as a test
- Shows full transaction details
- Returns HashScan link

---

## Alternative: Manual Setup

If you prefer to set it manually:

```bash
# Set your private key
export PRIVATE_KEY=0xYOUR_KEY_HERE

# Run any script
python3 lam/integrations/gotrax_hoverboard_integration.py
```

---

## Troubleshooting

**If you get "command not found":**
```bash
cd /home/dontelightfoot/PrimalRWA/MotorHandPro
./run_rpo_burns.sh
```

**If you get "permission denied":**
```bash
chmod +x run_rpo_burns.sh
./run_rpo_burns.sh
```

**If python3-dotenv error:**
```bash
pip3 install python-dotenv
```

---

## Security Note 🔒

Your private key is:
- ✅ Asked for securely (hidden input)
- ✅ Only stored in memory for this session
- ✅ Never written to disk
- ✅ Cleared when script exits

**Never share your private key with anyone!**

---

Ready to burn? Run:
```bash
./run_rpo_burns.sh
```

🔥🔥🔥
