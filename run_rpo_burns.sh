#!/bin/bash
# Quick setup script for $RPO token burns

echo "========================================================================"
echo "🔥 $RPO TOKEN BURN SETUP"
echo "========================================================================"
echo ""

# Check if private key is set
if [ -z "$PRIVATE_KEY" ]; then
    echo "⚠️  PRIVATE_KEY not set"
    echo ""
    echo "Please enter your private key (it will not be displayed):"
    read -s PRIVATE_KEY
    export PRIVATE_KEY
    echo ""
    echo "✅ Private key set (hidden)"
else
    echo "✅ PRIVATE_KEY is already set"
fi

# Set contract address
export HEDERA_CONTRACT_ID=0xa2e2b88620944085D390fdbe86CBa10A2f06a033
export USE_REAL_BURNS=true
export HEDERA_RPC_URL=https://testnet.hashio.io/api

echo ""
echo "📋 Configuration:"
echo "   Contract: $HEDERA_CONTRACT_ID"
echo "   Network: Hedera Testnet"
echo "   Real Burns: ENABLED 🔥"
echo ""

# Ask what to run
echo "What would you like to do?"
echo ""
echo "1) Test GoTrax integration (activate actuators)"
echo "2) Check your $RPO balance"
echo "3) Run direct burn test (0.1 $RPO)"
echo "4) Exit"
echo ""
read -p "Choose [1-4]: " choice

case $choice in
    1)
        echo ""
        echo "🚀 Running GoTrax integration with REAL BURNS..."
        echo ""
        python3 lam/integrations/gotrax_hoverboard_integration.py
        ;;
    2)
        echo ""
        echo "💰 Checking balance..."
        echo ""
        python3 -c "
from lam.integrations.hedera_rpo_burn import RPOTokenBurner
burner = RPOTokenBurner()
balance = burner.get_balance()
print(f'Your balance: {balance:,.2f} \$RPO')
print(f'Max actuation: {balance:,.2f} seconds')
print(f'             = {balance/60:,.2f} minutes')
print(f'             = {balance/3600:,.2f} hours')
"
        ;;
    3)
        echo ""
        echo "🔥 Running direct burn test..."
        echo ""
        python3 lam/integrations/hedera_rpo_burn.py
        ;;
    4)
        echo ""
        echo "👋 Goodbye!"
        exit 0
        ;;
    *)
        echo ""
        echo "❌ Invalid choice"
        exit 1
        ;;
esac

echo ""
echo "========================================================================"
echo "✅ Done!"
echo "========================================================================"
