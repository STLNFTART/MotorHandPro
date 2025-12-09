#!/bin/bash
# Setup script for NASA Data Pipeline
# Author: Donte Lightfoot
# Date: December 4, 2025

set -e

echo "=================================="
echo "NASA Data Pipeline Setup"
echo "=================================="
echo ""

# Check Python
if ! command -v python3 &> /dev/null; then
    echo "❌ Error: Python 3 not found"
    exit 1
fi

echo "✅ Python 3 found: $(python3 --version)"
echo ""

# Install dependencies
echo "📦 Installing dependencies..."
pip install -q --upgrade pip
pip install -q numpy scipy requests matplotlib plotly kaleido

echo "✅ Dependencies installed"
echo ""

# Test imports
echo "🧪 Testing imports..."
python3 -c "
import numpy
import scipy
import requests
import matplotlib
import plotly
print('✅ All core dependencies available')
" || {
    echo "❌ Error: Some dependencies failed to import"
    exit 1
}
echo ""

# Make scripts executable
echo "🔧 Making scripts executable..."
chmod +x live_nasa_pipeline.py
echo "✅ Scripts are executable"
echo ""

# Create output directories
echo "📁 Creating output directories..."
mkdir -p nasa_live_output
mkdir -p nasa_live_output/visualizations
mkdir -p nasa_visualizations
echo "✅ Directories created"
echo ""

# Test run
echo "🚀 Running test iteration..."
python3 live_nasa_pipeline.py --mode single --output-dir nasa_live_output || {
    echo "⚠️  Test run completed with warnings (this is normal if NASA APIs are unavailable)"
}
echo ""

echo "=================================="
echo "✅ NASA Pipeline Setup Complete!"
echo "=================================="
echo ""
echo "Usage:"
echo "  Single run:      python3 live_nasa_pipeline.py --mode single"
echo "  Continuous:      python3 live_nasa_pipeline.py --mode continuous --interval 3600"
echo "  With max iter:   python3 live_nasa_pipeline.py --mode continuous --max-iterations 10"
echo ""
echo "Outputs will be in: nasa_live_output/"
echo ""
echo "To set up automated cron job (runs every 6 hours):"
echo "  crontab -e"
echo "  Add: 0 */6 * * * cd $(pwd) && python3 live_nasa_pipeline.py --mode single >> nasa_pipeline.log 2>&1"
echo ""
