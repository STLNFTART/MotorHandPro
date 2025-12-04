#!/bin/bash
# Quick viewer for NASA visualizations
# Author: Donte Lightfoot

echo "=================================="
echo "🌌 NASA Visualization Viewer"
echo "=================================="
echo ""

# Check what's available
if [ -d "all_visualizations" ]; then
    echo "📊 Available Visualizations:"
    echo ""
    
    for lib in all_visualizations/*/; do
        if [ -d "$lib" ]; then
            lib_name=$(basename "$lib")
            echo "  $lib_name:"
            for file in "$lib"*.html; do
                if [ -f "$file" ]; then
                    size=$(du -h "$file" | cut -f1)
                    echo "    - $(basename "$file") ($size)"
                fi
            done
            echo ""
        fi
    done
fi

echo "=================================="
echo "🚀 Viewing Options"
echo "=================================="
echo ""
echo "OPTION 1: Open in default browser (recommended)"
echo "  Linux:"
echo "    xdg-open all_visualizations/plotly/interactive_sky_chart.html"
echo ""
echo "  Mac:"
echo "    open all_visualizations/plotly/interactive_sky_chart.html"
echo ""
echo "  Windows:"
echo "    start all_visualizations/plotly/interactive_sky_chart.html"
echo ""
echo "OPTION 2: Start HTTP server (for all files)"
echo "  cd all_visualizations"
echo "  python3 -m http.server 8000"
echo "  Then visit: http://localhost:8000"
echo ""
echo "OPTION 3: Copy to local machine (if on remote server)"
echo "  scp -r all_visualizations/ user@local:/path/"
echo ""
echo "=================================="
echo ""

# Offer to start server
read -p "Start HTTP server now? (y/n): " answer
if [ "$answer" = "y" ]; then
    echo ""
    echo "🚀 Starting HTTP server on port 8000..."
    echo "   Visit: http://localhost:8000"
    echo "   Press Ctrl+C to stop"
    echo ""
    cd all_visualizations && python3 -m http.server 8000
fi
