#!/bin/bash
# Monitor NASA Pipeline - Real-time status viewer
# Usage: ./monitor_nasa_pipeline.sh

echo "=================================="
echo "🌌 NASA Pipeline Monitor"
echo "=================================="
echo ""

# Check if pipeline is running
if pgrep -f "live_nasa_pipeline.py" > /dev/null; then
    echo "✅ Pipeline Status: RUNNING"
    echo ""

    # Count output files
    num_obs=$(ls -1 nasa_live_output/observations_*.json 2>/dev/null | wc -l)
    num_states=$(ls -1 nasa_live_output/states_*.json 2>/dev/null | wc -l)
    num_viz=$(ls -1 nasa_live_output/visualizations/*.html 2>/dev/null | wc -l)

    echo "📊 Output Files:"
    echo "   Observations: $num_obs"
    echo "   States: $num_states"
    echo "   Visualizations: $num_viz"
    echo ""

    # Show latest files
    echo "📄 Latest Output Files:"
    ls -lht nasa_live_output/observations_*.json 2>/dev/null | head -3 | awk '{print "   "$9" ("$5")"}'
    echo ""

    # Show disk usage
    total_size=$(du -sh nasa_live_output 2>/dev/null | awk '{print $1}')
    echo "💾 Total Output Size: $total_size"
    echo ""

    # Show latest observation data
    latest_obs=$(ls -t nasa_live_output/observations_*.json 2>/dev/null | head -1)
    if [ -n "$latest_obs" ]; then
        echo "🌌 Latest Comet Data:"
        python3 -c "
import json
with open('$latest_obs', 'r') as f:
    obs = json.load(f)
    latest = obs[-1]
    print(f\"   Time: {latest['timestamp']}\")
    print(f\"   RA: {latest['ra']:.4f}° | Dec: {latest['dec']:.4f}°\")
    print(f\"   Distance: {latest['distance_au']:.4f} AU\")
    print(f\"   Ni Flux: {latest.get('gas_production_rate', 0):.2f} g/s\")
" 2>/dev/null
        echo ""
    fi

    # Show process info
    echo "🔧 Process Info:"
    ps aux | grep -E "live_nasa_pipeline.py" | grep -v grep | awk '{print "   PID: "$2" | CPU: "$3"% | MEM: "$4"% | Runtime: "$10}'
    echo ""

    echo "=================================="
    echo "Press Ctrl+C to stop monitoring"
    echo "=================================="
    echo ""

    # Tail the latest output (if running in background)
    echo "📝 Recent Pipeline Activity:"
    # Note: Background process output may not be visible here
    echo "   (Run 'python3 live_nasa_pipeline.py --mode continuous' in foreground to see live logs)"

else
    echo "⚠️  Pipeline Status: NOT RUNNING"
    echo ""
    echo "To start pipeline:"
    echo "  python3 live_nasa_pipeline.py --mode continuous --interval 3600"
    echo ""

    # Show existing outputs
    if [ -d "nasa_live_output" ]; then
        num_obs=$(ls -1 nasa_live_output/observations_*.json 2>/dev/null | wc -l)
        num_viz=$(ls -1 nasa_live_output/visualizations/*.html 2>/dev/null | wc -l)

        if [ "$num_obs" -gt 0 ]; then
            echo "📊 Previous Run Data:"
            echo "   Observations: $num_obs"
            echo "   Visualizations: $num_viz"
            echo ""
            echo "   View visualizations:"
            echo "   open nasa_live_output/visualizations/interactive_sky_chart.html"
        fi
    fi
fi

echo ""
