# Control Panel

Web-based real-time control interface for MotorHandPro Primal Logic control system.

## Overview

The Control Panel provides a visual, interactive interface for:
- Adjusting Primal Logic parameters in real-time
- Monitoring system state and control signals
- Visualizing stability metrics and phase portraits
- Analyzing control energy evolution

## Features

### Real-Time Parameter Control

- **λ (Lambda - Lightfoot Constant)**: Exponential decay rate (0.01 - 1.0 s⁻¹)
  - Default: 0.16905 s⁻¹
  - Controls memory weighting and convergence speed

- **KE (Error Gain)**: Proportional error feedback (0.0 - 1.0)
  - Default: 0.3
  - Adjusts responsiveness to tracking errors

- **D (Donte Constant)**: Fixed-point attractor (read-only)
  - Value: 149.9992314000
  - Theoretical stability anchor point

### Visualizations

1. **Time-Series Plots** (Chart.js)
   - ψ(t) - Control command signal
   - γ(t) - Error signal
   - Ec(t) - Control energy functional

2. **3D Phase Portrait** (Three.js)
   - State space trajectory visualization
   - Shows system evolution in (ψ, γ, Ec) coordinates
   - Interactive rotation and zoom

3. **VTK Rendering** (vtk.js)
   - Advanced scientific visualization
   - Isosurface rendering of stability regions
   - Vector field displays

### Connection Status

- **Connected**: Live WebSocket/Serial connection to hardware
- **Disconnected**: Simulation mode or awaiting connection

## Files

- **`index.html`** - Main application structure
  - Loads visualization libraries (Three.js, Chart.js, VTK.js)
  - Defines UI layout and controls
  - Implements WebSocket/Serial communication handlers

- **`control_panel.js`** - Application logic
  - Parameter update handlers
  - Real-time data processing
  - Visualization rendering loops
  - Communication protocol implementation

- **`style.css`** - Visual styling
  - Responsive grid layout
  - Parameter control styling
  - Status indicators and alerts

## Usage

### Local Deployment

1. **Standalone (no server required)**
   ```bash
   # Open directly in browser
   open control_panel/index.html
   # or
   xdg-open control_panel/index.html  # Linux
   ```

2. **With local web server** (recommended for full features)
   ```bash
   cd control_panel
   python -m http.server 8080
   # Navigate to http://localhost:8080
   ```

### Connecting to Hardware

The control panel can connect to:

1. **Arduino/Embedded System** (Serial over WebSerial API)
   - Requires Chrome/Edge browser with WebSerial support
   - Click "Connect Serial" button
   - Select serial port (e.g., `/dev/ttyACM0` or `COM3`)
   - Baud rate: 115200

2. **LAM System** (WebSocket)
   - Connects to LAM orchestrator WebSocket endpoint
   - Default: `ws://localhost:8765`
   - See [LAM README](../lam/README.md) for setup

3. **Simulation Mode** (Local)
   - No connection required
   - Runs Primal Logic kernel in JavaScript
   - Useful for parameter exploration and testing

## Parameter Tuning Guidelines

### Stability Constraints

- **Always ensure:** F'(D) < 1 (Lipschitz contraction condition)
- **Monitor:** Ec(t) should remain bounded
- **Watch for:** Oscillations or unbounded growth in ψ(t)

### Common Tuning Scenarios

**Fast Response (Aggressive):**
```
λ = 0.25 s⁻¹
KE = 0.5
```
- Faster settling time
- May have higher overshoot

**Smooth Tracking (Conservative):**
```
λ = 0.16905 s⁻¹  (Default)
KE = 0.3
```
- Balanced performance
- Validated in benchmark tests

**Maximum Stability (Safe):**
```
λ = 0.10 s⁻¹
KE = 0.1
```
- Slowest response
- Minimal overshoot

## Technical Details

### Data Protocol

**From Hardware → Control Panel:**
```json
{
  "t": 1.23,
  "psi": 1.0145,
  "gamma": 0.0084,
  "Ec": 0.0031,
  "D": 149.9992314000,
  "lipschitz": 0.000129931830
}
```

**From Control Panel → Hardware:**
```json
{
  "command": "set_params",
  "lambda": 0.16905,
  "KE": 0.3
}
```

### Browser Compatibility

- **Full Support**: Chrome 89+, Edge 89+ (WebSerial API)
- **Partial Support**: Firefox, Safari (simulation mode only)
- **Recommended**: Chrome/Chromium for hardware connectivity

## Integration Points

- **LAM System**: [lam/README.md](../lam/README.md)
- **Infrastructure**: WebSocket gateway at [infrastructure/websocket/](../infrastructure/websocket/)
- **Node-RED**: MQTT bridge at [node-red/](../node-red/)

## Development

### Adding New Visualizations

1. Edit `control_panel.js`
2. Add canvas/div in `index.html`
3. Implement rendering function:
   ```javascript
   function renderCustomViz(data) {
     // Your visualization code
   }
   ```
4. Call from `updateDisplay(data)` function

### Extending Parameters

1. Add slider in `index.html`:
   ```html
   <input type="range" id="new-param" min="0" max="1" step="0.01">
   ```
2. Add handler in `control_panel.js`:
   ```javascript
   document.getElementById('new-param').addEventListener('input', (e) => {
     sendParameter('new_param', e.target.value);
   });
   ```

## Security Considerations

- **Local Only**: Not designed for public internet deployment
- **No Authentication**: Assumes trusted local network
- **WebSerial**: Requires user permission for hardware access
- **Production Use**: Implement authentication if deploying on network

## Troubleshooting

**"Disconnected" status won't change:**
- Check serial cable connection
- Verify correct baud rate (115200)
- Try different USB port
- Check browser console for errors

**Charts not updating:**
- Ensure data format matches expected JSON structure
- Check browser console for JavaScript errors
- Verify WebSocket connection is established

**3D visualization laggy:**
- Reduce update frequency in `control_panel.js`
- Lower rendering quality settings
- Close other browser tabs

## Related Documentation

- [Main README](../README.md) - System overview
- [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md) - Mathematical foundations
- [LAM Workflow Guide](../LAM_WORKFLOW_GUIDE.md) - Integration workflows

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
