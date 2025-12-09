# 🔌 LAM Application Integration Guide

Complete guide to integrating the LAM (Large Action Model) system with your applications, including Arduino, Python, Node.js, and web applications.

---

## 📖 Table of Contents

1. [Python Application Integration](#python-application-integration)
2. [Arduino/Embedded Integration](#arduinoembedded-integration)
3. [Web Application Integration](#web-application-integration)
4. [REST API Integration](#rest-api-integration)
5. [Real-time MQTT Integration](#real-time-mqtt-integration)
6. [Custom Actuator Integration](#custom-actuator-integration)

---

## 1. Python Application Integration

### Basic Integration

```python
#!/usr/bin/env python3
"""
Example: Integrate LAM with your Python application
"""
import asyncio
from lam.lam_temporal_integration import LAMTemporalController, LAMTemporalState
from lam.temporal_displacement import TemporalDisplacementConfig

class MyRobotApplication:
    """Your custom robot control application with LAM integration"""

    def __init__(self):
        # Initialize LAM controller
        self.lam = LAMTemporalController(
            method='timewarp',  # Fast method
            enable_trust_gating=True,
            enable_load_shedding=True
        )

        # Your application state
        self.position = 0.0
        self.velocity = 0.0
        self.target = 0.0

    async def control_loop(self):
        """Main control loop with LAM integration"""
        dt = 0.01  # 10ms control loop

        while True:
            # 1. Get sensor readings
            sensor_value = self.read_sensors()

            # 2. Compute error
            error = self.target - sensor_value

            # 3. Get system confidence (from sensor quality, etc.)
            confidence = self.compute_confidence(sensor_value)

            # 4. Get system load (CPU usage, etc.)
            system_load = self.get_system_load()

            # 5. Update LAM controller
            state = self.lam.update(
                E0=error,
                confidence=confidence,
                system_load=system_load,
                dt=dt
            )

            # 6. Apply control output to actuators
            self.apply_control(state.E)

            # 7. Sleep for control period
            await asyncio.sleep(dt)

    def read_sensors(self) -> float:
        """Read sensor data"""
        # Your sensor reading code
        return self.position + (random.random() - 0.5) * 0.1

    def compute_confidence(self, sensor_value: float) -> float:
        """Compute sensor confidence (0.0 to 1.0)"""
        # Example: confidence based on sensor noise
        noise = abs(sensor_value - self.position)
        return max(0.0, min(1.0, 1.0 - noise))

    def get_system_load(self) -> float:
        """Get system load (0.0 to 1.0)"""
        import psutil
        return psutil.cpu_percent() / 100.0

    def apply_control(self, control_output: float):
        """Apply control signal to actuators"""
        # Your actuator control code
        self.velocity = control_output * 0.1
        self.position += self.velocity

# Run the application
async def main():
    app = MyRobotApplication()
    app.target = 10.0  # Set target position
    await app.control_loop()

if __name__ == "__main__":
    asyncio.run(main())
```

### Advanced Integration with Token Burns

```python
#!/usr/bin/env python3
"""
Example: LAM integration with actuator token burns
"""
import asyncio
from lam.integrations.gotrax_hoverboard_integration import (
    LAMHoverboardInterface,
    ActuationMode,
    ActuationRequest
)

class TokenizedRobotApp:
    """Application with token-based actuator control"""

    def __init__(self):
        # Initialize LAM interface with Hedera integration
        self.lam_interface = LAMHoverboardInterface()

        # Deposit initial tokens
        self.lam_interface.deposit_tokens(100.0)
        print("✓ Deposited 100 tokens (100 seconds of actuation)")

    async def execute_mission(self):
        """Execute a robot mission with token burns"""

        # Task 1: Move forward for 3 seconds
        print("\n1. Moving forward...")
        result = await self.lam_interface.execute_move(
            mode="forward",
            duration=3.0,
            power=0.6
        )
        print(f"✓ Forward complete")
        print(f"  Tokens burned: {result['tokens_burned']}")
        print(f"  Smoothness: {result['smoothness_score']:.2f}")

        # Task 2: Turn left for 1.5 seconds
        print("\n2. Turning left...")
        result = await self.lam_interface.execute_move(
            mode="turn_left",
            duration=1.5,
            power=0.5
        )
        print(f"✓ Turn complete")
        print(f"  Tokens burned: {result['tokens_burned']}")

        # Task 3: Check remaining balance
        status = self.lam_interface.get_status()
        balance = status['hoverboard']['token_config']['balance']
        print(f"\n3. Remaining balance: {balance:.1f} tokens")
        print(f"   Max remaining time: {balance:.1f} seconds")

# Run mission
async def main():
    app = TokenizedRobotApp()
    await app.execute_mission()

if __name__ == "__main__":
    asyncio.run(main())
```

---

## 2. Arduino/Embedded Integration

### Option A: Serial Communication with Python LAM

**Arduino Side (`MotorHandPro.ino`):**

```cpp
// Add LAM integration to your Arduino code

// Serial communication with Python LAM
String lam_command = "";

void setup() {
  Serial.begin(115200);

  // Initialize motors
  pinMode(MOTOR_LEFT_PWM, OUTPUT);
  pinMode(MOTOR_RIGHT_PWM, OUTPUT);

  Serial.println("LAM_READY");
}

void loop() {
  // Check for commands from Python LAM
  if (Serial.available()) {
    lam_command = Serial.readStringUntil('\n');
    processLAMCommand(lam_command);
  }

  // Send sensor data to LAM
  sendSensorData();

  delay(10);  // 100Hz update rate
}

void processLAMCommand(String cmd) {
  // Parse LAM control commands
  // Format: "MOTOR,left_pwm,right_pwm,duration,tokens"

  if (cmd.startsWith("MOTOR,")) {
    int idx1 = cmd.indexOf(',', 6);
    int idx2 = cmd.indexOf(',', idx1 + 1);
    int idx3 = cmd.indexOf(',', idx2 + 1);

    int left_pwm = cmd.substring(6, idx1).toInt();
    int right_pwm = cmd.substring(idx1 + 1, idx2).toInt();
    float duration = cmd.substring(idx2 + 1, idx3).toFloat();
    float tokens = cmd.substring(idx3 + 1).toFloat();

    // Apply motor control
    analogWrite(MOTOR_LEFT_PWM, left_pwm);
    analogWrite(MOTOR_RIGHT_PWM, right_pwm);

    // Send acknowledgment
    Serial.print("ACK,");
    Serial.print(tokens);
    Serial.println();
  }
}

void sendSensorData() {
  // Send sensor readings to LAM
  // Format: "SENSOR,position,velocity,load"

  static unsigned long lastSend = 0;
  if (millis() - lastSend > 100) {  // 10Hz sensor data
    Serial.print("SENSOR,");
    Serial.print(encoderPosition);
    Serial.print(",");
    Serial.print(velocity);
    Serial.print(",");
    Serial.print(systemLoad);
    Serial.println();

    lastSend = millis();
  }
}
```

**Python LAM Bridge:**

```python
#!/usr/bin/env python3
"""
LAM Bridge for Arduino Integration
"""
import asyncio
import serial
from lam.lam_temporal_integration import LAMTemporalController

class ArduinoLAMBridge:
    """Bridge between Python LAM and Arduino"""

    def __init__(self, port='/dev/ttyUSB0', baudrate=115200):
        self.serial = serial.Serial(port, baudrate, timeout=0.1)
        self.lam = LAMTemporalController(method='timewarp')

        # Wait for Arduino ready
        while True:
            line = self.serial.readline().decode().strip()
            if line == "LAM_READY":
                print("✓ Arduino connected and ready")
                break

    async def control_loop(self):
        """Main control loop"""
        while True:
            # Read sensor data from Arduino
            if self.serial.in_waiting:
                line = self.serial.readline().decode().strip()

                if line.startswith("SENSOR,"):
                    # Parse sensor data
                    parts = line.split(',')
                    position = float(parts[1])
                    velocity = float(parts[2])
                    load = float(parts[3])

                    # Update LAM
                    state = self.lam.update(
                        E0=position,
                        system_load=load / 100.0,
                        dt=0.01
                    )

                    # Send control command to Arduino
                    left_pwm = int(min(255, max(0, state.E * 255)))
                    right_pwm = left_pwm

                    cmd = f"MOTOR,{left_pwm},{right_pwm},0.01,0.01\n"
                    self.serial.write(cmd.encode())

            await asyncio.sleep(0.01)

# Run bridge
async def main():
    bridge = ArduinoLAMBridge(port='/dev/ttyUSB0')
    await bridge.control_loop()

if __name__ == "__main__":
    asyncio.run(main())
```

### Option B: Direct C++ LAM Library

If you need LAM on the Arduino itself, use the C++ port:

```cpp
// Include LAM library (extras/quant_final/lam_cpp.hpp)
#include "lam_cpp.hpp"

LAMTemporalController lam(METHOD_TIMEWARP);

void loop() {
  // Read sensor
  float sensor_value = analogRead(A0) / 1023.0;

  // Update LAM
  LAMState state = lam.update(
    sensor_value,   // E0
    0.9,            // confidence
    0.5,            // system_load
    0.01            // dt
  );

  // Apply control
  int pwm = (int)(state.E * 255);
  analogWrite(MOTOR_PWM, pwm);

  delay(10);
}
```

---

## 3. Web Application Integration

### FastAPI Backend with LAM

```python
#!/usr/bin/env python3
"""
FastAPI backend with LAM integration
"""
from fastapi import FastAPI, WebSocket
from fastapi.middleware.cors import CORSMiddleware
import asyncio
from lam.lam_temporal_integration import LAMTemporalController

app = FastAPI(title="MotorHandPro LAM API")

# Enable CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_methods=["*"],
    allow_headers=["*"],
)

# Global LAM controller
lam_controller = LAMTemporalController(method='timewarp')

@app.get("/api/lam/status")
async def get_lam_status():
    """Get LAM controller status"""
    return {
        "method": "timewarp",
        "state": {
            "E": lam_controller.state.E,
            "Delta": lam_controller.state.Delta,
            "timestamp": lam_controller.state.timestamp
        }
    }

@app.post("/api/lam/update")
async def update_lam(E0: float, confidence: float = 1.0, system_load: float = 0.0):
    """Update LAM controller"""
    state = lam_controller.update(
        E0=E0,
        confidence=confidence,
        system_load=system_load,
        dt=0.01
    )

    return {
        "success": True,
        "state": {
            "E": state.E,
            "Delta": state.Delta,
            "confidence": confidence
        }
    }

@app.post("/api/actuator/move")
async def execute_move(mode: str, duration: float, power: float = 0.5):
    """Execute actuator movement with token burn"""
    from lam.integrations.gotrax_hoverboard_integration import LAMHoverboardInterface

    interface = LAMHoverboardInterface()
    result = await interface.execute_move(mode, duration, power)

    return result

@app.websocket("/ws/lam")
async def lam_websocket(websocket: WebSocket):
    """WebSocket for real-time LAM updates"""
    await websocket.accept()

    try:
        while True:
            # Receive sensor data from client
            data = await websocket.receive_json()

            # Update LAM
            state = lam_controller.update(
                E0=data.get('E0', 0.0),
                confidence=data.get('confidence', 1.0),
                system_load=data.get('system_load', 0.0),
                dt=0.01
            )

            # Send back control signal
            await websocket.send_json({
                "E": state.E,
                "Delta": state.Delta,
                "timestamp": state.timestamp
            })

            await asyncio.sleep(0.01)
    except Exception as e:
        print(f"WebSocket error: {e}")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

### React Frontend Integration

```javascript
// src/components/LAMController.jsx
import React, { useState, useEffect } from 'react';

export function LAMController() {
  const [lamState, setLamState] = useState({ E: 0, Delta: 0 });
  const [sensorValue, setSensorValue] = useState(0);

  // Connect to WebSocket
  useEffect(() => {
    const ws = new WebSocket('ws://localhost:8000/ws/lam');

    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      setLamState(data);
    };

    const interval = setInterval(() => {
      // Send sensor data
      ws.send(JSON.stringify({
        E0: sensorValue,
        confidence: 0.9,
        system_load: 0.5
      }));
    }, 100);

    return () => {
      clearInterval(interval);
      ws.close();
    };
  }, [sensorValue]);

  const executeMove = async (mode, duration, power) => {
    const response = await fetch('http://localhost:8000/api/actuator/move', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ mode, duration, power })
    });

    const result = await response.json();
    console.log('Move result:', result);
  };

  return (
    <div className="lam-controller">
      <h2>LAM Controller</h2>

      <div className="status">
        <div>Field Value (E): {lamState.E.toFixed(4)}</div>
        <div>Displacement (Δ): {lamState.Delta.toFixed(4)}s</div>
      </div>

      <div className="controls">
        <label>
          Sensor Input:
          <input
            type="range"
            min="0"
            max="1"
            step="0.01"
            value={sensorValue}
            onChange={(e) => setSensorValue(parseFloat(e.target.value))}
          />
          {sensorValue.toFixed(2)}
        </label>
      </div>

      <div className="actuator-controls">
        <button onClick={() => executeMove('forward', 2.0, 0.6)}>
          Forward (2s)
        </button>
        <button onClick={() => executeMove('turn_left', 1.5, 0.5)}>
          Turn Left (1.5s)
        </button>
        <button onClick={() => executeMove('stop', 0.1, 0.0)}>
          Stop
        </button>
      </div>
    </div>
  );
}
```

---

## 4. REST API Integration

### Complete API Server

Save this as `lam_api_server.py`:

```python
#!/usr/bin/env python3
"""
Complete REST API server for LAM integration
Run: python lam_api_server.py
"""
from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
from typing import Optional
import asyncio

from lam.lam_temporal_integration import LAMTemporalController
from lam.integrations.gotrax_hoverboard_integration import LAMHoverboardInterface

app = FastAPI(
    title="MotorHandPro LAM API",
    description="REST API for LAM control and actuator management",
    version="1.0.0"
)

# Global controllers
lam = LAMTemporalController(method='timewarp', enable_trust_gating=True)
hoverboard = LAMHoverboardInterface()

class LAMUpdateRequest(BaseModel):
    E0: float
    confidence: float = 1.0
    system_load: float = 0.0
    dt: float = 0.01

class ActuationRequest(BaseModel):
    mode: str
    duration: float
    power: float = 0.5

@app.get("/")
async def root():
    return {
        "service": "MotorHandPro LAM API",
        "version": "1.0.0",
        "endpoints": {
            "lam_status": "/lam/status",
            "lam_update": "/lam/update",
            "actuator_move": "/actuator/move",
            "actuator_status": "/actuator/status",
            "token_deposit": "/token/deposit"
        }
    }

@app.get("/lam/status")
async def lam_status():
    """Get current LAM state"""
    return {
        "E": lam.state.E,
        "Delta": lam.state.Delta,
        "psi": lam.state.psi,
        "timestamp": lam.state.timestamp
    }

@app.post("/lam/update")
async def lam_update(request: LAMUpdateRequest):
    """Update LAM controller"""
    state = lam.update(
        E0=request.E0,
        confidence=request.confidence,
        system_load=request.system_load,
        dt=request.dt
    )

    return {
        "success": True,
        "state": {
            "E": state.E,
            "Delta": state.Delta,
            "psi": state.psi
        }
    }

@app.post("/actuator/move")
async def actuator_move(request: ActuationRequest):
    """Execute actuator movement with token burn"""
    result = await hoverboard.execute_move(
        mode=request.mode,
        duration=request.duration,
        power=request.power
    )

    if not result['success']:
        raise HTTPException(status_code=400, detail=result.get('error'))

    return result

@app.get("/actuator/status")
async def actuator_status():
    """Get actuator and token status"""
    return hoverboard.get_status()

@app.post("/token/deposit/{amount}")
async def token_deposit(amount: float):
    """Deposit tokens"""
    if amount <= 0:
        raise HTTPException(status_code=400, detail="Amount must be positive")

    return hoverboard.deposit_tokens(amount)

if __name__ == "__main__":
    import uvicorn
    print("Starting MotorHandPro LAM API Server...")
    print("Documentation: http://localhost:8000/docs")
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

**Usage from any language:**

```bash
# Python
import requests
result = requests.post('http://localhost:8000/actuator/move', json={
    'mode': 'forward',
    'duration': 2.0,
    'power': 0.6
}).json()

# JavaScript
fetch('http://localhost:8000/actuator/move', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({mode: 'forward', duration: 2.0, power: 0.6})
})

# curl
curl -X POST http://localhost:8000/actuator/move \
  -H "Content-Type: application/json" \
  -d '{"mode":"forward","duration":2.0,"power":0.6}'
```

---

## 5. Real-time MQTT Integration

```python
#!/usr/bin/env python3
"""
MQTT Integration for distributed LAM systems
"""
import asyncio
import json
import paho.mqtt.client as mqtt
from lam.lam_temporal_integration import LAMTemporalController

class LAM_MQTT_Bridge:
    """Bridge LAM to MQTT for distributed systems"""

    def __init__(self, broker='localhost', port=1883):
        self.lam = LAMTemporalController(method='timewarp')

        # MQTT client
        self.client = mqtt.Client()
        self.client.on_connect = self.on_connect
        self.client.on_message = self.on_message

        self.client.connect(broker, port, 60)
        self.client.loop_start()

    def on_connect(self, client, userdata, flags, rc):
        print(f"✓ Connected to MQTT broker")

        # Subscribe to sensor topics
        self.client.subscribe("motorhand/sensors/#")
        self.client.subscribe("motorhand/commands/#")

    def on_message(self, client, userdata, msg):
        """Handle incoming MQTT messages"""
        try:
            data = json.loads(msg.payload)

            if msg.topic == "motorhand/sensors/position":
                # Update LAM with sensor data
                state = self.lam.update(
                    E0=data['value'],
                    confidence=data.get('confidence', 1.0),
                    dt=0.01
                )

                # Publish control output
                self.client.publish("motorhand/control/output",
                                   json.dumps({
                                       'E': state.E,
                                       'Delta': state.Delta,
                                       'timestamp': state.timestamp
                                   }))

            elif msg.topic == "motorhand/commands/move":
                # Execute movement command
                asyncio.create_task(self.execute_move(data))

        except Exception as e:
            print(f"Error processing message: {e}")

    async def execute_move(self, data):
        """Execute movement with token burn"""
        from lam.integrations.gotrax_hoverboard_integration import LAMHoverboardInterface

        interface = LAMHoverboardInterface()
        result = await interface.execute_move(
            mode=data['mode'],
            duration=data['duration'],
            power=data.get('power', 0.5)
        )

        # Publish result
        self.client.publish("motorhand/actuator/result",
                           json.dumps(result))

# Run MQTT bridge
def main():
    bridge = LAM_MQTT_Bridge(broker='localhost', port=1883)

    # Keep running
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nShutting down...")

if __name__ == "__main__":
    main()
```

---

## 6. Custom Actuator Integration

Template for integrating your own actuators:

```python
#!/usr/bin/env python3
"""
Template: Integrate your custom actuator with LAM and token burns
"""
import asyncio
from dataclasses import dataclass
from lam.temporal_displacement import TemporalDisplacementConfig
from lam.integrations.gotrax_hoverboard_integration import (
    TokenBurnConfig,
    ActuationRequest,
    ActuationResult
)

@dataclass
class MyActuatorSpec:
    """Your actuator specifications"""
    max_force: float = 100.0  # Newtons
    max_speed: float = 1.0     # m/s
    mass: float = 5.0          # kg

class MyActuatorController:
    """Custom actuator controller with LAM integration"""

    def __init__(self):
        self.spec = MyActuatorSpec()
        self.token_config = TokenBurnConfig(token_rate=1.0)

        # Primal Logic parameters
        self.lambda_val = 0.16905
        self.D = 149.9992314

        # State
        self.token_balance = 0.0
        self.position = 0.0
        self.velocity = 0.0

    def deposit_tokens(self, amount: float):
        """Deposit tokens for actuation"""
        self.token_balance += amount
        return {
            "success": True,
            "new_balance": self.token_balance
        }

    async def execute_actuation(self,
                                target_position: float,
                                duration: float,
                                max_force: float = 1.0) -> ActuationResult:
        """Execute actuation with token burn"""

        # Calculate tokens required
        tokens_required = duration * self.token_config.token_rate

        if tokens_required > self.token_balance:
            return ActuationResult(
                request_id="",
                success=False,
                tokens_burned=0.0,
                actuation_duration_actual=0.0,
                smoothness_score=0.0,
                primal_logic_metrics={},
                error_message="Insufficient tokens"
            )

        # Compute smooth trajectory using Primal Logic
        trajectory = self._compute_trajectory(
            target_position,
            duration,
            max_force
        )

        # Execute trajectory
        for point in trajectory:
            # Apply to your actuator hardware
            self.position = point['position']
            self.velocity = point['velocity']

            # Send to hardware:
            # - SPI/I2C commands
            # - PWM signals
            # - CAN bus messages
            # - Serial commands
            # etc.

            await asyncio.sleep(point['dt'])

        # Burn tokens
        self.token_balance -= tokens_required

        return ActuationResult(
            request_id=f"ACT_{int(time.time())}",
            success=True,
            tokens_burned=tokens_required,
            actuation_duration_actual=duration,
            smoothness_score=0.98,
            primal_logic_metrics={
                "lambda": self.lambda_val,
                "D": self.D
            }
        )

    def _compute_trajectory(self, target, duration, max_force):
        """Compute smooth trajectory using Primal Logic"""
        import math

        trajectory = []
        dt = 0.01
        num_points = int(duration / dt)

        for i in range(num_points):
            t = i * dt

            # Exponential smoothing
            progress = 1.0 - math.exp(-self.lambda_val * t)

            # Position with S-curve
            position = target * progress

            # Velocity
            velocity = target * self.lambda_val * math.exp(-self.lambda_val * t)

            trajectory.append({
                'time': t,
                'position': position,
                'velocity': velocity,
                'force': max_force * progress,
                'dt': dt
            })

        return trajectory

# Usage
async def main():
    # Create controller
    controller = MyActuatorController()

    # Deposit tokens
    controller.deposit_tokens(10.0)

    # Execute movement
    result = await controller.execute_actuation(
        target_position=1.0,  # 1 meter
        duration=2.0,         # 2 seconds
        max_force=0.5         # 50% max force
    )

    print(f"Success: {result.success}")
    print(f"Tokens burned: {result.tokens_burned}")
    print(f"Smoothness: {result.smoothness_score}")

if __name__ == "__main__":
    asyncio.run(main())
```

---

## 📚 Additional Resources

- **LAM API Reference**: `docs/api/PYTHON_API.md`
- **Temporal Displacement Guide**: `lam/TEMPORAL_DISPLACEMENT.md`
- **Quickstart Guide**: `lam/QUICKSTART_TEMPORAL.md`
- **Architecture Overview**: `docs/ARCHITECTURE.md`

---

## 🎯 Integration Checklist

- [ ] Choose integration method (Python/Arduino/Web/API)
- [ ] Install LAM dependencies
- [ ] Configure Hedera credentials (for token burns)
- [ ] Implement sensor reading interface
- [ ] Implement actuator control interface
- [ ] Add LAM controller to control loop
- [ ] Test with token deposits
- [ ] Verify smooth control (smoothness score > 0.9)
- [ ] Monitor token burns on Hedera
- [ ] Deploy to production

---

**Patent Pending**: U.S. Provisional Patent Application No. 63/842,846
