#!/usr/bin/env python3
"""
Hardware Bridge for Physical Prosthetic Devices
Seamless integration between LAM and Arduino/microcontroller hardware
"""
import sys
import json
import time
import serial
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Optional, Callable
from datetime import datetime
from dataclasses import dataclass
import threading
import queue

# Add paths
sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))


@dataclass
class HardwareStatus:
    """Hardware status information"""
    connected: bool
    streaming: bool
    radiation_dose: int
    uptime_ms: int
    last_update: datetime


class ArduinoProstheticBridge:
    """
    Bridge between LAM and Arduino prosthetic hardware
    Handles serial communication, EMG streaming, and motor control
    """

    def __init__(self,
                 port: Optional[str] = None,
                 baudrate: int = 115200,
                 timeout: float = 1.0):
        """
        Initialize hardware bridge

        Args:
            port: Serial port (e.g., '/dev/ttyACM0', 'COM3')
            baudrate: Serial baudrate
            timeout: Serial timeout in seconds
        """
        self.port = port
        self.baudrate = baudrate
        self.timeout = timeout

        self.serial: Optional[serial.Serial] = None
        self.connected = False

        # Data streaming
        self.streaming = False
        self.emg_queue = queue.Queue(maxsize=1000)
        self.stream_thread: Optional[threading.Thread] = None

        # Callbacks
        self.emg_callback: Optional[Callable] = None

        # Status
        self.status = HardwareStatus(
            connected=False,
            streaming=False,
            radiation_dose=0,
            uptime_ms=0,
            last_update=datetime.now()
        )

        print(f"ArduinoProstheticBridge initialized:")
        print(f"  Port: {port or 'Auto-detect'}")
        print(f"  Baudrate: {baudrate}")

    def auto_detect_port(self) -> Optional[str]:
        """Auto-detect Arduino serial port"""
        import serial.tools.list_ports

        ports = serial.tools.list_ports.comports()

        for port in ports:
            # Look for Arduino devices
            if 'Arduino' in port.description or 'CH340' in port.description or 'USB' in port.description:
                print(f"  Found Arduino on: {port.device}")
                return port.device

        return None

    def connect(self) -> bool:
        """
        Connect to Arduino hardware

        Returns:
            True if connection successful
        """
        if self.connected:
            print("Already connected")
            return True

        # Auto-detect port if not specified
        if self.port is None:
            self.port = self.auto_detect_port()
            if self.port is None:
                print("ERROR: No Arduino device found")
                print("Available ports:")
                import serial.tools.list_ports
                for port in serial.tools.list_ports.comports():
                    print(f"  - {port.device}: {port.description}")
                return False

        try:
            print(f"\nConnecting to {self.port}...")
            self.serial = serial.Serial(
                port=self.port,
                baudrate=self.baudrate,
                timeout=self.timeout
            )

            # Wait for Arduino to reset and send ready signal
            time.sleep(2.0)

            # Read ready message
            for _ in range(10):
                line = self.serial.readline().decode('utf-8').strip()
                if line:
                    print(f"  Arduino: {line}")
                    if '"status":"ready"' in line:
                        self.connected = True
                        self.status.connected = True
                        print("✓ Connected successfully!")
                        return True

            print("⚠ Connected but no ready signal received")
            self.connected = True
            return True

        except serial.SerialException as e:
            print(f"ERROR: Failed to connect: {e}")
            return False

    def disconnect(self):
        """Disconnect from hardware"""
        if self.streaming:
            self.stop_streaming()

        if self.serial and self.serial.is_open:
            self.serial.close()
            self.connected = False
            self.status.connected = False
            print("Disconnected from hardware")

    def send_command(self, command: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        Send command to Arduino

        Args:
            command: Command dictionary

        Returns:
            Response dictionary or None
        """
        if not self.connected:
            print("ERROR: Not connected")
            return None

        try:
            # Send command as JSON
            cmd_str = json.dumps(command)
            self.serial.write((cmd_str + '\n').encode('utf-8'))

            # Read response
            response_line = self.serial.readline().decode('utf-8').strip()
            if response_line:
                return json.loads(response_line)

        except Exception as e:
            print(f"ERROR: Command failed: {e}")

        return None

    def start_streaming(self, callback: Optional[Callable] = None):
        """
        Start EMG data streaming

        Args:
            callback: Optional callback function for EMG data
        """
        if self.streaming:
            print("Already streaming")
            return

        self.emg_callback = callback

        # Send start command
        response = self.send_command({"cmd": "start_stream"})
        if response and response.get("status") == "streaming":
            self.streaming = True
            self.status.streaming = True

            # Start stream reader thread
            self.stream_thread = threading.Thread(target=self._stream_reader, daemon=True)
            self.stream_thread.start()

            print("✓ Streaming started")

    def stop_streaming(self):
        """Stop EMG data streaming"""
        if not self.streaming:
            return

        # Send stop command
        self.send_command({"cmd": "stop_stream"})

        self.streaming = False
        self.status.streaming = False

        if self.stream_thread:
            self.stream_thread.join(timeout=2.0)

        print("Streaming stopped")

    def _stream_reader(self):
        """Background thread for reading EMG stream"""
        while self.streaming:
            try:
                if self.serial.in_waiting:
                    line = self.serial.readline().decode('utf-8').strip()
                    if line and line.startswith('{"emg"'):
                        data = json.loads(line)

                        # Put in queue
                        if not self.emg_queue.full():
                            self.emg_queue.put(data)

                        # Call callback if provided
                        if self.emg_callback:
                            self.emg_callback(data)

            except Exception as e:
                print(f"Stream error: {e}")
                time.sleep(0.1)

    def execute_gesture(self, gesture_type: str) -> bool:
        """
        Execute gesture on physical hardware

        Args:
            gesture_type: Gesture name

        Returns:
            True if successful
        """
        response = self.send_command({
            "cmd": "gesture",
            "type": gesture_type
        })

        if response and response.get("status") == "gesture_executed":
            print(f"✓ Executed gesture: {gesture_type}")
            return True

        return False

    def get_emg_buffer(self) -> Optional[np.ndarray]:
        """
        Get EMG buffer from Arduino

        Returns:
            EMG data array [samples x channels]
        """
        response = self.send_command({"cmd": "get_buffer"})

        if response and "buffer" in response:
            buffer_data = response["buffer"]

            # Convert to numpy array
            channels = []
            for ch in range(8):  # 8 channels
                ch_key = f"ch{ch}"
                if ch_key in buffer_data:
                    channels.append(buffer_data[ch_key])

            if channels:
                return np.array(channels).T  # Transpose to [samples x channels]

        return None

    def set_radiation_dose(self, dose_msv: int):
        """
        Set simulated radiation dose

        Args:
            dose_msv: Radiation dose in mSv
        """
        response = self.send_command({
            "cmd": "set_radiation",
            "dose": dose_msv
        })

        if response and response.get("status") == "radiation_set":
            self.status.radiation_dose = dose_msv
            print(f"✓ Radiation dose set to {dose_msv} mSv")

    def calibrate(self) -> Optional[List[int]]:
        """
        Calibrate EMG sensors

        Returns:
            Baseline values for each channel
        """
        print("Calibrating sensors (keep hand relaxed)...")

        response = self.send_command({"cmd": "calibrate"})

        if response and "calibration" in response:
            baselines = response["calibration"]
            print(f"✓ Calibration complete: {baselines}")
            return baselines

        return None

    def get_status(self) -> HardwareStatus:
        """Get hardware status"""
        response = self.send_command({"cmd": "status"})

        if response and "status" in response:
            status_data = response["status"]
            self.status = HardwareStatus(
                connected=True,
                streaming=status_data.get("streaming", False),
                radiation_dose=status_data.get("radiation_dose", 0),
                uptime_ms=status_data.get("uptime", 0),
                last_update=datetime.now()
            )

        return self.status


class LAMHardwareIntegration:
    """
    High-level integration between LAM and physical prosthetics
    Combines hardware bridge with LAM control system
    """

    def __init__(self, port: Optional[str] = None):
        """Initialize LAM hardware integration"""
        from prosthetics_integration import ProstheticsController

        self.hardware = ArduinoProstheticBridge(port=port)
        self.lam_controller = ProstheticsController(num_channels=8)

        self.gesture_history: List[Dict[str, Any]] = []

    def connect(self) -> bool:
        """Connect to hardware"""
        return self.hardware.connect()

    def disconnect(self):
        """Disconnect from hardware"""
        self.hardware.disconnect()

    def execute_lam_controlled_gesture(self, radiation_dose: int = 0) -> Dict[str, Any]:
        """
        Execute gesture using LAM control with hardware

        Args:
            radiation_dose: Current radiation dose (mSv)

        Returns:
            Execution result
        """
        # Get EMG buffer from hardware
        print("\nAcquiring EMG data from hardware...")
        emg_data = self.hardware.get_emg_buffer()

        if emg_data is None:
            return {"error": "Failed to get EMG data"}

        print(f"  EMG data shape: {emg_data.shape}")

        # Process with LAM
        print("Processing with LAM...")
        result = self.lam_controller.execute_gesture(
            emg_data,
            context={"radiation_dose": radiation_dose}
        )

        print(f"  Recognized gesture: {result['gesture']}")
        print(f"  Confidence: {result['confidence']:.3f}")
        print(f"  Temporal displacement: {result['temporal_displacement']:.4f}s")

        # Execute on hardware
        print("Executing on hardware...")
        success = self.hardware.execute_gesture(result["gesture"])

        result["hardware_executed"] = success

        # Record history
        self.gesture_history.append({
            "timestamp": datetime.now().isoformat(),
            "gesture": result["gesture"],
            "confidence": result["confidence"],
            "radiation_dose": radiation_dose,
            "hardware_success": success
        })

        return result

    def run_continuous_control(self, duration_seconds: int = 60):
        """
        Run continuous LAM-controlled operation

        Args:
            duration_seconds: Duration to run
        """
        print(f"\nStarting continuous LAM control ({duration_seconds}s)...")

        start_time = time.time()

        while time.time() - start_time < duration_seconds:
            # Execute gesture
            result = self.execute_lam_controlled_gesture()

            if "error" not in result:
                print(f"  [{time.time() - start_time:.1f}s] Gesture: {result['gesture']} "
                      f"(confidence: {result['confidence']:.2f})")

            # Wait before next execution
            time.sleep(1.0)

        print(f"\n✓ Completed {len(self.gesture_history)} gestures")

    def get_statistics(self) -> Dict[str, Any]:
        """Get control statistics"""
        if not self.gesture_history:
            return {"error": "No gesture history"}

        gestures = [g["gesture"] for g in self.gesture_history]
        confidences = [g["confidence"] for g in self.gesture_history]
        successes = sum(1 for g in self.gesture_history if g["hardware_success"])

        return {
            "total_gestures": len(self.gesture_history),
            "unique_gestures": len(set(gestures)),
            "success_rate": successes / len(self.gesture_history),
            "avg_confidence": np.mean(confidences),
            "gesture_distribution": {
                g: gestures.count(g) for g in set(gestures)
            }
        }


def demo_hardware_integration():
    """Demonstration of hardware integration"""
    print("=" * 70)
    print("MotorHandPro - Hardware Integration Demo")
    print("=" * 70)

    # Initialize integration
    integration = LAMHardwareIntegration()

    # Connect to hardware
    print("\n[1] Connecting to hardware...")
    if not integration.connect():
        print("\n⚠ Hardware not available. Running in simulation mode.")
        print("  To use real hardware:")
        print("  1. Upload prosthetic_hand.ino to Arduino")
        print("  2. Connect Arduino via USB")
        print("  3. Run this demo again")
        return

    # Calibrate sensors
    print("\n[2] Calibrating sensors...")
    baselines = integration.hardware.calibrate()

    # Execute LAM-controlled gesture
    print("\n[3] Executing LAM-controlled gesture...")
    result = integration.execute_lam_controlled_gesture(radiation_dose=50)

    print(f"\nResult:")
    print(f"  Gesture: {result.get('gesture')}")
    print(f"  Confidence: {result.get('confidence'):.3f}")
    print(f"  Hardware executed: {result.get('hardware_executed')}")

    # Get statistics
    print("\n[4] Statistics...")
    stats = integration.get_statistics()
    print(json.dumps(stats, indent=2))

    # Disconnect
    print("\n[5] Disconnecting...")
    integration.disconnect()

    print("\n" + "=" * 70)
    print("Demo complete!")
    print("=" * 70)


if __name__ == "__main__":
    demo_hardware_integration()
