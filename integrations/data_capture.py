#!/usr/bin/env python3
"""
Bi-directional Data Capture and Flow System for MotorHandPro
Integrates with SpaceX, Tesla, Firestorm, and visualization repositories

Copyright 2025 Donte Lightfoot - The Phoney Express LLC / Locked In Safety
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import json
import asyncio
import websockets
import requests
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
from datetime import datetime
import numpy as np
from pathlib import Path


@dataclass
class DataPacket:
    """Universal data packet for bi-directional flow"""
    timestamp: float
    source: str
    data_type: str
    payload: Dict[str, Any]
    metadata: Optional[Dict[str, Any]] = None


class BiDirectionalDataCapture:
    """
    Bi-directional data capture system that:
    1. Ingests data from external repositories (SpaceX, Tesla, Firestorm, Carla)
    2. Processes through Primal Logic framework
    3. Sends results back to visualization and control systems
    """

    def __init__(self, config_path: str = "integrations/repository_config.json"):
        self.config = self._load_config(config_path)
        self.data_buffer = []
        self.websocket_clients = set()
        self.running = False

    def _load_config(self, config_path: str) -> Dict:
        """Load repository configuration"""
        with open(config_path, 'r') as f:
            return json.load(f)

    # ==================== INBOUND DATA CAPTURE ====================

    async def capture_spacex_data(self) -> DataPacket:
        """
        Capture data from SpaceX API
        Inbound: telemetry, launch_data, vehicle_state
        """
        try:
            # SpaceX API endpoint
            url = "https://api.spacexdata.com/v5/launches/latest"
            response = requests.get(url, timeout=5)

            if response.status_code == 200:
                data = response.json()

                return DataPacket(
                    timestamp=datetime.now().timestamp(),
                    source="SpaceX-API",
                    data_type="launch_telemetry",
                    payload={
                        "flight_number": data.get("flight_number"),
                        "name": data.get("name"),
                        "success": data.get("success"),
                        "details": data.get("details"),
                        "rocket_id": data.get("rocket")
                    },
                    metadata={
                        "integration_point": "Launch trajectory data",
                        "validation_target": "Rocket trajectory stability"
                    }
                )
        except Exception as e:
            print(f"Error capturing SpaceX data: {e}")
            return None

    async def capture_tesla_actuator_data(self, sequence_file: str = None) -> DataPacket:
        """
        Capture Tesla light-show actuator sequences
        Inbound: actuator_sequences, timing_data
        """
        # Simulated actuator data - in production, this would read from Tesla repos
        actuator_sequence = {
            "vehicle_id": "tesla_model_s_01",
            "sequence": [
                {"time": 0.0, "actuator": "front_left", "position": 0.0},
                {"time": 0.1, "actuator": "front_left", "position": 0.5},
                {"time": 0.2, "actuator": "front_left", "position": 1.0},
            ],
            "timing_precision_ms": 10
        }

        return DataPacket(
            timestamp=datetime.now().timestamp(),
            source="Tesla-light-show",
            data_type="actuator_control",
            payload=actuator_sequence,
            metadata={
                "integration_point": "Vehicle actuator control",
                "validation_target": "Multi-actuator synchronization"
            }
        )

    async def capture_drone_telemetry(self, mavlink_stream: bool = False) -> DataPacket:
        """
        Capture Firestorm/PX4 drone telemetry
        Inbound: flight_telemetry, imu_data, control_commands
        """
        # Simulated PX4 telemetry - in production, use MAVLink protocol
        telemetry = {
            "vehicle_type": "firestorm_tempest",
            "position": {"x": 10.5, "y": 20.3, "z": 50.0},
            "velocity": {"vx": 2.5, "vy": 1.2, "vz": -0.5},
            "attitude": {"roll": 0.05, "pitch": 0.02, "yaw": 1.57},
            "imu": {
                "accel": {"x": 0.1, "y": 0.05, "z": 9.81},
                "gyro": {"x": 0.01, "y": 0.02, "z": 0.0}
            },
            "control_mode": "STABILIZE"
        }

        return DataPacket(
            timestamp=datetime.now().timestamp(),
            source="PX4-Autopilot",
            data_type="flight_telemetry",
            payload=telemetry,
            metadata={
                "integration_point": "Flight control algorithms",
                "validation_target": "Multi-rotor stabilization"
            }
        )

    async def capture_carla_simulation(self) -> DataPacket:
        """
        Capture CARLA simulator vehicle dynamics
        Inbound: simulation_state, vehicle_dynamics
        """
        # Simulated CARLA data - in production, connect to CARLA Python API
        simulation_state = {
            "vehicle_id": "ego_vehicle",
            "location": {"x": 150.2, "y": 30.5, "z": 0.5},
            "rotation": {"pitch": 0.0, "yaw": 90.0, "roll": 0.0},
            "velocity": {"x": 15.0, "y": 0.0, "z": 0.0},
            "acceleration": {"x": 0.5, "y": 0.0, "z": 0.0},
            "steering_angle": 0.15,
            "throttle": 0.6,
            "brake": 0.0,
            "sensors": {
                "lidar_points": 10000,
                "camera_frames": 1
            }
        }

        return DataPacket(
            timestamp=datetime.now().timestamp(),
            source="CARLA-Simulator",
            data_type="vehicle_dynamics",
            payload=simulation_state,
            metadata={
                "integration_point": "Vehicle dynamics simulation",
                "validation_target": "Autonomous vehicle control"
            }
        )

    # ==================== PRIMAL LOGIC PROCESSING ====================

    def process_with_primal_logic(self, data_packet: DataPacket) -> Dict[str, Any]:
        """
        Process incoming data through Primal Logic framework
        Apply exponential memory weighting and stability analysis
        """
        from extras.primal.primal_constants import DONTE_CONSTANT, KERNEL_MU

        # Extract control-relevant data
        payload = data_packet.payload

        # Primal Logic stability analysis
        result = {
            "timestamp": data_packet.timestamp,
            "source": data_packet.source,
            "primal_logic_analysis": {
                "D_constant": DONTE_CONSTANT,
                "lambda": KERNEL_MU,
                "stability_metric": None,
                "lipschitz_estimate": None,
                "control_energy": None
            }
        }

        # Example: Process drone telemetry
        if data_packet.data_type == "flight_telemetry":
            imu = payload.get("imu", {})
            accel = imu.get("accel", {})

            # Compute control energy functional
            accel_magnitude = np.sqrt(accel.get("x", 0)**2 +
                                     accel.get("y", 0)**2 +
                                     accel.get("z", 0)**2)

            # Apply exponential memory weighting
            time_constant = 1.0 / KERNEL_MU  # ~5.92 seconds
            weighted_accel = accel_magnitude * np.exp(-KERNEL_MU * 0.1)

            result["primal_logic_analysis"]["control_energy"] = weighted_accel
            result["primal_logic_analysis"]["stability_metric"] = abs(accel_magnitude - 9.81)

        # Example: Process Tesla actuator data
        elif data_packet.data_type == "actuator_control":
            sequence = payload.get("sequence", [])
            if len(sequence) > 1:
                # Analyze timing stability
                time_diffs = [sequence[i+1]["time"] - sequence[i]["time"]
                             for i in range(len(sequence)-1)]
                timing_std = np.std(time_diffs) if time_diffs else 0

                result["primal_logic_analysis"]["timing_stability"] = timing_std
                result["primal_logic_analysis"]["lipschitz_estimate"] = timing_std / DONTE_CONSTANT

        return result

    # ==================== OUTBOUND DATA TRANSMISSION ====================

    async def send_to_visualization(self, processed_data: Dict[str, Any]):
        """
        Send processed data to visualization systems (matplotlib, VTK, three.js)
        Outbound: primal_logic_predictions, stability_analysis, 3d_renders
        """
        visualization_packet = {
            "type": "visualization_update",
            "timestamp": datetime.now().isoformat(),
            "data": processed_data,
            "render_targets": ["matplotlib", "vtk.js", "three.js"]
        }

        # Send to all connected WebSocket clients (control panel)
        if self.websocket_clients:
            message = json.dumps(visualization_packet)
            await asyncio.gather(
                *[client.send(message) for client in self.websocket_clients],
                return_exceptions=True
            )

    async def send_to_control_panel(self, control_data: Dict[str, Any]):
        """
        Send data to web control panel
        Outbound: real-time metrics, control parameters, system state
        """
        control_packet = {
            "type": "control_update",
            "timestamp": datetime.now().isoformat(),
            "data": control_data
        }

        if self.websocket_clients:
            message = json.dumps(control_packet)
            await asyncio.gather(
                *[client.send(message) for client in self.websocket_clients],
                return_exceptions=True
            )

    async def export_to_latex(self, validation_results: Dict[str, Any], output_path: str):
        """
        Export validation results to LaTeX document
        Outbound: pdf_reports, documentation
        """
        latex_template = r"""
\documentclass{article}
\usepackage{amsmath}
\usepackage{graphicx}
\title{Primal Logic Framework Validation Report}
\author{MotorHandPro Integration System}
\date{\today}

\begin{document}
\maketitle

\section{Validation Results}

\subsection{Repository: """ + validation_results.get("repository", "Unknown") + r"""}

\begin{equation}
\psi(t) = -\lambda \psi(t) + K_E e(t)
\end{equation}

where $\lambda = """ + str(validation_results.get("lambda", 0.16905)) + r"""$ s$^{-1}$.

\subsection{Stability Metrics}

Lipschitz constant: $L = """ + str(validation_results.get("lipschitz", 0.0)) + r"""$

Control energy: $E_c = """ + str(validation_results.get("control_energy", 0.0)) + r"""$

\end{document}
"""

        # Write LaTeX file
        Path(output_path).parent.mkdir(parents=True, exist_ok=True)
        with open(output_path, 'w') as f:
            f.write(latex_template)

        print(f"LaTeX report exported to: {output_path}")

    # ==================== WEBSOCKET SERVER FOR REAL-TIME DATA ====================

    async def websocket_handler(self, websocket, path):
        """Handle WebSocket connections for real-time data streaming"""
        self.websocket_clients.add(websocket)
        print(f"Client connected. Total clients: {len(self.websocket_clients)}")

        try:
            async for message in websocket:
                # Handle incoming control commands from web panel
                data = json.loads(message)

                if data.get("type") == "control_command":
                    await self.handle_control_command(data)
                elif data.get("type") == "parameter_update":
                    await self.handle_parameter_update(data)

        except websockets.exceptions.ConnectionClosed:
            pass
        finally:
            self.websocket_clients.remove(websocket)
            print(f"Client disconnected. Total clients: {len(self.websocket_clients)}")

    async def handle_control_command(self, command: Dict[str, Any]):
        """Handle control commands from web panel"""
        print(f"Received control command: {command}")
        # Implement control logic here

    async def handle_parameter_update(self, update: Dict[str, Any]):
        """Handle parameter updates from web panel"""
        print(f"Received parameter update: {update}")
        # Update system parameters here

    # ==================== MAIN DATA FLOW LOOP ====================

    async def run_data_flow(self):
        """Main bi-directional data flow loop"""
        self.running = True

        print("Starting bi-directional data capture system...")
        print("Data flow: SpaceX ↔ Tesla ↔ Firestorm ↔ CARLA ↔ Primal Logic ↔ Visualization")

        while self.running:
            try:
                # Capture data from all sources (INBOUND)
                spacex_data = await self.capture_spacex_data()
                tesla_data = await self.capture_tesla_actuator_data()
                drone_data = await self.capture_drone_telemetry()
                carla_data = await self.capture_carla_simulation()

                # Process through Primal Logic
                for data_packet in [spacex_data, tesla_data, drone_data, carla_data]:
                    if data_packet:
                        processed = self.process_with_primal_logic(data_packet)

                        # Send to visualization (OUTBOUND)
                        await self.send_to_visualization(processed)

                        # Send to control panel (OUTBOUND)
                        await self.send_to_control_panel(processed)

                # Wait before next cycle
                await asyncio.sleep(1.0)

            except Exception as e:
                print(f"Error in data flow loop: {e}")
                await asyncio.sleep(5.0)

    async def start_websocket_server(self, host: str = "localhost", port: int = 8765):
        """Start WebSocket server for real-time communication"""
        async with websockets.serve(self.websocket_handler, host, port):
            print(f"WebSocket server started on ws://{host}:{port}")
            await asyncio.Future()  # Run forever

    async def run(self):
        """Run both data flow and WebSocket server"""
        await asyncio.gather(
            self.run_data_flow(),
            self.start_websocket_server()
        )


if __name__ == "__main__":
    # Initialize and run the bi-directional data capture system
    capture_system = BiDirectionalDataCapture()

    try:
        asyncio.run(capture_system.run())
    except KeyboardInterrupt:
        print("\nShutting down data capture system...")
        capture_system.running = False
