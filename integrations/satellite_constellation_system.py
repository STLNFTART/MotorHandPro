"""
Satellite Constellation Management System
=========================================
Real-time tracking and management of mega-constellations (50,000+ satellites).
Includes WebSocket/REST API for dashboard integration.

Author: MotorHandPro Integration Team
License: MIT
"""

import asyncio
import json
import logging
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Set
import numpy as np
from aiohttp import web
import aiohttp
from dataclasses import asdict

from satellite_orbital_mechanics import (
    ConstellationGenerator,
    ConstellationTracker,
    SatelliteState,
    OrbitalElements
)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class SatelliteConstellationSystem:
    """
    Main satellite constellation management system.
    Handles 50,000+ satellites with real-time tracking and API endpoints.
    """

    def __init__(self, num_satellites: int = 50000,
                 websocket_port: int = 8766,
                 http_port: int = 8080):
        self.num_satellites = num_satellites
        self.websocket_port = websocket_port
        self.http_port = http_port

        # Constellation data
        self.tracker: Optional[ConstellationTracker] = None
        self.orbital_elements: List[OrbitalElements] = []

        # Real-time state cache
        self.current_states: Dict[int, SatelliteState] = {}
        self.last_update: Optional[datetime] = None

        # WebSocket clients
        self.websocket_clients: Set = set()

        # Statistics
        self.stats = {
            'total_satellites': num_satellites,
            'active_satellites': 0,
            'propagations_per_second': 0,
            'api_requests_total': 0,
            'websocket_clients': 0
        }

        # Performance monitoring
        self.propagation_times = []

    async def initialize(self):
        """Initialize the constellation system"""
        logger.info(f"Initializing satellite constellation with {self.num_satellites} satellites...")

        # Generate constellation
        start_time = datetime.now()
        logger.info("Generating Starlink-like orbital elements...")

        self.orbital_elements = ConstellationGenerator.generate_starlink_constellation(
            num_satellites=self.num_satellites
        )

        generation_time = (datetime.now() - start_time).total_seconds()
        logger.info(f"Generated {len(self.orbital_elements)} satellites in {generation_time:.2f}s")

        # Initialize tracker
        logger.info("Initializing constellation tracker...")
        self.tracker = ConstellationTracker(self.orbital_elements)

        # Initial propagation
        logger.info("Performing initial propagation...")
        await self.update_constellation_state()

        logger.info("Satellite constellation system initialized successfully!")

    async def update_constellation_state(self, satellite_ids: Optional[List[int]] = None):
        """Update satellite positions (batch propagation)"""

        if self.tracker is None:
            raise RuntimeError("Constellation tracker not initialized")

        start_time = datetime.now()
        current_time = datetime.utcnow()

        # Propagate satellites
        states = await asyncio.to_thread(
            self.tracker.propagate_all,
            current_time,
            satellite_ids
        )

        # Update cache
        for state in states:
            self.current_states[state.satellite_id] = state

        self.last_update = current_time

        # Update statistics
        propagation_time = (datetime.now() - start_time).total_seconds()
        self.propagation_times.append(propagation_time)
        if len(self.propagation_times) > 100:
            self.propagation_times.pop(0)

        self.stats['active_satellites'] = len(states)
        self.stats['propagations_per_second'] = len(states) / propagation_time if propagation_time > 0 else 0

        logger.debug(f"Updated {len(states)} satellites in {propagation_time:.3f}s "
                    f"({self.stats['propagations_per_second']:.0f} sats/s)")

        return states

    async def get_satellite_state(self, satellite_id: int) -> Optional[SatelliteState]:
        """Get current state of specific satellite"""
        if satellite_id in self.current_states:
            return self.current_states[satellite_id]

        # Not in cache, propagate it
        if self.tracker and satellite_id in self.tracker.satellites:
            states = await self.update_constellation_state([satellite_id])
            return states[0] if states else None

        return None

    async def get_satellites_in_view(self, lat: float, lon: float, alt: float = 0.0,
                                     min_elevation: float = 10.0) -> List[SatelliteState]:
        """Get satellites visible from location"""

        if self.tracker is None:
            return []

        current_time = datetime.utcnow()

        visible = await asyncio.to_thread(
            self.tracker.get_satellites_in_view,
            lat, lon, alt, current_time, min_elevation
        )

        return visible

    async def get_coverage_statistics(self, grid_resolution: int = 50) -> Dict:
        """Calculate global coverage statistics"""

        if self.tracker is None:
            return {}

        logger.info(f"Calculating coverage statistics (grid: {grid_resolution}x{grid_resolution})...")

        start_time = datetime.now()
        current_time = datetime.utcnow()

        stats = await asyncio.to_thread(
            self.tracker.calculate_coverage_stats,
            current_time,
            grid_resolution
        )

        calc_time = (datetime.now() - start_time).total_seconds()
        logger.info(f"Coverage calculation complete in {calc_time:.2f}s")
        logger.info(f"  Average satellites per point: {stats['avg_satellites_per_point']:.1f}")
        logger.info(f"  Coverage: {stats['percent_coverage']:.1f}%")

        return stats

    # ========== REST API Endpoints ==========

    async def handle_get_status(self, request: web.Request) -> web.Response:
        """GET /api/status - System status"""
        self.stats['api_requests_total'] += 1
        self.stats['websocket_clients'] = len(self.websocket_clients)

        status = {
            'status': 'operational',
            'constellation': {
                'total_satellites': self.num_satellites,
                'active_satellites': len(self.current_states),
                'last_update': self.last_update.isoformat() if self.last_update else None
            },
            'performance': {
                'avg_propagation_time': np.mean(self.propagation_times) if self.propagation_times else 0,
                'propagations_per_second': self.stats['propagations_per_second']
            },
            'api': {
                'websocket_port': self.websocket_port,
                'http_port': self.http_port,
                'connected_clients': len(self.websocket_clients),
                'total_requests': self.stats['api_requests_total']
            }
        }

        return web.json_response(status)

    async def handle_get_satellite(self, request: web.Request) -> web.Response:
        """GET /api/satellite/{id} - Get satellite state"""
        self.stats['api_requests_total'] += 1

        try:
            sat_id = int(request.match_info['id'])
            state = await self.get_satellite_state(sat_id)

            if state is None:
                return web.json_response({'error': 'Satellite not found'}, status=404)

            return web.json_response(state.to_dict())

        except ValueError:
            return web.json_response({'error': 'Invalid satellite ID'}, status=400)

    async def handle_get_satellites_batch(self, request: web.Request) -> web.Response:
        """POST /api/satellites/batch - Get multiple satellites"""
        self.stats['api_requests_total'] += 1

        try:
            data = await request.json()
            sat_ids = data.get('satellite_ids', [])

            if not sat_ids:
                return web.json_response({'error': 'No satellite IDs provided'}, status=400)

            states = await self.update_constellation_state(sat_ids)
            return web.json_response({
                'satellites': [s.to_dict() for s in states],
                'count': len(states)
            })

        except Exception as e:
            logger.error(f"Error in batch request: {e}")
            return web.json_response({'error': str(e)}, status=500)

    async def handle_get_visible_satellites(self, request: web.Request) -> web.Response:
        """GET /api/visible?lat={lat}&lon={lon}&alt={alt}&min_el={el}"""
        self.stats['api_requests_total'] += 1

        try:
            lat = float(request.query.get('lat', 0))
            lon = float(request.query.get('lon', 0))
            alt = float(request.query.get('alt', 0))
            min_el = float(request.query.get('min_el', 10))

            visible = await self.get_satellites_in_view(lat, lon, alt, min_el)

            return web.json_response({
                'observer': {
                    'latitude': lat,
                    'longitude': lon,
                    'altitude': alt,
                    'min_elevation': min_el
                },
                'visible_satellites': [s.to_dict() for s in visible],
                'count': len(visible)
            })

        except ValueError as e:
            return web.json_response({'error': 'Invalid parameters'}, status=400)

    async def handle_get_coverage(self, request: web.Request) -> web.Response:
        """GET /api/coverage?resolution={res}"""
        self.stats['api_requests_total'] += 1

        try:
            resolution = int(request.query.get('resolution', 50))

            if resolution > 200:
                return web.json_response({'error': 'Resolution too high (max 200)'}, status=400)

            coverage = await self.get_coverage_statistics(resolution)
            return web.json_response(coverage)

        except ValueError:
            return web.json_response({'error': 'Invalid resolution'}, status=400)

    async def handle_get_constellation_info(self, request: web.Request) -> web.Response:
        """GET /api/constellation - Get constellation metadata"""
        self.stats['api_requests_total'] += 1

        # Analyze orbital shells
        shells = {}
        for elem in self.orbital_elements[:1000]:  # Sample first 1000
            key = f"{elem.inclination:.1f}°"
            if key not in shells:
                shells[key] = {
                    'inclination': elem.inclination,
                    'count': 0,
                    'avg_altitude': 0
                }
            shells[key]['count'] += 1

        return web.json_response({
            'total_satellites': self.num_satellites,
            'orbital_shells': list(shells.values()),
            'epoch': self.orbital_elements[0].epoch.isoformat() if self.orbital_elements else None,
            'constellation_type': 'Starlink-like LEO Mega-Constellation'
        })

    # ========== WebSocket Handler ==========

    async def websocket_handler(self, request: web.Request) -> web.WebSocketResponse:
        """WebSocket endpoint for real-time satellite data streaming"""

        ws = web.WebSocketResponse()
        await ws.prepare(request)

        self.websocket_clients.add(ws)
        logger.info(f"WebSocket client connected. Total clients: {len(self.websocket_clients)}")

        try:
            # Send initial status
            await ws.send_json({
                'type': 'status',
                'data': {
                    'total_satellites': self.num_satellites,
                    'message': 'Connected to Satellite Constellation System'
                }
            })

            async for msg in ws:
                if msg.type == aiohttp.WSMsgType.TEXT:
                    try:
                        data = json.loads(msg.data)
                        await self.handle_websocket_message(ws, data)
                    except json.JSONDecodeError:
                        await ws.send_json({'type': 'error', 'message': 'Invalid JSON'})

                elif msg.type == aiohttp.WSMsgType.ERROR:
                    logger.error(f'WebSocket error: {ws.exception()}')

        finally:
            self.websocket_clients.discard(ws)
            logger.info(f"WebSocket client disconnected. Total clients: {len(self.websocket_clients)}")

        return ws

    async def handle_websocket_message(self, ws: web.WebSocketResponse, data: Dict):
        """Handle incoming WebSocket messages"""

        msg_type = data.get('type')

        if msg_type == 'subscribe_satellite':
            sat_id = data.get('satellite_id')
            if sat_id:
                state = await self.get_satellite_state(sat_id)
                if state:
                    await ws.send_json({
                        'type': 'satellite_update',
                        'data': state.to_dict()
                    })

        elif msg_type == 'subscribe_visible':
            lat = data.get('latitude')
            lon = data.get('longitude')
            if lat is not None and lon is not None:
                visible = await self.get_satellites_in_view(lat, lon)
                await ws.send_json({
                    'type': 'visible_satellites',
                    'data': {
                        'satellites': [s.to_dict() for s in visible],
                        'count': len(visible)
                    }
                })

        elif msg_type == 'get_stats':
            await ws.send_json({
                'type': 'statistics',
                'data': self.stats
            })

    async def broadcast_update(self, data: Dict):
        """Broadcast update to all WebSocket clients"""
        if not self.websocket_clients:
            return

        message = json.dumps(data)
        dead_clients = set()

        for ws in self.websocket_clients:
            try:
                await ws.send_str(message)
            except Exception as e:
                logger.error(f"Error broadcasting to client: {e}")
                dead_clients.add(ws)

        # Clean up dead connections
        self.websocket_clients -= dead_clients

    # ========== Background Tasks ==========

    async def periodic_update_task(self, update_interval: int = 10):
        """Periodically update constellation state and broadcast"""

        logger.info(f"Starting periodic update task (interval: {update_interval}s)")

        while True:
            try:
                # Update sample of satellites (not all 50,000 every time)
                sample_size = min(1000, self.num_satellites)
                sample_ids = np.random.choice(
                    list(self.tracker.satellites.keys()),
                    sample_size,
                    replace=False
                ).tolist()

                states = await self.update_constellation_state(sample_ids)

                # Broadcast update to WebSocket clients
                if self.websocket_clients:
                    await self.broadcast_update({
                        'type': 'constellation_update',
                        'data': {
                            'timestamp': datetime.utcnow().isoformat(),
                            'updated_satellites': len(states),
                            'sample': [states[0].to_dict()] if states else []
                        }
                    })

                await asyncio.sleep(update_interval)

            except Exception as e:
                logger.error(f"Error in periodic update: {e}")
                await asyncio.sleep(update_interval)

    # ========== Main Server ==========

    async def start_server(self):
        """Start HTTP and WebSocket server"""

        app = web.Application()

        # REST API routes
        app.router.add_get('/api/status', self.handle_get_status)
        app.router.add_get('/api/satellite/{id}', self.handle_get_satellite)
        app.router.add_post('/api/satellites/batch', self.handle_get_satellites_batch)
        app.router.add_get('/api/visible', self.handle_get_visible_satellites)
        app.router.add_get('/api/coverage', self.handle_get_coverage)
        app.router.add_get('/api/constellation', self.handle_get_constellation_info)

        # WebSocket route
        app.router.add_get('/ws', self.websocket_handler)

        # Start background tasks
        app.on_startup.append(lambda app: asyncio.create_task(self.periodic_update_task()))

        logger.info(f"Starting HTTP server on port {self.http_port}")
        logger.info(f"WebSocket endpoint: ws://localhost:{self.http_port}/ws")
        logger.info(f"REST API: http://localhost:{self.http_port}/api/status")

        runner = web.AppRunner(app)
        await runner.setup()

        site = web.TCPSite(runner, 'localhost', self.http_port)
        await site.start()

        logger.info("Server started successfully!")

        # Keep running
        while True:
            await asyncio.sleep(3600)


async def main():
    """Main entry point"""

    logger.info("="*70)
    logger.info("  SATELLITE CONSTELLATION SYSTEM - 50,000 Satellites")
    logger.info("  Starlink-like Mega-Constellation Tracker")
    logger.info("="*70)

    # Create system
    system = SatelliteConstellationSystem(
        num_satellites=50000,
        http_port=8080
    )

    # Initialize
    await system.initialize()

    # Start server
    await system.start_server()


if __name__ == "__main__":
    asyncio.run(main())
