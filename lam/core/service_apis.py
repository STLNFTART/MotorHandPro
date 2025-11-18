"""
MotorHandPro Service API Integration
Complete API integration for all services (Docker, Database, MQTT, Redis, External APIs)
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import asyncio
import aiohttp
import asyncpg
import paho.mqtt.client as mqtt
import redis
import docker
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
import json


@dataclass
class ServiceAPIConfig:
    """Configuration for service API"""
    name: str
    host: str
    port: int
    username: Optional[str] = None
    password: Optional[str] = None
    database: Optional[str] = None
    ssl: bool = False


class DockerAPIClient:
    """Docker API client for container management"""

    def __init__(self):
        try:
            self.client = docker.from_env()
        except Exception as e:
            print(f"⚠️  Docker client initialization failed: {e}")
            self.client = None

    def list_containers(self, all: bool = True) -> List[Dict[str, Any]]:
        """List all containers"""
        if not self.client:
            return []

        containers = self.client.containers.list(all=all)
        return [{
            'id': c.id[:12],
            'name': c.name,
            'status': c.status,
            'image': c.image.tags[0] if c.image.tags else c.image.id[:12],
            'ports': c.ports
        } for c in containers]

    def get_container_logs(self, container_name: str, tail: int = 100) -> str:
        """Get container logs"""
        if not self.client:
            return ""

        try:
            container = self.client.containers.get(container_name)
            return container.logs(tail=tail).decode('utf-8')
        except Exception as e:
            return f"Error: {e}"

    def get_container_stats(self, container_name: str) -> Dict[str, Any]:
        """Get container resource usage stats"""
        if not self.client:
            return {}

        try:
            container = self.client.containers.get(container_name)
            stats = container.stats(stream=False)
            return stats
        except Exception as e:
            return {'error': str(e)}

    def restart_container(self, container_name: str) -> bool:
        """Restart a container"""
        if not self.client:
            return False

        try:
            container = self.client.containers.get(container_name)
            container.restart()
            return True
        except Exception as e:
            print(f"Error restarting container: {e}")
            return False

    def start_container(self, container_name: str) -> bool:
        """Start a stopped container"""
        if not self.client:
            return False

        try:
            container = self.client.containers.get(container_name)
            container.start()
            return True
        except Exception as e:
            print(f"Error starting container: {e}")
            return False

    def stop_container(self, container_name: str) -> bool:
        """Stop a running container"""
        if not self.client:
            return False

        try:
            container = self.client.containers.get(container_name)
            container.stop()
            return True
        except Exception as e:
            print(f"Error stopping container: {e}")
            return False


class TimescaleDBAPIClient:
    """TimescaleDB/PostgreSQL API client"""

    def __init__(self, config: ServiceAPIConfig):
        self.config = config
        self.pool = None

    async def connect(self):
        """Connect to database"""
        try:
            dsn = f"postgresql://{self.config.username}:{self.config.password}@{self.config.host}:{self.config.port}/{self.config.database}"
            self.pool = await asyncpg.create_pool(dsn, min_size=2, max_size=10)
            return True
        except Exception as e:
            print(f"Database connection failed: {e}")
            return False

    async def execute_query(self, query: str, *args) -> List[Dict[str, Any]]:
        """Execute a query and return results"""
        if not self.pool:
            await self.connect()

        async with self.pool.acquire() as conn:
            rows = await conn.fetch(query, *args)
            return [dict(row) for row in rows]

    async def get_telemetry_summary(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Get recent telemetry summary"""
        query = """
            SELECT spacecraft_id, COUNT(*) as count, MAX(time) as last_update
            FROM telemetry.spacecraft
            GROUP BY spacecraft_id
            ORDER BY last_update DESC
            LIMIT $1
        """
        return await self.execute_query(query, limit)

    async def get_agp_state_summary(self, limit: int = 10) -> List[Dict[str, Any]]:
        """Get recent AGP state summary"""
        query = """
            SELECT system_id, lipschitz_constant, control_mode, time
            FROM telemetry.agp_state
            ORDER BY time DESC
            LIMIT $1
        """
        return await self.execute_query(query, limit)

    async def get_active_experiments(self) -> List[Dict[str, Any]]:
        """Get active experiments"""
        query = """
            SELECT id, name, status, created_at
            FROM experiments.experiments
            WHERE status IN ('pending', 'running')
            ORDER BY created_at DESC
        """
        return await self.execute_query(query)

    async def close(self):
        """Close database connection"""
        if self.pool:
            await self.pool.close()


class MQTTAPIClient:
    """MQTT API client"""

    def __init__(self, config: ServiceAPIConfig):
        self.config = config
        self.client = None
        self.connected = False
        self.messages = []

    def connect(self) -> bool:
        """Connect to MQTT broker"""
        try:
            self.client = mqtt.Client()

            if self.config.username and self.config.password:
                self.client.username_pw_set(self.config.username, self.config.password)

            self.client.on_connect = self._on_connect
            self.client.on_message = self._on_message

            self.client.connect(self.config.host, self.config.port, 60)
            self.client.loop_start()
            return True
        except Exception as e:
            print(f"MQTT connection failed: {e}")
            return False

    def _on_connect(self, client, userdata, flags, rc):
        """MQTT connection callback"""
        if rc == 0:
            self.connected = True
            print("✅ Connected to MQTT broker")
        else:
            print(f"❌ MQTT connection failed with code {rc}")

    def _on_message(self, client, userdata, msg):
        """MQTT message callback"""
        self.messages.append({
            'topic': msg.topic,
            'payload': msg.payload.decode('utf-8'),
            'timestamp': datetime.now()
        })

    def publish(self, topic: str, payload: str) -> bool:
        """Publish message to topic"""
        if not self.client or not self.connected:
            return False

        try:
            self.client.publish(topic, payload)
            return True
        except Exception as e:
            print(f"Publish failed: {e}")
            return False

    def subscribe(self, topic: str) -> bool:
        """Subscribe to topic"""
        if not self.client or not self.connected:
            return False

        try:
            self.client.subscribe(topic)
            return True
        except Exception as e:
            print(f"Subscribe failed: {e}")
            return False

    def get_recent_messages(self, topic_filter: Optional[str] = None, limit: int = 50) -> List[Dict]:
        """Get recent messages"""
        messages = self.messages[-limit:]
        if topic_filter:
            messages = [m for m in messages if topic_filter in m['topic']]
        return messages

    def disconnect(self):
        """Disconnect from MQTT broker"""
        if self.client:
            self.client.loop_stop()
            self.client.disconnect()
            self.connected = False


class RedisAPIClient:
    """Redis API client"""

    def __init__(self, config: ServiceAPIConfig):
        self.config = config
        self.client = None

    def connect(self) -> bool:
        """Connect to Redis"""
        try:
            self.client = redis.Redis(
                host=self.config.host,
                port=self.config.port,
                password=self.config.password,
                decode_responses=True
            )
            self.client.ping()
            return True
        except Exception as e:
            print(f"Redis connection failed: {e}")
            return False

    def get(self, key: str) -> Optional[str]:
        """Get value from Redis"""
        if not self.client:
            return None

        try:
            return self.client.get(key)
        except Exception as e:
            print(f"Redis get failed: {e}")
            return None

    def set(self, key: str, value: str, expire: Optional[int] = None) -> bool:
        """Set value in Redis"""
        if not self.client:
            return False

        try:
            if expire:
                self.client.setex(key, expire, value)
            else:
                self.client.set(key, value)
            return True
        except Exception as e:
            print(f"Redis set failed: {e}")
            return False

    def delete(self, key: str) -> bool:
        """Delete key from Redis"""
        if not self.client:
            return False

        try:
            self.client.delete(key)
            return True
        except Exception as e:
            print(f"Redis delete failed: {e}")
            return False

    def get_info(self) -> Dict[str, Any]:
        """Get Redis server info"""
        if not self.client:
            return {}

        try:
            return self.client.info()
        except Exception as e:
            return {'error': str(e)}


class PrometheusAPIClient:
    """Prometheus API client"""

    def __init__(self, host: str = "localhost", port: int = 9090):
        self.base_url = f"http://{host}:{port}"

    async def query(self, query: str) -> Dict[str, Any]:
        """Execute Prometheus query"""
        async with aiohttp.ClientSession() as session:
            try:
                url = f"{self.base_url}/api/v1/query"
                params = {'query': query}
                async with session.get(url, params=params) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}

    async def query_range(self, query: str, start: str, end: str, step: str = "15s") -> Dict[str, Any]:
        """Execute Prometheus range query"""
        async with aiohttp.ClientSession() as session:
            try:
                url = f"{self.base_url}/api/v1/query_range"
                params = {
                    'query': query,
                    'start': start,
                    'end': end,
                    'step': step
                }
                async with session.get(url, params=params) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}

    async def get_targets(self) -> Dict[str, Any]:
        """Get scrape targets"""
        async with aiohttp.ClientSession() as session:
            try:
                url = f"{self.base_url}/api/v1/targets"
                async with session.get(url) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}


class GrafanaAPIClient:
    """Grafana API client"""

    def __init__(self, host: str = "localhost", port: int = 3001, api_key: Optional[str] = None):
        self.base_url = f"http://{host}:{port}/api"
        self.headers = {}
        if api_key:
            self.headers['Authorization'] = f'Bearer {api_key}'

    async def get_dashboards(self) -> List[Dict[str, Any]]:
        """Get all dashboards"""
        async with aiohttp.ClientSession() as session:
            try:
                url = f"{self.base_url}/search?type=dash-db"
                async with session.get(url, headers=self.headers) as resp:
                    return await resp.json()
            except Exception as e:
                return [{'error': str(e)}]

    async def get_dashboard(self, uid: str) -> Dict[str, Any]:
        """Get dashboard by UID"""
        async with aiohttp.ClientSession() as session:
            try:
                url = f"{self.base_url}/dashboards/uid/{uid}"
                async with session.get(url, headers=self.headers) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}


class ExternalAPIClients:
    """Collection of external API clients"""

    @staticmethod
    async def spacex_latest_launch() -> Dict[str, Any]:
        """Get latest SpaceX launch"""
        async with aiohttp.ClientSession() as session:
            try:
                url = "https://api.spacexdata.com/v4/launches/latest"
                async with session.get(url) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}

    @staticmethod
    async def nasa_asteroids(api_key: str = "DEMO_KEY") -> Dict[str, Any]:
        """Get near-Earth asteroids from NASA"""
        async with aiohttp.ClientSession() as session:
            try:
                from datetime import date
                today = date.today().isoformat()
                url = f"https://api.nasa.gov/neo/rest/v1/feed?start_date={today}&end_date={today}&api_key={api_key}"
                async with session.get(url) as resp:
                    return await resp.json()
            except Exception as e:
                return {'error': str(e)}


class ServiceAPIManager:
    """Central manager for all service APIs"""

    def __init__(self):
        self.docker = DockerAPIClient()
        self.database = None
        self.mqtt = None
        self.redis = None
        self.prometheus = PrometheusAPIClient()
        self.grafana = GrafanaAPIClient()
        self.external = ExternalAPIClients()

    async def initialize_database(self, config: ServiceAPIConfig):
        """Initialize database client"""
        self.database = TimescaleDBAPIClient(config)
        return await self.database.connect()

    def initialize_mqtt(self, config: ServiceAPIConfig):
        """Initialize MQTT client"""
        self.mqtt = MQTTAPIClient(config)
        return self.mqtt.connect()

    def initialize_redis(self, config: ServiceAPIConfig):
        """Initialize Redis client"""
        self.redis = RedisAPIClient(config)
        return self.redis.connect()

    async def get_system_overview(self) -> Dict[str, Any]:
        """Get complete system overview"""
        overview = {
            'containers': self.docker.list_containers(),
            'timestamp': datetime.now().isoformat()
        }

        if self.database:
            overview['telemetry_summary'] = await self.database.get_telemetry_summary()
            overview['agp_state'] = await self.database.get_agp_state_summary()
            overview['experiments'] = await self.database.get_active_experiments()

        if self.mqtt and self.mqtt.connected:
            overview['mqtt_messages'] = len(self.mqtt.get_recent_messages())

        if self.redis and self.redis.client:
            overview['redis_info'] = self.redis.get_info()

        return overview

    async def cleanup(self):
        """Cleanup all connections"""
        if self.database:
            await self.database.close()
        if self.mqtt:
            self.mqtt.disconnect()


# Global service API manager
_api_manager = None

def get_api_manager() -> ServiceAPIManager:
    """Get the global service API manager"""
    global _api_manager
    if _api_manager is None:
        _api_manager = ServiceAPIManager()
    return _api_manager
