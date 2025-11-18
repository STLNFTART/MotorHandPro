"""
MotorHandPro WebSocket Server
Real-time bi-directional communication server
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import asyncio
import websockets
import json
import logging
from datetime import datetime
from typing import Set, Dict, Any
import os
import paho.mqtt.client as mqtt
import asyncpg
from jose import jwt, JWTError

# Configuration
DATABASE_URL = os.getenv("DATABASE_URL", "postgresql://motorhand:motorhand_secure_password_change_in_production@timescaledb:5432/motorhand")
MQTT_BROKER = os.getenv("MQTT_BROKER", "mqtt://mqtt:1883").replace("mqtt://", "")
JWT_SECRET = os.getenv("JWT_SECRET", "change_this_secret_in_production")
JWT_ALGORITHM = "HS256"
PORT = int(os.getenv("PORT", "8765"))

# Logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Connected clients
clients: Set[websockets.WebSocketServerProtocol] = set()

# Client subscriptions (websocket -> list of topics)
subscriptions: Dict[websockets.WebSocketServerProtocol, Set[str]] = {}

# Database connection pool
db_pool = None

# MQTT Client
mqtt_client = None

# ============================================================================
# Authentication
# ============================================================================

def verify_token(token: str) -> dict:
    """Verify JWT token"""
    try:
        payload = jwt.decode(token, JWT_SECRET, algorithms=[JWT_ALGORITHM])
        return payload
    except JWTError:
        raise ValueError("Invalid token")

# ============================================================================
# MQTT Callbacks
# ============================================================================

def on_mqtt_message(client, userdata, message):
    """Handle incoming MQTT messages and forward to WebSocket clients"""
    topic = message.topic
    payload = message.payload.decode('utf-8')

    # Broadcast to subscribed WebSocket clients
    asyncio.create_task(broadcast_mqtt_message(topic, payload))

def on_mqtt_connect(client, userdata, flags, rc):
    """MQTT connection callback"""
    if rc == 0:
        logger.info("Connected to MQTT broker")
        # Subscribe to all motorhand topics
        client.subscribe("motorhand/#")
    else:
        logger.error(f"Failed to connect to MQTT broker: {rc}")

# ============================================================================
# WebSocket Handlers
# ============================================================================

async def broadcast_mqtt_message(topic: str, payload: str):
    """Broadcast MQTT message to subscribed WebSocket clients"""
    if not clients:
        return

    # Prepare message
    message = {
        "type": "mqtt",
        "topic": topic,
        "payload": payload,
        "timestamp": datetime.utcnow().isoformat()
    }

    # Send to subscribed clients
    dead_clients = set()
    for client in clients:
        if client in subscriptions:
            # Check if client is subscribed to this topic
            for subscription in subscriptions[client]:
                if topic.startswith(subscription):
                    try:
                        await client.send(json.dumps(message))
                    except Exception as e:
                        logger.error(f"Error sending to client: {e}")
                        dead_clients.add(client)
                    break

    # Remove dead clients
    for client in dead_clients:
        clients.discard(client)
        subscriptions.pop(client, None)

async def handle_subscribe(websocket, topics: list):
    """Handle client subscription request"""
    if websocket not in subscriptions:
        subscriptions[websocket] = set()

    for topic in topics:
        subscriptions[websocket].add(topic)
        logger.info(f"Client subscribed to: {topic}")

    return {"status": "ok", "subscribed": list(subscriptions[websocket])}

async def handle_unsubscribe(websocket, topics: list):
    """Handle client unsubscription request"""
    if websocket in subscriptions:
        for topic in topics:
            subscriptions[websocket].discard(topic)
        logger.info(f"Client unsubscribed from: {topics}")

    return {"status": "ok", "subscribed": list(subscriptions.get(websocket, []))}

async def handle_publish(websocket, topic: str, payload: Any):
    """Handle client publish request"""
    # Publish to MQTT
    if mqtt_client:
        mqtt_client.publish(topic, json.dumps(payload))
        logger.info(f"Published to MQTT topic {topic}")

    return {"status": "ok", "topic": topic}

async def handle_query(websocket, query_type: str, params: dict):
    """Handle database query request"""
    try:
        # Get database connection
        async with db_pool.acquire() as conn:
            if query_type == "telemetry":
                spacecraft_id = params.get("spacecraft_id")
                limit = params.get("limit", 100)

                rows = await conn.fetch("""
                    SELECT * FROM telemetry.spacecraft
                    WHERE spacecraft_id = $1
                    ORDER BY time DESC
                    LIMIT $2
                """, spacecraft_id, limit)

                return {
                    "status": "ok",
                    "data": [dict(row) for row in rows]
                }

            elif query_type == "agp_state":
                system_id = params.get("system_id")
                limit = params.get("limit", 100)

                rows = await conn.fetch("""
                    SELECT * FROM telemetry.agp_state
                    WHERE system_id = $1
                    ORDER BY time DESC
                    LIMIT $2
                """, system_id, limit)

                return {
                    "status": "ok",
                    "data": [dict(row) for row in rows]
                }

            else:
                return {"status": "error", "message": "Unknown query type"}

    except Exception as e:
        logger.error(f"Query error: {e}")
        return {"status": "error", "message": str(e)}

async def handle_websocket(websocket, path):
    """Handle WebSocket connection"""
    logger.info(f"New WebSocket connection from {websocket.remote_address}")

    # Authentication
    try:
        # Expect first message to be authentication
        auth_message = await asyncio.wait_for(websocket.recv(), timeout=10.0)
        auth_data = json.loads(auth_message)

        if auth_data.get("type") != "auth":
            await websocket.send(json.dumps({"status": "error", "message": "Authentication required"}))
            return

        token = auth_data.get("token")
        user = verify_token(token)
        logger.info(f"Authenticated user: {user.get('sub')}")

    except Exception as e:
        logger.error(f"Authentication failed: {e}")
        await websocket.send(json.dumps({"status": "error", "message": "Authentication failed"}))
        return

    # Add to clients
    clients.add(websocket)

    try:
        # Send welcome message
        await websocket.send(json.dumps({
            "type": "welcome",
            "message": "Connected to MotorHandPro WebSocket server",
            "timestamp": datetime.utcnow().isoformat()
        }))

        # Handle incoming messages
        async for message in websocket:
            try:
                data = json.loads(message)
                msg_type = data.get("type")

                if msg_type == "subscribe":
                    response = await handle_subscribe(websocket, data.get("topics", []))
                elif msg_type == "unsubscribe":
                    response = await handle_unsubscribe(websocket, data.get("topics", []))
                elif msg_type == "publish":
                    response = await handle_publish(websocket, data.get("topic"), data.get("payload"))
                elif msg_type == "query":
                    response = await handle_query(websocket, data.get("query_type"), data.get("params", {}))
                elif msg_type == "ping":
                    response = {"type": "pong", "timestamp": datetime.utcnow().isoformat()}
                else:
                    response = {"status": "error", "message": f"Unknown message type: {msg_type}"}

                await websocket.send(json.dumps(response))

            except json.JSONDecodeError:
                await websocket.send(json.dumps({"status": "error", "message": "Invalid JSON"}))
            except Exception as e:
                logger.error(f"Error handling message: {e}")
                await websocket.send(json.dumps({"status": "error", "message": str(e)}))

    except websockets.exceptions.ConnectionClosed:
        logger.info(f"WebSocket connection closed: {websocket.remote_address}")
    finally:
        # Remove from clients
        clients.discard(websocket)
        subscriptions.pop(websocket, None)

# ============================================================================
# Health Check Endpoint
# ============================================================================

async def handle_health(websocket, path):
    """Health check endpoint"""
    try:
        await websocket.send("pong")
        response = await websocket.recv()
        if response == "ping":
            await websocket.close()
    except:
        pass

# ============================================================================
# Main
# ============================================================================

async def main():
    """Main entry point"""
    global db_pool, mqtt_client

    # Initialize database connection pool
    db_pool = await asyncpg.create_pool(DATABASE_URL, min_size=5, max_size=20)
    logger.info("Database connection pool created")

    # Initialize MQTT client
    mqtt_client = mqtt.Client()
    mqtt_client.on_connect = on_mqtt_connect
    mqtt_client.on_message = on_mqtt_message

    broker_parts = MQTT_BROKER.split(':')
    mqtt_host = broker_parts[0]
    mqtt_port = int(broker_parts[1]) if len(broker_parts) > 1 else 1883

    mqtt_client.connect(mqtt_host, mqtt_port, 60)
    mqtt_client.loop_start()
    logger.info(f"Connected to MQTT broker at {mqtt_host}:{mqtt_port}")

    # Start WebSocket server
    async with websockets.serve(handle_websocket, "0.0.0.0", PORT):
        logger.info(f"WebSocket server started on port {PORT}")
        await asyncio.Future()  # Run forever

if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        logger.info("WebSocket server stopped")
