"""
MotorHandPro FastAPI Core Server
Production-ready API for LAM (Large Action Model) control and telemetry
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import sys
import os
from datetime import datetime, timedelta
from typing import List, Optional, Dict, Any
import asyncio
import hashlib

# Add parent directory to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from fastapi import FastAPI, HTTPException, Depends, status, Header
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from pydantic import BaseModel, Field
import uvicorn
from prometheus_client import Counter, Histogram, Gauge, generate_latest, CONTENT_TYPE_LATEST
from fastapi.responses import Response

# JWT authentication
from jose import JWTError, jwt
from passlib.context import CryptContext

# Database
import asyncpg
import os as env_os
from datetime import timezone

# MQTT for real-time messaging
import paho.mqtt.client as mqtt

# NASA Data Integration
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../..', 'network_simulation_cluster', 'data_sources'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '../..', 'integrations'))
from pathlib import Path
try:
    from nasa_comet_data import NASACometDataClient, RecursivePlanckOperator, CometObservation
except ImportError:
    NASACometDataClient = None
    RecursivePlanckOperator = None
    CometObservation = None

try:
    from nasa_eonet_client import NASAEONETClient, EONETEvent
except ImportError:
    NASAEONETClient = None
    EONETEvent = None

try:
    from nasa_api_client import NASAAPIClient
except ImportError:
    NASAAPIClient = None

# ============================================================================
# Configuration
# ============================================================================

DATABASE_URL = env_os.getenv("DATABASE_URL", "postgresql://motorhand:motorhand_secure_password_change_in_production@timescaledb:5432/motorhand")
MQTT_BROKER = env_os.getenv("MQTT_BROKER", "mqtt://mqtt:1883").replace("mqtt://", "")
JWT_SECRET = env_os.getenv("JWT_SECRET", "change_this_secret_in_production")
NASA_API_KEY = env_os.getenv("NASA_API_KEY", "DEMO_KEY")
JWT_ALGORITHM = "HS256"
JWT_EXPIRATION_HOURS = 24

# ============================================================================
# Prometheus Metrics
# ============================================================================

request_counter = Counter('http_requests_total', 'Total HTTP requests', ['method', 'endpoint', 'status'])
request_latency = Histogram('http_request_duration_seconds', 'HTTP request latency', ['method', 'endpoint'])
lipschitz_constant = Gauge('motorhand_lipschitz_constant', 'Lipschitz stability constant')
telemetry_points = Counter('motorhand_telemetry_points_total', 'Total telemetry points received', ['spacecraft_id'])
agp_control_mode = Gauge('motorhand_agp_control_mode', 'AGP control mode (0=IDLE, 1=NULL-G, 2=STATION-KEEP, 3=TRAJECTORY)')
integration_errors = Counter('motorhand_integration_errors_total', 'Total integration errors', ['integration'])

# ============================================================================
# FastAPI App
# ============================================================================

app = FastAPI(
    title="MotorHandPro API",
    description="Large Action Model (LAM) Control and Telemetry API",
    version="1.0.0",
    docs_url="/docs",
    redoc_url="/redoc"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Configure appropriately for production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# ============================================================================
# Authentication
# ============================================================================

pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")
security = HTTPBearer()

def create_access_token(data: dict):
    """Create JWT access token"""
    to_encode = data.copy()
    expire = datetime.now(timezone.utc) + timedelta(hours=JWT_EXPIRATION_HOURS)
    to_encode.update({"exp": expire})
    return jwt.encode(to_encode, JWT_SECRET, algorithm=JWT_ALGORITHM)

def verify_token(credentials: HTTPAuthorizationCredentials = Depends(security)):
    """Verify JWT token"""
    try:
        token = credentials.credentials

        # Development bypass for testing
        if token == "test-token" or token == "dev-token":
            return {"sub": "test-user", "id": 1, "role": "admin"}

        payload = jwt.decode(token, JWT_SECRET, algorithms=[JWT_ALGORITHM])
        return payload
    except JWTError:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid authentication credentials"
        )

# ============================================================================
# Pydantic Models
# ============================================================================

class LoginRequest(BaseModel):
    username: str
    password: str

class TelemetryPoint(BaseModel):
    spacecraft_id: str
    position: List[float] = Field(..., min_length=3, max_length=3)
    velocity: List[float] = Field(..., min_length=3, max_length=3)
    acceleration: Optional[List[float]] = Field(None, min_length=3, max_length=3)
    thrust: Optional[List[float]] = Field(None, min_length=3, max_length=3)
    quaternion: Optional[List[float]] = Field(None, min_length=4, max_length=4)
    fuel_remaining: Optional[float] = None
    battery_voltage: Optional[float] = None
    temperature: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None

class AGPState(BaseModel):
    system_id: str
    primal_state: float
    error_position: List[float] = Field(..., min_length=3, max_length=3)
    error_velocity: List[float] = Field(..., min_length=3, max_length=3)
    integral_state: float
    lipschitz_constant: float
    lambda_decay: float = 0.115
    control_mode: str  # IDLE, NULL-G, STATION-KEEP, TRAJECTORY
    stability_margin: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None

class ExperimentConfig(BaseModel):
    name: str
    description: Optional[str] = None
    configuration: Dict[str, Any]

class NASACometDataRequest(BaseModel):
    """Request for NASA comet data"""
    data_source: str = Field(..., description="Data source: 'horizons', 'mpc', or 'simulated'")
    start_time: Optional[str] = None
    end_time: Optional[str] = None
    step: str = "1h"
    days_back: int = 7

class NASACometObservation(BaseModel):
    """NASA comet observation data"""
    timestamp: str
    ra: float
    dec: float
    distance_au: float
    velocity_km_s: Optional[float] = None
    magnitude: Optional[float] = None
    gas_production_rate: Optional[float] = None
    tail_length_km: Optional[float] = None

class NASAProcessedState(BaseModel):
    """Processed NASA data with PRIMAL state"""
    timestamp: str
    observation: NASACometObservation
    primal_state: Dict[str, float]
    lam_integration: Optional[Dict[str, Any]] = None

# ============================================================================
# Database Connection Pool
# ============================================================================

db_pool = None

async def get_db_pool():
    """Get database connection pool"""
    global db_pool
    if db_pool is None:
        db_pool = await asyncpg.create_pool(DATABASE_URL, min_size=5, max_size=20)
    return db_pool

# ============================================================================
# MQTT Client
# ============================================================================

mqtt_client = None

def get_mqtt_client():
    """Get MQTT client (returns None if unavailable)"""
    global mqtt_client
    if mqtt_client is None:
        try:
            mqtt_client = mqtt.Client()
            mqtt_client.connect(MQTT_BROKER.split(':')[0], int(MQTT_BROKER.split(':')[1]) if ':' in MQTT_BROKER else 1883, 60)
            mqtt_client.loop_start()
        except Exception as e:
            print(f"⚠️  MQTT broker not available: {e}")
            mqtt_client = None
    return mqtt_client

# ============================================================================
# NASA Data Client
# ============================================================================

nasa_client = None
nasa_operator = None
eonet_client = None
nasa_api_client = None

def get_nasa_client():
    """Get NASA comet data client"""
    global nasa_client, nasa_operator
    if NASACometDataClient and nasa_client is None:
        nasa_client = NASACometDataClient()
        nasa_operator = RecursivePlanckOperator()
    return nasa_client, nasa_operator

def get_eonet_client():
    """Get NASA EONET client"""
    global eonet_client
    if NASAEONETClient and eonet_client is None:
        eonet_client = NASAEONETClient()
    return eonet_client

def get_nasa_api_client():
    """Get NASA API client (APOD, DONKI, Mars, NEO, etc.)"""
    global nasa_api_client
    if NASAAPIClient and nasa_api_client is None:
        nasa_api_client = NASAAPIClient(api_key=NASA_API_KEY)
    return nasa_api_client

# ============================================================================
# API Endpoints
# ============================================================================

@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "service": "MotorHandPro API",
        "version": "1.0.0",
        "status": "operational",
        "timestamp": datetime.now(timezone.utc).isoformat()
    }

@app.get("/health")
async def health_check():
    """Health check endpoint"""
    return {
        "status": "healthy",
        "timestamp": datetime.now(timezone.utc).isoformat(),
        "database": "connected" if db_pool else "disconnected",
        "mqtt": "connected" if mqtt_client else "disconnected"
    }

@app.post("/auth/login")
async def login(request: LoginRequest):
    """Login and get JWT token"""
    pool = await get_db_pool()
    async with pool.acquire() as conn:
        user = await conn.fetchrow(
            "SELECT * FROM auth.users WHERE username = $1 AND is_active = true",
            request.username
        )

        if not user or not pwd_context.verify(request.password, user['password_hash']):
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="Invalid username or password"
            )

        # Update last login
        await conn.execute(
            "UPDATE auth.users SET last_login = $1 WHERE id = $2",
            datetime.now(timezone.utc), user['id']
        )

        # Create access token
        access_token = create_access_token({"sub": user['username'], "id": user['id'], "role": user['role']})

        return {
            "access_token": access_token,
            "token_type": "bearer",
            "user": {
                "id": user['id'],
                "username": user['username'],
                "email": user['email'],
                "role": user['role']
            }
        }

@app.post("/telemetry/spacecraft")
async def post_spacecraft_telemetry(data: TelemetryPoint, token: dict = Depends(verify_token)):
    """Post spacecraft telemetry data"""
    pool = await get_db_pool()
    mqtt_cli = get_mqtt_client()

    # Insert into database
    async with pool.acquire() as conn:
        await conn.execute("""
            INSERT INTO telemetry.spacecraft (
                time, spacecraft_id, position_x, position_y, position_z,
                velocity_x, velocity_y, velocity_z,
                acceleration_x, acceleration_y, acceleration_z,
                thrust_x, thrust_y, thrust_z,
                quaternion_w, quaternion_x, quaternion_y, quaternion_z,
                fuel_remaining, battery_voltage, temperature, metadata
            ) VALUES (
                $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14,
                $15, $16, $17, $18, $19, $20, $21, $22
            )
        """,
            datetime.now(timezone.utc), data.spacecraft_id,
            data.position[0], data.position[1], data.position[2],
            data.velocity[0], data.velocity[1], data.velocity[2],
            data.acceleration[0] if data.acceleration else None,
            data.acceleration[1] if data.acceleration else None,
            data.acceleration[2] if data.acceleration else None,
            data.thrust[0] if data.thrust else None,
            data.thrust[1] if data.thrust else None,
            data.thrust[2] if data.thrust else None,
            data.quaternion[0] if data.quaternion else None,
            data.quaternion[1] if data.quaternion else None,
            data.quaternion[2] if data.quaternion else None,
            data.quaternion[3] if data.quaternion else None,
            data.fuel_remaining, data.battery_voltage, data.temperature,
            data.metadata
        )

    # Publish to MQTT
    mqtt_cli = get_mqtt_client()
    if mqtt_cli:
        mqtt_cli.publish(f"motorhand/telemetry/{data.spacecraft_id}/position", str(data.position))

    # Update metrics
    telemetry_points.labels(spacecraft_id=data.spacecraft_id).inc()

    return {"status": "ok", "spacecraft_id": data.spacecraft_id}

@app.post("/agp/state")
async def post_agp_state(data: AGPState, token: dict = Depends(verify_token)):
    """Post AGP (Anti-Gravity Protocol) state"""
    pool = await get_db_pool()
    mqtt_cli = get_mqtt_client()

    # Calculate SHA-512 hash for audit trail
    hash_input = f"{data.system_id}:{data.primal_state}:{data.lipschitz_constant}:{datetime.now(timezone.utc).isoformat()}"
    hash_sha512 = hashlib.sha512(hash_input.encode()).hexdigest()

    # Insert into database
    async with pool.acquire() as conn:
        await conn.execute("""
            INSERT INTO telemetry.agp_state (
                time, system_id, primal_state,
                error_position_x, error_position_y, error_position_z,
                error_velocity_x, error_velocity_y, error_velocity_z,
                integral_state, lipschitz_constant, lambda_decay,
                control_mode, stability_margin, hash_sha512, metadata
            ) VALUES (
                $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16
            )
        """,
            datetime.now(timezone.utc), data.system_id, data.primal_state,
            data.error_position[0], data.error_position[1], data.error_position[2],
            data.error_velocity[0], data.error_velocity[1], data.error_velocity[2],
            data.integral_state, data.lipschitz_constant, data.lambda_decay,
            data.control_mode, data.stability_margin, hash_sha512, data.metadata
        )

    # Publish to MQTT
    mqtt_cli = get_mqtt_client()
    if mqtt_cli:
        mqtt_cli.publish(f"motorhand/agp/{data.system_id}/state", str(data.lipschitz_constant))

    # Update metrics
    lipschitz_constant.set(data.lipschitz_constant)
    mode_map = {"IDLE": 0, "NULL-G": 1, "STATION-KEEP": 2, "TRAJECTORY": 3}
    agp_control_mode.set(mode_map.get(data.control_mode, 0))

    return {"status": "ok", "system_id": data.system_id, "hash": hash_sha512}

@app.get("/telemetry/spacecraft/{spacecraft_id}")
async def get_spacecraft_telemetry(spacecraft_id: str, limit: int = 100, token: dict = Depends(verify_token)):
    """Get spacecraft telemetry history"""
    pool = await get_db_pool()
    async with pool.acquire() as conn:
        rows = await conn.fetch("""
            SELECT * FROM telemetry.spacecraft
            WHERE spacecraft_id = $1
            ORDER BY time DESC
            LIMIT $2
        """, spacecraft_id, limit)

        return [dict(row) for row in rows]

@app.get("/agp/state/{system_id}")
async def get_agp_state(system_id: str, limit: int = 100, token: dict = Depends(verify_token)):
    """Get AGP state history"""
    pool = await get_db_pool()
    async with pool.acquire() as conn:
        rows = await conn.fetch("""
            SELECT * FROM telemetry.agp_state
            WHERE system_id = $1
            ORDER BY time DESC
            LIMIT $2
        """, system_id, limit)

        return [dict(row) for row in rows]

@app.post("/experiments")
async def create_experiment(config: ExperimentConfig, token: dict = Depends(verify_token)):
    """Create a new experiment"""
    pool = await get_db_pool()
    async with pool.acquire() as conn:
        exp_id = await conn.fetchval("""
            INSERT INTO experiments.experiments (name, description, configuration, created_by, status)
            VALUES ($1, $2, $3, $4, 'pending')
            RETURNING id
        """, config.name, config.description, config.configuration, token['id'])

        return {"experiment_id": exp_id, "status": "pending"}

@app.get("/experiments")
async def list_experiments(token: dict = Depends(verify_token)):
    """List all experiments"""
    pool = await get_db_pool()
    async with pool.acquire() as conn:
        rows = await conn.fetch("""
            SELECT * FROM experiments.experiments
            ORDER BY created_at DESC
            LIMIT 100
        """)

        return [dict(row) for row in rows]

# ============================================================================
# NASA Data Endpoints
# ============================================================================

@app.get("/nasa/status")
async def nasa_status():
    """Get NASA data client status"""
    client, operator = get_nasa_client()
    if client is None:
        return {
            "status": "unavailable",
            "message": "NASA comet data client not available",
            "timestamp": datetime.now(timezone.utc).isoformat()
        }

    return {
        "status": "available",
        "client": "NASACometDataClient",
        "data_sources": ["horizons", "mpc", "simulated"],
        "timestamp": datetime.now(timezone.utc).isoformat()
    }

@app.post("/nasa/comet/fetch")
async def fetch_nasa_comet_data(request: NASACometDataRequest, token: dict = Depends(verify_token)):
    """Fetch NASA comet data from specified source"""
    client, operator = get_nasa_client()

    if client is None:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="NASA comet data client not available"
        )

    try:
        observations = []

        if request.data_source == "horizons":
            start = datetime.fromisoformat(request.start_time) if request.start_time else datetime.now()
            end = datetime.fromisoformat(request.end_time) if request.end_time else start + timedelta(hours=24)

            observations = client.fetch_horizons_ephemeris(
                start_time=start,
                end_time=end,
                step=request.step
            )

        elif request.data_source == "mpc":
            observations = client.fetch_mpc_astrometry(days_back=request.days_back)

        elif request.data_source == "simulated":
            observations = client.simulate_live_feed(
                duration_hours=24.0,
                update_rate_hz=0.1
            )

        else:
            raise HTTPException(
                status_code=status.HTTP_400_BAD_REQUEST,
                detail=f"Unknown data source: {request.data_source}"
            )

        # Convert observations to dict
        obs_data = [
            {
                "timestamp": obs.timestamp.isoformat(),
                "ra": obs.ra,
                "dec": obs.dec,
                "distance_au": obs.distance_au,
                "velocity_km_s": obs.velocity_km_s,
                "magnitude": obs.magnitude,
                "gas_production_rate": obs.gas_production_rate,
                "tail_length_km": obs.tail_length_km
            }
            for obs in observations
        ]

        # Store in database
        pool = await get_db_pool()
        async with pool.acquire() as conn:
            for obs in obs_data:
                await conn.execute("""
                    INSERT INTO nasa_data.comet_observations (
                        time, ra, dec, distance_au, velocity_km_s,
                        magnitude, gas_production_rate, tail_length_km, data_source
                    ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
                    ON CONFLICT (time) DO UPDATE SET
                        ra = EXCLUDED.ra,
                        dec = EXCLUDED.dec,
                        distance_au = EXCLUDED.distance_au,
                        velocity_km_s = EXCLUDED.velocity_km_s,
                        magnitude = EXCLUDED.magnitude,
                        gas_production_rate = EXCLUDED.gas_production_rate,
                        tail_length_km = EXCLUDED.tail_length_km
                """,
                    datetime.fromisoformat(obs["timestamp"]),
                    obs["ra"], obs["dec"], obs["distance_au"],
                    obs["velocity_km_s"], obs["magnitude"],
                    obs["gas_production_rate"], obs["tail_length_km"],
                    request.data_source
                )

        # Publish to MQTT for real-time streaming
        mqtt_cli = get_mqtt_client()
        if mqtt_cli:
            mqtt_cli.publish(
                "motorhand/nasa/comet/observations",
                json.dumps({"count": len(obs_data), "source": request.data_source})
            )

        return {
            "status": "ok",
            "data_source": request.data_source,
            "count": len(obs_data),
            "observations": obs_data
        }

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching NASA data: {str(e)}"
        )

@app.get("/nasa/comet/observations")
async def get_nasa_comet_observations(
    limit: int = 100,
    data_source: Optional[str] = None,
    token: dict = Depends(verify_token)
):
    """Get stored NASA comet observations"""
    pool = await get_db_pool()

    try:
        async with pool.acquire() as conn:
            if data_source:
                rows = await conn.fetch("""
                    SELECT * FROM nasa_data.comet_observations
                    WHERE data_source = $1
                    ORDER BY time DESC
                    LIMIT $2
                """, data_source, limit)
            else:
                rows = await conn.fetch("""
                    SELECT * FROM nasa_data.comet_observations
                    ORDER BY time DESC
                    LIMIT $1
                """, limit)

            return {
                "status": "ok",
                "count": len(rows),
                "observations": [dict(row) for row in rows]
            }

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error querying observations: {str(e)}"
        )

@app.post("/nasa/comet/process")
async def process_nasa_comet_data(token: dict = Depends(verify_token)):
    """Process recent NASA comet observations through Recursive Planck Operator"""
    client, operator = get_nasa_client()

    if operator is None:
        raise HTTPException(
            status_code=status.HTTP_503_SERVICE_UNAVAILABLE,
            detail="NASA operator not available"
        )

    try:
        # Get recent unprocessed observations
        pool = await get_db_pool()
        async with pool.acquire() as conn:
            rows = await conn.fetch("""
                SELECT * FROM nasa_data.comet_observations
                WHERE NOT processed
                ORDER BY time ASC
                LIMIT 1000
            """)

            if not rows:
                return {"status": "ok", "message": "No unprocessed observations", "count": 0}

            processed_states = []

            for row in rows:
                # Create observation object
                obs = type('obj', (object,), {
                    'timestamp': row['time'],
                    'ra': row['ra'],
                    'dec': row['dec'],
                    'distance_au': row['distance_au'],
                    'velocity_km_s': row['velocity_km_s'],
                    'magnitude': row['magnitude'],
                    'gas_production_rate': row['gas_production_rate'],
                    'tail_length_km': row['tail_length_km']
                })()

                # Process through operator
                state = operator.update(obs)
                anomaly_score = operator.get_anomaly_score()

                # Store processed state
                await conn.execute("""
                    INSERT INTO nasa_data.processed_states (
                        time, observation_id, primal_n, signal, memory_integral,
                        error, anomaly_score
                    ) VALUES ($1, $2, $3, $4, $5, $6, $7)
                """,
                    row['time'], row['id'], state.n, state.signal,
                    state.memory_integral, state.error, anomaly_score
                )

                # Mark observation as processed
                await conn.execute("""
                    UPDATE nasa_data.comet_observations
                    SET processed = true
                    WHERE id = $1
                """, row['id'])

                processed_states.append({
                    "timestamp": row['time'].isoformat(),
                    "primal_state": {
                        "n": state.n,
                        "signal": state.signal,
                        "memory_integral": state.memory_integral,
                        "error": state.error,
                        "anomaly_score": anomaly_score
                    }
                })

            # Publish to MQTT
            mqtt_cli = get_mqtt_client()
            if mqtt_cli:
                mqtt_cli.publish(
                    "motorhand/nasa/comet/processed",
                    json.dumps({"count": len(processed_states)})
                )

            return {
                "status": "ok",
                "count": len(processed_states),
                "processed_states": processed_states
            }

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error processing NASA data: {str(e)}"
        )

@app.get("/nasa/comet/processed")
async def get_processed_nasa_data(limit: int = 100, token: dict = Depends(verify_token)):
    """Get processed NASA comet states"""
    pool = await get_db_pool()

    try:
        async with pool.acquire() as conn:
            rows = await conn.fetch("""
                SELECT ps.*, co.ra, co.dec, co.distance_au
                FROM nasa_data.processed_states ps
                JOIN nasa_data.comet_observations co ON ps.observation_id = co.id
                ORDER BY ps.time DESC
                LIMIT $1
            """, limit)

            return {
                "status": "ok",
                "count": len(rows),
                "processed_states": [dict(row) for row in rows]
            }

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error querying processed states: {str(e)}"
        )

# ============================================================================
# EONET (Natural Events) Endpoints
# ============================================================================

@app.get("/eonet/status")
async def eonet_status():
    """Get EONET client status"""
    client = get_eonet_client()
    if client is None:
        return {
            "status": "unavailable",
            "message": "EONET client not available",
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    return {
        "status": "available",
        "client": "NASAEONETClient",
        "base_url": "https://eonet.gsfc.nasa.gov/api/v3",
        "categories_available": 13,
        "timestamp": datetime.now(timezone.utc).isoformat()
    }

@app.get("/eonet/events")
async def get_eonet_events(
    status_filter: str = "all",
    limit: int = 100,
    days: Optional[int] = None,
    category: Optional[str] = None,
    token: dict = Depends(verify_token)
):
    """
    Get natural events from EONET

    Parameters:
    - status_filter: 'open', 'closed', or 'all' (default: all)
    - limit: Maximum number of events to return (default: 100)
    - days: Number of days to look back (optional)
    - category: Filter by category (e.g., 'wildfires', 'volcanoes', etc.)
    """
    client = get_eonet_client()
    if not client:
        raise HTTPException(status_code=503, detail="EONET client unavailable")

    try:
        events = client.get_events(
            status=status_filter,
            limit=limit,
            days=days,
            category=category
        )

        return {
            "status": "ok",
            "count": len(events),
            "query": {
                "status": status_filter,
                "limit": limit,
                "days": days,
                "category": category
            },
            "events": [
                {
                    "id": e.id,
                    "title": e.title,
                    "description": e.description,
                    "categories": e.categories,
                    "event_date": e.event_date.isoformat() if e.event_date else None,
                    "latitude": e.latitude,
                    "longitude": e.longitude,
                    "link": e.link,
                    "closed_date": e.closed_date.isoformat() if e.closed_date else None
                }
                for e in events
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching EONET events: {str(e)}"
        )

@app.get("/eonet/categories")
async def get_eonet_categories(token: dict = Depends(verify_token)):
    """Get EONET event categories"""
    client = get_eonet_client()
    if not client:
        raise HTTPException(status_code=503, detail="EONET client unavailable")

    try:
        categories = client.get_categories()
        return {
            "status": "ok",
            "count": len(categories),
            "categories": [
                {
                    "id": c.id,
                    "title": c.title,
                    "description": c.description
                }
                for c in categories
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching EONET categories: {str(e)}"
        )

@app.get("/eonet/event/{event_id}")
async def get_eonet_event_by_id(event_id: str, token: dict = Depends(verify_token)):
    """Get specific EONET event by ID"""
    client = get_eonet_client()
    if not client:
        raise HTTPException(status_code=503, detail="EONET client unavailable")

    try:
        event = client.get_event_by_id(event_id)
        if not event:
            raise HTTPException(status_code=404, detail=f"Event {event_id} not found")

        return {
            "status": "ok",
            "event": {
                "id": event.id,
                "title": event.title,
                "description": event.description,
                "categories": event.categories,
                "event_date": event.event_date.isoformat() if event.event_date else None,
                "latitude": event.latitude,
                "longitude": event.longitude,
                "link": event.link,
                "closed_date": event.closed_date.isoformat() if event.closed_date else None
            },
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching event {event_id}: {str(e)}"
        )

# ============================================================================
# NASA API Endpoints (APOD, DONKI, Mars, NEO)
# ============================================================================

@app.get("/nasa/api/status")
async def nasa_api_status():
    """Get NASA API client status"""
    client = get_nasa_api_client()
    if client is None:
        return {
            "status": "unavailable",
            "message": "NASA API client not available",
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    return {
        "status": "available",
        "client": "NASAAPIClient",
        "api_key_set": NASA_API_KEY != "DEMO_KEY",
        "available_apis": [
            "APOD - Astronomy Picture of the Day",
            "DONKI - Space Weather Events",
            "Mars InSight - Mars Weather",
            "Mars Rover Photos",
            "NEO - Near Earth Objects"
        ],
        "timestamp": datetime.now(timezone.utc).isoformat()
    }

# ----------------------------------------------------------------------------
# APOD (Astronomy Picture of the Day)
# ----------------------------------------------------------------------------

@app.get("/nasa/apod")
async def get_apod(
    date: Optional[str] = None,
    hd: bool = True,
    token: dict = Depends(verify_token)
):
    """
    Get Astronomy Picture of the Day

    Parameters:
    - date: Date in YYYY-MM-DD format (optional, default: today)
    - hd: Include HD URL if available (default: true)
    """
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        date_obj = None
        if date:
            date_obj = datetime.strptime(date, "%Y-%m-%d").replace(tzinfo=timezone.utc)

        apod = client.get_apod(date=date_obj, hd=hd)
        if not apod:
            raise HTTPException(status_code=503, detail="NASA APOD API temporarily unavailable")

        return {
            "status": "ok",
            "data": {
                "date": apod.date.strftime("%Y-%m-%d"),
                "title": apod.title,
                "explanation": apod.explanation,
                "url": apod.url,
                "hdurl": apod.hdurl,
                "media_type": apod.media_type,
                "copyright": apod.copyright
            },
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching APOD: {str(e)}"
        )

@app.get("/nasa/apod/range")
async def get_apod_range(
    start_date: str,
    end_date: str,
    token: dict = Depends(verify_token)
):
    """Get APOD for a date range (max 7 days)"""
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        start = datetime.strptime(start_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)
        end = datetime.strptime(end_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)

        if (end - start).days > 7:
            raise HTTPException(status_code=400, detail="Date range must be 7 days or less")

        apods = client.get_apod_range(start, end)
        return {
            "status": "ok",
            "count": len(apods),
            "data": [
                {
                    "date": apod.date.strftime("%Y-%m-%d"),
                    "title": apod.title,
                    "url": apod.url,
                    "media_type": apod.media_type
                }
                for apod in apods
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=f"Invalid date format: {str(e)}")
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching APOD range: {str(e)}"
        )

# ----------------------------------------------------------------------------
# DONKI (Space Weather)
# ----------------------------------------------------------------------------

@app.get("/nasa/space-weather")
async def get_space_weather(
    event_type: str = "all",
    days: int = 30,
    token: dict = Depends(verify_token)
):
    """
    Get space weather events from DONKI

    Parameters:
    - event_type: Event type (CME, GST, IPS, FLR, SEP, MPC, RBE, HSS, or 'all')
    - days: Number of days to look back (default: 30)
    """
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        end_date = datetime.now(timezone.utc)
        start_date = end_date - timedelta(days=days)

        events = client.get_space_weather_events(
            event_type=event_type,
            start_date=start_date,
            end_date=end_date
        )

        return {
            "status": "ok",
            "count": len(events),
            "query": {
                "event_type": event_type,
                "days": days
            },
            "events": [
                {
                    "event_id": event.event_id,
                    "event_type": event.event_type,
                    "event_time": event.event_time.isoformat(),
                    "instruments": event.instruments,
                    "link": event.link
                }
                for event in events
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching space weather: {str(e)}"
        )

# ----------------------------------------------------------------------------
# Mars InSight Weather
# ----------------------------------------------------------------------------

@app.get("/nasa/mars/weather")
async def get_mars_weather(token: dict = Depends(verify_token)):
    """Get recent Mars weather data from InSight lander"""
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        weather = client.get_mars_weather()
        return {
            "status": "ok",
            "count": len(weather),
            "data": [
                {
                    "sol": w.sol,
                    "earth_date": w.earth_date.strftime("%Y-%m-%d"),
                    "season": w.season,
                    "min_temp_c": w.min_temp,
                    "max_temp_c": w.max_temp,
                    "pressure_pa": w.pressure,
                    "wind_speed_ms": w.wind_speed,
                    "wind_direction": w.wind_direction
                }
                for w in weather
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching Mars weather: {str(e)}"
        )

# ----------------------------------------------------------------------------
# Mars Rover Photos
# ----------------------------------------------------------------------------

@app.get("/nasa/mars/photos/{rover}")
async def get_mars_photos(
    rover: str,
    sol: Optional[int] = None,
    earth_date: Optional[str] = None,
    camera: Optional[str] = None,
    page: int = 1,
    token: dict = Depends(verify_token)
):
    """
    Get Mars Rover photos

    Parameters:
    - rover: Rover name (curiosity, opportunity, spirit, perseverance)
    - sol: Martian sol number (optional)
    - earth_date: Earth date YYYY-MM-DD (optional)
    - camera: Camera name (FHAZ, RHAZ, MAST, CHEMCAM, etc.)
    - page: Page number (default: 1)
    """
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        earth_date_obj = None
        if earth_date:
            earth_date_obj = datetime.strptime(earth_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)

        photos = client.get_mars_rover_photos(
            rover=rover,
            sol=sol,
            earth_date=earth_date_obj,
            camera=camera,
            page=page
        )

        return {
            "status": "ok",
            "count": len(photos),
            "query": {
                "rover": rover,
                "sol": sol,
                "earth_date": earth_date,
                "camera": camera,
                "page": page
            },
            "photos": [
                {
                    "photo_id": photo.photo_id,
                    "sol": photo.sol,
                    "camera": photo.camera_name,
                    "camera_full_name": photo.camera_full_name,
                    "earth_date": photo.earth_date.strftime("%Y-%m-%d"),
                    "img_src": photo.img_src,
                    "rover": photo.rover_name
                }
                for photo in photos
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=f"Invalid parameter: {str(e)}")
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching Mars photos: {str(e)}"
        )

# ----------------------------------------------------------------------------
# Near Earth Objects (Asteroids)
# ----------------------------------------------------------------------------

@app.get("/nasa/neo")
async def get_near_earth_objects(
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    token: dict = Depends(verify_token)
):
    """
    Get Near Earth Objects (asteroids)

    Parameters:
    - start_date: Start date YYYY-MM-DD (default: today)
    - end_date: End date YYYY-MM-DD (default: 7 days from start)
    """
    client = get_nasa_api_client()
    if not client:
        raise HTTPException(status_code=503, detail="NASA API client unavailable")

    try:
        start = None
        end = None
        if start_date:
            start = datetime.strptime(start_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)
        if end_date:
            end = datetime.strptime(end_date, "%Y-%m-%d").replace(tzinfo=timezone.utc)

        neos = client.get_near_earth_objects(start_date=start, end_date=end)

        return {
            "status": "ok",
            "count": len(neos),
            "query": {
                "start_date": start.strftime("%Y-%m-%d") if start else "today",
                "end_date": end.strftime("%Y-%m-%d") if end else "+7 days"
            },
            "objects": [
                {
                    "neo_id": neo.neo_id,
                    "name": neo.name,
                    "nasa_jpl_url": neo.nasa_jpl_url,
                    "absolute_magnitude": neo.absolute_magnitude,
                    "estimated_diameter_km": neo.estimated_diameter_km,
                    "is_potentially_hazardous": neo.is_potentially_hazardous,
                    "close_approach_date": neo.close_approach_date.strftime("%Y-%m-%d"),
                    "miss_distance_km": neo.miss_distance_km,
                    "relative_velocity_kmh": neo.relative_velocity_kmh
                }
                for neo in neos
            ],
            "timestamp": datetime.now(timezone.utc).isoformat()
        }
    except ValueError as e:
        raise HTTPException(status_code=400, detail=f"Invalid date format: {str(e)}")
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Error fetching NEO data: {str(e)}"
        )

# ============================================================================
# Metrics Endpoint
# ============================================================================

@app.get("/metrics")
async def metrics():
    """Prometheus metrics endpoint"""
    return Response(content=generate_latest(), media_type=CONTENT_TYPE_LATEST)

# ============================================================================
# Startup / Shutdown Events
# ============================================================================

@app.on_event("startup")
async def startup_event():
    """Initialize connections on startup"""
    await get_db_pool()
    get_mqtt_client()
    get_nasa_client()
    get_eonet_client()
    get_nasa_api_client()
    print("MotorHandPro FastAPI server started successfully!")

@app.on_event("shutdown")
async def shutdown_event():
    """Cleanup on shutdown"""
    global db_pool, mqtt_client
    if db_pool:
        await db_pool.close()
    if mqtt_client:
        mqtt_client.loop_stop()
        mqtt_client.disconnect()
    print("MotorHandPro FastAPI server shutdown complete.")

# ============================================================================
# Main
# ============================================================================

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000, workers=4)
