/**
 * MotorHandPro Node.js Integration Gateway
 * Production-ready API for external integrations
 * Patent Pending: U.S. Provisional Patent Application No. 63/842,846
 */

const express = require('express');
const cors = require('cors');
const helmet = require('helmet');
const compression = require('compression');
const morgan = require('morgan');
const { Pool } = require('pg');
const mqtt = require('mqtt');
const jwt = require('jsonwebtoken');
const bcrypt = require('bcryptjs');
const axios = require('axios');
const promClient = require('prom-client');
require('dotenv').config();

// ============================================================================
// Configuration
// ============================================================================

const PORT = process.env.PORT || 3000;
const DATABASE_URL = process.env.DATABASE_URL || 'postgresql://motorhand:motorhand_secure_password_change_in_production@timescaledb:5432/motorhand';
const MQTT_BROKER = process.env.MQTT_BROKER || 'mqtt://mqtt:1883';
const FASTAPI_URL = process.env.FASTAPI_URL || 'http://fastapi:8000';
const JWT_SECRET = process.env.JWT_SECRET || 'change_this_secret_in_production';

// ============================================================================
// Express App Setup
// ============================================================================

const app = express();

// Middleware
app.use(helmet());
app.use(compression());
app.use(cors());
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true }));
app.use(morgan('combined'));

// ============================================================================
// Prometheus Metrics
// ============================================================================

const register = new promClient.Register();
promClient.collectDefaultMetrics({ register });

const httpRequestsTotal = new promClient.Counter({
  name: 'http_requests_total',
  help: 'Total HTTP requests',
  labelNames: ['method', 'path', 'status'],
  registers: [register]
});

const httpRequestDuration = new promClient.Histogram({
  name: 'http_request_duration_seconds',
  help: 'HTTP request duration in seconds',
  labelNames: ['method', 'path'],
  registers: [register]
});

// Middleware to track metrics
app.use((req, res, next) => {
  const start = Date.now();
  res.on('finish', () => {
    const duration = (Date.now() - start) / 1000;
    httpRequestsTotal.labels(req.method, req.path, res.statusCode).inc();
    httpRequestDuration.labels(req.method, req.path).observe(duration);
  });
  next();
});

// ============================================================================
// Database Connection
// ============================================================================

const db = new Pool({
  connectionString: DATABASE_URL,
  max: 20,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

// ============================================================================
// MQTT Client
// ============================================================================

const mqttClient = mqtt.connect(MQTT_BROKER);

mqttClient.on('connect', () => {
  console.log('Connected to MQTT broker');
  mqttClient.subscribe('motorhand/#');
});

mqttClient.on('message', (topic, message) => {
  console.log(`MQTT message on ${topic}: ${message.toString()}`);
});

// ============================================================================
// Authentication Middleware
// ============================================================================

function authenticateToken(req, res, next) {
  const authHeader = req.headers['authorization'];
  const token = authHeader && authHeader.split(' ')[1];

  if (!token) {
    return res.status(401).json({ error: 'No token provided' });
  }

  jwt.verify(token, JWT_SECRET, (err, user) => {
    if (err) {
      return res.status(403).json({ error: 'Invalid token' });
    }
    req.user = user;
    next();
  });
}

// ============================================================================
// API Routes
// ============================================================================

// Root
app.get('/', (req, res) => {
  res.json({
    service: 'MotorHandPro Integration Gateway',
    version: '1.0.0',
    status: 'operational',
    timestamp: new Date().toISOString()
  });
});

// Health check
app.get('/health', async (req, res) => {
  try {
    await db.query('SELECT 1');
    res.json({
      status: 'healthy',
      timestamp: new Date().toISOString(),
      database: 'connected',
      mqtt: mqttClient.connected ? 'connected' : 'disconnected'
    });
  } catch (error) {
    res.status(503).json({
      status: 'unhealthy',
      error: error.message
    });
  }
});

// Metrics endpoint
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', register.contentType);
  res.end(await register.metrics());
});

// ============================================================================
// Integration Endpoints
// ============================================================================

// SpaceX Integration
app.get('/integrations/spacex/launches', authenticateToken, async (req, res) => {
  try {
    const response = await axios.get('https://api.spacexdata.com/v4/launches/latest');

    // Log to database
    await db.query(`
      INSERT INTO integrations.events (time, endpoint_id, event_type, payload, status)
      SELECT NOW(), id, 'spacex_launch_query', $1, 'success'
      FROM integrations.endpoints WHERE type = 'spacex' LIMIT 1
    `, [response.data]);

    res.json(response.data);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Tesla Integration
app.post('/integrations/tesla/autopilot', authenticateToken, async (req, res) => {
  try {
    const { position, velocity, steering_angle } = req.body;

    // Publish to MQTT for LAM processing
    mqttClient.publish('motorhand/integrations/tesla/autopilot', JSON.stringify({
      position,
      velocity,
      steering_angle,
      timestamp: new Date().toISOString()
    }));

    // Log to database
    await db.query(`
      INSERT INTO integrations.events (time, endpoint_id, event_type, payload, status)
      SELECT NOW(), id, 'tesla_autopilot_data', $1, 'success'
      FROM integrations.endpoints WHERE type = 'tesla' LIMIT 1
    `, [req.body]);

    res.json({ status: 'ok', message: 'Data published to LAM system' });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// PX4 Flight Controller Integration
app.post('/integrations/px4/telemetry', authenticateToken, async (req, res) => {
  try {
    const telemetry = req.body;

    // Forward to FastAPI for LAM processing
    await axios.post(`${FASTAPI_URL}/telemetry/spacecraft`, {
      spacecraft_id: telemetry.vehicle_id || 'px4-drone-1',
      position: telemetry.position,
      velocity: telemetry.velocity,
      acceleration: telemetry.acceleration,
      quaternion: telemetry.attitude
    }, {
      headers: {
        'Authorization': req.headers['authorization']
      }
    });

    res.json({ status: 'ok', message: 'Telemetry forwarded to LAM' });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// CARLA Simulator Integration
app.post('/integrations/carla/simulation', authenticateToken, async (req, res) => {
  try {
    const simData = req.body;

    // Publish to MQTT
    mqttClient.publish('motorhand/integrations/carla/simulation', JSON.stringify(simData));

    // Log to database
    await db.query(`
      INSERT INTO integrations.events (time, endpoint_id, event_type, payload, status)
      SELECT NOW(), id, 'carla_simulation_data', $1, 'success'
      FROM integrations.endpoints WHERE type = 'carla' LIMIT 1
    `, [simData]);

    res.json({ status: 'ok', message: 'Simulation data processed' });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Starlink Integration
app.get('/integrations/starlink/status', authenticateToken, async (req, res) => {
  try {
    // Simulated Starlink status (replace with actual API when available)
    const status = {
      satellites_visible: Math.floor(Math.random() * 10) + 5,
      signal_strength: Math.random() * 100,
      latency_ms: Math.random() * 30 + 20,
      connected: true,
      timestamp: new Date().toISOString()
    };

    // Publish to MQTT
    mqttClient.publish('motorhand/integrations/starlink/status', JSON.stringify(status));

    res.json(status);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// NASA Data Integration
app.get('/integrations/nasa/asteroids', authenticateToken, async (req, res) => {
  try {
    // NASA NeoWs (Near Earth Object Web Service) API
    const nasa_api_key = process.env.NASA_API_KEY || 'DEMO_KEY';
    const response = await axios.get(`https://api.nasa.gov/neo/rest/v1/feed`, {
      params: {
        api_key: nasa_api_key,
        start_date: new Date().toISOString().split('T')[0],
        end_date: new Date().toISOString().split('T')[0]
      }
    });

    // Log to database
    await db.query(`
      INSERT INTO integrations.events (time, endpoint_id, event_type, payload, status)
      SELECT NOW(), id, 'nasa_asteroid_query', $1, 'success'
      FROM integrations.endpoints WHERE type = 'nasa' LIMIT 1
    `, [{ near_earth_objects: response.data.near_earth_objects }]);

    res.json(response.data);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// ============================================================================
// Proxy to FastAPI
// ============================================================================

app.all('/api/*', authenticateToken, async (req, res) => {
  try {
    const apiPath = req.path.replace('/api', '');
    const response = await axios({
      method: req.method,
      url: `${FASTAPI_URL}${apiPath}`,
      data: req.body,
      headers: {
        'Authorization': req.headers['authorization']
      }
    });
    res.json(response.data);
  } catch (error) {
    res.status(error.response?.status || 500).json({
      error: error.message,
      details: error.response?.data
    });
  }
});

// ============================================================================
// Error Handling
// ============================================================================

app.use((err, req, res, next) => {
  console.error(err.stack);
  res.status(500).json({
    error: 'Internal server error',
    message: process.env.NODE_ENV === 'development' ? err.message : undefined
  });
});

// ============================================================================
// Start Server
// ============================================================================

const server = app.listen(PORT, () => {
  console.log(`MotorHandPro Node.js Integration Gateway listening on port ${PORT}`);
  console.log(`Environment: ${process.env.NODE_ENV || 'development'}`);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  console.log('SIGTERM signal received: closing HTTP server');
  server.close(() => {
    console.log('HTTP server closed');
    db.end(() => {
      console.log('Database pool closed');
      mqttClient.end(() => {
        console.log('MQTT client disconnected');
        process.exit(0);
      });
    });
  });
});

module.exports = app;
