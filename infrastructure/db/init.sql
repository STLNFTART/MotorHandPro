-- MotorHandPro Database Initialization
-- PostgreSQL + TimescaleDB schema for telemetry and time-series data
-- Patent Pending: U.S. Provisional Patent Application No. 63/842,846

-- Enable TimescaleDB extension
CREATE EXTENSION IF NOT EXISTS timescaledb;

-- Create schemas
CREATE SCHEMA IF NOT EXISTS telemetry;
CREATE SCHEMA IF NOT EXISTS auth;
CREATE SCHEMA IF NOT EXISTS experiments;
CREATE SCHEMA IF NOT EXISTS integrations;

-- ============================================================================
-- AUTHENTICATION SCHEMA
-- ============================================================================

-- Users table
CREATE TABLE IF NOT EXISTS auth.users (
    id SERIAL PRIMARY KEY,
    username VARCHAR(255) UNIQUE NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    role VARCHAR(50) DEFAULT 'user',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_login TIMESTAMP WITH TIME ZONE,
    is_active BOOLEAN DEFAULT TRUE
);

-- API keys table
CREATE TABLE IF NOT EXISTS auth.api_keys (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES auth.users(id) ON DELETE CASCADE,
    key_hash VARCHAR(255) UNIQUE NOT NULL,
    name VARCHAR(255),
    scopes JSONB DEFAULT '[]'::jsonb,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE,
    last_used TIMESTAMP WITH TIME ZONE,
    is_active BOOLEAN DEFAULT TRUE
);

-- Sessions table
CREATE TABLE IF NOT EXISTS auth.sessions (
    id SERIAL PRIMARY KEY,
    user_id INTEGER REFERENCES auth.users(id) ON DELETE CASCADE,
    token_hash VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP WITH TIME ZONE NOT NULL,
    ip_address INET,
    user_agent TEXT
);

-- ============================================================================
-- TELEMETRY SCHEMA
-- ============================================================================

-- Spacecraft telemetry (time-series)
CREATE TABLE IF NOT EXISTS telemetry.spacecraft (
    time TIMESTAMPTZ NOT NULL,
    spacecraft_id VARCHAR(50) NOT NULL,
    position_x DOUBLE PRECISION,
    position_y DOUBLE PRECISION,
    position_z DOUBLE PRECISION,
    velocity_x DOUBLE PRECISION,
    velocity_y DOUBLE PRECISION,
    velocity_z DOUBLE PRECISION,
    acceleration_x DOUBLE PRECISION,
    acceleration_y DOUBLE PRECISION,
    acceleration_z DOUBLE PRECISION,
    thrust_x DOUBLE PRECISION,
    thrust_y DOUBLE PRECISION,
    thrust_z DOUBLE PRECISION,
    quaternion_w DOUBLE PRECISION,
    quaternion_x DOUBLE PRECISION,
    quaternion_y DOUBLE PRECISION,
    quaternion_z DOUBLE PRECISION,
    angular_velocity_x DOUBLE PRECISION,
    angular_velocity_y DOUBLE PRECISION,
    angular_velocity_z DOUBLE PRECISION,
    fuel_remaining DOUBLE PRECISION,
    battery_voltage DOUBLE PRECISION,
    temperature DOUBLE PRECISION,
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('telemetry.spacecraft', 'time', if_not_exists => TRUE);

-- Sensor data (time-series)
CREATE TABLE IF NOT EXISTS telemetry.sensors (
    time TIMESTAMPTZ NOT NULL,
    sensor_id VARCHAR(50) NOT NULL,
    sensor_type VARCHAR(50) NOT NULL,
    value DOUBLE PRECISION NOT NULL,
    unit VARCHAR(20),
    quality INTEGER DEFAULT 100,
    metadata JSONB
);

SELECT create_hypertable('telemetry.sensors', 'time', if_not_exists => TRUE);

-- AGP (Anti-Gravity Protocol) state (time-series)
CREATE TABLE IF NOT EXISTS telemetry.agp_state (
    time TIMESTAMPTZ NOT NULL,
    system_id VARCHAR(50) NOT NULL,
    primal_state DOUBLE PRECISION,
    error_position_x DOUBLE PRECISION,
    error_position_y DOUBLE PRECISION,
    error_position_z DOUBLE PRECISION,
    error_velocity_x DOUBLE PRECISION,
    error_velocity_y DOUBLE PRECISION,
    error_velocity_z DOUBLE PRECISION,
    integral_state DOUBLE PRECISION,
    lipschitz_constant DOUBLE PRECISION,
    lambda_decay DOUBLE PRECISION,
    control_mode VARCHAR(50),
    stability_margin DOUBLE PRECISION,
    hash_sha512 VARCHAR(128),
    metadata JSONB
);

SELECT create_hypertable('telemetry.agp_state', 'time', if_not_exists => TRUE);

-- System performance metrics (time-series)
CREATE TABLE IF NOT EXISTS telemetry.performance (
    time TIMESTAMPTZ NOT NULL,
    service_name VARCHAR(50) NOT NULL,
    metric_name VARCHAR(100) NOT NULL,
    value DOUBLE PRECISION NOT NULL,
    unit VARCHAR(20),
    tags JSONB
);

SELECT create_hypertable('telemetry.performance', 'time', if_not_exists => TRUE);

-- ============================================================================
-- EXPERIMENTS SCHEMA
-- ============================================================================

-- Experiments table
CREATE TABLE IF NOT EXISTS experiments.experiments (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    description TEXT,
    created_by INTEGER REFERENCES auth.users(id),
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    started_at TIMESTAMP WITH TIME ZONE,
    completed_at TIMESTAMP WITH TIME ZONE,
    status VARCHAR(50) DEFAULT 'pending',
    configuration JSONB,
    results JSONB,
    metadata JSONB
);

-- Experiment runs (time-series)
CREATE TABLE IF NOT EXISTS experiments.runs (
    time TIMESTAMPTZ NOT NULL,
    experiment_id INTEGER REFERENCES experiments.experiments(id) ON DELETE CASCADE,
    run_number INTEGER NOT NULL,
    parameter_name VARCHAR(100),
    parameter_value DOUBLE PRECISION,
    metric_name VARCHAR(100),
    metric_value DOUBLE PRECISION,
    metadata JSONB
);

SELECT create_hypertable('experiments.runs', 'time', if_not_exists => TRUE);

-- ============================================================================
-- INTEGRATIONS SCHEMA
-- ============================================================================

-- Integration endpoints
CREATE TABLE IF NOT EXISTS integrations.endpoints (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) UNIQUE NOT NULL,
    type VARCHAR(50) NOT NULL, -- spacex, tesla, px4, carla, starlink, nasa
    url VARCHAR(500),
    api_key_id INTEGER REFERENCES auth.api_keys(id),
    configuration JSONB,
    status VARCHAR(50) DEFAULT 'active',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    last_sync TIMESTAMP WITH TIME ZONE
);

-- Integration events (time-series)
CREATE TABLE IF NOT EXISTS integrations.events (
    time TIMESTAMPTZ NOT NULL,
    endpoint_id INTEGER REFERENCES integrations.endpoints(id),
    event_type VARCHAR(100) NOT NULL,
    payload JSONB,
    status VARCHAR(50),
    error_message TEXT
);

SELECT create_hypertable('integrations.events', 'time', if_not_exists => TRUE);

-- ============================================================================
-- INDEXES
-- ============================================================================

-- Auth indexes
CREATE INDEX IF NOT EXISTS idx_users_email ON auth.users(email);
CREATE INDEX IF NOT EXISTS idx_users_username ON auth.users(username);
CREATE INDEX IF NOT EXISTS idx_api_keys_user ON auth.api_keys(user_id);
CREATE INDEX IF NOT EXISTS idx_sessions_user ON auth.sessions(user_id);

-- Telemetry indexes
CREATE INDEX IF NOT EXISTS idx_spacecraft_id ON telemetry.spacecraft(spacecraft_id, time DESC);
CREATE INDEX IF NOT EXISTS idx_sensors_id_type ON telemetry.sensors(sensor_id, sensor_type, time DESC);
CREATE INDEX IF NOT EXISTS idx_agp_system ON telemetry.agp_state(system_id, time DESC);
CREATE INDEX IF NOT EXISTS idx_performance_service ON telemetry.performance(service_name, metric_name, time DESC);

-- Experiments indexes
CREATE INDEX IF NOT EXISTS idx_experiments_status ON experiments.experiments(status);
CREATE INDEX IF NOT EXISTS idx_experiments_created_by ON experiments.experiments(created_by);
CREATE INDEX IF NOT EXISTS idx_runs_experiment ON experiments.runs(experiment_id, time DESC);

-- Integrations indexes
CREATE INDEX IF NOT EXISTS idx_endpoints_type ON integrations.endpoints(type);
CREATE INDEX IF NOT EXISTS idx_events_endpoint ON integrations.events(endpoint_id, time DESC);

-- ============================================================================
-- FUNCTIONS AND TRIGGERS
-- ============================================================================

-- Update timestamp trigger function
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply to users table
CREATE TRIGGER update_users_updated_at BEFORE UPDATE ON auth.users
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- ============================================================================
-- INITIAL DATA
-- ============================================================================

-- Create default admin user (password: admin123 - CHANGE IN PRODUCTION!)
-- Password hash is bcrypt for 'admin123'
INSERT INTO auth.users (username, email, password_hash, role)
VALUES ('admin', 'admin@motorhand.local', '$2b$12$LQv3c1yqBWVHxkd0LHAkCOYz6TtxMQJqhN8/LewY5GyYqYj8YuYP6', 'admin')
ON CONFLICT (username) DO NOTHING;

-- Create integration endpoints
INSERT INTO integrations.endpoints (name, type, url, status)
VALUES
    ('SpaceX Telemetry', 'spacex', 'https://api.spacex.com/v4', 'active'),
    ('Tesla Autopilot', 'tesla', 'https://tesla-autopilot-integration.local', 'active'),
    ('PX4 Flight Controller', 'px4', 'http://px4-flight-controller.local:8080', 'active'),
    ('CARLA Simulator', 'carla', 'http://carla-simulator.local:2000', 'active'),
    ('Starlink Network', 'starlink', 'https://starlink-api.local', 'active'),
    ('NASA Data', 'nasa', 'https://api.nasa.gov', 'active')
ON CONFLICT (name) DO NOTHING;

-- Grant permissions
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA auth TO motorhand;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA telemetry TO motorhand;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA experiments TO motorhand;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA integrations TO motorhand;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA auth TO motorhand;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA experiments TO motorhand;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA integrations TO motorhand;

-- Create read-only user for Grafana
CREATE USER grafana_reader WITH PASSWORD 'grafana_readonly_password';
GRANT CONNECT ON DATABASE motorhand TO grafana_reader;
GRANT USAGE ON SCHEMA telemetry, experiments, integrations TO grafana_reader;
GRANT SELECT ON ALL TABLES IN SCHEMA telemetry, experiments, integrations TO grafana_reader;

-- Success message
DO $$
BEGIN
    RAISE NOTICE 'MotorHandPro database initialized successfully!';
    RAISE NOTICE 'TimescaleDB hypertables created for time-series data';
    RAISE NOTICE 'Default admin user: admin / admin123 (CHANGE PASSWORD!)';
END $$;
