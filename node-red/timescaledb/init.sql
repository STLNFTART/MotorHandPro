-- TimescaleDB initialization for MotorHandPro telemetry storage
-- This script creates the necessary tables and hypertables for time-series data

-- Enable TimescaleDB extension
CREATE EXTENSION IF NOT EXISTS timescaledb;

-- Create schema for MotorHandPro data
CREATE SCHEMA IF NOT EXISTS motorhand;

--------------------------------------------------------------------------------
-- Satellite Telemetry Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.satellite_telemetry (
    time TIMESTAMPTZ NOT NULL,
    satellite_id INTEGER NOT NULL,
    position_x DOUBLE PRECISION,
    position_y DOUBLE PRECISION,
    position_z DOUBLE PRECISION,
    velocity_x DOUBLE PRECISION,
    velocity_y DOUBLE PRECISION,
    velocity_z DOUBLE PRECISION,
    altitude_km DOUBLE PRECISION,
    latitude DOUBLE PRECISION,
    longitude DOUBLE PRECISION,
    status VARCHAR(50),
    metadata JSONB
);

-- Convert to hypertable (time-series optimization)
SELECT create_hypertable('motorhand.satellite_telemetry', 'time', if_not_exists => TRUE);

-- Add indexes for common queries
CREATE INDEX IF NOT EXISTS idx_sat_id_time ON motorhand.satellite_telemetry (satellite_id, time DESC);
CREATE INDEX IF NOT EXISTS idx_sat_status ON motorhand.satellite_telemetry (status, time DESC);

-- Add retention policy (keep 30 days of data)
SELECT add_retention_policy('motorhand.satellite_telemetry', INTERVAL '30 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- LAM Action Metrics Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.lam_action_metrics (
    time TIMESTAMPTZ NOT NULL,
    action_id INTEGER,
    action_type VARCHAR(100) NOT NULL,
    success BOOLEAN NOT NULL,
    duration_ms DOUBLE PRECISION,
    resonance_alpha DOUBLE PRECISION,
    resonance_lambda DOUBLE PRECISION,
    resonance_stable BOOLEAN,
    error_message TEXT,
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('motorhand.lam_action_metrics', 'time', if_not_exists => TRUE);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_action_type_time ON motorhand.lam_action_metrics (action_type, time DESC);
CREATE INDEX IF NOT EXISTS idx_action_success ON motorhand.lam_action_metrics (success, time DESC);

-- Retention policy
SELECT add_retention_policy('motorhand.lam_action_metrics', INTERVAL '90 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- Experiment Results Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.experiment_results (
    time TIMESTAMPTZ NOT NULL,
    goal_id VARCHAR(255) NOT NULL,
    title TEXT,
    status VARCHAR(50),
    phase_drift_ms DOUBLE PRECISION,
    max_p_lv DOUBLE PRECISION,
    control_energy DOUBLE PRECISION,
    lipschitz_estimate DOUBLE PRECISION,
    stability_metric DOUBLE PRECISION,
    results JSONB,
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('motorhand.experiment_results', 'time', if_not_exists => TRUE);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_goal_id_time ON motorhand.experiment_results (goal_id, time DESC);
CREATE INDEX IF NOT EXISTS idx_exp_status ON motorhand.experiment_results (status, time DESC);

-- Retention policy
SELECT add_retention_policy('motorhand.experiment_results', INTERVAL '180 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- Motor Control Telemetry Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.motor_telemetry (
    time TIMESTAMPTZ NOT NULL,
    motor_id VARCHAR(50) NOT NULL,
    psi DOUBLE PRECISION,  -- Control command signal
    gamma DOUBLE PRECISION,  -- Error signal
    ec DOUBLE PRECISION,  -- Integrated control energy
    mu DOUBLE PRECISION,  -- Lightfoot constant
    ke DOUBLE PRECISION,  -- Kinetic energy coefficient
    d_constant DOUBLE PRECISION,  -- Donte attractor
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('motorhand.motor_telemetry', 'time', if_not_exists => TRUE);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_motor_id_time ON motorhand.motor_telemetry (motor_id, time DESC);

-- Retention policy
SELECT add_retention_policy('motorhand.motor_telemetry', INTERVAL '60 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- Alert History Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.alert_history (
    time TIMESTAMPTZ NOT NULL,
    severity VARCHAR(20) NOT NULL,  -- critical, warning, info
    source VARCHAR(100) NOT NULL,  -- satellite, lam, motor, etc.
    title TEXT NOT NULL,
    message TEXT,
    acknowledged BOOLEAN DEFAULT FALSE,
    acknowledged_by VARCHAR(100),
    acknowledged_at TIMESTAMPTZ,
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('motorhand.alert_history', 'time', if_not_exists => TRUE);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_alert_severity ON motorhand.alert_history (severity, time DESC);
CREATE INDEX IF NOT EXISTS idx_alert_ack ON motorhand.alert_history (acknowledged, time DESC);

-- Retention policy
SELECT add_retention_policy('motorhand.alert_history', INTERVAL '365 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- MQTT Message Log Table
--------------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS motorhand.mqtt_messages (
    time TIMESTAMPTZ NOT NULL,
    topic VARCHAR(255) NOT NULL,
    payload TEXT,
    qos INTEGER,
    retained BOOLEAN,
    metadata JSONB
);

-- Convert to hypertable
SELECT create_hypertable('motorhand.mqtt_messages', 'time', if_not_exists => TRUE);

-- Indexes
CREATE INDEX IF NOT EXISTS idx_mqtt_topic ON motorhand.mqtt_messages (topic, time DESC);

-- Retention policy (short - only for debugging)
SELECT add_retention_policy('motorhand.mqtt_messages', INTERVAL '7 days', if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- Continuous Aggregates for Performance
--------------------------------------------------------------------------------

-- Hourly satellite telemetry summary
CREATE MATERIALIZED VIEW IF NOT EXISTS motorhand.satellite_telemetry_hourly
WITH (timescaledb.continuous) AS
SELECT
    time_bucket('1 hour', time) AS bucket,
    satellite_id,
    COUNT(*) as readings,
    AVG(altitude_km) as avg_altitude,
    MIN(altitude_km) as min_altitude,
    MAX(altitude_km) as max_altitude,
    LAST(status, time) as latest_status
FROM motorhand.satellite_telemetry
GROUP BY bucket, satellite_id;

-- Refresh policy for continuous aggregate
SELECT add_continuous_aggregate_policy('motorhand.satellite_telemetry_hourly',
    start_offset => INTERVAL '3 hours',
    end_offset => INTERVAL '1 hour',
    schedule_interval => INTERVAL '1 hour',
    if_not_exists => TRUE);

-- LAM action metrics by hour
CREATE MATERIALIZED VIEW IF NOT EXISTS motorhand.lam_metrics_hourly
WITH (timescaledb.continuous) AS
SELECT
    time_bucket('1 hour', time) AS bucket,
    action_type,
    COUNT(*) as total_actions,
    COUNT(*) FILTER (WHERE success = TRUE) as successful_actions,
    AVG(duration_ms) as avg_duration_ms,
    AVG(resonance_alpha) as avg_alpha,
    AVG(resonance_lambda) as avg_lambda
FROM motorhand.lam_action_metrics
GROUP BY bucket, action_type;

-- Refresh policy
SELECT add_continuous_aggregate_policy('motorhand.lam_metrics_hourly',
    start_offset => INTERVAL '3 hours',
    end_offset => INTERVAL '1 hour',
    schedule_interval => INTERVAL '1 hour',
    if_not_exists => TRUE);

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- Function to get latest satellite positions
CREATE OR REPLACE FUNCTION motorhand.get_latest_satellite_positions(limit_count INTEGER DEFAULT 100)
RETURNS TABLE (
    satellite_id INTEGER,
    time TIMESTAMPTZ,
    position_x DOUBLE PRECISION,
    position_y DOUBLE PRECISION,
    position_z DOUBLE PRECISION,
    altitude_km DOUBLE PRECISION
) AS $$
BEGIN
    RETURN QUERY
    SELECT DISTINCT ON (st.satellite_id)
        st.satellite_id,
        st.time,
        st.position_x,
        st.position_y,
        st.position_z,
        st.altitude_km
    FROM motorhand.satellite_telemetry st
    ORDER BY st.satellite_id, st.time DESC
    LIMIT limit_count;
END;
$$ LANGUAGE plpgsql;

-- Function to get system health metrics
CREATE OR REPLACE FUNCTION motorhand.get_system_health()
RETURNS TABLE (
    metric VARCHAR,
    value DOUBLE PRECISION,
    status VARCHAR
) AS $$
BEGIN
    RETURN QUERY
    SELECT
        'lam_success_rate'::VARCHAR as metric,
        (COUNT(*) FILTER (WHERE success = TRUE)::FLOAT / NULLIF(COUNT(*), 0))::DOUBLE PRECISION as value,
        CASE
            WHEN (COUNT(*) FILTER (WHERE success = TRUE)::FLOAT / NULLIF(COUNT(*), 0)) > 0.95 THEN 'healthy'
            WHEN (COUNT(*) FILTER (WHERE success = TRUE)::FLOAT / NULLIF(COUNT(*), 0)) > 0.80 THEN 'warning'
            ELSE 'critical'
        END as status
    FROM motorhand.lam_action_metrics
    WHERE time > NOW() - INTERVAL '1 hour';
END;
$$ LANGUAGE plpgsql;

--------------------------------------------------------------------------------
-- Grant Permissions
--------------------------------------------------------------------------------
GRANT USAGE ON SCHEMA motorhand TO nodered;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA motorhand TO nodered;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA motorhand TO nodered;

-- Print completion message
DO $$
BEGIN
    RAISE NOTICE 'MotorHandPro TimescaleDB initialization complete!';
    RAISE NOTICE 'Schema: motorhand';
    RAISE NOTICE 'Tables: satellite_telemetry, lam_action_metrics, experiment_results, motor_telemetry, alert_history, mqtt_messages';
    RAISE NOTICE 'Continuous aggregates: satellite_telemetry_hourly, lam_metrics_hourly';
END $$;
