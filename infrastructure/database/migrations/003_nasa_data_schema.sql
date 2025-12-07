-- NASA Data Schema Migration
-- Creates tables for storing NASA comet observations and processed states

-- Create nasa_data schema
CREATE SCHEMA IF NOT EXISTS nasa_data;

-- Create comet_observations table
CREATE TABLE IF NOT EXISTS nasa_data.comet_observations (
    id SERIAL PRIMARY KEY,
    time TIMESTAMPTZ NOT NULL,
    ra DOUBLE PRECISION NOT NULL,
    dec DOUBLE PRECISION NOT NULL,
    distance_au DOUBLE PRECISION NOT NULL,
    velocity_km_s DOUBLE PRECISION,
    magnitude DOUBLE PRECISION,
    gas_production_rate DOUBLE PRECISION,
    tail_length_km DOUBLE PRECISION,
    data_source VARCHAR(50) NOT NULL,
    processed BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    UNIQUE(time)
);

-- Create hypertable for time-series data (TimescaleDB)
SELECT create_hypertable(
    'nasa_data.comet_observations',
    'time',
    if_not_exists => TRUE
);

-- Create processed_states table
CREATE TABLE IF NOT EXISTS nasa_data.processed_states (
    id SERIAL PRIMARY KEY,
    time TIMESTAMPTZ NOT NULL,
    observation_id INTEGER REFERENCES nasa_data.comet_observations(id),
    primal_n INTEGER NOT NULL,
    signal DOUBLE PRECISION NOT NULL,
    memory_integral DOUBLE PRECISION NOT NULL,
    error DOUBLE PRECISION NOT NULL,
    anomaly_score DOUBLE PRECISION NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create hypertable for processed states
SELECT create_hypertable(
    'nasa_data.processed_states',
    'time',
    if_not_exists => TRUE
);

-- Create indexes
CREATE INDEX IF NOT EXISTS idx_comet_obs_time ON nasa_data.comet_observations(time DESC);
CREATE INDEX IF NOT EXISTS idx_comet_obs_source ON nasa_data.comet_observations(data_source);
CREATE INDEX IF NOT EXISTS idx_comet_obs_processed ON nasa_data.comet_observations(processed);
CREATE INDEX IF NOT EXISTS idx_processed_states_time ON nasa_data.processed_states(time DESC);
CREATE INDEX IF NOT EXISTS idx_processed_states_obs ON nasa_data.processed_states(observation_id);

-- Grant permissions
GRANT USAGE ON SCHEMA nasa_data TO motorhand;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA nasa_data TO motorhand;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA nasa_data TO motorhand;

-- Comments
COMMENT ON TABLE nasa_data.comet_observations IS 'NASA comet 3I/ATLAS observations from multiple data sources';
COMMENT ON TABLE nasa_data.processed_states IS 'Processed states from Recursive Planck Operator';
COMMENT ON COLUMN nasa_data.comet_observations.data_source IS 'Data source: horizons, mpc, or simulated';
COMMENT ON COLUMN nasa_data.comet_observations.processed IS 'Whether observation has been processed through operator';
