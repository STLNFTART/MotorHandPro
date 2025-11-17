#!/usr/bin/env python3
"""
LAM Database Models
SQLAlchemy models for PostgreSQL backend
"""
from datetime import datetime
from typing import Optional
from sqlalchemy import Column, Integer, String, Float, Boolean, DateTime, JSON, ForeignKey, Text
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship

Base = declarative_base()


class User(Base):
    """User model"""
    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    username = Column(String(50), unique=True, index=True, nullable=False)
    email = Column(String(100), unique=True, index=True)
    full_name = Column(String(100))
    hashed_password = Column(String(255), nullable=False)
    disabled = Column(Boolean, default=False)
    roles = Column(JSON, default=list)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relationships
    actions = relationship("Action", back_populates="user")
    experiments = relationship("Experiment", back_populates="user")


class Action(Base):
    """Action execution record"""
    __tablename__ = "actions"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    action_type = Column(String(50), index=True, nullable=False)
    params = Column(JSON)
    result = Column(JSON)
    success = Column(Boolean, default=True)
    duration_ms = Column(Float)
    error_message = Column(Text, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow, index=True)

    # Relationships
    user = relationship("User", back_populates="actions")


class ResonanceState(Base):
    """Quantum resonance field state log"""
    __tablename__ = "resonance_states"

    id = Column(Integer, primary_key=True, index=True)
    alpha = Column(Float, nullable=False)
    lambda_param = Column("lambda", Float, nullable=False)
    lightfoot_constant = Column(Float, nullable=False)
    donte_attractor = Column(Float, nullable=False)
    lipschitz_constant = Column(Float, nullable=False)
    epoch = Column(Integer, nullable=False)
    stable = Column(Boolean, default=True)
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)


class Experiment(Base):
    """Lab experiment record"""
    __tablename__ = "experiments"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    goal_id = Column(String(50), unique=True, index=True)
    title = Column(String(200), nullable=False)
    description = Column(Text)
    objective = Column(Text)
    success_criteria = Column(JSON)
    parameters = Column(JSON)
    status = Column(String(20), index=True)  # planning, running, analyzing, complete, failed
    results = Column(JSON, nullable=True)
    notes = Column(JSON, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)
    updated_at = Column(DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

    # Relationships
    user = relationship("User", back_populates="experiments")


class APIKey(Base):
    """API key management"""
    __tablename__ = "api_keys"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey("users.id"))
    key_name = Column(String(100), nullable=False)
    key_hash = Column(String(255), nullable=False)
    scopes = Column(JSON, default=list)
    enabled = Column(Boolean, default=True)
    last_used = Column(DateTime, nullable=True)
    expires_at = Column(DateTime, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)


class Metric(Base):
    """Performance metrics"""
    __tablename__ = "metrics"

    id = Column(Integer, primary_key=True, index=True)
    metric_type = Column(String(50), index=True, nullable=False)
    metric_name = Column(String(100), index=True, nullable=False)
    value = Column(Float, nullable=False)
    tags = Column(JSON, default=dict)
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)


class SatelliteData(Base):
    """Satellite constellation data"""
    __tablename__ = "satellite_data"

    id = Column(Integer, primary_key=True, index=True)
    satellite_id = Column(Integer, index=True)
    position_x = Column(Float)
    position_y = Column(Float)
    position_z = Column(Float)
    velocity_x = Column(Float)
    velocity_y = Column(Float)
    velocity_z = Column(Float)
    altitude_km = Column(Float)
    status = Column(String(20))
    timestamp = Column(DateTime, default=datetime.utcnow, index=True)
