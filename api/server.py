#!/usr/bin/env python3
"""
FastAPI Server for MotorHandPro
Provides REST API endpoints for:
- LAM actions
- NASA observations
- Test results
- Satellite tracking

Author: Donte Lightfoot
Date: December 11, 2025
"""

import os
import sys
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any, Optional
from dotenv import load_dotenv

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel

# Load environment variables
load_dotenv()

# Add config path
sys.path.insert(0, str(Path(__file__).parent.parent / "config"))

try:
    from mongodb import MotorHandProDB
except ImportError as e:
    print(f"❌ Error importing MongoDB module: {e}")
    print("   Make sure config/mongodb.py is available")
    sys.exit(1)


# =============================================================================
# Pydantic Models
# =============================================================================

class PrimalState(BaseModel):
    """Primal Logic state"""
    n: float
    signal: float
    memory_integral: float
    error: float
    anomaly_score: float


class LAMIntegration(BaseModel):
    """LAM integration data"""
    enabled: bool
    E_displaced: Optional[float] = None


class LAMActionRequest(BaseModel):
    """Request to save LAM action"""
    action_type: str
    user_id: str
    primal_state: PrimalState
    lam_integration: Optional[LAMIntegration] = None
    metadata: Optional[Dict[str, Any]] = None


class NASAObservation(BaseModel):
    """NASA comet observation"""
    ra: float
    dec: float
    distance_au: float
    velocity_km_s: Optional[float] = None
    magnitude: Optional[float] = None
    gas_production_rate: Optional[float] = None
    tail_length_km: Optional[float] = None


class NASAObservationRequest(BaseModel):
    """Request to save NASA observation"""
    comet_id: str
    observation: NASAObservation
    primal_state: PrimalState
    lam_integration: Optional[LAMIntegration] = None


class TestResultRequest(BaseModel):
    """Request to save test result"""
    branch: str
    test_type: str
    success: bool
    results: Dict[str, Any]
    primal_constants: Optional[Dict[str, float]] = None


class SatellitePosition(BaseModel):
    """Satellite position"""
    x: float
    y: float
    z: float


class SatelliteVelocity(BaseModel):
    """Satellite velocity"""
    vx: float
    vy: float
    vz: float


class SatelliteDataRequest(BaseModel):
    """Request to save satellite data"""
    satellite_id: str
    position: SatellitePosition
    velocity: SatelliteVelocity
    metadata: Optional[Dict[str, Any]] = None


# =============================================================================
# FastAPI App
# =============================================================================

app = FastAPI(
    title="MotorHandPro API",
    description="REST API for MotorHandPro with MongoDB integration",
    version="1.0.0"
)

# CORS middleware
origins = os.getenv('CORS_ORIGINS', 'http://localhost:3000').split(',')

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Database connection
db = None


@app.on_event("startup")
async def startup_event():
    """Initialize database connection on startup"""
    global db
    try:
        db = MotorHandProDB()
        if db.verify_connection():
            print("✅ Database connected successfully")
        else:
            print("❌ Database connection failed")
    except Exception as e:
        print(f"❌ Error initializing database: {e}")
        raise


@app.on_event("shutdown")
async def shutdown_event():
    """Close database connection on shutdown"""
    global db
    if db:
        db.close()
        print("✅ Database connection closed")


# =============================================================================
# Health Check
# =============================================================================

@app.get("/")
async def root():
    """Root endpoint - health check"""
    return {
        "status": "online",
        "service": "MotorHandPro API",
        "version": "1.0.0",
        "timestamp": datetime.utcnow().isoformat()
    }


@app.get("/health")
async def health_check():
    """Health check endpoint"""
    try:
        is_connected = db.verify_connection()
        return {
            "status": "healthy" if is_connected else "unhealthy",
            "database": "connected" if is_connected else "disconnected",
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=503, detail=f"Service unhealthy: {str(e)}")


@app.get("/stats")
async def get_stats():
    """Get overall database statistics"""
    try:
        stats = db.get_database_stats()
        return {
            "status": "success",
            "data": stats,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# LAM Actions Endpoints
# =============================================================================

@app.post("/lam/actions")
async def create_lam_action(action: LAMActionRequest):
    """Save new LAM action"""
    try:
        doc_id = db.save_lam_action(
            action_type=action.action_type,
            user_id=action.user_id,
            primal_state=action.primal_state.dict(),
            lam_integration=action.lam_integration.dict() if action.lam_integration else None,
            metadata=action.metadata
        )
        return {
            "status": "success",
            "id": doc_id,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/lam/actions")
async def get_lam_actions(
    user_id: Optional[str] = Query(None, description="Filter by user ID"),
    action_type: Optional[str] = Query(None, description="Filter by action type"),
    limit: int = Query(100, description="Maximum results", ge=1, le=1000)
):
    """Get LAM actions"""
    try:
        actions = db.get_lam_actions(
            user_id=user_id,
            action_type=action_type,
            limit=limit
        )
        return {
            "status": "success",
            "count": len(actions),
            "data": actions,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/lam/actions/stats")
async def get_lam_action_stats():
    """Get LAM action statistics"""
    try:
        stats = db.get_lam_action_stats()
        return {
            "status": "success",
            "data": stats,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# NASA Observations Endpoints
# =============================================================================

@app.post("/nasa/observations")
async def create_nasa_observation(observation: NASAObservationRequest):
    """Save new NASA observation"""
    try:
        doc_id = db.save_nasa_observation(
            comet_id=observation.comet_id,
            observation=observation.observation.dict(),
            primal_state=observation.primal_state.dict(),
            lam_integration=observation.lam_integration.dict() if observation.lam_integration else None
        )
        return {
            "status": "success",
            "id": doc_id,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/nasa/observations")
async def get_nasa_observations(
    comet_id: Optional[str] = Query(None, description="Filter by comet ID"),
    limit: int = Query(100, description="Maximum results", ge=1, le=1000)
):
    """Get NASA observations"""
    try:
        observations = db.get_nasa_observations(
            comet_id=comet_id,
            limit=limit
        )
        return {
            "status": "success",
            "count": len(observations),
            "data": observations,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/nasa/observations/stats")
async def get_nasa_observation_stats():
    """Get NASA observation statistics"""
    try:
        stats = db.get_nasa_observation_stats()
        return {
            "status": "success",
            "data": stats,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# Test Results Endpoints
# =============================================================================

@app.post("/test/results")
async def create_test_result(result: TestResultRequest):
    """Save new test result"""
    try:
        doc_id = db.save_test_result(
            branch=result.branch,
            test_type=result.test_type,
            success=result.success,
            results=result.results,
            primal_constants=result.primal_constants
        )
        return {
            "status": "success",
            "id": doc_id,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/test/results")
async def get_test_results(
    branch: Optional[str] = Query(None, description="Filter by branch"),
    test_type: Optional[str] = Query(None, description="Filter by test type"),
    success: Optional[bool] = Query(None, description="Filter by success status"),
    limit: int = Query(100, description="Maximum results", ge=1, le=1000)
):
    """Get test results"""
    try:
        results = db.get_test_results(
            branch=branch,
            test_type=test_type,
            success=success,
            limit=limit
        )
        return {
            "status": "success",
            "count": len(results),
            "data": results,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/test/results/stats")
async def get_test_result_stats():
    """Get test result statistics"""
    try:
        stats = db.get_test_result_stats()
        return {
            "status": "success",
            "data": stats,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# Satellite Tracking Endpoints
# =============================================================================

@app.post("/satellite/data")
async def create_satellite_data(data: SatelliteDataRequest):
    """Save new satellite tracking data"""
    try:
        doc_id = db.save_satellite_data(
            satellite_id=data.satellite_id,
            position=data.position.dict(),
            velocity=data.velocity.dict(),
            metadata=data.metadata
        )
        return {
            "status": "success",
            "id": doc_id,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/satellite/data")
async def get_satellite_data(
    satellite_id: Optional[str] = Query(None, description="Filter by satellite ID"),
    limit: int = Query(100, description="Maximum results", ge=1, le=1000)
):
    """Get satellite tracking data"""
    try:
        data = db.get_satellite_data(
            satellite_id=satellite_id,
            limit=limit
        )
        return {
            "status": "success",
            "count": len(data),
            "data": data,
            "timestamp": datetime.utcnow().isoformat()
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


# =============================================================================
# Main Entry Point
# =============================================================================

if __name__ == "__main__":
    import uvicorn

    host = os.getenv('API_HOST', '0.0.0.0')
    port = int(os.getenv('API_PORT', 8000))
    reload = os.getenv('API_RELOAD', 'true').lower() == 'true'

    print("=" * 80)
    print("🚀 MotorHandPro API Server")
    print("=" * 80)
    print(f"   Host: {host}")
    print(f"   Port: {port}")
    print(f"   Reload: {reload}")
    print("=" * 80)
    print()

    uvicorn.run(
        "server:app",
        host=host,
        port=port,
        reload=reload
    )
