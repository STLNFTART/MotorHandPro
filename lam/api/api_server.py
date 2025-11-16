#!/usr/bin/env python3
"""
LAM REST API Server
FastAPI-based API for LAM operations
"""
import sys
from pathlib import Path
from typing import Dict, Any, Optional, List
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

from fastapi import FastAPI, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
import uvicorn

try:
    from lam_main import LAM
    from core.primal_lam import PrimalLAM
    from monitoring.logger import LAMLogger
    LAM_AVAILABLE = True
except ImportError as e:
    print(f"Warning: LAM not fully available: {e}")
    LAM_AVAILABLE = False


# Pydantic models
class TripRequest(BaseModel):
    destination: str
    departure_date: str
    return_date: str
    budget: Optional[float] = None


class ReservationRequest(BaseModel):
    venue_type: str
    venue_name: str
    date: str
    time: str
    party_size: int
    special_requests: Optional[str] = None


class FoodOrderRequest(BaseModel):
    restaurant: str
    items: List[Dict[str, Any]]
    delivery_address: str
    special_instructions: Optional[str] = None


class QuestionRequest(BaseModel):
    question: str


class TaskRequest(BaseModel):
    task: str


# Initialize FastAPI
app = FastAPI(
    title="LAM API",
    description="Large Action Model REST API with Quantum-Semantic Intelligence",
    version="1.0.0"
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize LAM
lam_instance = None
logger = None

if LAM_AVAILABLE:
    try:
        lam_instance = LAM()
        logger = LAMLogger()
    except Exception as e:
        print(f"Warning: Could not initialize LAM: {e}")


@app.on_event("startup")
async def startup_event():
    """Startup event"""
    print("🚀 LAM API Server starting...")
    print(f"   Lightfoot constant: 0.16905")
    print(f"   Donte attractor: 149.9992314000")
    print(f"   Status: {'Ready' if lam_instance else 'Limited functionality'}")


@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "service": "LAM API",
        "version": "1.0.0",
        "status": "running" if lam_instance else "limited",
        "timestamp": datetime.now().isoformat()
    }


@app.get("/health")
async def health_check():
    """Health check endpoint"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    return {
        "status": "healthy",
        "lam_available": LAM_AVAILABLE,
        "timestamp": datetime.now().isoformat()
    }


@app.get("/status")
async def get_status():
    """Get LAM system status"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    status = lam_instance.get_status()

    return {
        "status": status,
        "timestamp": datetime.now().isoformat()
    }


@app.post("/trip/plan")
async def plan_trip(request: TripRequest, background_tasks: BackgroundTasks):
    """Plan a trip"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    try:
        result = lam_instance.plan_trip(
            destination=request.destination,
            departure=request.departure_date,
            return_date=request.return_date,
            budget=request.budget
        )

        # Log in background
        if logger:
            background_tasks.add_task(
                logger.log_action,
                "trip_planning",
                request.dict(),
                True,
                0.0,
                result
            )

        return {
            "success": True,
            "result": result,
            "timestamp": datetime.now().isoformat()
        }

    except Exception as e:
        if logger:
            background_tasks.add_task(
                logger.log_error,
                e,
                {"endpoint": "plan_trip", "request": request.dict()}
            )
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/reservation/make")
async def make_reservation(request: ReservationRequest, background_tasks: BackgroundTasks):
    """Make a reservation"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    try:
        result = lam_instance.make_reservation(
            venue_type=request.venue_type,
            venue_name=request.venue_name,
            date=request.date,
            time=request.time,
            party_size=request.party_size
        )

        if logger:
            background_tasks.add_task(
                logger.log_action,
                "make_reservation",
                request.dict(),
                True,
                0.0,
                result
            )

        return {
            "success": True,
            "result": result,
            "timestamp": datetime.now().isoformat()
        }

    except Exception as e:
        if logger:
            background_tasks.add_task(
                logger.log_error,
                e,
                {"endpoint": "make_reservation", "request": request.dict()}
            )
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/food/order")
async def order_food(request: FoodOrderRequest, background_tasks: BackgroundTasks):
    """Order food"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    try:
        result = lam_instance.order_food(
            restaurant=request.restaurant,
            items=request.items,
            address=request.delivery_address
        )

        if logger:
            background_tasks.add_task(
                logger.log_action,
                "order_food",
                request.dict(),
                True,
                0.0,
                result
            )

        return {
            "success": True,
            "result": result,
            "timestamp": datetime.now().isoformat()
        }

    except Exception as e:
        if logger:
            background_tasks.add_task(
                logger.log_error,
                e,
                {"endpoint": "order_food", "request": request.dict()}
            )
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/ask")
async def ask_question(request: QuestionRequest, background_tasks: BackgroundTasks):
    """Ask a question"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    try:
        answer = lam_instance.ask_question(request.question)

        if logger:
            background_tasks.add_task(
                logger.log_action,
                "ask_question",
                request.dict(),
                True,
                0.0,
                {"answer": answer}
            )

        return {
            "success": True,
            "question": request.question,
            "answer": answer,
            "timestamp": datetime.now().isoformat()
        }

    except Exception as e:
        if logger:
            background_tasks.add_task(
                logger.log_error,
                e,
                {"endpoint": "ask_question", "request": request.dict()}
            )
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/task")
async def complete_task(request: TaskRequest, background_tasks: BackgroundTasks):
    """Complete a task"""
    if not lam_instance:
        raise HTTPException(status_code=503, detail="LAM not available")

    try:
        result = lam_instance.complete_task(request.task)

        if logger:
            background_tasks.add_task(
                logger.log_action,
                "complete_task",
                request.dict(),
                True,
                0.0,
                {"result": result}
            )

        return {
            "success": True,
            "task": request.task,
            "result": result,
            "timestamp": datetime.now().isoformat()
        }

    except Exception as e:
        if logger:
            background_tasks.add_task(
                logger.log_error,
                e,
                {"endpoint": "complete_task", "request": request.dict()}
            )
        raise HTTPException(status_code=500, detail=str(e))


@app.get("/metrics")
async def get_metrics():
    """Get performance metrics"""
    if not logger:
        raise HTTPException(status_code=503, detail="Logging not available")

    metrics = logger.get_metrics()

    return {
        "success": True,
        "metrics": metrics,
        "timestamp": datetime.now().isoformat()
    }


def main():
    """Run API server"""
    print("Starting LAM API Server...")
    uvicorn.run(
        "api_server:app",
        host="0.0.0.0",
        port=8000,
        reload=True,
        log_level="info"
    )


if __name__ == "__main__":
    main()
