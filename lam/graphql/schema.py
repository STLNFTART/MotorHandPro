#!/usr/bin/env python3
"""
LAM GraphQL API Schema
Alternative to REST API with flexible queries
"""
import strawberry
from typing import List, Optional
from datetime import datetime


@strawberry.type
class User:
    id: int
    username: str
    email: Optional[str]
    full_name: Optional[str]
    roles: List[str]


@strawberry.type
class Action:
    id: int
    action_type: str
    success: bool
    duration_ms: float
    created_at: datetime


@strawberry.type
class ResonanceState:
    alpha: float
    lambda_param: float
    lightfoot_constant: float
    donte_attractor: float
    lipschitz_constant: float
    stable: bool
    timestamp: datetime


@strawberry.type
class TripPlan:
    destination: str
    departure_date: str
    return_date: str
    budget: Optional[float]
    estimated_cost: float


@strawberry.input
class TripPlanInput:
    destination: str
    departure_date: str
    return_date: str
    budget: Optional[float] = None


@strawberry.type
class Query:
    @strawberry.field
    def status(self) -> str:
        return "LAM GraphQL API - Online"

    @strawberry.field
    def resonance_state(self) -> ResonanceState:
        """Get current quantum resonance field state"""
        return ResonanceState(
            alpha=0.54,
            lambda_param=0.115,
            lightfoot_constant=0.16905,
            donte_attractor=149.9992314000,
            lipschitz_constant=0.000129932,
            stable=True,
            timestamp=datetime.now()
        )

    @strawberry.field
    def user(self, id: int) -> Optional[User]:
        """Get user by ID"""
        # Mock data - replace with database query
        return User(
            id=id,
            username="admin",
            email="admin@lam.local",
            full_name="Admin User",
            roles=["admin", "user"]
        )

    @strawberry.field
    def actions(self, limit: int = 10) -> List[Action]:
        """Get recent actions"""
        # Mock data - replace with database query
        return [
            Action(
                id=i,
                action_type="trip_planning",
                success=True,
                duration_ms=150.5,
                created_at=datetime.now()
            )
            for i in range(limit)
        ]


@strawberry.type
class Mutation:
    @strawberry.mutation
    def plan_trip(self, input: TripPlanInput) -> TripPlan:
        """Plan a trip"""
        # Implement trip planning logic
        return TripPlan(
            destination=input.destination,
            departure_date=input.departure_date,
            return_date=input.return_date,
            budget=input.budget,
            estimated_cost=2500.00
        )


schema = strawberry.Schema(query=Query, mutation=Mutation)
