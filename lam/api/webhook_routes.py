#!/usr/bin/env python3
"""
Webhook API Routes
FastAPI routes for managing webhook subscriptions
"""

from typing import Dict, Any, Optional, List
from datetime import datetime
from pydantic import BaseModel, HttpUrl
from fastapi import APIRouter, HTTPException, status

from .webhook_manager import webhook_manager, WebhookSubscription


# Pydantic models
class WebhookSubscribeRequest(BaseModel):
    """Request model for webhook subscription"""
    url: HttpUrl
    events: Optional[List[str]] = None  # None or empty = all events
    secret: Optional[str] = None


class WebhookSubscriptionResponse(BaseModel):
    """Response model for webhook subscription"""
    subscription_id: str
    url: str
    events: List[str]
    active: bool
    created_at: datetime
    last_triggered: Optional[datetime]
    success_count: int
    failure_count: int


class WebhookTestRequest(BaseModel):
    """Request model for testing webhook"""
    subscription_id: str


# Router
router = APIRouter(prefix="/webhooks", tags=["webhooks"])


@router.post("/subscribe", response_model=WebhookSubscriptionResponse)
async def subscribe_webhook(request: WebhookSubscribeRequest):
    """
    Subscribe to webhook events

    Create a new webhook subscription to receive events from the LAM system.

    **Event Types**:
    - `action.started` - Action execution started
    - `action.completed` - Action execution completed
    - `action.failed` - Action execution failed
    - `resonance.drift` - Resonance field parameters drifting
    - `resonance.unstable` - System unstable
    - `resonance.stable` - System stabilized
    - `trip.planned` - Trip planning completed
    - `reservation.made` - Reservation completed
    - `food.ordered` - Food order completed
    - `question.answered` - Question answered
    - `task.completed` - Task completed
    - `satellite.updated` - Satellite data updated
    - `satellite.coverage_low` - Low coverage detected
    - `experiment.started` - Experiment started
    - `experiment.completed` - Experiment completed
    - `experiment.failed` - Experiment failed
    - `health.warning` - System health warning
    - `health.critical` - System health critical
    - `health.recovered` - System health recovered

    **Wildcard**: Use `["*"]` or omit events to receive all events.

    **Example**:
    ```json
    {
      "url": "http://localhost:1880/webhook/lam-events",
      "events": ["action.completed", "health.warning"],
      "secret": "my_secret_key"
    }
    ```
    """
    try:
        # Generate subscription ID
        subscription_id = f"sub_{datetime.now().timestamp()}"

        # Create subscription
        subscription = webhook_manager.subscribe(
            subscription_id=subscription_id,
            url=str(request.url),
            events=request.events,
            secret=request.secret
        )

        return WebhookSubscriptionResponse(
            subscription_id=subscription_id,
            url=subscription.url,
            events=list(subscription.events),
            active=subscription.active,
            created_at=subscription.created_at,
            last_triggered=subscription.last_triggered,
            success_count=subscription.success_count,
            failure_count=subscription.failure_count
        )

    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Failed to create subscription: {str(e)}"
        )


@router.delete("/subscribe/{subscription_id}")
async def unsubscribe_webhook(subscription_id: str):
    """
    Unsubscribe from webhook events

    Remove an existing webhook subscription.
    """
    success = webhook_manager.unsubscribe(subscription_id)

    if not success:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Subscription not found: {subscription_id}"
        )

    return {
        "success": True,
        "message": f"Subscription {subscription_id} removed",
        "timestamp": datetime.now().isoformat()
    }


@router.get("/subscribe/{subscription_id}", response_model=WebhookSubscriptionResponse)
async def get_webhook_subscription(subscription_id: str):
    """
    Get webhook subscription details

    Retrieve information about a specific webhook subscription.
    """
    subscription = webhook_manager.get_subscription(subscription_id)

    if not subscription:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Subscription not found: {subscription_id}"
        )

    return WebhookSubscriptionResponse(
        subscription_id=subscription_id,
        url=subscription.url,
        events=list(subscription.events),
        active=subscription.active,
        created_at=subscription.created_at,
        last_triggered=subscription.last_triggered,
        success_count=subscription.success_count,
        failure_count=subscription.failure_count
    )


@router.get("/subscribe", response_model=List[WebhookSubscriptionResponse])
async def list_webhook_subscriptions():
    """
    List all webhook subscriptions

    Retrieve all active webhook subscriptions.
    """
    subscriptions = webhook_manager.list_subscriptions()

    return [
        WebhookSubscriptionResponse(
            subscription_id=sub_id,
            url=sub.url,
            events=list(sub.events),
            active=sub.active,
            created_at=sub.created_at,
            last_triggered=sub.last_triggered,
            success_count=sub.success_count,
            failure_count=sub.failure_count
        )
        for sub_id, sub in subscriptions.items()
    ]


@router.post("/test")
async def test_webhook(request: WebhookTestRequest):
    """
    Test webhook delivery

    Send a test event to a webhook subscription to verify connectivity.
    """
    subscription = webhook_manager.get_subscription(request.subscription_id)

    if not subscription:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Subscription not found: {request.subscription_id}"
        )

    # Emit test event
    await webhook_manager.emit("test.ping", {
        "message": "This is a test webhook event",
        "subscription_id": request.subscription_id,
        "timestamp": datetime.now().isoformat()
    })

    return {
        "success": True,
        "message": f"Test event sent to {subscription.url}",
        "subscription_id": request.subscription_id,
        "timestamp": datetime.now().isoformat()
    }


@router.get("/events")
async def list_event_types():
    """
    List available webhook event types

    Returns all event types that can be subscribed to.
    """
    from .webhook_manager import EventType

    events = {
        name: value
        for name, value in vars(EventType).items()
        if not name.startswith('_')
    }

    return {
        "event_types": events,
        "description": "Subscribe to these event types using /webhooks/subscribe",
        "wildcard": "Use '*' to subscribe to all events"
    }
