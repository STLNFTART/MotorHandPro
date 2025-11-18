#!/usr/bin/env python3
"""
Webhook Manager for LAM API
Manages webhook subscriptions and event broadcasting to Node-RED and other systems
"""

import asyncio
import httpx
from typing import Dict, List, Any, Optional, Set
from datetime import datetime
from dataclasses import dataclass, field
import logging

logger = logging.getLogger(__name__)


@dataclass
class WebhookSubscription:
    """Represents a webhook subscription"""
    url: str
    events: Set[str] = field(default_factory=lambda: {"*"})  # "*" means all events
    secret: Optional[str] = None
    active: bool = True
    created_at: datetime = field(default_factory=datetime.now)
    last_triggered: Optional[datetime] = None
    success_count: int = 0
    failure_count: int = 0

    def matches_event(self, event_type: str) -> bool:
        """Check if this subscription matches an event type"""
        return "*" in self.events or event_type in self.events


class WebhookManager:
    """Manages webhook subscriptions and event delivery"""

    def __init__(self, max_retries: int = 3, timeout: int = 10):
        self.subscriptions: Dict[str, WebhookSubscription] = {}
        self.max_retries = max_retries
        self.timeout = timeout
        self.event_queue: asyncio.Queue = asyncio.Queue()
        self.delivery_task: Optional[asyncio.Task] = None

    async def start(self):
        """Start the webhook delivery worker"""
        if not self.delivery_task:
            self.delivery_task = asyncio.create_task(self._delivery_worker())
            logger.info("Webhook delivery worker started")

    async def stop(self):
        """Stop the webhook delivery worker"""
        if self.delivery_task:
            self.delivery_task.cancel()
            try:
                await self.delivery_task
            except asyncio.CancelledError:
                pass
            logger.info("Webhook delivery worker stopped")

    def subscribe(
        self,
        subscription_id: str,
        url: str,
        events: Optional[List[str]] = None,
        secret: Optional[str] = None
    ) -> WebhookSubscription:
        """Subscribe to webhook events"""
        subscription = WebhookSubscription(
            url=url,
            events=set(events) if events else {"*"},
            secret=secret
        )
        self.subscriptions[subscription_id] = subscription
        logger.info(f"Webhook subscription created: {subscription_id} -> {url}")
        return subscription

    def unsubscribe(self, subscription_id: str) -> bool:
        """Unsubscribe from webhook events"""
        if subscription_id in self.subscriptions:
            del self.subscriptions[subscription_id]
            logger.info(f"Webhook subscription removed: {subscription_id}")
            return True
        return False

    def get_subscription(self, subscription_id: str) -> Optional[WebhookSubscription]:
        """Get a subscription by ID"""
        return self.subscriptions.get(subscription_id)

    def list_subscriptions(self) -> Dict[str, WebhookSubscription]:
        """List all subscriptions"""
        return self.subscriptions.copy()

    async def emit(self, event_type: str, data: Dict[str, Any]):
        """Emit an event to all matching subscribers"""
        event = {
            "type": event_type,
            "data": data,
            "timestamp": datetime.now().isoformat()
        }

        # Add to queue for async delivery
        await self.event_queue.put(event)
        logger.debug(f"Event queued: {event_type}")

    async def _delivery_worker(self):
        """Background worker for delivering webhook events"""
        async with httpx.AsyncClient(timeout=self.timeout) as client:
            while True:
                try:
                    event = await self.event_queue.get()
                    event_type = event["type"]

                    # Find matching subscriptions
                    tasks = []
                    for sub_id, sub in self.subscriptions.items():
                        if sub.active and sub.matches_event(event_type):
                            task = self._deliver_event(client, sub_id, sub, event)
                            tasks.append(task)

                    # Deliver to all subscribers concurrently
                    if tasks:
                        await asyncio.gather(*tasks, return_exceptions=True)

                    self.event_queue.task_done()

                except asyncio.CancelledError:
                    break
                except Exception as e:
                    logger.error(f"Error in delivery worker: {e}")

    async def _deliver_event(
        self,
        client: httpx.AsyncClient,
        sub_id: str,
        subscription: WebhookSubscription,
        event: Dict[str, Any]
    ):
        """Deliver an event to a specific webhook"""
        headers = {"Content-Type": "application/json"}

        # Add secret if configured
        if subscription.secret:
            headers["X-Webhook-Secret"] = subscription.secret

        # Retry logic
        for attempt in range(self.max_retries):
            try:
                response = await client.post(
                    subscription.url,
                    json=event,
                    headers=headers
                )

                if response.status_code < 400:
                    subscription.success_count += 1
                    subscription.last_triggered = datetime.now()
                    logger.debug(f"Webhook delivered: {sub_id} -> {subscription.url}")
                    return
                else:
                    logger.warning(
                        f"Webhook delivery failed (HTTP {response.status_code}): "
                        f"{sub_id} -> {subscription.url}"
                    )

            except Exception as e:
                logger.error(
                    f"Webhook delivery error (attempt {attempt + 1}/{self.max_retries}): "
                    f"{sub_id} -> {subscription.url}: {e}"
                )

                if attempt < self.max_retries - 1:
                    await asyncio.sleep(2 ** attempt)  # Exponential backoff

        # All retries failed
        subscription.failure_count += 1

        # Disable subscription after too many failures
        if subscription.failure_count > 10:
            subscription.active = False
            logger.error(f"Webhook disabled due to failures: {sub_id}")


# Global webhook manager instance
webhook_manager = WebhookManager()


# Event type constants
class EventType:
    """Webhook event types"""
    ACTION_STARTED = "action.started"
    ACTION_COMPLETED = "action.completed"
    ACTION_FAILED = "action.failed"

    RESONANCE_DRIFT = "resonance.drift"
    RESONANCE_UNSTABLE = "resonance.unstable"
    RESONANCE_STABLE = "resonance.stable"

    TRIP_PLANNED = "trip.planned"
    RESERVATION_MADE = "reservation.made"
    FOOD_ORDERED = "food.ordered"
    QUESTION_ANSWERED = "question.answered"
    TASK_COMPLETED = "task.completed"

    SATELLITE_UPDATED = "satellite.updated"
    SATELLITE_COVERAGE_LOW = "satellite.coverage_low"

    EXPERIMENT_STARTED = "experiment.started"
    EXPERIMENT_COMPLETED = "experiment.completed"
    EXPERIMENT_FAILED = "experiment.failed"

    HEALTH_WARNING = "health.warning"
    HEALTH_CRITICAL = "health.critical"
    HEALTH_RECOVERED = "health.recovered"


async def emit_action_event(
    action_type: str,
    status: str,
    data: Dict[str, Any],
    resonance_state: Optional[Dict[str, Any]] = None
):
    """Helper to emit action-related events"""
    event_type = {
        "started": EventType.ACTION_STARTED,
        "completed": EventType.ACTION_COMPLETED,
        "failed": EventType.ACTION_FAILED
    }.get(status, EventType.ACTION_COMPLETED)

    payload = {
        "action_type": action_type,
        "status": status,
        **data
    }

    if resonance_state:
        payload["resonance_state"] = resonance_state

    await webhook_manager.emit(event_type, payload)


async def emit_health_event(
    severity: str,
    health_score: float,
    resonance_state: Dict[str, Any],
    message: str
):
    """Helper to emit health-related events"""
    event_type = {
        "warning": EventType.HEALTH_WARNING,
        "critical": EventType.HEALTH_CRITICAL,
        "recovered": EventType.HEALTH_RECOVERED
    }.get(severity, EventType.HEALTH_WARNING)

    await webhook_manager.emit(event_type, {
        "severity": severity,
        "health_score": health_score,
        "resonance_state": resonance_state,
        "message": message
    })


async def emit_experiment_event(
    event_type: str,
    experiment_id: str,
    data: Dict[str, Any]
):
    """Helper to emit experiment-related events"""
    await webhook_manager.emit(event_type, {
        "experiment_id": experiment_id,
        **data
    })
