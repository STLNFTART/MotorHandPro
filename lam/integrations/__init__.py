"""
MotorHandPro LAM Integrations
100+ API integrations for universal connectivity
"""

from .api_framework import (
    BaseAPIExecutor,
    RESTAPIExecutor,
    WebhookReceiver,
    APIRegistry,
    api_registry
)

# Import all implemented executors
from .communication_apis import (
    SlackExecutor,
    DiscordExecutor,
    TelegramExecutor,
    TwilioExecutor,
    SendGridExecutor,
    MailchimpExecutor
)

__all__ = [
    # Framework
    "BaseAPIExecutor",
    "RESTAPIExecutor",
    "WebhookReceiver",
    "APIRegistry",
    "api_registry",

    # Communication APIs
    "SlackExecutor",
    "DiscordExecutor",
    "TelegramExecutor",
    "TwilioExecutor",
    "SendGridExecutor",
    "MailchimpExecutor",
]

# Quick access helper
def get_api(service_name: str, credentials: dict = None):
    """Quick access to API executors"""
    return api_registry.get_executor(service_name, credentials)


# List all available APIs
def list_apis():
    """List all registered API services"""
    return api_registry.list_services()
