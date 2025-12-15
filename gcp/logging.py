"""
Cloud Logging Client for MotorHandPro

Structured logging with:
- Severity levels
- JSON payloads
- Log correlation (trace IDs)
- Custom labels and resources

Log Types:
- application: General application logs
- telemetry: Sensor/experiment telemetry events
- audit: Security/access audit logs
- error: Error and exception tracking
"""

import logging
import traceback
from typing import Any, Dict, Optional
from datetime import datetime
import socket

logger = logging.getLogger(__name__)

try:
    from google.cloud import logging as cloud_logging
    from google.cloud.logging_v2.resource import Resource
    CLOUD_LOGGING_AVAILABLE = True
except ImportError:
    CLOUD_LOGGING_AVAILABLE = False
    logger.warning("google-cloud-logging not installed. Install with: pip install google-cloud-logging")

from . import config


class CloudLoggingClient:
    """Cloud Logging client for structured logging"""

    def __init__(self):
        self.client: Optional[cloud_logging.Client] = None
        self.logger_app: Optional[cloud_logging.Logger] = None
        self.logger_telemetry: Optional[cloud_logging.Logger] = None
        self.logger_audit: Optional[cloud_logging.Logger] = None
        self.logger_error: Optional[cloud_logging.Logger] = None

        self.hostname = socket.gethostname()

        if not config.is_gcp_enabled():
            logger.info("GCP backend disabled, Cloud Logging not initialized")
            return

        if not CLOUD_LOGGING_AVAILABLE:
            logger.error("Cloud Logging library not available")
            return

        try:
            self.client = cloud_logging.Client(project=config.project)

            # Create loggers for different log types
            self.logger_app = self.client.logger("motorhandpro-application")
            self.logger_telemetry = self.client.logger("motorhandpro-telemetry")
            self.logger_audit = self.client.logger("motorhandpro-audit")
            self.logger_error = self.client.logger("motorhandpro-error")

            logger.info("Cloud Logging client initialized")

        except Exception as e:
            logger.error(f"Failed to initialize Cloud Logging: {e}")
            self.client = None

    def is_available(self) -> bool:
        """Check if Cloud Logging is available"""
        return self.client is not None

    def log(
        self,
        message: str,
        severity: str = "INFO",
        log_type: str = "application",
        labels: Optional[Dict[str, str]] = None,
        payload: Optional[Dict[str, Any]] = None,
        trace: Optional[str] = None
    ) -> bool:
        """
        Write structured log entry

        Args:
            message: Log message
            severity: Log severity (DEBUG, INFO, WARNING, ERROR, CRITICAL)
            log_type: Log type (application, telemetry, audit, error)
            labels: Custom labels
            payload: Structured JSON payload
            trace: Trace ID for correlation

        Returns:
            Success status
        """
        if not self.is_available():
            # Fall back to standard logging
            logger.log(getattr(logging, severity), message)
            return False

        try:
            # Select logger
            log_logger = {
                'application': self.logger_app,
                'telemetry': self.logger_telemetry,
                'audit': self.logger_audit,
                'error': self.logger_error
            }.get(log_type, self.logger_app)

            # Build structured payload
            struct_payload = {
                'message': message,
                'hostname': self.hostname,
                'timestamp': datetime.utcnow().isoformat()
            }

            if payload:
                struct_payload.update(payload)

            # Add labels
            log_labels = labels or {}
            log_labels['log_type'] = log_type

            # Write log
            log_logger.log_struct(
                struct_payload,
                severity=severity,
                labels=log_labels,
                trace=trace
            )

            return True

        except Exception as e:
            logger.error(f"Failed to write log: {e}")
            return False

    def log_telemetry_event(
        self,
        event_type: str,
        data: Dict[str, Any],
        severity: str = "INFO",
        trace: Optional[str] = None
    ) -> bool:
        """
        Log telemetry event

        Args:
            event_type: Event type (sensor_reading, experiment_start, etc.)
            data: Event data
            severity: Log severity
            trace: Trace ID

        Returns:
            Success status
        """
        payload = {
            'event_type': event_type,
            **data
        }

        return self.log(
            message=f"Telemetry event: {event_type}",
            severity=severity,
            log_type='telemetry',
            payload=payload,
            trace=trace
        )

    def log_experiment_event(
        self,
        run_id: str,
        event: str,
        data: Optional[Dict[str, Any]] = None,
        severity: str = "INFO"
    ) -> bool:
        """
        Log experiment lifecycle event

        Args:
            run_id: Experiment run ID
            event: Event name (started, completed, failed, checkpoint)
            data: Additional event data
            severity: Log severity

        Returns:
            Success status
        """
        payload = {
            'run_id': run_id,
            'event': event
        }

        if data:
            payload.update(data)

        labels = {'run_id': run_id}

        return self.log(
            message=f"Experiment {event}: {run_id}",
            severity=severity,
            log_type='application',
            labels=labels,
            payload=payload,
            trace=run_id
        )

    def log_error(
        self,
        error: Exception,
        context: Optional[Dict[str, Any]] = None,
        trace: Optional[str] = None
    ) -> bool:
        """
        Log error with stack trace

        Args:
            error: Exception object
            context: Additional context
            trace: Trace ID

        Returns:
            Success status
        """
        payload = {
            'error_type': type(error).__name__,
            'error_message': str(error),
            'stack_trace': traceback.format_exc()
        }

        if context:
            payload['context'] = context

        return self.log(
            message=f"Error: {str(error)}",
            severity="ERROR",
            log_type='error',
            payload=payload,
            trace=trace
        )

    def log_audit(
        self,
        action: str,
        user: str,
        resource: str,
        success: bool,
        details: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Log audit event

        Args:
            action: Action performed (create, read, update, delete)
            user: User identifier
            resource: Resource identifier
            success: Whether action succeeded
            details: Additional details

        Returns:
            Success status
        """
        payload = {
            'action': action,
            'user': user,
            'resource': resource,
            'success': success
        }

        if details:
            payload.update(details)

        labels = {
            'user': user,
            'action': action
        }

        severity = "INFO" if success else "WARNING"

        return self.log(
            message=f"Audit: {user} {action} {resource}",
            severity=severity,
            log_type='audit',
            labels=labels,
            payload=payload
        )


# Global client instance
_client = None

def get_client() -> CloudLoggingClient:
    """Get or create Cloud Logging client singleton"""
    global _client
    if _client is None:
        _client = CloudLoggingClient()
    return _client


# Convenience functions
def log(
    message: str,
    severity: str = "INFO",
    log_type: str = "application",
    labels: Optional[Dict[str, str]] = None,
    payload: Optional[Dict[str, Any]] = None,
    trace: Optional[str] = None
) -> bool:
    """Write structured log entry"""
    return get_client().log(message, severity, log_type, labels, payload, trace)


def log_telemetry_event(
    event_type: str,
    data: Dict[str, Any],
    severity: str = "INFO",
    trace: Optional[str] = None
) -> bool:
    """Log telemetry event"""
    return get_client().log_telemetry_event(event_type, data, severity, trace)


def log_experiment_event(
    run_id: str,
    event: str,
    data: Optional[Dict[str, Any]] = None,
    severity: str = "INFO"
) -> bool:
    """Log experiment event"""
    return get_client().log_experiment_event(run_id, event, data, severity)


def log_error(
    error: Exception,
    context: Optional[Dict[str, Any]] = None,
    trace: Optional[str] = None
) -> bool:
    """Log error"""
    return get_client().log_error(error, context, trace)


def log_audit(
    action: str,
    user: str,
    resource: str,
    success: bool,
    details: Optional[Dict[str, Any]] = None
) -> bool:
    """Log audit event"""
    return get_client().log_audit(action, user, resource, success, details)
