"""
API Integration Framework for MotorHandPro LAM
Provides base classes and utilities for integrating with 100+ external APIs
"""

from typing import Dict, Any, Optional, List
from abc import ABC, abstractmethod
import httpx
import json
from datetime import datetime
import logging

logger = logging.getLogger(__name__)


class APICredentials:
    """Secure credential storage for API integrations"""

    def __init__(self):
        self.credentials: Dict[str, Dict[str, str]] = {}

    def set(self, service: str, **kwargs):
        """Store credentials for a service"""
        self.credentials[service] = kwargs

    def get(self, service: str) -> Optional[Dict[str, str]]:
        """Retrieve credentials for a service"""
        return self.credentials.get(service)

    def has(self, service: str) -> bool:
        """Check if credentials exist for a service"""
        return service in self.credentials


class BaseAPIExecutor(ABC):
    """Base class for all API executors"""

    def __init__(self, credentials: Optional[Dict[str, str]] = None):
        self.credentials = credentials or {}
        self.client = httpx.AsyncClient(timeout=30.0)
        self.execution_history: List[Dict[str, Any]] = []

    @property
    @abstractmethod
    def service_name(self) -> str:
        """Name of the API service"""
        pass

    @property
    @abstractmethod
    def base_url(self) -> str:
        """Base URL for API requests"""
        pass

    def _get_headers(self) -> Dict[str, str]:
        """Get common headers for API requests"""
        return {"Content-Type": "application/json"}

    def _record_execution(
        self,
        operation: str,
        params: Dict[str, Any],
        result: Any,
        success: bool = True,
        error: Optional[str] = None
    ):
        """Record API execution for history/debugging"""
        self.execution_history.append({
            "service": self.service_name,
            "operation": operation,
            "params": params,
            "result": result,
            "success": success,
            "error": error,
            "timestamp": datetime.now().isoformat()
        })

    async def close(self):
        """Close the HTTP client"""
        await self.client.aclose()

    @abstractmethod
    async def execute(self, operation: str, **kwargs) -> Dict[str, Any]:
        """Execute an API operation"""
        pass


class RESTAPIExecutor(BaseAPIExecutor):
    """Base class for REST API integrations"""

    async def _request(
        self,
        method: str,
        endpoint: str,
        params: Optional[Dict] = None,
        json_data: Optional[Dict] = None,
        headers: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """Make a REST API request"""
        url = f"{self.base_url}/{endpoint.lstrip('/')}"
        request_headers = {**self._get_headers(), **(headers or {})}

        try:
            response = await self.client.request(
                method=method,
                url=url,
                params=params,
                json=json_data,
                headers=request_headers
            )

            response.raise_for_status()
            return response.json() if response.text else {}

        except httpx.HTTPStatusError as e:
            logger.error(f"{self.service_name} API error: {e}")
            raise
        except Exception as e:
            logger.error(f"{self.service_name} request failed: {e}")
            raise

    async def get(self, endpoint: str, **params) -> Dict[str, Any]:
        """GET request"""
        return await self._request("GET", endpoint, params=params)

    async def post(self, endpoint: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """POST request"""
        return await self._request("POST", endpoint, json_data=data)

    async def put(self, endpoint: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """PUT request"""
        return await self._request("PUT", endpoint, json_data=data)

    async def delete(self, endpoint: str) -> Dict[str, Any]:
        """DELETE request"""
        return await self._request("DELETE", endpoint)


class WebhookReceiver(ABC):
    """Base class for webhook receivers"""

    @property
    @abstractmethod
    def service_name(self) -> str:
        """Name of the service sending webhooks"""
        pass

    @abstractmethod
    async def verify_signature(self, payload: bytes, signature: str) -> bool:
        """Verify webhook signature for security"""
        pass

    @abstractmethod
    async def process_event(self, event_type: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Process incoming webhook event"""
        pass


# API Registry
class APIRegistry:
    """Global registry of available API executors"""

    def __init__(self):
        self._executors: Dict[str, type] = {}
        self._instances: Dict[str, BaseAPIExecutor] = {}

    def register(self, executor_class: type):
        """Register an API executor class"""
        if not issubclass(executor_class, BaseAPIExecutor):
            raise ValueError("Executor must inherit from BaseAPIExecutor")

        # Get service name from an instance
        temp_instance = executor_class()
        self._executors[temp_instance.service_name] = executor_class
        return executor_class

    def get_executor(
        self,
        service_name: str,
        credentials: Optional[Dict[str, str]] = None
    ) -> Optional[BaseAPIExecutor]:
        """Get or create an executor instance"""
        if service_name in self._instances:
            return self._instances[service_name]

        if service_name in self._executors:
            executor = self._executors[service_name](credentials=credentials)
            self._instances[service_name] = executor
            return executor

        return None

    def list_services(self) -> List[str]:
        """List all registered services"""
        return list(self._executors.keys())


# Global registry instance
api_registry = APIRegistry()
