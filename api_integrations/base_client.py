"""
Base API client with common functionality for all API integrations.

Provides retry logic, error handling, and rate limiting.
"""

import time
import requests
from typing import Dict, Any, Optional
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
import logging

from .config import APIConfig

logger = logging.getLogger(__name__)


class APIError(Exception):
    """Base exception for API errors."""
    pass


class RateLimitError(APIError):
    """Raised when rate limit is exceeded."""
    pass


class BaseAPIClient:
    """Base class for all API clients."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize base API client.

        Args:
            config: API configuration instance (uses global config if None)
        """
        from .config import config as default_config
        self.config = config or default_config
        self.session = self._create_session()
        self._last_request_time = 0.0

    def _create_session(self) -> requests.Session:
        """
        Create requests session with retry logic.

        Returns:
            Configured requests.Session instance
        """
        session = requests.Session()

        retry_strategy = Retry(
            total=self.config.retry_attempts,
            backoff_factor=self.config.retry_delay,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["HEAD", "GET", "OPTIONS"]
        )

        adapter = HTTPAdapter(max_retries=retry_strategy)
        session.mount("http://", adapter)
        session.mount("https://", adapter)

        return session

    def _rate_limit_wait(self):
        """Implement basic rate limiting."""
        if not self.config.rate_limit_enabled:
            return

        min_interval = 3600.0 / self.config.requests_per_hour
        elapsed = time.time() - self._last_request_time

        if elapsed < min_interval:
            time.sleep(min_interval - elapsed)

        self._last_request_time = time.time()

    def _request(
        self,
        method: str,
        url: str,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Make HTTP request with error handling.

        Args:
            method: HTTP method (GET, POST, etc.)
            url: Request URL
            params: Query parameters
            headers: Request headers
            **kwargs: Additional arguments for requests

        Returns:
            Response JSON data

        Raises:
            APIError: On request failure
            RateLimitError: On rate limit exceeded
        """
        self._rate_limit_wait()

        try:
            response = self.session.request(
                method=method,
                url=url,
                params=params,
                headers=headers,
                timeout=self.config.timeout,
                **kwargs
            )

            if response.status_code == 429:
                raise RateLimitError("Rate limit exceeded")

            response.raise_for_status()

            # Some endpoints return non-JSON responses
            content_type = response.headers.get('Content-Type', '')
            if 'application/json' in content_type:
                return response.json()
            else:
                return {'content': response.text, 'url': response.url}

        except requests.exceptions.RequestException as e:
            logger.error(f"API request failed: {e}")
            raise APIError(f"Request failed: {e}") from e

    def get(
        self,
        url: str,
        params: Optional[Dict[str, Any]] = None,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Make GET request.

        Args:
            url: Request URL
            params: Query parameters
            **kwargs: Additional arguments

        Returns:
            Response JSON data
        """
        return self._request("GET", url, params=params, **kwargs)

    def post(
        self,
        url: str,
        data: Optional[Dict[str, Any]] = None,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Make POST request.

        Args:
            url: Request URL
            data: Request data
            **kwargs: Additional arguments

        Returns:
            Response JSON data
        """
        return self._request("POST", url, json=data, **kwargs)
