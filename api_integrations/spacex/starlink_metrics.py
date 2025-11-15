"""
Starlink Public Metrics API Client.

Provides access to public Starlink network performance metrics.
Data Source: https://api.starlink.com/public-files/
"""

from typing import Optional, Dict, Any
from ..base_client import BaseAPIClient, APIConfig


class StarlinkMetricsClient(BaseAPIClient):
    """Client for Starlink public metrics."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize Starlink metrics client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = self.config.starlink_metrics_url

    def get_residential_metrics(self) -> Dict[str, Any]:
        """
        Get residential service metrics.

        Returns:
            Dict containing:
                - latency_ms: Latency measurements by region
                - download_mbps: Download speed measurements
                - upload_mbps: Upload speed measurements
                - timestamp: Data timestamp
                - regions: Geographic breakdown
        """
        url = f"{self.base_endpoint}/metrics_residential.json"
        return self.get(url)

    def get_maritime_metrics(self) -> Dict[str, Any]:
        """
        Get maritime service metrics.

        Returns:
            Dict containing maritime Starlink performance data
        """
        url = f"{self.base_endpoint}/metrics_maritime.json"
        return self.get(url)

    def get_aviation_metrics(self) -> Dict[str, Any]:
        """
        Get aviation service metrics (if available).

        Returns:
            Dict containing aviation Starlink performance data
        """
        url = f"{self.base_endpoint}/metrics_aviation.json"
        return self.get(url)

    def get_all_metrics(self) -> Dict[str, Dict[str, Any]]:
        """
        Get all available Starlink metrics.

        Returns:
            Dict with keys for each service type:
                - residential: Residential metrics
                - maritime: Maritime metrics
                - aviation: Aviation metrics (if available)
        """
        metrics = {}

        try:
            metrics["residential"] = self.get_residential_metrics()
        except Exception as e:
            metrics["residential"] = {"error": str(e)}

        try:
            metrics["maritime"] = self.get_maritime_metrics()
        except Exception as e:
            metrics["maritime"] = {"error": str(e)}

        try:
            metrics["aviation"] = self.get_aviation_metrics()
        except Exception as e:
            metrics["aviation"] = {"error": str(e)}

        return metrics

    def analyze_latency(
        self,
        metrics: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Analyze latency data from metrics.

        Args:
            metrics: Metrics dict (fetches if None)

        Returns:
            Dict with latency analysis:
                - avg_latency_ms: Average latency
                - min_latency_ms: Minimum latency
                - max_latency_ms: Maximum latency
                - regions: Per-region breakdown
        """
        if metrics is None:
            metrics = self.get_residential_metrics()

        # Extract and analyze latency data
        # (Structure depends on actual API response format)
        analysis = {
            "raw_data": metrics,
            "summary": "Latency analysis placeholder"
        }

        return analysis

    def analyze_throughput(
        self,
        metrics: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Analyze download/upload throughput data.

        Args:
            metrics: Metrics dict (fetches if None)

        Returns:
            Dict with throughput analysis:
                - avg_download_mbps: Average download speed
                - avg_upload_mbps: Average upload speed
                - regions: Per-region breakdown
        """
        if metrics is None:
            metrics = self.get_residential_metrics()

        # Extract and analyze throughput data
        analysis = {
            "raw_data": metrics,
            "summary": "Throughput analysis placeholder"
        }

        return analysis
