"""
NASA Image and Video Library API Client.

Provides access to NASA's media library with over 140,000 assets.
API Documentation: https://images.nasa.gov/docs/images.nasa.gov_api_docs.pdf
"""

from typing import Optional, Dict, Any, List
from ..base_client import BaseAPIClient, APIConfig


class ImageLibraryClient(BaseAPIClient):
    """Client for NASA Image and Video Library API."""

    def __init__(self, config: Optional[APIConfig] = None):
        """
        Initialize Image Library client.

        Args:
            config: API configuration (uses global config if None)
        """
        super().__init__(config)
        self.base_endpoint = "https://images-api.nasa.gov"

    def search(
        self,
        query: str,
        media_type: Optional[str] = None,
        year_start: Optional[int] = None,
        year_end: Optional[int] = None,
        center: Optional[str] = None,
        keywords: Optional[List[str]] = None,
        page: int = 1,
        page_size: int = 100
    ) -> Dict[str, Any]:
        """
        Search NASA's image and video library.

        Args:
            query: Search query string
            media_type: Filter by type (image/video/audio)
            year_start: Filter by start year
            year_end: Filter by end year
            center: Filter by NASA center (e.g., "JPL", "GSFC")
            keywords: List of keywords to filter by
            page: Page number (1-indexed)
            page_size: Results per page (max 100)

        Returns:
            Dict containing:
                - collection: Search results
                    - items: List of media items
                    - metadata: Result metadata
                    - links: Pagination links
        """
        params = {
            "q": query,
            "page": page,
            "page_size": min(page_size, 100)
        }

        if media_type:
            params["media_type"] = media_type
        if year_start:
            params["year_start"] = year_start
        if year_end:
            params["year_end"] = year_end
        if center:
            params["center"] = center
        if keywords:
            params["keywords"] = ",".join(keywords)

        return self.get(f"{self.base_endpoint}/search", params=params)

    def get_asset(self, nasa_id: str) -> Dict[str, Any]:
        """
        Get asset manifest for a specific NASA ID.

        Args:
            nasa_id: NASA asset identifier

        Returns:
            Dict containing collection of asset URLs and metadata
        """
        return self.get(f"{self.base_endpoint}/asset/{nasa_id}")

    def get_metadata(self, nasa_id: str) -> Dict[str, Any]:
        """
        Get metadata for a specific NASA ID.

        Args:
            nasa_id: NASA asset identifier

        Returns:
            Dict containing detailed metadata
        """
        return self.get(f"{self.base_endpoint}/metadata/{nasa_id}")

    def get_captions(self, nasa_id: str) -> Dict[str, Any]:
        """
        Get video captions/subtitles (if available).

        Args:
            nasa_id: NASA video identifier

        Returns:
            Dict containing caption data
        """
        return self.get(f"{self.base_endpoint}/captions/{nasa_id}")

    def search_videos(
        self,
        query: str,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Convenience method to search for videos only.

        Args:
            query: Search query
            **kwargs: Additional search parameters

        Returns:
            Dict with video search results
        """
        return self.search(query, media_type="video", **kwargs)

    def search_images(
        self,
        query: str,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Convenience method to search for images only.

        Args:
            query: Search query
            **kwargs: Additional search parameters

        Returns:
            Dict with image search results
        """
        return self.search(query, media_type="image", **kwargs)

    def search_audio(
        self,
        query: str,
        **kwargs
    ) -> Dict[str, Any]:
        """
        Convenience method to search for audio only.

        Args:
            query: Search query
            **kwargs: Additional search parameters

        Returns:
            Dict with audio search results
        """
        return self.search(query, media_type="audio", **kwargs)
