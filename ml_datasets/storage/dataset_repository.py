"""
Dataset Repository
Local caching and storage management for datasets
"""

import logging
from typing import Optional, Any
from datetime import datetime
from pathlib import Path
import pickle

logger = logging.getLogger(__name__)


class DatasetRepository:
    """Manages local dataset cache."""

    def __init__(self, cache_dir: str):
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)

    async def get_cached(self, cache_key: str) -> Optional[Any]:
        """Retrieve cached dataset."""
        cache_file = self.cache_dir / f"{cache_key}.pkl"

        if cache_file.exists():
            logger.info(f"Cache hit: {cache_key}")
            with open(cache_file, 'rb') as f:
                return pickle.load(f)

        return None

    async def cache(self, cache_key: str, dataset: Any):
        """Cache dataset locally."""
        cache_file = self.cache_dir / f"{cache_key}.pkl"

        with open(cache_file, 'wb') as f:
            pickle.dump(dataset, f)

        logger.info(f"Cached dataset: {cache_key}")

    async def cleanup_old_cache(self, cutoff_date: datetime) -> int:
        """Remove old cache files."""
        removed = 0
        for cache_file in self.cache_dir.glob("*.pkl"):
            if datetime.fromtimestamp(cache_file.stat().st_mtime) < cutoff_date:
                cache_file.unlink()
                removed += 1

        return removed
