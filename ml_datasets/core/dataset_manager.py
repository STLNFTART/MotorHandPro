"""
MotorHandPro ML Dataset Manager
Central orchestration for on-demand dataset loading and management
"""

import asyncio
import logging
from pathlib import Path
from typing import Dict, List, Optional, Any
from datetime import datetime, timedelta
import yaml
import hashlib
import json

from ml_datasets.core.dataset_loader import DatasetLoader
from ml_datasets.core.data_validator import DataValidator
from ml_datasets.sources.physionet_connector import PhysioNetConnector
from ml_datasets.sources.ncbi_connector import NCBIConnector
from ml_datasets.sources.university_connectors import UniversityHubConnector
from ml_datasets.sources.hgp_connector import HumanGenomeConnector
from ml_datasets.storage.dataset_repository import DatasetRepository
from ml_datasets.storage.metadata_store import MetadataStore

logger = logging.getLogger(__name__)


class DatasetManager:
    """
    Central manager for ML dataset operations integrated with LAM framework.

    Provides:
    - On-demand dataset loading with intelligent caching
    - Quality validation and diversity metrics
    - Integration with multiple data sources
    - LAM quantum resonance scoring for dataset selection
    """

    def __init__(
        self,
        catalog_path: str = "/home/user/MotorHandPro/ml_datasets/config/dataset_catalog.yaml",
        cache_dir: str = "./ml_datasets/cache",
        db_url: Optional[str] = None
    ):
        """
        Initialize the Dataset Manager.

        Args:
            catalog_path: Path to dataset catalog YAML
            cache_dir: Directory for cached datasets
            db_url: PostgreSQL connection URL (uses env var if None)
        """
        self.catalog_path = Path(catalog_path)
        self.cache_dir = Path(cache_dir)
        self.cache_dir.mkdir(parents=True, exist_ok=True)

        # Load catalog
        self.catalog = self._load_catalog()

        # Initialize components
        self.loader = DatasetLoader(cache_dir=cache_dir)
        self.validator = DataValidator()
        self.repository = DatasetRepository(cache_dir=cache_dir)
        self.metadata_store = MetadataStore(db_url=db_url)

        # Initialize connectors
        self.connectors = {
            'physionet': PhysioNetConnector(),
            'ncbi': NCBIConnector(),
            'university_hub': UniversityHubConnector(),
            'hgp': HumanGenomeConnector()
        }

        logger.info(f"DatasetManager initialized with {len(self._get_all_datasets())} datasets")

    def _load_catalog(self) -> Dict[str, Any]:
        """Load dataset catalog from YAML file."""
        with open(self.catalog_path, 'r') as f:
            return yaml.safe_load(f)

    def _get_all_datasets(self) -> List[Dict]:
        """Get all datasets from catalog."""
        datasets = []
        for category in ['cardiovascular', 'neurological', 'muscular', 'genomic', 'multimodal', 'specialized']:
            if category in self.catalog:
                datasets.extend(self.catalog[category])
        return datasets

    async def load_dataset(
        self,
        dataset_id: str,
        version: Optional[str] = None,
        filters: Optional[Dict] = None,
        force_refresh: bool = False
    ) -> 'Dataset':
        """
        Load a dataset on-demand with intelligent caching.

        Args:
            dataset_id: Unique dataset identifier from catalog
            version: Specific version to load (None = latest)
            filters: Optional filters to apply (e.g., record_range, subjects)
            force_refresh: Force download even if cached

        Returns:
            Dataset object with data, metadata, and quality metrics

        Example:
            >>> dm = DatasetManager()
            >>> ecg_data = await dm.load_dataset(
            ...     dataset_id="mitdb",
            ...     filters={"record_range": [100, 109]}
            ... )
            >>> print(ecg_data.metadata['records'])
        """
        logger.info(f"Loading dataset: {dataset_id} (version={version}, force_refresh={force_refresh})")

        # Find dataset in catalog
        dataset_config = self._find_dataset_config(dataset_id)
        if not dataset_config:
            raise ValueError(f"Dataset '{dataset_id}' not found in catalog")

        # Check cache first
        cache_key = self._generate_cache_key(dataset_id, version, filters)

        if not force_refresh:
            cached_dataset = await self.repository.get_cached(cache_key)
            if cached_dataset:
                logger.info(f"Dataset {dataset_id} loaded from cache")
                return cached_dataset

        # Determine connector
        connector = self._get_connector(dataset_config)

        # Download dataset
        raw_data = await connector.download(
            dataset_id=dataset_id,
            version=version or dataset_config.get('version'),
            filters=filters
        )

        # Validate data quality
        validation_result = await self.validator.validate(
            data=raw_data,
            dataset_config=dataset_config
        )

        # Create Dataset object
        dataset = Dataset(
            id=dataset_id,
            version=version or dataset_config.get('version'),
            data=raw_data,
            metadata=dataset_config,
            quality_metrics=validation_result.metrics,
            loaded_at=datetime.now()
        )

        # Cache dataset
        await self.repository.cache(cache_key, dataset)

        # Store metadata
        await self.metadata_store.record_load(
            dataset_id=dataset_id,
            version=dataset.version,
            quality_metrics=validation_result.metrics,
            filters=filters
        )

        logger.info(f"Dataset {dataset_id} loaded successfully (quality={validation_result.overall_score:.2f})")
        return dataset

    def _find_dataset_config(self, dataset_id: str) -> Optional[Dict]:
        """Find dataset configuration by ID."""
        for dataset in self._get_all_datasets():
            if dataset.get('id') == dataset_id:
                return dataset
        return None

    def _get_connector(self, dataset_config: Dict):
        """Get appropriate connector for dataset source."""
        source = dataset_config.get('source', 'physionet')

        if source in ['physionet']:
            return self.connectors['physionet']
        elif source in ['ncbi', 'international_genome']:
            return self.connectors['ncbi']
        elif 'university' in source or dataset_config.get('institution'):
            return self.connectors['university_hub']
        elif 'genome' in dataset_config.get('name', '').lower():
            return self.connectors['hgp']
        else:
            return self.loader  # Generic loader

    def _generate_cache_key(self, dataset_id: str, version: Optional[str], filters: Optional[Dict]) -> str:
        """Generate unique cache key for dataset configuration."""
        key_data = {
            'dataset_id': dataset_id,
            'version': version,
            'filters': filters or {}
        }
        key_string = json.dumps(key_data, sort_keys=True)
        return hashlib.sha256(key_string.encode()).hexdigest()

    async def search_datasets(
        self,
        query: str,
        data_types: Optional[List[str]] = None,
        min_quality: float = 0.75,
        max_results: int = 10
    ) -> List[Dict]:
        """
        Search datasets by keywords and filters.

        Args:
            query: Search query (searches name, description, features)
            data_types: Filter by data type (e.g., ['ecg', 'eeg'])
            min_quality: Minimum quality score threshold
            max_results: Maximum number of results

        Returns:
            List of matching dataset configurations with relevance scores
        """
        results = []
        query_lower = query.lower()

        for dataset in self._get_all_datasets():
            # Calculate relevance score
            score = 0.0

            # Check name
            if query_lower in dataset.get('name', '').lower():
                score += 3.0

            # Check description
            if query_lower in dataset.get('description', '').lower():
                score += 2.0

            # Check features
            features = dataset.get('features', [])
            for feature in features:
                if query_lower in str(feature).lower():
                    score += 1.0

            # Filter by data type
            if data_types and dataset.get('data_type') not in data_types:
                continue

            # Filter by quality
            quality = dataset.get('quality_metrics', {}).get('completeness', 0)
            if quality < min_quality:
                continue

            if score > 0:
                results.append({
                    'dataset': dataset,
                    'relevance_score': score
                })

        # Sort by relevance and return top results
        results.sort(key=lambda x: x['relevance_score'], reverse=True)
        return results[:max_results]

    async def get_dataset_info(self, dataset_id: str) -> Dict:
        """Get detailed information about a dataset without loading it."""
        config = self._find_dataset_config(dataset_id)
        if not config:
            raise ValueError(f"Dataset '{dataset_id}' not found")

        # Add usage statistics from metadata store
        usage_stats = await self.metadata_store.get_usage_stats(dataset_id)

        return {
            **config,
            'usage_stats': usage_stats
        }

    async def list_datasets(
        self,
        category: Optional[str] = None,
        data_type: Optional[str] = None
    ) -> List[Dict]:
        """
        List available datasets with optional filtering.

        Args:
            category: Filter by category (cardiovascular, neurological, etc.)
            data_type: Filter by data type (ecg, eeg, emg, etc.)

        Returns:
            List of dataset configurations
        """
        datasets = []

        if category and category in self.catalog:
            datasets = self.catalog[category]
        else:
            datasets = self._get_all_datasets()

        if data_type:
            datasets = [d for d in datasets if d.get('data_type') == data_type]

        return datasets

    async def validate_dataset(self, dataset_id: str) -> Dict:
        """
        Run comprehensive validation on a dataset.

        Returns quality metrics, diversity scores, and validation report.
        """
        dataset = await self.load_dataset(dataset_id)
        return {
            'dataset_id': dataset_id,
            'quality_metrics': dataset.quality_metrics,
            'validation_timestamp': datetime.now().isoformat(),
            'passed_validation': all(
                v >= 0.75 for v in dataset.quality_metrics.values() if isinstance(v, (int, float))
            )
        }

    async def get_diversity_score(self, dataset_ids: List[str]) -> float:
        """
        Calculate diversity score across multiple datasets.

        Considers:
        - Data type diversity
        - Temporal diversity (collection dates)
        - Population diversity (subjects, demographics)
        - Signal diversity (sample rates, channels)

        Returns:
            Diversity score from 0.0 to 1.0
        """
        if not dataset_ids:
            return 0.0

        configs = [self._find_dataset_config(id) for id in dataset_ids]
        configs = [c for c in configs if c is not None]

        if not configs:
            return 0.0

        # Calculate diversity components
        data_types = set(c.get('data_type') for c in configs)
        sources = set(c.get('source') for c in configs)

        # Normalize to 0-1 scale
        type_diversity = len(data_types) / 10  # Assume max 10 types
        source_diversity = len(sources) / 5    # Assume max 5 sources

        # Weighted average
        diversity_score = (type_diversity * 0.6 + source_diversity * 0.4)
        return min(diversity_score, 1.0)

    async def cleanup_cache(self, older_than_days: int = 30) -> int:
        """
        Clean up cached datasets older than specified days.

        Returns:
            Number of datasets removed
        """
        cutoff_date = datetime.now() - timedelta(days=older_than_days)
        removed_count = await self.repository.cleanup_old_cache(cutoff_date)
        logger.info(f"Cleaned up {removed_count} cached datasets older than {older_than_days} days")
        return removed_count


class Dataset:
    """
    Dataset container with data, metadata, and quality metrics.
    """

    def __init__(
        self,
        id: str,
        version: str,
        data: Any,
        metadata: Dict,
        quality_metrics: Dict,
        loaded_at: datetime
    ):
        self.id = id
        self.version = version
        self.data = data
        self.metadata = metadata
        self.quality_metrics = quality_metrics
        self.loaded_at = loaded_at

    @property
    def signals(self):
        """Access signal data (for time-series datasets)."""
        if isinstance(self.data, dict):
            return self.data.get('signals')
        return self.data

    @property
    def annotations(self):
        """Access annotations/labels."""
        if isinstance(self.data, dict):
            return self.data.get('annotations')
        return None

    @property
    def info(self) -> Dict:
        """Get dataset information summary."""
        return {
            'id': self.id,
            'version': self.version,
            'name': self.metadata.get('name'),
            'data_type': self.metadata.get('data_type'),
            'quality_score': self.quality_metrics.get('completeness', 0),
            'loaded_at': self.loaded_at.isoformat()
        }

    def __repr__(self):
        return f"Dataset(id='{self.id}', version='{self.version}', quality={self.quality_metrics.get('completeness', 0):.2f})"


# Example usage
if __name__ == "__main__":
    async def main():
        # Initialize manager
        dm = DatasetManager()

        # Search for ECG datasets
        ecg_datasets = await dm.search_datasets(
            query="heart arrhythmia",
            data_types=['ecg'],
            min_quality=0.9
        )
        print(f"Found {len(ecg_datasets)} ECG datasets")

        # Load MIT-BIH dataset
        mitdb = await dm.load_dataset(
            dataset_id="mitdb",
            filters={"record_range": [100, 109]}
        )
        print(f"Loaded {mitdb.metadata['name']}")
        print(f"Quality: {mitdb.quality_metrics}")

        # Calculate diversity
        diversity = await dm.get_diversity_score(['mitdb', 'ptbdb', 'sleep_edf'])
        print(f"Dataset diversity score: {diversity:.2f}")

    asyncio.run(main())
