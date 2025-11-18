"""
Kaggle & UCI ML Repository Connector
Acts as internal data concierge for discovering and accessing datasets

Integrates:
- Kaggle Datasets API (50K+ datasets)
- UC Irvine Machine Learning Repository (600+ datasets)
- Aggregate search across multiple repositories
- Unified dataset discovery interface
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime
from enum import Enum
from dataclasses import dataclass
import aiohttp
import os

logger = logging.getLogger(__name__)


class DatasetRepository(Enum):
    """Dataset repository sources."""
    KAGGLE = "kaggle"
    UCI_ML = "uci_ml"
    PHYSIONET = "physionet"
    HUGGINGFACE = "huggingface"
    OPENML = "openml"
    AWS_OPEN_DATA = "aws_open_data"
    GOOGLE_DATASET_SEARCH = "google_dataset_search"


@dataclass
class DatasetSearchResult:
    """Unified dataset search result."""
    dataset_id: str
    title: str
    description: str
    repository: DatasetRepository
    url: str
    size_mb: Optional[float]
    downloads: Optional[int]
    usability_score: Optional[float]
    tags: List[str]
    data_types: List[str]
    license: str
    last_updated: datetime
    relevance_score: float
    metadata: Dict = None

    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}


class KaggleUCIConnector:
    """
    Kaggle and UCI ML Repository Connector.

    Acts as internal data concierge for dataset discovery across:
    - 50,000+ Kaggle datasets
    - 600+ UCI ML Repository datasets
    - Additional public repositories

    Features:
    - Unified search interface
    - Aggregate search across multiple sources
    - Automatic dataset ranking
    - Download management
    - Metadata enrichment
    """

    def __init__(
        self,
        kaggle_username: Optional[str] = None,
        kaggle_key: Optional[str] = None
    ):
        """
        Initialize Kaggle/UCI connector.

        Args:
            kaggle_username: Kaggle username (or from env KAGGLE_USERNAME)
            kaggle_key: Kaggle API key (or from env KAGGLE_KEY)
        """
        self.kaggle_username = kaggle_username or os.getenv('KAGGLE_USERNAME')
        self.kaggle_key = kaggle_key or os.getenv('KAGGLE_KEY')

        self.kaggle_api_base = "https://www.kaggle.com/api/v1"
        self.uci_api_base = "https://archive.ics.uci.edu/api"

        self.session = None

        logger.info("KaggleUCIConnector initialized as data concierge")

    async def __aenter__(self):
        """Async context manager entry."""
        self.session = aiohttp.ClientSession()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        if self.session:
            await self.session.close()

    # ========================================================================
    # Aggregate Search (Data Concierge)
    # ========================================================================

    async def aggregate_search(
        self,
        query: str,
        repositories: Optional[List[DatasetRepository]] = None,
        data_types: Optional[List[str]] = None,
        min_usability: float = 0.6,
        max_results: int = 50
    ) -> List[DatasetSearchResult]:
        """
        Search across multiple dataset repositories (Data Concierge).

        Args:
            query: Search query string
            repositories: List of repositories to search (None = all)
            data_types: Filter by data types (tabular, image, text, etc.)
            min_usability: Minimum usability score (0-1)
            max_results: Maximum total results

        Returns:
            Ranked list of dataset search results from all sources

        Example:
            >>> connector = KaggleUCIConnector()
            >>> results = await connector.aggregate_search(
            ...     query="heart disease classification",
            ...     data_types=["tabular"],
            ...     max_results=20
            ... )
            >>> for result in results:
            ...     print(f"{result.repository.value}: {result.title}")
        """
        logger.info(f"Aggregate search: '{query}' across {len(repositories or [DatasetRepository.KAGGLE, DatasetRepository.UCI_ML])} repositories")

        # Default to Kaggle and UCI if not specified
        search_repos = repositories or [DatasetRepository.KAGGLE, DatasetRepository.UCI_ML]

        # Execute searches in parallel
        tasks = []
        for repo in search_repos:
            if repo == DatasetRepository.KAGGLE:
                tasks.append(self._search_kaggle(query, data_types))
            elif repo == DatasetRepository.UCI_ML:
                tasks.append(self._search_uci(query, data_types))

        # Gather results
        results_by_repo = await asyncio.gather(*tasks, return_exceptions=True)

        # Aggregate and deduplicate
        all_results = []
        for results in results_by_repo:
            if isinstance(results, list):
                all_results.extend(results)
            elif isinstance(results, Exception):
                logger.warning(f"Repository search failed: {results}")

        # Filter by usability
        if min_usability > 0:
            all_results = [
                r for r in all_results
                if r.usability_score is None or r.usability_score >= min_usability
            ]

        # Rank by relevance score
        all_results.sort(key=lambda x: x.relevance_score, reverse=True)

        # Return top results
        return all_results[:max_results]

    # ========================================================================
    # Kaggle Search
    # ========================================================================

    async def _search_kaggle(
        self,
        query: str,
        data_types: Optional[List[str]] = None
    ) -> List[DatasetSearchResult]:
        """
        Search Kaggle datasets.

        In production:
        - Uses Kaggle API: kaggle datasets list -s "query"
        - Requires authentication with kaggle.json
        - Returns datasets with metadata
        """
        logger.info(f"Searching Kaggle for: {query}")

        # Stub implementation with popular medical/ML datasets
        kaggle_datasets = [
            DatasetSearchResult(
                dataset_id="uciml/pima-indians-diabetes-database",
                title="Pima Indians Diabetes Database",
                description="Predict onset of diabetes based on diagnostic measures",
                repository=DatasetRepository.KAGGLE,
                url="https://www.kaggle.com/datasets/uciml/pima-indians-diabetes-database",
                size_mb=23.0,
                downloads=85000,
                usability_score=0.94,
                tags=["health", "diabetes", "classification"],
                data_types=["tabular"],
                license="CC0: Public Domain",
                last_updated=datetime(2024, 1, 15),
                relevance_score=self._calculate_relevance(query, "diabetes classification health")
            ),
            DatasetSearchResult(
                dataset_id="johnsmith/heart-disease-dataset",
                title="Heart Disease Dataset",
                description="UCI Heart Disease dataset for cardiovascular disease prediction",
                repository=DatasetRepository.KAGGLE,
                url="https://www.kaggle.com/datasets/johnsmith/heart-disease-dataset",
                size_mb=12.5,
                downloads=120000,
                usability_score=0.97,
                tags=["heart", "cardiovascular", "classification", "medical"],
                data_types=["tabular"],
                license="CC BY 4.0",
                last_updated=datetime(2024, 3, 10),
                relevance_score=self._calculate_relevance(query, "heart disease cardiovascular")
            ),
            DatasetSearchResult(
                dataset_id="masoudnickparvar/brain-tumor-mri-dataset",
                title="Brain Tumor MRI Dataset",
                description="MRI scans for brain tumor classification (glioma, meningioma, pituitary)",
                repository=DatasetRepository.KAGGLE,
                url="https://www.kaggle.com/datasets/masoudnickparvar/brain-tumor-mri-dataset",
                size_mb=1200.0,
                downloads=45000,
                usability_score=0.88,
                tags=["medical", "imaging", "mri", "tumor", "deep learning"],
                data_types=["image"],
                license="CC0: Public Domain",
                last_updated=datetime(2024, 2, 20),
                relevance_score=self._calculate_relevance(query, "brain tumor mri imaging")
            ),
            DatasetSearchResult(
                dataset_id="tawsifurrahman/covid19-radiography-database",
                title="COVID-19 Radiography Database",
                description="Chest X-ray images for COVID-19 detection",
                repository=DatasetRepository.KAGGLE,
                url="https://www.kaggle.com/datasets/tawsifurrahman/covid19-radiography-database",
                size_mb=850.0,
                downloads=72000,
                usability_score=0.91,
                tags=["covid19", "xray", "medical", "classification"],
                data_types=["image"],
                license="CC BY 4.0",
                last_updated=datetime(2024, 4, 5),
                relevance_score=self._calculate_relevance(query, "covid xray radiography medical")
            ),
            DatasetSearchResult(
                dataset_id="hmavrodiev/london-bike-sharing-dataset",
                title="London Bike Sharing Dataset",
                description="Historical bike sharing data from London",
                repository=DatasetRepository.KAGGLE,
                url="https://www.kaggle.com/datasets/hmavrodiev/london-bike-sharing-dataset",
                size_mb=4.5,
                downloads=35000,
                usability_score=0.89,
                tags=["time series", "transportation", "regression"],
                data_types=["tabular", "time_series"],
                license="CC BY-SA 4.0",
                last_updated=datetime(2024, 1, 30),
                relevance_score=self._calculate_relevance(query, "time series bike sharing")
            ),
        ]

        # Filter by data types
        if data_types:
            kaggle_datasets = [
                d for d in kaggle_datasets
                if any(dt in d.data_types for dt in data_types)
            ]

        return kaggle_datasets

    async def download_kaggle_dataset(
        self,
        dataset_id: str,
        output_path: str = "./ml_datasets/cache/kaggle"
    ) -> Dict:
        """
        Download Kaggle dataset.

        Args:
            dataset_id: Kaggle dataset identifier (e.g., "uciml/diabetes")
            output_path: Local output directory

        Returns:
            Download status and file paths

        In production:
        Uses: kaggle datasets download -d {dataset_id} -p {output_path}
        """
        logger.info(f"Downloading Kaggle dataset: {dataset_id}")

        # Stub implementation
        return {
            'dataset_id': dataset_id,
            'status': 'downloaded',
            'output_path': f"{output_path}/{dataset_id.replace('/', '_')}",
            'files': [
                f"{dataset_id.replace('/', '_')}.csv"
            ],
            'size_mb': 12.5,
            'downloaded_at': datetime.now().isoformat()
        }

    # ========================================================================
    # UCI ML Repository Search
    # ========================================================================

    async def _search_uci(
        self,
        query: str,
        data_types: Optional[List[str]] = None
    ) -> List[DatasetSearchResult]:
        """
        Search UCI Machine Learning Repository.

        UCI ML Repository contains 600+ datasets for ML research.
        """
        logger.info(f"Searching UCI ML Repository for: {query}")

        # Popular UCI datasets (stub)
        uci_datasets = [
            DatasetSearchResult(
                dataset_id="uci_00267",
                title="SECOM Dataset",
                description="Semiconductor manufacturing process dataset for quality prediction",
                repository=DatasetRepository.UCI_ML,
                url="https://archive.ics.uci.edu/dataset/267/secom",
                size_mb=8.2,
                downloads=15000,
                usability_score=0.85,
                tags=["manufacturing", "quality", "semiconductor", "classification"],
                data_types=["tabular"],
                license="CC BY 4.0",
                last_updated=datetime(2023, 11, 10),
                relevance_score=self._calculate_relevance(query, "manufacturing quality process")
            ),
            DatasetSearchResult(
                dataset_id="uci_00519",
                title="Heart Failure Clinical Records",
                description="Medical records of heart failure patients with survival prediction",
                repository=DatasetRepository.UCI_ML,
                url="https://archive.ics.uci.edu/dataset/519/heart+failure+clinical+records",
                size_mb=0.05,
                downloads=32000,
                usability_score=0.92,
                tags=["medical", "heart", "survival", "classification"],
                data_types=["tabular"],
                license="CC BY 4.0",
                last_updated=datetime(2024, 1, 8),
                relevance_score=self._calculate_relevance(query, "heart failure medical clinical")
            ),
            DatasetSearchResult(
                dataset_id="uci_00352",
                title="Online Retail Dataset",
                description="Transactional data from UK-based online retail",
                repository=DatasetRepository.UCI_ML,
                url="https://archive.ics.uci.edu/dataset/352/online+retail",
                size_mb=24.0,
                downloads=58000,
                usability_score=0.88,
                tags=["retail", "transactions", "clustering", "time series"],
                data_types=["tabular", "time_series"],
                license="CC BY 4.0",
                last_updated=datetime(2023, 12, 15),
                relevance_score=self._calculate_relevance(query, "retail transactions online")
            ),
            DatasetSearchResult(
                dataset_id="uci_00240",
                title="Human Activity Recognition Using Smartphones",
                description="Sensor data from smartphone accelerometer and gyroscope",
                repository=DatasetRepository.UCI_ML,
                url="https://archive.ics.uci.edu/dataset/240/human+activity+recognition+using+smartphones",
                size_mb=25.0,
                downloads=42000,
                usability_score=0.94,
                tags=["sensor", "activity", "accelerometer", "classification"],
                data_types=["time_series", "sensor"],
                license="CC BY 4.0",
                last_updated=datetime(2024, 2, 5),
                relevance_score=self._calculate_relevance(query, "activity recognition sensor smartphone")
            ),
            DatasetSearchResult(
                dataset_id="uci_00481",
                title="EMG Data for Gestures",
                description="8-channel EMG signals for hand gesture recognition",
                repository=DatasetRepository.UCI_ML,
                url="https://archive.ics.uci.edu/dataset/481/emg+data+for+gestures",
                size_mb=0.85,
                downloads=18000,
                usability_score=0.87,
                tags=["emg", "gesture", "sensor", "classification", "biomedical"],
                data_types=["time_series", "sensor"],
                license="CC BY 4.0",
                last_updated=datetime(2023, 10, 20),
                relevance_score=self._calculate_relevance(query, "emg gesture recognition biomedical")
            ),
        ]

        # Filter by data types
        if data_types:
            uci_datasets = [
                d for d in uci_datasets
                if any(dt in d.data_types for dt in data_types)
            ]

        return uci_datasets

    async def download_uci_dataset(
        self,
        dataset_id: str,
        output_path: str = "./ml_datasets/cache/uci"
    ) -> Dict:
        """
        Download UCI ML Repository dataset.

        Args:
            dataset_id: UCI dataset ID (e.g., "uci_00267")
            output_path: Local output directory

        Returns:
            Download status and file paths
        """
        logger.info(f"Downloading UCI dataset: {dataset_id}")

        # Stub implementation
        return {
            'dataset_id': dataset_id,
            'status': 'downloaded',
            'output_path': f"{output_path}/{dataset_id}",
            'files': ['data.csv', 'metadata.txt'],
            'size_mb': 8.2,
            'downloaded_at': datetime.now().isoformat()
        }

    # ========================================================================
    # Unified Download Interface
    # ========================================================================

    async def download(
        self,
        dataset_id: str,
        version: str,
        filters: Optional[Dict] = None
    ) -> Any:
        """
        Unified download interface for both Kaggle and UCI.

        Args:
            dataset_id: Dataset identifier
            version: Version (not used for Kaggle/UCI)
            filters: Optional filters

        Returns:
            Downloaded dataset
        """
        # Determine repository from dataset_id
        if dataset_id.startswith('uci_'):
            return await self.download_uci_dataset(dataset_id)
        else:
            # Assume Kaggle
            return await self.download_kaggle_dataset(dataset_id)

    # ========================================================================
    # Utility Methods
    # ========================================================================

    def _calculate_relevance(self, query: str, content: str) -> float:
        """
        Calculate relevance score between query and content.

        Uses simple keyword matching. In production, would use:
        - TF-IDF scoring
        - Embeddings similarity
        - BM25 ranking
        """
        query_words = set(query.lower().split())
        content_words = set(content.lower().split())

        if not query_words:
            return 0.0

        overlap = len(query_words.intersection(content_words))
        relevance = overlap / len(query_words)

        return min(relevance, 1.0)

    async def get_popular_datasets(
        self,
        repository: DatasetRepository,
        category: Optional[str] = None,
        limit: int = 20
    ) -> List[DatasetSearchResult]:
        """
        Get popular datasets from a repository.

        Args:
            repository: Repository to query
            category: Optional category filter
            limit: Maximum results

        Returns:
            List of popular datasets
        """
        if repository == DatasetRepository.KAGGLE:
            results = await self._search_kaggle("popular", None)
        elif repository == DatasetRepository.UCI_ML:
            results = await self._search_uci("popular", None)
        else:
            return []

        # Sort by downloads
        results.sort(key=lambda x: x.downloads or 0, reverse=True)

        return results[:limit]

    def get_dataset_metadata(self, dataset_id: str) -> Dict:
        """Get detailed metadata for a dataset."""
        # Stub - would fetch from repository API
        return {
            'dataset_id': dataset_id,
            'columns': [],
            'statistics': {},
            'sample_data': [],
            'related_datasets': []
        }


# Example usage
if __name__ == "__main__":
    async def main():
        # Initialize data concierge
        concierge = KaggleUCIConnector()

        # Aggregate search across Kaggle and UCI
        print("=== Aggregate Search: Heart Disease ===")
        results = await concierge.aggregate_search(
            query="heart disease classification",
            data_types=["tabular"],
            max_results=10
        )

        for i, result in enumerate(results, 1):
            print(f"\n{i}. {result.title}")
            print(f"   Repository: {result.repository.value}")
            print(f"   URL: {result.url}")
            print(f"   Downloads: {result.downloads:,}" if result.downloads else "")
            print(f"   Usability: {result.usability_score:.2f}" if result.usability_score else "")
            print(f"   Relevance: {result.relevance_score:.2f}")
            print(f"   Tags: {', '.join(result.tags)}")

        # Get popular datasets
        print("\n\n=== Popular Kaggle Datasets ===")
        popular = await concierge.get_popular_datasets(
            repository=DatasetRepository.KAGGLE,
            limit=5
        )

        for dataset in popular:
            print(f"- {dataset.title} ({dataset.downloads:,} downloads)")

    asyncio.run(main())
