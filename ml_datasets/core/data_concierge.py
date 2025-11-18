"""
Data Concierge - Unified Dataset Discovery System
Aggregates search across Kaggle, UCI ML, PhysioNet, Universities, and more

Acts as intelligent assistant for finding the perfect dataset for any ML task.
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from datetime import datetime

from ml_datasets.sources.kaggle_uci_connector import KaggleUCIConnector, DatasetSearchResult, DatasetRepository
from ml_datasets.sources.university_connectors import UniversityHubConnector
from ml_datasets.core.dataset_manager import DatasetManager

logger = logging.getLogger(__name__)


@dataclass
class DataConciergeRecommendation:
    """Dataset recommendation from the data concierge."""
    datasets: List[DatasetSearchResult]
    query: str
    total_found: int
    search_time_sec: float
    repositories_searched: List[str]
    recommendation_summary: str
    best_match: Optional[DatasetSearchResult]


class DataConcierge:
    """
    Intelligent Data Concierge for unified dataset discovery.

    Searches across:
    - Kaggle (50K+ datasets)
    - UCI Machine Learning Repository (600+ datasets)
    - University repositories (50+ institutions)
    - PhysioNet (medical datasets)
    - Local catalog (curated datasets)

    Features:
    - Natural language queries
    - Intelligent ranking and recommendations
    - Multi-repository aggregation
    - Domain-specific filtering
    - Usage-based suggestions
    """

    def __init__(self):
        """Initialize Data Concierge with all connectors."""
        self.kaggle_uci = KaggleUCIConnector()
        self.universities = UniversityHubConnector()
        self.dataset_manager = DatasetManager()

        self.all_repositories = [
            DatasetRepository.KAGGLE,
            DatasetRepository.UCI_ML,
        ]

        logger.info("Data Concierge initialized - your intelligent dataset assistant")

    async def find_datasets(
        self,
        query: str,
        domain: Optional[str] = None,
        data_types: Optional[List[str]] = None,
        min_quality: float = 0.7,
        max_results: int = 20,
        include_universities: bool = False
    ) -> DataConciergeRecommendation:
        """
        Find datasets using natural language query (Main entry point).

        Args:
            query: Natural language search query
            domain: Domain filter (medical, robotics, finance, etc.)
            data_types: Data type filters (tabular, image, time_series, etc.)
            min_quality: Minimum quality/usability score
            max_results: Maximum results to return
            include_universities: Include university repositories

        Returns:
            DataConciergeRecommendation with ranked results

        Example:
            >>> concierge = DataConcierge()
            >>> rec = await concierge.find_datasets(
            ...     query="hand gesture recognition using EMG sensors",
            ...     domain="medical",
            ...     data_types=["time_series", "sensor"]
            ... )
            >>> print(rec.best_match.title)
            >>> for dataset in rec.datasets[:5]:
            ...     print(f"- {dataset.title} ({dataset.repository.value})")
        """
        start_time = datetime.now()

        logger.info(f"Data Concierge searching for: '{query}'")

        # Parallel search across all sources
        tasks = []

        # 1. Kaggle + UCI aggregate search
        tasks.append(
            self.kaggle_uci.aggregate_search(
                query=query,
                data_types=data_types,
                min_usability=min_quality,
                max_results=max_results
            )
        )

        # 2. Local curated catalog
        tasks.append(
            self.dataset_manager.search_datasets(
                query=query,
                data_types=data_types,
                max_results=max_results
            )
        )

        # 3. University repositories (if requested)
        if include_universities:
            tasks.append(
                self._search_universities(query, domain)
            )

        # Execute all searches
        results_by_source = await asyncio.gather(*tasks, return_exceptions=True)

        # Aggregate results
        all_datasets = []

        for results in results_by_source:
            if isinstance(results, list):
                for item in results:
                    if isinstance(item, DatasetSearchResult):
                        all_datasets.append(item)
                    elif isinstance(item, dict):
                        # Convert catalog results to DatasetSearchResult
                        all_datasets.append(self._convert_catalog_result(item))
            elif isinstance(results, Exception):
                logger.warning(f"Search source failed: {results}")

        # Rank and filter
        all_datasets = self._rank_datasets(all_datasets, query, domain)

        # Apply quality filter
        all_datasets = [
            d for d in all_datasets
            if d.usability_score is None or d.usability_score >= min_quality
        ]

        # Limit results
        top_datasets = all_datasets[:max_results]

        # Calculate search time
        search_time = (datetime.now() - start_time).total_seconds()

        # Identify best match
        best_match = top_datasets[0] if top_datasets else None

        # Generate recommendation summary
        summary = self._generate_recommendation_summary(
            query=query,
            datasets=top_datasets,
            best_match=best_match
        )

        # Get unique repositories searched
        repos_searched = list(set(d.repository.value for d in all_datasets))

        return DataConciergeRecommendation(
            datasets=top_datasets,
            query=query,
            total_found=len(all_datasets),
            search_time_sec=search_time,
            repositories_searched=repos_searched,
            recommendation_summary=summary,
            best_match=best_match
        )

    async def _search_universities(
        self,
        query: str,
        domain: Optional[str]
    ) -> List[DatasetSearchResult]:
        """Search university repositories."""
        # Convert domain to disciplines
        disciplines = [domain] if domain else None

        results = await self.universities.search(
            keywords=query.split(),
            disciplines=disciplines,
            max_results=10
        )

        # Convert to DatasetSearchResult format
        search_results = []
        for result in results:
            search_results.append(DatasetSearchResult(
                dataset_id=result['id'],
                title=result['title'],
                description=result.get('title', ''),
                repository=DatasetRepository.KAGGLE,  # Placeholder
                url=result['access_url'],
                size_mb=None,
                downloads=None,
                usability_score=None,
                tags=[],
                data_types=[result.get('data_type', 'unknown')],
                license='Unknown',
                last_updated=datetime.now(),
                relevance_score=result.get('relevance_score', 0.5),
                metadata={'institution': result.get('institution')}
            ))

        return search_results

    def _convert_catalog_result(self, catalog_item: Dict) -> DatasetSearchResult:
        """Convert catalog search result to unified format."""
        dataset = catalog_item.get('dataset', {})

        return DatasetSearchResult(
            dataset_id=dataset.get('id', 'unknown'),
            title=dataset.get('name', 'Unknown'),
            description=dataset.get('description', ''),
            repository=DatasetRepository.PHYSIONET,  # Assuming PhysioNet for now
            url=dataset.get('url', ''),
            size_mb=dataset.get('size_gb', 0) * 1024 if 'size_gb' in dataset else None,
            downloads=None,
            usability_score=dataset.get('quality_metrics', {}).get('completeness'),
            tags=dataset.get('features', []),
            data_types=[dataset.get('data_type', 'unknown')],
            license=dataset.get('license', 'Unknown'),
            last_updated=datetime.now(),
            relevance_score=catalog_item.get('relevance_score', 0.5)
        )

    def _rank_datasets(
        self,
        datasets: List[DatasetSearchResult],
        query: str,
        domain: Optional[str]
    ) -> List[DatasetSearchResult]:
        """
        Intelligent ranking of datasets.

        Considers:
        - Relevance to query
        - Usability score
        - Download popularity
        - Recency
        - Domain match
        """
        for dataset in datasets:
            # Calculate composite score
            relevance_weight = 0.4
            usability_weight = 0.3
            popularity_weight = 0.2
            recency_weight = 0.1

            # Relevance (already calculated)
            relevance_score = dataset.relevance_score

            # Usability
            usability_score = dataset.usability_score or 0.5

            # Popularity (normalize downloads)
            if dataset.downloads:
                popularity_score = min(dataset.downloads / 100000, 1.0)
            else:
                popularity_score = 0.3  # Default for unknown

            # Recency (datasets from last year get boost)
            days_old = (datetime.now() - dataset.last_updated).days
            if days_old < 365:
                recency_score = 1.0 - (days_old / 365)
            else:
                recency_score = 0.2

            # Domain bonus
            domain_bonus = 0.0
            if domain and domain.lower() in ' '.join(dataset.tags).lower():
                domain_bonus = 0.1

            # Composite score
            composite = (
                relevance_score * relevance_weight +
                usability_score * usability_weight +
                popularity_score * popularity_weight +
                recency_score * recency_weight +
                domain_bonus
            )

            # Update relevance score with composite
            dataset.relevance_score = composite

        # Sort by composite score
        datasets.sort(key=lambda x: x.relevance_score, reverse=True)

        return datasets

    def _generate_recommendation_summary(
        self,
        query: str,
        datasets: List[DatasetSearchResult],
        best_match: Optional[DatasetSearchResult]
    ) -> str:
        """Generate human-readable recommendation summary."""
        if not datasets:
            return f"No datasets found matching '{query}'. Try broadening your search."

        summary = f"Found {len(datasets)} datasets matching '{query}'.\n\n"

        if best_match:
            summary += f"**Best Match**: {best_match.title}\n"
            summary += f"  - Repository: {best_match.repository.value}\n"
            summary += f"  - Quality Score: {best_match.usability_score:.2f}\n" if best_match.usability_score else ""
            summary += f"  - Downloads: {best_match.downloads:,}\n" if best_match.downloads else ""
            summary += f"  - URL: {best_match.url}\n"

        # Repository distribution
        repo_counts = {}
        for dataset in datasets:
            repo = dataset.repository.value
            repo_counts[repo] = repo_counts.get(repo, 0) + 1

        summary += f"\n**Sources**: "
        summary += ", ".join([f"{repo} ({count})" for repo, count in repo_counts.items()])

        return summary

    async def recommend_by_task(
        self,
        task: str,
        domain: Optional[str] = None
    ) -> DataConciergeRecommendation:
        """
        Recommend datasets for a specific ML task.

        Args:
            task: ML task (classification, regression, segmentation, etc.)
            domain: Application domain

        Returns:
            Recommended datasets for the task
        """
        # Map tasks to search queries
        task_queries = {
            'classification': 'classification labeled dataset',
            'regression': 'regression continuous prediction',
            'segmentation': 'image segmentation pixel annotations',
            'object_detection': 'object detection bounding boxes',
            'time_series_forecasting': 'time series forecasting temporal',
            'clustering': 'clustering unlabeled grouping',
            'anomaly_detection': 'anomaly detection outlier',
            'nlp': 'natural language processing text',
            'recommendation': 'recommendation collaborative filtering'
        }

        query = task_queries.get(task.lower(), task)

        if domain:
            query = f"{domain} {query}"

        return await self.find_datasets(query=query, domain=domain)

    def get_trending_datasets(
        self,
        category: Optional[str] = None,
        days: int = 30
    ) -> List[str]:
        """Get trending datasets (stub for future implementation)."""
        return [
            "COVID-19 Radiography Database",
            "Heart Disease Dataset",
            "Brain Tumor MRI Dataset",
            "KITTI Vision Benchmark",
            "nuScenes Autonomous Driving"
        ]


# Example usage
if __name__ == "__main__":
    async def main():
        # Initialize Data Concierge
        concierge = DataConcierge()

        # Example 1: Find EMG datasets
        print("=== Example 1: Hand Gesture EMG ===")
        recommendation = await concierge.find_datasets(
            query="hand gesture recognition using EMG sensors",
            domain="medical",
            data_types=["time_series", "sensor"],
            max_results=10
        )

        print(recommendation.recommendation_summary)
        print(f"\nSearch completed in {recommendation.search_time_sec:.2f} seconds")
        print(f"Searched repositories: {', '.join(recommendation.repositories_searched)}")

        print("\nTop 5 Results:")
        for i, dataset in enumerate(recommendation.datasets[:5], 1):
            print(f"\n{i}. {dataset.title}")
            print(f"   Source: {dataset.repository.value}")
            print(f"   Score: {dataset.relevance_score:.3f}")
            if dataset.usability_score:
                print(f"   Quality: {dataset.usability_score:.2f}")

        # Example 2: Task-based recommendation
        print("\n\n=== Example 2: Image Classification Task ===")
        rec2 = await concierge.recommend_by_task(
            task="classification",
            domain="medical imaging"
        )

        print(f"Best dataset for medical image classification:")
        if rec2.best_match:
            print(f"  {rec2.best_match.title}")
            print(f"  URL: {rec2.best_match.url}")

        # Example 3: Get trending datasets
        print("\n\n=== Example 3: Trending Datasets ===")
        trending = concierge.get_trending_datasets(days=30)
        for i, name in enumerate(trending, 1):
            print(f"{i}. {name}")

    asyncio.run(main())
