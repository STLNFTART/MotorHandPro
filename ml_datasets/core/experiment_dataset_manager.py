"""
Experiment-Specific Dataset Manager
Manages datasets on a per-experiment basis with isolation and tracking

Integrates with LAM framework's experiment system for:
- Isolated dataset contexts per experiment
- Automatic versioning and lineage tracking
- Experiment-specific caching
- Dataset reproducibility
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime
from pathlib import Path
import json
import hashlib

from ml_datasets.core.dataset_manager import DatasetManager, Dataset

logger = logging.getLogger(__name__)


class ExperimentDatasetManager:
    """
    Manage datasets for individual experiments with isolation.

    Each experiment gets its own dataset context:
    - Separate cache directory
    - Experiment-specific metadata
    - Reproducible dataset snapshots
    - Integration with LAM experiment tracking

    Features:
    - Per-experiment dataset isolation
    - Automatic experiment ID generation
    - Dataset version locking for reproducibility
    - Experiment comparison across different datasets
    - Lineage tracking (which datasets used in which experiments)
    """

    def __init__(
        self,
        experiment_id: Optional[str] = None,
        experiment_name: Optional[str] = None,
        base_cache_dir: str = "./ml_datasets/experiments"
    ):
        """
        Initialize experiment-specific dataset manager.

        Args:
            experiment_id: Unique experiment ID (auto-generated if None)
            experiment_name: Human-readable experiment name
            base_cache_dir: Base directory for experiment caches
        """
        self.experiment_id = experiment_id or self._generate_experiment_id()
        self.experiment_name = experiment_name or f"experiment_{self.experiment_id[:8]}"
        self.base_cache_dir = Path(base_cache_dir)

        # Create experiment-specific cache directory
        self.experiment_cache_dir = self.base_cache_dir / self.experiment_id
        self.experiment_cache_dir.mkdir(parents=True, exist_ok=True)

        # Initialize dataset manager with experiment-specific cache
        self.dataset_manager = DatasetManager(
            cache_dir=str(self.experiment_cache_dir / "datasets")
        )

        # Track datasets loaded for this experiment
        self.loaded_datasets: Dict[str, Dataset] = {}
        self.dataset_versions: Dict[str, str] = {}

        # Experiment metadata
        self.metadata = {
            'experiment_id': self.experiment_id,
            'experiment_name': self.experiment_name,
            'created_at': datetime.now().isoformat(),
            'datasets': [],
            'parameters': {},
            'status': 'active'
        }

        # Save experiment metadata
        self._save_metadata()

        logger.info(f"ExperimentDatasetManager initialized for '{self.experiment_name}' ({self.experiment_id})")

    def _generate_experiment_id(self) -> str:
        """Generate unique experiment ID."""
        timestamp = datetime.now().isoformat()
        random_hash = hashlib.sha256(timestamp.encode()).hexdigest()[:16]
        return f"exp_{random_hash}"

    def _save_metadata(self):
        """Save experiment metadata to JSON file."""
        metadata_file = self.experiment_cache_dir / "experiment_metadata.json"
        with open(metadata_file, 'w') as f:
            json.dump(self.metadata, f, indent=2)

    async def load_dataset(
        self,
        dataset_id: str,
        version: Optional[str] = None,
        filters: Optional[Dict] = None,
        lock_version: bool = True
    ) -> Dataset:
        """
        Load dataset for this specific experiment.

        Args:
            dataset_id: Dataset identifier
            version: Dataset version (None = latest)
            filters: Optional filters
            lock_version: Lock dataset version for reproducibility

        Returns:
            Dataset object

        Example:
            >>> exp_manager = ExperimentDatasetManager(
            ...     experiment_name="cardiac_classification_v1"
            ... )
            >>> mitdb = await exp_manager.load_dataset(
            ...     dataset_id="mitdb",
            ...     filters={"record_range": [100, 109]},
            ...     lock_version=True
            ... )
            >>> print(f"Loaded for experiment: {exp_manager.experiment_name}")
        """
        logger.info(f"Loading dataset '{dataset_id}' for experiment '{self.experiment_name}'")

        # Load dataset
        dataset = await self.dataset_manager.load_dataset(
            dataset_id=dataset_id,
            version=version,
            filters=filters
        )

        # Track dataset for this experiment
        self.loaded_datasets[dataset_id] = dataset

        # Lock version if requested
        if lock_version:
            self.dataset_versions[dataset_id] = dataset.version

        # Update experiment metadata
        self.metadata['datasets'].append({
            'dataset_id': dataset_id,
            'version': dataset.version,
            'loaded_at': datetime.now().isoformat(),
            'filters': filters,
            'quality_metrics': dataset.quality_metrics
        })
        self._save_metadata()

        return dataset

    async def load_multiple_datasets(
        self,
        dataset_specs: List[Dict[str, Any]],
        parallel: bool = True
    ) -> Dict[str, Dataset]:
        """
        Load multiple datasets for this experiment.

        Args:
            dataset_specs: List of dataset specifications
                [
                    {'dataset_id': 'mitdb', 'version': '1.0.0', 'filters': {...}},
                    {'dataset_id': 'ptbdb', 'version': '1.0.0'}
                ]
            parallel: Load datasets in parallel

        Returns:
            Dictionary mapping dataset_id to Dataset object

        Example:
            >>> specs = [
            ...     {'dataset_id': 'mitdb', 'filters': {'record_range': [100, 109]}},
            ...     {'dataset_id': 'ptbdb', 'version': '1.0.0'},
            ...     {'dataset_id': 'sleep_edf'}
            ... ]
            >>> datasets = await exp_manager.load_multiple_datasets(specs)
            >>> print(f"Loaded {len(datasets)} datasets")
        """
        logger.info(f"Loading {len(dataset_specs)} datasets for experiment '{self.experiment_name}'")

        if parallel:
            # Load all datasets in parallel
            tasks = []
            for spec in dataset_specs:
                tasks.append(
                    self.load_dataset(
                        dataset_id=spec['dataset_id'],
                        version=spec.get('version'),
                        filters=spec.get('filters'),
                        lock_version=spec.get('lock_version', True)
                    )
                )

            datasets_list = await asyncio.gather(*tasks)

            # Create mapping
            result = {}
            for spec, dataset in zip(dataset_specs, datasets_list):
                result[spec['dataset_id']] = dataset

            return result
        else:
            # Load sequentially
            result = {}
            for spec in dataset_specs:
                dataset = await self.load_dataset(
                    dataset_id=spec['dataset_id'],
                    version=spec.get('version'),
                    filters=spec.get('filters'),
                    lock_version=spec.get('lock_version', True)
                )
                result[spec['dataset_id']] = dataset

            return result

    def get_dataset(self, dataset_id: str) -> Optional[Dataset]:
        """Get already-loaded dataset for this experiment."""
        return self.loaded_datasets.get(dataset_id)

    def list_datasets(self) -> List[str]:
        """List all datasets loaded for this experiment."""
        return list(self.loaded_datasets.keys())

    def get_experiment_summary(self) -> Dict:
        """Get experiment summary with dataset information."""
        return {
            'experiment_id': self.experiment_id,
            'experiment_name': self.experiment_name,
            'created_at': self.metadata['created_at'],
            'num_datasets': len(self.loaded_datasets),
            'datasets': self.metadata['datasets'],
            'cache_dir': str(self.experiment_cache_dir),
            'status': self.metadata['status']
        }

    def set_parameters(self, parameters: Dict):
        """Set experiment parameters (hyperparameters, config, etc.)."""
        self.metadata['parameters'] = parameters
        self._save_metadata()

    def add_result(self, key: str, value: Any):
        """Add experiment result."""
        if 'results' not in self.metadata:
            self.metadata['results'] = {}

        self.metadata['results'][key] = value
        self._save_metadata()

    def complete_experiment(self, status: str = "completed"):
        """Mark experiment as completed."""
        self.metadata['status'] = status
        self.metadata['completed_at'] = datetime.now().isoformat()
        self._save_metadata()

        logger.info(f"Experiment '{self.experiment_name}' marked as {status}")

    def get_reproducibility_config(self) -> Dict:
        """
        Get configuration for reproducing this experiment.

        Returns exact dataset versions and filters used.
        """
        return {
            'experiment_id': self.experiment_id,
            'experiment_name': self.experiment_name,
            'dataset_versions': self.dataset_versions,
            'datasets': self.metadata['datasets'],
            'parameters': self.metadata.get('parameters', {}),
            'created_at': self.metadata['created_at']
        }

    @classmethod
    def load_experiment(cls, experiment_id: str, base_cache_dir: str = "./ml_datasets/experiments"):
        """
        Load existing experiment from disk.

        Args:
            experiment_id: Experiment ID to load
            base_cache_dir: Base cache directory

        Returns:
            ExperimentDatasetManager instance
        """
        experiment_cache_dir = Path(base_cache_dir) / experiment_id
        metadata_file = experiment_cache_dir / "experiment_metadata.json"

        if not metadata_file.exists():
            raise ValueError(f"Experiment {experiment_id} not found")

        # Load metadata
        with open(metadata_file, 'r') as f:
            metadata = json.load(f)

        # Create instance
        manager = cls(
            experiment_id=experiment_id,
            experiment_name=metadata.get('experiment_name'),
            base_cache_dir=base_cache_dir
        )

        manager.metadata = metadata

        logger.info(f"Loaded existing experiment: {metadata.get('experiment_name')}")

        return manager

    @classmethod
    def list_experiments(cls, base_cache_dir: str = "./ml_datasets/experiments") -> List[Dict]:
        """
        List all experiments.

        Returns:
            List of experiment summaries
        """
        base_path = Path(base_cache_dir)
        if not base_path.exists():
            return []

        experiments = []
        for exp_dir in base_path.iterdir():
            if exp_dir.is_dir():
                metadata_file = exp_dir / "experiment_metadata.json"
                if metadata_file.exists():
                    with open(metadata_file, 'r') as f:
                        metadata = json.load(f)
                        experiments.append({
                            'experiment_id': metadata.get('experiment_id'),
                            'experiment_name': metadata.get('experiment_name'),
                            'created_at': metadata.get('created_at'),
                            'status': metadata.get('status'),
                            'num_datasets': len(metadata.get('datasets', []))
                        })

        # Sort by creation date (newest first)
        experiments.sort(key=lambda x: x.get('created_at', ''), reverse=True)

        return experiments


# Example usage
if __name__ == "__main__":
    async def main():
        # Example 1: Create experiment and load single dataset
        print("=== Example 1: Single Dataset for Experiment ===")
        exp1 = ExperimentDatasetManager(
            experiment_name="cardiac_arrhythmia_classification_v1"
        )

        mitdb = await exp1.load_dataset(
            dataset_id="mitdb",
            filters={"record_range": [100, 109]},
            lock_version=True
        )

        print(f"Experiment: {exp1.experiment_name}")
        print(f"Dataset: {mitdb.id} v{mitdb.version}")
        print(f"Quality: {mitdb.quality_metrics}")

        # Add experiment parameters
        exp1.set_parameters({
            'model': 'CNN',
            'learning_rate': 0.001,
            'batch_size': 32
        })

        # Add results
        exp1.add_result('accuracy', 0.95)
        exp1.add_result('f1_score', 0.93)

        # Complete experiment
        exp1.complete_experiment(status="completed")

        # Example 2: Load multiple datasets for an experiment
        print("\n\n=== Example 2: Multiple Datasets for Experiment ===")
        exp2 = ExperimentDatasetManager(
            experiment_name="multi_dataset_ecg_analysis"
        )

        dataset_specs = [
            {'dataset_id': 'mitdb', 'filters': {'record_range': [100, 104]}},
            {'dataset_id': 'ptbdb', 'version': '1.0.0'},
            {'dataset_id': 'sleep_edf'}
        ]

        datasets = await exp2.load_multiple_datasets(
            dataset_specs=dataset_specs,
            parallel=True
        )

        print(f"Loaded {len(datasets)} datasets:")
        for dataset_id, dataset in datasets.items():
            print(f"  - {dataset_id}: {dataset.metadata.get('name')}")

        # Example 3: Get reproducibility config
        print("\n\n=== Example 3: Reproducibility Config ===")
        repro_config = exp2.get_reproducibility_config()
        print(json.dumps(repro_config, indent=2))

        # Example 4: List all experiments
        print("\n\n=== Example 4: List All Experiments ===")
        all_experiments = ExperimentDatasetManager.list_experiments()
        print(f"Total experiments: {len(all_experiments)}")
        for exp in all_experiments:
            print(f"  - {exp['experiment_name']} ({exp['status']}, {exp['num_datasets']} datasets)")

    asyncio.run(main())
