"""
LAM-Dataset Integration Layer
Connects ML Dataset Infrastructure with LAM (Large Action Model) Framework

Enables:
- Dataset loading as LAM actions
- Quantum resonance scoring for dataset quality
- Experiment tracking integration
- Dataset operations with Lightfoot/Donte constants
"""

import asyncio
import logging
from typing import Dict, List, Optional, Any
from datetime import datetime
import sys
from pathlib import Path

# Import LAM core
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "lam"))
from lam.core.primal_lam import PrimalLAM, QuantumResonanceField

# Import dataset infrastructure
from ml_datasets.core.dataset_manager import DatasetManager, Dataset
from ml_datasets.core.experiment_dataset_manager import ExperimentDatasetManager
from ml_datasets.core.bulk_dataset_loader import BulkDatasetLoader
from ml_datasets.core.data_concierge import DataConcierge

logger = logging.getLogger(__name__)


class LAMDatasetBridge:
    """
    Integration bridge between LAM and ML Dataset Infrastructure.

    Provides:
    - Dataset loading as LAM-tracked actions
    - Quantum resonance quality scoring
    - Experiment integration
    - Semantic dataset recommendations
    """

    def __init__(self, lam_instance: Optional[PrimalLAM] = None):
        """
        Initialize LAM-Dataset bridge.

        Args:
            lam_instance: Existing PrimalLAM instance (creates new if None)
        """
        # Initialize or use provided LAM instance
        self.lam = lam_instance or PrimalLAM()

        # Initialize dataset components
        self.dataset_manager = DatasetManager()
        self.data_concierge = DataConcierge()
        self.bulk_loader = BulkDatasetLoader()

        # Track dataset operations
        self.dataset_operations = []

        logger.info("LAM-Dataset Bridge initialized")

    async def load_dataset_with_lam(
        self,
        dataset_id: str,
        version: Optional[str] = None,
        filters: Optional[Dict] = None
    ) -> Dict[str, Any]:
        """
        Load dataset as a LAM action with quantum resonance tracking.

        Args:
            dataset_id: Dataset identifier
            version: Dataset version
            filters: Optional filters

        Returns:
            Dictionary with dataset and LAM action record

        Example:
            >>> bridge = LAMDatasetBridge()
            >>> result = await bridge.load_dataset_with_lam(
            ...     dataset_id="mitdb",
            ...     filters={"record_range": [100, 109]}
            ... )
            >>> dataset = result['dataset']
            >>> lam_action = result['lam_action']
            >>> print(f"Resonance Quality: {lam_action['resonance_quality']:.4f}")
        """
        logger.info(f"LAM action: Loading dataset '{dataset_id}'")

        # Record as LAM action
        action_details = {
            'dataset_id': dataset_id,
            'version': version,
            'filters': filters,
            'operation': 'dataset_load'
        }

        # Get resonance state before action
        resonance_before = self.lam.resonance.get_state()

        # Load dataset
        dataset = await self.dataset_manager.load_dataset(
            dataset_id=dataset_id,
            version=version,
            filters=filters
        )

        # Update resonance parameters
        self.lam.resonance.update_resonance_parameters(len(self.lam.action_history))

        # Get resonance state after action
        resonance_after = self.lam.resonance.get_state()

        # Calculate quantum-resonance quality score
        resonance_quality = self._calculate_resonance_quality(
            dataset=dataset,
            resonance_state=resonance_after
        )

        # Record LAM action
        lam_action = self.lam._record_action(
            action_type="dataset_load",
            details={
                **action_details,
                'resonance_quality': resonance_quality,
                'dataset_quality': dataset.quality_metrics,
                'attractor_distance': self._calculate_attractor_distance(dataset)
            }
        )

        # Track operation
        operation_record = {
            'timestamp': datetime.now().isoformat(),
            'dataset_id': dataset_id,
            'dataset': dataset,
            'lam_action': lam_action,
            'resonance_quality': resonance_quality
        }

        self.dataset_operations.append(operation_record)

        return {
            'dataset': dataset,
            'lam_action': lam_action,
            'resonance_quality': resonance_quality,
            'resonance_before': resonance_before,
            'resonance_after': resonance_after
        }

    def _calculate_resonance_quality(
        self,
        dataset: Dataset,
        resonance_state: Dict
    ) -> float:
        """
        Calculate quantum-resonance quality score for dataset.

        Combines:
        - Dataset quality metrics (completeness, diversity, etc.)
        - LAM resonance parameters (alpha, lambda)
        - Lightfoot decay for temporal weighting
        - Donte attractor convergence
        """
        # Dataset quality (0-1)
        dataset_quality = dataset.quality_metrics.get('completeness', 0.5)

        # LAM resonance factors
        alpha = resonance_state['alpha']
        lmbd = resonance_state['lambda']

        # Temporal weight (Lightfoot decay)
        time_delta_hours = 0  # Could track dataset age
        temporal_weight = 1.0  # exp(-lambda * time_delta)

        # Attractor convergence (closer to Donte = higher quality)
        attractor_distance = self._calculate_attractor_distance(dataset)
        attractor_score = 1.0 - (attractor_distance / resonance_state['donte_attractor'])

        # Combined quantum-resonance quality
        resonance_quality = (
            dataset_quality * 0.4 +
            temporal_weight * 0.2 +
            attractor_score * 0.3 +
            alpha * 0.1  # Optimization factor influence
        )

        return max(min(resonance_quality, 1.0), 0.0)

    def _calculate_attractor_distance(self, dataset: Dataset) -> float:
        """
        Calculate distance from Donte attractor for a dataset.

        Maps dataset quality to proximity to fixed-point attractor.
        Higher quality datasets converge closer to Donte constant.
        """
        # Map dataset quality (0-1) to value near Donte constant
        quality = dataset.quality_metrics.get('completeness', 0.5)

        # Quality of 1.0 maps to Donte constant (149.999...)
        # Quality of 0.0 maps to ~100
        mapped_value = 100 + (quality * (self.lam.resonance.donte_attractor - 100))

        # Distance from attractor
        distance = abs(self.lam.resonance.donte_attractor - mapped_value)

        return distance

    async def create_lam_experiment(
        self,
        experiment_name: str,
        dataset_specs: List[Dict[str, Any]]
    ) -> Dict[str, Any]:
        """
        Create experiment with LAM tracking.

        Args:
            experiment_name: Name for the experiment
            dataset_specs: List of dataset specifications

        Returns:
            Experiment manager with LAM integration

        Example:
            >>> specs = [
            ...     {'dataset_id': 'mitdb'},
            ...     {'dataset_id': 'ptbdb'},
            ...     {'dataset_id': 'kitti'}
            ... ]
            >>> exp_result = await bridge.create_lam_experiment(
            ...     experiment_name="multi_modal_analysis",
            ...     dataset_specs=specs
            ... )
            >>> exp_manager = exp_result['experiment_manager']
            >>> lam_state = exp_result['lam_state']
        """
        logger.info(f"LAM: Creating experiment '{experiment_name}'")

        # Create experiment with dataset manager
        exp_manager = ExperimentDatasetManager(experiment_name=experiment_name)

        # Record as LAM action
        lam_action = self.lam._record_action(
            action_type="experiment_create",
            details={
                'experiment_name': experiment_name,
                'experiment_id': exp_manager.experiment_id,
                'num_datasets': len(dataset_specs)
            }
        )

        # Load datasets with LAM tracking
        dataset_results = []
        for spec in dataset_specs:
            result = await self.load_dataset_with_lam(
                dataset_id=spec['dataset_id'],
                version=spec.get('version'),
                filters=spec.get('filters')
            )
            dataset_results.append(result)

            # Add to experiment
            exp_manager.loaded_datasets[spec['dataset_id']] = result['dataset']

        # Calculate aggregate resonance quality
        avg_resonance_quality = sum(r['resonance_quality'] for r in dataset_results) / len(dataset_results)

        # Get current LAM state
        lam_state = self.lam.resonance.get_state()

        # Store LAM metadata in experiment
        exp_manager.metadata['lam_integration'] = {
            'resonance_quality': avg_resonance_quality,
            'lam_state': lam_state,
            'lipschitz_constant': lam_state['lipschitz_constant'],
            'stability_guaranteed': lam_state['lipschitz_constant'] < 1.0
        }

        exp_manager._save_metadata()

        return {
            'experiment_manager': exp_manager,
            'lam_action': lam_action,
            'lam_state': lam_state,
            'resonance_quality': avg_resonance_quality,
            'dataset_results': dataset_results
        }

    async def recommend_datasets_with_lam(
        self,
        query: str,
        max_results: int = 10
    ) -> Dict[str, Any]:
        """
        Use LAM semantic resonance for dataset recommendations.

        Args:
            query: Natural language query
            max_results: Maximum results

        Returns:
            Recommendations with quantum-resonance scoring
        """
        logger.info(f"LAM recommendation: '{query}'")

        # Use Data Concierge for search
        search_result = await self.data_concierge.find_datasets(
            query=query,
            max_results=max_results * 2  # Get extra for resonance filtering
        )

        # Apply quantum-resonance scoring
        resonance_scored_datasets = []

        for dataset_info in search_result.datasets:
            # Simulate dataset object for scoring
            mock_dataset = type('Dataset', (), {
                'quality_metrics': {
                    'completeness': dataset_info.usability_score or 0.5
                }
            })()

            # Get resonance state
            resonance_state = self.lam.resonance.get_state()

            # Calculate resonance quality
            resonance_quality = self._calculate_resonance_quality(
                dataset=mock_dataset,
                resonance_state=resonance_state
            )

            resonance_scored_datasets.append({
                'dataset_info': dataset_info,
                'resonance_quality': resonance_quality,
                'attractor_distance': self._calculate_attractor_distance(mock_dataset)
            })

        # Sort by resonance quality
        resonance_scored_datasets.sort(
            key=lambda x: x['resonance_quality'],
            reverse=True
        )

        # Record as LAM action
        lam_action = self.lam._record_action(
            action_type="dataset_recommendation",
            details={
                'query': query,
                'num_results': len(resonance_scored_datasets),
                'top_resonance_quality': resonance_scored_datasets[0]['resonance_quality'] if resonance_scored_datasets else 0
            }
        )

        return {
            'recommendations': resonance_scored_datasets[:max_results],
            'lam_action': lam_action,
            'search_result': search_result
        }

    def get_lam_dataset_history(self) -> List[Dict]:
        """Get history of all dataset operations tracked by LAM."""
        return self.dataset_operations

    def get_resonance_summary(self) -> Dict:
        """
        Get summary of LAM resonance state and dataset operations.

        Returns:
            Summary with stability metrics and operation statistics
        """
        resonance_state = self.lam.resonance.get_state()

        return {
            'lam_state': resonance_state,
            'stability': {
                'lipschitz_constant': resonance_state['lipschitz_constant'],
                'stable': resonance_state['lipschitz_constant'] < 1.0,
                'semantic_bounds': self.lam.resonance.check_semantic_bounds()
            },
            'operations': {
                'total_dataset_operations': len(self.dataset_operations),
                'total_lam_actions': len(self.lam.action_history)
            },
            'quantum_metrics': {
                'lightfoot_constant': resonance_state['lightfoot_constant'],
                'donte_attractor': resonance_state['donte_attractor'],
                'alpha': resonance_state['alpha'],
                'lambda': resonance_state['lambda']
            }
        }


# Example usage
if __name__ == "__main__":
    async def main():
        # Initialize LAM-Dataset bridge
        bridge = LAMDatasetBridge()

        print("=== LAM-Dataset Integration Example ===\n")

        # Example 1: Load dataset with LAM tracking
        print("1. Loading dataset with LAM tracking...")
        result = await bridge.load_dataset_with_lam(
            dataset_id="mitdb",
            filters={"record_range": [100, 109]}
        )

        print(f"   Dataset: {result['dataset'].id}")
        print(f"   Resonance Quality: {result['resonance_quality']:.4f}")
        print(f"   LAM Action ID: {result['lam_action']['action_record']['action_id']}")

        # Example 2: Create LAM-tracked experiment
        print("\n2. Creating experiment with LAM tracking...")
        exp_result = await bridge.create_lam_experiment(
            experiment_name="cardiac_analysis_lam_v1",
            dataset_specs=[
                {'dataset_id': 'mitdb'},
                {'dataset_id': 'ptbdb'}
            ]
        )

        print(f"   Experiment: {exp_result['experiment_manager'].experiment_name}")
        print(f"   Avg Resonance Quality: {exp_result['resonance_quality']:.4f}")
        print(f"   Stability Guaranteed: {exp_result['lam_state']['lipschitz_constant'] < 1.0}")

        # Example 3: LAM recommendations
        print("\n3. Getting LAM-enhanced recommendations...")
        recommendations = await bridge.recommend_datasets_with_lam(
            query="ECG arrhythmia detection",
            max_results=5
        )

        print(f"   Found {len(recommendations['recommendations'])} recommendations")
        for i, rec in enumerate(recommendations['recommendations'][:3], 1):
            print(f"   {i}. {rec['dataset_info'].title}")
            print(f"      Resonance Quality: {rec['resonance_quality']:.4f}")
            print(f"      Attractor Distance: {rec['attractor_distance']:.2f}")

        # Example 4: Resonance summary
        print("\n4. LAM Resonance Summary...")
        summary = bridge.get_resonance_summary()
        print(f"   Lipschitz Constant: {summary['stability']['lipschitz_constant']:.9f}")
        print(f"   System Stable: {summary['stability']['stable']}")
        print(f"   Total Dataset Operations: {summary['operations']['total_dataset_operations']}")
        print(f"   Lightfoot Constant: {summary['quantum_metrics']['lightfoot_constant']}")
        print(f"   Donte Attractor: {summary['quantum_metrics']['donte_attractor']:.4f}")

    asyncio.run(main())
