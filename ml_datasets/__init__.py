"""
MotorHandPro ML Datasets Module

Comprehensive medical, robotics, and sensor dataset infrastructure with:
- 30+ high-quality datasets (PhysioNet, NCBI, KITTI, nuScenes, etc.)
- LAM-inspired RAG agent for knowledge retrieval
- 50+ university research repository connections
- Human Genome Project integration
- Kaggle & UCI ML Repository (50K+ datasets via Data Concierge)
- UAV/LiDAR sensor datasets for robotics and autonomous systems
- On-demand loading with quality validation
- Intelligent Data Concierge for unified dataset discovery
- Per-experiment dataset isolation and tracking
- Bulk dataset loading (4+ datasets with parallel processing)
"""

from ml_datasets.core.dataset_manager import DatasetManager, Dataset
from ml_datasets.core.data_concierge import DataConcierge
from ml_datasets.core.experiment_dataset_manager import ExperimentDatasetManager
from ml_datasets.core.bulk_dataset_loader import BulkDatasetLoader
from ml_datasets.rag.lam_rag_agent import LAMRAGAgent
from ml_datasets.sources.kaggle_uci_connector import KaggleUCIConnector
from ml_datasets.sources.uav_lidar_connector import UAVLiDARConnector

__version__ = "2.1.0"
__all__ = [
    'DatasetManager',
    'Dataset',
    'LAMRAGAgent',
    'DataConcierge',
    'ExperimentDatasetManager',
    'BulkDatasetLoader',
    'KaggleUCIConnector',
    'UAVLiDARConnector'
]
