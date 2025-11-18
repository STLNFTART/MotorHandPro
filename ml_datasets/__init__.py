"""
MotorHandPro ML Datasets Module

Comprehensive medical and sensor dataset infrastructure with:
- 15+ high-quality datasets (PhysioNet, NCBI, etc.)
- LAM-inspired RAG agent for knowledge retrieval
- 50+ university research repository connections
- Human Genome Project integration
- On-demand loading with quality validation
"""

from ml_datasets.core.dataset_manager import DatasetManager, Dataset
from ml_datasets.rag.lam_rag_agent import LAMRAGAgent

__version__ = "1.0.0"
__all__ = ['DatasetManager', 'Dataset', 'LAMRAGAgent']
