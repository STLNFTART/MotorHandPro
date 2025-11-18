# MotorHandPro ML Dataset Infrastructure

## Overview

This infrastructure provides on-demand access to high-quality, multi-disciplinary medical and sensor datasets integrated with the LAM (Large Action Model) framework and RAG (Retrieval-Augmented Generation) agents.

## Architecture

```
ml_datasets/
├── core/
│   ├── dataset_manager.py        # Central dataset orchestration
│   ├── dataset_loader.py          # On-demand dataset loading
│   ├── data_validator.py          # Quality & diversity validation
│   └── dataset_catalog.py         # Dataset registry & metadata
├── sources/
│   ├── physionet_connector.py     # PhysioNet datasets (ECG, EEG, EMG)
│   ├── ncbi_connector.py          # NCBI Genomic datasets
│   ├── university_connectors.py   # Top 50 research universities
│   └── hgp_connector.py           # Human Genome Project integration
├── rag/
│   ├── lam_rag_agent.py          # LAM-inspired RAG agent
│   ├── vector_store.py            # Embeddings & semantic search
│   └── knowledge_retriever.py     # Multi-source retrieval
├── storage/
│   ├── dataset_repository.py      # Local dataset caching
│   └── metadata_store.py          # PostgreSQL integration
└── config/
    ├── dataset_catalog.yaml       # Dataset definitions
    └── university_endpoints.yaml  # Research API configurations
```

## Dataset Categories

### 1. Cardiovascular & Cardiac Data
- **MIT-BIH Arrhythmia Database**: 48 ECG recordings, 30-minute segments
- **PhysioNet MIMIC-III**: ICU patient data, vital signs
- **PTB Diagnostic ECG Database**: 549 records, 15 diagnostic classes

### 2. Neurological Data
- **Sleep-EDF Database**: 197 whole-night polysomnography recordings
- **EEG Motor Movement/Imagery**: 109 subjects, motor tasks
- **CHB-MIT Scalp EEG Database**: Pediatric seizure recordings

### 3. Muscular & Motion Data
- **EMG Dataset**: Surface electromyography signals
- **Wearable Sensor Data**: Accelerometer, gyroscope, temperature

### 4. Genomic Data
- **NCBI Datasets API**: Reference genomes, annotations
- **1000 Genomes Project**: Population genomic variation
- **Human Genome Project**: Complete human genome sequences

### 5. Multi-Modal Medical Data
- **PhysioNet Challenge Datasets**: Annual competition datasets
- **NIST Genome in a Bottle**: High-confidence genomic benchmarks

### 6. UAV & Drone Sensor Data (NEW!)
- **MUN-FRL Dataset**: Aerial Visual-Inertial-LiDAR (DJI-M600, VLP-16, 5km flights)
- **MARS-LVIG**: Multi-sensor Aerial SLAM (21 sequences, 577K m² coverage)
- **Multi-LiDAR UAV Tracking**: Velodyne HDL-64 + Livox solid-state LiDARs
- **UAVScenes**: Multi-modal aerial imagery and annotations

### 7. LiDAR & Autonomous Driving (NEW!)
- **KITTI**: 6 hours traffic, Velodyne HDL-64E, stereo cameras, GPS/IMU
- **SemanticKITTI**: 43K scans with 28-class semantic segmentation
- **nuScenes**: 1000 scenes, 32-beam LiDAR, 6 cameras, 5 radars, 1.4M samples
- **Waymo Open**: 230K frames, 5 LiDARs, 5 cameras, motion prediction
- **ApolloScape**: Baidu's 200K frames, 3D detection and tracking
- **Audi A2D2**: 41K sequences, 5 LiDARs, 6 cameras, 2.3TB

### 8. Robotics & SLAM (NEW!)
- **TUM RGB-D**: Kinect sensor, Vicon motion capture ground truth
- **ETH3D**: High-res stereo, laser scanner ground truth

### 9. Kaggle & UCI ML Repository (50K+ Datasets via Data Concierge!) (NEW!)
- **Kaggle**: 50,000+ datasets across all domains
- **UCI ML Repository**: 600+ curated ML datasets
- **Aggregate Search**: Unified discovery across all repositories
- **Popular Datasets**: Heart Disease, Diabetes, Brain MRI, COVID-19 CT, EMG Gestures

## Intelligent Data Concierge (NEW!)

The **Data Concierge** is your intelligent assistant for finding the perfect dataset:

```python
from ml_datasets import DataConcierge

concierge = DataConcierge()

# Natural language search across ALL repositories
recommendation = await concierge.find_datasets(
    query="hand gesture recognition using EMG sensors",
    domain="medical",
    data_types=["time_series", "sensor"],
    max_results=20,
    include_universities=True
)

print(recommendation.recommendation_summary)
print(f"Best match: {recommendation.best_match.title}")

# Search across:
# - Kaggle (50K+ datasets)
# - UCI ML Repository (600+ datasets)
# - University repositories (50+ institutions)
# - Local curated catalog (30+ datasets)
# - PhysioNet, NCBI, and more
```

### Data Concierge Features
- **Natural Language Queries**: "find cardiac arrhythmia datasets with ECG signals"
- **Multi-Repository Aggregation**: Search Kaggle, UCI, universities simultaneously
- **Intelligent Ranking**: Combines relevance, quality, popularity, recency
- **Task-Based Recommendations**: Get datasets for classification, regression, etc.
- **Domain Filtering**: Medical, robotics, finance, computer vision, NLP

## Key Features

### On-Demand Loading
- Lazy loading with intelligent caching
- Automatic dataset versioning
- Incremental updates from source repositories

### Data Quality Validation
- **Completeness**: Missing value analysis
- **Consistency**: Cross-field validation
- **Diversity**: Demographic & temporal distribution
- **Granularity**: Sampling rate verification

### LAM-RAG Integration
- Quantum-resonance enhanced retrieval
- Lightfoot constant (λ=0.16905) decay for temporal weighting
- Semantic search across research papers & datasets
- Multi-hop reasoning over knowledge graphs

### University Research Hooks
- API endpoints for 50 top research universities
- Standardized data sharing protocols (FAIR principles)
- Authenticated access with rate limiting
- Real-time collaboration features

### Human Genome Project Integration
- Direct NCBI API access
- GA4GH (Global Alliance for Genomics and Health) compatibility
- VCF/BAM/FASTA format support
- Annotation pipeline integration

## Usage

### Basic Dataset Loading

```python
from ml_datasets.core.dataset_manager import DatasetManager

# Initialize dataset manager
dm = DatasetManager()

# Load MIT-BIH Arrhythmia dataset
ecg_data = await dm.load_dataset(
    dataset_id="mitdb",
    version="1.0.0",
    filters={"record_range": [100, 109]}
)

# Access data
signals = ecg_data.signals  # NumPy array
annotations = ecg_data.annotations  # Structured annotations
metadata = ecg_data.metadata  # Dataset information
```

### RAG-Enhanced Queries

```python
from ml_datasets.rag.lam_rag_agent import LAMRAGAgent

# Initialize RAG agent with LAM integration
rag = LAMRAGAgent(lam_core=primal_lam)

# Query across datasets and research papers
result = await rag.query(
    question="What are the optimal preprocessing steps for EMG signals in hand prosthetics?",
    sources=["physionet", "pubmed", "ieee", "university_repos"],
    context_window=5000
)

# Get actionable insights
preprocessing_pipeline = result.extract_pipeline()
relevant_papers = result.citations
code_examples = result.code_snippets
```

### University Data Access

```python
from ml_datasets.sources.university_connectors import UniversityHubConnector

# Connect to university research repositories
hub = UniversityHubConnector()

# Search across 50 universities
datasets = await hub.search(
    keywords=["prosthetics", "EMG", "motor control"],
    institutions=["MIT", "Stanford", "Johns Hopkins"],
    data_types=["time_series", "imaging"]
)

# Request access to private datasets
await hub.request_access(
    dataset_id="stanford_motor_lab_2024",
    justification="MotorHandPro clinical trial integration"
)
```

### Per-Experiment Dataset Loading (NEW!)

```python
from ml_datasets import ExperimentDatasetManager

# Create experiment with isolated dataset context
exp_manager = ExperimentDatasetManager(
    experiment_name="cardiac_arrhythmia_classification_v1"
)

# Load single dataset for this experiment
mitdb = await exp_manager.load_dataset(
    dataset_id="mitdb",
    filters={"record_range": [100, 109]},
    lock_version=True  # Lock for reproducibility
)

# Load multiple datasets for experiment
dataset_specs = [
    {'dataset_id': 'mitdb', 'filters': {'record_range': [100, 104]}},
    {'dataset_id': 'ptbdb', 'version': '1.0.0'},
    {'dataset_id': 'sleep_edf'}
]

datasets = await exp_manager.load_multiple_datasets(
    dataset_specs=dataset_specs,
    parallel=True
)

# Set experiment parameters
exp_manager.set_parameters({
    'model': 'CNN',
    'learning_rate': 0.001,
    'batch_size': 32
})

# Add results
exp_manager.add_result('accuracy', 0.95)
exp_manager.add_result('f1_score', 0.93)

# Complete experiment
exp_manager.complete_experiment(status="completed")

# Get reproducibility config
repro_config = exp_manager.get_reproducibility_config()
# Includes exact dataset versions and filters used

# List all experiments
all_experiments = ExperimentDatasetManager.list_experiments()
```

### Bulk Dataset Loading (4+ Datasets) (NEW!)

```python
from ml_datasets import BulkDatasetLoader

# Initialize bulk loader with 10 parallel workers
bulk_loader = BulkDatasetLoader(max_parallel=10)

# Define 20 datasets to load
dataset_specs = [
    {'dataset_id': 'mitdb', 'filters': {'record_range': [100, 109]}},
    {'dataset_id': 'ptbdb'},
    {'dataset_id': 'sleep_edf'},
    {'dataset_id': 'kitti'},
    {'dataset_id': 'nuscenes'},
    {'dataset_id': 'waymo_open'},
    {'dataset_id': 'semantic_kitti'},
    {'dataset_id': 'apollo_scape'},
    {'dataset_id': 'mun_frl'},
    {'dataset_id': 'mars_lvig'},
    # ... up to 20+ datasets
]

# Progress callback
def show_progress(progress):
    print(f"Progress: {progress.percentage:.1f}% | "
          f"Completed: {progress.completed}/{progress.total} | "
          f"ETA: {progress.estimated_remaining_seconds:.0f}s")

# Execute bulk load with real-time progress
results = await bulk_loader.bulk_load(
    dataset_specs=dataset_specs,
    progress_callback=show_progress,
    resume=True  # Resume interrupted downloads
)

# Results summary
print(f"Loaded: {len(results['datasets'])} datasets")
print(f"Failed: {len(results['failed'])} datasets")
print(f"Success Rate: {results['summary']['success_rate']:.1f}%")
print(f"Total Time: {results['summary']['total_time_seconds']:.1f}s")

# Access loaded datasets
for dataset_id, dataset in results['datasets'].items():
    print(f"{dataset_id}: {dataset.metadata['name']}")
```

### LAM Integration - Quantum Resonance Dataset Scoring (NEW!)

```python
from ml_datasets import LAMDatasetBridge

# Initialize LAM-Dataset bridge
bridge = LAMDatasetBridge()  # Automatically creates LAM instance

# Load dataset as LAM action with quantum resonance tracking
result = await bridge.load_dataset_with_lam(
    dataset_id="mitdb",
    filters={"record_range": [100, 109]}
)

dataset = result['dataset']
lam_action = result['lam_action']
resonance_quality = result['resonance_quality']

print(f"Dataset: {dataset.id}")
print(f"Resonance Quality: {resonance_quality:.4f}")
print(f"Attractor Distance: {lam_action['action_record']['details']['attractor_distance']:.2f}")

# LAM uses:
# - Lightfoot constant (λ=0.16905) for exponential decay
# - Donte constant (D=149.999...) as fixed-point attractor
# - Lipschitz constant < 1.0 for stability guarantee
# - Quantum resonance for dataset quality scoring

# Create experiment with LAM tracking
exp_result = await bridge.create_lam_experiment(
    experiment_name="cardiac_analysis_lam_v1",
    dataset_specs=[
        {'dataset_id': 'mitdb'},
        {'dataset_id': 'ptbdb'},
        {'dataset_id': 'sleep_edf'}
    ]
)

exp_manager = exp_result['experiment_manager']
avg_resonance = exp_result['resonance_quality']

print(f"Experiment: {exp_manager.experiment_name}")
print(f"Avg Resonance Quality: {avg_resonance:.4f}")
print(f"Lipschitz Constant: {exp_result['lam_state']['lipschitz_constant']:.9f}")
print(f"Stability Guaranteed: {exp_result['lam_state']['lipschitz_constant'] < 1.0}")

# Get LAM-enhanced dataset recommendations
recommendations = await bridge.recommend_datasets_with_lam(
    query="ECG arrhythmia detection",
    max_results=10
)

for rec in recommendations['recommendations'][:5]:
    print(f"{rec['dataset_info'].title}")
    print(f"  Resonance Quality: {rec['resonance_quality']:.4f}")
    print(f"  Attractor Distance: {rec['attractor_distance']:.2f}")

# Get resonance summary
summary = bridge.get_resonance_summary()
print(f"System Stable: {summary['stability']['stable']}")
print(f"Total Operations: {summary['operations']['total_dataset_operations']}")
print(f"Lightfoot Constant: {summary['quantum_metrics']['lightfoot_constant']}")
print(f"Donte Attractor: {summary['quantum_metrics']['donte_attractor']:.4f}")
```

### Genomic Data Integration

```python
from ml_datasets.sources.hgp_connector import HumanGenomeConnector

# Access Human Genome Project data
hgp = HumanGenomeConnector()

# Query specific genes related to motor function
genes = await hgp.query_genes(
    gene_symbols=["DMPK", "HTT", "FMR1"],
    include_variants=True,
    population_frequencies=True
)

# Integrate with patient data
patient_analysis = await hgp.analyze_patient_genome(
    vcf_file="/data/patient_001.vcf",
    phenotype="motor_disorder",
    reference_build="GRCh38"
)
```

## Data Quality Metrics

The system tracks:
- **Diversity Score**: 0-1 scale measuring demographic, temporal, and signal diversity
- **Completeness**: Percentage of non-null values across critical fields
- **Granularity**: Time resolution (samples/second) for time-series data
- **Provenance**: Full lineage tracking from source to processed dataset
- **Validation Status**: Automated quality checks (PASSED/FAILED)

## Compliance & Security

- **HIPAA Compliance**: De-identification of patient data
- **IRB Approval Tracking**: Institutional review board documentation
- **Access Control**: Role-based permissions (RBAC)
- **Audit Logging**: Complete activity tracking
- **Data Use Agreements**: Automated DUA management

## Integration with LAM Framework

The dataset infrastructure integrates with the Primal LAM system:

1. **Quantum Resonance**: Dataset quality scores use Lightfoot decay for temporal relevance
2. **Attractor Dynamics**: Convergence toward high-quality, diverse datasets (Donte constant)
3. **Stability Guarantees**: Lipschitz constant ensures bounded dataset selection
4. **Real-time Updates**: MQTT integration for streaming dataset updates
5. **Experiment Tracking**: All dataset operations logged to experiment database

## Getting Started

1. Install dependencies:
   ```bash
   pip install -r ml_datasets/requirements.txt
   ```

2. Configure credentials:
   ```bash
   export PHYSIONET_USERNAME="your_username"
   export PHYSIONET_PASSWORD="your_password"
   export NCBI_API_KEY="your_ncbi_key"
   ```

3. Initialize database:
   ```bash
   python -m ml_datasets.setup_database
   ```

4. Run test suite:
   ```bash
   pytest ml_datasets/tests/
   ```

## Dataset Catalog

See `config/dataset_catalog.yaml` for the complete list of 50+ integrated datasets across:
- 15+ physiological signal types
- 10+ genomic databases
- 50+ university research repositories
- 100+ specialized medical domains

## Support

For issues or questions:
- GitHub Issues: https://github.com/STLNFTART/MotorHandPro/issues
- Documentation: https://motorhandpro.readthedocs.io/ml-datasets
- Email: datasets@motorhandpro.org
