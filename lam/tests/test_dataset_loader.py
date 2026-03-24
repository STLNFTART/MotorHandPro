#!/usr/bin/env python3
"""
Tests for lam/integrations/dataset_loader.py - EMGDatasetLoader
"""
import unittest
import json
import sys
import tempfile
import numpy as np
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

from integrations.dataset_loader import EMGDatasetLoader, DatasetMetadata


class TestEMGDatasetLoaderInit(unittest.TestCase):
    """Test EMGDatasetLoader initialization"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_cache_dir_created(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path / "cache")
        self.assertTrue((self.tmp_path / "cache").exists())

    def test_default_cache_dir_set(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        self.assertEqual(loader.cache_dir, self.tmp_path)

    def test_dataset_registry_not_empty(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        self.assertGreater(len(loader.DATASET_REGISTRY), 0)

    def test_metadata_cache_initialized(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        self.assertIsInstance(loader.metadata_cache, dict)


class TestDatasetRegistry(unittest.TestCase):
    """Test the dataset registry contents"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_emg_gesture_recognition_registered(self):
        self.assertIn('emg_gesture_recognition_2024', self.loader.DATASET_REGISTRY)

    def test_multi_day_emg_registered(self):
        self.assertIn('multi_day_emg_2022', self.loader.DATASET_REGISTRY)

    def test_high_density_semg_registered(self):
        self.assertIn('high_density_semg_2021', self.loader.DATASET_REGISTRY)

    def test_ninapro_registered(self):
        self.assertIn('ninapro_db1', self.loader.DATASET_REGISTRY)

    def test_meganepro_registered(self):
        self.assertIn('meganepro_mds1', self.loader.DATASET_REGISTRY)

    def test_all_entries_are_dataset_metadata(self):
        for key, value in self.loader.DATASET_REGISTRY.items():
            self.assertIsInstance(value, DatasetMetadata, f"{key} is not DatasetMetadata")

    def test_emg_2024_has_correct_subjects(self):
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(meta.num_subjects, 8)

    def test_emg_2024_has_correct_channels(self):
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(meta.num_channels, 8)

    def test_emg_2024_has_correct_gestures(self):
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(meta.num_gestures, 6)

    def test_ninapro_sample_rate(self):
        meta = self.loader.DATASET_REGISTRY['ninapro_db1']
        self.assertEqual(meta.sample_rate, 100)

    def test_high_density_semg_channels(self):
        meta = self.loader.DATASET_REGISTRY['high_density_semg_2021']
        self.assertEqual(meta.num_channels, 128)


class TestListDatasets(unittest.TestCase):
    """Test list_datasets method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_returns_list(self):
        datasets = self.loader.list_datasets()
        self.assertIsInstance(datasets, list)

    def test_returns_all_registered_datasets(self):
        datasets = self.loader.list_datasets()
        self.assertEqual(len(datasets), len(self.loader.DATASET_REGISTRY))

    def test_each_entry_has_id(self):
        for ds in self.loader.list_datasets():
            self.assertIn('id', ds)

    def test_each_entry_has_name(self):
        for ds in self.loader.list_datasets():
            self.assertIn('name', ds)

    def test_each_entry_has_subjects(self):
        for ds in self.loader.list_datasets():
            self.assertIn('subjects', ds)

    def test_each_entry_has_channels(self):
        for ds in self.loader.list_datasets():
            self.assertIn('channels', ds)

    def test_each_entry_has_gestures(self):
        for ds in self.loader.list_datasets():
            self.assertIn('gestures', ds)

    def test_each_entry_has_cached_flag(self):
        for ds in self.loader.list_datasets():
            self.assertIn('cached', ds)

    def test_cached_false_when_not_downloaded(self):
        for ds in self.loader.list_datasets():
            self.assertFalse(ds['cached'])

    def test_dataset_ids_match_registry_keys(self):
        registry_keys = set(self.loader.DATASET_REGISTRY.keys())
        list_ids = {ds['id'] for ds in self.loader.list_datasets()}
        self.assertEqual(registry_keys, list_ids)


class TestIsCached(unittest.TestCase):
    """Test _is_cached method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.loader = EMGDatasetLoader(cache_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_not_cached_when_no_dir(self):
        self.assertFalse(self.loader._is_cached('emg_gesture_recognition_2024'))

    def test_not_cached_when_dir_exists_but_no_processed_file(self):
        (self.tmp_path / 'emg_gesture_recognition_2024').mkdir()
        self.assertFalse(self.loader._is_cached('emg_gesture_recognition_2024'))

    def test_cached_when_processed_file_exists(self):
        dataset_dir = self.tmp_path / 'emg_gesture_recognition_2024'
        dataset_dir.mkdir()
        (dataset_dir / 'processed.npy').write_bytes(b'fake data')
        self.assertTrue(self.loader._is_cached('emg_gesture_recognition_2024'))


class TestLoadDataset(unittest.TestCase):
    """Test load_dataset method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_returns_dict(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIsInstance(data, dict)

    def test_unknown_dataset_raises_value_error(self):
        with self.assertRaises(ValueError):
            self.loader.load_dataset('nonexistent_dataset')

    def test_result_has_data_key(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIn('data', data)

    def test_result_has_labels_key(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIn('labels', data)

    def test_result_has_sample_rate_key(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIn('sample_rate', data)

    def test_result_has_num_channels_key(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIn('num_channels', data)

    def test_result_has_gesture_names_key(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIn('gesture_names', data)

    def test_data_is_numpy_array(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIsInstance(data['data'], np.ndarray)

    def test_labels_is_numpy_array(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        self.assertIsInstance(data['labels'], np.ndarray)

    def test_data_shape_has_correct_channels(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(data['data'].shape[1], meta.num_channels)

    def test_sample_rate_matches_metadata(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(data['sample_rate'], meta.sample_rate)

    def test_gesture_names_length_matches_num_gestures(self):
        data = self.loader.load_dataset('emg_gesture_recognition_2024')
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(len(data['gesture_names']), meta.num_gestures)

    def test_second_load_uses_cache(self):
        """Loading twice should use cached file"""
        data1 = self.loader.load_dataset('emg_gesture_recognition_2024')
        data2 = self.loader.load_dataset('emg_gesture_recognition_2024')
        np.testing.assert_array_equal(data1['data'], data2['data'])


class TestGenerateSyntheticDataset(unittest.TestCase):
    """Test _generate_synthetic_dataset method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))
        self.meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_returns_dict(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertIsInstance(data, dict)

    def test_data_is_float32(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertEqual(data['data'].dtype, np.float32)

    def test_labels_is_int32(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertEqual(data['labels'].dtype, np.int32)

    def test_num_channels_correct(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertEqual(data['data'].shape[1], self.meta.num_channels)

    def test_labels_range_correct(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertGreaterEqual(data['labels'].min(), 0)
        self.assertLess(data['labels'].max(), self.meta.num_gestures)

    def test_gesture_names_list(self):
        data = self.loader._generate_synthetic_dataset(self.meta)
        self.assertIsInstance(data['gesture_names'], list)
        self.assertEqual(len(data['gesture_names']), self.meta.num_gestures)


class TestFilterDataset(unittest.TestCase):
    """Test _filter_dataset method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))
        # Load a dataset to get a real data_dict
        self.data_dict = self.loader.load_dataset('emg_gesture_recognition_2024')

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_filter_by_subject_id(self):
        filtered = self.loader._filter_dataset(self.data_dict, subject_ids=[0], gestures=None)
        # Should have only subject 0 data
        self.assertTrue(np.all(filtered['subject_ids'] == 0))

    def test_filter_by_gesture(self):
        gesture_name = self.data_dict['gesture_names'][0]
        filtered = self.loader._filter_dataset(self.data_dict, subject_ids=None, gestures=[gesture_name])
        # All labels should be 0 (first gesture)
        self.assertTrue(np.all(filtered['labels'] == 0))

    def test_filter_preserves_data_structure(self):
        filtered = self.loader._filter_dataset(self.data_dict, subject_ids=[0], gestures=None)
        self.assertIn('data', filtered)
        self.assertIn('labels', filtered)
        self.assertIn('subject_ids', filtered)
        self.assertIn('sample_rate', filtered)
        self.assertIn('num_channels', filtered)
        self.assertIn('gesture_names', filtered)

    def test_filter_no_criteria_returns_same_size(self):
        filtered = self.loader._filter_dataset(self.data_dict, subject_ids=None, gestures=None)
        self.assertEqual(len(filtered['data']), len(self.data_dict['data']))

    def test_filter_unknown_gesture_returns_empty(self):
        filtered = self.loader._filter_dataset(
            self.data_dict, subject_ids=None, gestures=['nonexistent_gesture']
        )
        self.assertEqual(len(filtered['data']), 0)


class TestGetDatasetStatistics(unittest.TestCase):
    """Test get_dataset_statistics method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.loader = EMGDatasetLoader(cache_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_returns_dict(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIsInstance(stats, dict)

    def test_has_total_samples(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('total_samples', stats)

    def test_has_num_channels(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('num_channels', stats)

    def test_has_sample_rate(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('sample_rate', stats)

    def test_has_gestures(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('gestures', stats)

    def test_has_subjects(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('subjects', stats)

    def test_has_data_range(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('data_range', stats)
        self.assertIn('min', stats['data_range'])
        self.assertIn('max', stats['data_range'])
        self.assertIn('mean', stats['data_range'])
        self.assertIn('std', stats['data_range'])

    def test_has_samples_per_gesture(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertIn('samples_per_gesture', stats)
        self.assertIsInstance(stats['samples_per_gesture'], dict)

    def test_num_channels_matches_metadata(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        meta = self.loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        self.assertEqual(stats['num_channels'], meta.num_channels)

    def test_total_samples_positive(self):
        stats = self.loader.get_dataset_statistics('emg_gesture_recognition_2024')
        self.assertGreater(stats['total_samples'], 0)

    def test_unknown_dataset_raises(self):
        with self.assertRaises(ValueError):
            self.loader.get_dataset_statistics('unknown_dataset')


class TestMetadataCache(unittest.TestCase):
    """Test metadata cache persistence"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_metadata_cache_file_created(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        meta = loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        # _save_dataset_metadata expects the dataset directory to already exist
        dataset_dir = self.tmp_path / 'emg_gesture_recognition_2024'
        dataset_dir.mkdir(parents=True, exist_ok=True)
        loader._save_dataset_metadata('emg_gesture_recognition_2024', meta)
        self.assertTrue((dataset_dir / 'metadata.json').exists())

    def test_saved_metadata_is_valid_json(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        meta = loader.DATASET_REGISTRY['emg_gesture_recognition_2024']
        # _save_dataset_metadata expects the dataset directory to already exist
        dataset_dir = self.tmp_path / 'emg_gesture_recognition_2024'
        dataset_dir.mkdir(parents=True, exist_ok=True)
        loader._save_dataset_metadata('emg_gesture_recognition_2024', meta)
        meta_file = dataset_dir / 'metadata.json'
        with open(meta_file) as f:
            data = json.load(f)
        self.assertIn('name', data)
        self.assertIn('num_subjects', data)
        self.assertIn('num_channels', data)

    def test_load_metadata_cache_when_file_absent(self):
        loader = EMGDatasetLoader(cache_dir=self.tmp_path)
        self.assertIsInstance(loader.metadata_cache, dict)


if __name__ == '__main__':
    unittest.main(verbosity=2)