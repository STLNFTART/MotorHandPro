#!/usr/bin/env python3
"""
Tests for lam/integrations/benchmark_publisher.py - BenchmarkPublisher
"""
import unittest
import json
import sys
import types as _types
import tempfile
from pathlib import Path
from datetime import datetime

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

# pandas is an optional dependency; if missing, install a minimal mock so the
# benchmark_publisher module can be imported (methods that actually use it are
# skipped via the requires_pandas decorator below).
try:
    import pandas
    PANDAS_AVAILABLE = True
except ImportError:
    PANDAS_AVAILABLE = False
    _pd_mock = _types.ModuleType('pandas')

    class _MockDataFrame:
        def __init__(self, data):
            self._data = data

        def __getitem__(self, key):
            return _MockDataFrame({})

        def __len__(self):
            return 0

        def unique(self):
            return []

        def tolist(self):
            return []

        def mean(self):
            return float('nan')

        def min(self):
            return 0.0

        def max(self):
            return 0.0

        def idxmax(self):
            return 0

        def loc(self, *args):
            return {}

        def __eq__(self, other):
            return _MockDataFrame({})

        def iterrows(self):
            return iter([])

    _pd_mock.DataFrame = _MockDataFrame
    sys.modules['pandas'] = _pd_mock

from integrations.benchmark_publisher import BenchmarkPublisher, BenchmarkResult, BenchmarkSuite

# Decorator: skip tests that actually call pandas code if pandas is absent
requires_pandas = unittest.skipUnless(PANDAS_AVAILABLE, "pandas not installed")


def make_benchmark_result(**kwargs):
    """Create a BenchmarkResult with defaults"""
    defaults = dict(
        benchmark_id="test-id-001",
        timestamp=datetime.now(),
        dataset_name="test_dataset",
        model_name="TestModel",
        environment="none",
        task="gesture_recognition",
        metric_name="accuracy",
        metric_value=0.85,
        unit="confidence",
        hardware="CPU",
        radiation_dose_msv=0.0,
        metadata={}
    )
    defaults.update(kwargs)
    return BenchmarkResult(**defaults)


def make_benchmark_suite(results=None, **kwargs):
    """Create a BenchmarkSuite with defaults"""
    if results is None:
        results = [make_benchmark_result()]
    defaults = dict(
        suite_id="test-suite-001",
        suite_name="Test Suite",
        version="1.0",
        timestamp=datetime.now(),
        system_info={"platform": "test"},
        results=results,
        summary_statistics={}
    )
    defaults.update(kwargs)
    return BenchmarkSuite(**defaults)


class TestBenchmarkPublisherInit(unittest.TestCase):
    """Test BenchmarkPublisher initialization"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_output_dir_created(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path / "bench")
        self.assertTrue((self.tmp_path / "bench").exists())

    def test_results_subdir_created(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertTrue((self.tmp_path / "results").exists())

    def test_reports_subdir_created(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertTrue((self.tmp_path / "reports").exists())

    def test_figures_subdir_created(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertTrue((self.tmp_path / "figures").exists())

    def test_leaderboards_subdir_created(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertTrue((self.tmp_path / "leaderboards").exists())

    def test_initial_results_empty(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertEqual(publisher.results, [])

    def test_output_dir_stored(self):
        publisher = BenchmarkPublisher(output_dir=self.tmp_path)
        self.assertEqual(publisher.output_dir, self.tmp_path)


class TestAddResult(unittest.TestCase):
    """Test add_result method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.publisher = BenchmarkPublisher(output_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_add_result_increments_count(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertEqual(len(self.publisher.results), 1)

    def test_add_multiple_results(self):
        for i in range(5):
            self.publisher.add_result(
                dataset_name=f"dataset_{i}",
                model_name="model",
                environment="none",
                task="gesture",
                metric_name="accuracy",
                metric_value=0.8 + i * 0.01
            )
        self.assertEqual(len(self.publisher.results), 5)

    def test_result_is_benchmark_result_instance(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertIsInstance(self.publisher.results[0], BenchmarkResult)

    def test_result_stores_dataset_name(self):
        self.publisher.add_result(
            dataset_name="my_dataset",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertEqual(self.publisher.results[0].dataset_name, "my_dataset")

    def test_result_stores_metric_value(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.87654
        )
        self.assertAlmostEqual(self.publisher.results[0].metric_value, 0.87654, places=5)

    def test_result_stores_radiation_dose(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="leo",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.75,
            radiation_dose_msv=100.0
        )
        self.assertEqual(self.publisher.results[0].radiation_dose_msv, 100.0)

    def test_result_has_generated_benchmark_id(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertIsNotNone(self.publisher.results[0].benchmark_id)
        self.assertGreater(len(self.publisher.results[0].benchmark_id), 0)

    def test_result_has_timestamp(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertIsInstance(self.publisher.results[0].timestamp, datetime)

    def test_default_metadata_is_empty_dict(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9
        )
        self.assertEqual(self.publisher.results[0].metadata, {})

    def test_custom_metadata_stored(self):
        self.publisher.add_result(
            dataset_name="test",
            model_name="model",
            environment="none",
            task="gesture",
            metric_name="accuracy",
            metric_value=0.9,
            metadata={"notes": "pilot run"}
        )
        self.assertEqual(self.publisher.results[0].metadata, {"notes": "pilot run"})

    def test_benchmark_ids_are_different_for_different_calls(self):
        self.publisher.add_result("ds1", "m", "e", "t", "accuracy", 0.8)
        self.publisher.add_result("ds2", "m", "e", "t", "accuracy", 0.9)
        id1 = self.publisher.results[0].benchmark_id
        id2 = self.publisher.results[1].benchmark_id
        self.assertNotEqual(id1, id2)

    def test_add_result_default_hardware_is_cpu(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].hardware, "CPU")

    def test_add_result_default_radiation_zero(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].radiation_dose_msv, 0.0)

    def test_add_result_default_unit_empty_string(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].unit, "")


@requires_pandas
class TestComputeSummaryStatistics(unittest.TestCase):
    """Test _compute_summary_statistics method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.publisher = BenchmarkPublisher(output_dir=Path(self.tmpdir.name))

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_empty_results_returns_empty_dict(self):
        stats = self.publisher._compute_summary_statistics()
        self.assertEqual(stats, {})

    def test_returns_dict(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        self.assertIsInstance(stats, dict)

    def test_has_total_benchmarks(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('total_benchmarks', stats)

    def test_total_benchmarks_count_correct(self):
        self.publisher.add_result("ds1", "m", "e", "t", "accuracy", 0.85)
        self.publisher.add_result("ds2", "m", "e", "t", "accuracy", 0.90)
        stats = self.publisher._compute_summary_statistics()
        self.assertEqual(stats['total_benchmarks'], 2)

    def test_has_datasets(self):
        self.publisher.add_result("my_ds", "m", "e", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('datasets', stats)
        self.assertIn('my_ds', stats['datasets'])

    def test_has_environments(self):
        self.publisher.add_result("ds", "m", "leo", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('environments', stats)
        self.assertIn('leo', stats['environments'])

    def test_has_avg_accuracy(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.80)
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.90)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('avg_accuracy', stats)
        self.assertAlmostEqual(stats['avg_accuracy'], 0.85, places=5)

    def test_has_avg_response_time(self):
        self.publisher.add_result("ds", "m", "e", "t", "response_time", 100.0)
        self.publisher.add_result("ds", "m", "e", "t", "response_time", 200.0)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('avg_response_time_ms', stats)
        self.assertAlmostEqual(stats['avg_response_time_ms'], 150.0, places=1)

    def test_has_radiation_range(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.85, radiation_dose_msv=0.0)
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.70, radiation_dose_msv=200.0)
        stats = self.publisher._compute_summary_statistics()
        self.assertIn('radiation_range_msv', stats)
        self.assertEqual(stats['radiation_range_msv']['min'], 0.0)
        self.assertEqual(stats['radiation_range_msv']['max'], 200.0)

    def test_datasets_list_unique(self):
        self.publisher.add_result("ds1", "m", "e", "t", "accuracy", 0.85)
        self.publisher.add_result("ds1", "m", "e", "t", "accuracy", 0.90)
        self.publisher.add_result("ds2", "m", "e", "t", "accuracy", 0.80)
        stats = self.publisher._compute_summary_statistics()
        self.assertEqual(len(stats['datasets']), 2)


@requires_pandas
class TestSaveSuite(unittest.TestCase):
    """Test save_suite method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.publisher = BenchmarkPublisher(output_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _build_suite(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        return make_benchmark_suite(
            suite_id="save-test-001",
            results=self.publisher.results,
            summary_statistics=stats
        )

    def test_save_suite_creates_file(self):
        suite = self._build_suite()
        output_file = self.publisher.save_suite(suite)
        self.assertTrue(output_file.exists())

    def test_save_suite_creates_valid_json(self):
        suite = self._build_suite()
        output_file = self.publisher.save_suite(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIn('suite_id', data)
        self.assertIn('results', data)

    def test_save_suite_id_in_filename(self):
        suite = self._build_suite()
        output_file = self.publisher.save_suite(suite)
        self.assertIn("save-test-001", output_file.name)

    def test_save_suite_has_system_info(self):
        suite = self._build_suite()
        output_file = self.publisher.save_suite(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIn('system_info', data)

    def test_save_suite_results_include_timestamp_string(self):
        suite = self._build_suite()
        output_file = self.publisher.save_suite(suite)
        with open(output_file) as f:
            data = json.load(f)
        # Timestamp should be serialized as string
        if data['results']:
            self.assertIsInstance(data['results'][0]['timestamp'], str)


@requires_pandas
class TestGenerateLeaderboard(unittest.TestCase):
    """Test generate_leaderboard method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.publisher = BenchmarkPublisher(output_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _build_suite_with_stats(self):
        self.publisher.add_result("ds1", "model", "none", "gesture", "accuracy", 0.85,
                                  radiation_dose_msv=0.0)
        self.publisher.add_result("ds1", "model", "leo", "gesture", "response_time", 120.0,
                                  radiation_dose_msv=50.0)
        stats = self.publisher._compute_summary_statistics()
        return make_benchmark_suite(
            suite_id="leaderboard-test",
            results=self.publisher.results,
            summary_statistics=stats
        )

    def test_creates_file(self):
        suite = self._build_suite_with_stats()
        output_file = self.publisher.generate_leaderboard(suite)
        self.assertTrue(output_file.exists())

    def test_output_is_markdown(self):
        suite = self._build_suite_with_stats()
        output_file = self.publisher.generate_leaderboard(suite)
        content = output_file.read_text()
        self.assertIn('#', content)  # Has markdown headers

    def test_leaderboard_has_summary(self):
        suite = self._build_suite_with_stats()
        output_file = self.publisher.generate_leaderboard(suite)
        content = output_file.read_text()
        self.assertIn('Summary Statistics', content)

    def test_leaderboard_file_path(self):
        suite = self._build_suite_with_stats()
        output_file = self.publisher.generate_leaderboard(suite)
        self.assertEqual(output_file.name, 'LEADERBOARD.md')


@requires_pandas
class TestGeneratePublicationReport(unittest.TestCase):
    """Test generate_publication_report method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.publisher = BenchmarkPublisher(output_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _build_suite(self):
        self.publisher.add_result("ds1", "model", "none", "gesture", "accuracy", 0.85,
                                  radiation_dose_msv=0.0)
        self.publisher.add_result("ds1", "model", "leo", "gesture", "response_time", 120.0,
                                  radiation_dose_msv=50.0)
        stats = self.publisher._compute_summary_statistics()
        return make_benchmark_suite(
            suite_id="report-test",
            results=self.publisher.results,
            summary_statistics=stats
        )

    def test_creates_report_file(self):
        suite = self._build_suite()
        output_file = self.publisher.generate_publication_report(suite)
        self.assertTrue(output_file.exists())

    def test_report_is_markdown(self):
        suite = self._build_suite()
        output_file = self.publisher.generate_publication_report(suite)
        content = output_file.read_text()
        self.assertIn('#', content)

    def test_report_has_abstract(self):
        suite = self._build_suite()
        output_file = self.publisher.generate_publication_report(suite)
        content = output_file.read_text()
        self.assertIn('Abstract', content)

    def test_report_has_methodology(self):
        suite = self._build_suite()
        output_file = self.publisher.generate_publication_report(suite)
        content = output_file.read_text()
        self.assertIn('Methodology', content)

    def test_report_has_suite_id(self):
        suite = self._build_suite()
        output_file = self.publisher.generate_publication_report(suite)
        content = output_file.read_text()
        self.assertIn('report-test', content)


@requires_pandas
class TestExportToPapersWithCode(unittest.TestCase):
    """Test export_to_papers_with_code method"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.publisher = BenchmarkPublisher(output_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def _build_suite(self):
        self.publisher.add_result("ds1", "model", "none", "gesture", "accuracy", 0.85,
                                  radiation_dose_msv=0.0)
        stats = self.publisher._compute_summary_statistics()
        return make_benchmark_suite(
            suite_id="pwc-test",
            results=self.publisher.results,
            summary_statistics=stats
        )

    def test_creates_json_file(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        self.assertTrue(output_file.exists())

    def test_output_is_valid_json(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIsInstance(data, dict)

    def test_has_task_field(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIn('task', data)

    def test_has_metrics_field(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIn('metrics', data)

    def test_has_method_field(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        with open(output_file) as f:
            data = json.load(f)
        self.assertIn('method', data)

    def test_filename_includes_suite_id(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        self.assertIn('pwc-test', output_file.name)

    def test_accuracy_metric_scaled_to_percent(self):
        suite = self._build_suite()
        output_file = self.publisher.export_to_papers_with_code(suite)
        with open(output_file) as f:
            data = json.load(f)
        accuracy_value = data['metrics']['Accuracy']['value']
        # 0.85 should be 85.0
        self.assertAlmostEqual(accuracy_value, 85.0, places=1)


class TestBenchmarkResultDataclass(unittest.TestCase):
    """Test BenchmarkResult dataclass"""

    def test_all_fields_accessible(self):
        result = make_benchmark_result()
        self.assertEqual(result.benchmark_id, "test-id-001")
        self.assertEqual(result.dataset_name, "test_dataset")
        self.assertEqual(result.model_name, "TestModel")
        self.assertEqual(result.environment, "none")
        self.assertEqual(result.task, "gesture_recognition")
        self.assertEqual(result.metric_name, "accuracy")
        self.assertAlmostEqual(result.metric_value, 0.85, places=5)
        self.assertEqual(result.unit, "confidence")
        self.assertEqual(result.hardware, "CPU")
        self.assertEqual(result.radiation_dose_msv, 0.0)
        self.assertEqual(result.metadata, {})


@requires_pandas
class TestBenchmarkPublisherEdgeCases(unittest.TestCase):
    """Edge cases and regression tests for BenchmarkPublisher"""

    def setUp(self):
        self.tmpdir = tempfile.TemporaryDirectory()
        self.tmp_path = Path(self.tmpdir.name)
        self.publisher = BenchmarkPublisher(output_dir=self.tmp_path)

    def tearDown(self):
        self.tmpdir.cleanup()

    def test_add_result_default_hardware_is_cpu(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].hardware, "CPU")

    def test_add_result_default_radiation_zero(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].radiation_dose_msv, 0.0)

    def test_add_result_default_unit_empty_string(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.9)
        self.assertEqual(self.publisher.results[0].unit, "")

    def test_summary_stats_with_only_response_time_metrics(self):
        self.publisher.add_result("ds", "m", "e", "t", "response_time", 150.0)
        stats = self.publisher._compute_summary_statistics()
        # avg_accuracy should handle NaN (all NaN when no accuracy results)
        self.assertIn('avg_response_time_ms', stats)

    def test_save_suite_file_in_results_subdir(self):
        self.publisher.add_result("ds", "m", "e", "t", "accuracy", 0.85)
        stats = self.publisher._compute_summary_statistics()
        suite = make_benchmark_suite(
            suite_id="subdir-test",
            results=self.publisher.results,
            summary_statistics=stats
        )
        output_file = self.publisher.save_suite(suite)
        self.assertEqual(output_file.parent.name, 'results')

    def test_generate_leaderboard_in_leaderboards_subdir(self):
        self.publisher.add_result("ds", "m", "none", "t", "accuracy", 0.85,
                                  radiation_dose_msv=0.0)
        stats = self.publisher._compute_summary_statistics()
        suite = make_benchmark_suite(
            results=self.publisher.results,
            summary_statistics=stats
        )
        output_file = self.publisher.generate_leaderboard(suite)
        self.assertEqual(output_file.parent.name, 'leaderboards')

    def test_generate_report_in_reports_subdir(self):
        self.publisher.add_result("ds", "m", "none", "t", "accuracy", 0.85,
                                  radiation_dose_msv=0.0)
        stats = self.publisher._compute_summary_statistics()
        suite = make_benchmark_suite(
            suite_id="report-dir-test",
            results=self.publisher.results,
            summary_statistics=stats
        )
        output_file = self.publisher.generate_publication_report(suite)
        self.assertEqual(output_file.parent.name, 'reports')


if __name__ == '__main__':
    unittest.main(verbosity=2)