#!/usr/bin/env python3
"""
LAM Monitoring and Logging Infrastructure
Tracks actions, performance, and system health
"""
import json
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, Any, Optional
import time


class LAMLogger:
    """
    Comprehensive logging for LAM operations
    """

    def __init__(self, log_dir: Optional[Path] = None):
        if log_dir is None:
            log_dir = Path.home() / ".lam" / "logs"

        self.log_dir = Path(log_dir)
        self.log_dir.mkdir(parents=True, exist_ok=True)

        # Setup Python logging
        self.logger = self._setup_logger()

        # Metrics
        self.metrics = {
            "actions_total": 0,
            "actions_success": 0,
            "actions_failed": 0,
            "avg_response_time_ms": 0,
            "start_time": datetime.now().isoformat()
        }

    def _setup_logger(self) -> logging.Logger:
        """Configure Python logger"""
        logger = logging.getLogger("LAM")
        logger.setLevel(logging.INFO)

        # File handler
        log_file = self.log_dir / f"lam_{datetime.now().strftime('%Y%m%d')}.log"
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(logging.INFO)

        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.WARNING)

        # Formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)

        logger.addHandler(file_handler)
        logger.addHandler(console_handler)

        return logger

    def log_action(self, action_type: str, params: Dict[str, Any],
                   success: bool, duration_ms: float,
                   result: Optional[Dict[str, Any]] = None):
        """Log an action execution"""
        self.metrics["actions_total"] += 1
        if success:
            self.metrics["actions_success"] += 1
        else:
            self.metrics["actions_failed"] += 1

        # Update average response time
        n = self.metrics["actions_total"]
        old_avg = self.metrics["avg_response_time_ms"]
        self.metrics["avg_response_time_ms"] = ((old_avg * (n - 1)) + duration_ms) / n

        # Log entry
        log_entry = {
            "timestamp": datetime.now().isoformat(),
            "action_type": action_type,
            "params": params,
            "success": success,
            "duration_ms": duration_ms,
            "result_summary": self._summarize_result(result) if result else None
        }

        # Write to JSON log
        json_log_file = self.log_dir / f"actions_{datetime.now().strftime('%Y%m%d')}.jsonl"
        with open(json_log_file, 'a') as f:
            f.write(json.dumps(log_entry) + '\n')

        # Python logger
        if success:
            self.logger.info(f"Action {action_type} completed in {duration_ms:.2f}ms")
        else:
            self.logger.warning(f"Action {action_type} failed after {duration_ms:.2f}ms")

    def _summarize_result(self, result: Dict[str, Any]) -> Dict[str, Any]:
        """Create a summary of result for logging"""
        return {
            "success": result.get("success", False),
            "keys": list(result.keys())[:10],  # Limit keys
            "size": len(str(result))
        }

    def log_error(self, error: Exception, context: Dict[str, Any]):
        """Log an error"""
        error_entry = {
            "timestamp": datetime.now().isoformat(),
            "error_type": type(error).__name__,
            "error_message": str(error),
            "context": context
        }

        error_log_file = self.log_dir / f"errors_{datetime.now().strftime('%Y%m%d')}.jsonl"
        with open(error_log_file, 'a') as f:
            f.write(json.dumps(error_entry) + '\n')

        self.logger.error(f"{type(error).__name__}: {error}", exc_info=True)

    def log_resonance_update(self, state: Dict[str, Any]):
        """Log resonance field state"""
        resonance_log = self.log_dir / "resonance.jsonl"
        entry = {
            "timestamp": datetime.now().isoformat(),
            "state": state
        }

        with open(resonance_log, 'a') as f:
            f.write(json.dumps(entry) + '\n')

    def get_metrics(self) -> Dict[str, Any]:
        """Get current metrics"""
        uptime_seconds = (datetime.now() - datetime.fromisoformat(self.metrics["start_time"])).total_seconds()

        return {
            **self.metrics,
            "uptime_seconds": uptime_seconds,
            "uptime_hours": uptime_seconds / 3600,
            "success_rate": (self.metrics["actions_success"] / self.metrics["actions_total"] * 100)
                           if self.metrics["actions_total"] > 0 else 0
        }

    def export_metrics(self, output_file: Path):
        """Export metrics to file"""
        metrics = self.get_metrics()

        with open(output_file, 'w') as f:
            json.dump(metrics, f, indent=2)


class PerformanceMonitor:
    """
    Monitor performance metrics
    """

    def __init__(self):
        self.timings = {}

    def start(self, operation: str):
        """Start timing an operation"""
        self.timings[operation] = {"start": time.time()}

    def end(self, operation: str) -> float:
        """End timing and return duration in ms"""
        if operation not in self.timings:
            return 0.0

        duration = (time.time() - self.timings[operation]["start"]) * 1000
        self.timings[operation]["duration_ms"] = duration
        return duration

    def get_stats(self) -> Dict[str, float]:
        """Get timing statistics"""
        return {
            op: data.get("duration_ms", 0)
            for op, data in self.timings.items()
        }


# Decorator for automatic logging
def log_action(logger: LAMLogger):
    """Decorator to automatically log action execution"""
    def decorator(func):
        def wrapper(*args, **kwargs):
            monitor = PerformanceMonitor()
            monitor.start(func.__name__)

            try:
                result = func(*args, **kwargs)
                duration = monitor.end(func.__name__)

                logger.log_action(
                    action_type=func.__name__,
                    params={"args": str(args)[:100], "kwargs": str(kwargs)[:100]},
                    success=True,
                    duration_ms=duration,
                    result=result if isinstance(result, dict) else None
                )

                return result

            except Exception as e:
                duration = monitor.end(func.__name__)

                logger.log_action(
                    action_type=func.__name__,
                    params={"args": str(args)[:100], "kwargs": str(kwargs)[:100]},
                    success=False,
                    duration_ms=duration
                )

                logger.log_error(e, {"function": func.__name__})
                raise

        return wrapper
    return decorator


if __name__ == "__main__":
    # Test logging
    logger = LAMLogger()

    # Test action logging
    logger.log_action(
        action_type="trip_planning",
        params={"destination": "Paris"},
        success=True,
        duration_ms=150.5
    )

    # Get metrics
    print("Metrics:")
    print(json.dumps(logger.get_metrics(), indent=2))

    print(f"\nLogs saved to: {logger.log_dir}")
