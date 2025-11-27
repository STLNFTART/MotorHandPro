#!/usr/bin/env python3
"""
Master Comprehensive Sweep Runner
Executes all simulations with maximum parameter coverage and full output
"""
import sys
import subprocess
import time
from pathlib import Path
from datetime import datetime


class MasterSweepRunner:
    """Orchestrates comprehensive sweeps across all simulations"""

    def __init__(self):
        self.start_time = time.time()
        self.results = []
        self.sweep_dirs = []

    def print_banner(self, text):
        """Print formatted banner"""
        print("\n" + "="*80)
        print(f"  {text}")
        print("="*80 + "\n")

    def run_command(self, cmd, description):
        """Run command and capture output"""
        print(f"🚀 {description}")
        print(f"   Command: {' '.join(cmd)}")
        print()

        start = time.time()
        try:
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=600)
            elapsed = time.time() - start

            # Parse output for sweep directory
            for line in result.stdout.split('\n'):
                if 'Results in:' in line or 'experiments/runs/' in line:
                    # Extract directory path
                    parts = line.split('experiments/runs/')
                    if len(parts) > 1:
                        sweep_dir = 'experiments/runs/' + parts[1].strip()
                        self.sweep_dirs.append(sweep_dir)

            self.results.append({
                "description": description,
                "elapsed": elapsed,
                "success": result.returncode == 0,
                "stdout": result.stdout,
                "stderr": result.stderr
            })

            if result.returncode == 0:
                print(f"✓ {description} completed in {elapsed:.1f}s\n")
            else:
                print(f"✗ {description} failed")
                print(f"Error: {result.stderr}\n")

            return result.returncode == 0

        except subprocess.TimeoutExpired:
            print(f"⚠️  {description} timed out (>10 minutes)\n")
            return False
        except Exception as e:
            print(f"✗ {description} error: {e}\n")
            return False

    def run_primal_swarm_comprehensive(self):
        """Run comprehensive primal swarm sweep"""
        self.print_banner("1. PRIMAL SWARM - COMPREHENSIVE SWEEP")

        # Maximum parameter coverage
        cmd = [
            "python3", "experiments/sweep_primal_swarm.py"
            # Uses built-in comprehensive grid
        ]

        # Modify the script to use maximum grid
        import tempfile
        with open("experiments/sweep_primal_swarm.py", "r") as f:
            content = f.read()

        # Create temp version with max grid
        max_content = content.replace(
            'params = {',
            '''params = {
        # MAXIMUM COVERAGE
'''
        ).replace(
            '"n_drones": [10, 25, 50, 100]',
            '"n_drones": [10, 25, 50, 100, 200, 500]'
        ).replace(
            '"lambda_val": [0.05, 0.0893, 0.15]',
            '"lambda_val": [0.05, 0.0893, 0.12, 0.15, 0.20, 0.25]'
        ).replace(
            '"tau_val": [0.05, 0.0997, 0.15]',
            '"tau_val": [0.05, 0.0997, 0.12, 0.15, 0.20]'
        )

        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            f.write(max_content)
            temp_file = f.name

        cmd = ["python3", temp_file]
        success = self.run_command(cmd, "Primal Swarm Maximum Coverage Sweep")

        # Cleanup
        Path(temp_file).unlink()
        return success

    def run_mars_mission_comprehensive(self):
        """Run comprehensive Mars mission sweep"""
        self.print_banner("2. MARS MISSION - COMPREHENSIVE SWEEP")

        cmd = [
            "python3", "experiments/sweep_mars_mission.py"
        ]

        return self.run_command(cmd, "Mars Mission Maximum Coverage Sweep")

    def run_control_system_torque_regime(self):
        """Run high-torque regime sweep"""
        self.print_banner("3. CONTROL SYSTEM - HIGH-TORQUE REGIME")

        cmd = [
            "python3", "run_comprehensive_sweep.py",
            "--torque-max", "0.5", "0.7", "0.9", "1.1", "1.3", "1.5", "1.8", "2.0", "2.5", "3.0",
            "--steps", "2000",
            "--tag", "max_torque_sweep"
        ]

        return self.run_command(cmd, "Control System High-Torque Regime")

    def run_control_system_memory_comparison(self):
        """Run long-horizon memory comparison"""
        self.print_banner("4. CONTROL SYSTEM - MEMORY MODE COMPARISON")

        cmd = [
            "python3", "run_comprehensive_sweep.py",
            "--torque-max", "1.0", "1.5", "2.0",
            "--steps", "5000",
            "--memory-compare",
            "--tag", "memory_showdown"
        ]

        return self.run_command(cmd, "Control System Memory Mode Showdown")

    def run_control_system_parameter_exploration(self):
        """Run full parameter exploration"""
        self.print_banner("5. CONTROL SYSTEM - FULL PARAMETER EXPLORATION")

        cmd = [
            "python3", "run_comprehensive_sweep.py",
            "--torque-max", "0.8", "1.2", "1.6", "2.0", "2.4",
            "--steps", "3000",
            "--lambda-sweep",
            "--KE-sweep",
            "--memory-compare",
            "--tag", "full_exploration"
        ]

        return self.run_command(cmd, "Control System Full Parameter Exploration")

    def generate_all_heatmaps(self):
        """Generate heatmaps for all sweep results"""
        self.print_banner("VISUALIZATION - GENERATING ALL HEATMAPS")

        success_count = 0
        for sweep_dir in self.sweep_dirs:
            # Extract run ID from path
            parts = sweep_dir.split('/')
            if len(parts) >= 4:
                run_id = parts[-1]
                sim_name = parts[-2]

                cmd = [
                    "python3", "generate_heatmaps.py",
                    "--sweep-dir", run_id,
                    "--sim-name", sim_name
                ]

                if self.run_command(cmd, f"Heatmaps for {sim_name}/{run_id}"):
                    success_count += 1

        print(f"✓ Generated visualizations for {success_count}/{len(self.sweep_dirs)} sweeps\n")

    def generate_master_report(self):
        """Generate comprehensive master report"""
        self.print_banner("GENERATING MASTER REPORT")

        total_elapsed = time.time() - self.start_time
        success_count = sum(1 for r in self.results if r['success'])
        total_count = len(self.results)

        report_path = Path("MASTER_SWEEP_REPORT.md")
        with open(report_path, 'w') as f:
            f.write("# Master Comprehensive Sweep Report\n\n")
            f.write(f"**Generated:** {datetime.utcnow().isoformat()}Z\n\n")
            f.write(f"**Total Runtime:** {total_elapsed/60:.1f} minutes\n\n")
            f.write(f"**Success Rate:** {success_count}/{total_count} ({success_count/total_count*100:.1f}%)\n\n")

            f.write("## Sweeps Executed\n\n")
            for i, result in enumerate(self.results, 1):
                status = "✓" if result['success'] else "✗"
                f.write(f"{i}. {status} **{result['description']}** ({result['elapsed']:.1f}s)\n")

            f.write("\n## Sweep Directories\n\n")
            for sweep_dir in self.sweep_dirs:
                f.write(f"- `{sweep_dir}`\n")

            f.write("\n## Summary Statistics\n\n")
            f.write(f"- Total sweeps: {total_count}\n")
            f.write(f"- Successful: {success_count}\n")
            f.write(f"- Failed: {total_count - success_count}\n")
            f.write(f"- Total runtime: {total_elapsed:.1f}s ({total_elapsed/60:.1f}min)\n")
            f.write(f"- Average time per sweep: {total_elapsed/max(total_count,1):.1f}s\n")

            f.write("\n## Next Steps\n\n")
            f.write("1. Review individual sweep results in `experiments/runs/`\n")
            f.write("2. Analyze heatmaps in `<sweep_dir>/plots/`\n")
            f.write("3. Check summary CSVs for optimal configurations\n")
            f.write("4. Use LAM curator for adaptive parameter refinement\n")

        print(f"✓ Master report saved to: {report_path}\n")

    def run_all(self):
        """Execute all comprehensive sweeps"""
        self.print_banner("MASTER COMPREHENSIVE SWEEP RUNNER")
        print("Running all simulations with maximum parameter coverage")
        print(f"Start time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")

        # Run all sweeps
        self.run_primal_swarm_comprehensive()
        self.run_mars_mission_comprehensive()
        self.run_control_system_torque_regime()
        self.run_control_system_memory_comparison()
        self.run_control_system_parameter_exploration()

        # Generate visualizations
        self.generate_all_heatmaps()

        # Generate master report
        self.generate_master_report()

        # Final summary
        total_elapsed = time.time() - self.start_time
        self.print_banner("MASTER SWEEP COMPLETE")
        print(f"Total runtime: {total_elapsed/60:.1f} minutes")
        print(f"Sweeps executed: {len(self.results)}")
        print(f"Sweep directories: {len(self.sweep_dirs)}")
        print(f"\nResults saved to: MASTER_SWEEP_REPORT.md\n")


def main():
    """Run master sweep"""
    runner = MasterSweepRunner()
    runner.run_all()


if __name__ == "__main__":
    main()
