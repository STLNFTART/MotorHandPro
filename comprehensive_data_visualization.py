#!/usr/bin/env python3
"""
Comprehensive Data Visualization Suite for MotorHandPro
Processes ALL data files and generates maximum output visualizations

Analyzes:
- All Mars mission datasets (NASA-compliant, realistic, enhanced)
- Primal Logic kernel test runs
- Crew health metrics
- Consciousness adaptation
- Radiation exposure
- All iterations and combinations
"""

import os
import csv
import glob
from pathlib import Path
from collections import defaultdict

try:
    import matplotlib
    matplotlib.use('Agg')  # Non-interactive backend
    import matplotlib.pyplot as plt
    import numpy as np
    HAVE_MPL = True
except ImportError:
    HAVE_MPL = False
    print("⚠️  matplotlib not available - visualizations will be skipped")

# Output directory
OUTPUT_DIR = "comprehensive_visualizations"


class DataAnalyzer:
    """Comprehensive data analyzer for all MotorHandPro datasets"""

    def __init__(self):
        self.datasets = {
            'mars_missions': [],
            'primal_kernel': [],
            'crew_health': [],
            'consciousness': [],
            'nasa_compliant': []
        }
        self.stats = {}

    def discover_datasets(self):
        """Discover all CSV data files in repository"""
        print("=" * 100)
        print("🔍 DISCOVERING DATASETS")
        print("=" * 100)
        print()

        all_csv_files = sorted(glob.glob("*.csv"))

        for csv_file in all_csv_files:
            size_kb = os.path.getsize(csv_file) / 1024

            # Categorize
            if 'nasa_compliant' in csv_file:
                self.datasets['nasa_compliant'].append(csv_file)
                category = 'NASA COMPLIANT'
            elif 'realistic_mars' in csv_file:
                self.datasets['mars_missions'].append(csv_file)
                category = 'MARS MISSION'
            elif 'enhanced_consciousness' in csv_file or 'consciousness' in csv_file:
                self.datasets['consciousness'].append(csv_file)
                category = 'CONSCIOUSNESS'
            elif 'crew' in csv_file:
                self.datasets['crew_health'].append(csv_file)
                category = 'CREW HEALTH'
            elif 'run_' in csv_file:
                self.datasets['primal_kernel'].append(csv_file)
                category = 'PRIMAL KERNEL'
            else:
                category = 'OTHER'

            print(f"  [{category:18s}] {csv_file:60s} ({size_kb:>8.1f} KB)")

        print()
        print(f"📊 Total datasets found: {len(all_csv_files)}")
        print(f"   - NASA Compliant: {len(self.datasets['nasa_compliant'])}")
        print(f"   - Mars Missions: {len(self.datasets['mars_missions'])}")
        print(f"   - Consciousness: {len(self.datasets['consciousness'])}")
        print(f"   - Crew Health: {len(self.datasets['crew_health'])}")
        print(f"   - Primal Kernel: {len(self.datasets['primal_kernel'])}")
        print()

    def analyze_primal_kernel_data(self):
        """Analyze Primal Logic kernel test data (run_*.csv files)"""
        print("=" * 100)
        print("⚙️  ANALYZING PRIMAL LOGIC KERNEL DATA")
        print("=" * 100)
        print()

        if not HAVE_MPL:
            print("⚠️  Matplotlib not available - skipping visualizations")
            return

        kernel_files = self.datasets['primal_kernel']
        if not kernel_files:
            print("⚠️  No kernel data files found")
            return

        # Create figure with subplots for each run
        num_files = len([f for f in kernel_files if os.path.getsize(f) > 0])
        if num_files == 0:
            print("⚠️  All kernel files are empty")
            return

        fig, axes = plt.subplots(num_files, 3, figsize=(18, 6 * num_files))
        if num_files == 1:
            axes = axes.reshape(1, -1)

        file_idx = 0
        for csv_file in kernel_files:
            if os.path.getsize(csv_file) == 0:
                print(f"  ⚠️  Skipping empty file: {csv_file}")
                continue

            print(f"  Processing: {csv_file}")

            try:
                # Load data
                t_vals, psi_vals, gamma_vals, ec_vals = [], [], [], []

                with open(csv_file, 'r') as f:
                    reader = csv.reader(f)
                    for row in reader:
                        if not row or row[0].startswith('#') or row[0].lower() in ('t', 'time'):
                            continue
                        try:
                            t_vals.append(float(row[0]))
                            psi_vals.append(float(row[1]))
                            gamma_vals.append(float(row[2]) if len(row) > 2 else 0)
                            ec_vals.append(float(row[3]) if len(row) > 3 else 0)
                        except:
                            continue

                if not t_vals:
                    print(f"    ⚠️  No valid data in {csv_file}")
                    continue

                # Plot ψ(t) - Control Signal
                axes[file_idx, 0].plot(t_vals, psi_vals, 'b-', linewidth=2)
                axes[file_idx, 0].set_title(f'{Path(csv_file).stem}\nControl Signal ψ(t)')
                axes[file_idx, 0].set_xlabel('Time (s)')
                axes[file_idx, 0].set_ylabel('ψ')
                axes[file_idx, 0].grid(True, alpha=0.3)

                # Plot γ(t) - Error Signal
                axes[file_idx, 1].plot(t_vals, gamma_vals, 'g-', linewidth=2)
                axes[file_idx, 1].set_title(f'Error Signal γ(t)')
                axes[file_idx, 1].set_xlabel('Time (s)')
                axes[file_idx, 1].set_ylabel('γ')
                axes[file_idx, 1].axhline(y=0, color='r', linestyle='--', alpha=0.5)
                axes[file_idx, 1].grid(True, alpha=0.3)

                # Plot Ec(t) - Control Energy
                axes[file_idx, 2].plot(t_vals, ec_vals, 'r-', linewidth=2)
                axes[file_idx, 2].set_title(f'Control Energy Ec(t)')
                axes[file_idx, 2].set_xlabel('Time (s)')
                axes[file_idx, 2].set_ylabel('Ec')
                axes[file_idx, 2].axhline(y=0, color='k', linestyle='--', alpha=0.5)
                axes[file_idx, 2].grid(True, alpha=0.3)

                print(f"    ✅ Plotted {len(t_vals)} points")
                file_idx += 1

            except Exception as e:
                print(f"    ❌ Error processing {csv_file}: {e}")

        plt.tight_layout()
        output_path = f"{OUTPUT_DIR}/primal_kernel_analysis.png"
        plt.savefig(output_path, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"\n✅ Saved: {output_path}")
        print()

    def analyze_mars_mission_data(self):
        """Analyze Mars mission crew health data"""
        print("=" * 100)
        print("🚀 ANALYZING MARS MISSION DATA")
        print("=" * 100)
        print()

        if not HAVE_MPL:
            print("⚠️  Matplotlib not available - skipping visualizations")
            return

        # Combine all mars mission datasets
        all_missions = (self.datasets['mars_missions'] +
                       self.datasets['nasa_compliant'] +
                       self.datasets['crew_health'])

        mission_data_files = [f for f in all_missions if 'mission_data' in f or 'transit' in f or 'crew' in f]

        if not mission_data_files:
            print("⚠️  No Mars mission data files found")
            return

        for csv_file in mission_data_files:
            if os.path.getsize(csv_file) == 0:
                print(f"  ⚠️  Skipping empty file: {csv_file}")
                continue

            print(f"  Processing: {csv_file}")

            try:
                # Load crew data
                crew_data = defaultdict(lambda: {
                    'mission_day': [],
                    'hbc': [],
                    'tremor': [],
                    'consciousness': [],
                    'radiation': []
                })

                with open(csv_file, 'r') as f:
                    reader = csv.DictReader(f)
                    for row in reader:
                        try:
                            crew_id = row.get('crew_id', 'UNKNOWN')
                            crew_data[crew_id]['mission_day'].append(float(row['mission_day']))
                            crew_data[crew_id]['hbc'].append(float(row.get('hbc', 0)))
                            crew_data[crew_id]['tremor'].append(float(row.get('tremor_amplitude', 0)))
                            if 'consciousness' in row:
                                crew_data[crew_id]['consciousness'].append(float(row['consciousness']))
                            crew_data[crew_id]['radiation'].append(float(row.get('cumulative_radiation_dose', 0)))
                        except:
                            continue

                if not crew_data:
                    print(f"    ⚠️  No valid data in {csv_file}")
                    continue

                # Create comprehensive plot
                num_crew = len(crew_data)
                fig, axes = plt.subplots(4, num_crew, figsize=(6 * num_crew, 16))
                if num_crew == 1:
                    axes = axes.reshape(-1, 1)

                fig.suptitle(f'{Path(csv_file).stem} - Comprehensive Analysis', fontsize=16, y=0.995)

                for crew_idx, (crew_id, data) in enumerate(sorted(crew_data.items())):
                    # HBC over time
                    axes[0, crew_idx].plot(data['mission_day'], data['hbc'], 'b-', linewidth=2)
                    axes[0, crew_idx].set_title(f'{crew_id}\nHemoglobin Concentration')
                    axes[0, crew_idx].set_ylabel('HBC (g/dL)')
                    axes[0, crew_idx].grid(True, alpha=0.3)

                    # Tremor over time
                    axes[1, crew_idx].plot(data['mission_day'], data['tremor'], 'g-', linewidth=2)
                    axes[1, crew_idx].set_title('Tremor Amplitude')
                    axes[1, crew_idx].set_ylabel('Amplitude')
                    axes[1, crew_idx].grid(True, alpha=0.3)

                    # Consciousness over time (if available)
                    if data['consciousness']:
                        axes[2, crew_idx].plot(data['mission_day'], data['consciousness'], 'purple', linewidth=2)
                        axes[2, crew_idx].set_title('Consciousness Level (φ)')
                        axes[2, crew_idx].set_ylabel('φ')
                        axes[2, crew_idx].grid(True, alpha=0.3)
                    else:
                        axes[2, crew_idx].text(0.5, 0.5, 'No consciousness data',
                                              ha='center', va='center', transform=axes[2, crew_idx].transAxes)

                    # Cumulative radiation
                    axes[3, crew_idx].plot(data['mission_day'], data['radiation'], 'r-', linewidth=2)
                    axes[3, crew_idx].set_title('Cumulative Radiation Dose')
                    axes[3, crew_idx].set_xlabel('Mission Day')
                    axes[3, crew_idx].set_ylabel('Dose (mSv)')
                    axes[3, crew_idx].grid(True, alpha=0.3)

                plt.tight_layout()
                output_path = f"{OUTPUT_DIR}/{Path(csv_file).stem}_analysis.png"
                plt.savefig(output_path, dpi=300, bbox_inches='tight')
                plt.close()
                print(f"    ✅ Saved: {output_path}")

            except Exception as e:
                print(f"    ❌ Error processing {csv_file}: {e}")

        print()

    def analyze_consciousness_adaptation(self):
        """Analyze consciousness adaptation data"""
        print("=" * 100)
        print("🧠 ANALYZING CONSCIOUSNESS ADAPTATION DATA")
        print("=" * 100)
        print()

        if not HAVE_MPL:
            print("⚠️  Matplotlib not available - skipping visualizations")
            return

        consciousness_files = self.datasets['consciousness']

        if not consciousness_files:
            print("⚠️  No consciousness data files found")
            return

        for csv_file in consciousness_files:
            if os.path.getsize(csv_file) == 0:
                print(f"  ⚠️  Skipping empty file: {csv_file}")
                continue

            print(f"  Processing: {csv_file}")

            # Process consciousness evolution curves
            if 'curves' in csv_file or 'evolution' in csv_file:
                try:
                    crew_curves = defaultdict(lambda: {'mission_day': [], 'consciousness': []})

                    with open(csv_file, 'r') as f:
                        reader = csv.DictReader(f)
                        for row in reader:
                            try:
                                crew_id = row.get('crew_id', 'UNKNOWN')
                                crew_curves[crew_id]['mission_day'].append(float(row['mission_day']))
                                crew_curves[crew_id]['consciousness'].append(float(row['consciousness']))
                            except:
                                continue

                    if crew_curves:
                        fig, ax = plt.subplots(figsize=(12, 8))

                        for crew_id, data in sorted(crew_curves.items()):
                            ax.plot(data['mission_day'], data['consciousness'],
                                   marker='o', linewidth=2, label=crew_id)

                        ax.set_title(f'{Path(csv_file).stem}\nConsciousness Evolution', fontsize=14)
                        ax.set_xlabel('Mission Day', fontsize=12)
                        ax.set_ylabel('Consciousness Level (φ)', fontsize=12)
                        ax.legend(loc='best')
                        ax.grid(True, alpha=0.3)

                        plt.tight_layout()
                        output_path = f"{OUTPUT_DIR}/{Path(csv_file).stem}_plot.png"
                        plt.savefig(output_path, dpi=300, bbox_inches='tight')
                        plt.close()
                        print(f"    ✅ Saved: {output_path}")

                except Exception as e:
                    print(f"    ❌ Error processing {csv_file}: {e}")

        print()

    def generate_summary_report(self):
        """Generate comprehensive summary report"""
        print("=" * 100)
        print("📋 GENERATING COMPREHENSIVE SUMMARY REPORT")
        print("=" * 100)
        print()

        report_path = f"{OUTPUT_DIR}/comprehensive_analysis_report.txt"

        with open(report_path, 'w') as f:
            f.write("=" * 100 + "\n")
            f.write("MOTORHANDPRO - COMPREHENSIVE DATA ANALYSIS REPORT\n")
            f.write("=" * 100 + "\n\n")

            f.write("DATASETS ANALYZED:\n")
            f.write("-" * 100 + "\n\n")

            for category, files in self.datasets.items():
                f.write(f"\n{category.upper().replace('_', ' ')}:\n")
                for csv_file in files:
                    size_kb = os.path.getsize(csv_file) / 1024
                    f.write(f"  - {csv_file:60s} ({size_kb:>8.1f} KB)\n")

            f.write("\n" + "=" * 100 + "\n")
            f.write("ANALYSIS OUTPUTS:\n")
            f.write("=" * 100 + "\n\n")

            viz_files = sorted(glob.glob(f"{OUTPUT_DIR}/*.png"))
            f.write(f"Generated {len(viz_files)} visualization files:\n\n")
            for viz_file in viz_files:
                size_kb = os.path.getsize(viz_file) / 1024
                f.write(f"  - {Path(viz_file).name:70s} ({size_kb:>8.1f} KB)\n")

            f.write("\n" + "=" * 100 + "\n")
            f.write("END OF REPORT\n")
            f.write("=" * 100 + "\n")

        print(f"✅ Saved: {report_path}")
        print()

    def run_comprehensive_analysis(self):
        """Run complete analysis pipeline"""
        print("\n" + "=" * 100)
        print("🚀 MOTORHANDPRO COMPREHENSIVE DATA VISUALIZATION SUITE")
        print("=" * 100)
        print()

        # Create output directory
        Path(OUTPUT_DIR).mkdir(exist_ok=True)
        print(f"📁 Output directory: {OUTPUT_DIR}/\n")

        # Discover all datasets
        self.discover_datasets()

        # Run all analyses
        self.analyze_primal_kernel_data()
        self.analyze_mars_mission_data()
        self.analyze_consciousness_adaptation()

        # Generate summary
        self.generate_summary_report()

        print("=" * 100)
        print("✅ COMPREHENSIVE ANALYSIS COMPLETE")
        print("=" * 100)
        print()


if __name__ == "__main__":
    analyzer = DataAnalyzer()
    analyzer.run_comprehensive_analysis()
