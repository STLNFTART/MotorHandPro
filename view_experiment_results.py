#!/usr/bin/env python3
"""
View Experiment Results
Display detailed results from all completed experiments
"""

import sys
import os
from pathlib import Path
import json

# Add the repo to path
sys.path.insert(0, '/home/user/MotorHandPro')
sys.path.insert(0, str(Path('/home/user/MotorHandPro') / "extras" / "primal"))

from lam.assistants.lab_assistant import LabAssistant

def print_banner(text):
    """Print a banner"""
    print("\n" + "═" * 75)
    print(f"  {text}")
    print("═" * 75 + "\n")

def print_section(text):
    """Print a section header"""
    print("\n" + "─" * 75)
    print(f"  {text}")
    print("─" * 75)

def format_results(results):
    """Format results dictionary for display"""
    if not results:
        return "No results recorded"

    output = []
    for key, value in results.items():
        if isinstance(value, dict):
            output.append(f"  {key}:")
            for k, v in value.items():
                if isinstance(v, float):
                    output.append(f"    {k}: {v:.6f}")
                else:
                    output.append(f"    {k}: {v}")
        elif isinstance(value, list):
            if len(value) <= 5:
                output.append(f"  {key}: {value}")
            else:
                output.append(f"  {key}: [{len(value)} items]")
        elif isinstance(value, float):
            output.append(f"  {key}: {value:.6f}")
        else:
            output.append(f"  {key}: {value}")

    return "\n".join(output)

def main():
    """Display all experiment results"""
    print("\n" + "╔" + "═" * 73 + "╗")
    print("║" + " " * 73 + "║")
    print("║" + "         📊 LAM EXPERIMENT RESULTS VIEWER 📊         ".center(73) + "║")
    print("║" + " " * 73 + "║")
    print("╚" + "═" * 73 + "╝")

    # Initialize
    print("\n⚙️  Loading experiment data...\n")
    lab = LabAssistant()

    # Get all experiments
    all_experiments = lab.list_goals()

    if not all_experiments:
        print("No experiments found!")
        return

    print(f"Found {len(all_experiments)} experiments\n")

    # Group by status
    by_status = {}
    for exp in all_experiments:
        status = exp.status
        if status not in by_status:
            by_status[status] = []
        by_status[status].append(exp)

    # Display summary
    print_banner("EXPERIMENT SUMMARY")
    for status, exps in by_status.items():
        print(f"{status.upper()}: {len(exps)}")

    # Display each experiment in detail
    for i, exp in enumerate(all_experiments, 1):
        print_section(f"EXPERIMENT {i}/{len(all_experiments)}: {exp.goal_id}")

        print(f"Title: {exp.title}")
        print(f"Status: {exp.status}")
        print(f"Created: {exp.created_at}")
        print(f"Updated: {exp.updated_at}")
        print(f"\nDescription: {exp.description}")
        print(f"Objective: {exp.objective}")

        print(f"\nSuccess Criteria:")
        if isinstance(exp.success_criteria, list):
            for criterion in exp.success_criteria:
                print(f"  • {criterion}")
        else:
            print(f"  • {exp.success_criteria}")

        print(f"\nParameters:")
        for key, value in exp.parameters.items():
            if isinstance(value, dict):
                print(f"  {key}:")
                for k, v in value.items():
                    print(f"    {k}: {v}")
            else:
                print(f"  {key}: {value}")

        if exp.notes:
            print(f"\nNotes: ({len(exp.notes)} entries)")
            for note in exp.notes[-3:]:  # Show last 3 notes
                print(f"  • {note}")

        if exp.results:
            print(f"\nResults:")
            print(format_results(exp.results))
        else:
            print("\nResults: Not yet recorded")

    # Analyze results
    print_banner("RESULTS ANALYSIS")

    completed = [e for e in all_experiments if e.status == "completed"]
    failed = [e for e in all_experiments if e.status == "failed"]
    running = [e for e in all_experiments if e.status == "running"]
    planning = [e for e in all_experiments if e.status == "planning"]

    print(f"✅ Completed: {len(completed)}")
    print(f"❌ Failed: {len(failed)}")
    print(f"🔄 Running: {len(running)}")
    print(f"📋 Planning: {len(planning)}")

    if completed:
        print("\n📊 Completed Experiments:")
        for exp in completed:
            status_icon = "✓"
            print(f"  {status_icon} {exp.goal_id}: {exp.title}")
            if exp.results:
                # Try to extract key metrics
                results = exp.results
                if 'stability_maintained' in results:
                    print(f"    Stability: {'✓' if results['stability_maintained'] else '✗'}")
                if 'success_rate' in results:
                    print(f"    Success Rate: {results['success_rate']:.1f}%")
                if 'duration_seconds' in results:
                    print(f"    Duration: {results['duration_seconds']:.2f}s")

    if failed:
        print("\n❌ Failed Experiments:")
        for exp in failed:
            print(f"  ✗ {exp.goal_id}: {exp.title}")

    # Overall metrics
    if completed:
        print_banner("OVERALL METRICS")

        total_duration = sum(
            exp.results.get('duration_seconds', 0)
            for exp in completed
            if exp.results
        )

        stability_maintained_count = sum(
            1 for exp in completed
            if exp.results and exp.results.get('stability_maintained', False)
        )

        print(f"Total Experiments: {len(all_experiments)}")
        print(f"Success Rate: {len(completed)/len(all_experiments)*100:.1f}%")
        print(f"Total Execution Time: {total_duration:.2f}s")

        if stability_maintained_count > 0:
            print(f"Stability Maintained: {stability_maintained_count}/{len(completed)} experiments")

    print("\n" + "═" * 75)
    print("  Results review complete!")
    print("═" * 75 + "\n")

    # Offer to reset experiments
    print("💡 Options:")
    print("  To reset experiments to 'planning' status for re-running:")
    print("  (Modify the LabAssistant and update status manually)")
    print("\n  To create new experiments:")
    print("  python3 generate_experiment_ideas.py")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
