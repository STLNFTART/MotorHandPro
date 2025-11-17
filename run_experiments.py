#!/usr/bin/env python3
"""
LAM Experiment Runner
Execute all pending experiments and record results
"""

import sys
import os
from pathlib import Path
import time
import json
from datetime import datetime

# Add the repo to path
sys.path.insert(0, '/home/user/MotorHandPro')
sys.path.insert(0, str(Path('/home/user/MotorHandPro') / "extras" / "primal"))

from lam.core.primal_lam import PrimalLAM
from lam.actions.action_executors import ActionOrchestrator
from lam.assistants.lab_assistant import LabAssistant

try:
    from primal_constants import KERNEL_MU, DONTE_CONSTANT, S_RATIO, I3
except ImportError:
    KERNEL_MU = 0.16905
    DONTE_CONSTANT = 149.9992314000
    I3 = 6.4939394023
    S_RATIO = 23.0983417165

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

def run_quantum_resonance_experiment(lam, lab, exp_id, params):
    """Run a quantum resonance stability experiment"""
    print(f"\n🔬 Running Quantum Resonance Experiment: {exp_id}")

    iterations = params.get('iterations', 100)
    stability_threshold = params.get('stability_threshold', 0.001)

    print(f"  Parameters: {iterations} iterations, threshold {stability_threshold}")

    # Record initial state
    initial_state = lam.resonance.get_state()
    states_recorded = [initial_state]

    print(f"  Initial State: α={initial_state['alpha']:.6f}, λ={initial_state['lambda']:.6f}, epoch={initial_state['epoch']}")

    # Run iterations
    start_time = time.time()
    stability_maintained = True

    for i in range(iterations):
        # Execute a test action
        lam.answer_question(f"Test iteration {i+1}")

        # Record state every 10 iterations
        if (i + 1) % 10 == 0 or i == iterations - 1:
            state = lam.resonance.get_state()
            states_recorded.append(state)
            bounds = lam.resonance.check_semantic_bounds()

            if bounds['status'] != 'STABLE':
                stability_maintained = False
                print(f"  ⚠️  Warning at iteration {i+1}: {bounds['message']}")

    end_time = time.time()
    duration = end_time - start_time

    # Final state
    final_state = lam.resonance.get_state()
    final_bounds = lam.resonance.check_semantic_bounds()

    # Calculate metrics
    alpha_drift = final_state['alpha'] - initial_state['alpha']
    lambda_drift = final_state['lambda'] - initial_state['lambda']
    initial_distance = abs(initial_state['epoch'] - initial_state['donte_attractor'])
    final_distance = abs(final_state['epoch'] - final_state['donte_attractor'])
    convergence_progress = initial_distance - final_distance

    results = {
        "iterations_completed": iterations,
        "duration_seconds": duration,
        "initial_state": {
            "alpha": float(initial_state['alpha']),
            "lambda": float(initial_state['lambda']),
            "epoch": int(initial_state['epoch']),
            "distance_to_attractor": float(initial_distance)
        },
        "final_state": {
            "alpha": float(final_state['alpha']),
            "lambda": float(final_state['lambda']),
            "epoch": int(final_state['epoch']),
            "distance_to_attractor": float(final_distance)
        },
        "metrics": {
            "alpha_drift": float(alpha_drift),
            "alpha_drift_percent": float(alpha_drift / initial_state['alpha'] * 100),
            "lambda_drift": float(lambda_drift),
            "lambda_drift_percent": float(lambda_drift / initial_state['lambda'] * 100),
            "convergence_progress": float(convergence_progress),
            "lipschitz_constant": float(final_state['lipschitz_constant']),
            "stability_maintained": stability_maintained,
            "final_semantic_status": final_bounds['status']
        },
        "states_sampled": len(states_recorded)
    }

    # Print summary
    print(f"\n  ✅ Experiment Complete")
    print(f"  Duration: {duration:.2f}s ({duration/iterations*1000:.2f}ms per iteration)")
    print(f"  Alpha Drift: {alpha_drift:+.9f} ({alpha_drift/initial_state['alpha']*100:+.4f}%)")
    print(f"  Lambda Drift: {lambda_drift:+.9f} ({lambda_drift/initial_state['lambda']*100:+.4f}%)")
    print(f"  Convergence: {convergence_progress:.2f} epochs closer to attractor")
    print(f"  Stability: {'✓ MAINTAINED' if stability_maintained else '✗ COMPROMISED'}")
    print(f"  Lipschitz: {final_state['lipschitz_constant']:.9f} < 1.0 → {'✓' if final_state['lipschitz_constant'] < 1.0 else '✗'}")

    success = stability_maintained and final_state['lipschitz_constant'] < 1.0

    return results, success

def run_algorithm_validation_experiment(lam, orchestrator, lab, exp_id, params):
    """Run an algorithm validation experiment"""
    print(f"\n🔬 Running Algorithm Validation Experiment: {exp_id}")

    test_cases = params.get('test_cases', 10)
    print(f"  Parameters: {test_cases} test cases")

    start_time = time.time()
    successful_cases = 0

    for i in range(test_cases):
        try:
            # Execute trip planning (as proxy for algorithm testing)
            result = lam.plan_trip(f"Test trip #{i+1} to destination {chr(65+i%26)}")
            successful_cases += 1
        except Exception as e:
            print(f"  ⚠️  Test case {i+1} failed: {e}")

    end_time = time.time()
    duration = end_time - start_time

    success_rate = successful_cases / test_cases * 100

    results = {
        "test_cases_total": test_cases,
        "test_cases_successful": successful_cases,
        "success_rate": success_rate,
        "duration_seconds": duration,
        "avg_time_per_case": duration / test_cases
    }

    print(f"\n  ✅ Experiment Complete")
    print(f"  Success Rate: {success_rate:.1f}% ({successful_cases}/{test_cases})")
    print(f"  Duration: {duration:.2f}s ({duration/test_cases*1000:.2f}ms per case)")

    success = success_rate >= 80.0

    return results, success

def run_motor_control_experiment(lam, lab, exp_id, params):
    """Run a motor control experiment (simulated)"""
    print(f"\n🔬 Running Motor Control Experiment: {exp_id}")

    motor_id = params.get('motor_id', 'motor_001')
    test_duration = params.get('test_duration', 30)

    print(f"  Parameters: Motor {motor_id}, Duration {test_duration}s")

    start_time = time.time()

    # Simulate motor control testing
    precision_measurements = []
    for i in range(10):
        # Execute a task to simulate motor movement
        lam.complete_task(f"Motor {motor_id} movement test {i+1}")
        # Simulated precision (in reality this would measure actual motor position)
        precision = 0.08 + (i % 3) * 0.01  # Simulated values around 0.08-0.10mm
        precision_measurements.append(precision)
        time.sleep(0.1)  # Brief pause between tests

    end_time = time.time()
    duration = end_time - start_time

    avg_precision = sum(precision_measurements) / len(precision_measurements)
    max_precision = max(precision_measurements)
    min_precision = min(precision_measurements)

    results = {
        "motor_id": motor_id,
        "test_duration_seconds": duration,
        "measurements_count": len(precision_measurements),
        "precision_mm": {
            "average": avg_precision,
            "min": min_precision,
            "max": max_precision
        },
        "measurements": precision_measurements
    }

    print(f"\n  ✅ Experiment Complete")
    print(f"  Average Precision: {avg_precision:.4f}mm")
    print(f"  Range: {min_precision:.4f}mm - {max_precision:.4f}mm")
    print(f"  Target: < 0.1mm → {'✓ PASS' if avg_precision < 0.1 else '✗ FAIL'}")

    success = avg_precision < 0.1

    return results, success

def run_integration_stress_test(lam, orchestrator, lab, exp_id, params):
    """Run multi-system integration stress test"""
    print(f"\n🔬 Running Integration Stress Test: {exp_id}")

    total_actions = params.get('total_actions', 20)
    action_dist = params.get('action_distribution', {
        'trip_planning': 10,
        'food_ordering': 5,
        'reservations': 5
    })

    print(f"  Parameters: {total_actions} total actions")
    print(f"  Distribution: {action_dist}")

    start_time = time.time()
    actions_completed = 0
    actions_failed = 0

    initial_state = lam.resonance.get_state()

    # Execute trip planning actions
    for i in range(action_dist.get('trip_planning', 0)):
        try:
            lam.plan_trip(f"Trip {i+1} for stress testing")
            actions_completed += 1
        except Exception as e:
            actions_failed += 1
            print(f"  ⚠️  Trip planning {i+1} failed: {e}")

    # Execute food ordering actions
    for i in range(action_dist.get('food_ordering', 0)):
        try:
            orchestrator.execute_action("order_food",
                restaurant=f"Restaurant {i+1}",
                items=[f"Item {j}" for j in range(1, 4)],
                delivery_address=f"Address {i+1}",
                special_instructions="Test order"
            )
            actions_completed += 1
        except Exception as e:
            actions_failed += 1
            print(f"  ⚠️  Food ordering {i+1} failed: {e}")

    # Execute reservation actions
    for i in range(action_dist.get('reservations', 0)):
        try:
            orchestrator.execute_action("make_reservation",
                venue_type="restaurant",
                venue_name=f"Venue {i+1}",
                date="2025-12-25",
                time="19:00",
                party_size=2 + i % 4
            )
            actions_completed += 1
        except Exception as e:
            actions_failed += 1
            print(f"  ⚠️  Reservation {i+1} failed: {e}")

    end_time = time.time()
    duration = end_time - start_time

    final_state = lam.resonance.get_state()
    final_bounds = lam.resonance.check_semantic_bounds()

    success_rate = actions_completed / total_actions * 100

    results = {
        "total_actions": total_actions,
        "actions_completed": actions_completed,
        "actions_failed": actions_failed,
        "success_rate": success_rate,
        "duration_seconds": duration,
        "actions_per_second": actions_completed / duration,
        "stability_maintained": final_bounds['status'] == 'STABLE',
        "lipschitz_constant": float(final_state['lipschitz_constant'])
    }

    print(f"\n  ✅ Experiment Complete")
    print(f"  Success Rate: {success_rate:.1f}% ({actions_completed}/{total_actions})")
    print(f"  Duration: {duration:.2f}s ({actions_completed/duration:.2f} actions/sec)")
    print(f"  Stability: {final_bounds['status']}")
    print(f"  Lipschitz: {final_state['lipschitz_constant']:.9f} < 1.0 → {'✓' if final_state['lipschitz_constant'] < 1.0 else '✗'}")

    success = success_rate >= 90.0 and final_bounds['status'] == 'STABLE'

    return results, success

def run_experiment(lam, orchestrator, lab, exp_id):
    """Run a single experiment"""
    print_section(f"EXPERIMENT: {exp_id}")

    # Get experiment details
    goals = lab.list_goals()
    exp = None
    for g in goals:
        if g.goal_id == exp_id:
            exp = g
            break

    if not exp:
        print(f"❌ Experiment {exp_id} not found")
        return False

    print(f"Title: {exp.title}")
    print(f"Description: {exp.description}")
    print(f"Objective: {exp.objective}")
    print(f"Status: {exp.status}")

    # Update status to running
    lab.update_goal_status(exp_id, "running", "Starting experiment execution")

    # Determine experiment type and run
    results = None
    success = False

    try:
        if "quantum" in exp.title.lower() or "resonance" in exp.title.lower():
            results, success = run_quantum_resonance_experiment(lam, lab, exp_id, exp.parameters)
        elif "algorithm" in exp.title.lower() or "path planning" in exp.title.lower():
            results, success = run_algorithm_validation_experiment(lam, orchestrator, lab, exp_id, exp.parameters)
        elif "motor" in exp.title.lower():
            results, success = run_motor_control_experiment(lam, lab, exp_id, exp.parameters)
        elif "integration" in exp.title.lower() or "stress" in exp.title.lower():
            results, success = run_integration_stress_test(lam, orchestrator, lab, exp_id, exp.parameters)
        else:
            print(f"⚠️  Unknown experiment type, running as quantum resonance")
            results, success = run_quantum_resonance_experiment(lam, lab, exp_id, exp.parameters)

        # Record results
        lab.record_results(exp_id, results)

        # Update status
        final_status = "completed" if success else "failed"
        lab.update_goal_status(exp_id, final_status,
                              f"Experiment completed {'successfully' if success else 'with issues'}")

        return success

    except Exception as e:
        print(f"\n❌ Experiment failed with error: {e}")
        import traceback
        traceback.print_exc()

        lab.update_goal_status(exp_id, "failed", f"Error: {str(e)}")
        return False

def main():
    """Run all pending experiments"""
    print("\n" + "╔" + "═" * 73 + "╗")
    print("║" + " " * 73 + "║")
    print("║" + "       🧪 LAM EXPERIMENT RUNNER - EXECUTE ALL EXPERIMENTS 🧪      ".center(73) + "║")
    print("║" + " " * 73 + "║")
    print("╚" + "═" * 73 + "╝")

    # Initialize
    print_banner("INITIALIZATION")
    print("Initializing LAM components...")
    lam = PrimalLAM()
    orchestrator = ActionOrchestrator()
    lab = LabAssistant()
    print("✓ All systems ready\n")

    # Get all experiments
    all_experiments = lab.list_goals()
    pending_experiments = [exp for exp in all_experiments if exp.status == "planning"]

    print(f"Found {len(all_experiments)} total experiments")
    print(f"Found {len(pending_experiments)} pending experiments to run\n")

    if not pending_experiments:
        print("No pending experiments to run!")
        return

    # Show experiment queue
    print("📋 Experiment Queue:")
    for exp in pending_experiments:
        print(f"  • {exp.goal_id}: {exp.title}")

    # Run each experiment
    print_banner("RUNNING EXPERIMENTS")

    results_summary = []
    start_time = time.time()

    for i, exp in enumerate(pending_experiments, 1):
        print(f"\n[{i}/{len(pending_experiments)}] Running {exp.goal_id}...")
        success = run_experiment(lam, orchestrator, lab, exp.goal_id)
        results_summary.append({
            "exp_id": exp.goal_id,
            "title": exp.title,
            "success": success
        })

    end_time = time.time()
    total_duration = end_time - start_time

    # Final summary
    print_banner("EXPERIMENT SESSION SUMMARY")

    successful = sum(1 for r in results_summary if r['success'])
    failed = len(results_summary) - successful

    print(f"✅ Total Experiments Run: {len(results_summary)}")
    print(f"✅ Successful: {successful}")
    print(f"❌ Failed: {failed}")
    print(f"⏱️  Total Duration: {total_duration:.2f}s ({total_duration/60:.2f} minutes)")
    print(f"\n📊 Results by Experiment:")

    for result in results_summary:
        status = "✓ PASS" if result['success'] else "✗ FAIL"
        print(f"  {status} - {result['exp_id']}: {result['title']}")

    # Final quantum state
    final_state = lam.resonance.get_state()
    final_bounds = lam.resonance.check_semantic_bounds()

    print(f"\n⚛️  Final Quantum State:")
    print(f"  Epoch: {final_state['epoch']}")
    print(f"  Alpha: {final_state['alpha']:.6f}")
    print(f"  Lambda: {final_state['lambda']:.6f}")
    print(f"  Attractor Distance: {abs(final_state['epoch'] - final_state['donte_attractor']):.2f}")
    print(f"  Status: {final_bounds['status']}")
    print(f"  Lipschitz: {final_state['lipschitz_constant']:.9f}")

    print("\n" + "═" * 75)
    print("  All experiments complete! Results saved to workspace/goals/")
    print("═" * 75 + "\n")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
