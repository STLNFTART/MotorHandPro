#!/usr/bin/env python3
"""
Quick LAM Interactive Demo
Shows LAM processing various requests
"""

import sys
import os
from pathlib import Path
import time

# Ensure the repo root is importable without a hard-coded absolute path.
_REPO_ROOT = Path(__file__).parent.resolve()
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

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

def separator():
    print("\n" + "="*70 + "\n")

def main():
    print("\n🌟 LAM INTERACTIVE SESSION STARTING... 🌟\n")

    # Initialize components
    print("⚙️  Initializing LAM components...")
    lam = PrimalLAM()
    orchestrator = ActionOrchestrator()
    lab = LabAssistant()
    print("✓ All systems ready!\n")

    separator()

    # Show system constants
    print("📊 SYSTEM CONSTANTS")
    print(f"  Lightfoot (λ): {KERNEL_MU} - Memory decay rate")
    print(f"  Donte (D): {DONTE_CONSTANT} - Fixed-point attractor")
    print(f"  I3: {I3} - Normalization constant")
    print(f"  S-Ratio: {S_RATIO} - Scaling factor")

    separator()

    # Scenario 1: User wants to plan a vacation
    print("🎭 SCENARIO 1: Planning a Dream Vacation")
    print("User: 'I want to plan a trip to Bali for my honeymoon'")
    print("\nLAM Processing...\n")
    result = lam.plan_trip("Honeymoon trip to Bali, Indonesia for 10 days with budget $6000")
    print(result)

    separator()

    # Show quantum state after first action
    state = lam.resonance.get_state()
    print("⚛️  QUANTUM STATE UPDATE")
    print(f"  Epoch: {state['epoch']}")
    print(f"  Alpha: {state['alpha']:.6f}")
    print(f"  Lambda: {state['lambda']:.6f}")
    print(f"  Attractor Distance: {abs(state['epoch'] - state['donte_attractor']):.2f}")

    separator()

    # Scenario 2: Setting up a lab experiment
    print("🧪 SCENARIO 2: Lab Assistant - New Experiment")
    print("User: 'I need to test quantum resonance stability'")
    print("\nLAM Processing...\n")
    exp_id = lab.create_from_template("quantum_resonance", {
        "iterations": 1000,
        "stability_threshold": 0.001
    })
    print(f"✓ Experiment created: {exp_id}")
    print(f"  Type: Quantum Resonance")
    print(f"  Goal: Validate Lightfoot constant stability over 1000 iterations")
    print(f"  Template: quantum_resonance")

    separator()

    # Scenario 3: Making dinner reservations
    print("🍽️  SCENARIO 3: Last-Minute Dinner Reservation")
    print("User: 'Book a table for 4 at a nice Italian restaurant tonight'")
    print("\nLAM Processing...\n")
    result = orchestrator.execute_action("make_reservation",
        venue_type="restaurant",
        venue_name="Trattoria Bella Vista",
        date="2025-11-17",
        time="19:30",
        party_size=4,
        special_requests="Vegetarian options needed"
    )
    print(f"✓ Reservation confirmed!")
    print(f"  Venue: {result['confirmation']['venue']}")
    print(f"  Code: {result['confirmation']['code']}")
    print(f"  Party Size: {result['confirmation']['party_size']}")
    print(f"  Time: {result['confirmation']['time']}")

    separator()

    # Scenario 4: Question about the system
    print("❓ SCENARIO 4: Understanding the System")
    print("User: 'How does LAM ensure stability?'")
    print("\nLAM Processing...\n")
    result = lam.answer_question("How does the Lipschitz constant guarantee stability?")
    print(result)

    separator()

    # Scenario 5: Complex task
    print("✅ SCENARIO 5: Complex Task Execution")
    print("User: 'Run a full system diagnostic'")
    print("\nLAM Processing...\n")
    result = lam.complete_task("Run comprehensive system diagnostic with stability analysis")
    print(result)

    separator()

    # Scenario 6: Another question
    print("🤔 SCENARIO 6: Deep Technical Question")
    print("User: 'Explain the Lightfoot constant'")
    print("\nLAM Processing...\n")
    result = lam.answer_question("What is the Lightfoot constant and how does it work?")
    print(result)

    separator()

    # Final quantum state
    state = lam.resonance.get_state()
    bounds = lam.resonance.check_semantic_bounds()

    print("🎯 FINAL QUANTUM RESONANCE STATE")
    print(f"  Total Actions: {state['epoch']}")
    print(f"  Alpha (temporal): {state['alpha']:.6f}")
    print(f"  Lambda (memory): {state['lambda']:.6f}")
    print(f"  Attractor Distance: {abs(state['epoch'] - state['donte_attractor']):.2f}")
    print(f"  Status: {bounds['status']}")
    print(f"  Message: {bounds['message']}")
    print(f"  Stability: {'✓ GUARANTEED' if state['lipschitz_constant'] < 1.0 else '✗ UNSTABLE'}")

    separator()

    print("📈 SESSION SUMMARY")
    print(f"  Actions Executed: {state['epoch']}")
    print(f"  System Status: {bounds['status']}")
    print(f"  Memory Window: {1.0/KERNEL_MU:.2f} seconds")
    print(f"  Convergence: Active (→ {DONTE_CONSTANT})")
    print("\n✨ LAM session complete! All systems nominal. ✨\n")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
