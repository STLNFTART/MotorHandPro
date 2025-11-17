#!/usr/bin/env python3
"""
LAM Demo Script - Showcase all features
"""

import sys
import os
from pathlib import Path

# Add the repo to path
sys.path.insert(0, '/home/user/MotorHandPro')
sys.path.insert(0, str(Path('/home/user/MotorHandPro') / "extras" / "primal"))

from lam.core.primal_lam import PrimalLAM
from lam.actions.action_executors import ActionOrchestrator
try:
    from primal_constants import KERNEL_MU, DONTE_CONSTANT, S_RATIO, I3
except ImportError:
    # Fallback values
    KERNEL_MU = 0.16905
    DONTE_CONSTANT = 149.9992314000
    I3 = 6.4939394023
    S_RATIO = 23.0983417165

def print_header():
    """Print LAM header"""
    print("\n" + "="*70)
    print("🧠 PRIMAL LAM - Large Action Model Demo")
    print("Quantum-Semantic Framework with Lightfoot & Donte Constants")
    print("="*70)
    print(f"⚡ Lightfoot Constant (λ): {KERNEL_MU}")
    print(f"🎯 Donte Attractor (D): {DONTE_CONSTANT}")
    print(f"📊 I3 Normalization: {I3}")
    print(f"📈 S-Ratio (D/I3): {S_RATIO}")
    print("="*70 + "\n")

def print_section(title):
    """Print section header"""
    print("\n" + "─"*70)
    print(f"  {title}")
    print("─"*70)

def show_resonance_state(lam):
    """Display quantum resonance state"""
    state = lam.resonance.get_state()
    bounds = lam.resonance.check_semantic_bounds()
    print("\n⚛️  QUANTUM RESONANCE STATE")
    print("─" * 50)
    print(f"  Alpha (temporal):      {state['alpha']:.6f}")
    print(f"  Lambda (memory):       {state['lambda']:.6f}")
    print(f"  Epoch:                 {state['epoch']}")
    print(f"  Donte Attractor:       {state['donte_attractor']:.10f}")
    print(f"  Lipschitz Constant:    {state['lipschitz_constant']:.9f}")
    print(f"  Stable:                {'✓ YES' if state['stable'] else '✗ NO'}")
    print(f"  Semantic Status:       {bounds['status']}")
    print(f"  Message:               {bounds['message']}")
    print("─" * 50)

def main():
    """Run LAM demo"""
    print_header()

    # Initialize LAM
    print("🚀 Initializing LAM...\n")
    lam = PrimalLAM()
    print("✓ LAM initialized successfully!")

    print("\n🚀 Initializing Action Orchestrator...")
    orchestrator = ActionOrchestrator()
    print("✓ Action Orchestrator initialized!")

    # Show initial state
    show_resonance_state(lam)

    # Demo 1: Trip Planning
    print_section("Demo 1: 🌍 Trip Planning")
    result = lam.plan_trip("Trip to Tokyo, Japan from 2025-12-20 to 2025-12-27 with budget $5000")
    print(f"\n{result}\n")

    # Demo 2: Food Ordering via Orchestrator
    print_section("Demo 2: 🍕 Food Ordering")
    result = orchestrator.execute_action("order_food",
        restaurant="Sushi Palace",
        items=["Dragon Roll", "Miso Soup", "Green Tea"],
        delivery_address="456 Tech Ave",
        special_instructions="Extra ginger please"
    )
    print(f"\n{result}\n")

    # Demo 3: Restaurant Reservation via Orchestrator
    print_section("Demo 3: 🍽️  Restaurant Reservation")
    result = orchestrator.execute_action("make_reservation",
        venue_type="restaurant",
        venue_name="Le Bernardin",
        date="2025-12-31",
        time="20:00",
        party_size=2,
        special_requests="Window table preferred"
    )
    print(f"\n{result}\n")

    # Demo 4: Subscription Cancellation via Orchestrator
    print_section("Demo 4: ❌ Subscription Management")
    result = orchestrator.execute_action("cancel_subscription",
        service_name="Streaming Service XYZ",
        account_id="user@example.com",
        reason="No longer needed"
    )
    print(f"\n{result}\n")

    # Demo 5: Question Answering
    print_section("Demo 5: ❓ Question Answering")
    questions = [
        "What is the Lightfoot constant used for?",
        "Explain the quantum resonance field",
        "How does LAM ensure stability?"
    ]
    for q in questions:
        print(f"\nQ: {q}")
        result = lam.answer_question(q)
        print(f"A: {result}\n")

    # Demo 6: Task Completion
    print_section("Demo 6: ✅ Task Completion")
    tasks = [
        "Analyze system stability metrics",
        "Optimize quantum parameters",
        "Generate performance report"
    ]
    for task in tasks:
        print(f"\nTask: {task}")
        result = lam.complete_task(task)
        print(f"{result}\n")

    # Show final state
    print_section("Final Quantum Resonance State")
    show_resonance_state(lam)

    # Statistics
    state = lam.resonance.get_state()
    print("\n📊 SESSION STATISTICS")
    print("─" * 50)
    print(f"  Total Actions:         {state['epoch']}")
    print(f"  Memory Decay Rate:     {KERNEL_MU} (τ ≈ 5.92s)")
    print(f"  Convergence Guarantee: {'✓ YES' if state['lipschitz_constant'] < 1.0 else '✗ NO'}")
    print(f"  Attractor Distance:    {abs(state['epoch'] - state['donte_attractor']):.2f}")
    print(f"  System Stability:      {'✓ STABLE' if state['stable'] else '✗ UNSTABLE'}")
    print("─" * 50)

    print("\n" + "="*70)
    print("🎉 LAM Demo Complete!")
    print("="*70 + "\n")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
