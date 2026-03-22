#!/usr/bin/env python3
"""
Interactive LAM Session
Run the LAM framework and interact with all its features
"""

import sys
import os
from pathlib import Path

# Ensure the repo root is importable without a hard-coded absolute path.
_REPO_ROOT = Path(__file__).parent.resolve()
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from lam.core.primal_lam import PrimalLAM
try:
    from primal_constants import KERNEL_MU, DONTE_CONSTANT, S_RATIO, I3
except ImportError:
    # Fallback values
    KERNEL_MU = 0.16905
    DONTE_CONSTANT = 149.9992314000
    I3 = 6.4939394023
    S_RATIO = 23.0983417165

import json

def print_header():
    """Print LAM header"""
    print("\n" + "="*70)
    print("🧠 PRIMAL LAM - Large Action Model")
    print("Quantum-Semantic Framework with Lightfoot & Donte Constants")
    print("="*70)
    print(f"⚡ Lightfoot Constant (λ): {KERNEL_MU}")
    print(f"🎯 Donte Attractor (D): {DONTE_CONSTANT}")
    print(f"📊 I3 Normalization: {I3}")
    print(f"📈 S-Ratio (D/I3): {S_RATIO}")
    print("="*70 + "\n")

def print_menu():
    """Print interactive menu"""
    print("\n📋 What would you like LAM to do?")
    print("─" * 50)
    print("1. 🌍 Plan a Trip")
    print("2. 🍕 Order Food")
    print("3. 🍽️  Make a Reservation")
    print("4. ❌ Cancel a Subscription")
    print("5. ❓ Answer a Question")
    print("6. ✅ Complete a Task")
    print("7. 📊 Show Quantum Resonance State")
    print("8. 🧪 Lab Assistant - Set Experiment Goal")
    print("9. 🔧 Setup Wizard - Add Service Credentials")
    print("10. 💾 Show System Statistics")
    print("0. 🚪 Exit")
    print("─" * 50)

def show_resonance_state(lam):
    """Display quantum resonance state"""
    state = lam.get_resonance_state()
    print("\n⚛️  QUANTUM RESONANCE STATE")
    print("─" * 50)
    print(f"Alpha (temporal): {state['alpha']:.6f}")
    print(f"Lambda (memory): {state['lambda']:.6f}")
    print(f"Epoch: {state['epoch']}")
    print(f"Donte Attractor: {state['donte_attractor']:.10f}")
    print(f"Lipschitz Constant: {state['lipschitz_constant']:.9f}")
    print(f"Stable: {'✓ YES' if state['stable'] else '✗ NO'}")
    print(f"Semantic Bounds: {'✓ VALID' if state['semantic_bounds_valid'] else '✗ INVALID'}")
    print("─" * 50)

def show_stats(lam):
    """Display system statistics"""
    state = lam.get_resonance_state()
    print("\n📊 SYSTEM STATISTICS")
    print("─" * 50)
    print(f"Total Actions: {state['epoch']}")
    print(f"Memory Decay Rate: {KERNEL_MU} (τ ≈ 5.92s)")
    print(f"Convergence Guarantee: {'✓ YES' if state['lipschitz_constant'] < 1.0 else '✗ NO'}")
    print(f"Attractor Distance: {abs(state['epoch'] - state['donte_attractor']):.2f}")
    print(f"System Age: {state['epoch']} iterations")
    print("─" * 50)

def main():
    """Main interactive loop"""
    print_header()

    # Initialize LAM
    print("🚀 Initializing LAM...")
    lam = PrimalLAM()
    print("✓ LAM initialized successfully!\n")

    # Show initial state
    show_resonance_state(lam)

    while True:
        print_menu()
        choice = input("\n👉 Select an option (0-10): ").strip()

        if choice == "0":
            print("\n👋 Goodbye! LAM shutting down...")
            print(f"Final epoch: {lam.get_resonance_state()['epoch']}")
            break

        elif choice == "1":
            print("\n🌍 TRIP PLANNER")
            print("─" * 50)
            destination = input("Destination: ").strip() or "Paris"
            dates = input("Dates (e.g., 2025-06-01 to 2025-06-07): ").strip() or "2025-06-01 to 2025-06-07"
            budget = input("Budget: ").strip() or "$3000"

            result = lam.plan_trip(destination, dates, budget)
            print(f"\n✨ Result:\n{result}")

        elif choice == "2":
            print("\n🍕 FOOD ORDERING")
            print("─" * 50)
            restaurant = input("Restaurant: ").strip() or "Pizza Palace"
            items = input("Items (comma-separated): ").strip() or "Large pepperoni pizza, Garlic bread"
            address = input("Delivery address: ").strip() or "123 Main St"

            result = lam.order_food(restaurant, items, address)
            print(f"\n✨ Result:\n{result}")

        elif choice == "3":
            print("\n🍽️  RESERVATION MANAGER")
            print("─" * 50)
            place = input("Restaurant/Venue: ").strip() or "The French Laundry"
            datetime = input("Date & Time: ").strip() or "2025-12-25 19:00"
            party_size = input("Party size: ").strip() or "4"

            result = lam.make_reservation(place, datetime, party_size)
            print(f"\n✨ Result:\n{result}")

        elif choice == "4":
            print("\n❌ SUBSCRIPTION CANCELLATION")
            print("─" * 50)
            service = input("Service name: ").strip() or "Netflix"
            account = input("Account ID: ").strip() or "user@example.com"

            result = lam.cancel_subscription(service, account)
            print(f"\n✨ Result:\n{result}")

        elif choice == "5":
            print("\n❓ QUESTION ANSWERING")
            print("─" * 50)
            question = input("Your question: ").strip() or "What is the Lightfoot constant?"

            result = lam.answer_question(question)
            print(f"\n✨ Answer:\n{result}")

        elif choice == "6":
            print("\n✅ TASK COMPLETION")
            print("─" * 50)
            task = input("Task description: ").strip() or "Analyze quantum resonance stability"

            result = lam.complete_task(task)
            print(f"\n✨ Result:\n{result}")

        elif choice == "7":
            show_resonance_state(lam)

        elif choice == "8":
            print("\n🧪 LAB ASSISTANT")
            print("─" * 50)
            from lam.assistants.lab_assistant import LabAssistant
            lab = LabAssistant()

            print("\nExperiment Types:")
            print("1. Motor Control")
            print("2. Algorithm Validation")
            print("3. Quantum Resonance")
            print("4. Satellite Integration")
            print("5. Custom")

            exp_type = input("\nSelect type (1-5): ").strip()
            type_map = {
                "1": "motor_control",
                "2": "algorithm_validation",
                "3": "quantum_resonance",
                "4": "satellite_integration",
                "5": "custom"
            }
            exp_type = type_map.get(exp_type, "custom")

            goal = input("Experiment goal: ").strip() or "Test quantum stability"

            exp_id = lab.set_experiment_goal(exp_type, goal)
            print(f"\n✓ Experiment created: {exp_id}")
            print(f"\nGoal: {goal}")
            print(f"Type: {exp_type}")

        elif choice == "9":
            print("\n🔧 SETUP WIZARD")
            print("─" * 50)
            from lam.wizards.setup_wizard import SetupWizard
            wizard = SetupWizard()

            print("\nAvailable Services:")
            print("1. API Service (OpenAI, etc.)")
            print("2. Email Service")
            print("3. Calendar Service")
            print("4. Food Delivery")
            print("5. Travel Booking")
            print("6. Subscription Service")

            service_type = input("\nSelect service (1-6): ").strip()
            service_map = {
                "1": "api",
                "2": "email",
                "3": "calendar",
                "4": "food_delivery",
                "5": "travel",
                "6": "subscription"
            }
            service_type = service_map.get(service_type, "api")

            service_name = input(f"Service name: ").strip() or "test_service"
            api_key = input("API Key: ").strip() or "test_key_12345"

            wizard.save_credentials(service_name, {"api_key": api_key, "service_type": service_type})
            print(f"\n✓ Credentials saved for {service_name}")

        elif choice == "10":
            show_stats(lam)

        else:
            print("\n❌ Invalid option. Please select 0-10.")

        input("\n⏎ Press Enter to continue...")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\n👋 LAM interrupted by user. Shutting down...")
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
