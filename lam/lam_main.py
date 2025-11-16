#!/usr/bin/env python3
"""
LAM Main Interface - Unified Large Action Model
Integrates all LAM components with Lightfoot & Donte constants
"""
import sys
import json
from pathlib import Path
from typing import Dict, Any, Optional

# Add paths for imports
sys.path.insert(0, str(Path(__file__).parent))
sys.path.insert(0, str(Path(__file__).parent / "core"))
sys.path.insert(0, str(Path(__file__).parent / "wizards"))
sys.path.insert(0, str(Path(__file__).parent / "assistants"))
sys.path.insert(0, str(Path(__file__).parent / "actions"))

try:
    from core.primal_lam import PrimalLAM, QuantumResonanceField
    from wizards.setup_wizard import SetupWizard
    from assistants.lab_assistant import LabAssistant
    from actions.action_executors import ActionOrchestrator
    COMPONENTS_LOADED = True
except ImportError as e:
    print(f"Warning: Could not load all components: {e}")
    COMPONENTS_LOADED = False


class LAM:
    """
    Unified Large Action Model Interface
    Combines all LAM capabilities with quantum-semantic intelligence
    """
    def __init__(self, api_base: str = "http://172.19.209.13:8000"):
        print("Initializing LAM (Large Action Model)...")
        print("With Lightfoot & Donte Constants Integration\n")

        if not COMPONENTS_LOADED:
            print("ERROR: Components not loaded. Check imports.")
            return

        # Core LAM engine
        self.engine = PrimalLAM(api_base)

        # Setup wizard
        self.wizard = SetupWizard()

        # Lab assistant
        self.lab = LabAssistant()

        # Action orchestrator (with credentials from wizard)
        credentials = self._load_credentials()
        self.orchestrator = ActionOrchestrator(credentials)

        print("✓ LAM initialized successfully\n")

    def _load_credentials(self) -> Dict[str, Dict[str, str]]:
        """Load credentials from setup wizard"""
        try:
            all_creds = self.wizard.load_credentials()
            return all_creds
        except:
            return {}

    def run_setup_wizard(self):
        """Launch setup wizard"""
        print("\n=== LAM Setup Wizard ===")
        self.wizard.interactive_wizard()

    def run_lab_assistant(self):
        """Launch lab assistant"""
        print("\n=== LAM Lab Assistant ===")
        self.lab.interactive_assistant()

    def plan_trip(self, destination: str, departure: str, return_date: str, budget: float = None):
        """Plan a trip"""
        # Use both engine and orchestrator
        print(f"\nPlanning trip to {destination}...")

        # Get quantum-optimized plan from engine
        engine_plan = self.engine.plan_trip(f"{destination} from {departure} to {return_date}")
        print(engine_plan)
        print("\n" + "="*60 + "\n")

        # Get detailed execution plan from orchestrator
        orchestrator_plan = self.orchestrator.execute_action(
            "plan_trip",
            destination=destination,
            departure_date=departure,
            return_date=return_date,
            budget=budget
        )

        print("Detailed Trip Plan:")
        print(json.dumps(orchestrator_plan, indent=2, default=str))

        return orchestrator_plan

    def make_reservation(self, venue_type: str, venue_name: str, date: str, time: str, party_size: int):
        """Make a reservation"""
        result = self.orchestrator.execute_action(
            "make_reservation",
            venue_type=venue_type,
            venue_name=venue_name,
            date=date,
            time=time,
            party_size=party_size
        )

        print(f"\nReservation Result:")
        print(json.dumps(result, indent=2))
        return result

    def order_food(self, restaurant: str, items: list, address: str):
        """Order food"""
        result = self.orchestrator.execute_action(
            "order_food",
            restaurant=restaurant,
            items=items,
            delivery_address=address
        )

        print(f"\nFood Order:")
        print(json.dumps(result, indent=2))
        return result

    def cancel_subscription(self, service: str, subscription_id: str):
        """Cancel a subscription"""
        result = self.orchestrator.execute_action(
            "cancel_subscription",
            service_name=service,
            subscription_id=subscription_id
        )

        print(f"\nSubscription Cancellation:")
        print(json.dumps(result, indent=2))
        return result

    def ask_question(self, question: str):
        """Ask a question"""
        answer = self.engine.answer_question(question)
        print(f"\n{answer}")
        return answer

    def complete_task(self, task: str):
        """Complete a general task"""
        result = self.engine.complete_task(task)
        print(f"\n{result}")
        return result

    def get_status(self):
        """Get comprehensive status"""
        status = self.engine.get_status()
        print(f"\n{status}")
        return status

    def interactive_mode(self):
        """
        Main interactive mode with all capabilities
        """
        print("\n" + "="*70)
        print("LAM - LARGE ACTION MODEL")
        print("Quantum-Semantic Intelligence for Real-World Tasks")
        print("With Lightfoot & Donte Constants")
        print("="*70)

        print("\n📋 CAPABILITIES:")
        print("\n🎯 Actions:")
        print("  /trip        - Plan and book trips")
        print("  /reserve     - Make reservations")
        print("  /order       - Order food/services")
        print("  /cancel-sub  - Cancel subscriptions")
        print("\n🔧 Setup & Management:")
        print("  /wizard      - Setup wizard (configure credentials)")
        print("  /lab         - Lab assistant (experiment management)")
        print("\n💬 Interaction:")
        print("  /ask         - Ask questions about repository/experiments")
        print("  /task        - Complete general tasks")
        print("\n📊 Monitoring:")
        print("  /status      - System status & resonance field")
        print("  /history     - Action history")
        print("\n⚙️  System:")
        print("  /help        - Show this help")
        print("  /quit        - Exit LAM")
        print()

        while True:
            try:
                cmd = input("\n🤖 LAM> ").strip()

                if not cmd:
                    continue

                if cmd in ['/quit', '/exit', 'quit', 'exit', 'q']:
                    print("\n👋 Quantum resonance field collapsing gracefully...")
                    print("   LAM session ended.\n")
                    break

                elif cmd == '/help':
                    # Re-show capabilities
                    continue

                elif cmd == '/wizard':
                    self.run_setup_wizard()

                elif cmd == '/lab':
                    self.run_lab_assistant()

                elif cmd.startswith('/trip'):
                    print("Quick trip planning wizard:")
                    dest = input("  Destination: ").strip()
                    dep = input("  Departure date (YYYY-MM-DD): ").strip()
                    ret = input("  Return date (YYYY-MM-DD): ").strip()
                    budget_str = input("  Budget (optional): ").strip()
                    budget = float(budget_str) if budget_str else None
                    self.plan_trip(dest, dep, ret, budget)

                elif cmd.startswith('/reserve'):
                    print("Reservation wizard:")
                    venue_type = input("  Type (restaurant/event/etc): ").strip()
                    venue_name = input("  Venue name: ").strip()
                    date = input("  Date (YYYY-MM-DD): ").strip()
                    time = input("  Time (HH:MM): ").strip()
                    party_size = int(input("  Party size: ").strip())
                    self.make_reservation(venue_type, venue_name, date, time, party_size)

                elif cmd.startswith('/order'):
                    print("Food order wizard:")
                    restaurant = input("  Restaurant: ").strip()
                    address = input("  Delivery address: ").strip()
                    print("  Items (name,price,quantity per line, empty to finish):")
                    items = []
                    while True:
                        item_str = input("    ").strip()
                        if not item_str:
                            break
                        parts = item_str.split(',')
                        if len(parts) >= 2:
                            items.append({
                                "name": parts[0].strip(),
                                "price": float(parts[1].strip()),
                                "quantity": int(parts[2].strip()) if len(parts) > 2 else 1
                            })
                    self.order_food(restaurant, items, address)

                elif cmd.startswith('/cancel-sub'):
                    service = input("  Service name: ").strip()
                    sub_id = input("  Subscription ID: ").strip()
                    self.cancel_subscription(service, sub_id)

                elif cmd.startswith('/ask'):
                    question = cmd[5:].strip()
                    if not question:
                        question = input("  Question: ").strip()
                    self.ask_question(question)

                elif cmd.startswith('/task'):
                    task = cmd[6:].strip()
                    if not task:
                        task = input("  Task: ").strip()
                    self.complete_task(task)

                elif cmd == '/status':
                    self.get_status()

                elif cmd == '/history':
                    print(f"\n📜 Action History ({len(self.engine.action_history)} total):")
                    for i, action in enumerate(self.engine.action_history[-10:], 1):
                        print(f"  {i}. {action['action_type']} @ {action['timestamp']}")

                elif cmd.startswith('/'):
                    print("❌ Unknown command. Type /help for available commands.")

                else:
                    # Treat as a question
                    self.ask_question(cmd)

            except KeyboardInterrupt:
                print("\n\n⚠️  Interrupted. Use /quit to exit gracefully.")
            except EOFError:
                print("\n\n👋 EOF detected. Exiting...")
                break
            except Exception as e:
                print(f"\n❌ Error: {e}")
                import traceback
                traceback.print_exc()


def main():
    """Main entry point"""
    lam = LAM()

    if len(sys.argv) > 1:
        command = sys.argv[1]

        if command == 'wizard':
            lam.run_setup_wizard()

        elif command == 'lab':
            lam.run_lab_assistant()

        elif command == 'status':
            lam.get_status()

        elif command == 'help':
            print("LAM - Large Action Model")
            print("\nUsage:")
            print("  python lam_main.py          - Interactive mode")
            print("  python lam_main.py wizard   - Setup wizard")
            print("  python lam_main.py lab      - Lab assistant")
            print("  python lam_main.py status   - System status")

        else:
            print(f"Unknown command: {command}")
            print("Use: python lam_main.py help")
    else:
        # Interactive mode
        lam.interactive_mode()


if __name__ == "__main__":
    main()
