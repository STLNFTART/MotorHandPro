#!/usr/bin/env python3
"""
LAM Lab Assistant - Experiment Goal Management and Guidance
Helps users define, track, and achieve experimental objectives
"""
import json
import sys
from pathlib import Path
from typing import Dict, Any, List, Optional
from datetime import datetime
from dataclasses import dataclass, asdict


@dataclass
class ExperimentGoal:
    """Represents an experimental goal"""
    goal_id: str
    title: str
    description: str
    objective: str
    success_criteria: List[str]
    parameters: Dict[str, Any]
    status: str  # planning, running, analyzing, complete, failed
    created_at: str
    updated_at: str
    results: Optional[Dict[str, Any]] = None
    notes: Optional[List[str]] = None


class LabAssistant:
    """
    Intelligent lab assistant for experiment management
    Uses Primal Logic framework principles for goal tracking
    """
    def __init__(self, workspace_dir: Optional[Path] = None):
        if workspace_dir is None:
            workspace_dir = Path.home() / ".lam" / "experiments"

        self.workspace_dir = Path(workspace_dir)
        self.workspace_dir.mkdir(parents=True, exist_ok=True)

        self.goals_file = self.workspace_dir / "goals.json"
        self.goals: Dict[str, ExperimentGoal] = {}

        self._load_goals()

        # Experiment templates
        self.templates = {
            "motor_control": {
                "title": "Motor Control Experiment",
                "parameters": ["KE", "MU", "duration", "sample_rate"],
                "success_criteria": [
                    "Lipschitz constant < 1.0",
                    "Bounded convergence demonstrated",
                    "No integral windup"
                ]
            },
            "algorithm_validation": {
                "title": "Algorithm Validation",
                "parameters": ["algorithm_type", "test_data", "expected_output"],
                "success_criteria": [
                    "Accuracy > 95%",
                    "Performance within bounds",
                    "Cross-validation passed"
                ]
            },
            "quantum_resonance": {
                "title": "Quantum Resonance Field Study",
                "parameters": ["alpha", "lambda", "epoch_count"],
                "success_criteria": [
                    "Resonance field stability maintained",
                    "Semantic bounds preserved",
                    "Attractor convergence achieved"
                ]
            },
            "satellite_integration": {
                "title": "Satellite System Integration",
                "parameters": ["satellite_count", "orbital_parameters", "communication_range"],
                "success_criteria": [
                    "All satellites communicating",
                    "Position accuracy within tolerance",
                    "No collision risks"
                ]
            }
        }

    def _load_goals(self):
        """Load experiment goals from file"""
        if self.goals_file.exists():
            with open(self.goals_file, 'r') as f:
                data = json.load(f)
                for goal_id, goal_data in data.items():
                    self.goals[goal_id] = ExperimentGoal(**goal_data)

    def _save_goals(self):
        """Save experiment goals to file"""
        data = {goal_id: asdict(goal) for goal_id, goal in self.goals.items()}
        with open(self.goals_file, 'w') as f:
            json.dump(data, f, indent=2)

    def create_goal(self, title: str, description: str, objective: str,
                   success_criteria: List[str], parameters: Dict[str, Any]) -> str:
        """Create a new experiment goal"""
        goal_id = f"exp_{len(self.goals)+1:04d}"
        timestamp = datetime.now().isoformat()

        goal = ExperimentGoal(
            goal_id=goal_id,
            title=title,
            description=description,
            objective=objective,
            success_criteria=success_criteria,
            parameters=parameters,
            status="planning",
            created_at=timestamp,
            updated_at=timestamp,
            notes=[]
        )

        self.goals[goal_id] = goal
        self._save_goals()

        return goal_id

    def create_from_template(self, template_name: str, custom_params: Dict[str, Any]) -> str:
        """Create goal from template"""
        if template_name not in self.templates:
            raise ValueError(f"Unknown template: {template_name}")

        template = self.templates[template_name]

        # Build parameters
        parameters = {param: custom_params.get(param, "TBD") for param in template['parameters']}

        return self.create_goal(
            title=template['title'],
            description=f"Experiment based on {template_name} template",
            objective=custom_params.get('objective', 'Validate theoretical predictions'),
            success_criteria=template['success_criteria'],
            parameters=parameters
        )

    def update_goal_status(self, goal_id: str, new_status: str, notes: Optional[str] = None):
        """Update goal status"""
        if goal_id not in self.goals:
            print(f"Goal {goal_id} not found")
            return

        self.goals[goal_id].status = new_status
        self.goals[goal_id].updated_at = datetime.now().isoformat()

        if notes:
            if self.goals[goal_id].notes is None:
                self.goals[goal_id].notes = []
            self.goals[goal_id].notes.append(f"[{new_status}] {notes}")

        self._save_goals()

    def record_results(self, goal_id: str, results: Dict[str, Any]):
        """Record experiment results"""
        if goal_id not in self.goals:
            print(f"Goal {goal_id} not found")
            return

        self.goals[goal_id].results = results
        self.goals[goal_id].updated_at = datetime.now().isoformat()
        self._save_goals()

    def analyze_goal(self, goal_id: str) -> Dict[str, Any]:
        """Analyze goal progress and provide recommendations"""
        if goal_id not in self.goals:
            return {"error": "Goal not found"}

        goal = self.goals[goal_id]

        analysis = {
            "goal_id": goal_id,
            "title": goal.title,
            "status": goal.status,
            "progress": self._calculate_progress(goal),
            "recommendations": self._generate_recommendations(goal),
            "next_steps": self._suggest_next_steps(goal)
        }

        return analysis

    def _calculate_progress(self, goal: ExperimentGoal) -> float:
        """Calculate progress percentage"""
        status_progress = {
            "planning": 0.0,
            "running": 0.5,
            "analyzing": 0.75,
            "complete": 1.0,
            "failed": 0.0
        }

        base_progress = status_progress.get(goal.status, 0.0)

        # Adjust based on results
        if goal.results:
            criteria_met = sum(1 for criteria in goal.success_criteria
                             if self._check_criteria(criteria, goal.results))
            criteria_progress = criteria_met / len(goal.success_criteria) if goal.success_criteria else 0
            return (base_progress + criteria_progress) / 2

        return base_progress

    def _check_criteria(self, criteria: str, results: Dict[str, Any]) -> bool:
        """Check if success criteria is met (simplified)"""
        # Simple keyword matching for now
        criteria_lower = criteria.lower()

        if "lipschitz" in criteria_lower:
            return results.get("lipschitz_constant", 1.0) < 1.0
        elif "bounded" in criteria_lower:
            return results.get("bounded_convergence", False)
        elif "accuracy" in criteria_lower:
            return results.get("accuracy", 0) > 0.95
        elif "stability" in criteria_lower:
            return results.get("stability", False)

        return False

    def _generate_recommendations(self, goal: ExperimentGoal) -> List[str]:
        """Generate recommendations based on goal state"""
        recommendations = []

        if goal.status == "planning":
            recommendations.append("Define all parameters before starting")
            recommendations.append("Review success criteria for completeness")
            recommendations.append("Prepare data collection infrastructure")

        elif goal.status == "running":
            recommendations.append("Monitor for anomalies in real-time")
            recommendations.append("Record intermediate results")
            recommendations.append("Verify data quality continuously")

        elif goal.status == "analyzing":
            recommendations.append("Compare results against success criteria")
            recommendations.append("Look for unexpected patterns")
            recommendations.append("Document findings thoroughly")

        elif goal.status == "complete":
            recommendations.append("Archive results and data")
            recommendations.append("Update documentation")
            recommendations.append("Consider follow-up experiments")

        # Parameter-specific recommendations
        if "KE" in goal.parameters:
            recommendations.append(f"Current KE={goal.parameters['KE']}: Monitor for overshoot")

        if "alpha" in goal.parameters or "lambda" in goal.parameters:
            recommendations.append("Verify quantum resonance field stability")

        return recommendations

    def _suggest_next_steps(self, goal: ExperimentGoal) -> List[str]:
        """Suggest next steps based on current state"""
        steps = []

        status_steps = {
            "planning": [
                "1. Finalize all parameters",
                "2. Set up measurement infrastructure",
                "3. Run preliminary validation",
                "4. Start experiment (update status to 'running')"
            ],
            "running": [
                "1. Monitor progress continuously",
                "2. Collect and validate data",
                "3. Check intermediate results",
                "4. Complete run and update status to 'analyzing'"
            ],
            "analyzing": [
                "1. Process collected data",
                "2. Evaluate against success criteria",
                "3. Generate visualizations",
                "4. Mark as 'complete' or 'failed' based on results"
            ],
            "complete": [
                "1. Document conclusions",
                "2. Archive experiment data",
                "3. Plan follow-up experiments if needed"
            ],
            "failed": [
                "1. Analyze failure modes",
                "2. Adjust parameters or approach",
                "3. Create new goal with lessons learned"
            ]
        }

        return status_steps.get(goal.status, ["No specific steps available"])

    def list_goals(self, status_filter: Optional[str] = None) -> List[ExperimentGoal]:
        """List all goals, optionally filtered by status"""
        goals = list(self.goals.values())

        if status_filter:
            goals = [g for g in goals if g.status == status_filter]

        return sorted(goals, key=lambda g: g.created_at, reverse=True)

    def interactive_assistant(self):
        """Interactive lab assistant mode"""
        print("\n" + "="*60)
        print("LAM LAB ASSISTANT")
        print("Experiment Goal Management and Guidance")
        print("="*60)

        while True:
            print("\nCommands:")
            print("  new      - Create new experiment goal")
            print("  template - Create from template")
            print("  list     - List all goals")
            print("  analyze <id> - Analyze goal")
            print("  update <id> <status> - Update status")
            print("  results <id> - Record results")
            print("  quit     - Exit")

            cmd = input("\nLab> ").strip().split()

            if not cmd:
                continue

            action = cmd[0].lower()

            if action in ['quit', 'q', 'exit']:
                break

            elif action == 'new':
                self._interactive_create_goal()

            elif action == 'template':
                self._interactive_template()

            elif action == 'list':
                status_filter = cmd[1] if len(cmd) > 1 else None
                self._print_goals(status_filter)

            elif action == 'analyze' and len(cmd) > 1:
                goal_id = cmd[1]
                analysis = self.analyze_goal(goal_id)
                self._print_analysis(analysis)

            elif action == 'update' and len(cmd) > 2:
                goal_id = cmd[1]
                new_status = cmd[2]
                notes = input("Notes (optional): ").strip() or None
                self.update_goal_status(goal_id, new_status, notes)
                print(f"✓ Updated {goal_id} to {new_status}")

            elif action == 'results' and len(cmd) > 1:
                goal_id = cmd[1]
                self._interactive_record_results(goal_id)

            else:
                print("Unknown command")

        print("\nLab assistant session ended")

    def _interactive_create_goal(self):
        """Interactive goal creation"""
        print("\n=== New Experiment Goal ===")

        title = input("Title: ").strip()
        description = input("Description: ").strip()
        objective = input("Objective: ").strip()

        print("\nSuccess Criteria (one per line, empty line to finish):")
        success_criteria = []
        while True:
            criteria = input(f"  {len(success_criteria)+1}. ").strip()
            if not criteria:
                break
            success_criteria.append(criteria)

        print("\nParameters (key=value, empty line to finish):")
        parameters = {}
        while True:
            param = input(f"  ").strip()
            if not param:
                break
            if '=' in param:
                key, value = param.split('=', 1)
                parameters[key.strip()] = value.strip()

        goal_id = self.create_goal(title, description, objective, success_criteria, parameters)
        print(f"\n✓ Created goal: {goal_id}")

    def _interactive_template(self):
        """Interactive template-based creation"""
        print("\nAvailable Templates:")
        for i, (name, template) in enumerate(self.templates.items(), 1):
            print(f"  {i}. {template['title']} ({name})")

        choice = input("\nSelect template: ").strip()

        if choice.isdigit():
            idx = int(choice) - 1
            templates = list(self.templates.keys())
            if 0 <= idx < len(templates):
                template_name = templates[idx]
                template = self.templates[template_name]

                print(f"\n=== {template['title']} ===")
                print(f"Required parameters: {', '.join(template['parameters'])}")

                custom_params = {}
                for param in template['parameters']:
                    value = input(f"{param}: ").strip()
                    if value:
                        custom_params[param] = value

                custom_params['objective'] = input("Objective: ").strip()

                goal_id = self.create_from_template(template_name, custom_params)
                print(f"\n✓ Created goal from template: {goal_id}")

    def _interactive_record_results(self, goal_id: str):
        """Interactive results recording"""
        print(f"\n=== Record Results for {goal_id} ===")

        results = {}
        print("Enter results (key=value, empty line to finish):")

        while True:
            entry = input("  ").strip()
            if not entry:
                break
            if '=' in entry:
                key, value = entry.split('=', 1)
                # Try to convert to number
                try:
                    value = float(value)
                except ValueError:
                    pass
                results[key.strip()] = value

        self.record_results(goal_id, results)
        print(f"\n✓ Results recorded for {goal_id}")

    def _print_goals(self, status_filter: Optional[str] = None):
        """Print goals list"""
        goals = self.list_goals(status_filter)

        if not goals:
            print("\nNo goals found")
            return

        print(f"\n{'ID':<12} {'Title':<30} {'Status':<12} {'Progress':<10}")
        print("-" * 70)

        for goal in goals:
            progress = self._calculate_progress(goal) * 100
            print(f"{goal.goal_id:<12} {goal.title[:30]:<30} {goal.status:<12} {progress:>6.1f}%")

    def _print_analysis(self, analysis: Dict[str, Any]):
        """Print goal analysis"""
        if "error" in analysis:
            print(f"\nError: {analysis['error']}")
            return

        print(f"\n=== Analysis: {analysis['title']} ({analysis['goal_id']}) ===")
        print(f"\nStatus: {analysis['status']}")
        print(f"Progress: {analysis['progress']*100:.1f}%")

        print("\nRecommendations:")
        for rec in analysis['recommendations']:
            print(f"  • {rec}")

        print("\nNext Steps:")
        for step in analysis['next_steps']:
            print(f"  {step}")


def main():
    """Main entry point"""
    assistant = LabAssistant()

    if len(sys.argv) > 1:
        command = sys.argv[1]

        if command == 'list':
            status_filter = sys.argv[2] if len(sys.argv) > 2 else None
            goals = assistant.list_goals(status_filter)
            assistant._print_goals(status_filter)

        elif command == 'analyze' and len(sys.argv) > 2:
            goal_id = sys.argv[2]
            analysis = assistant.analyze_goal(goal_id)
            assistant._print_analysis(analysis)

        else:
            print("Usage: python lab_assistant.py [list|analyze <id>]")
            print("   or: python lab_assistant.py  (interactive mode)")
    else:
        assistant.interactive_assistant()


if __name__ == "__main__":
    main()
