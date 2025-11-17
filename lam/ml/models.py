#!/usr/bin/env python3
"""
LAM Custom ML Models
Machine learning for intelligent recommendations
"""
import numpy as np
from typing import Dict, Any, List
import json


class TripRecommender:
    """ML-based trip recommendation engine"""

    def __init__(self):
        self.user_preferences = {}
        self.trip_history = []

    def train(self, user_id: str, trips: List[Dict[str, Any]]):
        """Train recommender on user's trip history"""
        # Simple preference extraction
        destinations = [t.get("destination") for t in trips]
        budgets = [t.get("budget", 0) for t in trips if t.get("budget")]

        self.user_preferences[user_id] = {
            "favorite_destinations": list(set(destinations)),
            "avg_budget": np.mean(budgets) if budgets else 2000,
            "trip_count": len(trips)
        }

    def recommend(self, user_id: str, num_recommendations: int = 3) -> List[Dict[str, Any]]:
        """Generate trip recommendations"""
        prefs = self.user_preferences.get(user_id, {})
        avg_budget = prefs.get("avg_budget", 2000)

        # Mock recommendations based on budget tiers
        all_recommendations = [
            {"destination": "Paris", "estimated_cost": 2500, "score": 0.95},
            {"destination": "Tokyo", "estimated_cost": 3500, "score": 0.90},
            {"destination": "London", "estimated_cost": 2200, "score": 0.88},
            {"destination": "Barcelona", "estimated_cost": 1800, "score": 0.85},
            {"destination": "Dubai", "estimated_cost": 3000, "score": 0.82},
        ]

        # Filter by budget
        suitable = [r for r in all_recommendations if r["estimated_cost"] <= avg_budget * 1.3]

        return suitable[:num_recommendations]


class SentimentAnalyzer:
    """Analyze sentiment of user queries"""

    def analyze(self, text: str) -> Dict[str, Any]:
        """Analyze sentiment of text"""
        # Simple keyword-based sentiment (replace with actual ML model)
        positive_words = ["good", "great", "excellent", "amazing", "wonderful"]
        negative_words = ["bad", "poor", "terrible", "awful", "horrible"]

        text_lower = text.lower()

        positive_count = sum(1 for word in positive_words if word in text_lower)
        negative_count = sum(1 for word in negative_words if word in text_lower)

        if positive_count > negative_count:
            sentiment = "positive"
            score = 0.7 + (positive_count * 0.1)
        elif negative_count > positive_count:
            sentiment = "negative"
            score = 0.3 - (negative_count * 0.1)
        else:
            sentiment = "neutral"
            score = 0.5

        return {
            "sentiment": sentiment,
            "score": max(0, min(1, score)),
            "confidence": 0.8
        }


class QuantumOptimizer:
    """Quantum-inspired optimization for LAM decisions"""

    def __init__(self):
        self.lightfoot = 0.16905
        self.donte = 149.9992314000

    def optimize_route(self, waypoints: List[Dict[str, float]]) -> List[Dict[str, float]]:
        """Optimize travel route using quantum-inspired algorithm"""
        # Simplified quantum annealing simulation
        # In production, use actual quantum computing or advanced optimization

        if len(waypoints) <= 2:
            return waypoints

        # Simple nearest-neighbor with Lightfoot decay
        optimized = [waypoints[0]]
        remaining = waypoints[1:]

        while remaining:
            current = optimized[-1]
            # Find nearest with quantum decay factor
            distances = [
                self._distance(current, wp) * np.exp(-self.lightfoot * len(optimized))
                for wp in remaining
            ]

            nearest_idx = np.argmin(distances)
            optimized.append(remaining.pop(nearest_idx))

        return optimized

    def _distance(self, p1: Dict[str, float], p2: Dict[str, float]) -> float:
        """Calculate distance between two points"""
        return np.sqrt(
            (p1.get("lat", 0) - p2.get("lat", 0))**2 +
            (p1.get("lon", 0) - p2.get("lon", 0))**2
        )


class MLPipeline:
    """ML pipeline orchestrator"""

    def __init__(self):
        self.trip_recommender = TripRecommender()
        self.sentiment_analyzer = SentimentAnalyzer()
        self.quantum_optimizer = QuantumOptimizer()

    def process_query(self, query: str, user_id: str) -> Dict[str, Any]:
        """Process user query through ML pipeline"""
        # Analyze sentiment
        sentiment = self.sentiment_analyzer.analyze(query)

        # Get recommendations
        recommendations = self.trip_recommender.recommend(user_id)

        return {
            "query": query,
            "sentiment": sentiment,
            "recommendations": recommendations,
            "quantum_optimized": True
        }


if __name__ == "__main__":
    print("=== LAM Machine Learning Models ===\n")

    # Test trip recommender
    print("1. Trip Recommender:")
    recommender = TripRecommender()
    recommender.train("user123", [
        {"destination": "Paris", "budget": 2000},
        {"destination": "London", "budget": 1800}
    ])
    recs = recommender.recommend("user123")
    print(json.dumps(recs, indent=2))

    # Test sentiment analyzer
    print("\n2. Sentiment Analyzer:")
    analyzer = SentimentAnalyzer()
    sentiment = analyzer.analyze("This is an amazing trip planning service!")
    print(json.dumps(sentiment, indent=2))

    # Test quantum optimizer
    print("\n3. Quantum Optimizer:")
    optimizer = QuantumOptimizer()
    waypoints = [
        {"name": "NYC", "lat": 40.7, "lon": -74.0},
        {"name": "Boston", "lat": 42.3, "lon": -71.0},
        {"name": "DC", "lat": 38.9, "lon": -77.0}
    ]
    optimized = optimizer.optimize_route(waypoints)
    print("Optimized route:", [w["name"] for w in optimized])
