#!/usr/bin/env python3
"""
MongoDB Integration for MotorHandPro
Connects to MongoDB Atlas and provides database operations for:
- LAM actions
- NASA observations
- Test results
- Satellite data

Author: Donte Lightfoot
Date: December 11, 2025
"""

import os
from datetime import datetime
from typing import Dict, List, Any, Optional
from pymongo import MongoClient, ASCENDING, DESCENDING
from pymongo.errors import ConnectionFailure, OperationFailure


class MotorHandProDB:
    """
    MongoDB database client for MotorHandPro
    """

    def __init__(self, connection_string: Optional[str] = None):
        """
        Initialize MongoDB connection

        Args:
            connection_string: MongoDB connection string (defaults to env var)
        """
        self.connection_string = connection_string or os.getenv('MONGODB_URI')

        if not self.connection_string:
            raise ValueError(
                "MongoDB connection string not provided. "
                "Set MONGODB_URI environment variable or pass connection_string parameter"
            )

        # Connect to MongoDB
        self.client = MongoClient(self.connection_string)
        self.db = self.client.MotorHandProDB

        # Collections
        self.lam_actions = self.db.lam_actions
        self.nasa_observations = self.db.nasa_observations
        self.test_results = self.db.test_results
        self.satellites = self.db.satellites

        # Create indexes for performance
        self._create_indexes()

    def _create_indexes(self):
        """Create database indexes for performance"""
        try:
            # LAM actions indexes
            self.lam_actions.create_index([("timestamp", DESCENDING)])
            self.lam_actions.create_index([("action_type", ASCENDING)])
            self.lam_actions.create_index([("user_id", ASCENDING)])

            # NASA observations indexes
            self.nasa_observations.create_index([("timestamp", DESCENDING)])
            self.nasa_observations.create_index([("comet_id", ASCENDING)])

            # Test results indexes
            self.test_results.create_index([("timestamp", DESCENDING)])
            self.test_results.create_index([("branch", ASCENDING)])
            self.test_results.create_index([("success", ASCENDING)])

            # Satellite indexes
            self.satellites.create_index([("timestamp", DESCENDING)])
            self.satellites.create_index([("satellite_id", ASCENDING)])

        except OperationFailure as e:
            print(f"⚠️  Warning: Could not create indexes: {e}")

    def verify_connection(self) -> bool:
        """
        Verify MongoDB connection is working

        Returns:
            True if connection successful, False otherwise
        """
        try:
            # Ping the server
            self.client.admin.command('ping')
            print("✅ MongoDB connection successful")
            return True
        except ConnectionFailure as e:
            print(f"❌ MongoDB connection failed: {e}")
            return False

    # ========================================================================
    # LAM Actions
    # ========================================================================

    def save_lam_action(
        self,
        action_type: str,
        user_id: str,
        primal_state: Dict[str, float],
        lam_integration: Optional[Dict[str, Any]] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Save LAM action to database

        Args:
            action_type: Type of action (e.g., "plan_trip", "make_reservation")
            user_id: User identifier
            primal_state: Primal Logic state data
            lam_integration: LAM integration data (optional)
            metadata: Additional metadata (optional)

        Returns:
            Inserted document ID
        """
        document = {
            'timestamp': datetime.utcnow(),
            'action_type': action_type,
            'user_id': user_id,
            'primal_state': primal_state,
            'lam_integration': lam_integration or {},
            'metadata': metadata or {}
        }

        result = self.lam_actions.insert_one(document)
        return str(result.inserted_id)

    def get_lam_actions(
        self,
        user_id: Optional[str] = None,
        action_type: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retrieve LAM actions from database

        Args:
            user_id: Filter by user ID (optional)
            action_type: Filter by action type (optional)
            limit: Maximum number of results

        Returns:
            List of LAM action documents
        """
        query = {}

        if user_id:
            query['user_id'] = user_id

        if action_type:
            query['action_type'] = action_type

        cursor = self.lam_actions.find(query).sort('timestamp', DESCENDING).limit(limit)
        return list(cursor)

    def get_lam_action_stats(self) -> Dict[str, Any]:
        """
        Get statistics about LAM actions

        Returns:
            Dictionary with action statistics
        """
        total_actions = self.lam_actions.count_documents({})

        # Count by action type
        pipeline = [
            {
                '$group': {
                    '_id': '$action_type',
                    'count': {'$sum': 1}
                }
            }
        ]
        action_types = list(self.lam_actions.aggregate(pipeline))

        return {
            'total_actions': total_actions,
            'action_types': {item['_id']: item['count'] for item in action_types}
        }

    # ========================================================================
    # NASA Observations
    # ========================================================================

    def save_nasa_observation(
        self,
        comet_id: str,
        observation: Dict[str, Any],
        primal_state: Dict[str, float],
        lam_integration: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Save NASA observation to database

        Args:
            comet_id: Comet identifier (e.g., "3I/ATLAS")
            observation: Observation data
            primal_state: Primal Logic state
            lam_integration: LAM integration data (optional)

        Returns:
            Inserted document ID
        """
        document = {
            'timestamp': datetime.utcnow(),
            'comet_id': comet_id,
            'observation': observation,
            'primal_state': primal_state,
            'lam_integration': lam_integration or {}
        }

        result = self.nasa_observations.insert_one(document)
        return str(result.inserted_id)

    def save_nasa_observations_batch(
        self,
        comet_id: str,
        observations: List[Dict[str, Any]]
    ) -> List[str]:
        """
        Save multiple NASA observations in batch

        Args:
            comet_id: Comet identifier
            observations: List of observation documents

        Returns:
            List of inserted document IDs
        """
        documents = [
            {
                'timestamp': obs.get('timestamp', datetime.utcnow()),
                'comet_id': comet_id,
                'observation': obs.get('observation', {}),
                'primal_state': obs.get('primal_state', {}),
                'lam_integration': obs.get('lam_integration', {})
            }
            for obs in observations
        ]

        result = self.nasa_observations.insert_many(documents)
        return [str(id) for id in result.inserted_ids]

    def get_nasa_observations(
        self,
        comet_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retrieve NASA observations

        Args:
            comet_id: Filter by comet ID (optional)
            limit: Maximum number of results

        Returns:
            List of observation documents
        """
        query = {}

        if comet_id:
            query['comet_id'] = comet_id

        cursor = self.nasa_observations.find(query).sort('timestamp', DESCENDING).limit(limit)
        return list(cursor)

    def get_nasa_observation_stats(self) -> Dict[str, Any]:
        """
        Get statistics about NASA observations

        Returns:
            Dictionary with observation statistics
        """
        total_observations = self.nasa_observations.count_documents({})

        # Count by comet
        pipeline = [
            {
                '$group': {
                    '_id': '$comet_id',
                    'count': {'$sum': 1}
                }
            }
        ]
        comets = list(self.nasa_observations.aggregate(pipeline))

        return {
            'total_observations': total_observations,
            'comets': {item['_id']: item['count'] for item in comets}
        }

    # ========================================================================
    # Test Results
    # ========================================================================

    def save_test_result(
        self,
        branch: str,
        test_type: str,
        success: bool,
        results: Dict[str, Any],
        primal_constants: Optional[Dict[str, float]] = None
    ) -> str:
        """
        Save test result to database

        Args:
            branch: Git branch name
            test_type: Type of test (e.g., "LAM Core", "LAM Actions")
            success: Whether test passed
            results: Test result data
            primal_constants: Primal Logic constants used (optional)

        Returns:
            Inserted document ID
        """
        document = {
            'timestamp': datetime.utcnow(),
            'branch': branch,
            'test_type': test_type,
            'success': success,
            'results': results,
            'primal_constants': primal_constants or {}
        }

        result = self.test_results.insert_one(document)
        return str(result.inserted_id)

    def get_test_results(
        self,
        branch: Optional[str] = None,
        test_type: Optional[str] = None,
        success: Optional[bool] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retrieve test results

        Args:
            branch: Filter by branch (optional)
            test_type: Filter by test type (optional)
            success: Filter by success status (optional)
            limit: Maximum number of results

        Returns:
            List of test result documents
        """
        query = {}

        if branch:
            query['branch'] = branch

        if test_type:
            query['test_type'] = test_type

        if success is not None:
            query['success'] = success

        cursor = self.test_results.find(query).sort('timestamp', DESCENDING).limit(limit)
        return list(cursor)

    def get_test_result_stats(self) -> Dict[str, Any]:
        """
        Get statistics about test results

        Returns:
            Dictionary with test statistics
        """
        total_tests = self.test_results.count_documents({})
        passed_tests = self.test_results.count_documents({'success': True})
        failed_tests = self.test_results.count_documents({'success': False})

        # Count by branch
        pipeline = [
            {
                '$group': {
                    '_id': '$branch',
                    'total': {'$sum': 1},
                    'passed': {
                        '$sum': {'$cond': ['$success', 1, 0]}
                    }
                }
            }
        ]
        branches = list(self.test_results.aggregate(pipeline))

        return {
            'total_tests': total_tests,
            'passed_tests': passed_tests,
            'failed_tests': failed_tests,
            'pass_rate': (passed_tests / total_tests * 100) if total_tests > 0 else 0,
            'branches': {
                item['_id']: {
                    'total': item['total'],
                    'passed': item['passed'],
                    'pass_rate': (item['passed'] / item['total'] * 100) if item['total'] > 0 else 0
                }
                for item in branches
            }
        }

    # ========================================================================
    # Satellites
    # ========================================================================

    def save_satellite_data(
        self,
        satellite_id: str,
        position: Dict[str, float],
        velocity: Dict[str, float],
        metadata: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Save satellite tracking data

        Args:
            satellite_id: Satellite identifier
            position: Position data (x, y, z)
            velocity: Velocity data (vx, vy, vz)
            metadata: Additional metadata (optional)

        Returns:
            Inserted document ID
        """
        document = {
            'timestamp': datetime.utcnow(),
            'satellite_id': satellite_id,
            'position': position,
            'velocity': velocity,
            'metadata': metadata or {}
        }

        result = self.satellites.insert_one(document)
        return str(result.inserted_id)

    def get_satellite_data(
        self,
        satellite_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """
        Retrieve satellite tracking data

        Args:
            satellite_id: Filter by satellite ID (optional)
            limit: Maximum number of results

        Returns:
            List of satellite data documents
        """
        query = {}

        if satellite_id:
            query['satellite_id'] = satellite_id

        cursor = self.satellites.find(query).sort('timestamp', DESCENDING).limit(limit)
        return list(cursor)

    # ========================================================================
    # Utility Methods
    # ========================================================================

    def get_database_stats(self) -> Dict[str, Any]:
        """
        Get overall database statistics

        Returns:
            Dictionary with database statistics
        """
        return {
            'lam_actions': {
                'count': self.lam_actions.count_documents({}),
                'stats': self.get_lam_action_stats()
            },
            'nasa_observations': {
                'count': self.nasa_observations.count_documents({}),
                'stats': self.get_nasa_observation_stats()
            },
            'test_results': {
                'count': self.test_results.count_documents({}),
                'stats': self.get_test_result_stats()
            },
            'satellites': {
                'count': self.satellites.count_documents({})
            }
        }

    def close(self):
        """Close database connection"""
        self.client.close()
        print("✅ MongoDB connection closed")


# Example usage
if __name__ == "__main__":
    import json

    print("=" * 80)
    print("MongoDB Integration Test")
    print("=" * 80)
    print()

    # Initialize database
    db = MotorHandProDB()

    # Verify connection
    if not db.verify_connection():
        print("❌ Connection failed, exiting")
        exit(1)

    print()

    # Get database statistics
    print("📊 Database Statistics:")
    print("-" * 80)
    stats = db.get_database_stats()
    print(json.dumps(stats, indent=2, default=str))
    print()

    # Close connection
    db.close()
