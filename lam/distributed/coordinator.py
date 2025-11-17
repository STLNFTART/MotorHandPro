#!/usr/bin/env python3
"""
Distributed LAM Coordinator
Manages multiple LAM instances for load balancing and redundancy
"""
import sys
import asyncio
import json
from pathlib import Path
from typing import Dict, Any, List, Optional
from datetime import datetime
import hashlib

sys.path.insert(0, str(Path(__file__).parent.parent.parent))
sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    import websockets
    WEBSOCKETS_AVAILABLE = True
except ImportError:
    WEBSOCKETS_AVAILABLE = False


class LAMNode:
    """Represents a single LAM instance"""

    def __init__(self, node_id: str, host: str, port: int):
        self.node_id = node_id
        self.host = host
        self.port = port
        self.status = "initializing"
        self.load = 0.0
        self.last_heartbeat = None
        self.capabilities = []

    def get_url(self) -> str:
        return f"http://{self.host}:{self.port}"

    def is_healthy(self) -> bool:
        if self.last_heartbeat is None:
            return False
        # Check if heartbeat is recent (within 30 seconds)
        elapsed = (datetime.now() - self.last_heartbeat).total_seconds()
        return elapsed < 30 and self.status == "running"


class DistributedLAM:
    """
    Distributed LAM Coordinator
    Load balances requests across multiple LAM instances
    """

    def __init__(self):
        self.nodes: Dict[str, LAMNode] = {}
        self.request_count = 0

    def register_node(self, host: str, port: int, capabilities: List[str] = None) -> str:
        """Register a new LAM node"""
        node_id = self._generate_node_id(host, port)

        node = LAMNode(node_id, host, port)
        node.capabilities = capabilities or []
        node.status = "running"
        node.last_heartbeat = datetime.now()

        self.nodes[node_id] = node

        print(f"✓ Registered node {node_id} at {node.get_url()}")
        return node_id

    def _generate_node_id(self, host: str, port: int) -> str:
        """Generate unique node ID"""
        data = f"{host}:{port}:{datetime.now().isoformat()}"
        return hashlib.md5(data.encode()).hexdigest()[:8]

    def get_node_for_request(self, required_capability: Optional[str] = None) -> Optional[LAMNode]:
        """
        Select best node for a request using load balancing
        """
        # Filter healthy nodes
        healthy_nodes = [n for n in self.nodes.values() if n.is_healthy()]

        if not healthy_nodes:
            return None

        # Filter by capability if specified
        if required_capability:
            capable_nodes = [n for n in healthy_nodes if required_capability in n.capabilities]
            if capable_nodes:
                healthy_nodes = capable_nodes

        # Select node with lowest load
        return min(healthy_nodes, key=lambda n: n.load)

    def update_node_heartbeat(self, node_id: str, load: float, status: str):
        """Update node heartbeat"""
        if node_id in self.nodes:
            self.nodes[node_id].last_heartbeat = datetime.now()
            self.nodes[node_id].load = load
            self.nodes[node_id].status = status

    def remove_node(self, node_id: str):
        """Remove a node"""
        if node_id in self.nodes:
            del self.nodes[node_id]
            print(f"✗ Removed node {node_id}")

    async def distribute_request(self, action_type: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """
        Distribute a request to an appropriate node
        """
        node = self.get_node_for_request()

        if not node:
            return {
                "success": False,
                "error": "No healthy nodes available"
            }

        # In production, make HTTP request to node
        # For now, return simulation
        self.request_count += 1
        node.load += 0.1  # Simulate load increase

        return {
            "success": True,
            "node_id": node.node_id,
            "action_type": action_type,
            "result": f"Processed by node {node.node_id}"
        }

    def get_cluster_status(self) -> Dict[str, Any]:
        """Get status of the entire cluster"""
        healthy = sum(1 for n in self.nodes.values() if n.is_healthy())
        total_load = sum(n.load for n in self.nodes.values())
        avg_load = total_load / len(self.nodes) if self.nodes else 0

        return {
            "total_nodes": len(self.nodes),
            "healthy_nodes": healthy,
            "unhealthy_nodes": len(self.nodes) - healthy,
            "average_load": avg_load,
            "total_requests": self.request_count,
            "nodes": [
                {
                    "id": node.node_id,
                    "url": node.get_url(),
                    "status": node.status,
                    "load": node.load,
                    "healthy": node.is_healthy()
                }
                for node in self.nodes.values()
            ]
        }


class LAMCluster:
    """
    High-level cluster management
    """

    def __init__(self):
        self.coordinator = DistributedLAM()

    def start_cluster(self, num_nodes: int = 3):
        """Start a cluster with multiple LAM instances"""
        print(f"Starting LAM cluster with {num_nodes} nodes...")

        for i in range(num_nodes):
            port = 8000 + i
            node_id = self.coordinator.register_node("localhost", port)
            print(f"  Node {i+1}: {node_id} on port {port}")

        print(f"\n✓ Cluster started with {num_nodes} nodes")

    def scale_up(self, additional_nodes: int = 1):
        """Add nodes to the cluster"""
        current_count = len(self.coordinator.nodes)

        for i in range(additional_nodes):
            port = 8000 + current_count + i
            self.coordinator.register_node("localhost", port)

        print(f"✓ Scaled up by {additional_nodes} nodes")

    def scale_down(self, nodes_to_remove: int = 1):
        """Remove nodes from the cluster"""
        nodes = list(self.coordinator.nodes.keys())

        for i in range(min(nodes_to_remove, len(nodes))):
            self.coordinator.remove_node(nodes[i])

        print(f"✓ Scaled down by {nodes_to_remove} nodes")

    async def process_request(self, action_type: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Process a request through the cluster"""
        return await self.coordinator.distribute_request(action_type, params)

    def get_status(self) -> Dict[str, Any]:
        """Get cluster status"""
        return self.coordinator.get_cluster_status()


def main():
    """Test distributed LAM"""
    print("=== Distributed LAM Coordinator ===\n")

    cluster = LAMCluster()

    # Start cluster
    cluster.start_cluster(num_nodes=3)

    # Show status
    print("\n=== Cluster Status ===")
    status = cluster.get_status()
    print(json.dumps(status, indent=2))

    # Simulate requests
    print("\n=== Simulating Requests ===")
    loop = asyncio.get_event_loop()

    for i in range(5):
        result = loop.run_until_complete(
            cluster.process_request("trip_planning", {"destination": "Paris"})
        )
        print(f"Request {i+1}: Node {result.get('node_id')}")

    # Final status
    print("\n=== Final Cluster Status ===")
    status = cluster.get_status()
    print(json.dumps(status, indent=2))


if __name__ == "__main__":
    main()
