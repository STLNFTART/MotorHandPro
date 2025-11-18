"""
MotorHandPro Notification Aggregation System
Real-time notification collection and interaction from all service containers
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import asyncio
import json
from datetime import datetime
from typing import Dict, List, Optional, Callable, Any
from dataclasses import dataclass, asdict
from enum import Enum
import subprocess
import re


class NotificationLevel(Enum):
    """Notification severity levels"""
    DEBUG = "debug"
    INFO = "info"
    WARNING = "warning"
    ERROR = "error"
    CRITICAL = "critical"


class NotificationSource(Enum):
    """Notification source types"""
    DOCKER = "docker"
    MQTT = "mqtt"
    DATABASE = "database"
    API = "api"
    SYSTEM = "system"
    INTEGRATION = "integration"


@dataclass
class Notification:
    """Notification data structure"""
    id: str
    timestamp: datetime
    source: NotificationSource
    level: NotificationLevel
    service: str
    message: str
    details: Optional[Dict[str, Any]] = None
    acknowledged: bool = False
    action_required: bool = False

    def to_dict(self) -> dict:
        data = asdict(self)
        data['timestamp'] = self.timestamp.isoformat()
        data['source'] = self.source.value
        data['level'] = self.level.value
        return data


class NotificationAggregator:
    """Aggregate notifications from all services"""

    def __init__(self):
        self.notifications: List[Notification] = []
        self.handlers: Dict[NotificationLevel, List[Callable]] = {
            level: [] for level in NotificationLevel
        }
        self.max_notifications = 1000
        self.notification_id_counter = 0

    def subscribe(self, level: NotificationLevel, handler: Callable):
        """Subscribe to notifications of a specific level"""
        self.handlers[level].append(handler)

    async def add_notification(
        self,
        source: NotificationSource,
        level: NotificationLevel,
        service: str,
        message: str,
        details: Optional[Dict[str, Any]] = None,
        action_required: bool = False
    ) -> Notification:
        """Add a new notification"""
        self.notification_id_counter += 1
        notification = Notification(
            id=f"notif_{self.notification_id_counter:06d}",
            timestamp=datetime.now(),
            source=source,
            level=level,
            service=service,
            message=message,
            details=details or {},
            action_required=action_required
        )

        self.notifications.insert(0, notification)  # Add to front (newest first)

        # Trim old notifications
        if len(self.notifications) > self.max_notifications:
            self.notifications = self.notifications[:self.max_notifications]

        # Trigger handlers
        for handler in self.handlers[level]:
            try:
                if asyncio.iscoroutinefunction(handler):
                    await handler(notification)
                else:
                    handler(notification)
            except Exception as e:
                print(f"Error in notification handler: {e}")

        return notification

    def get_notifications(
        self,
        level: Optional[NotificationLevel] = None,
        source: Optional[NotificationSource] = None,
        unacknowledged_only: bool = False,
        limit: int = 50
    ) -> List[Notification]:
        """Get filtered notifications"""
        filtered = self.notifications

        if level:
            filtered = [n for n in filtered if n.level == level]
        if source:
            filtered = [n for n in filtered if n.source == source]
        if unacknowledged_only:
            filtered = [n for n in filtered if not n.acknowledged]

        return filtered[:limit]

    def acknowledge(self, notification_id: str):
        """Acknowledge a notification"""
        for notif in self.notifications:
            if notif.id == notification_id:
                notif.acknowledged = True
                return True
        return False

    def acknowledge_all(self, level: Optional[NotificationLevel] = None):
        """Acknowledge all notifications of a specific level"""
        for notif in self.notifications:
            if level is None or notif.level == level:
                notif.acknowledged = True

    def clear_acknowledged(self):
        """Clear all acknowledged notifications"""
        self.notifications = [n for n in self.notifications if not n.acknowledged]


class DockerLogMonitor:
    """Monitor Docker container logs for notifications"""

    def __init__(self, aggregator: NotificationAggregator):
        self.aggregator = aggregator
        self.monitoring = False
        self.containers = [
            'motorhand-timescaledb',
            'motorhand-mqtt',
            'motorhand-fastapi',
            'motorhand-nodejs-api',
            'motorhand-websocket',
            'motorhand-nodered',
            'motorhand-prometheus',
            'motorhand-grafana',
            'motorhand-redis',
            'motorhand-pgadmin'
        ]

    async def start_monitoring(self):
        """Start monitoring all containers"""
        self.monitoring = True
        tasks = [self.monitor_container(container) for container in self.containers]
        await asyncio.gather(*tasks)

    def stop_monitoring(self):
        """Stop monitoring"""
        self.monitoring = False

    async def monitor_container(self, container: str):
        """Monitor a single container's logs"""
        try:
            # Follow logs in real-time
            process = await asyncio.create_subprocess_exec(
                'docker', 'logs', '-f', '--tail', '10', container,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )

            while self.monitoring:
                line = await process.stdout.readline()
                if not line:
                    break

                log_line = line.decode('utf-8').strip()
                if log_line:
                    await self.parse_log_line(container, log_line)

        except Exception as e:
            await self.aggregator.add_notification(
                source=NotificationSource.DOCKER,
                level=NotificationLevel.ERROR,
                service=container,
                message=f"Failed to monitor container: {str(e)}"
            )

    async def parse_log_line(self, container: str, log_line: str):
        """Parse log line and create notification if needed"""
        # Detect error patterns
        if any(pattern in log_line.lower() for pattern in ['error', 'exception', 'failed']):
            level = NotificationLevel.ERROR
            action_required = True
        elif any(pattern in log_line.lower() for pattern in ['warning', 'warn']):
            level = NotificationLevel.WARNING
            action_required = False
        elif any(pattern in log_line.lower() for pattern in ['critical', 'fatal']):
            level = NotificationLevel.CRITICAL
            action_required = True
        else:
            # Only log INFO level for specific keywords
            if not any(pattern in log_line.lower() for pattern in ['started', 'connected', 'listening']):
                return
            level = NotificationLevel.INFO
            action_required = False

        await self.aggregator.add_notification(
            source=NotificationSource.DOCKER,
            level=level,
            service=container,
            message=log_line,
            action_required=action_required
        )


class NotificationDisplay:
    """Interactive notification display and management"""

    def __init__(self, aggregator: NotificationAggregator):
        self.aggregator = aggregator

    def display_notifications(
        self,
        level: Optional[NotificationLevel] = None,
        source: Optional[NotificationSource] = None,
        unacknowledged_only: bool = False
    ):
        """Display notifications in a formatted table"""
        notifications = self.aggregator.get_notifications(
            level=level,
            source=source,
            unacknowledged_only=unacknowledged_only
        )

        if not notifications:
            print("\n📭 No notifications")
            return

        print("\n" + "="*100)
        print("🔔 NOTIFICATIONS")
        print("="*100)

        # Group by level
        grouped = {}
        for notif in notifications:
            if notif.level not in grouped:
                grouped[notif.level] = []
            grouped[notif.level].append(notif)

        # Display by level (critical first)
        level_order = [
            NotificationLevel.CRITICAL,
            NotificationLevel.ERROR,
            NotificationLevel.WARNING,
            NotificationLevel.INFO,
            NotificationLevel.DEBUG
        ]

        for level in level_order:
            if level not in grouped:
                continue

            # Level icon
            icons = {
                NotificationLevel.CRITICAL: "🚨",
                NotificationLevel.ERROR: "❌",
                NotificationLevel.WARNING: "⚠️ ",
                NotificationLevel.INFO: "ℹ️ ",
                NotificationLevel.DEBUG: "🐛"
            }

            print(f"\n{icons[level]} {level.value.upper()} ({len(grouped[level])})")
            print("-" * 100)

            for notif in grouped[level][:10]:  # Show max 10 per level
                ack_icon = "✓" if notif.acknowledged else "○"
                action_icon = "⚡" if notif.action_required else " "

                timestamp = notif.timestamp.strftime("%H:%M:%S")
                service = notif.service[:20].ljust(20)
                message = notif.message[:50]

                print(f"[{ack_icon}] {action_icon} {timestamp} | {service} | {message}")

        print("\n" + "="*100)

        # Show totals
        total = len(notifications)
        unack = len([n for n in notifications if not n.acknowledged])
        action = len([n for n in notifications if n.action_required and not n.acknowledged])

        print(f"\nTotal: {total} | Unacknowledged: {unack} | Action Required: {action}")

    async def interactive_menu(self):
        """Interactive notification management menu"""
        while True:
            print("\n" + "="*80)
            print("🔔 NOTIFICATION CENTER")
            print("="*80)

            # Show unacknowledged notifications
            unack = self.aggregator.get_notifications(unacknowledged_only=True, limit=5)
            if unack:
                print(f"\n⚠️  {len(unack)} unacknowledged notifications")
                for notif in unack[:3]:
                    icon = "🚨" if notif.level == NotificationLevel.CRITICAL else "⚠️ "
                    print(f"  {icon} [{notif.service}] {notif.message[:50]}")
                if len(unack) > 3:
                    print(f"  ... and {len(unack) - 3} more")

            print("\n1. View all notifications")
            print("2. View by level (Critical/Error/Warning/Info)")
            print("3. View by source (Docker/MQTT/Database/API)")
            print("4. View notifications requiring action")
            print("5. Acknowledge notification")
            print("6. Acknowledge all")
            print("7. Clear acknowledged notifications")
            print("8. Start real-time monitoring")
            print("9. Export notifications to file")
            print("10. Back to main menu")

            choice = input("\nSelect option (1-10): ").strip()

            if choice == '1':
                self.display_notifications()
            elif choice == '2':
                await self.view_by_level()
            elif choice == '3':
                await self.view_by_source()
            elif choice == '4':
                action_notifs = [n for n in self.aggregator.get_notifications() if n.action_required and not n.acknowledged]
                print(f"\n⚡ {len(action_notifs)} notifications requiring action")
                self.display_notifications()
            elif choice == '5':
                await self.acknowledge_notification()
            elif choice == '6':
                self.aggregator.acknowledge_all()
                print("✅ All notifications acknowledged")
            elif choice == '7':
                self.aggregator.clear_acknowledged()
                print("🗑️  Acknowledged notifications cleared")
            elif choice == '8':
                await self.start_monitoring()
            elif choice == '9':
                self.export_notifications()
            elif choice == '10':
                break

    async def view_by_level(self):
        """View notifications filtered by level"""
        print("\n1. Critical")
        print("2. Error")
        print("3. Warning")
        print("4. Info")
        print("5. Debug")

        choice = input("\nSelect level (1-5): ").strip()
        level_map = {
            '1': NotificationLevel.CRITICAL,
            '2': NotificationLevel.ERROR,
            '3': NotificationLevel.WARNING,
            '4': NotificationLevel.INFO,
            '5': NotificationLevel.DEBUG
        }

        if choice in level_map:
            self.display_notifications(level=level_map[choice])

    async def view_by_source(self):
        """View notifications filtered by source"""
        print("\n1. Docker")
        print("2. MQTT")
        print("3. Database")
        print("4. API")
        print("5. System")
        print("6. Integration")

        choice = input("\nSelect source (1-6): ").strip()
        source_map = {
            '1': NotificationSource.DOCKER,
            '2': NotificationSource.MQTT,
            '3': NotificationSource.DATABASE,
            '4': NotificationSource.API,
            '5': NotificationSource.SYSTEM,
            '6': NotificationSource.INTEGRATION
        }

        if choice in source_map:
            self.display_notifications(source=source_map[choice])

    async def acknowledge_notification(self):
        """Acknowledge a specific notification"""
        notifs = self.aggregator.get_notifications(unacknowledged_only=True, limit=20)

        if not notifs:
            print("No unacknowledged notifications")
            return

        print("\nUnacknowledged Notifications:")
        for i, notif in enumerate(notifs, 1):
            print(f"{i}. [{notif.level.value}] {notif.service}: {notif.message[:50]}")

        choice = input("\nSelect notification number to acknowledge (or 'all'): ").strip()

        if choice.lower() == 'all':
            self.aggregator.acknowledge_all()
            print("✅ All notifications acknowledged")
        elif choice.isdigit():
            idx = int(choice) - 1
            if 0 <= idx < len(notifs):
                self.aggregator.acknowledge(notifs[idx].id)
                print("✅ Notification acknowledged")

    async def start_monitoring(self):
        """Start real-time monitoring"""
        print("\n📡 Starting real-time monitoring...")
        print("Press Ctrl+C to stop")

        monitor = DockerLogMonitor(self.aggregator)

        try:
            # Start monitoring in background
            monitoring_task = asyncio.create_task(monitor.start_monitoring())

            # Display updates
            while True:
                await asyncio.sleep(5)
                unack = self.aggregator.get_notifications(unacknowledged_only=True, limit=10)
                if unack:
                    print(f"\n🔔 {len(unack)} new notifications")
                    for notif in unack[:3]:
                        print(f"  [{notif.level.value}] {notif.service}: {notif.message[:60]}")

        except KeyboardInterrupt:
            monitor.stop_monitoring()
            print("\n✅ Monitoring stopped")

    def export_notifications(self):
        """Export notifications to JSON file"""
        filename = f"notifications_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"

        notifications_data = [n.to_dict() for n in self.aggregator.notifications]

        with open(filename, 'w') as f:
            json.dump(notifications_data, f, indent=2)

        print(f"✅ Notifications exported to: {filename}")


# Global notification aggregator instance
_aggregator = NotificationAggregator()

def get_aggregator() -> NotificationAggregator:
    """Get the global notification aggregator"""
    return _aggregator
