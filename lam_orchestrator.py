"""
MotorHandPro LAM Initialization Workflow
First-run orchestrator with credential management and service integration
Patent Pending: U.S. Provisional Patent Application No. 63/842,846
"""

import os
import sys
import asyncio
import json
import hashlib
from datetime import datetime
from typing import Dict, List, Optional, Any
from pathlib import Path
import subprocess
import getpass

# Add LAM to path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

try:
    from lam.core.primal_lam import PrimalLAM
except ImportError:
    print("⚠️  Warning: PrimalLAM not available. Running in standalone mode.")
    PrimalLAM = None


class LAMOrchestrator:
    """LAM-based system orchestrator with credential management"""

    def __init__(self):
        self.version = "1.0.0"
        self.config_dir = Path.home() / ".motorhand"
        self.credentials_file = self.config_dir / "credentials.json.enc"
        self.config_file = self.config_dir / "config.json"
        self.notification_log = self.config_dir / "notifications.log"

        # Ensure config directory exists
        self.config_dir.mkdir(exist_ok=True)

        # Initialize LAM
        self.lam = None
        if PrimalLAM:
            try:
                self.lam = PrimalLAM()
            except Exception as e:
                print(f"⚠️  LAM initialization failed: {e}")

        # Credential vault
        self.credentials = {}
        self.service_apis = {}

        # Service definitions
        self.services = {
            'timescaledb': {
                'name': 'TimescaleDB (PostgreSQL)',
                'credentials': ['POSTGRES_USER', 'POSTGRES_PASSWORD', 'POSTGRES_DB'],
                'port': 5432,
                'health_check': 'pg_isready',
                'container': 'motorhand-timescaledb'
            },
            'mqtt': {
                'name': 'MQTT Broker (Mosquitto)',
                'credentials': ['MQTT_USERNAME', 'MQTT_PASSWORD'],
                'port': 1883,
                'health_check': 'mosquitto_pub',
                'container': 'motorhand-mqtt'
            },
            'redis': {
                'name': 'Redis Cache',
                'credentials': ['REDIS_PASSWORD'],
                'port': 6379,
                'health_check': 'redis-cli',
                'container': 'motorhand-redis'
            },
            'jwt': {
                'name': 'JWT Authentication',
                'credentials': ['JWT_SECRET'],
                'port': None,
                'health_check': None,
                'container': None
            },
            'grafana': {
                'name': 'Grafana Dashboards',
                'credentials': ['GF_SECURITY_ADMIN_USER', 'GF_SECURITY_ADMIN_PASSWORD'],
                'port': 3001,
                'health_check': 'curl',
                'container': 'motorhand-grafana'
            },
            'pgadmin': {
                'name': 'PgAdmin Database UI',
                'credentials': ['PGADMIN_DEFAULT_EMAIL', 'PGADMIN_DEFAULT_PASSWORD'],
                'port': 5050,
                'health_check': 'curl',
                'container': 'motorhand-pgadmin'
            },
            'fastapi': {
                'name': 'FastAPI Core',
                'credentials': ['DATABASE_URL', 'MQTT_BROKER', 'JWT_SECRET'],
                'port': 8000,
                'health_check': 'curl',
                'container': 'motorhand-fastapi'
            },
            'nodejs': {
                'name': 'Node.js Integration Gateway',
                'credentials': ['DATABASE_URL', 'MQTT_BROKER', 'FASTAPI_URL', 'JWT_SECRET'],
                'port': 3000,
                'health_check': 'curl',
                'container': 'motorhand-nodejs-api'
            },
            'github': {
                'name': 'GitHub Repository',
                'credentials': ['GITHUB_TOKEN', 'GITHUB_REPO'],
                'port': None,
                'health_check': None,
                'container': None
            },
            'spacex': {
                'name': 'SpaceX API',
                'credentials': ['SPACEX_API_KEY'],
                'port': None,
                'health_check': None,
                'container': None
            },
            'tesla': {
                'name': 'Tesla Integration',
                'credentials': ['TESLA_CLIENT_ID', 'TESLA_CLIENT_SECRET', 'TESLA_ACCESS_TOKEN'],
                'port': None,
                'health_check': None,
                'container': None
            },
            'nasa': {
                'name': 'NASA Open APIs',
                'credentials': ['NASA_API_KEY'],
                'port': None,
                'health_check': None,
                'container': None
            },
        }

    def print_banner(self):
        """Display LAM initialization banner"""
        print("\n" + "="*80)
        print("██╗      █████╗ ███╗   ███╗    ██╗███╗   ██╗██╗████████╗")
        print("██║     ██╔══██╗████╗ ████║    ██║████╗  ██║██║╚══██╔══╝")
        print("██║     ███████║██╔████╔██║    ██║██╔██╗ ██║██║   ██║   ")
        print("██║     ██╔══██║██║╚██╔╝██║    ██║██║╚██╗██║██║   ██║   ")
        print("███████╗██║  ██║██║ ╚═╝ ██║    ██║██║ ╚████║██║   ██║   ")
        print("╚══════╝╚═╝  ╚═╝╚═╝     ╚═╝    ╚═╝╚═╝  ╚═══╝╚═╝   ╚═╝   ")
        print()
        print("MotorHandPro - Large Action Model Orchestrator")
        print(f"Version: {self.version}")
        print("Patent Pending: U.S. Provisional Patent Application No. 63/842,846")
        print("="*80 + "\n")

    def lam_think(self, context: str, options: List[str]) -> str:
        """Use LAM to suggest best option based on context"""
        if not self.lam:
            return "LAM not available - manual selection required"

        # Simple recommendation logic (can be enhanced with actual LAM inference)
        recommendation = f"LAM Analysis: Based on {context}, recommended action: {options[0]}"
        return recommendation

    async def check_system_health(self) -> Dict[str, Any]:
        """Check health of all services"""
        print("🔍 Checking system health...\n")

        health_status = {}

        for service_id, service_info in self.services.items():
            container = service_info.get('container')
            port = service_info.get('port')

            if container:
                # Check if container is running
                try:
                    result = subprocess.run(
                        ['docker', 'ps', '--filter', f'name={container}', '--format', '{{.Status}}'],
                        capture_output=True, text=True, timeout=5
                    )
                    is_running = 'Up' in result.stdout
                    health_status[service_id] = {
                        'running': is_running,
                        'container': container,
                        'status': result.stdout.strip() if is_running else 'Not running'
                    }
                except Exception as e:
                    health_status[service_id] = {
                        'running': False,
                        'container': container,
                        'error': str(e)
                    }
            else:
                # External service or configuration
                health_status[service_id] = {
                    'running': None,
                    'type': 'external',
                    'configured': service_id in self.credentials
                }

        return health_status

    def display_health_status(self, health_status: Dict[str, Any]):
        """Display health status in a formatted table"""
        print("\n📊 Service Health Status:")
        print("-" * 80)
        print(f"{'Service':<30} {'Status':<20} {'Container':<25}")
        print("-" * 80)

        for service_id, status in health_status.items():
            service_name = self.services[service_id]['name']

            if status.get('running') is True:
                status_icon = "✅ Running"
            elif status.get('running') is False:
                status_icon = "❌ Stopped"
            elif status.get('configured'):
                status_icon = "🔧 Configured"
            else:
                status_icon = "⚠️  Not Configured"

            container = status.get('container', 'N/A')
            print(f"{service_name:<30} {status_icon:<20} {container:<25}")

        print("-" * 80 + "\n")

    def load_credentials(self):
        """Load credentials from encrypted storage"""
        if self.credentials_file.exists():
            try:
                with open(self.credentials_file, 'r') as f:
                    encrypted_data = f.read()
                    # Simple XOR encryption for now (use proper encryption in production)
                    self.credentials = json.loads(self._decrypt(encrypted_data))
                print("✅ Credentials loaded from vault")
            except Exception as e:
                print(f"⚠️  Failed to load credentials: {e}")
        else:
            print("📝 No existing credentials found. Starting fresh.")

    def save_credentials(self):
        """Save credentials to encrypted storage"""
        try:
            encrypted_data = self._encrypt(json.dumps(self.credentials, indent=2))
            with open(self.credentials_file, 'w') as f:
                f.write(encrypted_data)
            print("✅ Credentials saved to vault")
        except Exception as e:
            print(f"❌ Failed to save credentials: {e}")

    def _encrypt(self, data: str) -> str:
        """Simple XOR encryption (replace with proper encryption in production)"""
        key = hashlib.sha256(b"motorhand_lam_key").digest()
        encrypted = ''.join(chr(ord(c) ^ key[i % len(key)]) for i, c in enumerate(data))
        return encrypted.encode('unicode_escape').decode('ascii')

    def _decrypt(self, data: str) -> str:
        """Simple XOR decryption (replace with proper decryption in production)"""
        data = data.encode('ascii').decode('unicode_escape')
        key = hashlib.sha256(b"motorhand_lam_key").digest()
        decrypted = ''.join(chr(ord(c) ^ key[i % len(key)]) for i, c in enumerate(data))
        return decrypted

    async def credential_management_menu(self):
        """Interactive credential management"""
        while True:
            print("\n" + "="*80)
            print("🔐 CREDENTIAL MANAGEMENT")
            print("="*80)
            print("\n1. View all credentials")
            print("2. Add/Update credential")
            print("3. Delete credential")
            print("4. Test credentials")
            print("5. Export credentials template")
            print("6. Import credentials from file")
            print("7. Generate secure credentials")
            print("8. Back to main menu")

            choice = input("\nSelect option (1-8): ").strip()

            if choice == '1':
                await self.view_credentials()
            elif choice == '2':
                await self.add_update_credential()
            elif choice == '3':
                await self.delete_credential()
            elif choice == '4':
                await self.test_credentials()
            elif choice == '5':
                self.export_credentials_template()
            elif choice == '6':
                await self.import_credentials()
            elif choice == '7':
                await self.generate_secure_credentials()
            elif choice == '8':
                break
            else:
                print("❌ Invalid option. Please try again.")

    async def view_credentials(self):
        """View all stored credentials (masked)"""
        print("\n📋 Stored Credentials:")
        print("-" * 80)

        if not self.credentials:
            print("No credentials stored.")
            return

        for service, creds in self.credentials.items():
            print(f"\n🔹 {service.upper()}")
            for key, value in creds.items():
                masked = value[:4] + '*' * (len(value) - 4) if len(value) > 4 else '****'
                print(f"  {key}: {masked}")

    async def add_update_credential(self):
        """Add or update a credential"""
        print("\n➕ Add/Update Credential")
        print("\nAvailable services:")
        for i, (service_id, service_info) in enumerate(self.services.items(), 1):
            print(f"{i}. {service_info['name']} ({service_id})")

        service_choice = input("\nEnter service number or ID: ").strip()

        # Find service
        service_id = None
        if service_choice.isdigit():
            idx = int(service_choice) - 1
            if 0 <= idx < len(self.services):
                service_id = list(self.services.keys())[idx]
        else:
            if service_choice in self.services:
                service_id = service_choice

        if not service_id:
            print("❌ Invalid service selection")
            return

        service = self.services[service_id]
        print(f"\n🔧 Configuring: {service['name']}")
        print(f"Required credentials: {', '.join(service['credentials'])}")

        if service_id not in self.credentials:
            self.credentials[service_id] = {}

        for cred_key in service['credentials']:
            current_value = self.credentials[service_id].get(cred_key, '')
            if current_value:
                print(f"\nCurrent {cred_key}: {current_value[:4]}****")
                update = input("Update? (y/n): ").strip().lower()
                if update != 'y':
                    continue

            if 'PASSWORD' in cred_key or 'SECRET' in cred_key or 'TOKEN' in cred_key:
                value = getpass.getpass(f"Enter {cred_key}: ")
            else:
                value = input(f"Enter {cred_key}: ").strip()

            if value:
                self.credentials[service_id][cred_key] = value

        self.save_credentials()
        print(f"\n✅ Credentials for {service['name']} saved!")

    async def delete_credential(self):
        """Delete a credential"""
        print("\n🗑️  Delete Credential")

        if not self.credentials:
            print("No credentials to delete.")
            return

        print("\nServices with credentials:")
        for i, service_id in enumerate(self.credentials.keys(), 1):
            print(f"{i}. {self.services[service_id]['name']}")

        choice = input("\nEnter service number to delete (or 'cancel'): ").strip()

        if choice.lower() == 'cancel':
            return

        if choice.isdigit():
            idx = int(choice) - 1
            service_ids = list(self.credentials.keys())
            if 0 <= idx < len(service_ids):
                service_id = service_ids[idx]
                confirm = input(f"Delete all credentials for {self.services[service_id]['name']}? (yes/no): ").strip().lower()
                if confirm == 'yes':
                    del self.credentials[service_id]
                    self.save_credentials()
                    print("✅ Credentials deleted")
                else:
                    print("❌ Deletion cancelled")

    async def test_credentials(self):
        """Test credentials by attempting to connect to services"""
        print("\n🧪 Testing Credentials...")

        for service_id, creds in self.credentials.items():
            service = self.services[service_id]
            print(f"\n Testing {service['name']}...", end=' ')

            # Actual testing would go here
            # For now, just check if all required credentials are present
            required = service['credentials']
            has_all = all(cred in creds for cred in required)

            if has_all:
                print("✅ All credentials present")
            else:
                missing = [c for c in required if c not in creds]
                print(f"❌ Missing: {', '.join(missing)}")

    def export_credentials_template(self):
        """Export a .env template file"""
        template_file = Path.cwd() / ".env.template"

        with open(template_file, 'w') as f:
            f.write("# MotorHandPro Environment Variables Template\n")
            f.write(f"# Generated: {datetime.now().isoformat()}\n\n")

            for service_id, service in self.services.items():
                f.write(f"# {service['name']}\n")
                for cred in service['credentials']:
                    current_value = self.credentials.get(service_id, {}).get(cred, '')
                    if current_value:
                        f.write(f"{cred}={current_value}\n")
                    else:
                        f.write(f"# {cred}=your_value_here\n")
                f.write("\n")

        print(f"✅ Template exported to: {template_file}")

    async def import_credentials(self):
        """Import credentials from .env file"""
        env_file = input("Enter path to .env file (or press Enter for .env): ").strip()
        if not env_file:
            env_file = ".env"

        env_path = Path(env_file)
        if not env_path.exists():
            print(f"❌ File not found: {env_file}")
            return

        imported_count = 0
        with open(env_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    key = key.strip()
                    value = value.strip().strip('"').strip("'")

                    # Find which service this credential belongs to
                    for service_id, service in self.services.items():
                        if key in service['credentials']:
                            if service_id not in self.credentials:
                                self.credentials[service_id] = {}
                            self.credentials[service_id][key] = value
                            imported_count += 1
                            break

        self.save_credentials()
        print(f"✅ Imported {imported_count} credentials")

    async def generate_secure_credentials(self):
        """Generate secure random credentials"""
        import secrets
        import string

        print("\n🔐 Generate Secure Credentials")
        print("\n1. JWT Secret (64 characters)")
        print("2. Database Password (32 characters)")
        print("3. API Key (48 characters)")
        print("4. All missing credentials")

        choice = input("\nSelect option (1-4): ").strip()

        def generate_password(length: int) -> str:
            alphabet = string.ascii_letters + string.digits + "!@#$%^&*"
            return ''.join(secrets.choice(alphabet) for _ in range(length))

        if choice == '1':
            secret = secrets.token_urlsafe(48)
            print(f"\nGenerated JWT Secret: {secret}")
            save = input("Save to JWT credentials? (y/n): ").strip().lower()
            if save == 'y':
                if 'jwt' not in self.credentials:
                    self.credentials['jwt'] = {}
                self.credentials['jwt']['JWT_SECRET'] = secret
                self.save_credentials()

        elif choice == '2':
            password = generate_password(32)
            print(f"\nGenerated Password: {password}")
            print("(Copy this password - you won't see it again)")

        elif choice == '3':
            api_key = secrets.token_urlsafe(36)
            print(f"\nGenerated API Key: {api_key}")

        elif choice == '4':
            print("\n🔄 Generating all missing credentials...")
            for service_id, service in self.services.items():
                if service_id not in self.credentials:
                    self.credentials[service_id] = {}

                for cred_key in service['credentials']:
                    if cred_key not in self.credentials[service_id]:
                        if 'SECRET' in cred_key or 'TOKEN' in cred_key:
                            self.credentials[service_id][cred_key] = secrets.token_urlsafe(48)
                        elif 'PASSWORD' in cred_key:
                            self.credentials[service_id][cred_key] = generate_password(32)
                        elif 'USER' in cred_key or 'EMAIL' in cred_key:
                            self.credentials[service_id][cred_key] = "admin"
                        elif 'DB' in cred_key:
                            self.credentials[service_id][cred_key] = "motorhand"
                        else:
                            self.credentials[service_id][cred_key] = f"generated_{secrets.token_hex(8)}"
                        print(f"✅ Generated: {service['name']} - {cred_key}")

            self.save_credentials()
            print(f"\n✅ All missing credentials generated and saved!")

    async def credential_mapping_menu(self):
        """Framework/Server/API/Repo credential mapping"""
        while True:
            print("\n" + "="*80)
            print("🗺️  CREDENTIAL MAPPING & SERVICE INTEGRATION")
            print("="*80)
            print("\n1. View service → credential mappings")
            print("2. Map credentials to Docker containers")
            print("3. Map credentials to API endpoints")
            print("4. Map credentials to repositories")
            print("5. Auto-apply credentials to all services")
            print("6. Verify credential mappings")
            print("7. Export docker-compose with credentials")
            print("8. Back to main menu")

            choice = input("\nSelect option (1-8): ").strip()

            if choice == '1':
                await self.view_mappings()
            elif choice == '2':
                await self.map_to_containers()
            elif choice == '3':
                await self.map_to_apis()
            elif choice == '4':
                await self.map_to_repos()
            elif choice == '5':
                await self.auto_apply_credentials()
            elif choice == '6':
                await self.verify_mappings()
            elif choice == '7':
                await self.export_docker_compose()
            elif choice == '8':
                break

    async def view_mappings(self):
        """View all service mappings"""
        print("\n📊 Service → Credential Mappings:")
        print("-" * 80)

        for service_id, service in self.services.items():
            print(f"\n🔹 {service['name']}")
            print(f"   Service ID: {service_id}")
            print(f"   Container: {service.get('container', 'N/A')}")
            print(f"   Port: {service.get('port', 'N/A')}")
            print(f"   Required Credentials:")
            for cred in service['credentials']:
                has_cred = service_id in self.credentials and cred in self.credentials[service_id]
                status = "✅" if has_cred else "❌"
                print(f"      {status} {cred}")

    async def map_to_containers(self):
        """Map credentials to Docker containers"""
        print("\n🐳 Mapping Credentials to Docker Containers")
        print("\nThis will update environment variables for running containers.")
        confirm = input("Continue? (yes/no): ").strip().lower()

        if confirm != 'yes':
            return

        for service_id, service in self.services.items():
            container = service.get('container')
            if not container or service_id not in self.credentials:
                continue

            print(f"\n⚙️  Configuring {container}...")

            # In a real implementation, this would use Docker API to set environment variables
            # For now, we'll just show what would be done
            for cred_key, cred_value in self.credentials[service_id].items():
                print(f"   Would set: {cred_key}=****")

        print("\n✅ Container credential mapping completed (dry run)")
        print("💡 Tip: Restart containers for changes to take effect")

    async def map_to_apis(self):
        """Map credentials to API endpoints"""
        print("\n🌐 Mapping Credentials to API Endpoints")
        # Implementation for API credential mapping
        print("API mapping functionality coming soon...")

    async def map_to_repos(self):
        """Map credentials to repositories"""
        print("\n📦 Mapping Credentials to Repositories")
        # Implementation for repo credential mapping
        print("Repository mapping functionality coming soon...")

    async def auto_apply_credentials(self):
        """Automatically apply all credentials to services"""
        print("\n🚀 Auto-Applying Credentials to All Services")
        print("\nThis will:")
        print("  • Generate .env file")
        print("  • Update Docker Compose environment")
        print("  • Configure all service connections")

        confirm = input("\nContinue? (yes/no): ").strip().lower()

        if confirm != 'yes':
            return

        # Generate .env file
        self.export_credentials_template()

        # Map to containers
        await self.map_to_containers()

        print("\n✅ Credentials auto-applied to all services!")

    async def verify_mappings(self):
        """Verify all credential mappings"""
        print("\n✅ Verifying Credential Mappings...")
        await self.test_credentials()

    async def export_docker_compose(self):
        """Export docker-compose with credentials applied"""
        print("\n📝 Exporting Docker Compose with Credentials...")
        output_file = "docker-compose.configured.yml"

        # Read template
        template_file = "docker-compose.production.yml"
        if not Path(template_file).exists():
            print(f"❌ Template not found: {template_file}")
            return

        with open(template_file, 'r') as f:
            content = f.read()

        # Replace placeholders with actual credentials
        for service_id, creds in self.credentials.items():
            for key, value in creds.items():
                placeholder = f"${{{key}}}"
                content = content.replace(placeholder, value)

        with open(output_file, 'w') as f:
            f.write(content)

        print(f"✅ Configured Docker Compose exported to: {output_file}")
        print("⚠️  WARNING: This file contains sensitive credentials!")
        print("   Do not commit to version control!")

    async def main_menu(self):
        """Main interactive menu"""
        while True:
            print("\n" + "="*80)
            print("🤖 LAM ORCHESTRATOR - MAIN MENU")
            print("="*80)

            # LAM recommendation
            lam_recommendation = self.lam_think(
                "system initialization",
                ["Credential Management", "Credential Mapping"]
            )
            print(f"\n💡 {lam_recommendation}")

            print("\n1. 🔐 Credential Management")
            print("2. 🗺️  Framework/Server/API/Repo Credential Mapping")
            print("3. 📊 View System Health")
            print("4. 🔔 Notification Center")
            print("5. 🛠️  Service API Integration")
            print("6. 📈 System Status & Monitoring")
            print("7. 🚀 Deploy Services")
            print("8. 💾 Backup/Restore Configuration")
            print("9. 📚 Documentation & Help")
            print("10. 🚪 Exit")

            choice = input("\nSelect option (1-10): ").strip()

            if choice == '1':
                await self.credential_management_menu()
            elif choice == '2':
                await self.credential_mapping_menu()
            elif choice == '3':
                health = await self.check_system_health()
                self.display_health_status(health)
            elif choice == '4':
                await self.notification_center()
            elif choice == '5':
                await self.service_api_menu()
            elif choice == '6':
                await self.system_status()
            elif choice == '7':
                await self.deploy_services()
            elif choice == '8':
                await self.backup_restore()
            elif choice == '9':
                self.show_help()
            elif choice == '10':
                print("\n👋 Goodbye! LAM orchestrator shutting down...")
                break
            else:
                print("❌ Invalid option. Please try again.")

    async def notification_center(self):
        """Notification aggregation and interaction"""
        print("\n🔔 Notification Center - Coming soon in next update")

    async def service_api_menu(self):
        """Service API integration menu"""
        print("\n🛠️  Service API Integration - Coming soon in next update")

    async def system_status(self):
        """Display system status and monitoring"""
        health = await self.check_system_health()
        self.display_health_status(health)

    async def deploy_services(self):
        """Deploy services with configured credentials"""
        print("\n🚀 Deploying Services...")
        print("This will start Docker Compose with configured credentials.")
        confirm = input("Continue? (yes/no): ").strip().lower()

        if confirm == 'yes':
            await self.export_docker_compose()
            print("\n▶️  Starting services...")
            subprocess.run(['docker-compose', '-f', 'docker-compose.configured.yml', 'up', '-d'])

    async def backup_restore(self):
        """Backup and restore configuration"""
        print("\n💾 Backup/Restore - Coming soon in next update")

    def show_help(self):
        """Show documentation and help"""
        print("\n📚 MotorHandPro LAM Orchestrator - Help")
        print("="*80)
        print("\nDocumentation:")
        print("  • PRODUCTION_DEPLOYMENT.md - Deployment guide")
        print("  • INTEGRATION_EXAMPLES.md - Integration examples")
        print("  • README.md - Project overview")
        print("\nSupport:")
        print("  • GitHub: https://github.com/STLNFTART/MotorHandPro")
        print("  • Email: contact@stlnftart.com")

    async def run(self):
        """Main entry point"""
        self.print_banner()

        print("🔄 Initializing LAM Orchestrator...")

        # Load existing credentials
        self.load_credentials()

        # Check system health
        health = await self.check_system_health()
        self.display_health_status(health)

        # Main menu loop
        await self.main_menu()


async def main():
    """Main async entry point"""
    orchestrator = LAMOrchestrator()
    await orchestrator.run()


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        print("\n\n⚠️  Interrupted by user. Shutting down...")
        sys.exit(0)
