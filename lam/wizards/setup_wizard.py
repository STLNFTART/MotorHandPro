#!/usr/bin/env python3
"""
LAM Setup Wizard - Credential and Service Configuration
Helps users set up services with secure credential management
"""
import json
import os
import getpass
from pathlib import Path
from typing import Dict, Any, List, Optional
from cryptography.fernet import Fernet
import base64


class SetupWizard:
    """
    Interactive setup wizard for service configuration and credential management
    """
    def __init__(self, config_dir: Optional[Path] = None):
        if config_dir is None:
            config_dir = Path.home() / ".lam" / "config"

        self.config_dir = Path(config_dir)
        self.config_dir.mkdir(parents=True, exist_ok=True)

        self.credentials_file = self.config_dir / "credentials.enc"
        self.services_file = self.config_dir / "services.json"
        self.key_file = self.config_dir / ".key"

        # Initialize encryption
        self._init_encryption()

        # Service templates
        self.service_templates = {
            "api_server": {
                "name": "API Server",
                "fields": ["host", "port", "api_key"],
                "defaults": {"host": "localhost", "port": "8000"}
            },
            "email": {
                "name": "Email Service",
                "fields": ["smtp_host", "smtp_port", "username", "password"],
                "defaults": {"smtp_port": "587"}
            },
            "calendar": {
                "name": "Calendar Service",
                "fields": ["provider", "api_key", "calendar_id"],
                "defaults": {"provider": "google"}
            },
            "food_delivery": {
                "name": "Food Delivery (Pizza, etc.)",
                "fields": ["service", "api_key", "default_address"],
                "defaults": {"service": "doordash"}
            },
            "travel": {
                "name": "Travel Booking",
                "fields": ["provider", "api_key", "traveler_id"],
                "defaults": {"provider": "expedia"}
            },
            "subscription": {
                "name": "Subscription Management",
                "fields": ["service_name", "account_id", "api_key"],
                "defaults": {}
            }
        }

    def _init_encryption(self):
        """Initialize or load encryption key"""
        if self.key_file.exists():
            with open(self.key_file, 'rb') as f:
                self.key = f.read()
        else:
            # Generate new key
            self.key = Fernet.generate_key()
            with open(self.key_file, 'wb') as f:
                f.write(self.key)
            # Secure the key file
            os.chmod(self.key_file, 0o600)

        self.cipher = Fernet(self.key)

    def _encrypt_credentials(self, credentials: Dict[str, Any]) -> bytes:
        """Encrypt credentials"""
        json_data = json.dumps(credentials).encode()
        return self.cipher.encrypt(json_data)

    def _decrypt_credentials(self, encrypted: bytes) -> Dict[str, Any]:
        """Decrypt credentials"""
        json_data = self.cipher.decrypt(encrypted)
        return json.loads(json_data.decode())

    def save_credentials(self, service_name: str, credentials: Dict[str, str]):
        """Save encrypted credentials for a service"""
        # Load existing credentials
        all_creds = {}
        if self.credentials_file.exists():
            with open(self.credentials_file, 'rb') as f:
                encrypted = f.read()
                if encrypted:
                    all_creds = self._decrypt_credentials(encrypted)

        # Update with new service
        all_creds[service_name] = credentials

        # Save encrypted
        encrypted = self._encrypt_credentials(all_creds)
        with open(self.credentials_file, 'wb') as f:
            f.write(encrypted)

        os.chmod(self.credentials_file, 0o600)

    def load_credentials(self, service_name: Optional[str] = None) -> Dict[str, Any]:
        """Load credentials (all or for specific service)"""
        if not self.credentials_file.exists():
            return {}

        with open(self.credentials_file, 'rb') as f:
            encrypted = f.read()
            if not encrypted:
                return {}

            all_creds = self._decrypt_credentials(encrypted)

            if service_name:
                return all_creds.get(service_name, {})
            return all_creds

    def configure_service(self, service_type: str) -> Dict[str, str]:
        """Interactive service configuration"""
        if service_type not in self.service_templates:
            print(f"Unknown service type: {service_type}")
            print(f"Available: {', '.join(self.service_templates.keys())}")
            return {}

        template = self.service_templates[service_type]

        print(f"\n=== {template['name']} Setup ===\n")

        credentials = {}

        for field in template['fields']:
            default = template['defaults'].get(field, "")
            default_text = f" [{default}]" if default else ""

            # Sensitive fields
            if field in ['password', 'api_key', 'secret']:
                value = getpass.getpass(f"{field}{default_text}: ")
            else:
                value = input(f"{field}{default_text}: ")

            # Use default if empty
            if not value and default:
                value = default

            credentials[field] = value

        # Confirm
        print("\nConfiguration:")
        for field, value in credentials.items():
            if field in ['password', 'api_key', 'secret']:
                print(f"  {field}: {'*' * len(value)}")
            else:
                print(f"  {field}: {value}")

        confirm = input("\nSave this configuration? [y/N]: ")
        if confirm.lower() == 'y':
            self.save_credentials(service_type, credentials)
            print(f"✓ {template['name']} configured successfully")
            return credentials
        else:
            print("Configuration cancelled")
            return {}

    def list_configured_services(self) -> List[str]:
        """List all configured services"""
        all_creds = self.load_credentials()
        return list(all_creds.keys())

    def interactive_wizard(self):
        """Run interactive setup wizard"""
        print("\n" + "="*60)
        print("LAM SETUP WIZARD")
        print("Configure services for your Large Action Model")
        print("="*60)

        while True:
            print("\nAvailable Services:")
            for i, (service_type, template) in enumerate(self.service_templates.items(), 1):
                configured = "✓" if service_type in self.list_configured_services() else " "
                print(f"  [{configured}] {i}. {template['name']} ({service_type})")

            print("\nOptions:")
            print("  <number> - Configure service")
            print("  list     - Show configured services")
            print("  test     - Test configuration")
            print("  quit     - Exit wizard")

            choice = input("\nWizard> ").strip().lower()

            if choice in ['quit', 'q', 'exit']:
                break

            elif choice == 'list':
                configured = self.list_configured_services()
                if configured:
                    print(f"\nConfigured services: {', '.join(configured)}")
                else:
                    print("\nNo services configured yet")

            elif choice == 'test':
                self.test_configuration()

            elif choice.isdigit():
                idx = int(choice) - 1
                services = list(self.service_templates.keys())
                if 0 <= idx < len(services):
                    self.configure_service(services[idx])
                else:
                    print("Invalid selection")

            else:
                print("Unknown command")

        print("\nSetup wizard complete!")

    def test_configuration(self):
        """Test configured services"""
        configured = self.list_configured_services()

        if not configured:
            print("\nNo services to test")
            return

        print("\n=== Testing Configured Services ===")

        for service_name in configured:
            creds = self.load_credentials(service_name)
            print(f"\n{service_name}:")

            # Basic validation
            template = self.service_templates.get(service_name, {})
            required_fields = template.get('fields', [])

            missing = [f for f in required_fields if f not in creds or not creds[f]]

            if missing:
                print(f"  ✗ Missing fields: {', '.join(missing)}")
            else:
                print(f"  ✓ All fields configured")

                # Service-specific tests
                if service_name == 'api_server':
                    host = creds.get('host', 'localhost')
                    port = creds.get('port', '8000')
                    print(f"  → API endpoint: http://{host}:{port}")

                elif service_name == 'email':
                    print(f"  → SMTP: {creds['smtp_host']}:{creds['smtp_port']}")
                    print(f"  → User: {creds['username']}")

    def export_config(self, output_file: Path):
        """Export configuration (excluding credentials) for sharing"""
        configured = self.list_configured_services()

        config = {
            "version": "1.0",
            "services": configured,
            "templates": self.service_templates
        }

        with open(output_file, 'w') as f:
            json.dump(config, f, indent=2)

        print(f"Configuration exported to {output_file}")


def main():
    """Main entry point for setup wizard"""
    import sys

    wizard = SetupWizard()

    if len(sys.argv) > 1:
        command = sys.argv[1]

        if command == 'configure' and len(sys.argv) > 2:
            service_type = sys.argv[2]
            wizard.configure_service(service_type)

        elif command == 'list':
            configured = wizard.list_configured_services()
            print(f"Configured: {', '.join(configured) if configured else 'none'}")

        elif command == 'test':
            wizard.test_configuration()

        elif command == 'export' and len(sys.argv) > 2:
            wizard.export_config(Path(sys.argv[2]))

        else:
            print("Usage: python setup_wizard.py [configure <service>|list|test|export <file>]")
            print("   or: python setup_wizard.py  (interactive mode)")
    else:
        wizard.interactive_wizard()


if __name__ == "__main__":
    main()
