#!/usr/bin/env python3
"""
Credential Initialization Script
Generates secure credentials for MCP server, Node-RED, and other services
"""
import json
import secrets
import hashlib
import bcrypt
from pathlib import Path
from datetime import datetime


def generate_secure_password(length: int = 32) -> str:
    """Generate cryptographically secure password"""
    return secrets.token_urlsafe(length)


def hash_password(password: str) -> str:
    """Hash password using bcrypt"""
    salt = bcrypt.gensalt()
    hashed = bcrypt.hashpw(password.encode('utf-8'), salt)
    return hashed.decode('utf-8')


def generate_jwt_secret() -> str:
    """Generate JWT secret key"""
    return secrets.token_hex(32)


def generate_api_key() -> str:
    """Generate API key"""
    return f"lam_{secrets.token_hex(16)}"


def init_credentials():
    """Initialize all credentials"""
    print("\n" + "="*70)
    print("MOTORHANDPRO CREDENTIAL INITIALIZATION")
    print("="*70)
    print()

    credentials = {
        "generated_at": datetime.utcnow().isoformat(),
        "version": "1.0.0"
    }

    # 1. MCP Server Credentials
    print("🔐 Generating MCP Server credentials...")
    credentials["mcp"] = {
        "secret_key": generate_jwt_secret(),
        "client_id": f"mcp_client_{secrets.token_hex(8)}",
        "client_secret": secrets.token_hex(32),
        "api_key": generate_api_key()
    }
    print(f"   ✓ MCP secret key: {credentials['mcp']['secret_key'][:16]}...")
    print(f"   ✓ MCP client ID: {credentials['mcp']['client_id']}")
    print()

    # 2. Node-RED Credentials
    print("🔧 Generating Node-RED credentials...")
    nodered_password = generate_secure_password(24)
    credentials["nodered"] = {
        "username": "admin",
        "password": nodered_password,
        "password_hash": hash_password(nodered_password),
        "secret": secrets.token_hex(32)  # For flows encryption
    }
    print(f"   ✓ Username: {credentials['nodered']['username']}")
    print(f"   ✓ Password: {nodered_password}")
    print(f"   ✓ Flow encryption key: {credentials['nodered']['secret'][:16]}...")
    print()

    # 3. LAM API Credentials
    print("🧠 Generating LAM API credentials...")
    credentials["lam_api"] = {
        "api_key": generate_api_key(),
        "secret": generate_jwt_secret()
    }
    print(f"   ✓ LAM API key: {credentials['lam_api']['api_key']}")
    print()

    # 4. PostgreSQL/TimescaleDB Credentials
    print("🗄️  Generating Database credentials...")
    credentials["database"] = {
        "postgres_password": generate_secure_password(32),
        "jwt_secret": generate_jwt_secret()
    }
    print(f"   ✓ PostgreSQL password: {credentials['database']['postgres_password'][:16]}...")
    print(f"   ✓ JWT secret: {credentials['database']['jwt_secret'][:16]}...")
    print()

    # 5. Redis Credentials
    print("💾 Generating Redis credentials...")
    credentials["redis"] = {
        "password": generate_secure_password(32)
    }
    print(f"   ✓ Redis password: {credentials['redis']['password'][:16]}...")
    print()

    # 6. Grafana Credentials
    print("📊 Generating Grafana credentials...")
    grafana_password = generate_secure_password(24)
    credentials["grafana"] = {
        "admin_user": "admin",
        "admin_password": grafana_password
    }
    print(f"   ✓ Admin user: {credentials['grafana']['admin_user']}")
    print(f"   ✓ Admin password: {grafana_password}")
    print()

    # Save to secure location
    creds_dir = Path(__file__).parent / "infrastructure" / "credentials"
    creds_dir.mkdir(parents=True, exist_ok=True)

    creds_file = creds_dir / "credentials.json"
    with open(creds_file, 'w') as f:
        json.dump(credentials, f, indent=2)

    # Set restrictive permissions
    creds_file.chmod(0o600)  # Owner read/write only

    print("="*70)
    print(f"✓ Credentials saved to: {creds_file}")
    print(f"  File permissions: 600 (owner read/write only)")
    print()

    # Generate .env file for docker-compose
    env_file = Path(__file__).parent / ".env.production"
    with open(env_file, 'w') as f:
        f.write("# MotorHandPro Production Environment Variables\n")
        f.write(f"# Generated: {datetime.utcnow().isoformat()}\n\n")

        f.write("# PostgreSQL/TimescaleDB\n")
        f.write(f"POSTGRES_PASSWORD={credentials['database']['postgres_password']}\n\n")

        f.write("# JWT Secrets\n")
        f.write(f"JWT_SECRET={credentials['database']['jwt_secret']}\n\n")

        f.write("# Redis\n")
        f.write(f"REDIS_PASSWORD={credentials['redis']['password']}\n\n")

        f.write("# Grafana\n")
        f.write(f"GF_SECURITY_ADMIN_PASSWORD={credentials['grafana']['admin_password']}\n\n")

        f.write("# MCP Server\n")
        f.write(f"MCP_SECRET_KEY={credentials['mcp']['secret_key']}\n")
        f.write(f"MCP_API_KEY={credentials['mcp']['api_key']}\n\n")

        f.write("# LAM API\n")
        f.write(f"LAM_API_KEY={credentials['lam_api']['api_key']}\n\n")

        f.write("# Node-RED\n")
        f.write(f"NODERED_PASSWORD={credentials['nodered']['password']}\n")
        f.write(f"NODERED_SECRET={credentials['nodered']['secret']}\n")

    env_file.chmod(0o600)

    print(f"✓ Environment file created: {env_file}")
    print(f"  File permissions: 600 (owner read/write only)")
    print()

    # Create Node-RED settings template
    print("📝 Generating Node-RED settings.js...")
    nodered_settings = Path(__file__).parent / "node-red" / "settings.js"
    nodered_settings.parent.mkdir(parents=True, exist_ok=True)

    with open(nodered_settings, 'w') as f:
        f.write(f"""// Node-RED Settings (Auto-generated)
module.exports = {{
    flowFile: 'flows.json',
    flowFilePretty: true,
    credentialSecret: "{credentials['nodered']['secret']}",
    adminAuth: {{
        type: "credentials",
        users: [{{
            username: "{credentials['nodered']['username']}",
            password: "{credentials['nodered']['password_hash']}",
            permissions: "*"
        }}]
    }},
    httpNodeAuth: {{user:"{credentials['nodered']['username']}",pass:"{credentials['nodered']['password_hash']}"}},
    ui: {{ path: "ui" }},
    logging: {{
        console: {{
            level: "info",
            metrics: false,
            audit: false
        }}
    }},
    editorTheme: {{
        projects: {{
            enabled: true
        }}
    }}
}};
""")

    print(f"   ✓ Settings saved to: {nodered_settings}")
    print()

    # Print summary
    print("="*70)
    print("🎉 CREDENTIAL INITIALIZATION COMPLETE")
    print("="*70)
    print()
    print("Next steps:")
    print("  1. Review generated credentials in infrastructure/credentials/")
    print("  2. Update docker-compose with: docker-compose --env-file .env.production")
    print("  3. Start MCP server: python infrastructure/mcp/lam_mcp_server.py")
    print("  4. Access Node-RED at http://localhost:1880")
    print(f"     Username: {credentials['nodered']['username']}")
    print(f"     Password: {nodered_password}")
    print()
    print("⚠️  IMPORTANT: Keep credentials.json and .env.production secure!")
    print("   These files contain sensitive authentication data.")
    print()


if __name__ == "__main__":
    init_credentials()
