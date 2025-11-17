#!/usr/bin/env python3
"""
LAM Authentication System
JWT token-based authentication with OAuth2 support
"""
import sys
from pathlib import Path
from datetime import datetime, timedelta
from typing import Optional, Dict, Any
import secrets
import hashlib

sys.path.insert(0, str(Path(__file__).parent.parent))

try:
    from jose import JWTError, jwt
    from passlib.context import CryptContext
    from fastapi import Depends, HTTPException, status
    from fastapi.security import OAuth2PasswordBearer, OAuth2PasswordRequestForm
    from pydantic import BaseModel
    JWT_AVAILABLE = True
except ImportError:
    print("Warning: Install: pip install python-jose[cryptography] passlib[bcrypt] python-multipart")
    JWT_AVAILABLE = False


# Configuration
SECRET_KEY = secrets.token_urlsafe(32)  # Generate on first run, store securely in production
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30
REFRESH_TOKEN_EXPIRE_DAYS = 7

# Password hashing
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")

# OAuth2 scheme
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="token")


# Pydantic models
class Token(BaseModel):
    access_token: str
    token_type: str
    refresh_token: Optional[str] = None


class TokenData(BaseModel):
    username: Optional[str] = None
    scopes: list = []


class User(BaseModel):
    username: str
    email: Optional[str] = None
    full_name: Optional[str] = None
    disabled: Optional[bool] = False
    roles: list = []


class UserInDB(User):
    hashed_password: str


# Mock database (replace with PostgreSQL in production)
fake_users_db = {
    "admin": {
        "username": "admin",
        "full_name": "Admin User",
        "email": "admin@lam.local",
        "hashed_password": "",  # Will be set on first run
        "disabled": False,
        "roles": ["admin", "user"]
    },
    "user": {
        "username": "user",
        "full_name": "Regular User",
        "email": "user@lam.local",
        "hashed_password": "",
        "disabled": False,
        "roles": ["user"]
    }
}


class AuthManager:
    """Authentication and authorization manager"""

    def __init__(self, secret_key: str = SECRET_KEY):
        self.secret_key = secret_key
        self.algorithm = ALGORITHM

        # Initialize default users
        self._init_default_users()

    def _init_default_users(self):
        """Initialize default user passwords"""
        if not fake_users_db["admin"]["hashed_password"]:
            fake_users_db["admin"]["hashed_password"] = self.hash_password("admin123")
        if not fake_users_db["user"]["hashed_password"]:
            fake_users_db["user"]["hashed_password"] = self.hash_password("user123")

    def verify_password(self, plain_password: str, hashed_password: str) -> bool:
        """Verify a password against its hash"""
        return pwd_context.verify(plain_password, hashed_password)

    def hash_password(self, password: str) -> str:
        """Hash a password"""
        return pwd_context.hash(password)

    def get_user(self, username: str) -> Optional[UserInDB]:
        """Get user from database"""
        if username in fake_users_db:
            user_dict = fake_users_db[username]
            return UserInDB(**user_dict)
        return None

    def authenticate_user(self, username: str, password: str) -> Optional[UserInDB]:
        """Authenticate a user"""
        user = self.get_user(username)
        if not user:
            return None
        if not self.verify_password(password, user.hashed_password):
            return None
        return user

    def create_access_token(self, data: dict, expires_delta: Optional[timedelta] = None) -> str:
        """Create JWT access token"""
        to_encode = data.copy()

        if expires_delta:
            expire = datetime.utcnow() + expires_delta
        else:
            expire = datetime.utcnow() + timedelta(minutes=15)

        to_encode.update({"exp": expire, "type": "access"})

        encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)
        return encoded_jwt

    def create_refresh_token(self, data: dict) -> str:
        """Create JWT refresh token"""
        to_encode = data.copy()
        expire = datetime.utcnow() + timedelta(days=REFRESH_TOKEN_EXPIRE_DAYS)
        to_encode.update({"exp": expire, "type": "refresh"})

        encoded_jwt = jwt.encode(to_encode, self.secret_key, algorithm=self.algorithm)
        return encoded_jwt

    def decode_token(self, token: str) -> Optional[Dict[str, Any]]:
        """Decode and verify JWT token"""
        try:
            payload = jwt.decode(token, self.secret_key, algorithms=[self.algorithm])
            return payload
        except JWTError:
            return None

    def get_current_user(self, token: str) -> Optional[User]:
        """Get current user from token"""
        credentials_exception = HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Could not validate credentials",
            headers={"WWW-Authenticate": "Bearer"},
        )

        payload = self.decode_token(token)
        if payload is None:
            raise credentials_exception

        username: str = payload.get("sub")
        if username is None:
            raise credentials_exception

        user = self.get_user(username)
        if user is None:
            raise credentials_exception

        return User(**user.dict())

    def check_permission(self, user: User, required_role: str) -> bool:
        """Check if user has required role"""
        return required_role in user.roles

    def create_user(self, username: str, password: str, email: str,
                   full_name: str, roles: list = None) -> UserInDB:
        """Create a new user"""
        if username in fake_users_db:
            raise ValueError("User already exists")

        hashed_password = self.hash_password(password)

        user_data = {
            "username": username,
            "email": email,
            "full_name": full_name,
            "hashed_password": hashed_password,
            "disabled": False,
            "roles": roles or ["user"]
        }

        fake_users_db[username] = user_data
        return UserInDB(**user_data)


# Dependency injection for FastAPI
async def get_current_user_dependency(token: str = Depends(oauth2_scheme)) -> User:
    """FastAPI dependency to get current user"""
    auth = AuthManager()
    return auth.get_current_user(token)


async def get_current_active_user(current_user: User = Depends(get_current_user_dependency)) -> User:
    """Get current active user"""
    if current_user.disabled:
        raise HTTPException(status_code=400, detail="Inactive user")
    return current_user


def require_role(required_role: str):
    """Decorator to require specific role"""
    async def role_checker(current_user: User = Depends(get_current_active_user)):
        auth = AuthManager()
        if not auth.check_permission(current_user, required_role):
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail=f"Required role: {required_role}"
            )
        return current_user
    return role_checker


# OAuth2 integration example
class OAuth2Provider:
    """OAuth2 provider integration (Google, GitHub, etc.)"""

    def __init__(self, provider_name: str, client_id: str, client_secret: str):
        self.provider_name = provider_name
        self.client_id = client_id
        self.client_secret = client_secret

        self.providers = {
            "google": {
                "auth_url": "https://accounts.google.com/o/oauth2/v2/auth",
                "token_url": "https://oauth2.googleapis.com/token",
                "userinfo_url": "https://www.googleapis.com/oauth2/v2/userinfo"
            },
            "github": {
                "auth_url": "https://github.com/login/oauth/authorize",
                "token_url": "https://github.com/login/oauth/access_token",
                "userinfo_url": "https://api.github.com/user"
            }
        }

    def get_authorization_url(self, redirect_uri: str, state: str) -> str:
        """Get OAuth2 authorization URL"""
        provider = self.providers.get(self.provider_name)
        if not provider:
            raise ValueError(f"Unknown provider: {self.provider_name}")

        params = {
            "client_id": self.client_id,
            "redirect_uri": redirect_uri,
            "state": state,
            "scope": "openid email profile",
            "response_type": "code"
        }

        # Build URL with params
        from urllib.parse import urlencode
        return f"{provider['auth_url']}?{urlencode(params)}"

    async def exchange_code_for_token(self, code: str, redirect_uri: str) -> Dict[str, Any]:
        """Exchange authorization code for access token"""
        # Implementation would make HTTP request to token endpoint
        # This is a placeholder
        return {
            "access_token": "oauth_access_token",
            "token_type": "Bearer",
            "expires_in": 3600
        }


if __name__ == "__main__":
    print("=== LAM Authentication System Test ===\n")

    auth = AuthManager()

    # Test user creation
    print("1. Creating test user...")
    try:
        new_user = auth.create_user(
            username="testuser",
            password="test123",
            email="test@lam.local",
            full_name="Test User",
            roles=["user"]
        )
        print(f"✓ Created user: {new_user.username}")
    except ValueError as e:
        print(f"User might already exist: {e}")

    # Test authentication
    print("\n2. Testing authentication...")
    user = auth.authenticate_user("admin", "admin123")
    if user:
        print(f"✓ Authenticated: {user.username}")
        print(f"  Roles: {user.roles}")

    # Test token creation
    print("\n3. Creating tokens...")
    access_token = auth.create_access_token({"sub": user.username})
    refresh_token = auth.create_refresh_token({"sub": user.username})
    print(f"✓ Access token: {access_token[:50]}...")
    print(f"✓ Refresh token: {refresh_token[:50]}...")

    # Test token verification
    print("\n4. Verifying token...")
    payload = auth.decode_token(access_token)
    if payload:
        print(f"✓ Token valid for: {payload['sub']}")
        print(f"  Expires: {datetime.fromtimestamp(payload['exp'])}")

    # Test permissions
    print("\n5. Testing permissions...")
    print(f"Has 'admin' role: {auth.check_permission(user, 'admin')}")
    print(f"Has 'user' role: {auth.check_permission(user, 'user')}")
    print(f"Has 'superuser' role: {auth.check_permission(user, 'superuser')}")

    print("\n=== Default Credentials ===")
    print("Username: admin  | Password: admin123 | Roles: admin, user")
    print("Username: user   | Password: user123  | Roles: user")
