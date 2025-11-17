#!/usr/bin/env python3
"""
LAM Database Manager
PostgreSQL connection and session management
"""
import os
from typing import Generator
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker, Session
from .models import Base

# Database URL from environment or default
DATABASE_URL = os.getenv(
    "DATABASE_URL",
    "postgresql://lam_user:lam_password@localhost:5432/lam_db"
)


class DatabaseManager:
    """Database connection and session manager"""

    def __init__(self, database_url: str = DATABASE_URL):
        self.database_url = database_url
        self.engine = create_engine(database_url, echo=False)
        self.SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=self.engine)

    def create_tables(self):
        """Create all tables"""
        Base.metadata.create_all(bind=self.engine)
        print("✓ Database tables created")

    def drop_tables(self):
        """Drop all tables (use with caution!)"""
        Base.metadata.drop_all(bind=self.engine)
        print("✓ Database tables dropped")

    def get_session(self) -> Generator[Session, None, None]:
        """Get database session (dependency injection for FastAPI)"""
        db = self.SessionLocal()
        try:
            yield db
        finally:
            db.close()

    def execute_raw_sql(self, sql: str):
        """Execute raw SQL"""
        with self.engine.connect() as conn:
            result = conn.execute(sql)
            return result


# Singleton instance
db_manager = DatabaseManager()


# Dependency for FastAPI
def get_db() -> Generator[Session, None, None]:
    """FastAPI dependency to get database session"""
    return db_manager.get_session()


if __name__ == "__main__":
    print("=== LAM Database Manager Test ===\n")

    # Test connection
    print("Testing database connection...")
    try:
        db = DatabaseManager()
        print(f"✓ Connected to: {DATABASE_URL}")

        # Create tables
        print("\nCreating tables...")
        db.create_tables()

        # Test session
        print("\nTesting session...")
        session_gen = db.get_session()
        session = next(session_gen)
        print(f"✓ Session created: {session}")
        session.close()

        print("\n=== Database Setup Complete ===")
        print("\nTo use PostgreSQL:")
        print("1. Install PostgreSQL")
        print("2. Create database: CREATE DATABASE lam_db;")
        print("3. Create user: CREATE USER lam_user WITH PASSWORD 'lam_password';")
        print("4. Grant privileges: GRANT ALL PRIVILEGES ON DATABASE lam_db TO lam_user;")
        print("5. Set DATABASE_URL environment variable")

    except Exception as e:
        print(f"✗ Database connection failed: {e}")
        print("\nUsing SQLite for development:")
        print("  DATABASE_URL=sqlite:///./lam.db python lam/database/database.py")
