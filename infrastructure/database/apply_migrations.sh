#!/bin/bash
# Apply database migrations for MotorHandPro
#
# Usage:
#   ./apply_migrations.sh [migration_number]
#
# If no migration number is provided, applies all migrations in order.
#
# Environment variables:
#   DATABASE_URL - PostgreSQL connection string
#   PGHOST, PGPORT, PGUSER, PGPASSWORD, PGDATABASE - PostgreSQL connection params

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MIGRATIONS_DIR="$SCRIPT_DIR/migrations"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "=============================================================================="
echo "MotorHandPro Database Migration Runner"
echo "=============================================================================="
echo ""

# Check if psql is available
if ! command -v psql &> /dev/null; then
    echo -e "${RED}❌ psql command not found${NC}"
    echo "Install PostgreSQL client:"
    echo "  Ubuntu/Debian: sudo apt-get install postgresql-client"
    echo "  macOS: brew install postgresql"
    exit 1
fi

# Database connection info
if [ -n "$DATABASE_URL" ]; then
    echo "Using DATABASE_URL environment variable"
    DB_CONN="$DATABASE_URL"
else
    PGHOST="${PGHOST:-localhost}"
    PGPORT="${PGPORT:-5432}"
    PGUSER="${PGUSER:-postgres}"
    PGDATABASE="${PGDATABASE:-motorhand}"

    echo "Database connection:"
    echo "  Host: $PGHOST"
    echo "  Port: $PGPORT"
    echo "  User: $PGUSER"
    echo "  Database: $PGDATABASE"

    if [ -z "$PGPASSWORD" ]; then
        echo -e "${YELLOW}⚠️  PGPASSWORD not set - psql may prompt for password${NC}"
    fi

    DB_CONN="postgresql://$PGUSER@$PGHOST:$PGPORT/$PGDATABASE"
fi

echo ""

# Test connection
echo "Testing database connection..."
if psql "$DB_CONN" -c "SELECT version();" > /dev/null 2>&1; then
    echo -e "${GREEN}✅ Connected to database${NC}"
else
    echo -e "${RED}❌ Failed to connect to database${NC}"
    echo ""
    echo "Check your connection settings and try again."
    exit 1
fi

echo ""
echo "=============================================================================="
echo "Available Migrations"
echo "=============================================================================="

# List all migration files
MIGRATIONS=($(ls -1 "$MIGRATIONS_DIR"/*.sql 2>/dev/null | sort))

if [ ${#MIGRATIONS[@]} -eq 0 ]; then
    echo -e "${YELLOW}No migration files found in $MIGRATIONS_DIR${NC}"
    exit 0
fi

for i in "${!MIGRATIONS[@]}"; do
    migration="${MIGRATIONS[$i]}"
    filename=$(basename "$migration")
    echo "  $((i+1)). $filename"
done

echo ""

# Apply specific migration or all
if [ -n "$1" ]; then
    # Apply specific migration
    MIGRATION_FILE="$MIGRATIONS_DIR/$1"

    if [ ! -f "$MIGRATION_FILE" ]; then
        echo -e "${RED}❌ Migration file not found: $MIGRATION_FILE${NC}"
        exit 1
    fi

    echo "Applying migration: $(basename "$MIGRATION_FILE")"
    echo "=============================================================================="
    echo ""

    if psql "$DB_CONN" -f "$MIGRATION_FILE"; then
        echo ""
        echo -e "${GREEN}✅ Migration applied successfully${NC}"
    else
        echo ""
        echo -e "${RED}❌ Migration failed${NC}"
        exit 1
    fi
else
    # Apply all migrations
    echo "Applying all migrations..."
    echo "=============================================================================="
    echo ""

    for migration in "${MIGRATIONS[@]}"; do
        filename=$(basename "$migration")
        echo "Applying: $filename"

        if psql "$DB_CONN" -f "$migration"; then
            echo -e "${GREEN}✅ $filename applied${NC}"
        else
            echo -e "${RED}❌ $filename failed${NC}"
            echo ""
            echo "Stopping migration process."
            exit 1
        fi

        echo ""
    done

    echo -e "${GREEN}✅ All migrations applied successfully${NC}"
fi

echo ""
echo "=============================================================================="
echo "Verifying NASA Data Schema"
echo "=============================================================================="
echo ""

echo "Checking nasa_data schema..."
psql "$DB_CONN" -c "\dn+ nasa_data" 2>/dev/null && echo -e "${GREEN}✅ nasa_data schema exists${NC}" || echo -e "${YELLOW}⚠️  nasa_data schema not found${NC}"

echo ""
echo "Checking comet_observations table..."
psql "$DB_CONN" -c "\dt+ nasa_data.comet_observations" 2>/dev/null && echo -e "${GREEN}✅ comet_observations table exists${NC}" || echo -e "${YELLOW}⚠️  comet_observations table not found${NC}"

echo ""
echo "Checking processed_states table..."
psql "$DB_CONN" -c "\dt+ nasa_data.processed_states" 2>/dev/null && echo -e "${GREEN}✅ processed_states table exists${NC}" || echo -e "${YELLOW}⚠️  processed_states table not found${NC}"

echo ""
echo "=============================================================================="
echo "Migration complete!"
echo "=============================================================================="
