#!/bin/bash
# LAM Deployment Script

set -e

echo "🚀 LAM Deployment Script"
echo "========================"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Function to print colored output
print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

# Check Docker
if ! command -v docker &> /dev/null; then
    print_error "Docker not found. Please install Docker first."
    exit 1
fi

print_success "Docker found"

# Check Docker Compose
if ! command -v docker-compose &> /dev/null; then
    print_error "Docker Compose not found. Please install Docker Compose first."
    exit 1
fi

print_success "Docker Compose found"

# Build Docker image
print_info "Building Docker image..."
docker build -t motorhandpro/lam:latest .
print_success "Docker image built"

# Run tests in container
print_info "Running tests in container..."
docker run --rm motorhandpro/lam:latest python lam/tests/test_core.py
docker run --rm motorhandpro/lam:latest python lam/tests/test_actions.py
print_success "All tests passed"

# Start services
print_info "Starting LAM services..."
docker-compose up -d
print_success "Services started"

# Wait for services to be ready
print_info "Waiting for services to be ready..."
sleep 5

# Check health
print_info "Checking service health..."
if docker ps | grep -q lam-core; then
    print_success "LAM Core: Running"
else
    print_error "LAM Core: Not running"
fi

if docker ps | grep -q lam-web-ui; then
    print_success "LAM Web UI: Running"
else
    print_error "LAM Web UI: Not running"
fi

echo ""
echo "🎉 Deployment Complete!"
echo ""
echo "Access points:"
echo "  • Web UI:    http://localhost:8888"
echo "  • API:       http://localhost:8001"
echo "  • Core:      http://localhost:8000"
echo ""
echo "Commands:"
echo "  • View logs:    docker-compose logs -f"
echo "  • Stop:         docker-compose down"
echo "  • Restart:      docker-compose restart"
echo "  • Shell:        docker exec -it lam-core bash"
echo ""
