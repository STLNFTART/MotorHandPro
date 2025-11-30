#!/usr/bin/env bash
# Run each branch in a repository: clone if needed, iterate branches and run tests/build
# Usage: ./run-every-branch.sh 

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Print functions
print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

print_warning() {
    echo -e "${YELLOW}⚠ $1${NC}"
}

print_error() {
    echo -e "${RED}✗ $1${NC}"
}

echo -e "${BLUE}"
echo "========================================"
echo "  Run Tests/Builds Across All Branches"
echo "========================================"
echo -e "${NC}"

# Get the repository root directory
REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Check if we're in a git repository
if ! git -C "$REPO_DIR" rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    print_error "Not a git repository: $REPO_DIR"
    exit 1
fi

print_success "Working in repository: $REPO_DIR"

# Save current branch to restore later
ORIGINAL_BRANCH=$(git -C "$REPO_DIR" rev-parse --abbrev-ref HEAD)
print_info "Current branch: $ORIGINAL_BRANCH"

# Fetch all remote branches
print_info "Fetching all remote branches..."
git -C "$REPO_DIR" fetch --all --prune

# Get list of all remote branches
BRANCHES=$(git -C "$REPO_DIR" branch -r | grep -v HEAD | sed 's/origin\///' | tr -d ' ')

# Track results
PASSED_BRANCHES=()
FAILED_BRANCHES=()

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}Processing branches...${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo ""

for BRANCH in $BRANCHES; do
    echo ""
    echo -e "${YELLOW}────────────────────────────────────────${NC}"
    echo -e "${BLUE}Branch: $BRANCH${NC}"
    echo -e "${YELLOW}────────────────────────────────────────${NC}"

    # Checkout the branch
    if ! git -C "$REPO_DIR" checkout "$BRANCH" --quiet 2>/dev/null; then
        print_warning "Could not checkout branch: $BRANCH (skipping)"
        FAILED_BRANCHES+=("$BRANCH (checkout failed)")
        continue
    fi

    # Pull latest changes
    git -C "$REPO_DIR" pull origin "$BRANCH" --quiet 2>/dev/null || true

    # Run tests/build based on what's available
    BRANCH_PASSED=true

    # Check for package.json and run npm test
    if [ -f "$REPO_DIR/package.json" ]; then
        print_info "Running npm test..."
        if npm test --prefix "$REPO_DIR" 2>/dev/null; then
            print_success "npm test passed"
        else
            print_warning "npm test failed or not configured"
            BRANCH_PASSED=false
        fi
    fi

    # Check for requirements.txt and run pytest
    if [ -f "$REPO_DIR/requirements.txt" ]; then
        print_info "Running Python tests..."
        if python3 -m pytest "$REPO_DIR/tests/" 2>/dev/null; then
            print_success "pytest passed"
        else
            print_warning "pytest failed or no tests found"
            BRANCH_PASSED=false
        fi
    fi

    # Check for Makefile
    if [ -f "$REPO_DIR/Makefile" ]; then
        print_info "Running make..."
        if make -C "$REPO_DIR" 2>/dev/null; then
            print_success "make passed"
        else
            print_warning "make failed"
            BRANCH_PASSED=false
        fi
    fi

    # Record result
    if [ "$BRANCH_PASSED" = true ]; then
        PASSED_BRANCHES+=("$BRANCH")
        print_success "Branch $BRANCH: PASSED"
    else
        FAILED_BRANCHES+=("$BRANCH")
        print_error "Branch $BRANCH: FAILED"
    fi
done

# Restore original branch
print_info "Restoring original branch: $ORIGINAL_BRANCH"
git -C "$REPO_DIR" checkout "$ORIGINAL_BRANCH" --quiet 2>/dev/null || true

# Print summary
echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo -e "${GREEN}Summary${NC}"
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
echo ""

echo -e "${GREEN}Passed branches (${#PASSED_BRANCHES[@]}):${NC}"
for BRANCH in "${PASSED_BRANCHES[@]}"; do
    echo "  ✓ $BRANCH"
done

echo ""

if [ ${#FAILED_BRANCHES[@]} -gt 0 ]; then
    echo -e "${RED}Failed branches (${#FAILED_BRANCHES[@]}):${NC}"
    for BRANCH in "${FAILED_BRANCHES[@]}"; do
        echo "  ✗ $BRANCH"
    done
fi

echo ""
echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"

# Exit with appropriate code
if [ ${#FAILED_BRANCHES[@]} -gt 0 ]; then
    exit 1
fi

exit 0
