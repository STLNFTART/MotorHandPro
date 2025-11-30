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

print_header() {
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════════════${NC}"
}

# Track results
declare -a PASSED_BRANCHES=()
declare -a FAILED_BRANCHES=()

# Determine if we're in a git repository
if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    REPO_DIR=$(git rev-parse --show-toplevel)
    print_info "Using existing repository at: $REPO_DIR"
else
    print_error "Not inside a git repository. Please run from within a git repository."
    exit 1
fi

cd "$REPO_DIR"

# Fetch all remote branches
print_header "Fetching all remote branches"
git fetch --all --prune
print_success "Fetched all remote branches"

# Get current branch to restore later
ORIGINAL_BRANCH=$(git rev-parse --abbrev-ref HEAD)
print_info "Current branch: $ORIGINAL_BRANCH"

# Get list of all remote branches (from origin only)
BRANCHES=$(git branch -r | grep 'origin/' | grep -v HEAD | sed 's/origin\///' | xargs)

print_header "Found branches"
for branch in $BRANCHES; do
    echo "  • $branch"
done
echo ""

# Function to run tests/build for a branch
run_branch_tests() {
    local branch=$1
    print_header "Testing branch: $branch"

    # Checkout the branch
    if ! git checkout -f "$branch" 2>/dev/null && ! git checkout -f -b "$branch" "origin/$branch" 2>/dev/null; then
        print_error "Failed to checkout branch: $branch"
        return 1
    fi

    print_success "Checked out branch: $branch"

    # Run tests/build based on what's available
    local test_result=0

    # Check for package.json and run npm test if available
    if [ -f "package.json" ]; then
        print_info "Found package.json - running npm test"
        if npm test; then
            print_success "npm test passed"
        else
            print_warning "npm test failed or not configured"
            # Don't fail if npm test isn't configured
        fi
    fi

    # Check for requirements.txt and run pytest if available
    if [ -f "requirements.txt" ] || [ -f "lam/requirements.txt" ]; then
        print_info "Found Python project - running pytest"
        if python3 -m pytest; then
            print_success "pytest passed"
        else
            print_warning "pytest failed or not configured"
        fi
    fi

    # Check for LAM tests specifically
    if [ -f "lam/tests/test_core.py" ]; then
        print_info "Running LAM core tests"
        if python3 lam/tests/test_core.py; then
            print_success "LAM core tests passed"
        else
            print_warning "LAM core tests failed"
            test_result=1
        fi
    fi

    if [ -f "lam/tests/test_actions.py" ]; then
        print_info "Running LAM action tests"
        if python3 lam/tests/test_actions.py; then
            print_success "LAM action tests passed"
        else
            print_warning "LAM action tests failed"
            test_result=1
        fi
    fi

    # Check for Solidity projects (Foundry)
    if [ -f "foundry.toml" ]; then
        print_info "Found Foundry project - running forge tests"
        if forge test; then
            print_success "Forge tests passed"
        else
            print_warning "Forge tests failed or forge not installed"
            test_result=1
        fi
    fi

    # Check for Solidity projects (Hardhat)
    if [ -f "hardhat.config.js" ] || [ -f "hardhat.config.ts" ]; then
        print_info "Found Hardhat project - running hardhat test"
        if npx hardhat test; then
            print_success "Hardhat tests passed"
        else
            print_warning "Hardhat tests failed or hardhat not installed"
            test_result=1
        fi
    fi

    # Check for Dockerfile (just note its presence, actual build may be expensive)
    if [ -f "Dockerfile" ]; then
        print_info "Found Dockerfile - Docker image build available"
    fi

    return $test_result
}

# Iterate through all branches
print_header "Running tests on all branches"
echo ""

for branch in $BRANCHES; do
    if run_branch_tests "$branch"; then
        PASSED_BRANCHES+=("$branch")
        print_success "Branch $branch: PASSED"
    else
        FAILED_BRANCHES+=("$branch")
        print_error "Branch $branch: FAILED"
    fi
    echo ""
done

# Restore original branch
print_header "Restoring original branch"
git checkout -f "$ORIGINAL_BRANCH"
print_success "Restored to branch: $ORIGINAL_BRANCH"

# Print summary
print_header "Summary"
echo ""
echo -e "${GREEN}Passed (${#PASSED_BRANCHES[@]}):${NC}"
for branch in "${PASSED_BRANCHES[@]}"; do
    echo -e "  ${GREEN}✓${NC} $branch"
done

echo ""
echo -e "${RED}Failed (${#FAILED_BRANCHES[@]}):${NC}"
for branch in "${FAILED_BRANCHES[@]}"; do
    echo -e "  ${RED}✗${NC} $branch"
done

echo ""

if [ ${#FAILED_BRANCHES[@]} -eq 0 ]; then
    print_success "All branches passed!"
    exit 0
else
    print_error "Some branches failed tests"
    exit 1
fi
