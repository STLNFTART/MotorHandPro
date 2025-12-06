#!/bin/bash
################################################################################
# MotorHandPro Environment Setup Script
# Patent Pending: U.S. Provisional Patent Application No. 63/842,846
#
# This script sets up the complete MotorHandPro environment after VM creation.
# It handles all dependencies, configurations, and initial setup.
#
# Usage:
#   ./setup.sh [options]
#
# Options:
#   --minimal      Install only core dependencies (no optional components)
#   --full         Install everything including optional components (default)
#   --dev          Install development tools and dependencies
#   --skip-tests   Skip running initial tests
#   --help         Show this help message
#
################################################################################

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Configuration
INSTALL_MODE="full"
SKIP_TESTS=false
INSTALL_DEV=false
PYTHON_MIN_VERSION="3.8"
NODE_MIN_VERSION="14"

################################################################################
# Parse command line arguments
################################################################################
parse_arguments() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            --minimal)
                INSTALL_MODE="minimal"
                shift
                ;;
            --full)
                INSTALL_MODE="full"
                shift
                ;;
            --dev)
                INSTALL_DEV=true
                shift
                ;;
            --skip-tests)
                SKIP_TESTS=true
                shift
                ;;
            --help)
                grep "^#" "$0" | grep -v "^#!/" | sed 's/^# //'
                exit 0
                ;;
            *)
                echo -e "${RED}Unknown option: $1${NC}"
                echo "Use --help for usage information"
                exit 1
                ;;
        esac
    done
}

################################################################################
# Print banner
################################################################################
print_banner() {
    echo -e "${BLUE}"
    cat << 'EOF'
████████████████████████████████████████████████████████████████████████████████
███╗   ███╗ ██████╗ ████████╗ ██████╗ ██████╗ ██╗  ██╗ █████╗ ███╗   ██╗██████╗
████╗ ████║██╔═══██╗╚══██╔══╝██╔═══██╗██╔══██╗██║  ██║██╔══██╗████╗  ██║██╔══██╗
██╔████╔██║██║   ██║   ██║   ██║   ██║██████╔╝███████║███████║██╔██╗ ██║██║  ██║
██║╚██╔╝██║██║   ██║   ██║   ██║   ██║██╔══██╗██╔══██║██╔══██║██║╚██╗██║██║  ██║
██║ ╚═╝ ██║╚██████╔╝   ██║   ╚██████╔╝██║  ██║██║  ██║██║  ██║██║ ╚████║██████╔╝
╚═╝     ╚═╝ ╚═════╝    ╚═╝    ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝╚═════╝
                                                    ██████╗ ██████╗  ██████╗
                                                    ██╔══██╗██╔══██╗██╔═══██╗
                                                    ██████╔╝██████╔╝██║   ██║
                                                    ██╔═══╝ ██╔══██╗██║   ██║
                                                    ██║     ██║  ██║╚██████╔╝
                                                    ╚═╝     ╚═╝  ╚═╝ ╚═════╝

            High-Precision Robotic Control with Primal Logic Framework
           Patent Pending: U.S. Provisional Patent Application No. 63/842,846
████████████████████████████████████████████████████████████████████████████████
EOF
    echo -e "${NC}\n"
}

################################################################################
# Utility functions
################################################################################
log_info() {
    echo -e "${CYAN}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

log_section() {
    echo -e "\n${MAGENTA}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${MAGENTA}  $1${NC}"
    echo -e "${MAGENTA}════════════════════════════════════════════════════════════════${NC}\n"
}

check_command() {
    if command -v "$1" &> /dev/null; then
        return 0
    else
        return 1
    fi
}

version_compare() {
    # Returns 0 if $1 >= $2, 1 otherwise
    printf '%s\n%s' "$2" "$1" | sort -C -V
}

################################################################################
# System information
################################################################################
print_system_info() {
    log_section "System Information"

    log_info "Operating System: $(uname -s)"
    log_info "Kernel Version: $(uname -r)"
    log_info "Architecture: $(uname -m)"
    log_info "Hostname: $(hostname)"

    if [ -f /etc/os-release ]; then
        . /etc/os-release
        log_info "Distribution: $NAME $VERSION"
    fi

    log_info "Install Mode: $INSTALL_MODE"
    log_info "Development Tools: $([ "$INSTALL_DEV" = true ] && echo "Yes" || echo "No")"
}

################################################################################
# Check prerequisites
################################################################################
check_prerequisites() {
    log_section "Checking Prerequisites"

    local all_ok=true

    # Check Python
    if check_command python3; then
        PYTHON_VERSION=$(python3 --version | awk '{print $2}')
        if version_compare "$PYTHON_VERSION" "$PYTHON_MIN_VERSION"; then
            log_success "Python $PYTHON_VERSION (>= $PYTHON_MIN_VERSION required)"
        else
            log_error "Python $PYTHON_VERSION found, but >= $PYTHON_MIN_VERSION required"
            all_ok=false
        fi
    else
        log_error "Python 3 not found. Please install Python >= $PYTHON_MIN_VERSION"
        all_ok=false
    fi

    # Check pip
    if check_command pip3; then
        PIP_VERSION=$(pip3 --version | awk '{print $2}')
        log_success "pip $PIP_VERSION found"
    else
        log_warning "pip3 not found. Will attempt to install."
    fi

    # Check Git
    if check_command git; then
        GIT_VERSION=$(git --version | awk '{print $3}')
        log_success "Git $GIT_VERSION found"
    else
        log_error "Git not found. Please install Git first."
        all_ok=false
    fi

    # Check optional tools
    if [ "$INSTALL_MODE" = "full" ]; then
        # Check Node.js
        if check_command node; then
            NODE_VERSION=$(node --version | sed 's/v//')
            if version_compare "$NODE_VERSION" "$NODE_MIN_VERSION"; then
                log_success "Node.js $NODE_VERSION (>= $NODE_MIN_VERSION required)"
            else
                log_warning "Node.js $NODE_VERSION found, but >= $NODE_MIN_VERSION recommended"
            fi
        else
            log_warning "Node.js not found. Control panel will not be available."
        fi

        # Check npm
        if check_command npm; then
            NPM_VERSION=$(npm --version)
            log_success "npm $NPM_VERSION found"
        else
            log_warning "npm not found. Node.js components will be skipped."
        fi

        # Check Docker
        if check_command docker; then
            DOCKER_VERSION=$(docker --version | awk '{print $3}' | sed 's/,//')
            log_success "Docker $DOCKER_VERSION found"
        else
            log_warning "Docker not found. Container deployment will not be available."
        fi

        # Check Docker Compose
        if check_command docker-compose; then
            COMPOSE_VERSION=$(docker-compose --version | awk '{print $3}' | sed 's/,//')
            log_success "Docker Compose $COMPOSE_VERSION found"
        else
            log_warning "Docker Compose not found. Multi-container deployment unavailable."
        fi
    fi

    if [ "$all_ok" = false ]; then
        log_error "Required prerequisites missing. Please install them and try again."
        exit 1
    fi

    echo ""
}

################################################################################
# Install system dependencies
################################################################################
install_system_dependencies() {
    log_section "Installing System Dependencies"

    # Detect package manager
    if check_command apt-get; then
        PKG_MANAGER="apt-get"
        UPDATE_CMD="apt-get update"
        INSTALL_CMD="apt-get install -y"
    elif check_command yum; then
        PKG_MANAGER="yum"
        UPDATE_CMD="yum check-update || true"
        INSTALL_CMD="yum install -y"
    elif check_command dnf; then
        PKG_MANAGER="dnf"
        UPDATE_CMD="dnf check-update || true"
        INSTALL_CMD="dnf install -y"
    elif check_command brew; then
        PKG_MANAGER="brew"
        UPDATE_CMD="brew update"
        INSTALL_CMD="brew install"
    else
        log_warning "No supported package manager found. Skipping system dependencies."
        return
    fi

    log_info "Using package manager: $PKG_MANAGER"

    # Update package lists
    log_info "Updating package lists..."
    if [ "$PKG_MANAGER" != "brew" ]; then
        sudo $UPDATE_CMD > /dev/null 2>&1 || true
    else
        $UPDATE_CMD > /dev/null 2>&1 || true
    fi

    # Install basic build tools
    log_info "Installing build essentials..."
    if [ "$PKG_MANAGER" = "apt-get" ]; then
        sudo $INSTALL_CMD build-essential python3-dev > /dev/null 2>&1 || true
    elif [[ "$PKG_MANAGER" =~ ^(yum|dnf)$ ]]; then
        sudo $INSTALL_CMD gcc gcc-c++ make python3-devel > /dev/null 2>&1 || true
    elif [ "$PKG_MANAGER" = "brew" ]; then
        # macOS typically has Xcode command line tools
        xcode-select --install > /dev/null 2>&1 || true
    fi

    log_success "System dependencies installed"
}

################################################################################
# Setup Python environment
################################################################################
setup_python_environment() {
    log_section "Setting Up Python Environment"

    # Upgrade pip (skip if system-managed)
    log_info "Upgrading pip..."
    python3 -m pip install --upgrade pip --quiet --user 2>/dev/null || log_info "Using system pip"
    log_success "pip ready"

    # Install core Python dependencies
    log_info "Installing core Python dependencies..."
    pip3 install --quiet -r requirements.txt
    log_success "Core dependencies installed"

    # Install LAM dependencies
    if [ -f lam_requirements.txt ]; then
        log_info "Installing LAM system dependencies..."
        pip3 install --quiet -r lam_requirements.txt
        log_success "LAM dependencies installed"
    fi

    # Install development dependencies
    if [ "$INSTALL_DEV" = true ]; then
        log_info "Installing development dependencies..."
        pip3 install --quiet pytest pytest-cov black flake8 mypy ipython jupyter
        log_success "Development tools installed"
    fi

    # Install ML dependencies if in full mode
    if [ "$INSTALL_MODE" = "full" ] && [ -f ml_datasets/requirements.txt ]; then
        log_info "Installing ML dataset dependencies..."
        pip3 install --quiet -r ml_datasets/requirements.txt || log_warning "Some ML dependencies failed to install"
    fi
}

################################################################################
# Setup Node.js environment
################################################################################
setup_nodejs_environment() {
    log_section "Setting Up Node.js Environment"

    if ! check_command npm; then
        log_warning "npm not found. Skipping Node.js setup."
        return
    fi

    # Install Node.js dependencies
    if [ -f package.json ]; then
        log_info "Installing Node.js dependencies..."
        npm install --silent > /dev/null 2>&1
        log_success "Node.js dependencies installed"
    fi

    # Install control panel dependencies
    if [ -d control_panel ] && [ -f control_panel/package.json ]; then
        log_info "Installing control panel dependencies..."
        cd control_panel
        npm install --silent > /dev/null 2>&1
        cd ..
        log_success "Control panel dependencies installed"
    fi
}

################################################################################
# Build native components
################################################################################
build_native_components() {
    log_section "Building Native Components"

    # Build D language components if compiler is available
    if check_command dmd || check_command ldc2 || check_command gdc; then
        log_info "D compiler found. Building D components..."

        if [ -d drug_safety ] && [ -f drug_safety/build.sh ]; then
            cd drug_safety
            bash build.sh > /dev/null 2>&1
            cd ..
            log_success "Drug safety module built"
        fi

        if [ -d dlang ] && [ -f dlang/build.sh ]; then
            cd dlang
            bash build.sh > /dev/null 2>&1 || log_warning "D language components build failed"
            cd ..
        fi
    else
        log_info "D compiler not found. Skipping D language components."
        log_info "Install DMD, LDC, or GDC to enable D language features."
    fi

    # Build APL components if available
    if [ -d apl ] && [ -f apl/build.sh ]; then
        log_info "Building APL components..."
        cd apl
        bash build.sh > /dev/null 2>&1 || log_warning "APL components build failed"
        cd ..
    fi

    # Build Prolog components if available
    if [ -d prolog ] && [ -f prolog/build.sh ]; then
        log_info "Building Prolog components..."
        cd prolog
        bash build.sh > /dev/null 2>&1 || log_warning "Prolog components build failed"
        cd ..
    fi

    # Build integration components
    if [ -d integration ] && [ -f integration/build_all.sh ]; then
        log_info "Building integration components..."
        cd integration
        bash build_all.sh > /dev/null 2>&1 || log_warning "Integration components build failed"
        cd ..
    fi
}

################################################################################
# Create necessary directories
################################################################################
create_directories() {
    log_section "Creating Directory Structure"

    log_info "Creating runtime directories..."

    mkdir -p logs
    mkdir -p data
    mkdir -p output
    mkdir -p results
    mkdir -p ~/.motorhand
    mkdir -p experiments/results
    mkdir -p validation_results

    log_success "Directory structure created"
}

################################################################################
# Setup configuration files
################################################################################
setup_configuration() {
    log_section "Setting Up Configuration"

    # Create .env file from example if it doesn't exist
    if [ ! -f .env ] && [ -f .env.example ]; then
        log_info "Creating .env file from template..."
        cp .env.example .env
        log_success ".env file created"
        log_warning "Please edit .env file with your configuration"
    elif [ -f .env ]; then
        log_info ".env file already exists"
    fi

    # Create npm config from template
    if [ ! -f .npmrc ] && [ -f .npmrc.template ]; then
        log_info "Creating .npmrc file from template..."
        cp .npmrc.template .npmrc
        log_success ".npmrc file created"
    fi

    # Create Docker production env if needed
    if [ -f .env.production ]; then
        log_info "Production environment configuration found"
    fi
}

################################################################################
# Initialize Git hooks
################################################################################
setup_git_hooks() {
    log_section "Setting Up Git Hooks"

    if [ -d .git ]; then
        log_info "Git repository detected"

        # Make sure all shell scripts are executable
        find . -name "*.sh" -type f -exec chmod +x {} \; 2>/dev/null
        log_success "Shell scripts made executable"

        # Set up pre-commit hook if dev mode
        if [ "$INSTALL_DEV" = true ]; then
            log_info "Installing pre-commit hooks..."
            if check_command pre-commit; then
                pre-commit install > /dev/null 2>&1 || true
                log_success "Pre-commit hooks installed"
            else
                log_info "pre-commit not installed. Skipping hooks."
            fi
        fi
    else
        log_info "Not a Git repository. Skipping Git setup."
    fi
}

################################################################################
# Run tests
################################################################################
run_tests() {
    if [ "$SKIP_TESTS" = true ]; then
        log_info "Skipping tests as requested"
        return
    fi

    log_section "Running Initial Tests"

    # Run Python smoke tests
    if [ -f lam/smoke_test.py ]; then
        log_info "Running LAM smoke tests..."
        if python3 lam/smoke_test.py > /dev/null 2>&1; then
            log_success "LAM smoke tests passed"
        else
            log_warning "LAM smoke tests failed (this may be expected if not configured)"
        fi
    fi

    # Run pytest if available
    if [ "$INSTALL_DEV" = true ] && check_command pytest; then
        log_info "Running pytest..."
        if pytest tests/ -v --tb=short > /dev/null 2>&1; then
            log_success "Unit tests passed"
        else
            log_warning "Some tests failed (check test output for details)"
        fi
    fi

    # Verify imports
    log_info "Verifying Python imports..."
    python3 -c "import numpy, scipy, matplotlib" 2>/dev/null && log_success "Core libraries import successfully"

    if [ -f lam_requirements.txt ]; then
        python3 -c "import asyncio, aiohttp" 2>/dev/null && log_success "Async libraries import successfully"
    fi
}

################################################################################
# Print summary and next steps
################################################################################
print_summary() {
    log_section "Setup Complete!"

    echo -e "${GREEN}"
    cat << 'EOF'
    ✅ MotorHandPro environment is ready!

    📁 Repository Structure:
       • /lam/              - Large Action Model system
       • /control_panel/    - Web-based control interface
       • /analysis/         - Data analysis tools
       • /docs/             - Documentation
       • /integrations/     - External integrations
       • /extras/           - Experimental features

EOF
    echo -e "${NC}"

    log_section "Quick Start Commands"

    echo -e "${CYAN}Run LAM interactive session:${NC}"
    echo "  python3 -i lam_interactive_session.py"
    echo ""

    echo -e "${CYAN}Start LAM system:${NC}"
    echo "  ./start_lam_system.sh"
    echo ""

    echo -e "${CYAN}Run analysis on benchmark data:${NC}"
    echo "  python3 analyze_runs.py"
    echo ""

    if check_command npm; then
        echo -e "${CYAN}Start control panel:${NC}"
        echo "  cd control_panel && npm start"
        echo ""
    fi

    if check_command docker-compose; then
        echo -e "${CYAN}Start with Docker:${NC}"
        echo "  docker-compose up -d"
        echo ""
    fi

    echo -e "${CYAN}View available demos:${NC}"
    echo "  python3 demo_lam.py"
    echo "  python3 lam_quick_demo.py"
    echo ""

    log_section "Documentation"

    echo -e "📖 Key documentation files:"
    echo "   • README.md                    - Project overview"
    echo "   • COMPLETE_SETUP_SUMMARY.md    - Complete setup guide"
    echo "   • PRIMAL_LOGIC_FRAMEWORK.md    - Mathematical framework"
    echo "   • LAM_APPLICATION_INTEGRATION.md - Integration guide"
    echo "   • docs/ARCHITECTURE.md         - System architecture"
    echo ""

    if [ ! -f .env ]; then
        log_warning "Don't forget to configure .env file for Hedera integration!"
    fi

    log_section "Support & Resources"

    echo "📧 Contact: Donte Lightfoot (STLNFTART)"
    echo "🔗 GitHub: https://github.com/STLNFTART/MotorHandPro"
    echo "📄 Patent Pending: U.S. Provisional Patent Application No. 63/842,846"
    echo ""

    log_success "Environment setup completed successfully! 🎉"
    echo ""
}

################################################################################
# Main execution
################################################################################
main() {
    # Parse arguments
    parse_arguments "$@"

    # Print banner
    print_banner

    # Show system info
    print_system_info

    # Check prerequisites
    check_prerequisites

    # Install system dependencies
    if [ "$INSTALL_MODE" = "full" ]; then
        install_system_dependencies
    fi

    # Setup Python environment
    setup_python_environment

    # Setup Node.js environment
    if [ "$INSTALL_MODE" = "full" ]; then
        setup_nodejs_environment
    fi

    # Build native components
    if [ "$INSTALL_MODE" = "full" ]; then
        build_native_components
    fi

    # Create directories
    create_directories

    # Setup configuration
    setup_configuration

    # Setup Git hooks
    setup_git_hooks

    # Run tests
    run_tests

    # Print summary
    print_summary
}

# Run main function
main "$@"
