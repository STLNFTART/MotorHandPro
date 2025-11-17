#!/bin/bash
# Build script for MotorHandPro Biomedical Framework
# Supports both LDC2 (LLVM D Compiler) and DMD (reference compiler)
# Usage: ./build.sh [--debug|--release|--unittest] [--run]

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Default values
BUILD_TYPE="debug"
RUN_AFTER_BUILD=false
COMPILER=""

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --debug)
            BUILD_TYPE="debug"
            shift
            ;;
        --release)
            BUILD_TYPE="release"
            shift
            ;;
        --unittest)
            BUILD_TYPE="unittest"
            shift
            ;;
        --run)
            RUN_AFTER_BUILD=true
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Usage: $0 [--debug|--release|--unittest] [--run]"
            exit 1
            ;;
    esac
done

# Detect available D compiler
echo -e "${YELLOW}🔍 Detecting D compiler...${NC}"
if command -v ldc2 &> /dev/null; then
    COMPILER="ldc2"
    echo -e "${GREEN}✓ Found LDC2 (LLVM D Compiler)${NC}"
elif command -v dmd &> /dev/null; then
    COMPILER="dmd"
    echo -e "${GREEN}✓ Found DMD (Digital Mars D Compiler)${NC}"
else
    echo -e "${RED}✗ No D compiler found!${NC}"
    echo "Please install either LDC2 or DMD:"
    echo "  - Ubuntu/Debian: sudo apt-get install ldc"
    echo "  - macOS: brew install ldc"
    echo "  - Or visit: https://dlang.org/download.html"
    exit 1
fi

# Check if DUB is available (preferred build method)
if command -v dub &> /dev/null; then
    echo -e "${GREEN}✓ Found DUB package manager${NC}"
    echo -e "${YELLOW}📦 Building with DUB (${BUILD_TYPE} mode)...${NC}"

    dub build --compiler=${COMPILER} --build=${BUILD_TYPE} --config=demo

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Build successful!${NC}"
        EXECUTABLE="./bin/biomedical_framework"
    else
        echo -e "${RED}✗ Build failed!${NC}"
        exit 1
    fi
else
    # Fallback to direct compiler invocation
    echo -e "${YELLOW}⚠ DUB not found, using direct compiler invocation${NC}"
    echo -e "${YELLOW}📦 Building with ${COMPILER} (${BUILD_TYPE} mode)...${NC}"

    mkdir -p bin

    # Set compiler flags based on build type and compiler
    if [ "$COMPILER" = "ldc2" ]; then
        case $BUILD_TYPE in
            debug)
                FLAGS="-g -w -d-debug"
                ;;
            release)
                FLAGS="-O3 -release -inline -w"
                ;;
            unittest)
                FLAGS="-g -unittest -w"
                ;;
        esac
    else  # DMD
        case $BUILD_TYPE in
            debug)
                FLAGS="-g -w -debug"
                ;;
            release)
                FLAGS="-O -release -inline -w"
                ;;
            unittest)
                FLAGS="-g -unittest -w"
                ;;
        esac
    fi

    # Compile
    ${COMPILER} ${FLAGS} biomedical_framework.d -of=bin/biomedical_framework

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Build successful!${NC}"
        EXECUTABLE="./bin/biomedical_framework"
    else
        echo -e "${RED}✗ Build failed!${NC}"
        exit 1
    fi
fi

# Run if requested
if [ "$RUN_AFTER_BUILD" = true ]; then
    echo -e "${YELLOW}🚀 Running biomedical framework...${NC}"
    echo "============================================"
    ${EXECUTABLE}
    echo "============================================"
    echo -e "${GREEN}✓ Execution completed${NC}"
fi

# Display usage instructions if not running
if [ "$RUN_AFTER_BUILD" = false ]; then
    echo ""
    echo -e "${GREEN}To run the biomedical framework:${NC}"
    echo "  ${EXECUTABLE}"
    echo ""
    echo -e "${GREEN}Or rebuild and run in one step:${NC}"
    echo "  ./build.sh --release --run"
fi
