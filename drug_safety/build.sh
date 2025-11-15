#!/bin/bash

###############################################################################
# Drug Safety Model Build Script
# Part of MotorHandPro Project
###############################################################################

set -e  # Exit on error

echo "======================================================================="
echo "Drug Safety Modeling System - Build Script"
echo "======================================================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Detect D compiler
if command -v ldc2 &> /dev/null; then
    COMPILER="ldc2"
    COMPILER_FLAGS="-of=drug_safety_model -wi -g"
    RELEASE_FLAGS="-O3 -release -boundscheck=off"
    echo -e "${GREEN}✓${NC} Found LDC2 compiler (LLVM-based)"
elif command -v dmd &> /dev/null; then
    COMPILER="dmd"
    COMPILER_FLAGS="-of=drug_safety_model -wi -g"
    RELEASE_FLAGS="-O -inline -release"
    echo -e "${GREEN}✓${NC} Found DMD compiler"
else
    echo -e "${RED}✗${NC} No D compiler found!"
    echo ""
    echo "Please install a D compiler:"
    echo "  Ubuntu/Debian: sudo apt install ldc (or dmd-compiler)"
    echo "  macOS: brew install ldc (or dmd)"
    echo "  Windows: Download from https://dlang.org/download.html"
    echo ""
    exit 1
fi

echo ""

# Parse arguments
BUILD_TYPE="debug"
RUN_AFTER_BUILD=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --release)
            BUILD_TYPE="release"
            shift
            ;;
        --run)
            RUN_AFTER_BUILD=true
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --release    Build optimized release version"
            echo "  --run        Run the program after building"
            echo "  --help       Show this help message"
            echo ""
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Build
echo "Building Drug Safety Model..."
echo "  Compiler: $COMPILER"
echo "  Build Type: $BUILD_TYPE"
echo ""

if [ "$BUILD_TYPE" = "release" ]; then
    echo "Compiling (optimized)..."
    $COMPILER $COMPILER_FLAGS $RELEASE_FLAGS drug_safety_model.d
else
    echo "Compiling (debug)..."
    $COMPILER $COMPILER_FLAGS drug_safety_model.d
fi

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Build successful!"
    echo ""
    echo "Executable: ./drug_safety_model"

    # Get file size
    SIZE=$(du -h drug_safety_model | cut -f1)
    echo "Size: $SIZE"
    echo ""

    if [ "$RUN_AFTER_BUILD" = true ]; then
        echo "======================================================================="
        echo "Running Drug Safety Model..."
        echo "======================================================================="
        echo ""
        ./drug_safety_model
    else
        echo "To run: ./drug_safety_model"
        echo "Or: $0 --run"
    fi
else
    echo -e "${RED}✗${NC} Build failed!"
    exit 1
fi

echo ""
echo "======================================================================="
echo "Build Complete!"
echo "======================================================================="
