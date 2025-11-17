# Installation Guide for Biomedical Framework

## Prerequisites

The Biomedical Framework requires a D language compiler. This guide will help you install the necessary tools.

## Installing D Compiler

### Ubuntu/Debian

```bash
# Install LDC2 (recommended - LLVM-based, best performance)
sudo apt-get update
sudo apt-get install ldc

# Or install DMD (reference compiler)
sudo apt-get install dmd

# Install DUB package manager
curl -fsS https://dlang.org/install.sh | bash -s dub
```

### macOS

```bash
# Install LDC2 using Homebrew
brew install ldc

# Or install DMD
brew install dmd
```

### Other Systems

Visit the official D language website for installation instructions:
https://dlang.org/download.html

## Verifying Installation

After installation, verify your D compiler is working:

```bash
# Check LDC2
ldc2 --version

# Or check DMD
dmd --version

# Check DUB
dub --version
```

## Building the Biomedical Framework

Once the D compiler is installed:

```bash
# Navigate to the biomedical_simulation directory
cd /path/to/MotorHandPro/biomedical_simulation

# Build using the automated script
./build.sh --release

# Or build with DUB directly
dub build --build=release --config=demo

# Run the framework
./bin/biomedical_framework
```

## Quick Test

```bash
# Build and run in one command
./build.sh --release --run
```

Expected output:
```
🧬 BIOMEDICAL INTEGRATION FRAMEWORK DEMONSTRATION
   Part of MotorHandPro Life Extension Research Initiative
============================================================

🔍 Starting Biomedical Model Discovery Phase...
   📈 Found 3 cardiac models
   🧪 Found 3 organ-on-chip experiments

📊 DISCOVERY RESULTS:
   • Total Life Extension Potential: 0.9XXX
   • Integration Opportunities: X

[... additional output ...]
```

## Troubleshooting

### "No D compiler found"

- Ensure you've installed either LDC2 or DMD
- Verify the compiler is in your PATH: `which ldc2` or `which dmd`
- Restart your terminal after installation

### "dub: command not found"

- Install DUB separately using the curl command above
- Or use direct compiler invocation (build.sh supports this)

### Build errors

- Ensure you're using D compiler version 2.0 or higher
- Check that all source files are present
- Try cleaning: `rm -rf .dub/ bin/` then rebuild

### Permission denied

```bash
chmod +x build.sh
./build.sh --release
```

## Alternative: Using Docker

If you prefer not to install D locally, you can use Docker:

```bash
# Pull official D image
docker pull dlang2/ldc-ubuntu

# Build in container
docker run --rm -v $(pwd):/src -w /src dlang2/ldc-ubuntu dub build --build=release

# Run
./bin/biomedical_framework
```

## Next Steps

After successful installation and build:

1. Read the [README.md](README.md) for usage instructions
2. Explore examples in [examples/cardiac_simulation.d](examples/cardiac_simulation.d)
3. Integrate with the broader MotorHandPro system
4. Review the [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md)

## Support

For installation issues:
- Check D Language documentation: https://dlang.org/
- Review DUB documentation: https://dub.pm/
- Consult the main MotorHandPro README

---

**Note**: The D compiler is not currently installed in the development environment. Please install it following the instructions above before building the biomedical framework.
