# Publishing Guide - MotorHandPro

This guide covers publishing MotorHandPro packages to GitHub Packages and DUB registry.

## Table of Contents

- [npm Publishing (JavaScript/Python Components)](#npm-publishing)
- [DUB Publishing (D Language Drug Safety Module)](#dub-publishing)
- [Version Management](#version-management)
- [CI/CD Integration](#cicd-integration)

---

## npm Publishing

MotorHandPro's JavaScript control panel and Python integrations can be published to GitHub Packages using npm.

### Prerequisites

1. **GitHub Personal Access Token (Classic)**
   - Go to: https://github.com/settings/tokens
   - Click "Generate new token (classic)"
   - Select scopes:
     - ✓ `read:packages`
     - ✓ `write:packages`
     - ✓ `delete:packages` (optional)
   - Copy the token

2. **Configure Authentication**

   ```bash
   # Copy the template
   cp .npmrc.template .npmrc

   # Edit .npmrc and replace YOUR_GITHUB_TOKEN with your actual token
   # The .npmrc file is gitignored to prevent token leakage
   ```

   Or login via npm:

   ```bash
   npm login --scope=@stlnftart --auth-type=legacy --registry=https://npm.pkg.github.com
   ```

   When prompted:
   - **Username:** Your GitHub username
   - **Password:** Your personal access token
   - **Email:** Your public GitHub email

### Publishing to GitHub Packages

1. **Update Version**

   ```bash
   # Bump version (patch, minor, or major)
   npm version patch -m "Release v%s"
   ```

2. **Publish**

   ```bash
   npm publish
   ```

3. **Verify**

   Visit: `https://github.com/STLNFTART/MotorHandPro/packages`

### Installing from GitHub Packages

**For other projects to use MotorHandPro:**

1. **Create `.npmrc` in your project:**

   ```
   @stlnftart:registry=https://npm.pkg.github.com
   //npm.pkg.github.com/:_authToken=YOUR_GITHUB_TOKEN
   ```

2. **Install:**

   ```bash
   npm install @stlnftart/motorhandpro
   ```

3. **Use in your code:**

   ```javascript
   // Import MotorHandPro control panel utilities
   const motorControl = require('@stlnftart/motorhandpro');
   ```

---

## DUB Publishing

The D Language Drug Safety Modeling System uses DUB, the official D package manager.

### Prerequisites

1. **Install DUB**

   DUB comes with DMD and LDC2 compilers:

   ```bash
   # Ubuntu/Debian
   sudo apt install dub

   # macOS
   brew install dub

   # Verify
   dub --version
   ```

2. **DUB Account** (for code.dlang.org registry)

   - Register at: https://code.dlang.org/register
   - Or use GitHub authentication

### Local Development

**Build with DUB:**

```bash
cd drug_safety

# Build demo executable
dub build

# Build as library
dub build --config=library

# Run
dub run

# Run tests (when implemented)
dub test
```

**Configuration Options:**

```bash
# Debug build
dub build --build=debug

# Release build (optimized)
dub build --build=release

# Specific compiler
dub build --compiler=ldc2
dub build --compiler=dmd
```

### Publishing to DUB Registry (code.dlang.org)

1. **Update `drug_safety/dub.json`**

   ```json
   {
     "name": "motorhandpro-drug-safety",
     "version": "1.0.0",
     ...
   }
   ```

2. **Create Git Tag**

   DUB uses git tags for versions:

   ```bash
   git tag v1.0.0
   git push origin v1.0.0
   ```

3. **Publish to DUB Registry**

   Option A: Automatic via GitHub

   - DUB automatically indexes public GitHub repositories
   - Ensure your repo URL is in `dub.json`
   - Tags become package versions

   Option B: Manual Registration

   ```bash
   dub publish --skip-registry=false
   ```

4. **Verify**

   Visit: `https://code.dlang.org/packages/motorhandpro-drug-safety`

### Using the DUB Package

**For other D projects to use the drug safety module:**

1. **Add dependency to `dub.json`:**

   ```json
   {
     "dependencies": {
       "motorhandpro-drug-safety": "~>1.0.0"
     }
   }
   ```

2. **Or add via command line:**

   ```bash
   dub add motorhandpro-drug-safety
   ```

3. **Use in your D code:**

   ```d
   import drug_safety_model;

   void main() {
       auto controller = new CardiacMetaController(512);
       controller.registerCardiacModel("my_model", "ECG", initialState);
       // ...
   }
   ```

---

## Version Management

### Semantic Versioning

MotorHandPro follows [Semantic Versioning 2.0.0](https://semver.org/):

- **MAJOR** version: Incompatible API changes
- **MINOR** version: Backwards-compatible functionality additions
- **PATCH** version: Backwards-compatible bug fixes

### Coordinating Versions Across Package Managers

When releasing a new version:

1. **Update both package files:**

   ```bash
   # package.json
   npm version <major|minor|patch>

   # drug_safety/dub.json (manual edit)
   # Update "version": "x.y.z"
   ```

2. **Create unified git tag:**

   ```bash
   git tag -a v1.0.0 -m "Release v1.0.0: Description"
   git push origin v1.0.0
   ```

3. **Publish both:**

   ```bash
   # npm
   npm publish

   # DUB (automatic from git tag)
   # Or manually:
   cd drug_safety && dub publish
   ```

---

## CI/CD Integration

### GitHub Actions Workflow Example

Create `.github/workflows/publish.yml`:

```yaml
name: Publish Packages

on:
  push:
    tags:
      - 'v*'

jobs:
  publish-npm:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write
    steps:
      - uses: actions/checkout@v3

      - uses: actions/setup-node@v3
        with:
          node-version: '18'
          registry-url: 'https://npm.pkg.github.com'
          scope: '@stlnftart'

      - run: npm publish
        env:
          NODE_AUTH_TOKEN: ${{ secrets.GITHUB_TOKEN }}

  publish-dub:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install D compiler
        uses: dlang-community/setup-dlang@v1
        with:
          compiler: ldc-latest

      - name: Build and test
        run: |
          cd drug_safety
          dub build --build=release
          dub test

      # DUB auto-publishes from GitHub tags
```

### Automated Version Bumping

```yaml
# .github/workflows/version-bump.yml
name: Version Bump

on:
  workflow_dispatch:
    inputs:
      version:
        description: 'Version bump type'
        required: true
        type: choice
        options:
          - patch
          - minor
          - major

jobs:
  bump:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: actions/setup-node@v3

      - name: Configure git
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"

      - name: Bump version
        run: |
          npm version ${{ github.event.inputs.version }} -m "chore: bump version to %s"
          git push && git push --tags
```

---

## Security Best Practices

### Token Management

1. **Never commit tokens:**
   - `.npmrc` is gitignored
   - Use environment variables in CI/CD
   - Rotate tokens regularly

2. **Use scoped tokens:**
   - Minimum required permissions
   - Separate tokens for different purposes

3. **GitHub Actions:**
   - Use `GITHUB_TOKEN` (automatic)
   - Store PATs in repository secrets

### Package Security

1. **Audit dependencies:**

   ```bash
   npm audit
   ```

2. **Enable 2FA:**
   - On GitHub account
   - On npm account (if using npm registry)

3. **Sign commits and tags:**

   ```bash
   git config commit.gpgsign true
   git config tag.gpgsign true
   ```

---

## Troubleshooting

### npm Publishing Issues

**Error: 403 Forbidden**
- Check token permissions (`write:packages`)
- Verify package scope matches registry
- Ensure package name is available

**Error: 404 Not Found**
- Verify registry URL in `.npmrc`
- Check package scope `@stlnftart`
- Ensure authentication is configured

### DUB Publishing Issues

**"Package already exists"**
- DUB auto-publishes from git tags
- Check existing versions at code.dlang.org
- Increment version in `dub.json`

**Build failures:**

```bash
# Clean and rebuild
dub clean
dub build --force

# Verbose output
dub build --verbose
```

---

## Package URLs

- **npm (GitHub Packages):** https://github.com/STLNFTART/MotorHandPro/packages
- **DUB (code.dlang.org):** https://code.dlang.org/packages/motorhandpro-drug-safety

---

## Support

For publishing issues:
- GitHub Issues: https://github.com/STLNFTART/MotorHandPro/issues
- Contact: Donte Lightfoot (STLNFTART)

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
