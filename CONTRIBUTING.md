# Contributing to MotorHandPro

Thank you for your interest in contributing to MotorHandPro! This document provides guidelines for contributing to the project while respecting the patent-pending status and research-focused nature of this work.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Types of Contributions](#types-of-contributions)
- [Development Process](#development-process)
- [Pull Request Guidelines](#pull-request-guidelines)
- [Coding Standards](#coding-standards)
- [Documentation](#documentation)
- [Testing Requirements](#testing-requirements)
- [Licensing and IP Considerations](#licensing-and-ip-considerations)
- [Communication Channels](#communication-channels)

## Code of Conduct

Please read and follow our [Code of Conduct](CODE_OF_CONDUCT.md) to ensure a welcoming and professional environment for all contributors.

## Getting Started

### Prerequisites

1. **Review the License**
   - Read the [LICENSE](LICENSE) file carefully
   - Understand the patent-pending status and restrictions
   - Note that commercial use requires separate licensing

2. **Understand the Framework**
   - Read [README.md](README.md) for system overview
   - Study [PRIMAL_LOGIC_FRAMEWORK.md](PRIMAL_LOGIC_FRAMEWORK.md) for mathematical foundations
   - Review existing documentation in your area of interest

3. **Set Up Development Environment**
   ```bash
   # Clone the repository
   git clone https://github.com/STLNFTART/MotorHandPro.git
   cd MotorHandPro

   # Create development branch
   git checkout -b feature/your-feature-name

   # Install dependencies
   pip install -r requirements.txt
   npm install  # If working on web components
   ```

### First-Time Contributors

Good first issues are tagged with `good-first-issue` and `help-wanted`. These are great entry points for new contributors.

**Suggested starting points:**
- Documentation improvements
- Unit tests for existing code
- Bug fixes with clear reproduction steps
- Validation of published results
- Performance benchmarking

## Types of Contributions

### 1. Bug Reports

**Before submitting:**
- Search existing issues to avoid duplicates
- Verify the bug in the latest version
- Gather reproduction steps and environment details

**Bug report template:**
```markdown
### Description
[Clear description of the bug]

### Reproduction Steps
1. Step one
2. Step two
3. ...

### Expected Behavior
[What should happen]

### Actual Behavior
[What actually happens]

### Environment
- OS: [e.g., Ubuntu 22.04]
- Python version: [e.g., 3.10.5]
- Hardware: [e.g., Arduino Uno, Raspberry Pi 4]

### Additional Context
[Logs, screenshots, data files]
```

### 2. Feature Requests

**Feature requests should:**
- Align with the Primal Logic framework philosophy
- Describe the use case and benefits
- Consider implications for patent protection
- Be discussed before implementation

**Feature request template:**
```markdown
### Feature Description
[Clear description of the proposed feature]

### Motivation
[Why is this feature needed?]

### Proposed Implementation
[How might this be implemented?]

### Alternatives Considered
[Other approaches you've thought about]

### Impact on Primal Logic Framework
[How does this relate to core concepts?]
```

### 3. Code Contributions

**Acceptable contributions:**
- Bug fixes
- Performance optimizations
- Additional test coverage
- Documentation improvements
- New validation examples
- Experimental features (in `extras/`)
- Integration examples
- Visualization improvements

**Requires discussion first:**
- Core algorithm changes
- New control laws
- API modifications
- Changes to fundamental constants
- Safety-critical functionality

### 4. Documentation Contributions

**Always welcome:**
- Fixing typos and grammar
- Clarifying explanations
- Adding examples and tutorials
- Improving API documentation
- Translating documentation
- Creating diagrams and visualizations

### 5. Validation and Testing

**Highly valuable:**
- Independent validation of results
- Cross-platform testing
- Hardware-in-the-loop testing
- Comparative studies vs. other control methods
- Stress testing and edge cases

## Development Process

### 1. Fork and Branch

```bash
# Fork the repository on GitHub
# Clone your fork
git clone https://github.com/YOUR-USERNAME/MotorHandPro.git
cd MotorHandPro

# Add upstream remote
git remote add upstream https://github.com/STLNFTART/MotorHandPro.git

# Create feature branch
git checkout -b feature/descriptive-name
```

### 2. Make Changes

- Write clean, readable code
- Follow existing code style
- Add comments for complex logic
- Update documentation as needed
- Add tests for new functionality

### 3. Test Thoroughly

```bash
# Run existing tests
python -m pytest tests/

# Run specific validation
python validate_vs_optimus.py

# Check code style
black --check .
flake8 .

# Verify stability conditions
python validate_algorithms.py
```

### 4. Commit

**Commit message format:**
```
<type>(<scope>): <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `test`: Adding or updating tests
- `refactor`: Code refactoring
- `perf`: Performance improvement
- `chore`: Maintenance tasks

**Example:**
```
feat(control): Add adaptive KE tuning based on error magnitude

Implements automatic adjustment of error gain (KE) based on real-time
error signal magnitude to improve transient response while maintaining
stability guarantees.

Validated against step response and sinusoidal tracking benchmarks.
Lipschitz condition verified for KE range [0.1, 0.8].

Closes #42
```

### 5. Push and Pull Request

```bash
# Push to your fork
git push origin feature/descriptive-name

# Create pull request on GitHub
# Fill out PR template with details
```

## Pull Request Guidelines

### PR Checklist

- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Comments added for complex code
- [ ] Documentation updated
- [ ] Tests added/updated and passing
- [ ] No new warnings or errors
- [ ] Stability conditions verified (F'(D) < 1.0)
- [ ] Patent considerations noted if applicable
- [ ] Commit messages are clear and descriptive

### PR Description Template

```markdown
## Description
[Clear description of changes]

## Motivation and Context
[Why is this change needed? What problem does it solve?]

## Type of Change
- [ ] Bug fix (non-breaking change fixing an issue)
- [ ] New feature (non-breaking change adding functionality)
- [ ] Breaking change (fix or feature causing existing functionality to change)
- [ ] Documentation update
- [ ] Performance improvement
- [ ] Refactoring

## Testing
- [ ] All existing tests pass
- [ ] New tests added and passing
- [ ] Manual testing completed
- [ ] Hardware tested (if applicable)

### Test Results
[Paste test output or link to validation results]

## Validation
- [ ] Stability conditions verified (F'(D) < 1.0)
- [ ] Benchmark comparison completed
- [ ] No regression in performance
- [ ] Cross-platform tested (if applicable)

## Screenshots (if applicable)
[Add visualizations, plots, or UI changes]

## Additional Notes
[Any additional information for reviewers]

## Checklist
- [ ] Code follows style guidelines
- [ ] Documentation updated
- [ ] Tests pass
- [ ] Commit messages are clear
- [ ] Branch is up to date with main
```

## Coding Standards

### Python Code

**Style:**
- Follow PEP 8
- Use `black` for formatting
- Type hints encouraged
- Maximum line length: 88 characters (black default)

**Example:**
```python
def compute_control_signal(
    psi: float, gamma: float, lambda_val: float = 0.16905, KE: float = 0.3
) -> float:
    """
    Compute Primal Logic control signal.

    Args:
        psi: Current control state
        gamma: Error signal
        lambda_val: Lightfoot constant (default: 0.16905)
        KE: Error gain (default: 0.3)

    Returns:
        Control signal update (dpsi/dt)

    Raises:
        ValueError: If stability condition violated
    """
    # Verify stability
    lipschitz = compute_lipschitz(lambda_val)
    if lipschitz >= 1.0:
        raise ValueError(f"Unstable: F'(D) = {lipschitz} >= 1.0")

    # Primal Logic control law
    return -lambda_val * psi + KE * gamma
```

### C/C++ Code (Arduino/Embedded)

**Style:**
- Follow Google C++ Style Guide (with Arduino adaptations)
- Descriptive variable names
- Constants in UPPER_CASE
- Functions in camelCase

**Example:**
```cpp
// Primal Logic constants
static constexpr double DONTE_CONSTANT = 149.9992314000;
static constexpr double LIGHTFOOT_LAMBDA = 0.16905;

// Compute control update
double computeControlUpdate(double psi, double gamma, double KE = 0.3) {
    // Exponential decay + error feedback
    return -LIGHTFOOT_LAMBDA * psi + KE * gamma;
}
```

### JavaScript/TypeScript (Web Interface)

**Style:**
- ES6+ syntax
- Use `const` and `let` (no `var`)
- Async/await over promises when possible
- Type annotations in TypeScript

**Example:**
```javascript
// Primal Logic visualization
class PrimalLogicViz {
    constructor(canvasId) {
        this.canvas = document.getElementById(canvasId);
        this.ctx = this.canvas.getContext('2d');
        this.lambda = 0.16905;
        this.KE = 0.3;
    }

    async updateState(psi, gamma, Ec) {
        // Verify stability
        const lipschitz = this.computeLipschitz();
        if (lipschitz >= 1.0) {
            console.warn(`Unstable: F'(D) = ${lipschitz}`);
        }

        // Render visualization
        this.renderPhasePlot(psi, gamma, Ec);
    }
}
```

## Documentation

### Code Comments

**When to comment:**
- Complex algorithms
- Non-obvious mathematical formulas
- Stability conditions and proofs
- Hardware-specific quirks
- Performance optimizations

**When NOT to comment:**
- Self-explanatory code
- Obvious operations

**Example:**
```python
# BAD: Obvious comment
x = x + 1  # Increment x

# GOOD: Explains WHY and context
# Exponential memory decay ensures bounded integration.
# This prevents integral windup while maintaining stability (F'(D) < 1.0).
# See PRIMAL_LOGIC_FRAMEWORK.md Theorem 3.2 for proof.
psi = psi * exp(-lambda_val * dt) + KE * error * dt
```

### README Updates

When adding features:
1. Update relevant README files
2. Add usage examples
3. Document new parameters
4. Update table of contents if needed

### API Documentation

Use docstrings for all public functions:
- Description
- Parameters with types
- Return values
- Exceptions
- Examples (optional but encouraged)

## Testing Requirements

### Unit Tests

**Required for:**
- All new functions
- Bug fixes
- Algorithm changes

**Example:**
```python
import pytest
from primal_logic import compute_control_signal, DONTE_CONSTANT

def test_control_signal_stability():
    """Verify control signal maintains stability conditions."""
    psi = 1.0
    gamma = 0.01
    lambda_val = 0.16905
    KE = 0.3

    dpsi = compute_control_signal(psi, gamma, lambda_val, KE)

    # Verify bounded response
    assert abs(dpsi) < 10.0, "Control signal unbounded"

def test_lipschitz_condition():
    """Verify Lipschitz constant < 1.0 for stability."""
    from primal_logic import compute_lipschitz

    lipschitz = compute_lipschitz(0.16905)
    assert lipschitz < 1.0, f"Unstable: F'(D) = {lipschitz}"
    assert abs(lipschitz - 0.000129931830) < 1e-10, "Lipschitz value incorrect"
```

### Integration Tests

For larger features:
- End-to-end simulation
- Hardware-in-the-loop (if applicable)
- Performance benchmarks

### Validation Tests

Compare against:
- Reference implementations
- Published results
- Theoretical predictions

## Licensing and IP Considerations

### Important Notes

1. **Patent Protection**
   - The Primal Logic framework is patent-pending
   - Contributions may be subject to patent rights
   - Commercial use requires licensing

2. **Contributor Agreement**
   By contributing, you agree that:
   - Your contributions may be included in future patent filings
   - Your contributions are provided under the project license
   - You have rights to contribute the code
   - You will be credited for your contributions

3. **Credit and Attribution**
   - All contributors will be acknowledged
   - Significant contributions may warrant co-authorship on papers
   - Contributor names maintained in git history

4. **Avoiding IP Conflicts**
   - Do not contribute code you don't have rights to
   - Do not contribute patented algorithms without disclosure
   - Flag any potential IP issues immediately

## Communication Channels

### GitHub Issues

Primary venue for:
- Bug reports
- Feature requests
- Technical discussions

### Pull Requests

For code contributions and reviews

### Discussions (if enabled)

For:
- Questions
- Ideas and brainstorming
- Showcasing uses
- General discussions

### Email

For:
- Licensing inquiries
- Collaboration proposals
- Private security issues
- IP-related questions

**Contact:** Donte Lightfoot (STLNFTART)

## Review Process

### Timeline

- Initial review: Within 7 days
- Feedback and iteration: Ongoing
- Final approval: When criteria met

### What Reviewers Check

1. **Correctness**
   - Code works as intended
   - Tests pass
   - No regressions

2. **Quality**
   - Follows coding standards
   - Well-documented
   - Maintainable

3. **Stability**
   - Maintains mathematical guarantees
   - No stability violations
   - Performance acceptable

4. **Safety**
   - No security vulnerabilities
   - Safe for intended use cases
   - Appropriate warnings

## Recognition

### Contributors

All contributors will be recognized in:
- Repository README
- Release notes
- Academic publications (for significant contributions)

### Types of Recognition

- Code contributions: Git commit history
- Documentation: Attribution in docs
- Research validation: Co-authorship consideration
- Bug reports: Credit in changelog

## Questions?

If you have questions not covered here:
1. Check existing issues and discussions
2. Review documentation in /docs
3. Open a new discussion or issue
4. Contact maintainers directly for sensitive matters

Thank you for contributing to MotorHandPro!

---

**Last Updated:** November 30, 2025
**Version:** 1.0

© 2025 Donte Lightfoot — The Phoney Express LLC / Locked In Safety
