# Generated Constants (gen/)

Precomputed Primal Logic constants and kernel parameters for embedded systems and high-performance applications.

## Overview

This directory contains C/C++ header files with compile-time constants derived from the Primal Logic mathematical framework. These files eliminate runtime computation overhead by providing pre-calculated values for embedded controllers, real-time systems, and performance-critical applications.

## Files

### quant_full.h

**Purpose:** Complete Primal Logic kernel implementation with runtime computation capabilities.

**Contents:**
- Full namespace `QUANT` with all fundamental constants
- `planckTail(X)` - Planck tail series computation
- `solveCutoffXc()` - Cutoff threshold solver
- Helper functions for kernel iteration
- Numerical solvers with configurable tolerance

**Usage:**
```cpp
#include "gen/quant_full.h"

// Access precomputed constants
double D = QUANT::DONTE_CONSTANT;  // 149.9992314000
double mu = QUANT::KERNEL_MU;      // 0.169050000000
double S = QUANT::PLANCK_SCALE;    // 23.098341716530

// Or compute at runtime if needed
double Xc = QUANT::solveCutoffXc();
double tail = QUANT::planckTail(19.358674138784);
```

**Key Constants:**

| Constant | Value | Description |
|----------|-------|-------------|
| `PLANCK_D` | 149.9992314000 | Donte constant (fixed point) |
| `PLANCK_I3` | 6.4939394023 | π⁴/15 normalization |
| `PLANCK_SCALE` | 23.098341716530 | S = D/I3 |
| `KERNEL_MU` | 0.169050000000 | Lightfoot constant (λ) |
| `CUTOFF_XC` | 19.358674138784 | Planck tail cutoff |
| `F_PRIME_D_CONST` | 0.000129931830 | Lipschitz constant |

**Compilation:**
- Requires C++11 or later
- Arduino-compatible (`#include <Arduino.h>`)
- Can be used in desktop applications (remove Arduino.h dependency)

---

### quant_bridge.h

**Purpose:** Lightweight constant-only header for maximum portability and minimal overhead.

**Contents:**
- Static `constexpr` constants only
- No function definitions
- No external dependencies (Arduino-independent)
- Derived convenience constants

**Usage:**
```cpp
#include "gen/quant_bridge.h"

// Immediate access to constants at compile time
constexpr double lambda = KERNEL_MU;
constexpr double stability = F_PRIME_D;
constexpr double energy_scale = ENERGY_SCALE;
```

**Additional Derived Constants:**

| Constant | Formula | Purpose |
|----------|---------|---------|
| `LAMBDA_FACTOR` | μ × S | Combined decay-scale factor |
| `ENERGY_SCALE` | D × μ | Energy normalization |
| `KERNEL_RESPONSE` | c × F'(D) | System responsiveness |
| `STABILITY_INDEX` | Complex ratio | Stability metric |
| `NORMALIZED_SUM` | S + Xc + D | System balance check |

**Compilation:**
- Pure C++ constants
- No runtime overhead
- Header-only (no .cpp needed)
- Suitable for constexpr evaluation

---

## Generation Process

These constants are derived through:

1. **Planck Tail Series Computation**
   ```
   I3 = Σ(n=1→∞) e^(-nx) * [polynomial terms] / n⁴
   ```
   - Computed in `quant_runtime.h` or external Python scripts
   - Converged to 12+ decimal places

2. **Fixed-Point Iteration**
   ```
   D = lim(n→∞) F^n(x0)
   where F(x) = c·μ·e^(-μx), c = (150-x)·e^(μx)
   ```
   - Iterative solver in `quant_full.h::solveFixedPoint()`
   - Tolerance: 1e-12

3. **Cutoff Threshold Solution**
   ```
   Solve: planckTail(Xc) / I3 = ε_δ
   ```
   - Binary search or Newton's method
   - Target ε_δ = 0.000005124

4. **Lipschitz Constant**
   ```
   F'(D) = c·μ·e^(-μD)
   ```
   - Evaluated at fixed point D
   - Proves contraction (F'(D) < 1)

## Use Cases

### Embedded Systems (Arduino, ARM)

```cpp
#include "gen/quant_bridge.h"

void setup() {
  Serial.begin(115200);
  Serial.print("D = "); Serial.println(DONTE_CONSTANT, 10);
  Serial.print("μ = "); Serial.println(KERNEL_MU, 12);
  Serial.print("F'(D) = "); Serial.println(F_PRIME_D, 12);
}

double compute_control(double error) {
  // Use constants in control law
  return KERNEL_MU * error + LAMBDA_FACTOR * state;
}
```

**Benefits:**
- Zero runtime computation cost
- Deterministic timing (no iterative solvers)
- Minimal flash/RAM usage

### Real-Time Systems

```cpp
#include "gen/quant_full.h"

// Precomputed constants for fast execution
constexpr double MU = QUANT::KERNEL_MU;
constexpr double D = QUANT::DONTE_CONSTANT;

// Critical control loop (< 1 ms deadline)
void control_loop() {
  double psi = /* read state */;
  double gamma = /* compute error */;

  // Direct constant usage (no function calls)
  double cmd = -MU * psi + KE * gamma;

  /* actuate */
}
```

### Scientific Validation

```cpp
#include "gen/quant_full.h"
#include <iostream>

int main() {
  // Verify constants against theoretical predictions
  double computed_Xc = QUANT::solveCutoffXc();
  double precomputed_Xc = QUANT::CUTOFF_XC;

  double error = std::abs(computed_Xc - precomputed_Xc);

  std::cout << "Error: " << error << std::endl;
  if (error < 1e-10) {
    std::cout << "Validation PASSED" << std::endl;
  }

  return 0;
}
```

## Updating Constants

If the underlying mathematical framework changes:

1. **Recompute in Python:**
   ```bash
   python scripts/compute_primal_constants.py > new_constants.txt
   ```

2. **Update header files:**
   ```bash
   # Edit quant_bridge.h and quant_full.h with new values
   vim gen/quant_bridge.h
   vim gen/quant_full.h
   ```

3. **Validate:**
   ```bash
   # Compile and run validation
   g++ -std=c++17 -o validate validate_constants.cpp
   ./validate
   ```

4. **Propagate:**
   ```bash
   # Copy to deployment locations
   cp gen/quant_bridge.h arduino_projects/MotorHandPro/
   cp gen/quant_full.h embedded_systems/
   ```

## Precision Notes

**Decimal Places:**
- Most constants: 10-12 significant figures
- Critical constants (D, μ): 12+ decimal places
- Derived ratios: Limited by component precision

**Floating-Point Considerations:**
- `double` (64-bit): ~15-17 decimal digits precision
- `float` (32-bit): ~6-7 decimal digits precision
- Use `double` for all Primal Logic computations

**Verification:**
```cpp
static_assert(PLANCK_D > 149.0 && PLANCK_D < 150.0, "D sanity check");
static_assert(F_PRIME_D_CONST < 1.0, "Contraction condition");
static_assert(KERNEL_MU > 0.0, "Positive decay rate");
```

## Related Files

**In Repository Root:**
- `quant_full.h` - Master copy with full implementation
- `quant_runtime.h` - Lightweight runtime version

**In Documentation:**
- [Primal Logic Framework](../PRIMAL_LOGIC_FRAMEWORK.md) - Mathematical derivations
- [Empirical Constants Database](../EMPIRICAL_CONSTANTS_DATABASE.md) - Constant definitions
- [AGP Physics Clarification](../AGP_PHYSICS_CLARIFICATION.md) - Physical interpretation

**In Arduino:**
- `MotorHandPro.ino` - Hardware implementation example

## Testing

To verify constant correctness:

```bash
# Run embedded validation
cd gen
g++ -std=c++17 -DTEST_MODE -o test_constants quant_full.h -lm
./test_constants
```

Expected output:
```
D = 149.9992314000 ✓
I3 = 6.4939394023 ✓
μ = 0.169050000000 ✓
F'(D) = 0.000129931830 < 1.0 ✓
Planck tail convergence: PASS ✓
Fixed point stability: PASS ✓
```

## Performance

**Compile-time constants (`quant_bridge.h`):**
- Zero runtime cost
- Fully inlined by optimizer
- Single instruction access

**Runtime functions (`quant_full.h`):**
- `planckTail(X)`: ~1-10 ms (depends on X, convergence)
- `solveCutoffXc()`: ~10-100 ms (binary search)
- Use only for validation, not real-time control

---

**Patent Pending:** U.S. Provisional Patent Application No. 63/842,846
