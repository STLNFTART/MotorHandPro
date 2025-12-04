/**
 * Temporal Displacement Field Implementation in D
 *
 * High-performance implementation of temporal displacement for real-time systems.
 *
 * Author: Donte Lightfoot
 * Date: September 20, 2025
 * Patent Pending: U.S. Provisional Application No. 63/842,846
 */

import std.math;
import std.stdio;
import std.algorithm;
import std.range;

/**
 * Displaced, leaky field with fractional delay (order-1 interpolation)
 */
struct TemporalDisplacedField {
    double alpha;       // Field strength coefficient
    double beta;        // Decay rate (leaky integrator)
    double kappa;       // Disturbance coupling
    double E;           // Current field state

    double[] E0buf;     // Ring buffer of E0 history (newest at back)
    size_t N;           // Buffer length
    size_t idx;         // Write index

    /**
     * Initialize temporal displaced field.
     *
     * Params:
     *   history = Number of samples to keep in history buffer
     *   alpha = Field strength coefficient
     *   beta = Decay rate (must be > 0 for stability)
     *   kappa = Disturbance coupling coefficient
     */
    this(size_t history, double alpha, double beta, double kappa) {
        this.N = history;
        this.alpha = alpha;
        this.beta = beta;
        this.kappa = kappa;
        this.E = 0.0;
        this.idx = 0;
        E0buf.length = N;
        E0buf[] = 0.0;  // Initialize to zero
    }

    /**
     * Update field with new sample and displacement.
     *
     * Implements: E = (1-β)*E + α*E0(t-Δ) - κ*d
     *
     * Params:
     *   E0_now = Current driver field value
     *   Delta = Temporal displacement in samples (can be fractional)
     *   d = Disturbance value
     *
     * Returns:
     *   Updated field value E
     */
    double step(double E0_now, double Delta, double d) @safe {
        // Write E0 to ring buffer
        E0buf[idx] = E0_now;

        // Enforce causality for online systems
        if (Delta < 0.0) Delta = 0.0;

        // Fractional delay using linear interpolation
        // Delta = m + frac, where m is integer part, frac is fractional part
        auto m = cast(size_t) Delta;
        double frac = Delta - m;

        // Clamp m to valid range
        if (m >= N) m = N - 1;

        // Get indices for interpolation (wrapping around ring buffer)
        size_t i0 = (idx + N - m) % N;
        size_t i1 = (idx + N - m - 1) % N;

        // Linear interpolation: E0(t-Δ) ≈ (1-frac)*E0[i0] + frac*E0[i1]
        double E0_delayed = (1.0 - frac) * E0buf[i0] + frac * E0buf[i1];

        // Leaky integrator with displacement and disturbance
        // E(t+dt) = (1-β)*E(t) + α*E0(t-Δ) - κ*d(t)
        E = (1.0 - beta) * E + alpha * E0_delayed - kappa * d;

        // Advance write index
        idx = (idx + 1) % N;

        return E;
    }

    /**
     * Reset field to initial state.
     */
    void reset() @safe nothrow {
        E = 0.0;
        E0buf[] = 0.0;
        idx = 0;
    }

    /**
     * Get current field value (read-only).
     */
    double getCurrentValue() const @safe nothrow @nogc {
        return E;
    }
}


/**
 * Memory Kernel Field with Exponential Kernel
 *
 * E(t) = ∫ K(t-τ-Δ(τ)) [α*E0(τ) - κ*d(τ)] dτ
 * where K(s) = λ*exp(-λ*s) for s >= 0
 */
struct MemoryKernelField {
    double alpha;
    double beta;        // Not used directly, but kept for consistency
    double kappa;
    double lambda;      // Kernel decay rate

    double E;

    struct HistorySample {
        double time;
        double E0;
        double d;
        double Delta;
    }

    HistorySample[] history;
    size_t maxHistory;

    this(size_t maxHistory, double alpha, double lambda, double kappa) {
        this.maxHistory = maxHistory;
        this.alpha = alpha;
        this.lambda = lambda;
        this.kappa = kappa;
        this.beta = 0.0;  // Unused for kernel method
        this.E = 0.0;
    }

    /**
     * Exponential memory kernel.
     */
    double kernel(double s) const @safe nothrow @nogc {
        if (s < 0.0) return 0.0;  // Causality
        return lambda * exp(-lambda * s);
    }

    /**
     * Update field using kernel integration.
     */
    double step(double t, double E0_now, double Delta, double d, double dt) @safe {
        // Store current sample
        history ~= HistorySample(t, E0_now, d, Delta);

        // Trim history if too long
        if (history.length > maxHistory) {
            history = history[$-maxHistory .. $];
        }

        // Integrate: E(t) = ∫ K(t-τ-Δ(τ)) [α*E0(τ) - κ*d(τ)] dτ
        double integral = 0.0;

        foreach (sample; history) {
            // Kernel argument: s = t - τ - Δ(τ)
            double s = t - sample.time - sample.Delta;

            // Causality check
            if (s < 0.0) continue;

            // Kernel value
            double K_s = kernel(s);

            // Integrand: K(s) * [α*E0(τ) - κ*d(τ)]
            double integrand = K_s * (alpha * sample.E0 - kappa * sample.d);

            // Accumulate (rectangular integration)
            integral += integrand * dt;
        }

        E = integral;
        return E;
    }

    void reset() @safe nothrow {
        E = 0.0;
        history.length = 0;
    }
}


/**
 * Trust-Gated Displacement
 *
 * Δ(t) = Δ₀ + Δ_trust * (1 - confidence(t))
 */
struct TrustGatedDisplacement {
    double Delta_0;      // Baseline displacement
    double Delta_trust;  // Trust sensitivity

    this(double Delta_0, double Delta_trust) {
        this.Delta_0 = Delta_0;
        this.Delta_trust = Delta_trust;
    }

    double computeDisplacement(double confidence) const @safe nothrow @nogc {
        // Clamp confidence to [0, 1]
        if (confidence < 0.0) confidence = 0.0;
        if (confidence > 1.0) confidence = 1.0;

        return Delta_0 + Delta_trust * (1.0 - confidence);
    }
}


/**
 * Load Shedding Displacement
 *
 * Increase displacement under high load for graceful degradation.
 */
struct LoadSheddingDisplacement {
    double Delta_base;      // Baseline displacement (low load)
    double Delta_max;       // Maximum displacement (high load)
    double load_threshold;  // Load threshold to start increasing Delta

    this(double Delta_base, double Delta_max, double load_threshold) {
        this.Delta_base = Delta_base;
        this.Delta_max = Delta_max;
        this.load_threshold = load_threshold;
    }

    double computeDisplacement(double load) const @safe nothrow @nogc {
        // Clamp load to [0, 1]
        if (load < 0.0) load = 0.0;
        if (load > 1.0) load = 1.0;

        if (load < load_threshold) {
            return Delta_base;
        }

        // Linear ramp from Delta_base to Delta_max
        double excess_load = (load - load_threshold) / (1.0 - load_threshold);
        return Delta_base + excess_load * (Delta_max - Delta_base);
    }
}


/**
 * Validation and Testing
 */

/**
 * Test causality: verify all Delta values are non-negative.
 */
bool validateCausality(const double[] Delta_history) @safe nothrow @nogc {
    foreach (Delta; Delta_history) {
        if (Delta < 0.0) return false;
    }
    return true;
}


/**
 * Verify stability by running simulation.
 */
struct StabilityResults {
    bool bounded;
    bool converges;
    double finalValue;
    double maxValue;
}

StabilityResults verifyStability(
    TemporalDisplacedField* field,
    double duration,
    double dt,
    double function(double) @safe E0_func = null
) @safe {
    // Default: step input
    if (E0_func is null) {
        E0_func = (double t) @safe => (t > 1.0) ? 1.0 : 0.0;
    }

    field.reset();

    double maxAbsValue = 0.0;
    double[] recentValues;
    const size_t tailSamples = 100;

    for (double t = 0.0; t < duration; t += dt) {
        double E0 = E0_func(t);
        double Delta = 0.1;  // Fixed displacement for test
        double d = 0.0;

        double E = field.step(E0, Delta, d);

        // Track max
        double absE = abs(E);
        if (absE > maxAbsValue) {
            maxAbsValue = absE;
        }

        // Store recent values for convergence check
        recentValues ~= E;
        if (recentValues.length > tailSamples) {
            recentValues = recentValues[$ - tailSamples .. $];
        }
    }

    // Check if bounded
    bool bounded = maxAbsValue < 1e6;

    // Check convergence: std dev of last 100 samples < 0.01
    bool converges = false;
    if (recentValues.length >= tailSamples) {
        double mean = recentValues.sum / recentValues.length;
        double variance = 0.0;
        foreach (val; recentValues) {
            double diff = val - mean;
            variance += diff * diff;
        }
        variance /= recentValues.length;
        double stdDev = sqrt(variance);
        converges = stdDev < 0.01;
    }

    return StabilityResults(
        bounded,
        converges,
        field.E,
        maxAbsValue
    );
}


/**
 * Example usage and testing
 */
void main() {
    writeln("=== Temporal Displacement Field Demo ===\n");

    // Configuration
    const size_t history = 1000;
    const double alpha = 1.0;
    const double beta = 0.1;
    const double kappa = 0.1;

    // Create field
    auto field = TemporalDisplacedField(history, alpha, beta, kappa);

    writeln("Configuration:");
    writefln("  α (alpha)  = %.2f", alpha);
    writefln("  β (beta)   = %.2f", beta);
    writefln("  κ (kappa)  = %.2f", kappa);
    writefln("  History    = %d samples\n", history);

    // Test 1: Step response with fixed displacement
    writeln("Test 1: Step Response (Δ = 0.1 samples)");
    field.reset();

    const double dt = 0.01;
    const double duration = 10.0;
    double Delta = 10.0;  // 10 samples = 0.1 seconds at 100 Hz

    for (double t = 0.0; t < duration; t += dt) {
        double E0 = (t > 1.0) ? 1.0 : 0.0;  // Step input at t=1s
        double d = 0.0;

        double E = field.step(E0, Delta, d);

        // Print key milestones
        if (abs(t - 1.0) < dt/2 || abs(t - 2.0) < dt/2 || abs(t - 5.0) < dt/2) {
            writefln("  t=%.2f: E0=%.2f, E=%.6f", t, E0, E);
        }
    }
    writefln("  Final: E=%.6f\n", field.E);

    // Test 2: Stability verification
    writeln("Test 2: Stability Verification");
    auto stability = verifyStability(&field, 100.0, 0.01);
    writefln("  Bounded:    %s", stability.bounded ? "✓" : "✗");
    writefln("  Converges:  %s", stability.converges ? "✓" : "✗");
    writefln("  Final:      %.6f", stability.finalValue);
    writefln("  Max |E|:    %.6f\n", stability.maxValue);

    // Test 3: Trust-gated displacement
    writeln("Test 3: Trust-Gated Displacement");
    auto trustGate = TrustGatedDisplacement(0.0, 2.0);

    foreach (conf; [1.0, 0.8, 0.5, 0.2, 0.0]) {
        double Delta_trust = trustGate.computeDisplacement(conf);
        writefln("  Confidence=%.1f → Δ=%.2f", conf, Delta_trust);
    }
    writeln();

    // Test 4: Load shedding
    writeln("Test 4: Load Shedding Displacement");
    auto loadShed = LoadSheddingDisplacement(0.0, 5.0, 0.8);

    foreach (load; [0.0, 0.5, 0.8, 0.9, 1.0]) {
        double Delta_load = loadShed.computeDisplacement(load);
        writefln("  Load=%.1f → Δ=%.2f", load, Delta_load);
    }
    writeln();

    // Test 5: Fractional delay accuracy
    writeln("Test 5: Fractional Delay Interpolation");
    field.reset();

    // Fill buffer with known pattern
    for (size_t i = 0; i < 20; i++) {
        field.step(cast(double)i, 0.0, 0.0);
    }

    // Test fractional delays
    foreach (Delta_frac; [0.0, 0.5, 1.0, 1.5, 2.0]) {
        field.idx = 19;  // Reset read position
        double E = field.step(19.0, Delta_frac, 0.0);
        writefln("  Δ=%.1f → E0(t-Δ)=%.2f", Delta_frac, E/(alpha*(1-beta)));
    }

    writeln("\n=== All Tests Complete ===");
}
