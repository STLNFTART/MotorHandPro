# SDACS / Primal Logic Benchmarks

This repo contains a minimal, reproducible benchmark of the SDACS / Primal Logic
controller against a proportional (Tesla-style) baseline for an Optimus-style actuator.

## Setup

- Environment: 1-DOF longitudinal actuator model (Optimus hand joint analogue)
- Integration: 1 kHz loop, 1.0 s horizon
- Control laws:
  - Baseline: u(t) = -K * e(t)
  - SDACS / Primal Logic: exponentially weighted integral with adaptive modulation (see SDACS.pdf)
- Parameters:
  - K = 0.5
  - λ = 0.115 s^-1
  - Modulation Θ(t) as specified in SDACS.pdf
- Inputs:
  - Step + noisy perturbations to mimic sensor noise / shock
  - Same initial conditions and disturbance sequence for both controllers

## Files

- `run_default.csv`, `run_mu015_ke03.csv`, `run_mu02.csv`:
  Time series of ψ(t), γ(t), and Ec for different (μ, KE) settings.
- `run_*_plot.png`:
  Plots of command vs time and energy-like Ec(t).
- `summary.csv`:
  Aggregated metrics per run:
  - max_psi       : peak control effort / excursion
  - t_zero        : time-to-error-crossing (settling proxy)
  - L_Ec          : integral of Ec(t) (actuation energy proxy)

## Observed effects (current data)

Across the provided runs, SDACS-style weighting:
- Reduces overshoot and oscillation vs. proportional baseline.
- Lowers integrated Ec (L_Ec) by ≈15–25% in the tuned configurations,
  indicating reduced actuation energy and smoother trajectories.
- Keeps effective jerk bounded via exponential decay, consistent with SDACS.pdf.

These are first-pass actuator-level results. Full-vehicle validation (CARLA/HIL/track)
would reuse the same control law and metrics.
