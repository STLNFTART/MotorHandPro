#!/usr/bin/env python3
"""
Generate Primal Logic kernel test data for KE=0.5
Fixes missing run_ke05.csv file
"""

import numpy as np

# Primal Logic constants
MU = 0.16905  # Lightfoot constant
KE = 0.5      # Error gain (the missing parameter combination!)
D0 = 149.9992314000
I3 = 6.4939394023
S = 23.0983417165
FPRIME_D0 = 0.000129931830

# Simulation parameters
dt = 0.01  # Time step
t_end = 5.0  # End time
n_steps = int(t_end / dt) + 1

# Initial conditions
psi = 1.0  # Initial control signal
gamma = 0.004  # Initial error (small perturbation)
Ec = 0.0  # Initial control energy

# Arrays to store results
t_vals = np.linspace(0, t_end, n_steps)
psi_vals = np.zeros(n_steps)
gamma_vals = np.zeros(n_steps)
Ec_vals = np.zeros(n_steps)

# Set initial values
psi_vals[0] = psi
gamma_vals[0] = gamma
Ec_vals[0] = Ec

print(f"Generating Primal Logic kernel test with KE={KE}")
print(f"Parameters: MU={MU}, KE={KE}, D0={D0}")

# Integrate using Primal Logic control law
for i in range(1, n_steps):
    # Primal Logic control law: dψ/dt = -λ·ψ(t) + KE·e(t)
    # For kernel test, error signal decays
    dpsi_dt = -MU * psi + KE * gamma
    dgamma_dt = -0.5 * gamma  # Error decay

    # Update states
    psi = psi + dpsi_dt * dt
    gamma = gamma + dgamma_dt * dt
    Ec = Ec + psi * gamma * dt

    # Store values
    psi_vals[i] = psi
    gamma_vals[i] = gamma
    Ec_vals[i] = Ec

# Write to CSV
output_file = "run_ke05.csv"
with open(output_file, 'w') as f:
    # Write header
    f.write(f"# MU={MU:.5f} KE={KE:.5f}\n")
    f.write(f"# Core: D0={D0:.10f} I3={I3:.10f} S={S:.10f} F'(D0)={FPRIME_D0:.12f}\n")
    f.write("# t,psi,gamma,Ec\n")

    # Write data
    for i in range(n_steps):
        f.write(f"{t_vals[i]:.2f},{psi_vals[i]:.10f},{gamma_vals[i]:.10f},{Ec_vals[i]:.10f}\n")

print(f"✅ Generated {output_file} with {n_steps} data points")
print(f"   Max psi: {np.max(psi_vals):.6f}")
print(f"   Min Ec: {np.min(Ec_vals):.6f}")
print(f"   Max Ec: {np.max(Ec_vals):.6f}")
