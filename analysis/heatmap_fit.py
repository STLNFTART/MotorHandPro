#!/usr/bin/env python3
import csv
import glob
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Base paths
BASE = Path(__file__).resolve().parent.parent
SUMMARY_PATH = BASE / "summary.csv"
OUT_DIR = BASE / "analysis"
OUT_DIR.mkdir(exist_ok=True)

# 1. Load summary.csv
if not SUMMARY_PATH.exists():
    print("summary.csv not found. Run analyze_runs.py first.")
    raise SystemExit(1)

df = pd.read_csv(SUMMARY_PATH)

# Keep only rows with needed fields
df = df.dropna(subset=["MU", "KE", "t_zero_cross", "L_Ec_estimate"])
if df.empty:
    print("No valid rows in summary.csv for heatmaps.")
    raise SystemExit(0)

# 2. Pivot tables for heatmaps
pivot_tzero = df.pivot_table(values="t_zero_cross", index="MU", columns="KE")
pivot_lec = df.pivot_table(values="L_Ec_estimate", index="MU", columns="KE")


def plot_heatmap(pivot, title, fname):
    if pivot.empty:
        print(f"Skipping {title} heatmap, no data.")
        return
    plt.figure(figsize=(6, 5))
    plt.title(title)
    # Basic imshow without custom colormap
    plt.imshow(
        pivot.values,
        origin="lower",
        aspect="auto",
        extent=[
            float(pivot.columns.min()),
            float(pivot.columns.max()),
            float(pivot.index.min()),
            float(pivot.index.max()),
        ],
    )
    plt.colorbar(label=title)
    plt.xlabel("KE")
    plt.ylabel("MU")
    plt.tight_layout()
    plt.savefig(OUT_DIR / fname, dpi=200)
    plt.close()


plot_heatmap(pivot_tzero, "t_zero_cross", "heatmap_tzero.png")
plot_heatmap(pivot_lec, "L_Ec_estimate", "heatmap_Lec.png")

# 3. Lambda-fit without SciPy
#    Fit Ec(t) ~ -E0 * exp(-lambda * t) on tail where Ec < 0
#    Use linear regression on log(|Ec|): log|Ec| = log(E0) - lambda * t

def load_run_data(path):
    ts, ecs = [], []
    with open(path, "r") as f:
        reader = csv.reader(f)
        for row in reader:
            if not row or row[0].startswith("#"):
                continue
            # try to detect header
            if row[0].lower() in ("t", "time"):
                continue
            try:
                t = float(row[0])
                # if only t,psi,gamma present, skip
                if len(row) < 4:
                    continue
                Ec = float(row[3])
                ts.append(t)
                ecs.append(Ec)
            except Exception:
                continue
    return np.array(ts), np.array(ecs)


lambda_rows = []

for runfile in sorted(glob.glob(str(BASE / "run_*.csv"))):
    ts, ecs = load_run_data(runfile)
    if ts.size == 0:
        continue

    # select tail where Ec < 0
    mask = ecs < 0
    tail_t = ts[mask]
    tail_ec = ecs[mask]

    if tail_t.size < 5:
        continue

    # use absolute value for log, avoid zeros
    y = np.abs(tail_ec)
    y = y[y > 0]
    if y.size < 5:
        continue

    t_used = tail_t[: y.size]
    logy = np.log(y)

    # simple linear fit: logy = a + b t, lambda = -b
    try:
        b, a = np.polyfit(t_used, logy, 1)
        lam = -b
        if lam > 0:
            lambda_rows.append(
                {
                    "file": Path(runfile).name,
                    "lambda_fit": float(lam),
                }
            )
    except Exception:
        continue

# Save lambda fits
lambda_path = OUT_DIR / "lambda_fit.csv"
with open(lambda_path, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=["file", "lambda_fit"])
    writer.writeheader()
    for r in lambda_rows:
        writer.writerow(r)

print("Generated heatmap_tzero.png, heatmap_Lec.png, lambda_fit.csv")
