#!/usr/bin/env python3
import glob
import csv
import math
import statistics
from pathlib import Path

try:
    import matplotlib.pyplot as plt
    HAVE_MPL = True
except ImportError:
    HAVE_MPL = False

DATA_GLOB = "run_*.csv"
OUT_SUMMARY = "summary.csv"


def parse_header_value(line, key):
    if key + "=" not in line:
        return None
    try:
        after = line.split(key + "=")[1]
        val = ""
        for ch in after:
            if ch in "0123456789+-.eE":
                val += ch
            else:
                break
        return float(val) if val else None
    except Exception:
        return None


def parse_core_line(line):
    vals = {}
    for key in ["D0", "I3", "S"]:
        v = parse_header_value(line, key)
        if v is not None:
            vals[key] = v

    if "F'(D0)=" in line:
        try:
            after = line.split("F'(D0)=")[1]
            num = ""
            for ch in after:
                if ch in "0123456789+-.eE":
                    num += ch
                else:
                    break
            vals["Fprime_D0"] = float(num) if num else None
        except Exception:
            vals["Fprime_D0"] = None

    return vals


def load_run(path):
    meta = {
        "MU": None,
        "KE": None,
        "D0": None,
        "I3": None,
        "S": None,
        "Fprime_D0": None,
    }
    data = []

    with open(path, "r") as f:
        reader = csv.reader(f)
        for row in reader:
            if not row:
                continue

            if row[0].startswith("#"):
                line = ",".join(row)
                if "MU=" in line:
                    mu = parse_header_value(line, "MU")
                    if mu is not None:
                        meta["MU"] = mu
                if "KE=" in line:
                    ke = parse_header_value(line, "KE")
                    if ke is not None:
                        meta["KE"] = ke
                if "Core:" in line:
                    core_vals = parse_core_line(line)
                    meta.update(core_vals)
                continue

            if row[0].lower() in ("t", "time"):
                continue

            try:
                t = float(row[0])
                psi = float(row[1])
                gamma = float(row[2]) if len(row) > 2 else float("nan")
                Ec = float(row[3]) if len(row) > 3 else float("nan")
                data.append((t, psi, gamma, Ec))
            except Exception:
                continue

    return meta, data


def find_zero_cross_time(data):
    for (t0, _, _, e0), (t1, _, _, e1) in zip(data, data[1:]):
        if e0 >= 0.0 and e1 < 0.0:
            if e1 == e0:
                return t0
            alpha = e0 / (e0 - e1)
            return t0 + alpha * (t1 - t0)
    return None


def linear_fit_slope(xs, ys):
    if len(xs) < 2:
        return None
    x_mean = statistics.fmean(xs)
    y_mean = statistics.fmean(ys)
    num = sum((x - x_mean) * (y - y_mean) for x, y in zip(xs, ys))
    den = sum((x - x_mean) ** 2 for x in xs)
    if den == 0:
        return None
    return num / den


def compute_metrics(meta, data):
    if not data:
        return {}

    ts = [r[0] for r in data]
    psis = [r[1] for r in data]
    gammas = [r[2] for r in data]
    ecs = [r[3] for r in data]

    if not psis:
        return {}

    max_psi = max(psis)
    max_gamma = max(gammas) if gammas else None
    max_ec = max(ecs) if ecs else None
    min_ec = min(ecs) if ecs else None

    t_zero = find_zero_cross_time(data)

    if t_zero is not None:
        tail = [(t, e) for (t, _, _, e) in data if t >= t_zero]
        if len(tail) >= 3:
            xs = [t for t, e in tail]
            ys = [e for t, e in tail]
            damping_slope = linear_fit_slope(xs, ys)
        else:
            damping_slope = None
    else:
        damping_slope = None

    max_delta_ec = 0.0
    for (t0, _, _, e0), (t1, _, _, e1) in zip(data, data[1:]):
        dt = t1 - t0
        if dt <= 0:
            continue
        delta = abs(e1 - e0) / dt
        if delta > max_delta_ec:
            max_delta_ec = delta

    max_abs_ec = max((abs(e) for e in ecs), default=0.0) or 1e-12
    L_est = max_delta_ec / max_abs_ec

    out = dict(meta)
    out.update(
        {
            "max_psi": max_psi,
            "max_gamma": max_gamma,
            "max_Ec": max_ec,
            "min_Ec": min_ec,
            "t_zero_cross": t_zero,
            "damping_slope": damping_slope,
            "L_Ec_estimate": L_est,
        }
    )
    return out


def plot_run(path, data, t_zero=None):
    if not HAVE_MPL or not data:
        return

    ts = [r[0] for r in data]
    psis = [r[1] for r in data]
    gammas = [r[2] for r in data]
    ecs = [r[3] for r in data]

    base = Path(path).stem
    png = f"{base}_plot.png"

    plt.figure(figsize=(6, 8))

    plt.subplot(3, 1, 1)
    plt.plot(ts, psis)
    if t_zero is not None:
        plt.axvline(t_zero, linestyle="--")
    plt.ylabel("psi")

    plt.subplot(3, 1, 2)
    plt.plot(ts, gammas)
    if t_zero is not None:
        plt.axvline(t_zero, linestyle="--")
    plt.ylabel("gamma")

    plt.subplot(3, 1, 3)
    plt.plot(ts, ecs)
    if t_zero is not None:
        plt.axvline(t_zero, linestyle="--")
    plt.xlabel("time")
    plt.ylabel("Ec")

    plt.tight_layout()
    plt.savefig(png, dpi=200)
    plt.close()


def main():
    files = sorted(glob.glob(DATA_GLOB))
    if not files:
        print("No run_*.csv files found")
        return

    fieldnames = [
        "file",
        "MU",
        "KE",
        "D0",
        "I3",
        "S",
        "Fprime_D0",
        "max_psi",
        "max_gamma",
        "max_Ec",
        "min_Ec",
        "t_zero_cross",
        "damping_slope",
        "L_Ec_estimate",
    ]

    rows = []

    for path in files:
        meta, data = load_run(path)
        metrics = compute_metrics(meta, data)
        metrics["file"] = Path(path).name

        if not data or metrics.get("max_psi") is None:
            print(f"{metrics['file']}: no valid data, skipped")
            continue

        rows.append(metrics)

        print(
            f"{metrics['file']}: "
            f"MU={metrics.get('MU')}, "
            f"KE={metrics.get('KE')}, "
            f"max_psi={metrics.get('max_psi'):.6f} "
            f"t_zero={metrics.get('t_zero_cross')} "
            f"L_Ec≈{metrics.get('L_Ec_estimate'):.3e}"
        )

        plot_run(path, data, metrics.get("t_zero_cross"))

    with open(OUT_SUMMARY, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        for r in rows:
            writer.writerow({k: r.get(k, "") for k in fieldnames})

    print(f"\nSummary written to {OUT_SUMMARY}")
    if HAVE_MPL:
        print("Per-run plots written as run_XXX_plot.png")


if __name__ == "__main__":
    main()
