#!/usr/bin/env bash
set -e

SIM_FILE="$HOME/primal/swarm_sim.py"

if [ ! -f "$SIM_FILE" ]; then
  echo "[!] swarm_sim.py not found — creating new file..."
fi

cat > "$SIM_FILE" <<'PY'
#!/usr/bin/env python3
import json, hashlib, time, random, requests

# =====================================================
# Gateway configuration
# =====================================================
GW = "http://127.0.0.1:9000/proof"   # Local gateway endpoint
HEADERS = {"Content-Type": "application/json"}

# =====================================================
# Helper: post to gateway
# =====================================================
def post_position(lat, lon, alt, device_id="drone-001"):
    payload = {
        "device_id": device_id,
        "ts_unix_ms": int(time.time() * 1000),
        "lat": lat,
        "lon": lon,
        "alt_m": alt
    }
    payload_hash = hashlib.sha256(
        json.dumps(payload, separators=(",", ":"), sort_keys=True).encode()
    ).hexdigest()
    data = {
        "schema": "primal.v1.position",
        "payload": payload,
        "payload_hash": payload_hash,
        "signature": None
    }
    try:
        r = requests.post(GW, headers=HEADERS, json=data, timeout=5)
        print("POST", r.status_code, r.text)
    except Exception as e:
        print("post err:", e)

# =====================================================
# Swarm simulator
# =====================================================
if __name__ == "__main__":
    base_lat, base_lon, base_alt = 37.7749, -122.4194, 120
    for drone_id in range(1, 15001):  # up to 15,000 drones
        lat = base_lat + random.uniform(-0.05, 0.05)
        lon = base_lon + random.uniform(-0.05, 0.05)
        alt = base_alt + random.uniform(30, 300)
        post_position(lat, lon, alt, device_id=f"drone-{drone_id:05d}")
        time.sleep(0.01)
PY

echo "[✔] swarm_sim.py updated successfully."
