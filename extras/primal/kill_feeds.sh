#!/usr/bin/env bash
set -e
echo "[*] Killing uvicorn + python…"
pkill -f "uvicorn" 2>/dev/null || true
pkill -f "gateway:app" 2>/dev/null || true
pkill -f "swarm_sim" 2>/dev/null || true
pkill -f "python" 2>/dev/null || true

# tiny wait to let sockets close
sleep 1

echo "[*] Verifying localhost responds (should fail)…"
if curl -s --max-time 1 http://127.0.0.1:9000/ >/dev/null; then
  echo "[!] Port 9000 still answers; will try harder"
fi

echo "[*] Hard kill any leftover Python PIDs…"
for pid in $(ps -A | awk '/python|uvicorn/ {print $2}'); do
  kill -9 "$pid" 2>/dev/null || true
done

echo "[*] Done. All feeds should be down."
