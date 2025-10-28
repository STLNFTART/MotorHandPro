#!/usr/bin/env bash
set -euo pipefail

cd "$HOME/primal"

# 1) constants
cat > primal_constants.py <<'PY'
COMM_RANGE_M = 13500.0   # 13.5 km neighbor radius
LAMBDA       = 0.12      # temporal decay
TAU          = 0.35      # trust floor
HEARTBEAT_S  = 2.0       # min seconds between posts
R_EARTH      = 6371000.0
PY

# 2) swarm simulator: continuous 8 km, role cascade, adaptive posting, mission intent
cat > swarm_sim.py <<'PY'
#!/usr/bin/env python3
import json, time, math, random, requests, hashlib
from primal_constants import COMM_RANGE_M, LAMBDA, TAU, HEARTBEAT_S, R_EARTH as R
GW = "http://127.0.0.1:9000/proof"
N=500; DT=0.4; TOTAL_KM=8.0; BASE=(37.7749,-122.4194)
sess=requests.Session(); sess.trust_env=False; sess.headers.update({"Connection":"close","Content-Type":"application/json"})

def haversine_m(lat1,lon1,lat2,lon2):
    φ1,λ1,φ2,λ2=map(math.radians,[lat1,lon1,lat2,lon2]); dφ=φ2-φ1; dλ=λ2-λ1
    a=math.sin(dφ/2)**2+math.cos(φ1)*math.cos(φ2)*math.sin(dλ/2)**2
    return 2*R*math.asin(math.sqrt(a))
def move(lat,lon,brg_deg,dist_m):
    br=math.radians(brg_deg); lat1,lon1=map(math.radians,[lat,lon]); dR=dist_m/R
    lat2=math.asin(math.sin(lat1)*math.cos(dR)+math.cos(lat1)*math.sin(dR)*math.cos(br))
    lon2=lon1+math.atan2(math.sin(br)*math.sin(dR)*math.cos(lat1), math.cos(dR)-math.sin(lat1)*math.sin(lat2))
    return math.degrees(lat2), ((math.degrees(lon2)+540)%360)-180
def trust(dt,q): return TAU+(1.0-TAU)*math.exp(-LAMBDA*dt)*max(0.0,min(1.0,q))
def post(payload):
    b=json.dumps(payload,separators=(',',':'),sort_keys=True).encode()
    proof={"schema":"primal.v1.position","payload":payload,"payload_hash":hashlib.sha256(b).hexdigest(),"signature":None}
    try: sess.post(GW,json=proof,timeout=5)
    except Exception as e: print("post err:",e)

def main():
    rng=random.Random(42)
    speeds=[6,8,10,12]
    drones=[]
    for i in range(N):
        sp=rng.choice(speeds)*3.0
        alt= rng.uniform(120,240)
        drones.append({"id":f"uav-{i:05d}","lat":BASE[0]+rng.uniform(-0.002,0.002),
            "lon":BASE[1]+rng.uniform(-0.002,0.002),"alt":alt,"course":rng.uniform(0,360),
            "speed":sp,"remaining":TOTAL_KM*1000.0,"last_post":0.0,"last_seen_ts":time.time(),
            "last_nbrs":-1,"q":0.9,"role":"WING","intent":"TRANSIT"})
    while any(d["remaining"]>0 for d in drones):
        now=time.time(); tms=int(now*1000)
        for d in drones:
            if d["remaining"]<=0: continue
            d["course"]=(d["course"]+random.uniform(-3,3))%360
            step=d["speed"]*DT; d["lat"],d["lon"]=move(d["lat"],d["lon"],d["course"],step)
            d["remaining"]-=step; d["alt"]+=math.sin(tms/2500 + hash(d["id"])%100/25.0)*0.12
            # neighbors within 13.5 km
            nbrs=0
            for o in drones:
                if o is d: continue
                if haversine_m(d["lat"],d["lon"],o["lat"],o["lon"])<=COMM_RANGE_M: nbrs+=1
            # trust + roles
            dt=now-d.get("last_seen_ts",now); T=trust(dt, d.get("q",0.9)); d["last_seen_ts"]=now
            d["role"]="LEADER" if (T>=0.85 and nbrs>=8) else ("RELAY" if (T>=0.65 and nbrs>=4) else "WING")
            d["intent"]="HOLD" if d["remaining"]<200 else "TRANSIT"
            # event-driven post
            if nbrs!=d["last_nbrs"] or (now-d["last_post"])>=HEARTBEAT_S:
                d["last_nbrs"]=nbrs; d["last_post"]=now
                vx=d["speed"]*math.cos(math.radians(d["course"]))
                vy=d["speed"]*math.sin(math.radians(d["course"]))
                payload={"device_id":d["id"],"ts_unix_ms":tms,"lat":d["lat"],"lon":d["lon"],
                    "alt_m":round(d["alt"],2),"hdg_deg":round(d["course"],1),
                    "vx_ms":vx,"vy_ms":vy,"vz_ms":0.0,
                    "swarm":{"neighbors":nbrs,"trust_T":round(T,4),"tau":TAU,"lambda":LAMBDA,
                             "role":d["role"],"intent":d["intent"],
                             "remaining_km":round(max(d["remaining"],0)/1000.0,3)}}
                post(payload)
        time.sleep(DT)
if __name__=="__main__": main()
PY

# 3) gateway: add /snapshot (JSON dump) and /atc_feed (NDJSON mirror)
# (non-destructive: tries to patch; if patterns missing, appends routes)
if ! grep -q "def snapshot" gateway.py 2>/dev/null; then
cat >> gateway.py <<'PY'

# --- Primal add-ons: snapshot + ATC NDJSON mirror ---
from starlette.responses import JSONResponse, PlainTextResponse
from starlette.routing import Route

def _as_ndjson(records):
    return "\n".join(json.dumps(r, separators=(',',':')) for r in records) + "\n"

async def snapshot(request):
    # last 1000 proofs
    data = list(RING)[-1000:]
    return JSONResponse(data)

async def atc_feed(request):
    # stream-friendly NDJSON snapshot (pull; not SSE)
    data = list(RING)[-5000:]
    return PlainTextResponse(_as_ndjson(data), media_type="application/x-ndjson")

try:
    routes += [Route("/snapshot", snapshot), Route("/atc_feed", atc_feed)]
except Exception:
    pass
PY
fi

echo "[ok] Upgraded."
