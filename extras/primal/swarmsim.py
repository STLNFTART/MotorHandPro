# swarm_sim.py
import json, time, math, random, httpx
from hashlib import sha256

GW = "http://127.0.0.1:8080/proof"
N = 12                 # raise later if phone handles it
TOTAL_KM = 8.0
SPEED_SCALE = 3.0
BASE_SPEEDS_MS = [6,8,10,12]
DT = 0.5
BASE = (37.7749, -122.4194)
ALT_BANDS_M = [(90,130),(130,170),(170,210),(210,250)]
R = 6371000.0

def move(lat,lon,course_deg,dist_m):
    br=math.radians(course_deg); lat1,lon1=math.radians(lat),math.radians(lon)
    dR=dist_m/R
    lat2=math.asin(math.sin(lat1)*math.cos(dR)+math.cos(lat1)*math.sin(dR)*math.cos(br))
    lon2=lon1+math.atan2(math.sin(br)*math.sin(dR)*math.cos(lat1), math.cos(dR)-math.sin(lat1)*math.sin(lat2))
    return math.degrees(lat2), ((math.degrees(lon2)+540)%360)-180

def main():
    drones=[]
    for i in range(N):
        sp = random.choice(BASE_SPEEDS_MS)*SPEED_SCALE
        a0,a1 = ALT_BANDS_M[i%len(ALT_BANDS_M)]
        d={
          "id": f"uav-{i:03d}",
          "lat": BASE[0]+random.uniform(-0.002,0.002),
          "lon": BASE[1]+random.uniform(-0.002,0.002),
          "alt": random.uniform(a0,a1),
          "course": random.uniform(0,360),
          "speed": sp,
          "remaining": TOTAL_KM*1000.0
        }
        drones.append(d)

    with httpx.Client(timeout=5.0) as client:
        while any(d["remaining"]>0 for d in drones):
            tms=int(time.time()*1000)
            for d in drones:
                if d["remaining"]<=0: continue
                step = d["speed"]*DT
                d["lat"], d["lon"] = move(d["lat"], d["lon"], d["course"], step)
                d["remaining"] -= step
                d["alt"] += math.sin(tms/5000 + i)*0.05

                payload = {
                  "device_id": d["id"], "ts_unix_ms": tms,
                  "lat": d["lat"], "lon": d["lon"], "alt_m": round(d["alt"],2),
                  "vx_ms": d["speed"]*math.cos(math.radians(d["course"])),
                  "vy_ms": d["speed"]*math.sin(math.radians(d["course"])),
                  "vz_ms": 0.0, "hdg_deg": round(d["course"],1),
                  "swarm":{"intent":"transit","remaining_km":round(max(d["remaining"],0)/1000.0,2)}
                }
                b = json.dumps(payload, separators=(',',':'), sort_keys=True).encode()
                proof = {"schema":"primal.v1.position","payload":payload,"payload_hash":sha256(b).hexdigest(),"signature":None}
                try: client.post(GW, json=proof)
                except Exception as e: print("post err:", e)
            time.sleep(DT)

if __name__=="__main__": main()
