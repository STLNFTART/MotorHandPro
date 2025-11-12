import os, json, time, hashlib, asyncio
from collections import deque
from fastapi import FastAPI, Body
from fastapi.responses import PlainTextResponse, HTMLResponse
from pydantic import BaseModel
from typing import Any, Dict, List, Optional

app = FastAPI(title="Primal Gateway")

# last 10k events
RECENT: deque = deque(maxlen=10000)
# SSE client queues
CLIENTS: List[asyncio.Queue] = []

def now_ms() -> int: return int(time.time()*1000)
def jhash(payload: Dict[str, Any]) -> str:
    b = json.dumps(payload, separators=(',',':'), sort_keys=True).encode()
    return hashlib.sha256(b).hexdigest()

async def _broadcast(obj: Dict[str, Any]):
    dead = []
    for q in CLIENTS:
        try: q.put_nowait(obj)
        except Exception: dead.append(q)
    for q in dead:
        try: CLIENTS.remove(q)
        except ValueError: pass

def _log(evt: Dict[str, Any]):
    RECENT.append(evt)

class Proof(BaseModel):
    schema: str
    payload: Dict[str, Any]
    payload_hash: str
    signature: Optional[str] = None  # optional; accepted but not required

@app.get("/", response_class=PlainTextResponse)
def root():
    return "Primal Gateway OK\nTry /dashboard or /events"

@app.get("/dashboard", response_class=HTMLResponse)
def dashboard():
    return HTML

@app.get("/events")
async def events():
    q: asyncio.Queue = asyncio.Queue()
    CLIENTS.append(q)
    async def gen():
        yield "data: "+json.dumps({"type":"hello","data":"connected"})+"\n\n"
        try:
            while True:
                item = await q.get()
                yield "data: "+json.dumps(item)+"\n\n"
        except asyncio.CancelledError:
            pass
        finally:
            try: CLIENTS.remove(q)
            except ValueError: pass
    from fastapi.responses import StreamingResponse
    return StreamingResponse(gen(), media_type="text/event-stream")

@app.get("/proofs")
def proofs():
    return list(RECENT)[-200:]

@app.post("/proof")
async def proof(p: Proof):
    # verify payload hash
    ok = (jhash(p.payload) == p.payload_hash)
    lat = p.payload.get("lat"); lon = p.payload.get("lon")
    dev = p.payload.get("device_id", "unknown")
    ack = hashlib.sha256(p.payload_hash.encode()).hexdigest()
    rec = {
        "ok": ok, "ack": ack[:12], "schema": p.schema, "t_ms": now_ms(),
        "hash": p.payload_hash[:12], "lat": lat, "lon": lon,
        "device_id": dev, "sig": True if p.signature else False
    }
    _log(rec)
    await _broadcast({"type":"proof","data":rec, "raw":p.payload})
    return {"ok": ok, "ack": ack, "schema": p.schema, "t_ms": now_ms(),
            "hash": p.payload_hash[:12], "lat": lat, "lon": lon}

@app.post("/proofs/batch")
async def proofs_batch(items: List[Proof]):
    out_events = []
    for p in items:
        ok = (jhash(p.payload) == p.payload_hash)
        lat = p.payload.get("lat"); lon = p.payload.get("lon")
        dev = p.payload.get("device_id","unknown")
        ack = hashlib.sha256(p.payload_hash.encode()).hexdigest()
        rec = {"ok": ok, "ack": ack[:12], "schema": p.schema, "t_ms": now_ms(),
               "hash": p.payload_hash[:12], "lat": lat, "lon": lon, "device_id": dev}
        RECENT.append(rec)
        out_events.append({"type":"proof","data":rec, "raw":p.payload})
    for e in out_events: await _broadcast(e)
    return {"ok": True, "accepted": len(items)}

@app.post("/atc/plan")
async def atc_plan(plan: Dict[str, Any] = Body(...)):
    pkt = {"type":"atc_plan","data": plan, "t_ms": now_ms()}
    _log(pkt)
    await _broadcast(pkt)
    return {"ok": True}

HTML = """<!doctype html><meta charset="utf-8">
<title>Primal Dashboard</title>
<style>
body{background:#0b1220;color:#e6eefc;font:14px/1.4 system-ui,Segoe UI,Roboto}
h1{font-size:18px;margin:8px 0}
#row{display:flex;gap:16px}
.card{background:#0f172a;border:1px solid #1f2b46;border-radius:14px;padding:12px;box-shadow:0 0 0 1px rgba(255,255,255,.02) inset}
#log{height:200px;overflow:auto;white-space:pre-wrap}
#c{width:100%;height:520px;background:#061024;border-radius:14px;border:1px solid #1f2b46}
</style>
<h1>Primal Gateway — Live</h1>
<div id="row">
  <div class="card" style="flex:2">
    <canvas id="c" width="900" height="520"></canvas>
  </div>
  <div class="card" style="flex:1">
    <div>Status: <span id="status">—</span></div>
    <div>Events: <span id="count">0</span></div>
    <div>Last: <div id="last" style="font-family:monospace"></div></div>
    <div style="margin-top:8px">Log:</div>
    <div id="log"></div>
  </div>
</div>
<script>
const es=new EventSource('/events');
const statusEl=document.getElementById('status');
const lastEl=document.getElementById('last');
const logEl=document.getElementById('log');
const countEl=document.getElementById('count');
const canvas=document.getElementById('c'), ctx=canvas.getContext('2d');
let count=0; const colors={};
function color(id){ if(colors[id]) return colors[id]; const h=(Object.keys(colors).length*67)%360; return colors[id]=`hsl(${h} 90% 60%)`; }
function toXY(lat,lon){ const W=canvas.width,H=canvas.height; return [(lon+180)/360*W,(90-lat)/180*H]; }
function dot(lat,lon,id){
  ctx.fillStyle='rgba(255,255,255,0.04)'; ctx.fillRect(0,0,canvas.width,canvas.height);
  const [x,y]=toXY(lat,lon); ctx.fillStyle=color(id); ctx.beginPath(); ctx.arc(x,y,3,0,6.283); ctx.fill();
  ctx.fillText(id,x+6,y-6);
}
function pushLog(s){ const at=new Date().toISOString().split('T')[1].split('.')[0]; logEl.textContent=(`[${at}] `+s+"\\n"+logEl.textContent).slice(0,4000); }
es.onopen=()=>statusEl.textContent='connected';
es.onerror=()=>statusEl.textContent='reconnecting…';
es.onmessage=(e)=>{
  const m=JSON.parse(e.data);
  if(m.type==='proof'){
    const d=m.data; const id=m.raw?.device_id||d.device_id||'drone';
    if(typeof d.lat==='number'&&typeof d.lon==='number'){
      dot(d.lat,d.lon,id); countEl.textContent=(++count).toString();
      lastEl.textContent=`${id} lat=${d.lat.toFixed(6)} lon=${d.lon.toFixed(6)} ack=${d.ack}`;
    }
  }else if(m.type==='atc_plan'){ pushLog('ATC plan: '+JSON.stringify(m.data)); }
};
</script>
# --- Primal live event stream ---
from fastapi.responses import StreamingResponse
import json, asyncio

RING = []

@app.get("/events")
async def events():
    async def event_stream():
        last = 0
        while True:
            await asyncio.sleep(0.5)
            if len(RING) > last:
                for ev in RING[last:]:
                    yield f"data: {json.dumps(ev)}\n\n"
                last = len(RING)
    return StreamingResponse(event_stream(), media_type="text/event-stream")
