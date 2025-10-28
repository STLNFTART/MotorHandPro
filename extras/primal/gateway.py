import json, time, hashlib, asyncio
from collections import deque
from typing import Any, Dict, List
from starlette.applications import Starlette
from starlette.responses import PlainTextResponse, HTMLResponse, StreamingResponse, JSONResponse
from starlette.routing import Route

RECENT = deque(maxlen=10000)
CLIENTS: List[asyncio.Queue] = []
now_ms = lambda: int(time.time()*1000)
def jh(obj: Dict[str, Any]) -> str: return hashlib.sha256(json.dumps(obj, separators=(',',':'), sort_keys=True).encode()).hexdigest()

async def root(_): return PlainTextResponse("OK\nTry /dashboard or /events")
HTML = """<!doctype html><meta charset=utf-8><title>Primal</title>
<style>body{background:#0b1220;color:#e6eefc;font:14px system-ui;margin:12px}#c{width:100%;height:520px;background:#061024;border:1px solid #1f2b46;border-radius:14px}</style>
<h3>Primal Live</h3><canvas id=c width=900 height=520></canvas>
<div>events: <span id=n>0</span> • <span id=s>…</span><br><code id=last></code></div>
<script>const es=new EventSource('/events'),c=document.getElementById('c'),x=c.getContext('2d');let n=0,colors={};
function col(id){if(colors[id])return colors[id];const h=(Object.keys(colors).length*67)%360;return colors[id]=`hsl(${h} 90% 60%)`;}
function toXY(lat,lon){return[(lon+180)/360*c.width,(90-lat)/180*c.height]}
es.onopen=()=>s.textContent='connected';es.onerror=()=>s.textContent='reconnecting…';
es.onmessage=e=>{const m=JSON.parse(e.data);if(m.type!=='proof')return;const d=m.data,id=m.raw?.device_id||d.device_id||'uav';
if(typeof d.lat!=='number'||typeof d.lon!=='number')return;const [X,Y]=toXY(d.lat,d.lon);x.fillStyle='rgba(255,255,255,.05)';x.fillRect(0,0,c.width,c.height);
x.fillStyle=col(id);x.beginPath();x.arc(X,Y,3,0,6.283);x.fill();x.fillText(id,X+6,Y-6);document.getElementById('n').textContent=(++n).toString();
document.getElementById('last').textContent=`${id} lat=${d.lat.toFixed(6)} lon=${d.lon.toFixed(6)} ack=${d.ack}`;};</script>"""
async def dashboard(_): return HTMLResponse(HTML)

async def events(_):
    q=asyncio.Queue(); CLIENTS.append(q)
    async def gen():
        yield "data: "+json.dumps({"type":"hello"})+"\n\n"
        try:
            while True: yield "data: "+json.dumps(await q.get())+"\n\n"
        except asyncio.CancelledError: pass
        finally:
            if q in CLIENTS: CLIENTS.remove(q)
    return StreamingResponse(gen(), media_type="text/event-stream")

async def broadcast(m): 
    for q in CLIENTS[:]:
        try: q.put_nowait(m)
        except Exception:
            try: CLIENTS.remove(q)
            except ValueError: pass

async def proof(request):
    data = await request.json()  # {schema,payload,payload_hash,signature?}
    payload = data.get("payload", {})
    ok = (jh(payload) == data.get("payload_hash",""))
    rec = {"ok":ok,"ack":hashlib.sha256(str(data.get("payload_hash","")).encode()).hexdigest()[:12],
           "schema":data.get("schema",""),"t_ms":now_ms(),"hash":str(data.get("payload_hash",""))[:12],
           "lat":payload.get("lat"),"lon":payload.get("lon"),"device_id":payload.get("device_id","dev")}
    RECENT.append(rec); await broadcast({"type":"proof","data":rec,"raw":payload})
    return JSONResponse(rec)

async def proofs(_): return JSONResponse(list(RECENT)[-500:])

routes=[Route("/",root),Route("/dashboard",dashboard),Route("/events",events),
        Route("/proof",proof,methods=["POST"]),Route("/proofs",proofs)]
app=Starlette(debug=False,routes=routes)

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
