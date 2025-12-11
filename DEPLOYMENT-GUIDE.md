# MotorHandPro Deployment Guide
## MongoDB + Namecheap Domain Integration

**Date:** December 10, 2025  
**Author:** Claude  

---

## 🎯 OVERVIEW

This guide shows how to leverage your MongoDB account and Namecheap domain to deploy MotorHandPro components for production use.

---

## 📊 MONGODB INTEGRATION OPTIONS

### Option 1: LAM Action History Database

Store all LAM actions, states, and history:

```javascript
// MongoDB Schema for LAM
{
  _id: ObjectId,
  timestamp: ISODate,
  action_type: "plan_trip" | "make_reservation" | "order_food" | "manage_subscription",
  user_id: String,
  input: {
    destination: String,
    dates: Object,
    budget: Number
  },
  primal_state: {
    n: Number,
    signal: Number,
    memory_integral: Number,
    error: Number,
    anomaly_score: Number
  },
  lam_integration: {
    enabled: Boolean,
    E_displaced: Number
  },
  result: Object,
  execution_time_ms: Number
}
```

### Option 2: NASA Pipeline Data Storage

Store comet observations and states:

```javascript
// MongoDB Schema for NASA Data
{
  _id: ObjectId,
  pipeline_run_id: String,
  timestamp: ISODate,
  target: "3I/ATLAS",
  observation: {
    ra: Number,
    dec: Number,
    distance_au: Number,
    velocity_km_s: Number,
    magnitude: Number,
    gas_production_rate: Number,
    tail_length_km: Number
  },
  primal_state: {
    n: Number,
    signal: Number,
    memory_integral: Number,
    error: Number,
    anomaly_score: Number
  }
}
```

### Option 3: Test Results Database

Store all branch test results:

```javascript
// MongoDB Schema for Test Results
{
  _id: ObjectId,
  test_run_id: String,
  timestamp: ISODate,
  branch_name: String,
  commit_hash: String,
  tests: [
    {
      name: String,
      type: "lam_core" | "lam_actions" | "hardhat" | "npm",
      passed: Boolean,
      duration_ms: Number,
      details: Object
    }
  ],
  primal_constants: {
    lightfoot: 0.16905,
    donte: 149.9992314,
    lipschitz: 0.000129932
  },
  summary: {
    total: Number,
    passed: Number,
    failed: Number,
    success_rate: Number
  }
}
```

### Option 4: Satellite Tracking Database

Store satellite constellation data:

```javascript
// MongoDB Schema for Satellites
{
  _id: ObjectId,
  satellite_id: String,
  timestamp: ISODate,
  position: {
    latitude: Number,
    longitude: Number,
    altitude_km: Number
  },
  velocity: {
    x: Number,
    y: Number,
    z: Number
  },
  tracking: {
    visible: Boolean,
    elevation: Number,
    azimuth: Number
  }
}
```

---

## 🌐 NAMECHEAP DOMAIN SETUP

### Recommended Domain Structure

**Primary Domain:** `motorhandpro.com` (or your domain)

**Subdomains:**
```
api.motorhandpro.com       → Backend API
lam.motorhandpro.com       → LAM Web Interface
dashboard.motorhandpro.com → Control Panel
nasa.motorhandpro.com      → NASA Pipeline Dashboard
satellite.motorhandpro.com → Satellite Tracking
docs.motorhandpro.com      → Documentation
```

### DNS Configuration (Namecheap)

```
Type   Host            Value                    TTL
A      @               YOUR_SERVER_IP           300
A      api             YOUR_SERVER_IP           300
A      lam             YOUR_SERVER_IP           300
A      dashboard       YOUR_SERVER_IP           300
A      nasa            YOUR_SERVER_IP           300
A      satellite       YOUR_SERVER_IP           300
CNAME  www             motorhandpro.com         300
```

---

## 🚀 DEPLOYMENT ARCHITECTURE

### Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     motorhandpro.com                        │
│                    (Static Landing Page)                     │
└─────────────────────────────────────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
┌───────▼────────┐  ┌─────────▼────────┐  ┌────────▼────────┐
│ lam.           │  │ dashboard.       │  │ nasa.           │
│ motorhandpro   │  │ motorhandpro     │  │ motorhandpro    │
│ (LAM Web UI)   │  │ (Control Panel)  │  │ (NASA Pipeline) │
└────────┬───────┘  └────────┬─────────┘  └────────┬────────┘
         │                   │                      │
         └───────────────────┼──────────────────────┘
                             │
                   ┌─────────▼─────────┐
                   │  api.motorhandpro │
                   │  (FastAPI Backend)│
                   └─────────┬─────────┘
                             │
                   ┌─────────▼─────────┐
                   │   MongoDB Atlas   │
                   │  (Cloud Database) │
                   └───────────────────┘
```

---

## 💻 IMPLEMENTATION

### 1. MongoDB Connection Setup

Create `config/mongodb.py`:

```python
from pymongo import MongoClient
from datetime import datetime
import os

class MotorHandProDB:
    def __init__(self):
        # MongoDB Atlas connection string
        self.connection_string = os.getenv(
            'MONGODB_URI',
            'mongodb+srv://username:password@cluster.mongodb.net/motorhandpro'
        )
        self.client = MongoClient(self.connection_string)
        self.db = self.client.motorhandpro
        
        # Collections
        self.lam_actions = self.db.lam_actions
        self.nasa_observations = self.db.nasa_observations
        self.test_results = self.db.test_results
        self.satellites = self.db.satellites
    
    def save_lam_action(self, action_data):
        """Save LAM action to database"""
        action_data['timestamp'] = datetime.utcnow()
        return self.lam_actions.insert_one(action_data)
    
    def save_nasa_observation(self, observation_data):
        """Save NASA observation to database"""
        observation_data['timestamp'] = datetime.utcnow()
        return self.nasa_observations.insert_one(observation_data)
    
    def save_test_results(self, test_data):
        """Save test results to database"""
        test_data['timestamp'] = datetime.utcnow()
        return self.test_results.insert_one(test_data)
    
    def get_lam_history(self, user_id=None, limit=100):
        """Get LAM action history"""
        query = {'user_id': user_id} if user_id else {}
        return list(self.lam_actions.find(query).sort('timestamp', -1).limit(limit))
    
    def get_nasa_observations(self, target='3I/ATLAS', limit=1000):
        """Get NASA observations"""
        return list(self.nasa_observations.find({'target': target})
                   .sort('timestamp', -1).limit(limit))
```

### 2. FastAPI Backend

Create `api/server.py`:

```python
from fastapi import FastAPI, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import Optional, List
import sys
sys.path.append('..')

from config.mongodb import MotorHandProDB
from lam.core.primal_lam import PrimalLAM

app = FastAPI(title="MotorHandPro API", version="1.0.0")

# CORS configuration
app.add_middleware(
    CORSMiddleware,
    allow_origins=[
        "https://lam.motorhandpro.com",
        "https://dashboard.motorhandpro.com",
        "https://nasa.motorhandpro.com"
    ],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Initialize
db = MotorHandProDB()
lam = PrimalLAM()

class TripRequest(BaseModel):
    destination: str
    departure_date: str
    return_date: str
    budget: Optional[float] = None
    user_id: Optional[str] = None

@app.post("/api/lam/plan-trip")
async def plan_trip(request: TripRequest):
    """Plan a trip using LAM"""
    try:
        # Execute LAM action
        result = lam.plan_trip(
            destination=request.destination,
            departure_date=request.departure_date,
            return_date=request.return_date,
            budget=request.budget
        )
        
        # Get Primal state
        state = lam.get_state()
        
        # Save to MongoDB
        action_data = {
            'action_type': 'plan_trip',
            'user_id': request.user_id,
            'input': request.dict(),
            'primal_state': state,
            'result': result
        }
        db.save_lam_action(action_data)
        
        return {
            'success': True,
            'result': result,
            'primal_state': state
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@app.get("/api/lam/history")
async def get_lam_history(user_id: Optional[str] = None, limit: int = 100):
    """Get LAM action history"""
    history = db.get_lam_history(user_id=user_id, limit=limit)
    return {'history': history, 'count': len(history)}

@app.get("/api/nasa/observations")
async def get_nasa_observations(target: str = "3I/ATLAS", limit: int = 1000):
    """Get NASA observations"""
    observations = db.get_nasa_observations(target=target, limit=limit)
    return {'observations': observations, 'count': len(observations)}

@app.get("/api/health")
async def health_check():
    """Health check endpoint"""
    return {
        'status': 'healthy',
        'mongodb': db.client.server_info() is not None,
        'primal_constants': {
            'lightfoot': 0.16905,
            'donte': 149.9992314,
            'lipschitz': 0.000129932
        }
    }

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

### 3. Nginx Configuration

Create `/etc/nginx/sites-available/motorhandpro`:

```nginx
# API Backend
server {
    listen 80;
    server_name api.motorhandpro.com;
    
    location / {
        proxy_pass http://localhost:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}

# LAM Web Interface
server {
    listen 80;
    server_name lam.motorhandpro.com;
    
    root /var/www/motorhandpro/lam/web;
    index index.html;
    
    location / {
        try_files $uri $uri/ =404;
    }
}

# Control Panel
server {
    listen 80;
    server_name dashboard.motorhandpro.com;
    
    root /var/www/motorhandpro/control_panel;
    index index.html;
    
    location / {
        try_files $uri $uri/ =404;
    }
}

# NASA Pipeline Dashboard
server {
    listen 80;
    server_name nasa.motorhandpro.com;
    
    root /var/www/motorhandpro/nasa_pipeline_output/visualizations;
    index interactive_sky_chart.html;
    
    location / {
        try_files $uri $uri/ =404;
    }
}

# Satellite Dashboard
server {
    listen 80;
    server_name satellite.motorhandpro.com;
    
    root /var/www/motorhandpro/integrations;
    index satellite_dashboard.html;
    
    location / {
        try_files $uri $uri/ =404;
    }
}
```

---

## 🔧 SETUP STEPS

### Step 1: MongoDB Atlas Setup

1. **Create Cluster:**
   ```
   - Go to mongodb.com/atlas
   - Create free M0 cluster
   - Choose region closest to your server
   - Create database user
   - Whitelist IP (0.0.0.0/0 for testing)
   ```

2. **Get Connection String:**
   ```
   mongodb+srv://<username>:<password>@cluster.mongodb.net/motorhandpro
   ```

3. **Create Collections:**
   - lam_actions
   - nasa_observations
   - test_results
   - satellites

### Step 2: Server Setup (VPS/Cloud)

```bash
# Update system
sudo apt update && sudo apt upgrade -y

# Install dependencies
sudo apt install -y nginx python3-pip git

# Install Python packages
pip3 install fastapi uvicorn pymongo dnspython

# Clone repository
cd /var/www
git clone https://github.com/STLNFTART/MotorHandPro.git motorhandpro
cd motorhandpro

# Set environment variables
export MONGODB_URI="mongodb+srv://username:password@cluster.mongodb.net/motorhandpro"

# Start API server
python3 api/server.py &

# Configure Nginx
sudo cp nginx.conf /etc/nginx/sites-available/motorhandpro
sudo ln -s /etc/nginx/sites-available/motorhandpro /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl restart nginx
```

### Step 3: SSL Certificates (Let's Encrypt)

```bash
# Install certbot
sudo apt install -y certbot python3-certbot-nginx

# Get certificates for all subdomains
sudo certbot --nginx -d motorhandpro.com \
  -d api.motorhandpro.com \
  -d lam.motorhandpro.com \
  -d dashboard.motorhandpro.com \
  -d nasa.motorhandpro.com \
  -d satellite.motorhandpro.com

# Auto-renewal (already configured by certbot)
```

---

## 📊 USAGE EXAMPLES

### Example 1: Store LAM Action

```python
from config.mongodb import MotorHandProDB

db = MotorHandProDB()

action = {
    'action_type': 'plan_trip',
    'user_id': 'user123',
    'input': {
        'destination': 'Paris',
        'dates': {'departure': '2025-12-20', 'return': '2025-12-27'},
        'budget': 2000
    },
    'primal_state': {
        'n': 0.1234,
        'signal': 12.34,
        'memory_integral': 0.5678,
        'error': 0.0123,
        'anomaly_score': 0.00123
    },
    'result': {'status': 'success', 'flights_found': 15}
}

db.save_lam_action(action)
```

### Example 2: Query NASA Data

```python
# Get latest 100 observations
observations = db.get_nasa_observations(target='3I/ATLAS', limit=100)

# Analyze
avg_distance = sum(obs['observation']['distance_au'] for obs in observations) / len(observations)
print(f"Average distance: {avg_distance:.2f} AU")
```

### Example 3: API Request (JavaScript)

```javascript
// From lam.motorhandpro.com
async function planTrip() {
  const response = await fetch('https://api.motorhandpro.com/api/lam/plan-trip', {
    method: 'POST',
    headers: {'Content-Type': 'application/json'},
    body: JSON.stringify({
      destination: 'Paris',
      departure_date: '2025-12-20',
      return_date: '2025-12-27',
      budget: 2000,
      user_id: 'user123'
    })
  });
  
  const data = await response.json();
  console.log('Trip plan:', data.result);
  console.log('Primal state:', data.primal_state);
}
```

---

## 💰 COST ESTIMATE

### MongoDB Atlas
- **Free Tier (M0):** $0/month
  - 512 MB storage
  - Shared RAM
  - Good for development/testing

- **Paid Tier (M10):** ~$57/month
  - 10 GB storage
  - 2 GB RAM
  - Recommended for production

### Namecheap Domain
- **Domain:** ~$10-15/year
- **SSL:** Free (Let's Encrypt)

### Server (VPS)
- **DigitalOcean Droplet:** $6-12/month
- **AWS EC2 t3.micro:** ~$8/month
- **Linode Shared:** $5-10/month

**Total:** $5-80/month depending on scale

---

## 🎯 NEXT STEPS

### Immediate Actions

1. ✅ Set up MongoDB Atlas account
2. ✅ Configure Namecheap DNS
3. ✅ Get VPS/cloud server
4. ✅ Deploy backend API
5. ✅ Deploy web interfaces
6. ✅ Configure SSL certificates

### Future Enhancements

1. **User Authentication**
   - JWT tokens
   - OAuth integration
   - User profiles in MongoDB

2. **Real-time Updates**
   - WebSocket connections
   - Live dashboard updates
   - Push notifications

3. **Analytics Dashboard**
   - MongoDB Charts
   - Custom visualizations
   - Performance metrics

4. **CI/CD Pipeline**
   - GitHub Actions
   - Auto-deployment
   - Automated testing

---

## 📖 RESOURCES

- MongoDB Atlas: https://www.mongodb.com/atlas
- Namecheap: https://www.namecheap.com
- FastAPI: https://fastapi.tiangolo.com
- Nginx: https://nginx.org
- Let's Encrypt: https://letsencrypt.org

---

**Ready to deploy MotorHandPro to production!** 🚀
