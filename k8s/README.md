# LAM Kubernetes Deployment

Production-ready Kubernetes manifests for deploying LAM at scale.

## Quick Start

```bash
# 1. Create namespace
kubectl apply -f namespace.yaml

# 2. Apply secrets (edit secrets.yaml first!)
kubectl apply -f secrets.yaml

# 3. Apply config
kubectl apply -f configmap.yaml

# 4. Deploy PostgreSQL
kubectl apply -f postgres.yaml

# 5. Deploy LAM
kubectl apply -f lam-deployment.yaml

# 6. Setup ingress (optional)
kubectl apply -f ingress.yaml
```

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                   Ingress (HTTPS)                   │
│             lam.yourdomain.com                      │
└────────────────┬────────────────────────────────────┘
                 │
        ┌────────┴────────┐
        │                 │
    ┌───▼───┐         ┌───▼───┐
    │  API  │         │  Web  │
    │Service│         │Service│
    └───┬───┘         └───────┘
        │
   ┌────┴────┐
   │         │         │
┌──▼──┐  ┌──▼──┐  ┌──▼──┐
│ LAM │  │ LAM │  │ LAM │  (3-10 replicas, auto-scaled)
│ Pod │  │ Pod │  │ Pod │
└──┬──┘  └──┬──┘  └──┬──┘
   │        │        │
   └────────┴────────┘
            │
      ┌─────▼─────┐
      │ PostgreSQL│
      │  Service  │
      └─────┬─────┘
            │
      ┌─────▼─────┐
      │PostgreSQL │
      │    Pod    │
      └───────────┘
```

## Files

- `namespace.yaml` - LAM system namespace
- `configmap.yaml` - Configuration (environment variables)
- `secrets.yaml` - Sensitive data (passwords, API keys)
- `postgres.yaml` - PostgreSQL database
- `lam-deployment.yaml` - LAM application deployment
- `ingress.yaml` - HTTPS ingress configuration

## Configuration

### 1. Edit Secrets

**IMPORTANT**: Update `secrets.yaml` with your actual values:

```yaml
stringData:
  DATABASE_URL: "postgresql://user:password@postgres:5432/lam_db"
  JWT_SECRET_KEY: "$(openssl rand -base64 32)"
  AMADEUS_KEY: "your-key"
  # ... etc
```

### 2. Set Ingress Host

Edit `ingress.yaml` to use your domain:

```yaml
- host: lam.yourdomain.com
```

## Scaling

### Manual Scaling

```bash
# Scale to 5 replicas
kubectl scale deployment/lam-core --replicas=5 -n lam-system
```

### Auto-Scaling

HPA is configured to scale between 3-10 replicas based on:
- CPU utilization (70% threshold)
- Memory utilization (80% threshold)

```bash
# Check HPA status
kubectl get hpa -n lam-system

# Describe HPA
kubectl describe hpa lam-core-hpa -n lam-system
```

## Monitoring

```bash
# Watch pods
kubectl get pods -n lam-system -w

# View logs
kubectl logs -f deployment/lam-core -n lam-system

# Port-forward for local testing
kubectl port-forward svc/lam-api 8000:8000 -n lam-system

# Check resource usage
kubectl top pods -n lam-system
```

## Storage

### Persistent Volumes

- **PostgreSQL**: 10Gi PVC for database storage
- **LAM Data**: 5Gi PVC for application data (credentials, logs, experiments)

```bash
# Check PVCs
kubectl get pvc -n lam-system

# Check PVs
kubectl get pv
```

## Database Management

### Access PostgreSQL

```bash
# Port-forward to local
kubectl port-forward svc/postgres 5432:5432 -n lam-system

# Connect with psql
psql postgresql://lam_user:lam_password@localhost:5432/lam_db
```

### Backup Database

```bash
# Create backup
kubectl exec -n lam-system deployment/postgres -- \
  pg_dump -U lam_user lam_db > backup.sql

# Restore backup
cat backup.sql | kubectl exec -i -n lam-system deployment/postgres -- \
  psql -U lam_user lam_db
```

## Security

### TLS/SSL

The ingress is configured for HTTPS with cert-manager:

```bash
# Install cert-manager
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.0/cert-manager.yaml

# Create ClusterIssuer for Let's Encrypt
kubectl apply -f - <<EOF
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: your-email@example.com
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
    - http01:
        ingress:
          class: nginx
EOF
```

### Network Policies

```bash
# Apply network policies (optional)
kubectl apply -f network-policies.yaml
```

## Troubleshooting

### Pods Not Starting

```bash
# Check pod status
kubectl describe pod -n lam-system <pod-name>

# Check events
kubectl get events -n lam-system --sort-by='.lastTimestamp'
```

### Database Connection Issues

```bash
# Test PostgreSQL connectivity
kubectl run -it --rm debug --image=postgres:15-alpine --restart=Never -n lam-system -- \
  psql postgresql://lam_user:lam_password@postgres:5432/lam_db
```

### Resource Limits

```bash
# Check resource usage
kubectl top pods -n lam-system

# Adjust limits in lam-deployment.yaml if needed
```

## Production Checklist

- [ ] Update all secrets in `secrets.yaml`
- [ ] Configure ingress with your domain
- [ ] Set up cert-manager for HTTPS
- [ ] Configure database backups
- [ ] Set appropriate resource limits
- [ ] Enable monitoring (Prometheus/Grafana)
- [ ] Configure logging aggregation (ELK/Loki)
- [ ] Set up alerting
- [ ] Review security policies
- [ ] Enable pod security policies
- [ ] Configure network policies
- [ ] Set up disaster recovery plan

## Cloud Provider Specific

### AWS (EKS)

```bash
# Create EKS cluster
eksctl create cluster --name lam-cluster --region us-west-2

# Deploy
kubectl apply -f k8s/
```

### Google Cloud (GKE)

```bash
# Create GKE cluster
gcloud container clusters create lam-cluster --num-nodes=3

# Deploy
kubectl apply -f k8s/
```

### Azure (AKS)

```bash
# Create AKS cluster
az aks create --name lam-cluster --resource-group lam-rg --node-count 3

# Get credentials
az aks get-credentials --name lam-cluster --resource-group lam-rg

# Deploy
kubectl apply -f k8s/
```

## Cleanup

```bash
# Delete everything
kubectl delete namespace lam-system

# Or delete specific resources
kubectl delete -f k8s/
```
