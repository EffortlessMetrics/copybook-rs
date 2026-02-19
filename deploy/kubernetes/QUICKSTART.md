<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Kubernetes Quick Start for copybook-rs

This guide provides quick deployment commands for copybook-rs on Kubernetes.

## Prerequisites

- Kubernetes cluster (1.20+)
- kubectl configured
- Storage provisioner for PersistentVolumeClaims

## 5-Minute Quick Start

### 1. Deploy Base Configuration

```bash
cd deploy/kubernetes

# Apply all base resources
kubectl apply -k .

# Wait for deployment to be ready
kubectl rollout status deployment/copybook-rs

# Verify resources
kubectl get pods,svc,pvc -l app=copybook-rs
```

### 2. Upload Data to Pod

```bash
# Get pod name
POD=$(kubectl get pod -l app=copybook-rs -o jsonpath='{.items[0].metadata.name}')

# Upload copybook definition
kubectl cp /path/to/customers.cpy ${POD}:/data/copybooks/

# Upload binary data
kubectl cp /path/to/data.bin ${POD}:/data/input/
```

### 3. Run Decode Operation

```bash
# Execute decode
kubectl exec -it ${POD} -- /usr/local/bin/copybook decode \
  /data/copybooks/customers.cpy \
  /data/input/data.bin \
  --output /data/output/customers.jsonl \
  --format fixed \
  --codepage cp037

# Download results
kubectl cp ${POD}:/data/output/customers.jsonl ./customers.jsonl
```

## Environment-Specific Deployments

### Development Environment

```bash
# Deploy to dev namespace
kubectl apply -k overlays/dev/

# Verify
kubectl get pods -n dev -l app=copybook-rs
```

### Staging Environment

```bash
# Deploy to staging namespace
kubectl apply -k overlays/staging/

# Verify
kubectl get pods -n staging -l app=copybook-rs
```

### Production Environment

```bash
# Deploy to production namespace
kubectl apply -k overlays/production/

# Verify (should have 2 replicas)
kubectl get pods -n production -l app=copybook-rs
```

## Using Jobs Instead of Deployments

### One-Off Decode Job

```bash
# Create decode job
kubectl apply -f jobs/decode-job.yaml

# Monitor progress
kubectl get jobs
kubectl logs job/copybook-decode-job -f

# Check completion
kubectl get job copybook-decode-job -o jsonpath='{.status.succeeded}'
```

### Scheduled Processing with CronJobs

```bash
# Deploy nightly decode job
kubectl apply -f jobs/cronjob-example.yaml

# List CronJobs
kubectl get cronjobs

# Check recent runs
kubectl get jobs -l app=copybook-rs
```

## Common Operations

### Check Logs

```bash
# Deployment logs
kubectl logs -l app=copybook-rs --tail=50

# Job logs
kubectl logs job/copybook-decode-job

# Follow logs
kubectl logs -l app=copybook-rs -f
```

### Check Resource Usage

```bash
# CPU and memory usage
kubectl top pod -l app=copybook-rs

# Detailed resource information
kubectl describe pod -l app=copybook-rs
```

### Scale Deployment

```bash
# Scale to 3 replicas
kubectl scale deployment/copybook-rs --replicas=3

# Verify scaling
kubectl get deployment copybook-rs
```

### Update Image Version

```bash
# Update to specific version
kubectl set image deployment/copybook-rs \
  copybook=ghcr.io/effortlessmetrics/copybook-rs:v0.5.0

# Check rollout status
kubectl rollout status deployment/copybook-rs
```

### Access Shell in Pod

```bash
# Get shell access
kubectl exec -it ${POD} -- /bin/sh

# Navigate to data directory
cd /data
ls -la copybooks/ input/ output/
```

## Troubleshooting

### Pod Won't Start

```bash
# Check pod events
kubectl describe pod -l app=copybook-rs

# Check PVC status
kubectl get pvc copybook-data-pvc
kubectl describe pvc copybook-data-pvc

# Check pod logs
kubectl logs -l app=copybook-rs --previous
```

### Job Failed

```bash
# Check job status
kubectl describe job copybook-decode-job

# Check job logs
kubectl logs job/copybook-decode-job

# Delete and recreate
kubectl delete job copybook-decode-job
kubectl apply -f jobs/decode-job.yaml
```

### Out of Memory

```bash
# Check current memory usage
kubectl top pod -l app=copybook-rs

# Edit deployment for more memory
kubectl edit deployment copybook-rs
# Update: spec.template.spec.containers[0].resources.limits.memory

# Or apply overlay with higher resources
kubectl apply -k overlays/production/
```

## Configuration Changes

### Change Logging Level

```bash
# Edit ConfigMap
kubectl edit configmap copybook-config

# Update RUST_LOG value (debug/info/warn/error)

# Restart pods to apply
kubectl rollout restart deployment/copybook-rs
```

### Change Dialect Setting

```bash
# Edit ConfigMap
kubectl edit configmap copybook-config

# Update COPYBOOK_DIALECT value (0/1/n)

# Restart pods
kubectl rollout restart deployment/copybook-rs
```

## Monitoring

### Prometheus Metrics

```bash
# Port-forward to metrics endpoint
kubectl port-forward svc/copybook-rs 9300:9300

# Access metrics in browser
open http://localhost:9300/metrics

# Or with curl
curl http://localhost:9300/metrics
```

### Health Checks

```bash
# Check liveness probe
kubectl exec ${POD} -- /usr/local/bin/copybook --version

# Check readiness
kubectl get pod ${POD} -o jsonpath='{.status.conditions[?(@.type=="Ready")].status}'
```

## Cleanup

### Remove All Resources

```bash
# Delete base deployment
kubectl delete -k .

# Or delete specific environment
kubectl delete -k overlays/production/

# Delete jobs
kubectl delete -f jobs/
```

### Remove Specific Job

```bash
# Delete completed job
kubectl delete job copybook-decode-job

# Delete CronJob
kubectl delete cronjob copybook-nightly-decode
```

### Clean PVC (Warning: Deletes Data!)

```bash
# Delete PVC and data
kubectl delete pvc copybook-data-pvc
```

## Next Steps

- See [README.md](README.md) for comprehensive documentation
- Check [deployment.yaml](deployment.yaml) for configuration details
- Review [jobs/](jobs/) directory for example jobs
- Customize [overlays/](overlays/) for your environments

## Helpful Commands Reference

```bash
# Get all copybook-rs resources
kubectl get all -l app=copybook-rs

# Watch deployment rollout
kubectl rollout status deployment/copybook-rs -w

# Get pod name quickly
POD=$(kubectl get pod -l app=copybook-rs -o jsonpath='{.items[0].metadata.name}')

# Copy file to/from pod
kubectl cp local-file.txt ${POD}:/data/
kubectl cp ${POD}:/data/output.jsonl local-output.jsonl

# Execute command in pod
kubectl exec ${POD} -- /usr/local/bin/copybook --help

# Get pod shell
kubectl exec -it ${POD} -- /bin/sh

# Check pod resource limits
kubectl get pod ${POD} -o jsonpath='{.spec.containers[0].resources}'

# Validate manifests
./validate.sh
```
