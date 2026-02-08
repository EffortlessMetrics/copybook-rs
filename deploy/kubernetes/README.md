# Kubernetes Deployment for copybook-rs

This directory contains Kubernetes manifests for deploying copybook-rs as a batch processing system.

## Overview

copybook-rs is deployed as a **batch job processor** for COBOL copybook data conversion. The deployment supports two modes:

1. **Long-lived Deployment**: Persistent pods that accept work via `kubectl exec`
2. **Kubernetes Jobs**: One-off batch processing jobs (see `jobs/` directory)

## Directory Structure

```
deploy/kubernetes/
├── deployment.yaml           # Base deployment manifest
├── kustomization.yaml        # Base kustomization configuration
├── overlays/
│   ├── dev/                 # Development environment overlay
│   ├── staging/             # Staging environment overlay
│   └── production/          # Production environment overlay
├── jobs/
│   ├── decode-job.yaml      # Example decode job
│   └── encode-job.yaml      # Example encode job
└── README.md                # This file
```

## Quick Start

### Deploy Base Configuration

```bash
# Apply base deployment to default namespace
kubectl apply -k .

# Verify deployment
kubectl get pods -l app=copybook-rs
kubectl get svc copybook-rs
kubectl get pvc copybook-data-pvc
```

### Deploy to Specific Environment

```bash
# Development
kubectl apply -k overlays/dev/

# Staging
kubectl apply -k overlays/staging/

# Production
kubectl apply -k overlays/production/
```

## Components

### 1. ConfigMap (copybook-config)

Provides default configuration for copybook-rs operations:

- `DEFAULT_CODEPAGE`: EBCDIC codepage (cp037 for IBM US)
- `DEFAULT_FORMAT`: Record format (fixed/rdw)
- `COPYBOOK_DIALECT`: ODO min_count interpretation (0=zero-tolerant, 1=one-tolerant, n=normative)
- `RUST_LOG`: Logging level (off/error/warn/info/debug/trace)
- `JSON_NUMBER_MODE`: JSON numeric encoding (lossless/native)

#### Enterprise Feature Flags

The ConfigMap also includes enterprise feature flags for production deployments:

**Audit & Compliance:**
- `COPYBOOK_FF_AUDIT_SYSTEM`: Enable audit system (0=disabled, 1=enabled)
- `COPYBOOK_FF_SOX_COMPLIANCE`: Enable SOX compliance validation
- `COPYBOOK_FF_HIPAA_COMPLIANCE`: Enable HIPAA compliance validation
- `COPYBOOK_FF_GDPR_COMPLIANCE`: Enable GDPR compliance validation
- `COPYBOOK_FF_PCI_DSS_COMPLIANCE`: Enable PCI DSS compliance validation
- `COPYBOOK_FF_SECURITY_MONITORING`: Enable security monitoring

**Performance Features:**
- `COPYBOOK_FF_LRU_CACHE`: Enable LRU cache for parsed copybooks
- `COPYBOOK_FF_ADVANCED_OPTIMIZATION`: Enable advanced optimization (SIMD, vectorization)
- `COPYBOOK_FF_PARALLEL_DECODE`: Enable parallel decoding for large files
- `COPYBOOK_FF_ZERO_COPY`: Enable zero-copy parsing

**Debug Features (disable in production):**
- `COPYBOOK_FF_VERBOSE_LOGGING`: Enable verbose logging
- `COPYBOOK_FF_DIAGNOSTIC_OUTPUT`: Enable diagnostic output

**Audit Configuration:**
- `AUDIT_FORMAT`: Audit output format (jsonl/stdout)
- `AUDIT_OUTPUT`: Audit output destination
- `AUDIT_BATCH_SIZE`: Number of audit events to batch
- `AUDIT_BATCH_INTERVAL_SECONDS`: Batch interval in seconds

**Metrics Configuration:**
- `METRICS_ENABLED`: Enable Prometheus metrics
- `METRICS_PORT`: Metrics port (default: 9300)
- `METRICS_PATH`: Metrics endpoint path (default: /metrics)

> **Note**: See [Enterprise Deployment Guide](../../docs/ENTERPRISE_DEPLOYMENT.md) for detailed enterprise configuration.

### 2. PersistentVolumeClaim (copybook-data-pvc)

Storage for batch processing data:

- **Development**: 1Gi
- **Staging**: 5Gi
- **Production**: 10Gi

Mount structure:
```
/data/
├── copybooks/     # COBOL copybook definitions (.cpy)
├── input/         # Binary data files to decode
├── output/        # JSONL output files
└── encoded/       # Binary output from encode operations
```

### 3. Deployment (copybook-rs)

Long-lived pods for batch processing:

- **Image**: `ghcr.io/effortlessmetrics/copybook-rs:latest`
- **Replicas**: 1 (dev/staging), 2 (production)
- **Resources**:
  - Dev: 128Mi-256Mi RAM, 250m-500m CPU
  - Staging: 192Mi-384Mi RAM, 375m-750m CPU
  - Production: 256Mi-512Mi RAM, 500m-1000m CPU

### 4. Service (copybook-rs)

Internal ClusterIP service for metrics access:

- **Port 9300**: Prometheus metrics endpoint

## Usage Patterns

### Pattern 1: Long-lived Deployment with kubectl exec

Deploy persistent pods and submit work via `kubectl exec`:

```bash
# Upload data to PVC
kubectl cp customers.cpy copybook-rs-pod:/data/copybooks/
kubectl cp data.bin copybook-rs-pod:/data/input/

# Execute decode operation
kubectl exec -it copybook-rs-pod -- \
  /usr/local/bin/copybook decode \
  /data/copybooks/customers.cpy \
  /data/input/data.bin \
  --output /data/output/customers.jsonl \
  --format fixed \
  --codepage cp037

# Download results
kubectl cp copybook-rs-pod:/data/output/customers.jsonl ./customers.jsonl
```

### Pattern 2: Kubernetes Jobs (Recommended)

Use one-off Jobs for batch processing:

```bash
# Create decode job
kubectl apply -f jobs/decode-job.yaml

# Monitor job
kubectl get jobs
kubectl logs job/copybook-decode-job

# Retrieve results
kubectl cp job-pod:/data/output/customers.jsonl ./customers.jsonl
```

See `jobs/` directory for example Job manifests.

## Resource Sizing Guidelines

### Memory Requirements

copybook-rs uses streaming I/O with bounded memory:

- **Base**: 128Mi (sufficient for most workloads)
- **Large copybooks** (>1000 fields): 256Mi
- **High parallelism** (8+ threads): 512Mi
- **Steady-state**: <256 MiB for multi-GB files

### CPU Requirements

Performance scales with CPU allocation:

- **Single-threaded**: 250m-500m (sequential processing)
- **Parallel** (2-4 threads): 500m-1000m
- **High-throughput** (8 threads): 1000m-2000m

Set `RAYON_NUM_THREADS` environment variable to match CPU limits.

### Performance Expectations

Based on established baselines (commit 1fa63633):

- **DISPLAY-heavy**: 205 MiB/s (single-threaded)
- **COMP-3-heavy**: 58 MiB/s (single-threaded)
- **Parallel processing**: Up to 177 MiB/s with 8 threads

Baseline established on AMD Ryzen 9 9950X3D; Kubernetes performance may vary based on node hardware.

## Environment Overlays

### Development Overlay

Optimized for local testing:

- Debug logging (`RUST_LOG=debug`)
- Normative dialect (`COPYBOOK_DIALECT=n`)
- Latest image tag
- Minimal resources (128Mi-256Mi RAM)
- 1Gi storage

### Staging Overlay

Pre-production validation:

- Info logging (`RUST_LOG=info`)
- Zero-tolerant dialect (`COPYBOOK_DIALECT=0`)
- Versioned image (`v0.4.0`)
- Production-like resources (192Mi-384Mi RAM)
- 5Gi storage

### Production Overlay

Enterprise deployment:

- Warning logging (`RUST_LOG=warn`)
- Zero-tolerant dialect (`COPYBOOK_DIALECT=0`)
- Pinned version (`v0.4.0`)
- High resources (256Mi-512Mi RAM)
- 10Gi storage
- 2 replicas for availability

## Customization

### Change Replica Count

```bash
# Edit overlays/{env}/kustomization.yaml
replicas:
  - name: copybook-rs
    count: 3
```

### Adjust Resource Limits

```bash
# Add patch to overlays/{env}/kustomization.yaml
patches:
  - target:
      kind: Deployment
      name: copybook-rs
    patch: |-
      - op: replace
        path: /spec/template/spec/containers/0/resources/limits/memory
        value: 1Gi
```

### Change Image Version

```bash
# Edit overlays/{env}/kustomization.yaml
images:
  - name: ghcr.io/effortlessmetrics/copybook-rs
    newTag: v0.5.0
```

### Add Node Selector

```bash
# Edit deployment.yaml
spec:
  template:
    spec:
      nodeSelector:
        workload-type: batch-processing
```

## Enterprise Deployment

### Enabling Enterprise Features

Enterprise features are disabled by default in the base deployment. Enable them in environment overlays:

**Production Overlay** (enabled by default):
```yaml
# overlays/production/kustomization.yaml
configMapGenerator:
  - name: copybook-config
    behavior: merge
    literals:
      # Enterprise features - enabled for production
      - COPYBOOK_FF_AUDIT_SYSTEM=1
      - COPYBOOK_FF_SOX_COMPLIANCE=1
      - COPYBOOK_FF_GDPR_COMPLIANCE=1
      - COPYBOOK_FF_SECURITY_MONITORING=1
      - COPYBOOK_FF_LRU_CACHE=1
      - COPYBOOK_FF_ADVANCED_OPTIMIZATION=1
      - COPYBOOK_FF_PARALLEL_DECODE=1
      - COPYBOOK_FF_ZERO_COPY=1
```

**Staging Overlay** (enabled for testing):
```yaml
# overlays/staging/kustomization.yaml
configMapGenerator:
  - name: copybook-config
    behavior: merge
    literals:
      # Enterprise features - enabled for staging testing
      - COPYBOOK_FF_AUDIT_SYSTEM=1
      - COPYBOOK_FF_SOX_COMPLIANCE=1
      - COPYBOOK_FF_GDPR_COMPLIANCE=1
      - COPYBOOK_FF_SECURITY_MONITORING=1
      - COPYBOOK_FF_LRU_CACHE=1
      - COPYBOOK_FF_ADVANCED_OPTIMIZATION=1
      - COPYBOOK_FF_PARALLEL_DECODE=1
```

### Compliance Configuration

Configure compliance profiles based on regulatory requirements:

```yaml
# Enable HIPAA compliance for healthcare data
- COPYBOOK_FF_HIPAA_COMPLIANCE=1

# Enable PCI DSS compliance for payment processing
- COPYBOOK_FF_PCI_DSS_COMPLIANCE=1
```

### Audit Log Collection

Audit logs are written to stdout by default for collection by log aggregators:

```bash
# View audit logs
kubectl logs -l app=copybook-rs | grep "event_type"

# Export audit logs to file
kubectl logs -l app=copybook-rs > audit-logs.jsonl
```

For file-based audit logs, configure a sidecar container or persistent volume:

```yaml
# Add audit volume mount
volumeMounts:
  - name: audit-logs
    mountPath: /var/log/copybook/audit

volumes:
  - name: audit-logs
    persistentVolumeClaim:
      claimName: copybook-audit-pvc
```

### Metrics and Monitoring

Prometheus metrics are exposed on port 9300:

```bash
# Access metrics
kubectl port-forward svc/copybook-rs 9300:9300
curl http://localhost:9300/metrics
```

Key enterprise metrics:
- `copybook_audit_events_total`: Total audit events
- `copybook_compliance_violations_total`: Compliance violations by profile
- `copybook_security_events_total`: Security-related events

### Performance Impact

Enterprise features add minimal overhead (< 5% combined):

| Feature | Overhead | Production Status |
|---------|----------|------------------|
| Audit | < 2% | Enabled |
| Compliance | < 3% | Enabled |
| Security | < 1% | Enabled |
| LRU Cache | Negative (improves) | Enabled |
| Advanced Optimization | Negative (improves) | Enabled |

> **Note**: See [Enterprise Deployment Guide](../../docs/ENTERPRISE_DEPLOYMENT.md) for detailed enterprise configuration, performance considerations, and upgrade practices.

## Monitoring

### Prometheus Metrics

The service exposes Prometheus metrics on port 9300:

- **Endpoint**: `http://copybook-rs:9300/metrics`
- **Annotations**: Pre-configured for Prometheus scraping

Example metrics:
- `copybook_records_decoded_total`
- `copybook_decode_duration_seconds`
- `copybook_decode_throughput_bytes_per_second`

### Health Checks

Liveness and readiness probes use `copybook --version`:

```yaml
livenessProbe:
  exec:
    command: ["/usr/local/bin/copybook", "--version"]
  initialDelaySeconds: 10
  periodSeconds: 30
```

## Troubleshooting

### Pod Not Starting

```bash
# Check pod status
kubectl describe pod -l app=copybook-rs

# Check logs
kubectl logs -l app=copybook-rs --tail=50

# Common issues:
# - PVC not bound: Check storage class availability
# - Image pull errors: Verify GHCR access
# - Resource limits: Node may not have sufficient capacity
```

### Out of Memory

```bash
# Check memory usage
kubectl top pod -l app=copybook-rs

# Solutions:
# - Increase memory limits in overlay patches
# - Reduce RAYON_NUM_THREADS
# - Process smaller batches
```

### Performance Issues

```bash
# Check CPU throttling
kubectl top pod -l app=copybook-rs

# Solutions:
# - Increase CPU limits
# - Set RAYON_NUM_THREADS to match CPU allocation
# - Use production overlay for higher resources
```

## Migration from Existing Deployment

The existing `deploy/staging-copybook-cli.yaml` can coexist with this structure:

```bash
# Keep existing staging deployment
kubectl apply -f ../staging-copybook-cli.yaml

# Deploy new Kustomize-based configuration
kubectl apply -k overlays/staging/
```

Key differences:
- New deployment uses PVC for persistent storage
- ConfigMap-based configuration management
- Environment-specific overlays
- Support for both long-lived and Job-based patterns

## Security Considerations

### Container Security

- **Non-root user**: Consider adding `securityContext.runAsNonRoot: true`
- **Read-only filesystem**: Consider `securityContext.readOnlyRootFilesystem: true`
- **Drop capabilities**: Add `securityContext.capabilities.drop: ["ALL"]`

### Network Policies

Restrict network access to copybook-rs pods:

```yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: copybook-rs-netpol
spec:
  podSelector:
    matchLabels:
      app: copybook-rs
  policyTypes:
    - Ingress
    - Egress
  ingress:
    - from:
        - podSelector:
            matchLabels:
              app: prometheus
      ports:
        - port: 9300
```

### RBAC

Create ServiceAccount with minimal permissions:

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: copybook-rs-sa
---
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  name: copybook-rs-role
rules:
  - apiGroups: [""]
    resources: ["persistentvolumeclaims"]
    verbs: ["get", "list"]
```

## References

- [copybook-rs Documentation](https://github.com/EffortlessMetrics/copybook-rs)
- [CLAUDE.md](../../CLAUDE.md) - Project development guidelines
- [Performance Baselines](../../copybook-bench/BASELINE_METHODOLOGY.md)
- [Kubernetes Kustomize](https://kustomize.io/)
- [Kubernetes Jobs](https://kubernetes.io/docs/concepts/workloads/controllers/job/)
