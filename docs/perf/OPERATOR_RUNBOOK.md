<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Benchmark Container Operator Runbook

**Document Version**: 1.0
**Last Updated**: 2025-11-14
**Audience**: DevOps, SREs, Performance Engineers

## Overview

The copybook-rs benchmark container provides a **reproducible, isolated environment** for running performance benchmarks and generating perf receipts. This container ensures consistent measurements across different machines and CI environments.

**Use Cases**:
- Validate performance on specific hardware configurations
- Compare throughput across different environments (cloud vs on-prem)
- Establish baseline metrics for capacity planning
- Regression testing in CI/CD pipelines
- Performance validation before production deployment

**Container Contents**:
- Rust 1.92 toolchain (matches MSRV)
- Pre-built copybook-bench crate with criterion benchmarks
- Performance measurement scripts (bench.sh, perf-annotate-host.sh)
- Runtime dependencies (Python 3, jq, git, time)

**Outputs**:
- `perf.json` - Structured performance receipts with throughput metrics
- Console summary with SLO comparison

---

## Prerequisites

### Required Software
- Docker 20.10+ or Podman 3.0+
- 4+ CPU cores (recommended 8+ for representative results)
- 8 GB RAM minimum
- 10 GB free disk space for image and build cache

### Optional Tools
- `jq` - For parsing perf.json receipts locally
- `docker-compose` - For orchestrated benchmark runs

### Hardware Considerations

**Performance measurements are hardware-dependent**. The baseline was established on:
- AMD Ryzen 9 9950X3D (32 threads)
- 196 GiB RAM
- WSL2/Ubuntu 22.04

Your results will vary based on:
- CPU architecture and clock speed
- Memory bandwidth
- Container runtime (Docker vs Podman)
- Host OS and kernel version
- Background processes and system load

**Best Practice**: Run benchmarks on idle systems with minimal background processes for consistent results.

---

## Quick Start

### 1. Pull Pre-built Image (Recommended)

If available from GitHub Container Registry:

```bash
docker pull ghcr.io/effortlessmetrics/copybook-rs/bench:latest
docker run -v $(pwd)/output:/workspace/output ghcr.io/effortlessmetrics/copybook-rs/bench:latest
cat output/perf.json
```

### 2. Build from Source

From the copybook-rs repository root:

```bash
# Build the container
docker build -t copybook-rs-bench .

# Run benchmarks
mkdir -p output
docker run -v $(pwd)/output:/workspace/output copybook-rs-bench

# View results
cat output/perf.json
```

**Build time**: ~10-15 minutes (first build), ~2-3 minutes (incremental)
**Run time**: ~5-10 minutes (depending on hardware)

---

## Understanding the Output

### perf.json Structure

```json
{
  "timestamp": "2025-11-14T12:34:56Z",
  "commit": "abc1234",
  "toolchain": "cargo bench (criterion)",
  "status": "pass",
  "display_mibps": 205.4,
  "display_gibps": 0.200,
  "comp3_mibps": 58.2,
  "benchmarks": [
    {
      "name": "slo_validation/display_heavy_slo_80mbps",
      "mean_ns": 48756123.45,
      "bytes_processed": 10485760,
      "mean_mibps": 205.4
    },
    {
      "name": "slo_validation/comp3_heavy_slo_40mbps",
      "mean_ns": 180234567.89,
      "bytes_processed": 10485760,
      "mean_mibps": 58.2
    }
  ],
  "summary": {
    "display_mibps": 205.4,
    "comp3_mibps": 58.2,
    "max_rss_mib": 128,
    "host_cpu": "AMD Ryzen 9 9950X3D",
    "host_cores": 32,
    "host_kernel": "6.6.87.2-microsoft-standard-WSL2",
    "host_os": "Linux"
  }
}
```

### Key Metrics

| Metric | Description | SLO Threshold | Baseline (v0.3.1) |
|--------|-------------|---------------|-------------------|
| `display_mibps` | DISPLAY-heavy workload throughput | ≥ 80 MiB/s | 205 MiB/s |
| `comp3_mibps` | COMP-3-heavy workload throughput | ≥ 40 MiB/s | 58 MiB/s |
| `max_rss_mib` | Maximum resident memory | ≤ 256 MiB | ~128 MiB |

### Interpreting Results

**Good Performance** (meets SLOs):
```
DISPLAY: 205.0 MiB/s (SLO 80 MiB/s, +156.2%)
COMP-3:  58.0 MiB/s (SLO 40 MiB/s, +45.0%)
✓ All SLOs met
```

**Degraded Performance** (below SLO):
```
DISPLAY: 65.0 MiB/s (SLO 80 MiB/s, -18.8%)
COMP-3:  32.0 MiB/s (SLO 40 MiB/s, -20.0%)
⚠ SLOs not met
```

**Action Items**:
- Below SLO: Investigate system load, CPU throttling, or regressions
- Significantly above SLO: Consider raising thresholds or hardware upgrade
- High variance (>10%): Reduce background processes, check thermal throttling

---

## Advanced Usage

### Custom Benchmark Filters

Run specific benchmark suites:

```bash
# Only DISPLAY benchmarks
docker run -e BENCH_FILTER="display" \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench

# Only COMP-3 benchmarks
docker run -e BENCH_FILTER="comp3" \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench

# All benchmarks (not just SLO validation)
docker run -e BENCH_FILTER="" \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench
```

### Resource Constraints

Limit CPU and memory for capacity planning:

```bash
# 4 cores, 4GB RAM
docker run --cpus=4 --memory=4g \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench

# Single core (worst-case scenario)
docker run --cpus=1 --memory=2g \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench
```

### Extracting Detailed Logs

Preserve full criterion output:

```bash
docker run -v $(pwd)/output:/workspace/output \
  copybook-rs-bench 2>&1 | tee benchmark-run.log
```

### Running on Specific Commit

Build from a specific git commit:

```bash
git checkout v0.3.1
docker build -t copybook-rs-bench:v0.3.1 .
docker run -v $(pwd)/output:/workspace/output copybook-rs-bench:v0.3.1
```

---

## Troubleshooting

### Container Build Fails

**Symptom**: `cargo fetch` or `cargo build` errors during build

**Common Causes**:
- Network connectivity issues
- Rust toolchain mismatch
- Insufficient disk space

**Solutions**:
```bash
# Clear Docker build cache
docker builder prune -af

# Check disk space (need 10+ GB free)
df -h

# Verify network access
curl -I https://crates.io
```

### Benchmarks Fail to Run

**Symptom**: Container exits with non-zero code, no perf.json generated

**Debugging**:
```bash
# Run container interactively
docker run -it --entrypoint /bin/bash \
  -v $(pwd)/output:/workspace/output \
  copybook-rs-bench

# Inside container:
bash scripts/bench.sh  # Run manually
```

**Common Issues**:
- Missing test fixtures: Check `copybook-bench/test_fixtures/`
- Permission errors: Ensure output volume is writable
- OOM killer: Increase container memory limit

### Results Don't Match Baseline

**Symptom**: Throughput significantly lower than baseline (>30% difference)

**Checklist**:
1. ✅ System idle (check `top`, `htop`)
2. ✅ CPU turbo/boost enabled (check `/proc/cpuinfo`)
3. ✅ Not running in nested virtualization
4. ✅ Docker using native runtime (not qemu)
5. ✅ Sufficient free memory (no swapping)

**Architecture Differences**:
- ARM vs x86-64: Expect 20-50% variance
- Virtualized environments: 10-30% overhead
- Cloud instances: High variance due to noisy neighbors

### perf.json Missing or Incomplete

**Symptom**: Output directory exists but perf.json not created

**Verification**:
```bash
# Check if volume mount worked
docker run -v $(pwd)/output:/workspace/output \
  --entrypoint ls \
  copybook-rs-bench -la /workspace/output

# Check if benchmarks ran
docker run -v $(pwd)/output:/workspace/output \
  copybook-rs-bench 2>&1 | grep "✅ receipts"
```

---

## Integration Patterns

### CI/CD Pipeline Integration

**GitHub Actions**:
See `.github/workflows/perf-container.yml` for reference implementation.

**GitLab CI**:
```yaml
benchmark:
  image: docker:latest
  services:
    - docker:dind
  script:
    - docker build -t bench .
    - mkdir -p output
    - docker run -v $(pwd)/output:/workspace/output bench
  artifacts:
    paths:
      - output/perf.json
    expire_in: 90 days
```

**Jenkins**:
```groovy
pipeline {
  agent any
  stages {
    stage('Benchmark') {
      steps {
        sh 'docker build -t copybook-rs-bench .'
        sh 'mkdir -p output'
        sh 'docker run -v $(pwd)/output:/workspace/output copybook-rs-bench'
        archiveArtifacts artifacts: 'output/perf.json'
      }
    }
  }
}
```

### Scheduled Performance Monitoring

**Cron-based execution**:
```bash
#!/bin/bash
# /etc/cron.daily/copybook-benchmark
set -euo pipefail

OUTPUT_DIR="/var/lib/perf-receipts/$(date +%Y-%m-%d)"
mkdir -p "$OUTPUT_DIR"

docker pull ghcr.io/effortlessmetrics/copybook-rs/bench:latest
docker run -v "$OUTPUT_DIR:/workspace/output" \
  ghcr.io/effortlessmetrics/copybook-rs/bench:latest

# Upload to metrics system
curl -X POST https://metrics.example.com/receipts \
  -H "Content-Type: application/json" \
  -d @"$OUTPUT_DIR/perf.json"
```

### Regression Detection

**Compare against baseline**:
```bash
#!/bin/bash
# Check for performance regressions

BASELINE_DISPLAY=205.0
BASELINE_COMP3=58.0
THRESHOLD_PCT=10  # Fail if >10% regression

docker run -v $(pwd)/output:/workspace/output copybook-rs-bench

DISPLAY=$(jq -r '.display_mibps' output/perf.json)
COMP3=$(jq -r '.comp3_mibps' output/perf.json)

# Calculate regression
DISPLAY_DELTA=$(echo "scale=2; (($BASELINE_DISPLAY - $DISPLAY) / $BASELINE_DISPLAY) * 100" | bc)
COMP3_DELTA=$(echo "scale=2; (($BASELINE_COMP3 - $COMP3) / $BASELINE_COMP3) * 100" | bc)

if (( $(echo "$DISPLAY_DELTA > $THRESHOLD_PCT" | bc -l) )); then
  echo "❌ DISPLAY regression: ${DISPLAY_DELTA}%"
  exit 1
fi

if (( $(echo "$COMP3_DELTA > $THRESHOLD_PCT" | bc -l) )); then
  echo "❌ COMP-3 regression: ${COMP3_DELTA}%"
  exit 1
fi

echo "✅ No significant regressions detected"
```

---

## Maintenance

### Updating the Container

When updating copybook-rs source:

```bash
# Pull latest code
git pull origin main

# Rebuild container
docker build --no-cache -t copybook-rs-bench .

# Verify new build
docker run -v $(pwd)/output:/workspace/output copybook-rs-bench
```

### Managing Image Storage

```bash
# List images
docker images | grep copybook-rs-bench

# Remove old images
docker image prune -af

# Remove specific version
docker rmi copybook-rs-bench:old-tag
```

### Security Updates

Container includes:
- Rust toolchain (updates with base image)
- Debian packages (apt-get update in Dockerfile)

**Update cadence**: Monthly or when security advisories published

```bash
# Rebuild with latest base image
docker build --pull --no-cache -t copybook-rs-bench .
```

---

## Performance Baselines

### Reference Hardware (v0.3.1 Baseline)

**Environment**:
- CPU: AMD Ryzen 9 9950X3D (32 threads @ 4.7 GHz)
- RAM: 196 GiB DDR5
- OS: WSL2/Ubuntu 22.04 (Linux 6.6.87.2)
- Date: 2025-09-30
- Commit: 1fa63633

**Results**:
- DISPLAY: 205 MiB/s (2.56x SLO)
- COMP-3: 58 MiB/s (1.45x SLO)
- Max RSS: ~128 MiB

**Variance**: ±5% (DISPLAY), ±8% (COMP-3) across 5 runs

### Expected Results by Hardware Class

| Hardware Class | DISPLAY (MiB/s) | COMP-3 (MiB/s) | Notes |
|----------------|-----------------|----------------|-------|
| High-end desktop (8+ cores, 3.5+ GHz) | 180-220 | 50-65 | Reference baseline |
| Mid-range server (16+ cores, 2.5+ GHz) | 150-200 | 45-60 | Good cloud instance equivalent |
| Cloud VM (4 cores, burstable) | 80-120 | 30-45 | May meet minimum SLO |
| Low-end/virtualized (2 cores) | 40-80 | 20-35 | Below SLO, not recommended |

---

## References

- [Baseline Methodology](../../copybook-bench/BASELINE_METHODOLOGY.md) - How baselines are established
- [Hardware Specifications](../../copybook-bench/HARDWARE_SPECS.md) - Reference hardware details
- [Performance Report](../REPORT.md) - Comprehensive performance analysis
- [GitHub Workflow](.github/workflows/perf-container.yml) - Automated CI integration

---

## Support

**Issues**: https://github.com/EffortlessMetrics/copybook-rs/issues
**Discussions**: https://github.com/EffortlessMetrics/copybook-rs/discussions

For performance-related questions, include:
- perf.json receipt
- Container build logs
- Host hardware specs (`lscpu`, `free -h`)
- Docker/Podman version (`docker version`)
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
