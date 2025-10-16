# Telemetry Rollout Guide

## Build the metrics-enabled image

```bash
cargo build -p copybook-cli --features metrics --release
docker build -t ghcr.io/effortlessmetrics/copybook-cli:latest-metrics .
```

Run locally to sanity check the exporter:

```bash
./target/release/copybook decode \
  --metrics-listen 127.0.0.1:9300 \
  --codepage cp037 --format rdw \
  --input ./data/sample.rdw --output /dev/null
curl -s http://127.0.0.1:9300/metrics | rg '^copybook_'
```

## Kubernetes deployment (staging)

Apply `deploy/staging-copybook-cli.yaml` to roll out a single metrics-enabled pod. It exposes `/metrics` on port `9300` and wires Prometheus scrape annotations so you can rely on either annotation-based or static service discovery.

```bash
kubectl apply -f deploy/staging-copybook-cli.yaml
```

## Prometheus scrape options

* **Annotations:** Already on the Deployment manifest above.
* **Static config:** If you need an explicit scrape job, drop `deploy/prometheus.copybook-cli-staging.yml` into your `prometheus.yml`:

```yaml
- job_name: copybook-cli-staging
  metrics_path: /metrics
  scrape_interval: 15s
  static_configs:
    - targets:
        - copybook-cli.staging.svc.cluster.local:9300
```

## Grafana dashboards & alerts

* Import `grafana/dashboards/copybook-telemetry.json`.
* Add alert rules from `grafana/alerts/copybook-cli.yaml` to your rule group. The alerts cover **error bursts** (5m rate > 0 for 10m) and **throughput drops** (15m avg MiB/s < floor).

Suggested queries for panel wiring:

* `sum(rate(copybook_records_total[5m])) by (format, codepage)` — QPS
* `sum(rate(copybook_bytes_total[5m])) by (format, codepage)` — Bytes/s
* `sum(rate(copybook_decode_errors_total[5m])) by (family)` — Error rate
* `avg(copybook_throughput_mibps) by (format, codepage)` — MiB/s

## Perf receipts & CI integration

GitHub Actions (`.github/workflows/perf.yml`) uploads `scripts/bench/perf.json` for 90 days, annotates host metadata via `scripts/perf-annotate-host.sh`, and publishes a neutral check-run badge when thresholds are not met. Thresholds currently expect:

* DISPLAY ≥ 80 MiB/s
* COMP-3 ≥ 40 MiB/s
* Max RSS ≤ 256 MiB

## Operator runbook

```bash
# Perf receipts (staging host)
BENCH_FILTER=slo_validation bash scripts/bench.sh
bash scripts/perf-annotate-host.sh
jq '.summary' scripts/bench/perf.json

# Shadow compare
bash scripts/shadow-diff.sh /path/current.jsonl /path/copybook.jsonl

# Soak dispatch
bash scripts/soak-dispatch.sh
```

## See also

- **Launch checklist (operators):** [`docs/telemetry-launch-checklist.md`](telemetry-launch-checklist.md) — copy-paste runbook for staging build/deploy, dashboards/alerts, perf receipts, shadow/soak, and the roadmap flip.

## Notes

* Metrics remain feature-gated; exporter binds only with `--metrics-listen`.
* Labels stay low-cardinality (`format`, `codepage`, `zero_policy`, `family`).
* Use `scripts/roadmap-phase6-done.sh` when the staging rollout checks are complete—this script flips `docs/ROADMAP.md` and opens a PR scaffold.
