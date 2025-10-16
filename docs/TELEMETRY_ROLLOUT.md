# Telemetry Rollout Guide

## Build & run (local)

```bash
cargo build -p copybook-cli --features metrics --release
./target/release/copybook decode \
  --metrics-listen 127.0.0.1:9300 \
  --codepage cp037 --format rdw \
  --input ./data/sample.rdw --output /dev/null
curl -s http://127.0.0.1:9300/metrics | rg '^copybook_'
```

## Prometheus scrape (staging)

```yaml
- job_name: 'copybook-cli'
  static_configs:
  - targets: ['copybook-cli:9300']   # replace with service DNS
```

## Grafana panels (queries)

* **QPS:** `sum(rate(copybook_records_total[5m])) by (format, codepage)`
* **Bytes/s:** `sum(rate(copybook_bytes_total[5m])) by (format, codepage)`
* **Error rate:** `sum(rate(copybook_decode_errors_total[5m])) by (family)`
* **MiB/s:** `avg(copybook_throughput_mibps) by (format, codepage)`

## Alerts (lightweight)

* **Error burst:** `sum(rate(copybook_decode_errors_total[5m])) > 0`
* **Throughput drop:** `avg_over_time(copybook_throughput_mibps[15m]) < FLOOR`

## Perf sanity

```bash
BENCH_FILTER=slo_validation bash scripts/bench.sh
jq '.summary | {display_mibps, comp3_mibps, max_rss_mib}' scripts/bench/perf.json
```

## Notes

* Metrics **off** by default; exporter binds only with `--metrics-listen`.
* Labels are bounded: `format`, `codepage`, `zero_policy` (and `family` for errors). Avoid high-cardinality labels (paths, schema IDs); use logs/run summary instead.

