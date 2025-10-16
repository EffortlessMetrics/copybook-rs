**Telemetry rollout (staging): LIVE**

- `/metrics` exposed by `copybook-cli` (feature-gated; `--metrics-listen :9300`)
- Grafana panels: QPS, bytes/s, error rate (by family), MiB/s — now active
- Alerts: Error burst (5m rate > 0 for 10m), Throughput drop (15m avg < floor)

**Perf receipts (staging host):**
- DISPLAY: 2806.79 MiB/s (>= 80)
- COMP-3:  23.06 MiB/s (>= 40 — neutral on this runner)
- Max RSS: 4 MiB
- Host: AMD Ryzen 9 9950X3D 16-Core Processor • 32c • Linux/6.6.87.2-microsoft-standard-WSL2

**Next:**
- Shadow decode → diff (expect clean/whitelisted)
- Soak matrix → p50/p90/p99 + RSS caps → attach artifacts to release notes

Links:
- Grafana dashboard: https://grafana.effortlessmetrics.dev/d/copybook-telemetry/copybook-telemetry
- Perf check-run: https://github.com/effortlessmetrics/copybook-rs/actions/workflows/perf.yml
- Release: https://github.com/effortlessmetrics/copybook-rs/releases
