# Telemetry Phase 6 Operator Checklist

Use this checklist when promoting the telemetry exporter to *launch-ready*. Each section pairs the exact commands with what to verify before checking the box.

## 1. Build & deploy (staging)

```bash
cargo build -p copybook-cli --features metrics --release
docker build -t ghcr.io/effortlessmetrics/copybook-cli:latest-metrics .
docker push ghcr.io/effortlessmetrics/copybook-cli:latest-metrics
kubectl apply -f deploy/staging-copybook-cli.yaml
# OR: kubectl apply -f deploy/prometheus.copybook-cli-staging.yml
```

Verify:

```bash
kubectl -n staging rollout status deploy/copybook-cli
kubectl -n staging port-forward deploy/copybook-cli 9300:9300 &
curl -s http://127.0.0.1:9300/metrics | head
```

## 2. Grafana + alerts

- Import the dashboard JSON in `grafana/` (no edits needed before import).
- Apply alerts:

  ```bash
  kubectl -n monitoring apply -f grafana/alerts/copybook-cli.yaml
  ```

- Run a decode with `--metrics-grace-ms 2000`; expect the dashboard QPS/bytes/s/MiB/s panels to move.
- Induce a single malformed RDW to ensure `copybook_decode_errors_total{family="CBKF"}` increments and **Error burst** alert fires.

## 3. Receipts & badges (neutral gate)

```bash
BENCH_FILTER=slo_validation bash scripts/bench.sh
bash scripts/perf-annotate-host.sh
```

- Confirm the `scripts/bench/perf.json` artifact uploads under **Actions → Perf Receipts**.
- Check the Throughput badge populates host CPU/OS info and presents as **neutral** if the runner misses SLOs.
- Validate the job summary includes host metadata.

## 4. Shadow + soak

Shadow diff:

```bash
bash scripts/shadow-diff.sh /path/current.jsonl /path/copybook.jsonl
```

- Review the first 200 diff lines; whitelist expected changes.

Soak run:

```bash
bash scripts/soak-dispatch.sh
```

- After completion, confirm p50/p90/p99 metrics and **Max RSS ≤ 256 MiB** in each artifact.

## 5. Flip ROADMAP + notify stakeholders

```bash
bash scripts/roadmap-phase6-done.sh
```

- Open the PR for the generated branch and add live dashboard + release links.
- Paste `docs/templates/stakeholder-update-telemetry.md` into your stakeholder update, filling in:
  - Grafana dashboard URL
  - Latest perf summary (`perf.json.summary`)
  - Policy note: telemetry is feature/flag gated; hot-path performance unchanged by default.

## 6. Definition of Done (sign-off)

- [ ] Exporter **UP** in staging; `/metrics` returns content; panels/alerts verified.
- [ ] Perf receipts uploaded with host metadata; badge posted, non-blocking.
- [ ] Shadow diff clean/whitelisted; soak percentiles & RSS captured.
- [ ] ROADMAP Phase 6 marked **done**; stakeholder update sent.

## 7. Optional nice-to-haves (post-launch)

1. Add a weekly cron to `metrics-smoke.yml` so exporter regressions surface early.
2. Echo soak percentiles to `$GITHUB_STEP_SUMMARY` inside `soak.yml` for easy review.
3. Keep the perf gate neutral across heterogeneous runners, but always publish the artifact.

---

_Last verified against commit: <GIT_SHORT_SHA>_
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
