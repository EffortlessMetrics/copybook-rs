## Performance Validation Summary

| Check | Result | Target | Delta | Status | Notes |
|-------|--------|--------|-------|--------|-------|
| slo_validation/display_heavy_slo_80mbps | 169.19 MiB/s | 80.0 MiB/s | +111% | ✅ pass | Mean throughput from `scripts/bench/perf.json` (criterion `slo_validation` group) |
| slo_validation/comp3_heavy_slo_40mbps | 49.48 MiB/s | 40.0 MiB/s | +24% | ✅ pass | Mean throughput from `scripts/bench/perf.json` (criterion `slo_validation` group) |

### Gate Decision
- Verdict: ✅ Ready – throughput floors cleared with comfortable headroom (DISPLAY 169.19 MiB/s, COMP-3 49.48 MiB/s)
- Evidence: `scripts/bench/perf.json` (generated via `scripts/bench.sh`), local `cargo bench -p copybook-bench -- slo_validation --quiet` output

### Observations
- Measurements captured on native CPU with `RUSTFLAGS="-C target-cpu=native"` to mirror CI expectations.
- Receipts follow the 90-day retention policy (`scripts/bench/perf.json`) and are ready for artifact upload.
- Criterion directories at `target/criterion/slo_validation/*` hold the raw estimates (mean ns and bytes processed) backing the JSON report.

### Recommended Follow-Up
- Attach `scripts/bench/perf.json` to the PR once CI uploads artifacts.
- Run `cargo run --bin bench-report -p copybook-bench -- summary` after merge to update baselines as needed.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
