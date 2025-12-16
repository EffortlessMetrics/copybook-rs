## Performance Validation Summary

| Check | Result | Target | Delta | Status | Notes |
|-------|--------|--------|-------|--------|-------|
| slo_validation/display_heavy_slo_80mbps | 3597.91 MiB/s (3.51 GiB/s) | 80.0 MiB/s | +4397% | ✅ pass | Measured in `scripts/bench/perf.json` (generated via `scripts/bench.sh`) |
| slo_validation/comp3_heavy_slo_40mbps | 29.95 MiB/s | 40.0 MiB/s | -25% | ❌ fail | Measured in `scripts/bench/perf.json`; current run below COMP-3 floor |

### Gate Decision
- Verdict: ⚠️ Mixed — DISPLAY comfortably clears the 80 MiB/s floor; COMP-3 remains below the 40 MiB/s floor. Treat numbers as environment-specific until CI gating is reinstated.
- Evidence: `scripts/bench/perf.json` (generated via `scripts/bench.sh`), local `cargo bench -p copybook-bench -- slo_validation --quiet` output

### Observations
- Measurements captured with `RUSTFLAGS="-C target-cpu=native"`; numbers vary by environment and should be treated as advisory (especially COMP-3).
- Receipts follow the 90-day retention policy (`scripts/bench/perf.json`) and are ready for artifact upload.
- Criterion directories at `target/criterion/slo_validation/*` hold the raw estimates (mean ns and bytes processed) backing the JSON report.

### Recommended Follow-Up
- Attach `scripts/bench/perf.json` to the PR once CI uploads artifacts.
- Run `cargo run --bin bench-report -p copybook-bench -- summary` after merge to update baselines as needed.
