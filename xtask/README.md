# `xtask`

`xtask` contains repository maintenance and CI-adjacent automation for copybook-rs.

## Purpose

- Sync generated test status into docs (`README.md` and `docs/REPORT.md`).
- Verify test-status blocks and support-matrix consistency.
- Run and summarize performance benchmarks.
- Emit and validate performance receipts (`perf.json` and `meta.json`).

## Commands

```bash
# Synchronize test status from target/nextest/junit.xml
cargo run -p xtask -- docs sync-tests

# Verify docs are in sync with latest junit output
cargo run -p xtask -- docs verify-tests

# Verify support matrix registry is documented in docs
cargo run -p xtask -- docs verify-support-matrix

# Run perf benchmarks
cargo run -p xtask -- perf

# Run perf with SLO hard enforcement
cargo run -p xtask -- perf --enforce

# Write benchmark receipts to a custom output directory
cargo run -p xtask -- perf --out-dir target/custom-bench

# Print a summary from the latest perf receipt
cargo run -p xtask -- perf --summarize-last
```

## Outputs

- `target/benchmarks/<timestamp>/perf.json`
- `target/benchmarks/<timestamp>/meta.json`
- Updated coverage status blocks in docs where expected.

## Implementation Notes

Tasks are intentionally small and composable so CI workflows can call `xtask` for deterministic
maintenance steps.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
