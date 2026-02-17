# `xtask`

Internal build/maintenance tasks for `copybook-rs`.

`xtask` is a local CLI used by CI and release workflows to keep repository state synchronized.

## What it does

- Sync test status and support-matrix docs from CI artifacts.
- Validate repository reports and metadata consistency.
- Run perf workflows and generate baseline receipts (`perf.json`, `meta.json`).
- Produce last-run benchmark summaries for PR and release checks.

## Commands

```bash
cargo run -p xtask -- docs sync-tests
cargo run -p xtask -- docs verify-tests
cargo run -p xtask -- docs verify-support-matrix
cargo run -p xtask -- perf --out-dir target/custom-bench
cargo run -p xtask -- perf --summarize-last
```

## Outputs

- `target/benchmarks/<timestamp>/perf.json`
- `target/benchmarks/<timestamp>/meta.json`
- Updated report blocks in docs when supported by the command.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
