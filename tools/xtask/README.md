<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# `xtask`

Internal build/maintenance tasks for `copybook-rs`.

`xtask` is a local CLI used by CI and release workflows to keep repository state synchronized.

## What it does

- Sync test status and support-matrix docs from CI artifacts.
- Regenerate public Shields endpoint JSON for README badges.
- Produce and validate PR-scoped RIPR evidence artifacts.
- Validate repository reports and metadata consistency.
- Run perf workflows and generate baseline receipts (`perf.json`, `meta.json`).
- Produce last-run benchmark summaries for PR and release checks.

## Commands

```bash
cargo xtask badges
cargo xtask badges --check
cargo run -p xtask -- docs sync-tests
cargo run -p xtask -- docs verify-tests
cargo run -p xtask -- docs verify-support-matrix
cargo run -p xtask -- perf --out-dir target/custom-bench
cargo run -p xtask -- perf --summarize-last
cargo xtask ripr-pr
cargo xtask ripr-pr --check
cargo xtask ripr-review-comments
cargo xtask ripr-review-comments --check
```

## Outputs

- `badges/ripr-plus.json`
- `target/ripr/pr/repo-exposure.json` and `target/ripr/pr/repo-exposure.md`
- `target/ripr/review/comments.json` and `target/ripr/review/comments.md`
- `target/benchmarks/<timestamp>/perf.json`
- `target/benchmarks/<timestamp>/meta.json`
- Updated report blocks in docs when supported by the command.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
