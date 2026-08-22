<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# `copybook-bench`

Benchmark harness and baseline tooling for `copybook-rs`.

`copybook-bench` holds deterministic performance checks, baseline workflows, and issue-specific
regression suites used by CI and release validation.

## What it does

- Define and run Criterion benches (`decode_performance`, `comp3`, etc.).
- Run regression/acceptance test suites in CI-like conditions.
- Produce performance receipts used by gating and reporting scripts.
- Support optional modes:
  - `progressive` for complexity ramp-up scenarios
  - `diagnostics` for infrastructure overhead measurement
  - `perf` for CI-style performance runs

## Quick start

```bash
cargo bench -p copybook-bench
cargo bench -p copybook-bench --bench comp3
cargo test -p copybook-bench --features perf

COPYBOOK_EXTERNAL_INPUT_MANIFEST="$(pwd)/tools/copybook-bench/test_fixtures/external_input/fixed-ascii.json" \
  cargo bench -p copybook-bench --features external-input --bench external_input_decode

cargo test -p copybook-bench --test baseline_reconciliation
cargo test -p copybook-bench --test regression_detection
cargo test -p copybook-bench --test ci_integration
```

The external-input target is an opt-in diagnostic measurement over one
validated manifest. It can run locally or through the manual
`.github/workflows/external-input-criterion.yml` workflow for the four checked-in
manifests. Both the `external-input` Cargo feature and the manifest environment
variable are required, so ordinary benchmark suites do not launch it. It
reports payload-byte throughput and does not define a threshold, SLO, receipt,
or scheduled performance claim.

To pair an existing generated dataset with its copybook, create a deterministic
schema-valid manifest beside both inputs:

```bash
cargo run -p copybook-bench --bin gen-external-input-manifest -- \
  --copybook record.cpy --dataset records.bin --format fixed --codepage ascii \
  --workload mixed --record-length 256 --output records.json
```

The utility validates framing, records exact copybook/dataset SHA-256 values,
and emits only relative paths. It describes input data; it does not run a
benchmark or establish a performance claim.

## Historical performance context

The following legacy values are historical context from the 2025-09-30
measurement (commit `1fa63633`), not current external-input results or SLOs:

| Workload | Baseline | CI Floor |
|----------|----------|----------|
| DISPLAY-heavy | 205 MiB/s | 80 MiB/s |
| COMP-3-heavy | 58 MiB/s | 40 MiB/s |

Current performance claims and CI thresholds are governed by
[`scripts/bench/perf.json`](../../scripts/bench/perf.json),
[`scripts/bench/baseline.json`](../../scripts/bench/baseline.json), and
[`docs/PERFORMANCE_GOVERNANCE.md`](../../docs/PERFORMANCE_GOVERNANCE.md).
The external-input target above remains manual diagnostic telemetry only,
whether run locally or by the hosted workflow; it does not promote a baseline
or establish a threshold, SLO, receipt, or scheduled claim.
See [`BASELINE_METHODOLOGY.md`](BASELINE_METHODOLOGY.md) and
[`docs/PERFORMANCE_GOVERNANCE.md`](../../docs/PERFORMANCE_GOVERNANCE.md) for
the separate canonical baseline procedure and the evidence promotion boundary.
Any future promotion requires pinned input/environment identity, raw
integrity-checked measurements, independent-run variance evidence, baseline
review, and a separately approved threshold or SLO decision. The current
checked-in manifests do not satisfy that promotion boundary; generated
representativeness and scheduled execution remain separate follow-ups.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
