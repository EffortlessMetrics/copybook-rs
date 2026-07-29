<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Repository Agent Guide

This is the tool-neutral maintenance contract for `copybook-rs`. The closest
`AGENTS.md` applies to work in its directory and descendants; scoped guides
also exist under `crates/`, `tests/`, `tools/`, `examples/`, and `fuzz/`.

## Project overview

`copybook-rs` is a Rust toolkit for COBOL copybook parsing and deterministic
fixed-record data conversion between mainframe binary formats (EBCDIC/ASCII,
fixed-length and RDW records) and JSON. It is currently at Engineering Preview
(v0.5.0): stable CLI and library APIs, preview-level feature completeness.

- **Repository**: https://github.com/EffortlessMetrics/copybook-rs
- **Language / toolchain**: Rust, edition 2024, MSRV 1.92 (enforced in CI and
  `Cargo.toml`); license AGPL-3.0-or-later.
- **Deliverables**: the `copybook` CLI binary (crate `copybook-cli`), the
  `copybook` facade library crate, and granular published crates
  (`copybook-core`, `copybook-codec`, and supporting microcrates).
- **Status docs**: `docs/ROADMAP.md` (adoption guidance, known limitations),
  `docs/REPORT.md` (engineering status).

## Repository layout

- `crates/` — all library/CLI workspace crates (see Architecture below).
- `tools/` — dev-only tooling: `copybook-gen` (fixture/synthetic data
  generation), `copybook-bench` (criterion benchmarks + perf receipts),
  `copybook-scripts`, `xtask` (automation).
- `tests/` — workspace test crates: `bdd/` (Cucumber/Gherkin), `e2e/` (CLI
  end-to-end), `proptest/` (property tests); plus top-level integration tests.
- `fixtures/`, `test-data/` — copybooks, binary records, golden outputs.
- `fuzz/` — cargo-fuzz targets (separate workspace, own `Cargo.toml`).
- `examples/` — usage examples; `examples/kafka_pipeline` and
  `examples/kafka_streaming` are excluded from the workspace.
- `docs/` — documentation (Diataxis-organized; index at `docs/README.md`,
  entry point `docs/START_HERE.md`).
- `scripts/` — CI, bench, API-baseline, and release scripts; CI gates live in
  `scripts/ci/`.
- `schemas/` — JSON schemas (copybook schema, record format, perf receipt).
- `deploy/`, `grafana/` — Kubernetes staging manifests, Prometheus config,
  Grafana dashboards/alerts for the CLI's metrics surface.
- `Dockerfile` — reproducible benchmark container (not a production image).

## Architecture and ownership

The principal flow is `copybook-core` (parse and schema) -> `copybook-codec`
(binary/JSON transformation) -> `copybook-cli` (orchestration). Put domain
logic in libraries rather than the CLI. Prefer an internal module before adding
a workspace crate; a public crate creates a support obligation.

Key crates and roles:

- `copybook` — canonical facade crate that re-exports the granular crates as
  modules (`copybook::core`, `copybook::codec`, ...). `copybook-rs` is a
  redirect/search alias for the same API.
- `copybook-core` — copybook parsing: lexer, parser, AST, schema, layout
  resolution, projection, dialect, feature flags, support matrix.
- `copybook-codec` — record encode/decode: charset conversion, numeric
  (COMP-3, zoned, overpunch), edited PIC, ODO/REDEFINES handling, round-trip
  and determinism helpers.
- `copybook-cli` — clap-based CLI; subcommands in `src/commands/` include
  `parse`, `inspect`, `decode`, `encode`, `verify`, `verify-report`,
  `determinism`, `support`, `audit`. Exit codes map to error families
  (2=CBKD, 3=CBKE, 4=CBKF, 5=CBKI).
- Record format primitives: `copybook-fixed`, `copybook-rdw`,
  `copybook-record-io`. Numeric/text primitives: `copybook-zoned-format`,
  `copybook-overpunch`, `copybook-overflow`, `copybook-safe-*`.
- `copybook-error`, `copybook-error-reporter` — typed error taxonomy and
  structured reporting.
- `copybook-charset`, `copybook-codepage` — EBCDIC/ASCII conversion and
  codepage policy (CP037/CP273/CP500/CP1047/CP1140).
- `copybook-dialect`, `copybook-options`, `copybook-determinism` — dialect
  lever, shared codec options, stable hash/diff primitives.
- `copybook-corruption*` — corruption detection and RDW predicates.
- `copybook-governance*` + `copybook-contracts`, `copybook-support-matrix` —
  feature-flag governance microcrate stack.
- `copybook-arrow` — experimental Apache Arrow/Parquet adapter.

Before editing, inspect the branch, status, relevant canonical documents,
nearby tests, and any existing aligned PR. Keep one review-forward concern per
PR. Update the source-truth artifact when behavior, public API, policy, support,
or release claims change. Do not tag, publish, deploy, force-push, or mutate
`origin/main` directly without explicit authorization.

## Authority and source truth

Use current repository evidence in this order:

1. Canonical behavior documents, code, tests, policy, and generated receipts.
2. This file and the closest scoped `AGENTS.md` for workflow and ownership.
3. Nested `claude.md` files as topology maps only.
4. Chat history and prior agent notes.

Do not copy volatile facts such as crate, test, dependency, or error-code counts
into guidance. Verify them from the workspace when they matter.

Canonical references include:

| Concern | Source |
| --- | --- |
| Documentation index | `docs/README.md` |
| Feature support | `docs/reference/COBOL_SUPPORT_MATRIX.md` |
| Error contracts | `docs/reference/ERROR_CODES.md` |
| CLI behavior | `docs/CLI_REFERENCE.md` |
| Library API | `docs/reference/LIBRARY_API.md` |
| JSONL schema | `docs/jsonl-schema.md` |
| Performance policy and receipts | `docs/PERFORMANCE_GOVERNANCE.md`, `scripts/bench/perf.json` |
| Project status | `docs/ROADMAP.md`, `docs/REPORT.md` |
| Dialect and edited PIC contracts | `docs/internal/features/d0_dialect_lever_contract.md`, `docs/internal/features/e3_edited_pic_encode_contract.md` |
| ODO and RENAMES behavior | `docs/design/NESTED_ODO_BEHAVIOR.md`, `docs/design/RENAMES_NESTED_GROUPS.md` |
| Stability and support | `docs/STABILITY_GUARANTEES.md`, `docs/SUPPORT_POLICY.md` |
| Release procedure | `docs/RELEASE_RUNBOOK.md` |

## Build and test commands

The task runner is [`just`](https://github.com/casey/just); see the root
`justfile` for the full recipe list. Dev bootstrap: `just setup` (installs
cargo-nextest, cargo-deny, etc.).

```text
just build                 # cargo build --workspace
just build-release         # release build
just test                  # cargo nextest run (excludes bdd + bench crates)
just test-crate <crate>    # nextest for one crate
just bdd-smoke             # gated Cucumber smoke suite
just lint                  # clippy, pedantic; tests get panic-family allows
just fmt / just fmt-check  # rustfmt
just deny                  # cargo-deny (licenses, advisories, bans)
just docs                  # cargo doc --workspace --no-deps
just check-msrv            # cargo +1.92.0 check --workspace
just bench-json            # perf receipts via scripts/bench.sh (SLO suite)
just coverage              # cargo llvm-cov -> lcov.info
```

Without `just`, the equivalents are plain cargo: `cargo build --workspace`,
`cargo test --workspace`, `cargo clippy --workspace -- -D warnings -W
clippy::pedantic`, `cargo fmt --all -- --check`.

## Validation

Run the narrowest proof first, then the repository gate appropriate to risk:

```text
just test-crate <crate>
just bdd-smoke
just fmt-check
just lint
just deny
just check-msrv
just ci-quick
just pr
```

`just pr` (also exposed as `just ci`) is the canonical local PR-parity gate:
it runs `scripts/ci/quick.sh` (fmt -> clippy -> build -> nextest -> doctests)
then `scripts/ci/security.sh` (cargo-deny + conditional audit). Use `just deny`
for dependency or lockfile changes, `just check-msrv` for compatibility work,
and `just bdd-smoke` when governance or BDD behavior moves. On Windows,
workspace-wide rustfmt can hit OS error 206; a scoped
`cargo fmt -p <crate> --check` is useful local evidence, but report the gap and
use Linux CI for full-workspace proof.

Report checks as pass, fail, or not run. A skipped policy lane is not a pass,
and a green CI run proves only the checks that ran against that commit.

CI (`.github/workflows/`) has many lanes beyond the PR gate: quick CI,
coverage, proptest, ripr test-oracle reports (advisory), fuzzing,
perf gates/benchmarks with receipts, determinism smoke, docs-truth, SBOM,
security scans, API-freeze checks, and publish dry-runs. Mutation testing
(`mutants.toml`) is local-only via `just mutants`. Scheduled lanes can be
sampled locally via `just scheduled`.

## Code style and data rules

- The workspace uses Rust 2024 with MSRV 1.92. Preserve MSRV unless the change
  explicitly updates the compatibility contract.
- Keep shipped code safe and fallible: no new `unsafe` (workspace lints forbid
  `unsafe_code`), no panic-family calls (`unwrap`/`expect`/`panic!`/
  `unreachable!`/`todo!`/`unimplemented!`) or unchecked indexing in shipped
  code without a documented, governed exception. Clippy warns on these; the
  lint gate allows them only in tests.
- Clippy pedantic compliance is enforced in CI; match idiomatic patterns
  already in the codebase (`div_ceil`, `is_empty`, range `contains`,
  `try_from()` conversions, `Display` for user-facing types).
- Preserve typed errors, causal context, and the stable error-code taxonomy
  (families include CBKP* parse, CBKS* schema, CBKD* data, CBKE* encode,
  CBKR* record format; see `docs/reference/ERROR_CODES.md`).
- Treat copybooks, binary records, JSON, paths, and process input as hostile.
- Keep encode/decode output deterministic. Add focused success and failure
  coverage beside the changed seam.
- Use existing scratch-buffer and streaming patterns in hot paths; benchmark
  performance claims against the canonical receipt
  (`scripts/bench/perf.json`) rather than intuition. Bench builds pin
  `RUSTFLAGS="-C target-cpu=x86-64-v3"` to match CI (native-compiled artifacts
  can SIGILL across CPU generations).
- Source files carry an SPDX license header (`AGPL-3.0-or-later`); keep it in
  new files.

### Raw capture contract

- `RawMode::Record` emits the record payload as canonical `raw_b64`; legacy
  `__raw_b64` is also emitted/accepted for compatibility.
- `RawMode::RecordRDW` captures the RDW header plus record payload.
- `RawMode::Field` emits only `<FIELD_NAME>_raw_b64` values and no whole-record
  raw payload.
- `RawMode::Off` emits no raw payload.

The library API, CLI reference, and JSONL schema listed above are authoritative.

## Testing strategy

- **Unit tests** live beside the code in each crate.
- **Integration tests** in each crate's `tests/` directory and the top-level
  `tests/` workspace crates: `copybook-bdd` (Cucumber/Gherkin features under
  `tests/bdd/features/`), `copybook-e2e` (CLI end-to-end, exit codes, error
  code coverage), `copybook-proptest` (property tests; extended runs via
  `PROPTEST_CASES`/`PROPTEST_SEED`).
- **Golden fixtures**: structural validation with SHA-256 verification under
  `fixtures/` (ODO, Level-88, REDEFINES); update them for end-to-end behavior
  changes.
- **Benchmarks**: criterion via `copybook-bench`; perf receipts must compare
  against `scripts/bench/perf.json` per `docs/PERFORMANCE_GOVERNANCE.md`.
- Name tests by feature: `cobol_*`, `enterprise_*`, `parsing_*`, `encoding_*`.
- Generate crypto-shaped test inputs (PEM, DER, private keys, certificates) at
  runtime with the workspace `uselesskey` dev dependency instead of committing
  secret-like blobs under `fixtures/` or `tests/fixtures/`; prefer
  deterministic seeds (e.g. derived from `module_path!()`) so CI output stays
  reproducible.
- Longer-horizon quality lanes: mutation testing (`just mutants`), fuzzing
  (`fuzz/` workspace), soak and leak-detection workflows — all run in CI on
  schedule.

## Security considerations

- `unsafe_code` is forbidden workspace-wide; panic-family lints keep shipped
  code fallible.
- `cargo deny check` (config `deny.toml`) gates licenses, advisories, bans, and
  sources; run `just deny` on any dependency or lockfile change.
- Weekly security-scan and SBOM workflows run in CI; `SECURITY.md` documents
  the reporting process. Contributors sign a CLA (`CLA.md`).
- All external input (copybooks, binary records, JSONL, file paths, CLI/env
  input) is untrusted: validate, bound, and surface typed errors instead of
  panicking.
- Never commit secrets or secret-shaped fixtures (see Testing strategy for the
  `uselesskey` rule). The CLI's raw-capture modes embed record data as base64
  in JSONL output — treat outputs as sensitive when inputs are.

## Deployment and release

- Releases follow `docs/RELEASE_RUNBOOK.md`; versioning and support windows are
  governed by `docs/STABILITY_GUARANTEES.md`, `docs/API_FREEZE.md`, and
  `docs/SUPPORT_POLICY.md`. The API baseline is managed via
  `scripts/api-baseline.sh` (`just api-check`, `just api-baseline`,
  `just api-info`, `just api-freeze-status`).
- Crates are published to crates.io with exact pinned inter-crate versions;
  `publish.yml` / `publish-dry-run.yml` workflows handle publication. Do not
  tag or publish without explicit authorization.
- The root `Dockerfile` builds a reproducible benchmark container that emits
  perf receipts; it is not a production service image. `deploy/` and
  `grafana/` contain staging Kubernetes manifests and observability config for
  CLI metrics.

## Review, merge, and cleanup

Use Conventional Commit titles (`<type>(<crate>): <description>`; types `feat`,
`fix`, `docs`, `test`, `refactor`, `perf`, `chore`, `ci`) and the repository PR
template. Reuse and improve aligned inactive PRs when practical. Before
committing, inspect status, the full diff, and the staged diff; stage only the
selected lane. Merge only when the diff is reviewed, required/current checks
are green, and actionable feedback is resolved. After merge, sync the target
branch and remove only the branch, worktree, and temporary artifacts created by
the lane. Capture genuine remaining work as a focused issue instead of widening
the merged concern.
