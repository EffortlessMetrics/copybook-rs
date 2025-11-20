# PR Validation Sections

Copy-paste these into the respective PRs.

---

## PR #151 – thiserror / error-core

```markdown
## Semantic Validation

Local validation:

- ✅ `./scripts/ci/offline-semantic.sh`
- ✅ `cargo test -p copybook-core --lib` (error module + surrounding tests)
- ✅ `cargo test -p copybook-core --doc` (doctests for `Error` usage)
- ✅ No new clippy warnings in `copybook-core/src/error.rs` (with `-D warnings`)

Behavioral checks:

- ✅ Display format preserved: `"CODE: message (context)"` for errors with context.
- ✅ `ErrorCode` serde round-trip: `Serialize` → `Deserialize` returns original enum value.
- ✅ Existing `error!` macro sites compile and behave unchanged.

Notes:

- CI is currently unstable; local gates above are the source of truth.
```

---

## PR #152 – perf receipts / `xtask perf` / CI wiring

```markdown
## Semantic Validation

Local validation:

- ✅ `./scripts/ci/offline-semantic.sh`
- ✅ `cargo test -p xtask` (perf unit + integration tests)
- ✅ `cargo run -p xtask -- perf --summarize-last` using `scripts/bench/perf.json`
- ✅ `cargo fmt --all --check`
- ✅ `cargo clippy -p xtask -- -D warnings`

Perf receipts:

- ✅ Manual math check:
  - DISPLAY/COMP-3 MiB/s recomputed from `bytes` + `ns` in Python matches tool output (< 0.1% diff).
- ✅ SLO boundaries:
  - `display_mibps = 80.0` → "✓ All SLOs met"
  - `display_mibps = 79.9` → "⚠ SLOs not met"
- ✅ Malformed JSON:
  - Deliberately invalid `scripts/bench/perf.json` → `serde_json` parse error with clear message (no silent fallback).

Contract:

- ✅ Flat and nested receipts (`display_mibps` / `comp3_mibps` at top level or under `summary`) both supported.

**CI note:** GitHub Actions is currently unstable in this environment. Until it recovers, the commands above are the canonical validation for this PR.
```

---

## PR #154 – `copybook support` CLI

```markdown
## Semantic Validation

Local validation:

- ✅ `./scripts/ci/offline-semantic.sh`
- ✅ `cargo test -p copybook-core support_matrix`
- ✅ `cargo test -p copybook-cli --test support_cli`
- ✅ `cargo run --bin copybook -- support`
- ✅ `cargo run --bin copybook -- support --json`
- ✅ `cargo fmt --all --check`
- ✅ `cargo clippy -p copybook-core -p copybook-cli -- -D warnings`

Behavioral checks:

- ✅ JSON round-trip: feature IDs from `support_matrix::all_features()` match `copybook support --json` payload 1:1 (HashSet equality).
- ✅ Exit codes:
  - `copybook support --check level-88` → exit 0 (supported)
  - `copybook support --check level-66-renames` → non-zero (partial/planned)
  - `copybook support --check no-such-feature` → non-zero, clear error on stderr.
- ✅ Table output includes:
  - Header row (`FEATURE`, `STATUS`, `DESCRIPTION`)
  - At least one `supported` feature and at least one non-supported feature.

Docs:

- ✅ README updated with `copybook support` / `--json` / `--check` usage.
- ✅ `COBOL_SUPPORT_MATRIX.md` linked as canonical source.

**CI note:** GitHub Actions is currently unstable in this environment. Until it recovers, the commands above are the canonical validation for this PR.
```

---

## PR #155 – docs-truth, validation protocol, infra scripts

```markdown
## Semantic Validation

Local validation:

- ✅ `./scripts/ci/offline-semantic.sh`
- ✅ `cargo run -p xtask -- docs verify-support-matrix`
- ✅ `cargo test -p xtask` (perf + docs verification modules)
- ✅ `cargo test -p copybook-cli --bin copybook support::tests`
- ✅ `cargo fmt --all --check`
- ✅ `cargo clippy -p xtask -p copybook-core -p copybook-cli -- -D warnings`

Adversarial tests:

- ✅ Support matrix drift:
  - Commented out the `level-66-renames` row in `COBOL_SUPPORT_MATRIX.md` → `verify-support-matrix` failed with "Support matrix drift detected" and listed feature ID.
  - Restored docs → `verify-support-matrix` passed.
- ✅ Perf receipts:
  - Malformed `scripts/bench/perf.json` (`{"bad": "json",}`) → `perf --summarize-last` failed with a JSON parse error.
  - Boundary SLO tests:
    - `display_mibps = 80.0, comp3_mibps = 40.0` → SLO pass (`✓ All SLOs met`).
    - `display_mibps = 79.9, comp3_mibps = 40.0` → SLO fail (`⚠ SLOs not met`).
- ✅ Manual math:
  - Throughput recomputed by hand / in Python from `bytes` and `ns` matches reported MiB/s within <0.1% tolerance.

Docs:

- ✅ `CONTRIBUTING.md` updated with semantic validation contract and adversarial checklist.
- ✅ `.github/PULL_REQUEST_TEMPLATE.md` updated with infra-specific validation section.
- ✅ `docs/VALIDATION_PROTOCOL.md` examples verified end-to-end (copy-pasteable as written).

**CI note:** GitHub Actions is currently unstable in this environment. Until it recovers, the commands above are the canonical validation for this PR.
```

---

## Merge Strategy (when CI stabilizes)

Merge order (lightest coupling → most coupled):

1. **#154 – Support CLI**: pure CLI + core registry; low blast radius.
2. **#155 – docs-truth + validation protocol**: makes CI + contributor UX better, doesn't touch runtime.
3. **#152 – perf receipts / xtask perf**: touches perf infra; already well-guarded by tests and manual math.
4. **#151 – error handling**: core semantics; merge last so any latent issues are easy to attribute.

Between each merge:

```bash
git checkout main
git pull origin main
./scripts/ci/offline-semantic.sh
```

If that passes, you know the integration is still sound even if Actions is slow to confirm.
## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](LICENSE).
