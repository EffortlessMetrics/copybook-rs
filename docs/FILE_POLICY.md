<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# File Policy

## Purpose

`copybook-rs` is a Rust workspace. All files in the repository should be
either Rust source or explicitly accounted for in the non-Rust allowlist.

The non-Rust allowlist (`policy/non-rust-allowlist.toml`) records every
non-Rust file category, its owner, its classification, its purpose, and
what CI lane verifies it. Files not matching any allowlist glob fail the
`xtask check-file-policy` check.

## Why This Matters

Unaccounted non-Rust files create maintenance debt:
- YAML with no owner drifts without review.
- Scripts accumulate without xtask migration.
- Binary fixtures grow without expiry or coverage linkage.
- Generated artifacts are committed without regeneration tracking.

The allowlist makes every non-Rust surface visible, owned, and covered.

## Allowlist Schema

Entries in `policy/non-rust-allowlist.toml`:

```toml
schema_version = "1.0"

[[allow]]
glob = ".github/workflows/*.yml"
kind = "ci_declarative"
owner = "release/ci"
surface = "ci"
classification = "config"
reason = "GitHub Actions workflow definitions are platform-required YAML."
covered_by = ["cargo run -p xtask -- check-ci-lane-whitelist"]
```

### Required Fields

| Field | Type | Description |
|-------|------|-------------|
| `glob` | string | Glob pattern relative to repo root |
| `kind` | string | File category (see below) |
| `owner` | string | Team or individual responsible |
| `surface` | string | High-level surface area |
| `classification` | string | One of the classification values below |
| `reason` | string | Why this non-Rust surface exists |
| `covered_by` | string[] | CI lanes or xtask commands that verify this surface |

### Optional Fields

| Field | Type | Description |
|-------|------|-------------|
| `expires` | date | For temporary entries; fails check after this date |
| `review_after` | date | Advisory review date |
| `issue` | string | Issue or PR tracking eventual removal/migration |

### Kind Values

| Kind | Examples |
|------|---------|
| `ci_declarative` | `.github/workflows/*.yml` |
| `documentation` | `docs/**`, `README.md`, `CHANGELOG.md` |
| `copybook_fixture` | `fixtures/copybooks/**/*.cpy`, `*.copybook` |
| `record_fixture` | Binary EBCDIC fixture data, RDW test records |
| `codepage_artifact` | Codepage tables, charset mapping files |
| `schema_definition` | JSON Schema, TOML schema files |
| `compatibility_script` | Shell scripts pending xtask migration |
| `nix_config` | `flake.nix`, `shell.nix`, `default.nix` |
| `perf_receipt` | `scripts/bench/perf.json`, perf snapshots |
| `cargo_config` | `.cargo/config.toml`, `rust-toolchain.toml` |
| `generated_data` | Code-generated artifacts tracked in VCS |
| `license` | `LICENSE`, `NOTICE`, `LICENSES/**` |
| `github_config` | `CODEOWNERS`, `dependabot.yml`, `settings.yml` |

### Classification Values

| Classification | Meaning |
|----------------|---------|
| `config` | Platform or tool configuration required by the runtime |
| `documentation` | Human-readable docs; not executed by CI |
| `test` | Test fixtures and inputs; verified by test lanes |
| `tooling` | Developer or CI tooling; not shipped |
| `data` | Binary or structured data used by tests or benchmarks |
| `generated` | Generated artifact; the generator must also be covered |
| `legal` | License and attribution files |

## Covered-By Convention

The `covered_by` array should list either:

1. A `cargo run -p xtask -- <command>` invocation that validates the file, or
2. A `cargo test --workspace <test_name_prefix>` invocation that exercises it.

At least one entry is required. Empty `covered_by` arrays fail
`xtask check-file-policy`.

## Commands

After PR 02 and PR 05:

```bash
# Check all non-Rust files against the allowlist
cargo run -p xtask -- check-file-policy

# Full policy report including file policy status
cargo run -p xtask -- policy-report
```

## Enforcement Phasing

| Phase | State | Condition |
|-------|-------|-----------|
| 1 — Allowlist creation | No CI gate | PR 05 merged |
| 2 — Advisory check | `check-file-policy` in CI; non-blocking | PR 02 + 05 merged |
| 3 — Hard gate | Unallowlisted non-Rust files fail CI | After allowlist covers all files |

The allowlist must cover 100% of current non-Rust files before Phase 3.

## Temporary Entries

Compatibility scripts and generated artifacts that should eventually be
removed or migrated carry an `expires` date. Example:

```toml
[[allow]]
glob = "scripts/**"
kind = "compatibility_script"
owner = "release/ci"
surface = "tooling"
classification = "tooling"
reason = "Legacy CI scripts retained until xtask migration completes."
covered_by = ["cargo run -p xtask -- check-file-policy"]
expires = "2026-08-08"
issue = "TODO"
```

Expired entries fail `xtask check-file-policy`. The owner must either
migrate the files or extend the expiry with justification.
