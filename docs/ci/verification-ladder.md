<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Verification Ladder

This document describes the tiers of verification available in `copybook-rs`,
from fastest/cheapest to deepest/most expensive.

## Tier 0 — Local (Zero CI Cost)

These checks run locally before a PR is opened. They are not CI lanes.

| Check | Command | Cost |
|-------|---------|------|
| Format | `cargo fmt --all -- --check` | Instant |
| Check | `cargo check --workspace` | Seconds |
| Clippy strict | `cargo clippy --workspace -- -D warnings -W clippy::pedantic` | Seconds |
| Unit tests | `cargo test --workspace` | < 2 min on developer machine |
| xtask policy | `cargo run -p xtask -- policy-report` | Seconds |

## Tier 1 — Rust Fast Gate (~8–15 LEM)

The default ordinary PR gate on Ubuntu/stable/default features.

| Lane | Proof Obligation |
|------|-----------------|
| fmt | Formatting is canonical |
| check | Workspace compiles with stable |
| clippy strict | No clippy::pedantic findings; no panic-family in production |
| no-panic policy | No unaccounted panic-family calls |
| file policy | No unaccounted non-Rust files |
| selected crate tests | Tests for crates touched by the PR pass on Linux/stable |

## Tier 2 — Risk-Pack Tests (~5–20 LEM each)

Run automatically when the PR touches crates in the relevant risk pack.
See `policy/ci-risk-packs.toml` for path-to-lane mappings.

| Risk Pack | Lanes |
|-----------|-------|
| lexer/parser | Unit tests + ripr advisory |
| record/codec | Unit tests + RDW iterator tests + ripr advisory |
| numeric encoding | Unit tests + ripr advisory |
| CLI/determinism | Unit tests + determinism smoke + exit code matrix + ripr advisory |
| governance/BDD | Unit tests + BDD tests |
| Arrow/WASM | Unit tests (deep via label) |
| release/security | Unit tests + security check |
| docs only | Docs gate only; no Rust compile |

## Tier 3 — Ordinary PR Gate Total

Tiers 1 + 2 combined. Target: **below 35 LEM**.

After PR 09, `PR Gate Success` summarizes tier 1 and tier 2 as a single
required check.

## Tier 4 — Elevated PR (Label-Triggered, ~36–75 LEM)

Activated by labels like `property-tests`, `coverage`, `security-audit`.

| Lane | Trigger |
|------|---------|
| Heavy proptest | `property-tests` or parser/codec risk on main |
| Coverage | `coverage` label or main |
| Full security audit | `security-audit` label or manifest risk on main |
| Docs/rustdoc | `release-check` or docs/API risk on main |
| Examples | `release-check` or main |

## Tier 5 — High-Cost PR (Label-Triggered, ~76–125 LEM)

Activated by `platform-matrix`, `full-ci`, `mutation`, `fuzz`, `perf`.

| Lane | Trigger |
|------|---------|
| macOS matrix | `platform-matrix` or `full-ci` |
| Windows matrix | `platform-matrix` or `full-ci` |
| beta toolchain | `platform-matrix` or `full-ci` or main |
| Full feature matrix | `full-ci` or main |
| Mutation testing | `mutation` or nightly |
| Fuzz integration | `fuzz` or nightly |
| Benchmark/enterprise perf | `perf` or main or manual |
| WASM build | `wasm` or `full-ci` |
| Arrow integration | `arrow` or `full-ci` |
| Leak detection | main or `full-ci` |

## Tier 6 — Deep / Main / Nightly (~75–200+ LEM)

Always runs on `main` pushes, nightly schedule, or release triggers.
Never required for ordinary PR merge.

| Lane | Schedule |
|------|----------|
| Nightly soak (full toolchain matrix) | Nightly |
| Publish dry-run | Release trigger, manual |
| SBOM generation | Release trigger, main |
| Changelog validation | Release trigger |
| API freeze check | Release trigger |
| Long-duration fuzz | Weekly schedule |
| Enterprise perf regression | main push, `perf` label |
| Full BDD coverage | main push |

## Verification-to-Risk Mapping

| Changed Surface | Minimum Tier | Recommended Tier |
|----------------|--------------|-----------------|
| `docs/**` only | Tier 0 + docs gate | Tier 1 skip Rust compile |
| Config/TOML/policy | Tier 1 | Tier 1 + policy check |
| `crates/copybook-lexer` | Tier 1 + 2 (parser pack) | Tier 2 + proptest label |
| `crates/copybook-core` | Tier 1 + 2 (parser pack) | Tier 2 + proptest label |
| `crates/copybook-codec*` | Tier 1 + 2 (codec pack) | Tier 2 + proptest label |
| `crates/copybook-rdw*` | Tier 1 + 2 (codec pack) | Tier 2 + RDW tests |
| `crates/copybook-codepage*` | Tier 1 + 2 (codec pack) | Tier 2 |
| `crates/copybook-overpunch` | Tier 1 + 2 (numeric pack) | Tier 2 |
| `crates/copybook-cli*` | Tier 1 + 2 (CLI pack) | Tier 2 + determinism |
| `crates/copybook-governance*` | Tier 1 + 2 (governance pack) | Tier 2 + BDD |
| `crates/copybook-arrow` | Tier 1 | Tier 4 + `arrow` label |
| `Cargo.toml` / `Cargo.lock` | Tier 1 + security | Tier 3 + `security-audit` |
| `.github/workflows/**` | Tier 1 + CI policy | Tier 3 + `full-ci` on release |
