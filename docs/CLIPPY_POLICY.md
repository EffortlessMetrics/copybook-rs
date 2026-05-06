# Clippy policy

copybook-rs uses the Effortless Metrics Rust lint policy as a governed
engineering surface. The workspace baseline is intentionally strict: production
code and tests are panic-free, parser boundaries avoid unchecked string/slice
indexing, silent failures are rejected, and suppression requires a narrow
`#[expect(..., reason = "...")]` receipt.

## Active baseline

The root `Cargo.toml` owns the active `[workspace.lints.rust]` and
`[workspace.lints.clippy]` block. Workspace crates inherit that block with:

```toml
[lints]
workspace = true
```

The machine-readable source of truth is `policy/clippy-lints.toml`. It records:

- the workspace MSRV (`1.93`),
- the active Rust and Clippy lint levels,
- the no-test-carveout policy,
- the suppression style, and
- planned Rust 1.94/1.95 flips.

## Suppression style

Prefer fixing the lint. If a suppression is necessary, use a narrow `expect`
with a reason:

```rust
#[expect(clippy::indexing_slicing, reason = "Bounds checked by parser table generator.")]
fn generated_table_lookup(...) { ... }
```

Do not add broad test carveouts in `clippy.toml`, and do not use silent
`#[allow(...)]` suppressions for policy lints. Temporary repo-local exceptions
belong in `policy/clippy-debt.toml` with an owner, reason, path, lint, and
expiry date.

## Parser/protocol overlay

copybook-rs is a parser/protocol workspace. The standard baseline therefore
keeps UTF-8, string slicing, indexing, numeric conversion, and unreachable-code
lints active rather than treating them as local taste. This protects copybook
parse boundaries and fixed-record offsets from unchecked byte/character drift.

## Planned Rust upgrades

`policy/clippy-lints.toml` tracks lints planned for Rust 1.94 and 1.95 before
those lints are activated. `cargo xtask check-lint-policy` fails if a planned
future lint is accidentally activated before the MSRV bump.

## Checks

Run:

```console
cargo xtask check-lint-policy
```

The gate verifies the MSRV ledger, workspace lint inheritance, active lint
consistency, absence of Clippy test carveouts, planned upgrade flips, and
required fields for debt and allowlist policy data.
