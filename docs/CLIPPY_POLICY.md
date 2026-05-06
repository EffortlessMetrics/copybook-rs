# Strict Clippy Policy

copybook-rs treats Clippy as a governed engineering surface, not as an ad hoc
set of local preferences. The workspace policy is intentionally uniform with the
Effortless Metrics Rust platform baseline: MSRV 1.93, panic-free production and
test code, parser-safe indexing and UTF-8 defaults, explicit suppression
receipts, and planned lint flips tracked before Rust upgrades land.

## Workspace baseline

The root `Cargo.toml` owns the active lint surface in `[workspace.lints.rust]`
and `[workspace.lints.clippy]`. Workspace members inherit that surface with:

```toml
[lints]
workspace = true
```

The baseline is organized around these policy classes:

- **Panic-free production and tests**: rejects `unwrap`, `expect`, `panic!`,
  `todo!`, `unimplemented!`, `unreachable!`, and `dbg!` across all targets.
- **Parser / AST / UTF-8 / slice safety**: rejects string slicing, unchecked
  indexing, byte/character index confusion, and out-of-bounds indexing shapes.
- **Silent-failure prevention**: rejects ignored futures, locks, `Result` work,
  swallowed `map_err`, and `lines().filter_map(Result::ok)` hazards.
- **Async and concurrency hygiene**: rejects lock/refcell hazards across await
  points and non-send shared-state footguns.
- **Unsafe, memory, numeric, file/process/path, and API correctness**: makes
  record-boundary corruption hazards visible during review.
- **Good-taste reviewability**: uses warn/deny levels for formatting, control
  flow, allocation, and public-contract documentation lints that make reviews
  less noisy.
- **Suppression governance**: rejects silent `#[allow]`-style suppressions and
  requires narrow, justified receipts.

## Machine-readable ledger

`policy/clippy-lints.toml` is the policy ledger. It mirrors active lints from
`Cargo.toml` and tracks planned Rust 1.94/1.95 flips before the MSRV bump. Each
entry records the lint name, level, status, class, and reason.

`cargo run -p xtask -- check-lint-policy` verifies that:

1. `workspace.package.rust-version` matches the policy ledger MSRV.
2. Every workspace member inherits workspace lints.
3. The active lint ledger matches the root `Cargo.toml` lint block.
4. Planned Rust 1.94/1.95 flips are not accidentally activated early.
5. `clippy.toml` does not enable test carveouts such as
   `allow-unwrap-in-tests = true`.
6. `policy/clippy-debt.toml` entries, when present, have lint, path, owner,
   reason, and expiry fields and are not expired.

## Suppression style

The default is global deny. Local exceptions must be structured receipts:

```rust
#[expect(
    clippy::indexing_slicing,
    reason = "generated COBOL table access is bounded by copybook-safe-index"
)]
fn generated_table_lookup() {
    // ...
}
```

Do not add broad `#[allow(...)]` blocks, crate-wide panic/test carveouts, or
`clippy.toml` test exemptions. Temporary migration debt belongs in
`policy/clippy-debt.toml`, with an owner, reason, path, lint, and expiry date.

## Allowlist policy model

Panic-family and non-Rust file exceptions are structured TOML receipts:

- `policy/no-panic-allowlist.toml` uses semantic identity:
  `path + family + selector`, with `last_seen` only as an advisory locator.
- `policy/non-rust-allowlist.toml` records every non-Rust surface with a path or
  glob, kind, owner, reason, surface, classification, and coverage command.

The intended operating model is:

```text
cargo fmt
cargo clippy --workspace --all-targets --all-features -- -D warnings
cargo test --workspace
cargo run -p xtask -- check-lint-policy
cargo run -p xtask -- policy-report
```

Clippy governs code shape. The policy files govern exceptions. ripr-style
repo-evidence tooling should then show whether the behavior seams are activated,
propagated, and observed by tests.
