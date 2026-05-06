# Effortless Metrics Clippy Policy

copybook-rs follows the shared Effortless Metrics Rust lint posture: one
workspace-level baseline, no test carveouts, and every temporary exception
represented as policy data instead of hidden configuration drift.

## Goals

- Keep production and test code panic-free by denying `unwrap`, `expect`,
  `panic!`, `todo!`, `unimplemented!`, `unreachable!`, unchecked indexing, and
  string slicing at the workspace boundary.
- Prevent silent failure by denying ignored futures, ignored must-use values,
  ignored locks, ignored `Result::ok`, ignored `map_err`, and result-state
  assertions that do not observe the error payload.
- Make suppression explicit: do not use broad `#[allow]` attributes. Use a
  narrow `#[expect(..., reason = "...")]` only when a reviewed local exception
  is better than weakening policy for the whole workspace.
- Track future Clippy flips for Rust 1.94 and 1.95 before the MSRV bump so the
  upgrade is a planned ratchet instead of surprise churn.

## Source of truth

The root `Cargo.toml` contains the active compiler and Clippy lint block. The
machine-readable ledger lives in `policy/clippy-lints.toml` and must match the
active block exactly for current lints. Planned lints remain in the ledger with
`status = "planned"` and `activate_when_msrv` until the workspace MSRV reaches
that release.

Temporary exceptions belong in `policy/clippy-debt.toml`. Debt entries must have
all of these fields:

- `lint`
- `path`
- `owner`
- `reason`
- `expires`

Expired debt fails the policy gate.

## No test carveouts

`clippy.toml` must not contain any of Clippy's test carveout settings, including:

- `allow-unwrap-in-tests`
- `allow-expect-in-tests`
- `allow-panic-in-tests`
- `allow-indexing-slicing-in-tests`
- `allow-dbg-in-tests`

Tests should return `Result` and use explicit assertion helpers instead of
panic-driven setup.

```rust
#[test]
fn parses_fixture() -> Result<(), Box<dyn std::error::Error>> {
    let fixture = std::fs::read_to_string("tests/fixtures/input.cpy")?;
    let parsed = parse_copybook(&fixture)?;

    assert_eq!(parsed.records().len(), 3, "fixture should expose three records");
    Ok(())
}
```

## Suppression style

Use `#[expect]` instead of `#[allow]` so suppressions are self-expiring when the
lint no longer triggers. Every expectation must include a reason.

```rust
#[expect(
    clippy::indexing_slicing,
    reason = "Generated parser table access is guarded by table-shape tests."
)]
fn generated_table_lookup(table: &[u8], index: usize) -> u8 {
    table[index]
}
```

If the exception is expected to live beyond the local change, add it to
`policy/clippy-debt.toml` with an owner and expiry.

## Parser/protocol overlay

copybook-rs is a parser/protocol workspace. The shared baseline is intentionally
strict around UTF-8, string boundaries, indexing, unreachable states, and numeric
casts. Those lints catch the local bad shapes most likely to become parsing bugs,
fixture-only assumptions, or mainframe encoding edge cases.

## Policy gate

Run the policy gate with:

```sh
cargo xtask check-lint-policy
```

The gate verifies the MSRV ledger, workspace lint inheritance, active/planned
lint consistency, absence of test carveouts, and debt metadata.
