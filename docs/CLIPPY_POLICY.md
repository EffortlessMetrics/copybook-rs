# Clippy Policy

`copybook-rs` treats Clippy as a governed engineering surface. The workspace lint block, policy ledger, and `xtask` checks are intended to make parser and codec safety reviewable instead of relying on local convention.

## Policy goals

The active policy has four non-negotiable goals:

1. **Panic-free production and tests**: `unwrap`, `expect`, `panic!`, `todo!`, `unimplemented!`, `unreachable!`, unchecked string slicing, and unchecked indexing are denied at the workspace lint layer.
2. **Silent-failure prevention**: ignored futures, ignored must-use values, ignored lock guards, `Result::ok()` swallowing, ignored `map_err`, and lossy line iteration patterns are denied.
3. **Suppression governance**: broad `#[allow]` attributes are not the desired end state. New suppressions should use `#[expect(..., reason = "...")]` so each exception is narrow and carries a reviewable receipt.
4. **Parser/codec safety**: copybook parsing is sensitive to UTF-8 boundaries, record offsets, numeric conversion, and file/process edges, so the baseline includes AST/string/indexing, numeric, memory, and filesystem/process lint coverage.

## Files

The policy is split across these files:

| File | Purpose |
| --- | --- |
| `Cargo.toml` | Active workspace lint block consumed by Cargo/Clippy. |
| `clippy.toml` | Repo-specific Clippy configuration. It must not contain test carveouts such as `allow-unwrap-in-tests = true`. |
| `policy/clippy-lints.toml` | Machine-readable source of truth for active lints and planned Rust 1.94/1.95 flips. |
| `policy/clippy-debt.toml` | Temporary, expiring debt receipts for current exceptions while follow-up cleanup PRs ratchet the repo. |
| `policy/no-panic-allowlist.toml` | Semantic panic-family allowlist schema. The target state is empty; any entry needs owner, explanation, selector identity, and optional expiry. |
| `policy/non-rust-allowlist.toml` | Structured allowlist for non-Rust programming files with owner, reason, surface, classification, and CI coverage. |

## Suppression style

Preferred new suppression:

```rust
#[expect(
    clippy::cast_possible_truncation,
    reason = "validated by CopybookField::byte_len upper bound before narrowing"
)]
let byte_len = len as u16;
```

Avoid new broad suppressions:

```rust
#[allow(clippy::unwrap_used)]
```

Historical `#[allow]` attributes are tracked as temporary debt in `policy/clippy-debt.toml` so this PR can land the policy surface before the cleanup PRs migrate every call site.

## No test carveouts

The standard is workspace panic-free, not production-only panic-free. Do not add these settings to `clippy.toml`:

```toml
allow-unwrap-in-tests = true
allow-expect-in-tests = true
allow-panic-in-tests = true
allow-indexing-slicing-in-tests = true
allow-dbg-in-tests = true
```

Prefer fallible tests:

```rust
#[test]
fn parses_fixture() -> Result<(), Box<dyn std::error::Error>> {
    let fixture = std::fs::read_to_string("tests/fixtures/input.cbl")?;
    let parsed = parse_copybook(&fixture)?;

    assert_eq!(parsed.items.len(), 3, "fixture should expose three items");
    Ok(())
}
```

## Upgrade ledger

`policy/clippy-lints.toml` tracks Rust 1.94 and 1.95 planned flips before the MSRV bump. The `cargo xtask check-lint-policy` gate verifies that planned lints stay planned until the workspace MSRV reaches their activation version.

## Checks

Run the policy gate with:

```console
cargo xtask check-lint-policy
```

The check verifies:

- workspace MSRV matches the policy ledger;
- all workspace members inherit `[lints] workspace = true`;
- active lints in `policy/clippy-lints.toml` match the root `Cargo.toml` lint block;
- planned 1.94/1.95 lints are not active before their MSRV bump;
- `clippy.toml` does not enable test carveouts;
- current `#[allow]` suppressions are covered by expiring debt receipts;
- debt entries have owner, reason, path, lint, and unexpired expiry;
- non-Rust programming files are covered by structured policy entries.

Follow-up PRs should shrink `policy/clippy-debt.toml`, migrate broad `#[allow]` attributes to narrow `#[expect(..., reason = "...")]` receipts, and then make source-suppression debt more specific.
