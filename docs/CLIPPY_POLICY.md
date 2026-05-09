<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Clippy Policy

## Target Profile

`copybook-rs` targets MSRV **1.95** with a strict, uniform Clippy profile.
No test carveouts. Tests compile under the same pedantic lint profile as
production code.

## Invariants

| Invariant | Value |
|-----------|-------|
| `unsafe_code` | `forbid` — permanent; matches zero-unsafe public API guarantee |
| Suppression style | `#[expect(..., reason = "...")]` only; bare `#[allow(...)]` forbidden |
| Test carveouts | None — `allow-unwrap-in-tests = false`, `allow-expect-in-tests = false` |
| Blanket categories | Forbidden — no `#[expect(clippy::all)]` or similar |
| Rust default implementation | Always active — no `rust_default_implementation = false` |

## Current State vs Target State

| Lint | Current | Target (post PR 01) |
|------|---------|---------------------|
| `clippy::unwrap_used` | `warn` | `deny` |
| `clippy::expect_used` | `warn` | `deny` |
| `clippy::panic` | `warn` | `deny` |
| `clippy::unreachable` | `warn` | `deny` |
| `clippy::todo` | `warn` | `deny` |
| `clippy::unimplemented` | `warn` | `deny` |
| `clippy::dbg_macro` | `warn` | `deny` |
| `clippy::cast_lossless` | `allow` | `warn` |
| `clippy::cast_possible_truncation` | `allow` | `warn` |
| `clippy::cast_precision_loss` | `allow` | `warn` |
| `clippy::cast_sign_loss` | `allow` | `warn` |
| `clippy::missing_inline_in_public_items` | `warn` | `warn` (keep) |
| Test carveout (`-A clippy::unwrap_used` etc.) | Active in CI | **Removed** |

## Planned Lint Flips

These lints are planned but not yet active. They activate when the listed
MSRV ships and the workspace MSRV is bumped to meet or exceed it.

Tracked in `policy/clippy-lints.toml` under `[[planned]]`.

### Activate with MSRV 1.94

| Lint | Level | Rationale |
|------|-------|-----------|
| `clippy::same_length_and_capacity` | `deny` | Catch raw-parts reconstruction mistakes |
| `clippy::manual_ilog2` | `warn` | Prefer standard integer log helper |
| `clippy::decimal_bitwise_operands` | `warn` | Make bit masks visually inspectable |
| `clippy::needless_type_cast` | `warn` | Avoid stale numeric type drift |

### Activate with MSRV 1.95

| Lint | Level | Rationale |
|------|-------|-----------|
| `clippy::disallowed_fields` | `deny` | Ban direct field access across protected seams |
| `clippy::manual_checked_ops` | `warn` | Prefer checked arithmetic over manual guards |
| `clippy::manual_take` | `warn` | Use standard ownership helper |
| `clippy::manual_pop_if` | `warn` | Use collection APIs that encode predicate-and-pop intent |
| `clippy::duration_suboptimal_units` | `warn` | Make durations legible without mental conversion |
| `clippy::unnecessary_trailing_comma` | `warn` | Keep format macro calls clean |

## Debt and Exceptions

All temporary exceptions are tracked in `policy/clippy-debt.toml` and
`policy/clippy-exceptions.toml`.

Each exception entry must include:

```toml
[[exception]]
id = "clippy-XXXX"
lint = "clippy::unwrap_used"
path = "crates/copybook-X/src/Y.rs"
selector = "method_call::callee::unwrap"
owner = "team/owner"
reason = "..."
created = "YYYY-MM-DD"
expires = "YYYY-MM-DD"  # Required; max 90 days from creation
```

Entries without an expiry date are invalid and will fail `xtask policy-report`.

## Suppression Syntax

When source-level suppression is truly needed (after exhausting refactor options):

```rust
// Correct
#[expect(clippy::unwrap_used, reason = "invariant guaranteed by caller X; see issue #NNN")]
fn load_fixture(path: &Path) -> Vec<u8> { ... }

// Wrong — bare allow
#[allow(clippy::unwrap_used)]
fn load_fixture(path: &Path) -> Vec<u8> { ... }

// Wrong — no reason
#[expect(clippy::unwrap_used)]
fn load_fixture(path: &Path) -> Vec<u8> { ... }
```

## `clippy.toml` / `config/clippy.toml` Settings

After PR 01:

```toml
# No test-carveout permissions
allow-unwrap-in-tests = false
allow-expect-in-tests = false
allow-panic-in-tests = false
allow-indexing-slicing-in-tests = false
allow-dbg-in-tests = false

# Suppression style enforcement
msrv = "1.95"
```

## Commands

```bash
# Full strict check (matches CI)
cargo clippy --workspace --lib --bins --examples --tests --all-features \
  -- -D warnings -W clippy::pedantic

# Policy report (requires PR 02)
cargo run -p xtask -- policy-report

# Audit for unwrap/expect
cargo run -p xtask -- audit-unwraps --json
```
