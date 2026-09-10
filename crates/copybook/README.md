<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook

`copybook` is the canonical crates.io entrypoint for the
[`copybook-rs`](https://github.com/EffortlessMetrics/copybook-rs) project.

`copybook-rs` is an existing Rust crate family for COBOL copybook and mainframe
data tooling. The component crates remain available for users who want narrow
dependencies. This root crate provides a convenient facade over the public
component crates.

## Facade Modules

The facade keeps the public surface module-shaped and explicit:

```rust
use copybook::codec;
use copybook::core;
use copybook::error;
```

Each module re-exports the corresponding published component crate.
Preferred paths are task/domain-shaped; deprecated aliases keep the 0.5
surface compiling through the 0.6 migration window:

| Module | Component crate | Status |
| --- | --- | --- |
| `charset` | `copybook-charset` | preferred |
| `codec` | `copybook-codec` | preferred |
| `codepage` | `copybook-charset` | deprecated, use `charset` |
| `contracts` | `copybook-core` | deprecated, use `core::feature_flags` |
| `core` | `copybook-core` | preferred |
| `determinism` | `copybook-codec` | deprecated, use `codec::determinism` |
| `error` | `copybook-error` | preferred |
| `fixed` | `copybook-fixed` | deprecated, use `framing::fixed` |
| `framing` | `copybook-fixed` | preferred (fixed-length records) |
| `framing` | `copybook-rdw` | preferred (RDW records) |
| `governance` | `copybook-governance` | adapter (selected) |
| `options` | `copybook-codec` | deprecated, use `codec::options` |
| `overpunch` | `copybook-codec::numeric::overpunch` | deprecated, use `codec::numeric::overpunch` |
| `rdw` | `copybook-rdw` | deprecated, use `framing::rdw` |
| `record_io` | `copybook-codec` | deprecated, use `codec::record` |
| `support_matrix` | `copybook-support-matrix` | contract (selected) |

Retired 0.5 paths (`overflow`, `utils`) are gone: their behavior moved to
the owning crates under #655, and the 0.5.0 artifacts stay on crates.io.

Use the component crates directly when you need the smallest possible dependency
surface. Use `copybook` when you want the canonical project entrypoint and a
single dependency over the public crate family.

Use `copybook::charset` for new code. The `copybook::codepage` module remains
available as a deprecated compatibility alias during the planned 0.6 migration
window. The `copybook::options` module is also deprecated since 0.6.0; use
`copybook::codec::options` for codec operation options. Record framing moved
under `copybook::framing` (`fixed`, `rdw`); the flat modules are deprecated
aliases. The installation example below uses the currently published 0.6
release.

Add `copybook` in your `Cargo.toml`:

```toml
[dependencies]
copybook = "0.6"
```
