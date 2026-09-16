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
All paths are task/domain-shaped; the 0.5 retired paths (`overflow`,
`utils`) and the 0.6 compatibility aliases were removed in 0.7. The
`docs/migration/` guides record each window:

| Module | Component crate | Status |
| --- | --- | --- |
| `charset` | `copybook-charset` | preferred |
| `codec` | `copybook-codec` | preferred |
| `core` | `copybook-core` | preferred |
| `error` | `copybook-error` | preferred |
| `framing` | `copybook-fixed` | preferred (fixed-length records) |
| `framing` | `copybook-rdw` | preferred (RDW records) |
| `governance` | `copybook-governance` | adapter (selected) |
| `support_matrix` | `copybook-support-matrix` | contract (selected) |

Retired 0.5 paths (`overflow`, `utils`) are gone: their behavior moved to
the owning crates under #655, and the 0.5.0 artifacts stay on crates.io.

Use the component crates directly when you need the smallest possible dependency
surface. Use `copybook` when you want the canonical project entrypoint and a
single dependency over the public crate family.

Use `copybook::charset` for character conversion and
`copybook::codec::options` for codec operation options. Record framing lives
under `copybook::framing` (`fixed`, `rdw`). The installation example below
uses the currently published 0.6 release.

Add `copybook` in your `Cargo.toml`:

```toml
[dependencies]
copybook = "0.8"
```
