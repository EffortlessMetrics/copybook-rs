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

Each module re-exports the corresponding published component crate:

| Module | Component crate |
| --- | --- |
| `charset` | `copybook-charset` |
| `codec` | `copybook-codec` |
| `codepage` | `copybook-codepage` |
| `contracts` | `copybook-contracts` |
| `core` | `copybook-core` |
| `determinism` | `copybook-determinism` |
| `error` | `copybook-error` |
| `error_reporter` | `copybook-error-reporter` |
| `fixed` | `copybook-fixed` |
| `governance_contracts` | `copybook-governance-contracts` |
| `options` | `copybook-options` |
| `overflow` | `copybook-overflow` |
| `overpunch` | `copybook-overpunch` |
| `rdw` | `copybook-rdw` |
| `record_io` | `copybook-record-io` |
| `support_matrix` | `copybook-support-matrix` |
| `utils` | `copybook-utils` |

Use the component crates directly when you need the smallest possible dependency
surface. Use `copybook` when you want the canonical project entrypoint and a
single dependency over the public crate family.

Add `copybook` in your `Cargo.toml`:

```toml
[dependencies]
copybook = "0.5"
```
