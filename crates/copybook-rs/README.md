<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs

The canonical crate is [`copybook`](https://crates.io/crates/copybook).

This package exists so crates.io searches for the project name `copybook-rs`
land on the correct project. It is a thin redirect package and does not define
an independent API surface.

Use `copybook` directly in new projects:

```toml
[dependencies]
copybook = "0.4.3"
```
