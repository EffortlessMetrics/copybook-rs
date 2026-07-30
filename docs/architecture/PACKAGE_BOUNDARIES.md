<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Package boundaries

`copybook-rs` designs implementation seams aggressively but publishes packages conservatively. A Cargo package is a distribution and compatibility promise, not merely a convenient folder.

## Decision rule

A production seam remains a published package only when it is one of:

1. a canonical product or application entrypoint;
2. an independently useful domain engine;
3. a shared leaf required by multiple surviving public packages without dependency inversion;
4. a real external-format or ecosystem adapter;
5. the permanent `copybook-rs` search alias; or
6. a finite compatibility package during an already-published migration.

Single-owner implementation seams remain explicit module families under their true owner. They keep focused folders, narrow visibility, invariant tests, fuzz/property targets, and review ownership without retaining a separate `Cargo.toml` or crates.io support promise.

## Required package evidence

A retained public package must name an external user story, its invariant, its dependency direction, its stability class, and why a module is insufficient. Compatibility and retiring package names must instead state their finite migration story, forwarding/no-new-implementation invariant, and why package retention is temporary. The authoritative decisions live in `docs/stability/surface-registry.json` under each package's `boundary` object. Consumer and publish-closure data are generated from Cargo metadata by the architecture checker tracked in #644.

## Migration rules

- Never yank 0.5.0 artifacts.
- Primary packages never depend on forwarding packages.
- Ownership moves and behavior corrections use separate PRs whenever practical.
- Compatibility packages contain only re-exports, deprecations, migration documentation, and compile-contract tests.
- Deep packages already classified internal-only receive no automatic forwarding release; they remain available at 0.5.0 and stop receiving versions once primary consumers move.
- `copybook-rs` remains a permanent alias containing only `pub use copybook::*;`.

## Working primary family for 0.6.0

The current target is `copybook`, `copybook-rs`, `copybook-core`, `copybook-codec`, `copybook-error`, `copybook-charset`, `copybook-cli`, and `copybook-arrow`. `copybook-fixed` is retained as a primary external-format package after the clean-room proof in `examples/copybook-fixed-clean-room`; `copybook-rdw` remains conditional on its own schema-independent direct-use proof. Governance and support-matrix packages remain conditional rather than being promoted by topology alone.
