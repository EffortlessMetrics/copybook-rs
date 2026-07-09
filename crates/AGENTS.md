<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Workspace Crate Guidance

This file extends the repository-level [`AGENTS.md`](../AGENTS.md) for crates.
Read the crate's `claude.md` and closer source/test maps for navigation, but use
code, tests, and canonical docs as behavior truth.

Keep dependency direction aligned with the main flow: parsing and schemas in
`copybook-core`, data transformation in `copybook-codec`, and user-facing
orchestration in `copybook-cli`. Governance, format, adapter, and compatibility
crates should retain one named responsibility. Do not move shared code merely
because it looks similar; extract only a shared invariant with proof on both
call sites.

Public APIs are support promises. When changing one, check downstream crates,
examples, the library API reference, support/stability docs, and error
contracts. Prefer crate-scoped build, test, rustdoc, and pedantic Clippy checks
before workspace gates. If adding or changing features, verify feature
combinations and the support-matrix contract.
