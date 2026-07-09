<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-core Guidance

This file extends [`crates/AGENTS.md`](../AGENTS.md). `copybook-core` owns COBOL
parsing, AST/schema construction, layout resolution, and schema audit; it must
not depend on codec or CLI behavior.

Treat copybook text as hostile input. Preserve source locations and stable
`CBKP*`/`CBKS*` diagnostics, use checked layout arithmetic, and keep dialect,
ODO, REDEFINES/RENAMES, and edited-PIC behavior aligned with their canonical
documents. Put parser edge cases beside the parser seam and use fixture or
integration coverage for resolved-layout contracts.

Start with scoped core tests and pedantic Clippy. When schema shapes or public
types change, verify downstream codec/CLI consumers and update the library API,
support matrix, or compatibility documentation that owns the changed claim.
