<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Cross-Workspace Test Guidance

This file extends the root [`AGENTS.md`](../AGENTS.md). Tests here cover
cross-crate workflows, CLI behavior, BDD scenarios, property invariants, and
golden contracts; unit behavior should stay near the owning crate.

Use sanitized or generated data, deterministic seeds, and stable assertions.
Name tests for the behavior they protect, include the former failing input for
regressions, and keep fixtures minimal. Update golden data only when the
contract intentionally changes and explain the delta.

Use focused `cargo test --test <target>` or nextest selection first. Run
`just bdd-smoke` for the governed smoke set. Exploratory BDD, property, fuzz,
or broad stress execution is supporting evidence, not universal correctness or
release proof.
