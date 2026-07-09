<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Fuzzing Guidance

This file extends the root [`AGENTS.md`](../AGENTS.md). This is a standalone
nightly `cargo-fuzz` project for hostile-input exploration.

Keep targets narrow, deterministic outside the fuzzer input, and free of
network or secret dependencies. Preserve and minimize a crash artifact before
turning it into a nearby regression test. Do not commit bulky generated corpora
without an explicit repository need. A clean fuzz run proves only the concrete
executions explored for that target, duration, toolchain, and corpus.
