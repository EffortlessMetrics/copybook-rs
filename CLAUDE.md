<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Claude Code Entry Point

Read and follow [`AGENTS.md`](AGENTS.md), then the closest scoped `AGENTS.md`
for the files being changed. That hierarchy is the tool-neutral workflow and
maintenance contract.

Nested `claude.md` files are optional topology maps. They help locate modules,
tests, and ownership boundaries, but they are not behavior source truth. If a
map conflicts with the `AGENTS.md` hierarchy, canonical documentation, code,
tests, policy, or receipts, use the current canonical repository evidence.
