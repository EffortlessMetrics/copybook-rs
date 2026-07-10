<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-cli Guidance

This file extends [`crates/AGENTS.md`](../AGENTS.md). The CLI owns argument
parsing, IO orchestration, exit behavior, and presentation. Keep parsing,
schema, encode/decode, and governance domain rules in their library crates.

Clap definitions and exercised behavior are the starting point for CLI truth;
keep `docs/CLI_REFERENCE.md`, examples, help text, and completion surfaces in
sync. Preserve stable error codes, causal stderr context, machine-readable
stdout, and documented exit statuses. Never mix progress or diagnostics into
JSON/JSONL stdout.

Cover command success, invalid input, IO failure, and stdout/stderr separation
near the changed command. Run focused CLI tests and relevant end-to-end tests,
then package fmt/Clippy and the appropriate repository gate.
