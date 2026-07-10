<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# claude.md

## Scope
- Kafka streaming example with separate producer and consumer binaries.

## Navigation
- `README.md` for runbook and local compose instructions.
- `sample_data/` for schema + payload fixtures.
- Repository root + invariants: `../../AGENTS.md`

## Run
- Producer: `cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example producer`
- Consumer: `cargo run --manifest-path examples/kafka_streaming/Cargo.toml --example consumer`
