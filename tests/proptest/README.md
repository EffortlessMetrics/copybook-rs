<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# `copybook-proptest`

`copybook-proptest` is the property-based testing package for copybook-rs.

## Purpose

- Validate parser and codec invariants across large input spaces.
- Catch regressions that are hard to reproduce with finite hand-written examples.
- Provide randomized coverage for parsing, serialization, arrays, and numeric coercion behavior.

## Current Scope

- Schema roundtrip invariants
- Parsing and encoding/decoding properties
- PIC-clause behavior coverage
- Numerical parsing and REDEFINES scenarios
- Determinism hash/diff invariants
- Negative/edge-case generation checks

## Running

```bash
# Run all property tests
cargo test -p copybook-proptest

# Run a specific module
cargo test -p copybook-proptest --test schema_invariants

# Run until enough generated cases for a long stress run
cargo test -p copybook-proptest -- --test-threads=1
```

## Structure

- `tests/proptest/main.rs` main entrypoint
- `tests/proptest/{arrays,determinism,numeric,parsing,pic_clauses,redefines,roundtrip,schema_invariants}.rs`
  for grouped property modules
- `tests/proptest/config.rs` and `tests/proptest/generators.rs` for generator definitions

## Notes

These tests are run as part of the project’s broader testing and quality gates.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../../LICENSE).
