# copybook-record-format

Microcrate for copybook-rs fixed-vs-RDW record format contracts.

This crate provides:
- `RecordFormat` enum (`Fixed` | `RDW`)
- convenience helpers (`is_fixed`, `is_variable`, `description`)

It is intentionally minimal so other crates can depend on record format semantics
without pulling the full options surface.
