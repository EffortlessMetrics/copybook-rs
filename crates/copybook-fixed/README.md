# copybook-fixed

Fixed-length (LRECL) record framing primitives for copybook-rs.

This crate has one responsibility: read and write fixed-size records with
consistent error mapping and padding behavior.

## Public API

- `FixedRecordReader`
- `FixedRecordWriter`
