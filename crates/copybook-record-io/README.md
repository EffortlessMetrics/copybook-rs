<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-record-io

Record-format dispatch primitives for copybook-rs.

This crate has one responsibility: provide a stable fixed-vs-RDW dispatch
surface for legacy single-record read/write operations while re-exporting the
dedicated framing microcrate types.

## Public API

- `read_record`
- `write_record`
- `FixedRecordReader`
- `FixedRecordWriter`
- `RDWRecord`
- `RDWRecordReader`
- `RDWRecordWriter`
