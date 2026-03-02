# copybook-rdw

RDW (Record Descriptor Word) header primitives for copybook-rs.

This crate has one responsibility: parse/build RDW headers and provide
buffered helpers plus reader/writer types for RDW framing.

Low-level header helpers are implemented in `copybook-rdw-header` and
re-exported here for API compatibility.

## Public API

- `RdwHeader`
- `RDWRecord`
- `RDWRecordReader`
- `RDWRecordWriter`
- `rdw_try_peek_len`
- `rdw_read_len`
- `rdw_slice_body`
- `rdw_validate_and_finish`
- `rdw_payload_len_to_u16`
