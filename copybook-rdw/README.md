# copybook-rdw

RDW (Record Descriptor Word) header primitives for copybook-rs.

This crate has one responsibility: parse/build RDW headers and provide
buffered helpers for RDW framing (`peek/read/slice`).

## Public API

- `RdwHeader`
- `rdw_try_peek_len`
- `rdw_read_len`
- `rdw_slice_body`
- `rdw_validate_and_finish`
- `rdw_payload_len_to_u16`
