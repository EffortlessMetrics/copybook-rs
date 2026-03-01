# copybook-rdw

RDW (Record Descriptor Word) header primitives for copybook-rs.

## Overview

Handles variable-length record framing using the mainframe RDW convention: each record is
prefixed with a 4-byte header (2-byte big-endian payload length + 2 reserved bytes).
Provides streaming `RDWRecordReader` / `RDWRecordWriter` for record I/O, plus lower-level
helpers for custom framing scenarios.

## Usage

```rust
use copybook_rdw::{RDWRecordWriter, RDWRecordReader};

// Write an RDW-framed record
let mut output = Vec::new();
let mut writer = RDWRecordWriter::new(&mut output);
writer.write_record_from_payload(b"HELLO", None).unwrap();

// Read it back
let mut reader = RDWRecordReader::new(std::io::Cursor::new(&output), false);
let record = reader.read_record().unwrap().unwrap();
assert_eq!(record.payload, b"HELLO");
```

## Public API

- `RdwHeader` — Parsed 4-byte RDW header
- `RDWRecord` — Header + payload pair
- `RDWRecordReader<R>` — Streaming reader for RDW-framed records
- `RDWRecordWriter<W>` — Streaming writer for RDW-framed records
- `rdw_read_len`, `rdw_slice_body`, `rdw_try_peek_len` — Low-level helpers

## License

AGPL-3.0-or-later
