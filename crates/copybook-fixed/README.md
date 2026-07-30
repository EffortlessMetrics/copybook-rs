# copybook-fixed

Fixed-length (LRECL) record framing primitives for copybook-rs.

## Overview

This crate provides streaming reader and writer types for fixed-length record framing.
`FixedRecordReader` consumes LRECL-sized records from a byte stream, while
`FixedRecordWriter` produces them with automatic null-byte padding for short payloads.

## Usage

```rust
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use std::io::Cursor;

// Write fixed-length records (LRECL = 8)
let mut output = Vec::new();
let mut writer = FixedRecordWriter::with_lrecl(&mut output, 8).unwrap();
writer.write_record(b"ABCD").unwrap(); // padded to 8 bytes
writer.flush().unwrap();

// Read fixed-length records
let mut reader = FixedRecordReader::with_lrecl(Cursor::new(&output), 8).unwrap();
let record = reader.read_record().unwrap().unwrap();
assert_eq!(&record[..4], b"ABCD");
```

## Public API

- `FixedRecordReader<R>` — Streaming reader for fixed-length records
- `FixedRecordWriter<W>` — Streaming writer with automatic padding

The `with_lrecl` constructors are the canonical schema-independent framing
surface. Copybook schema compatibility is validated by `copybook-codec` before
it constructs these framing primitives.

## License

AGPL-3.0-or-later
