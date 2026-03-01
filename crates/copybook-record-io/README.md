<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-record-io

Record-format dispatch for copybook-rs.

## Overview

Provides a format-agnostic API for single-record I/O that dispatches to either fixed-length
(LRECL) or RDW (variable-length) framing. This crate bridges the dedicated framing microcrates
(`copybook-fixed`, `copybook-rdw`) behind a unified `read_record` / `write_record` interface.

## Usage

```rust
use copybook_record_io::{read_record, write_record};
use copybook_options::RecordFormat;
use std::io::Cursor;

// Write a fixed-length record
let mut output = Vec::new();
write_record(&mut output, b"DATA", RecordFormat::Fixed).unwrap();

// Read it back
let mut input = Cursor::new(&output);
let record = read_record(&mut input, RecordFormat::Fixed, Some(4)).unwrap();
assert_eq!(record.unwrap(), b"DATA");
```

## Public API

- `read_record` / `write_record` — Format-agnostic single-record I/O
- Re-exports: `FixedRecordReader`, `FixedRecordWriter`, `RDWRecord`, `RDWRecordReader`, `RDWRecordWriter`

## License

AGPL-3.0-or-later
