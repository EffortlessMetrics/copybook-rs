<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-record-io

Compatibility forwarding package for copybook-rs record dispatch.

## Overview

This published 0.5 package remains resolvable for compatibility.

## Usage

```rust
use copybook_record_io::{read_record, write_record};
use copybook_codec::options::RecordFormat;
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

- `read_record` / `write_record` — forwarded format-agnostic single-record I/O
- Forwarded framing types: `FixedRecordReader`, `FixedRecordWriter`, `RDWRecord`, `RDWRecordReader`, `RDWRecordWriter`

## License

AGPL-3.0-or-later
