<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# How-To: Stream Records with the Iterator API

Decode COBOL records one at a time with bounded memory, choosing how to handle
errors, collect results, or access raw bytes.

## Goal

Use `copybook_codec`'s `RecordIterator` to decode fixed-length or RDW
variable-length records from a file or in-memory buffer, processing each record
individually without loading the entire file.

## Prerequisites

- A parsed copybook schema (`copybook_core::parse_copybook`)
- `copybook-codec` as a dependency (`cargo add copybook-codec`)
- Record data as a file path, byte slice, or any `std::io::Read`

For full API details, see the [Record Iterators reference](../reference/iterators.md).

## 1. Iterate records from an in-memory buffer

The simplest case: wrap a byte slice in `Cursor` and iterate.

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let copybook = "01 CUSTOMER.  05 ID PIC 9(5).  05 NAME PIC X(10).";
let schema = parse_copybook(copybook)?;
let options = DecodeOptions::default();

// Three 15-byte records (5-digit ID + 10-char name)
let data = b"00001ALICE      00002BOB        00003CAROL      ";

let iterator = iter_records(Cursor::new(data), &schema, &options)?;
for result in iterator {
    match result {
        Ok(json) => println!("{json}"),
        Err(e) => eprintln!("error: {e}"),
    }
}
```

## 2. Iterate records from a file

Use `iter_records_from_file` to open the file for you:

```rust
use copybook_codec::{iter_records_from_file, Codepage, DecodeOptions, RecordFormat};
use copybook_core::parse_copybook;

let schema = parse_copybook(include_str!("customer.cbl"))?;
let options = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::CP037); // EBCDIC

let iterator = iter_records_from_file("customers.dat", &schema, &options)?;
for result in iterator {
    match result {
        Ok(json) => println!("{json}"),
        Err(e) => eprintln!("error: {e}"),
    }
}
```

## 3. Recover from errors without stopping

Decode errors yield `Some(Err(...))` and **iteration continues** — so you can
skip bad records and keep going. Track a count to report at the end:

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook("01 REC.  05 ID PIC 9(5).  05 VAL PIC X(10).")?;
let options = DecodeOptions::default();

// Third record has non-numeric ID data -> decode error, others succeed
let data = b"00001GOODDATA00\
             00002GOODDATA01\
             XXXXXBADDATA02\
             00003GOODDATA03";

let mut ok = 0;
let mut err = 0;
for result in iter_records(Cursor::new(data), &schema, &options)? {
    match result {
        Ok(_) => ok += 1,
        Err(_) => err += 1,
    }
}
println!("decoded {ok} records, {err} errors");
```

## 4. Collect records into a Vec

For small inputs, collect the successful records into a vector:

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use serde_json::Value;
use std::io::Cursor;

let schema = parse_copybook("01 REC.  05 ID PIC 9(5).")?;
let data = b"000010000200003";

let records: Vec<Value> = iter_records(Cursor::new(data), &schema, &DecodeOptions::default())?
    .filter_map(Result::ok) // skip any decode errors
    .collect();

assert_eq!(records.len(), 3);
```

## 5. Access raw record bytes

When you need the undecoded bytes (e.g., to log them alongside the decoded
JSON, or to feed a secondary pipeline), use `read_raw_record()`:

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook("01 REC.  05 DATA PIC X(10).")?;
let options = DecodeOptions::default();
let data = b"RECORD0001RECORD0002";

let mut iter = iter_records(Cursor::new(data), &schema, &options)?;
while let Some(raw_bytes) = iter.read_raw_record()? {
    println!(
        "record {}: {} bytes",
        iter.current_record_index(),
        raw_bytes.len()
    );
}
```

> `read_raw_record()` and `next()` both advance the iterator — pick one mode per
> loop. See the [reference](../reference/iterators.md#raw-byte-access) for details.

## 6. Decode RDW variable-length records

Switch the format to `RecordFormat::RDW`. Each record is prefixed by a 4-byte
Record Descriptor Word (RDW) header; the iterator reads the header, then the
payload:

```rust
use copybook_codec::{iter_records, DecodeOptions, RecordFormat};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook("01 REC.  05 DATA PIC X(100).")?;
let options = DecodeOptions::new().with_format(RecordFormat::RDW);

// (Construct an RDW-framed input from your source — e.g. a file on disk.)
// let iterator = iter_records_from_file("transactions.dat", &schema, &options)?;
```

## Validation

Run the bundled example to see all six scenarios in action:

```sh
cargo run --example record_iterator -p copybook-codec
```

Confirm your own snippet compiles with:

```sh
cargo check
```

## When to use the iterator vs the bulk pipeline

- **Iterator** — per-record control: filtering, transformation, early
  termination, or raw-byte access. Single-threaded.
- **[`decode_file_to_jsonl`](../reference/LIBRARY_API.md)** — bulk conversion of
  a whole file to JSONL output. Multi-threaded via `options.with_threads(n)`.

See the [Record Iterators reference](../reference/iterators.md#iterator-vs-decode_file_to_jsonl)
for the full comparison.

## Further reading

- [Record Iterators reference](../reference/iterators.md)
- [Library API](../reference/LIBRARY_API.md)
- [Getting Started](../tutorials/getting-started.md)
