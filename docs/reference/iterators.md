<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Record Iterators

`copybook-codec` exposes a streaming iterator API for decoding COBOL records
one at a time with bounded memory. This page documents the public types,
their error semantics, and when to choose the iterator over the bulk
[`decode_file_to_jsonl`](LIBRARY_API.md) pipeline.

## Public entry points

All three live at the crate root (`copybook_codec::`) and are defined in
[`crates/copybook-codec/src/iterator.rs`](../../crates/copybook-codec/src/iterator.rs).

| Entry point | Signature | Use when |
|---|---|---|
| `iter_records` | `(reader: R, schema, options) -> Result<RecordIterator<R>>` where `R: Read` | You have any readable source (file, `Cursor`, network stream) |
| `iter_records_from_file` | `(file_path: P, schema, options) -> Result<RecordIterator<File>>` | You have a file path and want the file opened for you |
| `RecordIterator::new` | `(reader: R, schema, options) -> Result<Self>` | You want the constructor directly (equivalent to `iter_records`) |

`iter_records` and `iter_records_from_file` are thin convenience wrappers over
`RecordIterator::new`.

## The `RecordIterator` type

```rust
pub struct RecordIterator<R: Read> { /* private fields */ }

impl<R: Read> Iterator for RecordIterator<R> {
    type Item = Result<serde_json::Value>;
    fn next(&mut self) -> Option<Self::Item>;
}
```

Each call to `next()` decodes **one record** into a `serde_json::Value`. The
iterator owns its own record buffer (typically < 32 KiB per record), so it can
process multi-gigabyte files in bounded memory.

### Accessor methods

| Method | Returns | Notes |
|---|---|---|
| `current_record_index()` | `u64` | 1-based index of the most recently read record |
| `is_eof()` | `bool` | `true` once the underlying reader is exhausted |
| `schema()` | `&Schema` | Borrowed reference to the parsed copybook schema |
| `options()` | `&DecodeOptions` | Borrowed reference to the decode options |
| `read_raw_record()` | `Result<Option<Vec<u8>>>` | Read raw record **bytes** without JSON decoding |

### Raw byte access

`read_raw_record()` gives you the undecoded record bytes, useful when you need
the raw data alongside (or instead of) the JSON decode. It advances the
iterator the same way `next()` does, so the two cannot be freely interleaved
on the same record — pick one mode per iteration loop.

```rust
use copybook_codec::{iter_records, DecodeOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook("01 REC.  05 DATA PIC X(10).")?;
let options = DecodeOptions::default();
let mut iter = iter_records(Cursor::new(b"RECORD0001RECORD0002"), &schema, &options)?;

while let Some(raw) = iter.read_raw_record()? {
    println!("record {} = {} bytes", iter.current_record_index(), raw.len());
}
```

## Error semantics

The iterator **never panics**. Errors surface as `Result` values:

- **Decode failure on a record** → `next()` yields `Some(Err(...))`, and
  **iteration continues** to the next record. This makes error recovery
  straightforward — `match` on each item and decide whether to skip, log, or
  stop:
  ```rust
  for result in iterator {
      match result {
          Ok(value) => { /* process */ }
          Err(e) => eprintln!("skipping bad record: {e}"),
      }
  }
  ```

- **Truncated record at EOF** → `next()` returns `Ok(None)` (a clean stop),
  **not** an error. A short trailing record is treated as end-of-input.

- **Missing `lrecl_fixed` with `Fixed` format** → the iterator *constructs
  successfully* (validation is deferred), but the first `next()` /
  `read_raw_record()` yields `Err(CBKI001_INVALID_STATE)`. Set
  `schema.lrecl_fixed` (the parser does this automatically for copybooks with a
  record length) or use `RecordFormat::RDW`.

- **RDW underflow** (corrupt/truncated RDW header) → `Err(CBKF221_RDW_UNDERFLOW)`.

- **File-open failure** (`iter_records_from_file`) → `Err(CBKI001_INVALID_STATE)`
  with a message like `"failed to open input file: ..."`.

> **Lazy validation note:** `RecordIterator::new` does **not** validate
> `schema.lrecl_fixed` at construction time. Validation happens on the first
> read. This is by design — it keeps construction infallible and surfaces
> format mismatches at the point they actually matter (when bytes are read).

## Iterator vs `decode_file_to_jsonl`

The crate offers two decode paths. They are **independent implementations**
(the iterator does its own framing inline rather than reusing
`FixedRecordReader`/`RDWRecordReader`):

| | `RecordIterator` | `decode_file_to_jsonl` |
|---|---|---|
| **Output** | One `serde_json::Value` per record (in memory) | JSONL lines written to a `Write` sink |
| **Control** | Per-record: filter, transform, short-circuit | Whole-file pipeline |
| **Parallelism** | Single-threaded | Multi-threaded (`options.with_threads(n)`) |
| **Error handling** | `Result` per record, iteration continues | Collects into `RunSummary`, continues |
| **Memory** | One record buffer | Streaming, bounded |
| **Best for** | Programmatic inspection, transforms, early exit | Bulk file → JSONL conversion |

Use the **iterator** when you need per-record control (filtering, transformation,
early termination, or access to raw bytes). Use **`decode_file_to_jsonl`** for
high-throughput bulk conversion to a JSONL file.

## Configuration

Iterators honor the same `DecodeOptions` as the bulk pipeline:

```rust
use copybook_codec::{iter_records, Codepage, DecodeOptions, RecordFormat};

let options = DecodeOptions::new()
    .with_format(RecordFormat::RDW)          // Fixed (default) or RDW
    .with_codepage(Codepage::CP037)          // EBCDIC codepage
    .with_strict_mode(true)                  // fail-fast on field errors
    .with_max_errors(100);                   // error budget (bulk path only)
```

See [`DecodeOptions`](LIBRARY_API.md) for the full builder surface.

## Runnable examples

A complete, runnable example covering six scenarios (basic iteration, error
recovery, collecting to `Vec`, raw access, file-based, and RDW) lives at
[`crates/copybook-codec/examples/record_iterator.rs`](../../crates/copybook-codec/examples/record_iterator.rs).
Run it with:

```sh
cargo run --example record_iterator -p copybook-codec
```

For a step-by-step tutorial, see [Streaming Decode How-To](../how-to/streaming-decode.md).
