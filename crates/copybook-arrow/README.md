<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-arrow

Typed conversion layer between `copybook-rs` schemas/records and Apache Arrow/Parquet.

## What it does

- Convert `copybook_core::Schema` to Arrow schema (`cobol_schema_to_arrow`).
- Build typed Arrow record batches via `stream_to_batches` and `RecordBatchBuilder`.
- Write Arrow IPC and Parquet output (`write_ipc`, `write_parquet`).
- Preserve COBOL semantic meaning in Arrow types for easier analytics.

The legacy JSON-based API is still available (`json_to_schema`, `json_to_record_batch`,
`ArrowWriter`, `ParquetFileWriter`) but marked deprecated in favor of the typed API.

## Example

```rust
use copybook_arrow::{stream_to_batches, ArrowOptions};
use copybook_core::parse_copybook;
use std::io::Cursor;

let schema = parse_copybook("01 EMPLOYEE.\n   05 EMPLOYEE-ID PIC 9(9).\n   05 EMPLOYEE-NAME PIC X(20).\n")?;
let opts = ArrowOptions::default();

let source = Cursor::new(vec![0u8; 100]);
let batches = stream_to_batches(source, &schema, &opts)?;
```

## Examples and CLI

- `examples/decode_to_arrow.rs` — decode a single record to Arrow structures.
- `examples/decode_to_parquet.rs` — decode and write Parquet with compression.
- `examples/batch_processing.rs` — batched write workflow.

## API docs

See [docs.rs/copybook-arrow](https://docs.rs/copybook-arrow) for full API reference.

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
