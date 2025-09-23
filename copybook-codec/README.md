# copybook-codec

High-throughput COBOL copybook codec: decode/encode, streaming, codepages.

## Overview

`copybook-codec` provides high-performance encoding and decoding of COBOL binary data to/from JSON. It handles character conversion, numeric format translation, and streaming I/O for production data processing workloads.

## Features

- **Exceptional Performance**: 4.1+ GiB/s DISPLAY, 560+ MiB/s COMP-3 processing
- **Memory Efficient**: <256 MiB steady-state for multi-GB files
- **Complete Codepage Support**: CP037, CP273, CP500, CP1047, CP1140 + ASCII
- **Streaming Architecture**: Bounded memory usage with parallel processing
- **Lossless Conversion**: Guaranteed round-trip fidelity for binary↔JSON
- **Record Format Support**: Fixed-length and variable (RDW) records

## Quick Start

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage, JsonNumberMode};

// Parse copybook schema
let schema = parse_copybook(copybook_text)?;

// Configure decoder
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless)
    .with_emit_meta(true);

// Decode binary record to JSON
let json_value = decode_record(&schema, &record_data, &options)?;
```

## Streaming Processing

```rust
use copybook_codec::iter_records_from_file;

let iterator = iter_records_from_file("data.bin", &schema, &options)?;
for record_result in iterator {
    let json_value = record_result?;
    // Process record
}
```

## API Documentation

See the [full documentation](https://docs.rs/copybook-codec) for detailed API reference.

## Performance & Production Use

For complete performance specifications, benchmarks, and production deployment guidance, see the [main copybook-rs README](https://github.com/EffortlessMetrics/copybook-rs#readme).

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
