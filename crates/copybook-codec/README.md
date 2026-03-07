<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-codec

High-performance COBOL copybook codec for mainframe-style fixed and RDW records.

`copybook-codec` converts decoded/encoded records between COBOL binary formats (DISPLAY, COMP-3, COMP/BINARY) and JSON.

## What it does

- Parse and validate copybook schema output from `copybook-core`.
- Decode fixed-length and variable-length (RDW) files into JSON records.
- Encode JSON back to binary COBOL layouts with strict or tolerant mode.
- Configure behavior through `DecodeOptions` / `EncodeOptions`.
- Handle common mainframe codepages (`CP037`, `CP273`, `CP500`, `CP1047`, `CP1140`, ASCII).

## Quick start

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_record, DecodeOptions, Codepage, JsonNumberMode};

let schema = parse_copybook("01 A.\n   05 AMOUNT PIC S9(7)V99 COMP-3.\n")?;
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless);

let json = decode_record(&schema, &vec![0x00; 4], &options)?;
println!("{json}");
```

## Features

- Record streaming for large fixed-width files
- Deterministic conversions with controlled rounding/precision behavior
- Optional metrics and strict compatibility-policy checks
- Optional audit-related feature path for enterprise usage
- Edited PIC decode and encode support (E2/E3 phases)
- Field projection via `project_schema()` for selective field decoding
- Raw data capture modes (`RawMode::Record`, `RawMode::Field`, `RawMode::RecordRDW`)
- Scratch buffer optimization for hot-path performance (`decode_record_with_scratch`)

## Scratch buffer example

```rust
use copybook_core::parse_copybook;
use copybook_codec::{decode_record_with_scratch, DecodeOptions, Codepage, JsonNumberMode};
use copybook_codec::memory::ScratchBuffers;

let schema = parse_copybook("01 A.\n   05 AMOUNT PIC S9(7)V99 COMP-3.\n")?;
let options = DecodeOptions::new()
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless);

let mut scratch = ScratchBuffers::new();
let json = decode_record_with_scratch(&schema, &record_data, &options, &mut scratch)?;
```

## API docs

See [docs.rs/copybook-codec](https://docs.rs/copybook-codec).

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
