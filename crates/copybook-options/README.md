# copybook-options

Configuration primitives for copybook codec behavior.

## Overview

Defines the `DecodeOptions` and `EncodeOptions` structs that control record format, codepage,
JSON number mode, raw capture, zoned encoding preferences, and float format. These option
types are the shared configuration surface used by the codec and CLI layers.

## Usage

```rust
use copybook_options::{DecodeOptions, RecordFormat, JsonNumberMode, Codepage};

let options = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless)
    .with_threads(4);
```

## Public API

- `DecodeOptions` / `EncodeOptions` — Builder-pattern configuration structs
- `RecordFormat` — `Fixed` or `RDW`
- `JsonNumberMode` — `Lossless` or `Native`
- `RawMode` — `Off`, `Record`, `Field`, `RecordRDW`
- `FloatFormat` — `IeeeBigEndian` or `IbmHex`
- `Codepage` / `UnmappablePolicy` / `ZonedEncodingFormat` — Re-exported charset types

## License

AGPL-3.0-or-later
