# copybook-options

Compatibility forwarding package for copybook codec options.

## Overview

The option contracts now live under `copybook_codec::options`, where codec-owned operation
policy belongs. This published package remains available as a forwarding compatibility path
for existing 0.5 consumers.

## Usage

```rust
use copybook_options::{DecodeOptions, RecordFormat, JsonNumberMode, Codepage};

let options = DecodeOptions::new()
    .with_format(RecordFormat::Fixed)
    .with_codepage(Codepage::CP037)
    .with_json_number_mode(JsonNumberMode::Lossless)
    .with_threads(4);
```

## Forwarded API

- `DecodeOptions` / `EncodeOptions` — Builder-pattern configuration structs
- `RecordFormat` — `Fixed` or `RDW`
- `JsonNumberMode` — `Lossless` or `Native`
- `RawMode` — `Off`, `Record`, `Field`, `RecordRDW`
- `FloatFormat` — `IeeeBigEndian` or `IbmHex`
- `Codepage` / `UnmappablePolicy` / `ZonedEncodingFormat` — Re-exported charset types

## RawMode variants

| Variant | Description |
|---------|-------------|
| `Off` | No raw payload captured (default) |
| `Record` | Capture record payload bytes in `__raw_b64` |
| `RecordRDW` | Capture RDW header + payload bytes in `__raw_b64` |
| `Field` | Capture per-field raw values in `<FIELD_NAME>__raw_b64` |

## License

AGPL-3.0-or-later
