<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-rs

Umbrella facade crate for the copybook-rs library surface.

`copybook-rs` is imported as `copybook_rs` in Rust code and re-exports the
public APIs from `copybook-core` and `copybook-codec` behind a single
dependency.

## Quick start

```toml
[dependencies]
copybook-rs = "0.4.3"
```

```rust
use copybook_rs::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_record, parse_copybook};

let schema = parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n")?;
let options = DecodeOptions::new()
    .with_codepage(Codepage::ASCII)
    .with_format(RecordFormat::Fixed)
    .with_json_number_mode(JsonNumberMode::Lossless);

let json = decode_record(&schema, b"A", &options)?;
assert_eq!(json["FLAG"], "A");
```

## What it re-exports

- `copybook-core` at the crate root and under `copybook_rs::core`
- `copybook-codec` at the crate root and under `copybook_rs::codec`
- `copybook-arrow` under `copybook_rs::arrow` when the `arrow` feature is enabled

## License

Licensed under **AGPL-3.0-or-later**.
