<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook

Short alias package for [`copybook-rs`](https://docs.rs/copybook-rs).

Use this crate if you want the umbrella facade with the shorter import path
`copybook::...` instead of `copybook_rs::...`.

## Quick start

```toml
[dependencies]
copybook = "0.4.3"
```

```rust
use copybook::{Codepage, DecodeOptions, RecordFormat, decode_record, parse_copybook};

let schema = parse_copybook("01 REC.\n   05 FLAG PIC X(1).\n")?;
let options = DecodeOptions::new()
    .with_codepage(Codepage::ASCII)
    .with_format(RecordFormat::Fixed);

let json = decode_record(&schema, b"A", &options)?;
assert_eq!(json["FLAG"], "A");
```

## License

Licensed under **AGPL-3.0-or-later**.
