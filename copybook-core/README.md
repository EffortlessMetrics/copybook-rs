# copybook-core

Core parsing and schema primitives for COBOL copybooks.

`copybook-core` is the foundation crate for all copybook-rs parsing and layout workflows.

## What it does

- Parse COBOL copybook text and emit a validated schema model.
- Resolve field offsets, lengths, OCCURS/ODO, REDEFINES, and sync/alignment details.
- Apply parser options and feature flags used by higher-level crates.
- Report structured, location-aware errors for integration use.

## API example

```rust
use copybook_core::{parse_copybook, ParseOptions, parse_copybook_with_options};

let text = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(6).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC S9(7)V99 COMP-3.
"#;

let schema = parse_copybook(text)?;
let strict = ParseOptions { strict: true, allow_inline_comments: true, ..Default::default() };
let strict_schema = parse_copybook_with_options(text, &strict)?;

println!("fields: {}", schema.fields.len());
println!("strict fields: {}", strict_schema.fields.len());
```

## API docs

See [docs.rs/copybook-core](https://docs.rs/copybook-core).

## License

Licensed under AGPL-3.0-or-later. See [LICENSE](../LICENSE).
