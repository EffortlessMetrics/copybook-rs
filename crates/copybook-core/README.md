<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# copybook-core

Core parsing and schema primitives for COBOL copybooks.

`copybook-core` is the foundation crate for all copybook-rs parsing and layout workflows.

## What it does

- Parse COBOL copybook text and emit a validated schema model.
- Resolve field offsets, lengths, OCCURS/ODO, REDEFINES, and sync/alignment details.
- Apply parser options and feature flags used by higher-level crates.
- Report structured, location-aware errors for integration use.
- Support Level-88 condition values in parsed schema.
- Provide dialect lever for ODO min_count interpretation (`Normative`, `ZeroTolerant`, `OneTolerant`).
- Project schemas to selected fields via `project_schema()` for field-level decoding.

## API example

```rust
use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions};
use copybook_core::dialect::Dialect;

let text = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(6).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC S9(7)V99 COMP-3.
   05 STATUS         PIC X.
      88 ACTIVE      VALUE 'A'.
      88 INACTIVE    VALUE 'I'.
"#;

// Default parsing
let schema = parse_copybook(text)?;

// Parse with IBM Enterprise dialect (zero-tolerant ODO)
let opts = ParseOptions {
    dialect: Dialect::ZeroTolerant,
    allow_inline_comments: true,
    ..Default::default()
};
let schema = parse_copybook_with_options(text, &opts)?;

// Field projection
use copybook_core::project_schema;
let projected = project_schema(&schema, &["CUSTOMER-ID".to_string(), "BALANCE".to_string()])?;
```

## API docs

See [docs.rs/copybook-core](https://docs.rs/copybook-core).

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
