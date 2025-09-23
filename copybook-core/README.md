# copybook-core

Core COBOL copybook parser, schema, and validation primitives.

## Overview

`copybook-core` provides the fundamental parsing and schema representation for COBOL copybooks. It handles lexical analysis, parsing, AST construction, and layout resolution to create a structured schema that can be used for data processing.

## Features

- **Complete COBOL Grammar**: Supports all major COBOL data types and structures
- **Memory-Safe Parsing**: Zero unsafe code with comprehensive error handling
- **Schema Generation**: Produces structured layouts for data processing
- **Field Resolution**: Handles REDEFINES, ODO (Occurs Depending On), SYNC alignment
- **Format Support**: COMP, COMP-3, DISPLAY, BINARY fields with proper sizing

## Quick Start

```rust
use copybook_core::parse_copybook;

let copybook_text = r#"
01 CUSTOMER-RECORD.
   05 CUSTOMER-ID    PIC 9(6).
   05 CUSTOMER-NAME  PIC X(30).
   05 BALANCE        PIC S9(7)V99 COMP-3.
"#;

let schema = parse_copybook(copybook_text)?;
println!("Parsed {} fields", schema.fields().len());
```

## API Documentation

See the [full documentation](https://docs.rs/copybook-core) for detailed API reference.

## Performance & Production Use

For performance specifications, production readiness details, and comprehensive examples, see the [main copybook-rs README](https://github.com/EffortlessMetrics/copybook-rs#readme).

## License

Licensed under **AGPL-3.0-or-later**. See [LICENSE](../LICENSE).
