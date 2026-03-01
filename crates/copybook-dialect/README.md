# copybook-dialect

Shared dialect contract for ODO (`OCCURS DEPENDING ON`) `min_count` semantics.

## Overview

Different COBOL compilers interpret the minimum occurrence count in ODO declarations
differently. This crate provides the `Dialect` enum and an `effective_min_count` helper
that normalizes declared bounds according to the selected dialect (IBM, Micro Focus, etc.).

## Usage

```rust
use copybook_dialect::{Dialect, effective_min_count};
use std::str::FromStr;

let dialect = Dialect::from_str("1")?; // OneTolerant (Micro Focus)
assert_eq!(effective_min_count(dialect, 0), 1);
# Ok::<(), String>(())
```

## Public API

- `Dialect` — `Normative`, `ZeroTolerant`, `OneTolerant`
- `effective_min_count(dialect, declared_min_count)` — Apply dialect rules

## License

AGPL-3.0-or-later
