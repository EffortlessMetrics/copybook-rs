# copybook-dialect

Deprecated compatibility package for the ODO (`OCCURS DEPENDING ON`) dialect
contract.

## Overview

The implementation now lives in `copybook-core::dialect`. This package forwards
the 0.5 API during the 0.6 compatibility window and will not receive new
implementation behavior.

## Usage

```rust
use copybook_dialect::{Dialect, effective_min_count};
use std::str::FromStr;

let dialect = Dialect::from_str("1")?; // OneTolerant (Micro Focus)
assert_eq!(effective_min_count(dialect, 0), 1);
# Ok::<(), String>(())
```

## Migration

Use the core-owned path for new code:

```rust
use copybook_core::dialect::{Dialect, effective_min_count};
```

## Compatibility API

- `Dialect` — `Normative`, `ZeroTolerant`, `OneTolerant`
- `effective_min_count(dialect, declared_min_count)` — Apply dialect rules

## License

AGPL-3.0-or-later
