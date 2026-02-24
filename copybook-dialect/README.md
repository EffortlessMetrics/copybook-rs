# copybook-dialect

Shared dialect contract for ODO (`OCCURS DEPENDING ON`) `min_count` semantics.

This crate provides:
- `Dialect` enum (`Normative`, `ZeroTolerant`, `OneTolerant`)
- `effective_min_count(dialect, declared_min_count)` helper
- String parsing (`FromStr`) and CLI-friendly display (`Display`)

## Usage

```rust
use copybook_dialect::{Dialect, effective_min_count};
use std::str::FromStr;

let dialect = Dialect::from_str("1")?;
assert_eq!(effective_min_count(dialect, 0), 1);
# Ok::<(), String>(())
```
