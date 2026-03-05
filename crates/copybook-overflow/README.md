# copybook-overflow

Overflow-safe integer narrowing and bounds arithmetic for copybook-rs.

## Overview

This microcrate isolates checked arithmetic and checked narrowing conversions that are
performance-sensitive and correctness-critical. All functions return structured
`copybook_error::Result<T>` with domain-specific error codes on overflow, ensuring
COBOL array bounds and record sizes are computed safely.

## Usage

```rust
use copybook_overflow::{safe_array_bound, safe_u64_to_u32};

// Safely compute array end offset: base + (count * item_size)
let end = safe_array_bound(10, 3, 4, "my-array").unwrap();
assert_eq!(end, 22);

// Safely narrow u64 to u32
let narrow = safe_u64_to_u32(42, "record-length").unwrap();
assert_eq!(narrow, 42u32);
```

## Public API

- `safe_array_bound` — Overflow-checked `base + count * item_size`
- `safe_u64_to_u32` — Checked narrowing from `u64` to `u32`
- `safe_u64_to_u16` — Checked narrowing from `u64` to `u16`
- `safe_usize_to_u32` — Checked narrowing from `usize` to `u32`

## License

AGPL-3.0-or-later
