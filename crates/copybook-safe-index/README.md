# copybook-safe-index

Panic-safe helpers for indexing and arithmetic operations used by higher-level safety
facades in copybook-rs.

## Public API

- `safe_divide`
- `safe_slice_get`

Functions return `copybook_error::Error` with explicit error codes instead of panicking.
