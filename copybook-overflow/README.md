# copybook-overflow

Overflow-safe integer narrowing and bounds arithmetic for copybook-rs.

This crate is intentionally tiny and focused on one responsibility:
numeric operations that must fail safely instead of truncating or wrapping.

## Public API

- `safe_array_bound`
- `safe_u64_to_u32`
- `safe_u64_to_u16`
- `safe_usize_to_u32`

All functions return `copybook_error::Result<T>` with structured error codes.
