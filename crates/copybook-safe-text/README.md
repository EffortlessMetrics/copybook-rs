# copybook-safe-text

Panic-safe helpers for parsing textual inputs and working with string buffers.

This crate owns narrowly scoped helpers that avoid panics and map failures into
`copybook_error::Error` with explicit codes.

## Public API

- `parse_usize`
- `parse_isize`
- `safe_parse_u16`
- `safe_string_char_at`
- `safe_write`
- `safe_write_str`

These helpers are used by higher-level panic-safe utility crates to keep parse and
string operations fully fallible.
