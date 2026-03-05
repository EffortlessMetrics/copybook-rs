# copybook-safe-ops

Panic-safe conversion and arithmetic helpers extracted from `copybook-utils`.

This crate owns narrowly scoped helpers that keep operations fallible:

- parsing numeric text into integers,
- checked division and array-bound arithmetic,
- checked numeric narrowing conversions,
- checked string and slice access helpers,
- safe string write utilities.

Division and slice access helpers are delegated to `copybook-safe-index`.
Text-oriented parsing/write helpers (`parse_*`, `safe_parse_*`, `safe_string_char_at`,
`safe_write*`) are delegated to `copybook-safe-text`.

All helpers return `copybook_error::Result` with explicit `copybook-error` codes and are intended
for internal copybook-rs crates where panic-free execution is required.
