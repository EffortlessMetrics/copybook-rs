# copybook-safe-ops

Panic-safe conversion and arithmetic helpers extracted from `copybook-utils`.

This crate owns narrowly scoped helpers that keep operations fallible:

- parsing numeric text into integers,
- checked division and array-bound arithmetic,
- checked numeric narrowing conversions,
- checked string and slice access helpers,
- safe string write utilities.

All helpers return `copybook_error::Result` with explicit `copybook-error` codes and are intended
for internal copybook-rs crates where panic-free execution is required.
