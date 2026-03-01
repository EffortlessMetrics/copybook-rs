# copybook-corruption-detectors

Single-purpose corruption detectors for transfer-corruption heuristics.

This crate owns the concrete scanning logic for:

- EBCDIC control-byte style corruption in text-like fields.
- Packed-decimal nibble-pattern corruption in numeric fields.

It intentionally returns structured `copybook-core::Error` values and keeps all
predicate-level checks delegated to `copybook-corruption-predicates`.
