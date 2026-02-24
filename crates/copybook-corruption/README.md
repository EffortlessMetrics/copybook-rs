# copybook-corruption

Transfer-corruption detection façade crate.

This crate re-exports the focused detector microcrates used by higher-level codec
logic:

- `copybook-corruption-detectors` for packed-decimal and EBCDIC detector bodies.
- `copybook-corruption-rdw` for RDW ASCII corruption heuristics.

It keeps a compact public API while preserving existing callsites that depend on
`copybook-corruption` directly.
