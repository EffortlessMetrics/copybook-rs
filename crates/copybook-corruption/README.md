# copybook-corruption

Transfer-corruption detection façade crate.

This crate re-exports the focused detector microcrates used by higher-level codec
logic:

- `copybook-corruption-detectors` for packed-decimal and EBCDIC detector bodies.
- `copybook_rdw::diagnostics` for RDW ASCII corruption heuristics.

The former `copybook-corruption-rdw` package remains a 0.5 compatibility
surface while primary consumers use the RDW owner directly.

It keeps a compact public API while preserving existing callsites that depend on
`copybook-corruption` directly.
