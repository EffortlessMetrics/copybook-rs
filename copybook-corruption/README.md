# copybook-corruption

Heuristic corruption detection primitives for COBOL transfer pipelines.

This crate contains small, focused detectors used by higher-level codec crates:
- RDW ASCII transfer corruption suspicion
- EBCDIC field-byte corruption heuristics
- Packed decimal nibble corruption checks
