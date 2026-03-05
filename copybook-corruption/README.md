# copybook-corruption

Transfer corruption detection heuristics for COBOL record pipelines.

This microcrate centralizes single-responsibility checks for likely transfer corruption:

- RDW ASCII header corruption heuristics
- EBCDIC suspicious-byte detection
- Packed decimal nibble corruption checks

`copybook-codec` re-exports this crate through its legacy `corruption` module for backward compatibility.
