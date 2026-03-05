# Fidelity & Determinism

## Core Invariants
- decode(binary)->encode(json)->binary must be byte-identical
- Same inputs => identical outputs across runs and thread configurations
- REDEFINES encode requires removing alternate views from JSON to avoid CBKE501

## Known Normalizations
- FILLER fields named by byte offset (`_filler_00000XXX`)
- EBCDIC trailing spaces preserved in PIC X decode (not trimmed by default)

## Edge Cases Under Test
- COMP-3 sign nibble normalization (C/D/F all valid positive; D negative)
- Overpunch sign encoding round-trip
- Edited PIC decimal point vs COBOL period statement terminator

## Test Links
- tests/e2e/e2e_determinism.rs — basic determinism validation
- tests/e2e/e2e_determinism_extended.rs — multi-schema, cross-codepage determinism
- tests/proptest/prop_codec_roundtrip.rs — property-based round-trip
