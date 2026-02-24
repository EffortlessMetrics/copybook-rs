# copybook-corruption

Small, focused crate for transfer-corruption heuristics used by copybook decoding.

This crate intentionally owns only lightweight heuristics for detecting common
binary-to-text corruption patterns so higher-level codecs can decide how to
report and handle violations.
