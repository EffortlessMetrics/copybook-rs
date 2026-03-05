# copybook-corruption-rdw

Small, single-purpose crate for RDW ASCII-transfer corruption heuristics.

This crate isolates the RDW header corruption decision logic from
`copybook-corruption` so it can be exercised independently by unit tests,
property tests, and fuzzing while keeping the existing API surface in
`copybook-corruption` stable.
