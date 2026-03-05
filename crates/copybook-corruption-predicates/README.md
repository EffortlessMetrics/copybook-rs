# copybook-corruption-predicates

Single-purpose, allocation-free predicates used by the corruption detection
microcrate to classify transfer-corruption patterns.

This crate intentionally contains **no `copybook-core` dependency** and returns only
boolean signals. Callers are responsible for translating predicate findings into
error objects and contextual diagnostics.
