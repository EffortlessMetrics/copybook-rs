# copybook-governance-runtime

Focused microcrate for runtime governance state:

- Resolves support-matrix entries against active feature flags
- Produces interoperable governance state rows and summaries
- Reuses canonical governance-grid + contracts crates

This crate keeps runtime evaluation logic SRP-scoped, while `copybook-governance`
continues as the compatibility façade for consumers.
