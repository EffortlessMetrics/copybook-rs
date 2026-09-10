# copybook-governance

This microcrate is the compatibility façade that links runtime feature flags to
the COBOL support matrix.

## Purpose

- Keep runtime feature flag contracts (`copybook-contracts`) and support-matrix
  status definitions (`copybook-support-matrix`) interoperable.
- Own static support-to-flag bindings (`bindings` module, collapsed here per
  #656 Phase F).
- Own runtime state evaluation (`runtime` module, collapsed here per #656
  Phase F).
- Act as the stable import surface for downstream crates (CLI, BDD, adapters).

## API Surface

- Re-exports:
  - `feature_flags`: `Feature`, `FeatureFlags`, `FeatureCategory`, etc.
  - `support_matrix`: `FeatureId`, `FeatureSupport`, `SupportStatus`, etc.
- Runtime helpers:
  - `support_states`
  - `governance_states`
  - `governance_state_for_support_id`
  - `runtime_summary`
