# copybook-governance

This microcrate is the compatibility façade that links runtime feature flags to
the COBOL support matrix.

## Purpose

- Keep runtime feature flag contracts and support-matrix status definitions
  interoperable via the `copybook-governance-contracts` façade.
- Re-export static support-to-flag bindings from `copybook-governance-grid`.
- Re-export runtime state evaluation from `copybook-governance-runtime`.
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
