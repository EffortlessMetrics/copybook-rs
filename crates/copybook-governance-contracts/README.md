# copybook-governance-contracts

Microcrate that provides a stable interoperability façade for:

- Runtime feature flags (`copybook-contracts`)
- COBOL support-matrix registry (`copybook-support-matrix`)

This crate intentionally keeps the API small and explicit so higher-level crates can
depend on one contract surface instead of reaching into each source crate
directly.

## Public API

- Re-exported `feature_flags` module
  - `Feature`, `FeatureCategory`, `FeatureFlags`, `FeatureFlagsBuilder`,
    `FeatureFlagsHandle`, `FeatureLifecycle`
- Re-exported `support_matrix` module
  - `FeatureId`, `FeatureSupport`, `SupportStatus`, `find_feature`,
    `find_feature_by_id`, `all_features`
