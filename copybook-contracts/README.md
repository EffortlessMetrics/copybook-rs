# copybook-contracts

This crate stores shared contract types for copybook-rs feature governance:

- `Feature` enum and metadata
- Feature categories and lifecycle
- `FeatureFlags`, `FeatureFlagsBuilder`, and runtime handle APIs

## Public API

- `Feature`, `FeatureCategory`, `FeatureLifecycle`
- `FeatureFlags`, `FeatureFlagsBuilder`, `FeatureFlagsHandle`
- `FeatureFlags::default()`, `FeatureFlags::global()`, and `FeatureFlags::builder()`
- String/env interop via serde names and `to_string`/`FromStr`
