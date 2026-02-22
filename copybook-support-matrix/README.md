# copybook-support-matrix

This crate stores the shared COBOL support-matrix contract.

- Stable support feature IDs
- Per-feature support status
- Structured `FeatureSupport` entries

## Public API

- `FeatureId` (`#[serde]`-stable identifiers)
- `SupportStatus` (`Supported`, `Partial`, `Planned`, `NotPlanned`)
- `FeatureSupport` (name, description, docs, status)
- `all_features() -> &'static [FeatureSupport]`
- `find_feature(id: &str) -> Option<&'static FeatureSupport>`
- `find_feature_by_id(id: FeatureId) -> Option<&'static FeatureSupport>`
