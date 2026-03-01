# copybook-governance-contracts

Stable interoperability façade for feature flags and the COBOL support matrix.

## Overview

Unifies `copybook-contracts` (feature flags) and `copybook-support-matrix` (COBOL feature
registry) behind a single, small dependency surface. Higher-level governance crates depend
on this façade instead of reaching into each source crate directly, keeping the dependency
graph clean and the contract surface explicit.

## Usage

```rust
use copybook_governance_contracts::{
    Feature, FeatureFlags, FeatureFlagsBuilder,
    FeatureId, SupportStatus, find_feature_by_id,
};

// Feature flags
let flags = FeatureFlags::builder().enable(Feature::Comp1).build();
assert!(flags.is_enabled(Feature::Comp1));

// Support matrix lookup
let entry = find_feature_by_id(FeatureId::EditedPic).unwrap();
assert_eq!(entry.status, SupportStatus::Supported);
```

## Public API

- **`feature_flags`** module — `Feature`, `FeatureCategory`, `FeatureFlags`, `FeatureFlagsBuilder`, `FeatureFlagsHandle`, `FeatureLifecycle`
- **`support_matrix`** module — `FeatureId`, `FeatureSupport`, `SupportStatus`, `find_feature`, `find_feature_by_id`, `all_features`

## License

AGPL-3.0-or-later
