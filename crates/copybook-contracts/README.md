# copybook-contracts

Shared contract types for copybook-rs feature-flag governance.

## Overview

Defines the `Feature` enum and its metadata (categories, lifecycle stages), along with the
`FeatureFlags` runtime bit-set and its builder/handle APIs. This crate is the single source
of truth for feature-flag definitions used across all governance and runtime crates.

## Usage

```rust
use copybook_contracts::{Feature, FeatureFlags, FeatureFlagsBuilder, FeatureCategory};

let flags = FeatureFlags::builder()
    .enable(Feature::Comp1)
    .disable(Feature::AuditSystem)
    .build();

assert!(flags.is_enabled(Feature::Comp1));
assert_eq!(Feature::Comp1.category(), FeatureCategory::Experimental);
```

## Public API

- `Feature` — Enum of all governance-controlled features
- `FeatureCategory` / `FeatureLifecycle` — Classification and maturity metadata
- `FeatureFlags` — Runtime bit-set with `is_enabled` / `enable` / `disable`
- `FeatureFlagsBuilder` — Fluent builder for constructing flag sets
- `FeatureFlagsHandle` — Thread-safe handle for dynamic flag toggling

## License

AGPL-3.0-or-later
