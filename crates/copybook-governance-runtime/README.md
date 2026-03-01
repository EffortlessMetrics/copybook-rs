# copybook-governance-runtime

Runtime governance evaluation for copybook-rs feature controls.

## Overview

Turns static governance mappings into runtime state by evaluating active feature flags against
support-matrix rows. Produces `FeatureGovernanceState` rows that indicate whether each COBOL
feature is currently enabled, and aggregates availability into a `FeatureGovernanceSummary`.

## Usage

```rust
use copybook_governance_runtime::{
    governance_states, runtime_summary, is_support_runtime_available,
    FeatureFlags, FeatureId,
};

let flags = FeatureFlags::default();

// Check if a specific feature is runtime-available
let available = is_support_runtime_available(FeatureId::SignSeparate, &flags);

// Get full runtime summary
let summary = runtime_summary(&flags);
println!("Enabled: {}, Disabled: {}",
    summary.runtime_enabled_features,
    summary.runtime_disabled_features);
```

## Public API

- `FeatureGovernanceState` — Runtime state for a single support-matrix feature
- `FeatureGovernanceSummary` — Aggregate runtime availability statistics
- `governance_states()` — All governance rows evaluated against active flags
- `is_support_runtime_available()` — Single-feature runtime check
- `runtime_summary()` — Aggregate summary with enabled/disabled counts

## License

AGPL-3.0-or-later
