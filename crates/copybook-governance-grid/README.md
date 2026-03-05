# copybook-governance-grid

Governance mapping between COBOL support-matrix entries and runtime feature flags.

## Overview

This crate owns the explicit mapping that links each documented COBOL feature (from the
support matrix) to the runtime feature flags that gate it. Every mapping row is declared
statically and total coverage is asserted by tests, ensuring no support-matrix entry is
left ungoverned.

## Usage

```rust
use copybook_governance_grid::{governance_bindings, summarize_governance};

// List all governance bindings
for binding in governance_bindings() {
    println!("{:?} -> {:?}", binding.support_id, binding.feature_flags);
}

// Get summary statistics
let summary = summarize_governance();
assert!(summary.total_support_features > 0);
```

## Public API

- `GovernedFeatureBinding` — Maps a support-matrix ID to required feature flags
- `GovernanceSummary` — Coverage statistics
- `governance_bindings()` — All static governance rows
- `feature_flags_for_support_id()` — Look up flags for a specific support entry
- `summarize_governance()` — Aggregate coverage summary

## License

AGPL-3.0-or-later
