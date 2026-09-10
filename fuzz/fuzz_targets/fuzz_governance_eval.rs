#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_governance::{
    FeatureFlags, feature_flags::all_features, governance_states, is_support_runtime_available,
    runtime_summary, support_states,
};
use libfuzzer_sys::fuzz_target;

/// Fuzz target for governance runtime evaluation.
///
/// Derives feature flag combinations from fuzzed bytes and exercises
/// governance state queries, summary computation, and availability checks.
fuzz_target!(|data: &[u8]| {
    // Enumerate the live flag set so flag collapses (#656) cannot desync
    // this target from the contracts crate.
    let all_features = all_features();

    // Build feature flags from fuzz data bits
    let mut builder = FeatureFlags::builder();
    for (i, feature) in all_features.iter().enumerate() {
        let byte_idx = i / 8;
        let bit_idx = i % 8;
        let enabled = data
            .get(byte_idx)
            .map_or(false, |b| (b >> bit_idx) & 1 == 1);
        if enabled {
            builder = builder.enable(*feature);
        }
    }
    let flags = builder.build();

    // Exercise governance queries
    let states = governance_states(&flags);
    let _ = runtime_summary(&flags);
    let _ = support_states();

    // Check availability for every discovered support_id
    for state in &states {
        let _ = is_support_runtime_available(state.support_id, &flags);
    }
});
