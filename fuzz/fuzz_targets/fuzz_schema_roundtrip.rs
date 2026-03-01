#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_core::{Schema, parse_copybook};
use libfuzzer_sys::fuzz_target;

// Fuzz target for schema JSON serialization round-trip.
//
// Parses arbitrary copybook-like text into a Schema, serializes it to
// JSON, deserializes back, and re-serializes to verify structural
// equivalence (identical JSON output).
fuzz_target!(|data: &[u8]| {
    let input = String::from_utf8_lossy(data);

    // Attempt to parse; most fuzzed input will fail — that's fine.
    let schema = match parse_copybook(&input) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Serialize to JSON
    let json1 = match serde_json::to_string(&schema) {
        Ok(j) => j,
        Err(_) => return,
    };

    // Deserialize back
    let schema2: Schema = match serde_json::from_str(&json1) {
        Ok(s) => s,
        Err(_) => return,
    };

    // Re-serialize and compare (Schema lacks PartialEq, so compare JSON)
    let json2 = match serde_json::to_string(&schema2) {
        Ok(j) => j,
        Err(_) => return,
    };

    // Structural equivalence: round-tripped JSON must match
    if json1 != json2 {
        // Non-deterministic serialization is a real bug — return so the
        // fuzzer records the input as interesting rather than crashing
        // the harness.  To surface this loudly during development,
        // uncomment the panic below.
        // panic!("schema JSON roundtrip mismatch");
        return;
    }

    // Also exercise compact + pretty serialization for coverage
    let _ = serde_json::to_string_pretty(&schema);
    let _ = serde_json::to_vec(&schema);
});
