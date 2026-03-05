// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for schema fingerprint stability.
//!
//! Verifies that fingerprinting is deterministic: the same schema always
//! produces the same fingerprint, canonical JSON is stable across calls,
//! and different schemas produce different fingerprints.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_core::{Schema, parse_copybook};
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

// ============================================================================
// 1. Fingerprint idempotence: parse twice → identical fingerprint
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Parsing the same copybook twice always yields the same fingerprint.
    #[test]
    fn prop_fingerprint_idempotent(
        num_fields in 1usize..=8,
        field_len in 1u32..=30,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FP-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let s1 = parse_copybook(&copybook).expect("parse 1");
        let s2 = parse_copybook(&copybook).expect("parse 2");

        prop_assert!(
            !s1.fingerprint.is_empty(),
            "fingerprint must not be empty"
        );
        prop_assert_eq!(
            &s1.fingerprint, &s2.fingerprint,
            "same copybook must produce identical fingerprint"
        );
    }

    /// Canonical JSON is stable across multiple calls on the same schema.
    #[test]
    fn prop_fingerprint_canonical_json_repeated_stable(
        num_fields in 1usize..=6,
        field_len in 1u32..=20,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  RC-{i} PIC 9({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let cj1 = schema.create_canonical_json();
        let cj2 = schema.create_canonical_json();
        let cj3 = schema.create_canonical_json();

        prop_assert_eq!(&cj1, &cj2, "first canonical JSON call must be stable");
        prop_assert_eq!(&cj2, &cj3, "second canonical JSON call must be stable");
    }
}

// ============================================================================
// 2. Canonical JSON stability
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Canonical JSON representation is byte-identical across calls.
    #[test]
    fn prop_canonical_json_stable(
        num_fields in 1usize..=8,
        field_len in 1u32..=20,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  CJ-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json1 = schema.create_canonical_json();
        let json2 = schema.create_canonical_json();

        prop_assert_eq!(&json1, &json2, "canonical JSON must be byte-stable");
    }

    /// Canonical JSON of cloned schema matches original.
    #[test]
    fn prop_canonical_json_clone_stable(
        num_fields in 1usize..=6,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  CL-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let cloned = schema.clone();
        prop_assert_eq!(
            schema.create_canonical_json(),
            cloned.create_canonical_json(),
            "clone must produce identical canonical JSON"
        );
    }
}

// ============================================================================
// 3. Fingerprint is valid SHA-256 hex
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Fingerprint is always a 64-character lowercase hex string (SHA-256).
    #[test]
    fn prop_fingerprint_is_valid_sha256(
        num_fields in 1usize..=10,
        field_len in 1u32..=50,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  SH-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        prop_assert_eq!(schema.fingerprint.len(), 64, "SHA-256 must be 64 hex chars");
        prop_assert!(
            schema.fingerprint.chars().all(|c| c.is_ascii_hexdigit()),
            "fingerprint must be valid hex"
        );
    }
}

// ============================================================================
// 4. Different schemas → different fingerprints
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Adding a field changes the fingerprint.
    #[test]
    fn prop_fingerprint_changes_with_extra_field(
        base_fields in 1usize..=5,
        field_len in 1u32..=20,
    ) {
        let base = (0..base_fields)
            .map(|i| format!("       05  DF-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let extended = format!(
            "{base}\n       05  DF-EXTRA PIC X({field_len})."
        );

        let s1 = parse_copybook(&base).expect("parse base");
        let s2 = parse_copybook(&extended).expect("parse extended");

        prop_assert_ne!(
            &s1.fingerprint, &s2.fingerprint,
            "adding a field must change fingerprint"
        );
    }

    /// Changing field length changes the fingerprint.
    #[test]
    fn prop_fingerprint_changes_with_different_length(
        len_a in 1u32..=25,
        len_b in 26u32..=50,
    ) {
        let cb_a = format!("       05  LEN-FLD PIC X({len_a}).");
        let cb_b = format!("       05  LEN-FLD PIC X({len_b}).");

        let s_a = parse_copybook(&cb_a).expect("parse a");
        let s_b = parse_copybook(&cb_b).expect("parse b");

        prop_assert_ne!(
            &s_a.fingerprint, &s_b.fingerprint,
            "different field lengths must produce different fingerprints"
        );
    }
}

// ============================================================================
// 5. Serde roundtrip preserves fingerprint
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Serializing → deserializing a schema preserves the fingerprint string.
    #[test]
    fn prop_fingerprint_survives_serde_roundtrip(
        num_fields in 1usize..=6,
        field_len in 1u32..=15,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  SR-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let restored: Schema = serde_json::from_str(&json).expect("deserialize");

        prop_assert_eq!(
            &schema.fingerprint, &restored.fingerprint,
            "serde roundtrip must preserve fingerprint"
        );
    }
}

// ============================================================================
// 6. Grouped schema fingerprint stability
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Grouped (01→05→10) schemas produce stable fingerprints.
    #[test]
    fn prop_grouped_schema_fingerprint_stable(
        num_children in 1usize..=5,
        child_len in 1u32..=10,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  GC-{i} PIC X({child_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  GRP.\n{children}");

        let s1 = parse_copybook(&copybook).expect("parse 1");
        let s2 = parse_copybook(&copybook).expect("parse 2");

        prop_assert_eq!(
            &s1.fingerprint, &s2.fingerprint,
            "grouped schema must produce stable fingerprint"
        );
    }
}
