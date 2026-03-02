// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep tests for copybook-dialect: min_count behavior per dialect, parsing,
//! Display/Debug/Clone traits, serde roundtrips, and edge cases.
#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_dialect::{Dialect, effective_min_count};
use std::collections::HashMap;
use std::str::FromStr;

// ============================================================================
// 1. Normative dialect min_count behavior
// ============================================================================

#[test]
fn normative_preserves_zero() {
    assert_eq!(effective_min_count(Dialect::Normative, 0), 0);
}

#[test]
fn normative_preserves_one() {
    assert_eq!(effective_min_count(Dialect::Normative, 1), 1);
}

#[test]
fn normative_preserves_large_value() {
    assert_eq!(effective_min_count(Dialect::Normative, 9999), 9999);
}

#[test]
fn normative_preserves_u32_max() {
    assert_eq!(effective_min_count(Dialect::Normative, u32::MAX), u32::MAX);
}

// ============================================================================
// 2. ZeroTolerant dialect min_count behavior
// ============================================================================

#[test]
fn zero_tolerant_ignores_declared_zero() {
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 0), 0);
}

#[test]
fn zero_tolerant_ignores_declared_one() {
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 1), 0);
}

#[test]
fn zero_tolerant_ignores_declared_large() {
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 500), 0);
}

#[test]
fn zero_tolerant_ignores_u32_max() {
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, u32::MAX), 0);
}

// ============================================================================
// 3. OneTolerant dialect min_count behavior
// ============================================================================

#[test]
fn one_tolerant_clamps_zero_to_one() {
    assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
}

#[test]
fn one_tolerant_preserves_one() {
    assert_eq!(effective_min_count(Dialect::OneTolerant, 1), 1);
}

#[test]
fn one_tolerant_preserves_values_above_one() {
    assert_eq!(effective_min_count(Dialect::OneTolerant, 5), 5);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 100), 100);
}

#[test]
fn one_tolerant_preserves_u32_max() {
    assert_eq!(
        effective_min_count(Dialect::OneTolerant, u32::MAX),
        u32::MAX
    );
}

// ============================================================================
// 4. Dialect from string parsing
// ============================================================================

#[test]
fn from_str_normative_lowercase() {
    assert_eq!(Dialect::from_str("n").unwrap(), Dialect::Normative);
}

#[test]
fn from_str_normative_uppercase() {
    assert_eq!(Dialect::from_str("N").unwrap(), Dialect::Normative);
}

#[test]
fn from_str_zero_tolerant() {
    assert_eq!(Dialect::from_str("0").unwrap(), Dialect::ZeroTolerant);
}

#[test]
fn from_str_one_tolerant() {
    assert_eq!(Dialect::from_str("1").unwrap(), Dialect::OneTolerant);
}

#[test]
fn from_str_whitespace_trimmed() {
    assert_eq!(Dialect::from_str("  n  ").unwrap(), Dialect::Normative);
    assert_eq!(Dialect::from_str(" 0 ").unwrap(), Dialect::ZeroTolerant);
    assert_eq!(Dialect::from_str("\t1\t").unwrap(), Dialect::OneTolerant);
}

#[test]
fn from_str_invalid_values_produce_error() {
    for s in ["", "2", "x", "normative", "zero", "-1", "10", "nn"] {
        assert!(Dialect::from_str(s).is_err(), "should reject '{s}'");
    }
}

#[test]
fn from_str_error_message_contains_hint() {
    let err = Dialect::from_str("bad").unwrap_err();
    assert!(err.contains("normative"), "error should mention normative");
    assert!(
        err.contains("zero-tolerant"),
        "error should mention zero-tolerant"
    );
    assert!(
        err.contains("one-tolerant"),
        "error should mention one-tolerant"
    );
}

// ============================================================================
// 5. Default dialect behavior
// ============================================================================

#[test]
fn default_is_normative() {
    assert_eq!(Dialect::default(), Dialect::Normative);
}

#[test]
fn default_effective_min_count_passthrough() {
    let d = Dialect::default();
    for val in [0, 1, 5, 100, u32::MAX] {
        assert_eq!(effective_min_count(d, val), val);
    }
}

// ============================================================================
// 6. Display trait
// ============================================================================

#[test]
fn display_normative() {
    assert_eq!(format!("{}", Dialect::Normative), "n");
}

#[test]
fn display_zero_tolerant() {
    assert_eq!(format!("{}", Dialect::ZeroTolerant), "0");
}

#[test]
fn display_one_tolerant() {
    assert_eq!(format!("{}", Dialect::OneTolerant), "1");
}

#[test]
fn display_roundtrip_all_variants() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let s = dialect.to_string();
        let parsed = Dialect::from_str(&s).unwrap();
        assert_eq!(parsed, dialect, "roundtrip failed for {dialect:?}");
    }
}

// ============================================================================
// 7. Debug and Clone traits
// ============================================================================

#[test]
fn debug_contains_variant_name() {
    assert!(format!("{:?}", Dialect::Normative).contains("Normative"));
    assert!(format!("{:?}", Dialect::ZeroTolerant).contains("ZeroTolerant"));
    assert!(format!("{:?}", Dialect::OneTolerant).contains("OneTolerant"));
}

#[test]
fn clone_equals_original() {
    let variants = [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ];
    for d in variants {
        #[allow(clippy::clone_on_copy)]
        let cloned = d.clone();
        assert_eq!(d, cloned);
    }
}

#[test]
fn copy_semantics_preserved() {
    let original = Dialect::ZeroTolerant;
    let copied = original;
    // Original still usable after copy
    assert_eq!(original, copied);
    assert_eq!(effective_min_count(original, 5), 0);
}

// ============================================================================
// 8. Hash trait / HashMap usage
// ============================================================================

#[test]
fn dialect_usable_as_hashmap_key() {
    let mut map = HashMap::new();
    map.insert(Dialect::Normative, "strict");
    map.insert(Dialect::ZeroTolerant, "ibm");
    map.insert(Dialect::OneTolerant, "microfocus");
    assert_eq!(map.len(), 3);
    assert_eq!(map[&Dialect::Normative], "strict");
    assert_eq!(map[&Dialect::ZeroTolerant], "ibm");
    assert_eq!(map[&Dialect::OneTolerant], "microfocus");
}

// ============================================================================
// 9. Serde roundtrip
// ============================================================================

#[test]
fn serde_roundtrip_all_variants() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let json = serde_json::to_string(&dialect).unwrap();
        let back: Dialect = serde_json::from_str(&json).unwrap();
        assert_eq!(back, dialect, "serde roundtrip for {dialect:?}");
    }
}

#[test]
fn serde_json_string_representation() {
    let json = serde_json::to_string(&Dialect::Normative).unwrap();
    assert!(
        json.contains("Normative") || json.contains("normative"),
        "JSON: {json}"
    );
}

// ============================================================================
// 10. Effective min_count boundary cases across dialects
// ============================================================================

#[test]
fn effective_min_count_boundary_two_all_dialects() {
    assert_eq!(effective_min_count(Dialect::Normative, 2), 2);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 2), 0);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 2), 2);
}

#[test]
fn effective_min_count_consecutive_values() {
    for val in 0..=20 {
        let n = effective_min_count(Dialect::Normative, val);
        let z = effective_min_count(Dialect::ZeroTolerant, val);
        let o = effective_min_count(Dialect::OneTolerant, val);

        assert_eq!(n, val, "normative for {val}");
        assert_eq!(z, 0, "zero-tolerant for {val}");
        if val == 0 {
            assert_eq!(o, 1, "one-tolerant clamps 0→1");
        } else {
            assert_eq!(o, val, "one-tolerant for {val}");
        }
    }
}
