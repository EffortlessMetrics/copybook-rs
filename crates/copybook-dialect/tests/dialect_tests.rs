// SPDX-License-Identifier: AGPL-3.0-or-later
//! Integration tests for the `copybook-dialect` crate.

use std::collections::HashMap;
use std::str::FromStr;

use copybook_dialect::{Dialect, effective_min_count};

// ---------------------------------------------------------------------------
// 1. Dialect enum: creation and comparison
// ---------------------------------------------------------------------------

#[test]
fn dialect_variants_are_distinct() {
    let variants = [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ];
    for (i, a) in variants.iter().enumerate() {
        for (j, b) in variants.iter().enumerate() {
            if i == j {
                assert_eq!(a, b);
            } else {
                assert_ne!(a, b);
            }
        }
    }
}

#[test]
fn dialect_copy_semantics() {
    let original = Dialect::OneTolerant;
    let copied = original;
    assert_eq!(original, copied);
}

#[test]
fn dialect_clone_equals_original() {
    let original = Dialect::ZeroTolerant;
    #[allow(clippy::clone_on_copy)]
    let cloned = original.clone();
    assert_eq!(original, cloned);
}

// ---------------------------------------------------------------------------
// 2. Default
// ---------------------------------------------------------------------------

#[test]
fn default_dialect_is_normative() {
    assert_eq!(Dialect::default(), Dialect::Normative);
}

// ---------------------------------------------------------------------------
// 3. FromStr parsing
// ---------------------------------------------------------------------------

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
fn from_str_trims_whitespace() {
    assert_eq!(Dialect::from_str("  n  ").unwrap(), Dialect::Normative);
    assert_eq!(Dialect::from_str("\t0\t").unwrap(), Dialect::ZeroTolerant);
    assert_eq!(Dialect::from_str(" 1\n").unwrap(), Dialect::OneTolerant);
}

#[test]
fn from_str_rejects_invalid_values() {
    let invalid = ["", "x", "2", "-1", "normative", "zero", "one", "nn", "00"];
    for s in invalid {
        let result = Dialect::from_str(s);
        assert!(result.is_err(), "Expected error for input {s:?}");
    }
}

#[test]
fn from_str_error_contains_helpful_message() {
    let err = Dialect::from_str("bad").unwrap_err();
    assert!(err.contains("Invalid dialect"), "Error: {err}");
    assert!(err.contains("'bad'"), "Error should quote input: {err}");
    assert!(
        err.contains("normative"),
        "Error should list valid options: {err}"
    );
    assert!(
        err.contains("zero-tolerant"),
        "Error should list valid options: {err}"
    );
    assert!(
        err.contains("one-tolerant"),
        "Error should list valid options: {err}"
    );
}

// ---------------------------------------------------------------------------
// 4. Display
// ---------------------------------------------------------------------------

#[test]
fn display_roundtrips_through_from_str() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let displayed = dialect.to_string();
        let parsed = Dialect::from_str(&displayed).unwrap();
        assert_eq!(dialect, parsed, "Roundtrip failed for {dialect:?}");
    }
}

#[test]
fn display_produces_expected_strings() {
    assert_eq!(Dialect::Normative.to_string(), "n");
    assert_eq!(Dialect::ZeroTolerant.to_string(), "0");
    assert_eq!(Dialect::OneTolerant.to_string(), "1");
}

// ---------------------------------------------------------------------------
// 5. Debug
// ---------------------------------------------------------------------------

#[test]
fn debug_contains_variant_name() {
    assert_eq!(format!("{:?}", Dialect::Normative), "Normative");
    assert_eq!(format!("{:?}", Dialect::ZeroTolerant), "ZeroTolerant");
    assert_eq!(format!("{:?}", Dialect::OneTolerant), "OneTolerant");
}

// ---------------------------------------------------------------------------
// 6. Hash: usable as map keys
// ---------------------------------------------------------------------------

#[test]
fn dialect_usable_as_hash_map_key() {
    let mut map = HashMap::new();
    map.insert(Dialect::Normative, "strict");
    map.insert(Dialect::ZeroTolerant, "ibm");
    map.insert(Dialect::OneTolerant, "microfocus");

    assert_eq!(map.len(), 3);
    assert_eq!(map[&Dialect::Normative], "strict");
    assert_eq!(map[&Dialect::ZeroTolerant], "ibm");
    assert_eq!(map[&Dialect::OneTolerant], "microfocus");

    // Duplicate key overwrites
    map.insert(Dialect::Normative, "updated");
    assert_eq!(map.len(), 3);
    assert_eq!(map[&Dialect::Normative], "updated");
}

// ---------------------------------------------------------------------------
// 7. effective_min_count – Normative
// ---------------------------------------------------------------------------

#[test]
fn normative_passes_through_min_count_unchanged() {
    let test_values: &[u32] = &[0, 1, 2, 5, 10, 100, 999, u32::MAX];
    for &v in test_values {
        assert_eq!(
            effective_min_count(Dialect::Normative, v),
            v,
            "Normative should return {v} unchanged"
        );
    }
}

// ---------------------------------------------------------------------------
// 8. effective_min_count – ZeroTolerant
// ---------------------------------------------------------------------------

#[test]
fn zero_tolerant_always_returns_zero() {
    let test_values: &[u32] = &[0, 1, 2, 5, 10, 100, 999, u32::MAX];
    for &v in test_values {
        assert_eq!(
            effective_min_count(Dialect::ZeroTolerant, v),
            0,
            "ZeroTolerant should return 0 for input {v}"
        );
    }
}

// ---------------------------------------------------------------------------
// 9. effective_min_count – OneTolerant
// ---------------------------------------------------------------------------

#[test]
fn one_tolerant_clamps_zero_to_one() {
    assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
}

#[test]
fn one_tolerant_preserves_values_gte_one() {
    let test_values: &[u32] = &[1, 2, 5, 10, 100, 999, u32::MAX];
    for &v in test_values {
        assert_eq!(
            effective_min_count(Dialect::OneTolerant, v),
            v,
            "OneTolerant should preserve {v} (≥1)"
        );
    }
}

// ---------------------------------------------------------------------------
// 10. effective_min_count – cross-dialect edge cases
// ---------------------------------------------------------------------------

#[test]
fn edge_case_min_count_zero_all_dialects() {
    assert_eq!(effective_min_count(Dialect::Normative, 0), 0);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 0), 0);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
}

#[test]
fn edge_case_min_count_one_all_dialects() {
    assert_eq!(effective_min_count(Dialect::Normative, 1), 1);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 1), 0);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 1), 1);
}

#[test]
fn edge_case_min_count_large_all_dialects() {
    let large = 1_000_000;
    assert_eq!(effective_min_count(Dialect::Normative, large), large);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, large), 0);
    assert_eq!(effective_min_count(Dialect::OneTolerant, large), large);
}

#[test]
fn edge_case_min_count_u32_max_all_dialects() {
    assert_eq!(effective_min_count(Dialect::Normative, u32::MAX), u32::MAX);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, u32::MAX), 0);
    assert_eq!(
        effective_min_count(Dialect::OneTolerant, u32::MAX),
        u32::MAX
    );
}

// ---------------------------------------------------------------------------
// 11. Serde: JSON serialization/deserialization
// ---------------------------------------------------------------------------

#[test]
fn serde_json_roundtrip_all_variants() {
    for dialect in [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ] {
        let json = serde_json::to_string(&dialect).unwrap();
        let deserialized: Dialect = serde_json::from_str(&json).unwrap();
        assert_eq!(
            dialect, deserialized,
            "Serde roundtrip failed for {dialect:?}"
        );
    }
}

#[test]
fn serde_json_serialized_forms() {
    // Verify the exact serialized JSON representation
    let normative_json = serde_json::to_string(&Dialect::Normative).unwrap();
    let zero_json = serde_json::to_string(&Dialect::ZeroTolerant).unwrap();
    let one_json = serde_json::to_string(&Dialect::OneTolerant).unwrap();

    assert_eq!(normative_json, "\"Normative\"");
    assert_eq!(zero_json, "\"ZeroTolerant\"");
    assert_eq!(one_json, "\"OneTolerant\"");
}

#[test]
fn serde_json_deserialize_from_known_strings() {
    let normative: Dialect = serde_json::from_str("\"Normative\"").unwrap();
    let zero: Dialect = serde_json::from_str("\"ZeroTolerant\"").unwrap();
    let one: Dialect = serde_json::from_str("\"OneTolerant\"").unwrap();

    assert_eq!(normative, Dialect::Normative);
    assert_eq!(zero, Dialect::ZeroTolerant);
    assert_eq!(one, Dialect::OneTolerant);
}

#[test]
fn serde_json_rejects_invalid_variant() {
    let result = serde_json::from_str::<Dialect>("\"Invalid\"");
    assert!(result.is_err());
}

#[test]
fn serde_json_in_struct_context() {
    #[derive(serde::Serialize, serde::Deserialize, Debug, PartialEq)]
    struct Config {
        dialect: Dialect,
        name: String,
    }

    let config = Config {
        dialect: Dialect::ZeroTolerant,
        name: "ibm_enterprise".into(),
    };

    let json = serde_json::to_string(&config).unwrap();
    let deserialized: Config = serde_json::from_str(&json).unwrap();
    assert_eq!(config, deserialized);
}

// ---------------------------------------------------------------------------
// 12. Environment variable parsing (simulated via FromStr)
// ---------------------------------------------------------------------------

#[test]
fn env_var_style_parsing() {
    // Simulate what happens when COPYBOOK_DIALECT env var values are parsed.
    // The CLI reads the env var and calls FromStr, so we test those exact values.
    let env_scenarios = [
        ("n", Dialect::Normative),
        ("N", Dialect::Normative),
        ("0", Dialect::ZeroTolerant),
        ("1", Dialect::OneTolerant),
    ];

    for (input, expected) in env_scenarios {
        let parsed = Dialect::from_str(input).unwrap();
        assert_eq!(
            parsed, expected,
            "COPYBOOK_DIALECT={input:?} should parse to {expected:?}"
        );
    }
}

// ---------------------------------------------------------------------------
// 13. Exhaustive dialect × min_count matrix
// ---------------------------------------------------------------------------

#[test]
fn exhaustive_dialect_min_count_matrix() {
    // A compact table-driven test covering all dialect × min_count combinations
    #[rustfmt::skip]
    let cases: &[(Dialect, u32, u32)] = &[
        // (dialect,         declared, expected)
        (Dialect::Normative,     0,   0),
        (Dialect::Normative,     1,   1),
        (Dialect::Normative,     5,   5),
        (Dialect::ZeroTolerant,  0,   0),
        (Dialect::ZeroTolerant,  1,   0),
        (Dialect::ZeroTolerant,  5,   0),
        (Dialect::OneTolerant,   0,   1),
        (Dialect::OneTolerant,   1,   1),
        (Dialect::OneTolerant,   5,   5),
    ];

    for &(dialect, declared, expected) in cases {
        let actual = effective_min_count(dialect, declared);
        assert_eq!(
            actual, expected,
            "effective_min_count({dialect:?}, {declared}) = {actual}, expected {expected}"
        );
    }
}
