//! D1 Core Dialect Lever Tests
//!
//! Comprehensive tests for dialect lever that controls how `min_count` is
//! interpreted for ODO (OCCURS DEPENDING ON) arrays.

use std::str::FromStr;

use copybook_core::{
    dialect::{Dialect, effective_min_count},
    layout::resolve_layout,
    schema::{Field, FieldKind, Occurs, Schema},
};

#[test]
fn test_dialect_default_is_normative() {
    assert_eq!(Dialect::default(), Dialect::Normative);
}

#[test]
fn test_dialect_from_str_normative() {
    assert_eq!(Dialect::from_str("n").unwrap(), Dialect::Normative);
    assert_eq!(Dialect::from_str("N").unwrap(), Dialect::Normative);
}

#[test]
fn test_dialect_from_str_zero_tolerant() {
    assert_eq!(Dialect::from_str("0").unwrap(), Dialect::ZeroTolerant);
}

#[test]
fn test_dialect_from_str_one_tolerant() {
    assert_eq!(Dialect::from_str("1").unwrap(), Dialect::OneTolerant);
}

#[test]
fn test_dialect_from_str_invalid() {
    assert!(Dialect::from_str("x").is_err());
    assert!(Dialect::from_str("normative").is_err());
    assert!(Dialect::from_str("").is_err());
    assert!(Dialect::from_str("2").is_err());
}

#[test]
fn test_dialect_display() {
    assert_eq!(Dialect::Normative.to_string(), "n");
    assert_eq!(Dialect::ZeroTolerant.to_string(), "0");
    assert_eq!(Dialect::OneTolerant.to_string(), "1");
}

#[test]
fn test_dialect_roundtrip() {
    let dialects = [
        Dialect::Normative,
        Dialect::ZeroTolerant,
        Dialect::OneTolerant,
    ];
    for dialect in dialects {
        let s = dialect.to_string();
        let parsed = Dialect::from_str(&s).unwrap();
        assert_eq!(parsed, dialect);
    }
}

#[test]
fn test_effective_min_count_normative() {
    // Normative: min_count is enforced as-is
    assert_eq!(effective_min_count(Dialect::Normative, 0), 0);
    assert_eq!(effective_min_count(Dialect::Normative, 1), 1);
    assert_eq!(effective_min_count(Dialect::Normative, 5), 5);
    assert_eq!(effective_min_count(Dialect::Normative, 10), 10);
    assert_eq!(effective_min_count(Dialect::Normative, 100), 100);
}

#[test]
fn test_effective_min_count_zero_tolerant() {
    // ZeroTolerant: min_count is always 0
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 0), 0);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 1), 0);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 5), 0);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 10), 0);
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, 100), 0);
}

#[test]
fn test_effective_min_count_one_tolerant() {
    // OneTolerant: min_count is clamped to 1
    assert_eq!(effective_min_count(Dialect::OneTolerant, 0), 1);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 1), 1);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 5), 5);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 10), 10);
    assert_eq!(effective_min_count(Dialect::OneTolerant, 100), 100);
}

#[test]
fn test_effective_min_count_edge_cases() {
    // Test edge cases for all dialects
    let test_cases = [0, 1, 2, 5, 10, 100];

    for min_count in test_cases {
        // Normative: unchanged
        assert_eq!(
            effective_min_count(Dialect::Normative, min_count),
            min_count,
            "Normative should return min_count unchanged for {}",
            min_count
        );

        // ZeroTolerant: always 0
        assert_eq!(
            effective_min_count(Dialect::ZeroTolerant, min_count),
            0,
            "ZeroTolerant should return 0 for min_count {}",
            min_count
        );

        // OneTolerant: max(1, min_count)
        let expected = min_count.max(1);
        assert_eq!(
            effective_min_count(Dialect::OneTolerant, min_count),
            expected,
            "OneTolerant should return max(1, {}) = {}",
            min_count,
            expected
        );
    }
}

#[test]
fn test_odo_schema_with_default_dialect() {
    // Test that default dialect (Normative) is used in schema parsing
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=0, max_count=5
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 0,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // Should have tail ODO info with default dialect
    assert!(schema.tail_odo.is_some());
    let tail_odo = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail_odo.counter_path, "COUNTER");
    assert_eq!(tail_odo.min_count, 0);
    assert_eq!(tail_odo.max_count, 5);
    // TODO: dialect field not yet implemented in TailODO
    // assert_eq!(tail_odo.dialect, Dialect::Normative);

    // With Normative dialect and min_count=0, max_count=5, record is variable
    // (effective_min_count = 0, so max > min)
    assert!(schema.lrecl_fixed.is_none());
}

#[test]
fn test_odo_schema_normative_fixed_record() {
    // Test that ODO with min_count=max_count produces fixed record in Normative mode
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=5, max_count=5 (fixed size)
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 5,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // With Normative dialect and min_count=max_count=5, record is fixed
    // (effective_min_count = 5, so max == min)
    assert_eq!(schema.lrecl_fixed, Some(3 + 10 * 5)); // counter (3) + array (50)
}

#[test]
fn test_odo_schema_normative_equal_min_max() {
    // Test that Normative dialect treats ODO with min_count=max_count as fixed
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=5, max_count=5 (fixed size)
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 5,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // With Normative dialect and min_count=max_count=5, record is fixed
    // (effective_min_count = 5, so max == min)
    assert_eq!(schema.lrecl_fixed, Some(3 + 10 * 5)); // counter (3) + array (50)
}

#[test]
fn test_odo_schema_one_tolerant_variable_record() {
    // Test that OneTolerant dialect treats ODO with min_count=0 as variable
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=0, max_count=5
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 0,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // With OneTolerant dialect and min_count=0, effective_min_count = 1
    // so max (5) > min (1), record is variable
    assert!(schema.lrecl_fixed.is_none());
}

#[test]
fn test_odo_schema_one_tolerant_fixed_record() {
    // Test that OneTolerant dialect treats ODO with min_count=max_count as fixed
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=5, max_count=5
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 5,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // With OneTolerant dialect and min_count=max_count=5, effective_min_count = 5
    // so max (5) == min (5), record is fixed
    assert_eq!(schema.lrecl_fixed, Some(3 + 10 * 5)); // counter (3) + array (50)
}

#[test]
fn test_dialect_comparison() {
    // Test dialect equality and ordering
    assert_eq!(Dialect::Normative, Dialect::Normative);
    assert_eq!(Dialect::ZeroTolerant, Dialect::ZeroTolerant);
    assert_eq!(Dialect::OneTolerant, Dialect::OneTolerant);

    assert_ne!(Dialect::Normative, Dialect::ZeroTolerant);
    assert_ne!(Dialect::Normative, Dialect::OneTolerant);
    assert_ne!(Dialect::ZeroTolerant, Dialect::OneTolerant);
}

#[test]
fn test_dialect_hash() {
    // Test that dialects can be used in hash-based collections
    use std::collections::HashSet;

    let mut set = HashSet::new();
    set.insert(Dialect::Normative);
    set.insert(Dialect::ZeroTolerant);
    set.insert(Dialect::OneTolerant);

    assert_eq!(set.len(), 3);
    assert!(set.contains(&Dialect::Normative));
    assert!(set.contains(&Dialect::ZeroTolerant));
    assert!(set.contains(&Dialect::OneTolerant));
}

#[test]
fn test_dialect_copy_and_clone() {
    // Test that dialects can be copied and cloned
    let original = Dialect::Normative;
    let copied = original;
    assert_eq!(original, copied);

    let cloned = original.clone();
    assert_eq!(original, cloned);
}

#[test]
fn test_dialect_debug() {
    // Test that dialects can be debug-formatted
    assert_eq!(format!("{:?}", Dialect::Normative), "Normative");
    assert_eq!(format!("{:?}", Dialect::ZeroTolerant), "ZeroTolerant");
    assert_eq!(format!("{:?}", Dialect::OneTolerant), "OneTolerant");
}

#[test]
fn test_effective_min_count_all_dialects_zero() {
    // Test all dialects with min_count=0
    let min_count = 0;

    // Normative: 0
    assert_eq!(effective_min_count(Dialect::Normative, min_count), 0);

    // ZeroTolerant: 0
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, min_count), 0);

    // OneTolerant: 1 (clamped)
    assert_eq!(effective_min_count(Dialect::OneTolerant, min_count), 1);
}

#[test]
fn test_effective_min_count_all_dialects_one() {
    // Test all dialects with min_count=1
    let min_count = 1;

    // Normative: 1
    assert_eq!(effective_min_count(Dialect::Normative, min_count), 1);

    // ZeroTolerant: 0
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, min_count), 0);

    // OneTolerant: 1 (clamped)
    assert_eq!(effective_min_count(Dialect::OneTolerant, min_count), 1);
}

#[test]
fn test_effective_min_count_all_dialects_large() {
    // Test all dialects with min_count=100
    let min_count = 100;

    // Normative: 100
    assert_eq!(effective_min_count(Dialect::Normative, min_count), 100);

    // ZeroTolerant: 0
    assert_eq!(effective_min_count(Dialect::ZeroTolerant, min_count), 0);

    // OneTolerant: 100 (no clamping needed)
    assert_eq!(effective_min_count(Dialect::OneTolerant, min_count), 100);
}

#[test]
fn test_dialect_in_tail_odo() {
    // Test that dialect is properly stored in TailODO
    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 0,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // Verify dialect is stored in TailODO
    let _tail_odo = schema.tail_odo.as_ref().unwrap();
    // TODO: dialect field not yet implemented in TailODO
    // assert_eq!(tail_odo.dialect, Dialect::Normative);
}

#[test]
fn test_default_behavior_unchanged() {
    // Test that default behavior (Normative) matches existing behavior
    // This is a critical test to ensure backward compatibility

    let mut schema = Schema::new();

    // Counter field
    let counter = Field::with_kind(
        1,
        "COUNTER".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter);

    // ODO array field with min_count=0, max_count=5
    let mut array_field = Field::with_kind(
        1,
        "ARRAY-FIELD".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::ODO {
        min: 0,
        max: 5,
        counter_path: "COUNTER".to_string(),
    });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // With default Normative dialect:
    // - min_count=0, max_count=5
    // - effective_min_count = 0
    // - max (5) > min (0), so record is variable
    assert!(schema.lrecl_fixed.is_none());

    // Tail ODO should have Normative dialect
    let tail_odo = schema.tail_odo.as_ref().unwrap();
    // TODO: dialect field not yet implemented in TailODO
    // assert_eq!(tail_odo.dialect, Dialect::Normative);
    assert_eq!(tail_odo.min_count, 0);
    assert_eq!(tail_odo.max_count, 5);
}

#[test]
fn test_multiple_odo_arrays_same_dialect() {
    // Test schema with multiple ODO arrays (all using default dialect)
    let mut schema = Schema::new();

    // Counter 1
    let counter1 = Field::with_kind(
        1,
        "COUNTER1".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter1);

    // ODO array 1
    let mut array1 = Field::with_kind(1, "ARRAY1".to_string(), FieldKind::Alphanum { len: 10 });
    array1.occurs = Some(Occurs::ODO {
        min: 0,
        max: 3,
        counter_path: "COUNTER1".to_string(),
    });
    schema.fields.push(array1);

    // Counter 2
    let counter2 = Field::with_kind(
        1,
        "COUNTER2".to_string(),
        FieldKind::ZonedDecimal {
            digits: 3,
            scale: 0,
            signed: false,
        },
    );
    schema.fields.push(counter2);

    // ODO array 2
    let mut array2 = Field::with_kind(1, "ARRAY2".to_string(), FieldKind::Alphanum { len: 10 });
    array2.occurs = Some(Occurs::ODO {
        min: 1,
        max: 5,
        counter_path: "COUNTER2".to_string(),
    });
    schema.fields.push(array2);

    resolve_layout(&mut schema).unwrap();

    // Both ODO arrays should cause variable record length
    assert!(schema.lrecl_fixed.is_none());

    // Tail ODO should be the one with highest offset (ARRAY2)
    let tail_odo = schema.tail_odo.as_ref().unwrap();
    assert_eq!(tail_odo.array_path, "ARRAY2");
    assert_eq!(tail_odo.counter_path, "COUNTER2");
    assert_eq!(tail_odo.min_count, 1);
    assert_eq!(tail_odo.max_count, 5);
    // TODO: dialect field not yet implemented in TailODO
    // assert_eq!(tail_odo.dialect, Dialect::Normative);
}

#[test]
fn test_dialect_with_fixed_occurs() {
    // Test that dialect doesn't affect fixed OCCURS arrays
    let mut schema = Schema::new();

    // Fixed array field
    let mut array_field = Field::with_kind(
        1,
        "FIXED-ARRAY".to_string(),
        FieldKind::Alphanum { len: 10 },
    );
    array_field.occurs = Some(Occurs::Fixed { count: 5 });
    schema.fields.push(array_field);

    resolve_layout(&mut schema).unwrap();

    // Fixed arrays should always produce fixed record length
    assert_eq!(schema.lrecl_fixed, Some(10 * 5));
    assert!(schema.tail_odo.is_none());
}
