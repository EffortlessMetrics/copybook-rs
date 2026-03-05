// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for schema generation and parsing.
//!
//! Tests that generated copybook text always parses without panic,
//! PIC clauses produce valid field kinds, LRECL is the sum of field
//! sizes, group nesting preserves hierarchy, REDEFINES share offsets,
//! OCCURS counts are correct, serde round-trips are lossless,
//! fingerprints are 64 hex chars, parsing is deterministic, and
//! arbitrary valid COBOL names parse successfully.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_core::{FieldKind, Occurs, Schema, parse_copybook};
use proptest::prelude::*;
use std::panic::catch_unwind;

use super::config::DEFAULT_CASES;
use super::generators::{field_name_strategy, pic_clause_strategy, simple_copybook_strategy};

// -------------------------------------------------------------------------
// 1. Arbitrary valid copybook text → parse → never panics
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_valid_copybook_never_panics(copybook in simple_copybook_strategy()) {
        let result = catch_unwind(move || {
            let _ = parse_copybook(&copybook);
        });
        prop_assert!(result.is_ok(), "parse_copybook panicked on generated copybook");
    }
}

// -------------------------------------------------------------------------
// 2. Arbitrary PIC clause → parse → field kind is valid
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_pic_produces_valid_field_kind(pic in pic_clause_strategy()) {
        let copybook = format!("       05  TEST-FLD PIC {pic}.");
        let schema = parse_copybook(&copybook).expect("parse");
        let field = &schema.fields[0];

        // Every parsed field must have a recognised kind.
        let valid = matches!(
            field.kind,
            FieldKind::Alphanum { .. }
                | FieldKind::ZonedDecimal { .. }
                | FieldKind::BinaryInt { .. }
                | FieldKind::PackedDecimal { .. }
                | FieldKind::EditedNumeric { .. }
                | FieldKind::Group
                | FieldKind::Condition { .. }
                | FieldKind::Renames { .. }
                | FieldKind::FloatSingle
                | FieldKind::FloatDouble
        );
        prop_assert!(valid, "Unexpected FieldKind for PIC {pic}");
    }
}

// -------------------------------------------------------------------------
// 3. N random fields → parse → LRECL == sum of sizes
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_lrecl_equals_sum_of_sizes(
        n in 1usize..=10,
        len in 1u32..=50,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  FLD-{i:03} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");

        let max_end: u32 = schema
            .all_fields()
            .iter()
            .map(|f| f.offset.saturating_add(f.len))
            .max()
            .unwrap_or(0);

        let expected = n as u32 * len;
        prop_assert_eq!(max_end, expected);
    }
}

// -------------------------------------------------------------------------
// 4. Arbitrary group nesting → parse → hierarchy correct
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_group_nesting_hierarchy(depth in 1usize..=5) {
        let mut lines = Vec::new();
        for i in 0..depth {
            let level = (i + 1) * 5;
            lines.push(format!("       {level:02}  GRP-{i}."));
        }
        let leaf_level = (depth + 1) * 5;
        lines.push(format!("       {leaf_level:02}  LEAF-FLD PIC X(5)."));

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        // total fields = depth groups + 1 leaf
        let all = schema.all_fields();
        prop_assert_eq!(
            all.len(),
            depth + 1,
            "Expected {} fields, got {}",
            depth + 1,
            all.len()
        );

        // Walk the nesting chain; each group must have exactly one child.
        let mut node = &schema.fields[0];
        for _ in 0..(depth - 1) {
            prop_assert_eq!(
                node.children.len(),
                1,
                "Group {} should have 1 child",
                node.name
            );
            node = &node.children[0];
        }
        // Innermost group holds the leaf.
        prop_assert_eq!(
            node.children.len(),
            1,
            "Innermost group should contain the leaf"
        );
        prop_assert!(
            node.children[0].children.is_empty(),
            "Leaf should have no children"
        );
    }
}

// -------------------------------------------------------------------------
// 5. REDEFINES → parse → overlay offset matches
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_redefines_offset_matches(len in 1u32..=50) {
        let copybook = format!(
            "       05  BASE-FLD PIC X({len}).\n       05  RED-FLD REDEFINES BASE-FLD PIC X({len})."
        );
        let schema = parse_copybook(&copybook).expect("parse");

        let base = &schema.fields[0];
        let redef = &schema.fields[1];

        prop_assert_eq!(
            redef.offset, base.offset,
            "REDEFINES offset {} != base offset {}",
            redef.offset, base.offset
        );
        prop_assert_eq!(
            redef.redefines_of.as_deref(),
            Some(base.path.as_str()),
            "redefines_of should point to base path"
        );
    }
}

// -------------------------------------------------------------------------
// 6. OCCURS → parse → array count correct
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_fixed_occurs_count(
        elem_len in 1u32..=20,
        count in 1u32..=50,
    ) {
        let copybook = format!(
            "       05  ARR-FLD PIC X({elem_len}) OCCURS {count} TIMES."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let field = &schema.fields[0];

        match &field.occurs {
            Some(Occurs::Fixed { count: c }) => {
                prop_assert_eq!(*c, count, "Fixed OCCURS count mismatch");
            }
            other => {
                prop_assert!(false, "Expected Occurs::Fixed, got {other:?}");
            }
        }

        prop_assert_eq!(
            field.effective_length(),
            elem_len * count,
            "effective_length should be elem * count"
        );
    }

    #[test]
    fn prop_gen_odo_occurs_count(
        elem_len in 1u32..=20,
        min_count in 1u32..=5,
        max_count in 6u32..=20,
    ) {
        let copybook = format!(
            "       05  CTR PIC 9(3).\n       05  ARR-FLD PIC X({elem_len}) OCCURS {min_count} TO {max_count} TIMES DEPENDING ON CTR."
        );
        let schema = parse_copybook(&copybook).expect("parse");

        let odo_field = schema.fields.iter()
            .find(|f| f.name == "ARR-FLD")
            .expect("ARR-FLD not found");

        match &odo_field.occurs {
            Some(Occurs::ODO { min, max, .. }) => {
                prop_assert_eq!(*min, min_count, "ODO min mismatch");
                prop_assert_eq!(*max, max_count, "ODO max mismatch");
            }
            other => {
                prop_assert!(false, "Expected Occurs::ODO, got {other:?}");
            }
        }
    }
}

// -------------------------------------------------------------------------
// 7. Mixed copybook → parse → serde roundtrip preserves
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_serde_roundtrip_preserves(copybook in simple_copybook_strategy()) {
        let schema = parse_copybook(&copybook).expect("parse");

        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");

        // Field count preserved
        prop_assert_eq!(
            back.all_fields().len(),
            schema.all_fields().len(),
            "all_fields count mismatch after serde roundtrip"
        );
        // lrecl_fixed preserved
        prop_assert_eq!(back.lrecl_fixed, schema.lrecl_fixed);

        // Per-field identity checks
        for (orig, rt) in schema.fields.iter().zip(back.fields.iter()) {
            prop_assert_eq!(&orig.name, &rt.name);
            prop_assert_eq!(orig.level, rt.level);
            prop_assert_eq!(orig.offset, rt.offset);
            prop_assert_eq!(orig.len, rt.len);
        }
    }
}

// -------------------------------------------------------------------------
// 8. Fingerprint is always 64 hex chars
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_fingerprint_64_hex_chars(n in 1usize..=8) {
        let copybook = (0..n)
            .map(|i| format!("       05  FP-{i:03} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");

        prop_assert_eq!(
            schema.fingerprint.len(),
            64,
            "Fingerprint length {} != 64",
            schema.fingerprint.len()
        );
        prop_assert!(
            schema.fingerprint.chars().all(|c| c.is_ascii_hexdigit()),
            "Fingerprint contains non-hex chars: {}",
            schema.fingerprint
        );
    }
}

// -------------------------------------------------------------------------
// 9. Parse same copybook twice → identical Schema
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_parse_deterministic(copybook in simple_copybook_strategy()) {
        let s1 = parse_copybook(&copybook).expect("parse 1");
        let s2 = parse_copybook(&copybook).expect("parse 2");

        prop_assert_eq!(&s1.fingerprint, &s2.fingerprint, "fingerprints differ");
        prop_assert_eq!(s1.lrecl_fixed, s2.lrecl_fixed, "lrecl_fixed differs");
        prop_assert_eq!(
            s1.all_fields().len(),
            s2.all_fields().len(),
            "field count differs"
        );

        for (a, b) in s1.all_fields().iter().zip(s2.all_fields().iter()) {
            prop_assert_eq!(&a.name, &b.name);
            prop_assert_eq!(a.offset, b.offset);
            prop_assert_eq!(a.len, b.len);
        }
    }
}

// -------------------------------------------------------------------------
// 10. Arbitrary valid COBOL names → parse succeeds
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_arbitrary_valid_cobol_names(name in field_name_strategy()) {
        let copybook = format!("       05  {name} PIC X(10).");
        let result = parse_copybook(&copybook);

        prop_assert!(
            result.is_ok(),
            "Valid COBOL name '{name}' should parse: {:?}",
            result.err()
        );

        let schema = result.unwrap();
        prop_assert_eq!(&schema.fields[0].name, &name);
    }
}

// -------------------------------------------------------------------------
// 11. Mixed PIC types in one copybook → all field kinds valid
// -------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_gen_mixed_pic_types_all_valid(
        pics in proptest::collection::vec(pic_clause_strategy(), 2..=6)
    ) {
        let copybook = pics
            .iter()
            .enumerate()
            .map(|(i, pic)| format!("       05  MIX-{i:03} PIC {pic}."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");

        for field in &schema.fields {
            let valid = matches!(
                field.kind,
                FieldKind::Alphanum { .. }
                    | FieldKind::ZonedDecimal { .. }
                    | FieldKind::BinaryInt { .. }
                    | FieldKind::PackedDecimal { .. }
                    | FieldKind::EditedNumeric { .. }
                    | FieldKind::Group
                    | FieldKind::FloatSingle
                    | FieldKind::FloatDouble
                    | FieldKind::Condition { .. }
                    | FieldKind::Renames { .. }
            );
            prop_assert!(valid, "Unexpected FieldKind for field {}", field.name);
        }
    }
}
