#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for field layout invariants.
//!
//! Verifies that field byte offsets are sequential within a group,
//! group sizes sum correctly from their children, and LRECL matches
//! the maximum field extent.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

use copybook_core::parse_copybook;
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

/// Helper: collect all leaf (scalar) fields from a schema via pre-order traversal.
fn leaf_fields(schema: &copybook_core::Schema) -> Vec<&copybook_core::Field> {
    schema
        .all_fields()
        .into_iter()
        .filter(|f| f.is_scalar())
        .collect()
}

/// Property: Sibling scalar fields within the same group have non-overlapping,
/// sequential byte ranges (offset + len of one <= offset of the next),
/// excluding REDEFINES which intentionally overlap.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_sibling_scalar_offsets_are_sequential(num_fields in 2usize..=12) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FLD-{i} PIC X(10)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");

        let leaves = leaf_fields(&schema);
        for pair in leaves.windows(2) {
            let (a, b) = (pair[0], pair[1]);
            if b.redefines_of.is_some() {
                continue;
            }
            prop_assert!(
                a.offset + a.len <= b.offset,
                "field {} ends at {} but field {} starts at {}",
                a.name, a.offset + a.len, b.name, b.offset,
            );
        }
    }
}

/// Property: A group field's length equals the sum of its children's
/// effective lengths (accounting for OCCURS multipliers).
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_group_len_equals_children_sum(num_children in 1usize..=8) {
        let children: String = (0..num_children)
            .map(|i| format!("       05  CHILD-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  GRP-PARENT.\n{children}");

        let schema = parse_copybook(&copybook).expect("parse");
        let group = &schema.fields[0];

        prop_assert!(group.is_group(), "top-level field should be a group");

        let children_total: u32 = group.children.iter().map(|c| c.effective_length()).sum();
        prop_assert_eq!(
            group.len, children_total,
            "group len {} != sum of children effective lengths {}",
            group.len, children_total,
        );
    }
}

/// Property: When a schema has an LRECL, it matches the maximum
/// (offset + effective_length) across all leaf fields.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_lrecl_matches_max_field_extent(num_fields in 1usize..=10) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FLD-{i} PIC X(7)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");

        let max_extent = schema
            .all_fields()
            .iter()
            .filter(|f| f.is_scalar())
            .map(|f| f.offset + f.effective_length())
            .max()
            .unwrap_or(0);

        if let Some(lrecl) = schema.lrecl_fixed {
            prop_assert_eq!(
                lrecl, max_extent,
                "lrecl_fixed {} != max field extent {}",
                lrecl, max_extent,
            );
        } else {
            prop_assert!(max_extent > 0);
        }
    }
}

/// Property: Fields with OCCURS have effective_length == len * count.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_occurs_field_effective_length(
        elem_len in 1u32..=20,
        count in 2u32..=10,
    ) {
        let copybook = format!(
            "       05  ARR-FLD PIC X({elem_len}) OCCURS {count} TIMES."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let field = &schema.fields[0];

        prop_assert_eq!(field.len, elem_len);
        prop_assert_eq!(field.effective_length(), elem_len * count);
    }
}

/// Property: Nested groups accumulate offsets correctly - the first
/// child of each nested group starts at the group's own offset.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_nested_group_child_offset_starts_at_group_offset(depth in 1usize..=4) {
        let mut lines = Vec::new();
        for i in 0..depth {
            let level = (i + 1) * 5;
            lines.push(format!("       {:02}  NEST-{i}.", level));
        }
        let leaf_level = depth * 5 + 5;
        lines.push(format!("       {:02}  LEAF PIC X(3).", leaf_level));

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");

        let mut current = &schema.fields[0];
        for _ in 1..depth {
            prop_assert!(current.is_group());
            prop_assert!(!current.children.is_empty());
            let child = &current.children[0];
            prop_assert_eq!(
                child.offset, current.offset,
                "first child offset {} != parent offset {}",
                child.offset, current.offset,
            );
            current = child;
        }
    }
}

/// Property: Adjacent sibling groups do not have overlapping byte ranges.
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    #[test]
    fn prop_adjacent_groups_no_overlap(
        len_a in 1u32..=20,
        len_b in 1u32..=20,
    ) {
        let copybook = format!(
            "       01  ROOT.\n       05  GRP-A.\n           10  FA PIC X({len_a}).\n       05  GRP-B.\n           10  FB PIC X({len_b}).",
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let root = &schema.fields[0];

        prop_assert_eq!(root.children.len(), 2);
        let grp_a = &root.children[0];
        let grp_b = &root.children[1];

        prop_assert!(
            grp_a.offset + grp_a.len <= grp_b.offset,
            "GRP-A [{}, {}) overlaps GRP-B [{}, {})",
            grp_a.offset, grp_a.offset + grp_a.len,
            grp_b.offset, grp_b.offset + grp_b.len,
        );
    }
}
