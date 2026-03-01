// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for schema structural invariants.
//!
//! Tests that parsed schemas satisfy key properties: positive LRECL,
//! non-overlapping offsets (except REDEFINES), unique names within a level,
//! and consistency between parent and child offsets.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_core::{Field, Schema, parse_copybook};
use proptest::prelude::*;
use std::collections::HashSet;

use super::config::DEFAULT_CASES;

/// Helper: collect all scalar (leaf) fields via depth-first traversal.
fn collect_leaf_fields(fields: &[Field]) -> Vec<&Field> {
    let mut result = Vec::new();
    for f in fields {
        if f.children.is_empty() {
            result.push(f);
        } else {
            result.extend(collect_leaf_fields(&f.children));
        }
    }
    result
}

/// Helper: collect immediate-child names from a field list.
fn child_names(fields: &[Field]) -> Vec<&str> {
    fields.iter().map(|f| f.name.as_str()).collect()
}

// ---------------------------------------------------------------------------
// Schema with fields has positive LRECL
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Any schema with at least one PIC field has a positive max-end.
    #[test]
    fn prop_schema_positive_lrecl(
        num_fields in 1usize..=10,
        field_len in 1u32..=50,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FLD-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let max_end: u32 = schema
            .all_fields()
            .iter()
            .map(|f| f.offset.saturating_add(f.len))
            .max()
            .unwrap_or(0);
        prop_assert!(max_end > 0, "max_end must be positive when fields exist");
    }

    /// LRECL grows proportionally to the number of fixed-size fields.
    #[test]
    fn prop_lrecl_proportional(
        n in 1usize..=10,
        len in 1u32..=20,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  FLD-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let max_end: u32 = schema
            .all_fields()
            .iter()
            .map(|f| f.offset.saturating_add(f.len))
            .max()
            .unwrap_or(0);
        prop_assert_eq!(max_end, n as u32 * len);
    }
}

// ---------------------------------------------------------------------------
// Field offsets are non-overlapping (except REDEFINES)
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For a flat schema (no REDEFINES), leaf field byte ranges never overlap.
    #[test]
    fn prop_leaf_offsets_non_overlapping(
        num_fields in 1usize..=10,
        field_len in 1u32..=20,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  FLD-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let leaves = collect_leaf_fields(&schema.fields);

        for i in 0..leaves.len() {
            for j in (i + 1)..leaves.len() {
                let a = leaves[i];
                let b = leaves[j];
                let a_end = a.offset + a.effective_length();
                let b_end = b.offset + b.effective_length();
                let overlaps = a.offset < b_end && b.offset < a_end;
                prop_assert!(
                    !overlaps,
                    "Fields {} and {} overlap: [{},{}] vs [{},{}]",
                    a.name, b.name, a.offset, a_end, b.offset, b_end,
                );
            }
        }
    }

    /// REDEFINES fields share the same offset as their base.
    #[test]
    fn prop_redefines_share_offset(
        len in 1u32..=30,
    ) {
        let copybook = format!(
            "       05  BASE-FLD PIC X({len}).\n       05  RED-FLD REDEFINES BASE-FLD PIC X({len})."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let base = &schema.fields[0];
        let redef = &schema.fields[1];
        prop_assert_eq!(redef.offset, base.offset, "REDEFINES must share offset");
    }
}

// ---------------------------------------------------------------------------
// Field names unique within their level
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// In a flat schema each field name is unique among siblings.
    #[test]
    fn prop_field_names_unique_flat(
        num_fields in 1usize..=10,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  UNIQ-FLD-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let names = child_names(&schema.fields);
        let set: HashSet<&str> = names.iter().copied().collect();
        prop_assert_eq!(set.len(), names.len(), "duplicate field names at top level");
    }

    /// In a grouped schema, children within each group have unique names.
    #[test]
    fn prop_field_names_unique_in_group(
        num_children in 1usize..=5,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  CHILD-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  GRP.\n{children}");

        let schema = parse_copybook(&copybook).expect("parse");
        let group = &schema.fields[0];
        let names: Vec<&str> = group.children.iter().map(|f| f.name.as_str()).collect();
        let set: HashSet<&str> = names.iter().copied().collect();
        prop_assert_eq!(set.len(), names.len(), "duplicate child names");
    }
}

// ---------------------------------------------------------------------------
// Parent–child offset consistency
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Every child field's offset >= its parent's offset.
    #[test]
    fn prop_child_offset_gte_parent(
        num_children in 1usize..=5,
        child_len in 1u32..=10,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  CH-{i} PIC X({child_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  PARENT.\n{children}");

        let schema = parse_copybook(&copybook).expect("parse");
        let parent = &schema.fields[0];
        for child in &parent.children {
            prop_assert!(
                child.offset >= parent.offset,
                "child {} offset {} < parent offset {}",
                child.name, child.offset, parent.offset,
            );
        }
    }

    /// The parent group's effective length covers all children.
    #[test]
    fn prop_parent_covers_children(
        num_children in 1usize..=5,
        child_len in 1u32..=10,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  CH-{i} PIC X({child_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  PARENT.\n{children}");

        let schema = parse_copybook(&copybook).expect("parse");
        let parent = &schema.fields[0];
        let parent_end = parent.offset + parent.effective_length();
        for child in &parent.children {
            let child_end = child.offset + child.effective_length();
            prop_assert!(
                child_end <= parent_end,
                "child {} end {} exceeds parent end {}",
                child.name, child_end, parent_end,
            );
        }
    }
}

// ---------------------------------------------------------------------------
// Schema serialization preserves all_fields count
// ---------------------------------------------------------------------------
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Serializing and deserializing a schema preserves the all_fields count.
    #[test]
    fn prop_serde_preserves_field_count(
        num_fields in 1usize..=8,
    ) {
        let copybook = (0..num_fields)
            .map(|i| format!("       05  SF-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");
        prop_assert_eq!(back.all_fields().len(), schema.all_fields().len());
    }
}
