// SPDX-License-Identifier: AGPL-3.0-or-later
//! Deep property tests for schema structural invariants.
//!
//! Tests that parsed schemas satisfy strict structural properties: LRECL equals
//! sum of field widths, fingerprints are stable, offsets are monotonically
//! increasing, OCCURS fields have correct multiplied widths, and schemas
//! round-trip through serde.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_core::{Field, Schema, parse_copybook};
use proptest::prelude::*;
use std::collections::HashSet;

use super::config::{DEFAULT_CASES, QUICK_CASES};

/// Collect all leaf (non-group, non-FILLER) fields recursively.
fn collect_leaves(fields: &[Field]) -> Vec<&Field> {
    let mut result = Vec::new();
    for f in fields {
        if f.children.is_empty() {
            result.push(f);
        } else {
            result.extend(collect_leaves(&f.children));
        }
    }
    result
}

/// Compute max(offset + `effective_length`) across all fields.
fn max_end(fields: &[Field]) -> u32 {
    let mut result = 0u32;
    for f in fields {
        let end = f.offset.saturating_add(f.effective_length());
        if end > result {
            result = end;
        }
        let child_end = max_end(&f.children);
        if child_end > result {
            result = child_end;
        }
    }
    result
}

// ============================================================================
// 1. LRECL matches sum of field widths (flat PIC X)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For uniform flat PIC X fields, max-end == count × width.
    #[test]
    fn prop_sid_lrecl_matches_flat_pic_x_sum(
        n in 1usize..=12,
        len in 1u32..=25,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  FL-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        #[allow(clippy::cast_possible_truncation)]
        let expected = n as u32 * len;
        prop_assert_eq!(computed, expected);
    }

    /// For mixed PIC X + PIC 9, max-end == sum of individual sizes.
    #[test]
    fn prop_sid_lrecl_matches_mixed_sum(
        x_lens in prop::collection::vec(1u32..=20, 1..=5),
        n_lens in prop::collection::vec(1u32..=9, 1..=5),
    ) {
        let mut lines = Vec::new();
        let mut expected_sum = 0u32;
        for (i, &len) in x_lens.iter().enumerate() {
            lines.push(format!("       05  MX-{i} PIC X({len})."));
            expected_sum += len;
        }
        for (i, &digits) in n_lens.iter().enumerate() {
            lines.push(format!("       05  MN-{i} PIC 9({digits})."));
            expected_sum += digits;
        }

        let copybook = lines.join("\n");
        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        prop_assert_eq!(computed, expected_sum);
    }

    /// COMP-3 fields have correct LRECL contribution: (digits+2)/2 bytes.
    #[test]
    fn prop_sid_lrecl_comp3_field_size(
        prefix_len in 1u32..=10,
        comp3_digits in 1u32..=9,
    ) {
        let copybook = format!(
            "       05  PRE PIC X({prefix_len}).\n       05  PKD PIC S9({comp3_digits}) COMP-3."
        );

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        let packed_len = (comp3_digits + 2) / 2;
        prop_assert_eq!(computed, prefix_len + packed_len);
    }
}

// ============================================================================
// 2. Schema fingerprint is stable across parse iterations
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Parsing the same copybook N times always yields the same fingerprint.
    #[test]
    fn prop_sid_fingerprint_stable_across_iterations(
        n in 1usize..=8,
        len in 1u32..=20,
        iterations in 2usize..=5,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  FP-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let first = parse_copybook(&copybook).expect("parse 1");
        for _ in 1..iterations {
            let again = parse_copybook(&copybook).expect("parse N");
            prop_assert_eq!(&first.fingerprint, &again.fingerprint);
        }
    }

    /// Fingerprint changes when any field name differs.
    #[test]
    fn prop_sid_fingerprint_changes_with_name(
        base_count in 1usize..=5,
        len in 1u32..=10,
    ) {
        let base = (0..base_count)
            .map(|i| format!("       05  NA-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let altered = (0..base_count)
            .map(|i| format!("       05  NB-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let s1 = parse_copybook(&base).expect("parse base");
        let s2 = parse_copybook(&altered).expect("parse altered");
        prop_assert_ne!(&s1.fingerprint, &s2.fingerprint);
    }

    /// Canonical JSON is identical across independent parse calls.
    #[test]
    fn prop_sid_canonical_json_stable(
        n in 1usize..=6,
        len in 1u32..=15,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  CJ-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let s1 = parse_copybook(&copybook).expect("parse 1");
        let s2 = parse_copybook(&copybook).expect("parse 2");
        prop_assert_eq!(s1.create_canonical_json(), s2.create_canonical_json());
    }
}

// ============================================================================
// 3. Field offsets are monotonically increasing (flat schemas)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// In flat schemas, each leaf field's offset strictly increases.
    #[test]
    fn prop_sid_offsets_monotonically_increasing(
        field_lens in prop::collection::vec(1u32..=20, 2..=10),
    ) {
        let copybook = field_lens
            .iter()
            .enumerate()
            .map(|(i, &len)| format!("       05  MO-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let leaves = collect_leaves(&schema.fields);

        for pair in leaves.windows(2) {
            prop_assert!(
                pair[1].offset > pair[0].offset,
                "offset of {} ({}) must be > offset of {} ({})",
                pair[1].name, pair[1].offset, pair[0].name, pair[0].offset,
            );
        }
    }

    /// Each leaf field's offset equals the cumulative sum of preceding field lengths.
    #[test]
    fn prop_sid_offsets_equal_cumulative_sum(
        field_lens in prop::collection::vec(1u32..=15, 1..=8),
    ) {
        let copybook = field_lens
            .iter()
            .enumerate()
            .map(|(i, &len)| format!("       05  CS-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let leaves = collect_leaves(&schema.fields);

        let mut expected_offset = 0u32;
        for (i, leaf) in leaves.iter().enumerate() {
            prop_assert_eq!(
                leaf.offset, expected_offset,
                "field {} at index {} expected offset {}",
                leaf.name, i, expected_offset
            );
            expected_offset += leaf.effective_length();
        }
    }

    /// Grouped children have offsets ≥ parent offset.
    #[test]
    fn prop_sid_child_offsets_gte_parent(
        num_children in 1usize..=6,
        child_len in 1u32..=10,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  GC-{i} PIC X({child_len})."))
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
}

// ============================================================================
// 4. OCCURS fields have correct array multiplied widths
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: QUICK_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC X(n) OCCURS k: effective_length == n × k.
    #[test]
    fn prop_sid_occurs_multiplied_width(
        field_len in 1u32..=10,
        occurs in 2u32..=10,
    ) {
        let copybook = format!(
            "       01 REC.\n           05 ARR PIC X({field_len}) OCCURS {occurs}."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let arr_field = &schema.fields[0].children[0];

        prop_assert_eq!(
            arr_field.effective_length(),
            field_len * occurs,
            "OCCURS {} × PIC X({}) should be {}",
            occurs, field_len, field_len * occurs,
        );
    }

    /// OCCURS on group: group effective_length == child sum × count.
    #[test]
    fn prop_sid_occurs_group_width(
        child_len in 1u32..=5,
        num_children in 1usize..=3,
        occurs in 2u32..=5,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("           10  CH-{i} PIC X({child_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!(
            "       01 REC.\n           05 GRP OCCURS {occurs}.\n{children}"
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let grp = &schema.fields[0].children[0];

        #[allow(clippy::cast_possible_truncation)]
        let expected = child_len * (num_children as u32) * occurs;
        prop_assert_eq!(
            grp.effective_length(),
            expected,
            "OCCURS {} × {} children × PIC X({}) should be {}",
            occurs, num_children, child_len, expected,
        );
    }

    /// Schema LRECL includes OCCURS multiplied size.
    #[test]
    fn prop_sid_occurs_lrecl_includes_array(
        prefix_len in 1u32..=10,
        elem_len in 1u32..=5,
        occurs in 2u32..=8,
    ) {
        let copybook = format!(
            "       01 REC.\n           05 PRE PIC X({prefix_len}).\n           05 ARR PIC X({elem_len}) OCCURS {occurs}."
        );
        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        prop_assert_eq!(computed, prefix_len + elem_len * occurs);
    }
}

// ============================================================================
// 5. Schema serialization round-trips through serde
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Schema → JSON → Schema preserves all_fields count.
    #[test]
    fn prop_sid_serde_preserves_field_count(
        n in 1usize..=10,
        len in 1u32..=20,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  SE-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");
        prop_assert_eq!(back.all_fields().len(), schema.all_fields().len());
    }

    /// Schema → JSON → Schema preserves fingerprint.
    #[test]
    fn prop_sid_serde_preserves_fingerprint(
        n in 1usize..=8,
        len in 1u32..=15,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  SF-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");
        prop_assert_eq!(&schema.fingerprint, &back.fingerprint);
    }

    /// Schema → JSON → Schema preserves field names.
    #[test]
    fn prop_sid_serde_preserves_field_names(
        n in 1usize..=8,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  NM-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");

        let orig_names: Vec<&str> = schema.all_fields().iter().map(|f| f.name.as_str()).collect();
        let back_names: Vec<&str> = back.all_fields().iter().map(|f| f.name.as_str()).collect();
        prop_assert_eq!(orig_names, back_names);
    }

    /// Schema → JSON → Schema preserves field offsets and lengths.
    #[test]
    fn prop_sid_serde_preserves_offsets_and_lengths(
        field_lens in prop::collection::vec(1u32..=15, 1..=6),
    ) {
        let copybook = field_lens
            .iter()
            .enumerate()
            .map(|(i, &len)| format!("       05  OL-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let json = serde_json::to_string(&schema).expect("serialize");
        let back: Schema = serde_json::from_str(&json).expect("deserialize");

        for (orig, restored) in schema.all_fields().iter().zip(back.all_fields().iter()) {
            prop_assert_eq!(orig.offset, restored.offset);
            prop_assert_eq!(orig.len, restored.len);
        }
    }
}

// ============================================================================
// 6. Additional structural invariants
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Flat schemas have unique field names among siblings.
    #[test]
    fn prop_sid_unique_field_names(
        n in 1usize..=10,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  UQ-{i} PIC X(5)."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let names: Vec<&str> = schema.fields.iter().map(|f| f.name.as_str()).collect();
        let unique: HashSet<&str> = names.iter().copied().collect();
        prop_assert_eq!(unique.len(), names.len());
    }

    /// All leaf fields have positive effective_length.
    #[test]
    fn prop_sid_leaf_fields_positive_length(
        field_lens in prop::collection::vec(1u32..=30, 1..=8),
    ) {
        let copybook = field_lens
            .iter()
            .enumerate()
            .map(|(i, &len)| format!("       05  PL-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        for leaf in collect_leaves(&schema.fields) {
            prop_assert!(
                leaf.effective_length() > 0,
                "field {} must have positive effective_length",
                leaf.name,
            );
        }
    }
}
