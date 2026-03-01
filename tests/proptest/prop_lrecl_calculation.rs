// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property tests for LRECL (Logical Record Length) calculation invariants.
//!
//! Verifies that LRECL equals the sum of leaf field sizes for flat schemas,
//! that grouped schemas maintain consistent offset/length relationships,
//! and that parsed schemas satisfy structural invariants around record length.

#![allow(clippy::expect_used, clippy::unwrap_used)]

use copybook_codec::{
    Codepage, DecodeOptions, EncodeOptions, RecordFormat, decode_record, encode_record,
};
use copybook_core::{Field, parse_copybook};
use proptest::prelude::*;

use super::config::DEFAULT_CASES;

/// Collect all leaf (non-group) fields recursively.
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

/// Compute max(offset + len) across all fields.
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
// 1. Flat PIC X: LRECL == num_fields × field_len
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// For uniform PIC X fields, max-end equals num_fields × field_len.
    #[test]
    fn prop_lrecl_flat_pic_x_sum(
        n in 1usize..=15,
        len in 1u32..=30,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  LR-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        prop_assert_eq!(
            computed,
            n as u32 * len,
            "LRECL must be {} × {} = {}",
            n, len, n as u32 * len
        );
    }

    /// For uniform PIC 9 fields, max-end equals num_fields × digits.
    #[test]
    fn prop_lrecl_flat_pic_9_sum(
        n in 1usize..=10,
        digits in 1u32..=9,
    ) {
        let copybook = (0..n)
            .map(|i| format!("       05  NF-{i} PIC 9({digits})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        prop_assert_eq!(
            computed,
            n as u32 * digits,
            "LRECL for PIC 9({}) × {}",
            digits, n
        );
    }
}

// ============================================================================
// 2. Mixed field types: LRECL equals sum of individual sizes
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// PIC X + PIC 9 fields: max-end equals sum of their lengths.
    #[test]
    fn prop_lrecl_mixed_pic_x_and_9(
        x_len in 1u32..=30,
        digits in 1u32..=9,
    ) {
        let copybook = format!(
            "       05  MX-ALPHA PIC X({x_len}).\n       05  MX-NUM PIC 9({digits})."
        );

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        prop_assert_eq!(
            computed,
            x_len + digits,
            "LRECL must be {} + {}",
            x_len, digits
        );
    }

    /// PIC X + COMP-3 fields: max-end equals PIC X size + packed bytes.
    #[test]
    fn prop_lrecl_pic_x_plus_comp3(
        x_len in 1u32..=20,
        comp3_digits in 1u32..=9,
    ) {
        let copybook = format!(
            "       05  MC-ALPHA PIC X({x_len}).\n       05  MC-PKD PIC S9({comp3_digits}) COMP-3."
        );

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        let packed_len = (comp3_digits + 2) / 2; // COMP-3: (digits + 1 sign nibble + 1) / 2
        prop_assert_eq!(
            computed,
            x_len + packed_len,
            "LRECL must be {} + {}",
            x_len, packed_len
        );
    }

    /// PIC X + BINARY fields: max-end includes binary field size.
    #[test]
    fn prop_lrecl_pic_x_plus_binary(
        x_len in 1u32..=20,
    ) {
        let copybook = format!(
            "       05  MB-ALPHA PIC X({x_len}).\n       05  MB-BIN PIC S9(4) BINARY."
        );

        let schema = parse_copybook(&copybook).expect("parse");
        let computed = max_end(&schema.fields);
        // PIC S9(4) BINARY = 2 bytes (halfword)
        prop_assert_eq!(computed, x_len + 2, "LRECL must include 2-byte binary");
    }
}

// ============================================================================
// 3. Grouped schemas: parent length covers children
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Group field effective_length equals sum of child lengths.
    #[test]
    fn prop_lrecl_group_covers_children(
        num_children in 1usize..=8,
        child_len in 1u32..=10,
    ) {
        let children: String = (0..num_children)
            .map(|i| format!("       10  GCH-{i} PIC X({child_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01  GRPREC.\n{children}");

        let schema = parse_copybook(&copybook).expect("parse");
        let group = &schema.fields[0];
        prop_assert_eq!(
            group.effective_length(),
            num_children as u32 * child_len,
            "group effective_length must equal sum of children"
        );
    }
}

// ============================================================================
// 4. Leaf offsets are sequential (non-overlapping, contiguous)
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// In flat schemas, each leaf offset equals the sum of preceding field lengths.
    #[test]
    fn prop_lrecl_sequential_offsets(
        field_lens in prop::collection::vec(1u32..=20, 1..=10),
    ) {
        let copybook = field_lens
            .iter()
            .enumerate()
            .map(|(i, &len)| format!("       05  SO-{i} PIC X({len})."))
            .collect::<Vec<_>>()
            .join("\n");

        let schema = parse_copybook(&copybook).expect("parse");
        let leaves = collect_leaves(&schema.fields);

        let mut expected_offset = 0u32;
        for leaf in &leaves {
            prop_assert_eq!(
                leaf.offset, expected_offset,
                "field {} offset should be {}, got {}",
                leaf.name, expected_offset, leaf.offset
            );
            expected_offset += leaf.effective_length();
        }
    }
}

// ============================================================================
// 5. Encoded record length matches LRECL
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// encode_record output length matches the schema's computed max-end.
    #[test]
    fn prop_lrecl_encode_output_matches(
        n in 1usize..=5,
        field_len in 1u32..=15,
    ) {
        let fields: String = (0..n)
            .map(|i| format!("           05 EO-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let copybook = format!("       01 REC.\n{fields}");

        let schema = parse_copybook(&copybook).expect("parse");
        let expected_len = max_end(&schema.fields);

        // Build JSON with placeholder values
        let mut obj = serde_json::Map::new();
        for i in 0..n {
            obj.insert(format!("EO-{i}"), serde_json::Value::String("A".to_string()));
        }
        let json_in = serde_json::Value::Object({
            let mut root = serde_json::Map::new();
            root.insert("REC".to_string(), serde_json::Value::Object(obj));
            root
        });

        let enc = EncodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);
        let binary = encode_record(&schema, &json_in, &enc).expect("encode");

        prop_assert_eq!(
            binary.len(),
            expected_len as usize,
            "encode output length must match computed LRECL"
        );
    }
}

// ============================================================================
// 6. LRECL monotonically increases with added fields
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Adding a field always increases the computed LRECL.
    #[test]
    fn prop_lrecl_monotonic_with_extra_field(
        base_count in 1usize..=8,
        field_len in 1u32..=20,
    ) {
        let base = (0..base_count)
            .map(|i| format!("       05  MN-{i} PIC X({field_len})."))
            .collect::<Vec<_>>()
            .join("\n");
        let extended = format!(
            "{base}\n       05  MN-EXTRA PIC X({field_len})."
        );

        let s1 = parse_copybook(&base).expect("parse base");
        let s2 = parse_copybook(&extended).expect("parse extended");

        let lrecl1 = max_end(&s1.fields);
        let lrecl2 = max_end(&s2.fields);

        prop_assert!(
            lrecl2 > lrecl1,
            "adding a field must increase LRECL: {} → {}",
            lrecl1, lrecl2
        );
    }
}

// ============================================================================
// 7. Error code stability for truncated data
// ============================================================================
proptest! {
    #![proptest_config(ProptestConfig {
        cases: DEFAULT_CASES,
        ..ProptestConfig::default()
    })]

    /// Decoding data shorter than LRECL produces consistent error behavior.
    #[test]
    fn prop_lrecl_truncated_data_consistent_error(
        field_len in 5u32..=30,
        short_len in 0usize..=4,
    ) {
        let copybook = format!("       01 REC.\n           05 FLD PIC X({field_len}).");
        let schema = parse_copybook(&copybook).expect("parse");

        let data = vec![b'A'; short_len];
        let dec = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::ASCII);

        let result1 = decode_record(&schema, &data, &dec);
        let result2 = decode_record(&schema, &data, &dec);

        // Both attempts must agree: both Ok or both Err
        prop_assert_eq!(
            result1.is_ok(), result2.is_ok(),
            "truncated decode must produce consistent success/failure"
        );

        // If both are errors, error messages must match
        if let (Err(e1), Err(e2)) = (&result1, &result2) {
            prop_assert_eq!(
                format!("{e1}"), format!("{e2}"),
                "error messages must be identical for same truncated input"
            );
        }
    }
}
