#![allow(unused_doc_comments, unused_imports, dead_code)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property-based tests for copybook-rs
//!
//! This is the entry point for all property tests. Property tests verify
//! that certain invariants hold true for a wide range of randomly
//! generated inputs.

#![allow(clippy::expect_used)]
#![allow(clippy::unwrap_used)]

mod arrays;
mod config;
mod corruption;
mod determinism;
mod fixed;
mod generators;
mod lexer;
mod numeric;
mod parsing;
mod pic_clauses;
mod prop_codec_bijection;
mod prop_codec_roundtrip;
mod prop_codepage;
mod prop_corruption;
mod prop_corruption_detect;
mod prop_determinism;
mod prop_edited_pic;
mod prop_edited_pic_extended;
mod prop_field_layout;
mod prop_field_roundtrip;
mod prop_governance;
mod prop_no_panic;
mod prop_overflow;
mod prop_overflow_extended;
mod prop_overflow_safety;
mod prop_rdw;
mod prop_schema_gen;
mod prop_schema_invariants;
mod prop_schema_projection;
mod prop_sign_separate;
mod rdw;
mod record_io;
mod redefines;
mod roundtrip;

use copybook_core::Schema;

fn schema_max_end(schema: &Schema) -> u32 {
    schema
        .all_fields()
        .iter()
        .map(|field| field.offset.saturating_add(field.len))
        .max()
        .unwrap_or(0)
}

fn schema_record_length(schema: &Schema) -> Option<u32> {
    schema.lrecl_fixed.or_else(|| {
        let max_end = schema_max_end(schema);
        (max_end > 0).then_some(max_end)
    })
}

fn main() {
    // Property tests are run via cargo test, not as a binary
    println!("Run property tests with: cargo test -p copybook-proptest");
    println!("Or run with: cargo test --test proptest");
}
