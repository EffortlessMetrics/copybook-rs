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
mod determinism;
mod fixed;
mod generators;
mod numeric;
mod parsing;
mod pic_clauses;
mod rdw;
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
