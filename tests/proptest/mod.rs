#![allow(unused_doc_comments, unused_imports, dead_code)]
#![allow(clippy::missing_inline_in_public_items)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::expect_used)]
// SPDX-License-Identifier: AGPL-3.0-or-later
//! Property testing module for copybook-rs
//!
//! This module contains property-based tests using the `proptest` framework.
//! Property tests verify that certain invariants hold true for a wide range
//! of randomly generated inputs, helping to catch edge cases that traditional
//! unit tests might miss.

pub mod arrays;
pub mod determinism;
pub mod fixed;
pub mod generators;
pub mod numeric;
pub mod parsing;
pub mod pic_clauses;
pub mod prop_codec_bijection;
pub mod prop_codec_roundtrip;
pub mod prop_codepage;
pub mod prop_corruption_detect;
pub mod prop_edited_pic;
pub mod prop_field_layout;
pub mod prop_governance;
pub mod prop_no_panic;
pub mod prop_overflow;
pub mod prop_overflow_extended;
pub mod prop_overflow_safety;
pub mod prop_rdw;
pub mod prop_schema_invariants;
pub mod prop_schema_projection;
pub mod prop_sign_separate;
pub mod rdw;
pub mod record_io;
pub mod redefines;
pub mod roundtrip;
pub mod safe_index;
pub mod safe_text;
pub mod schema_invariants;

use copybook_core::Schema;

pub fn schema_max_end(schema: &Schema) -> u32 {
    schema
        .all_fields()
        .iter()
        .map(|field| field.offset.saturating_add(field.len))
        .max()
        .unwrap_or(0)
}

pub fn schema_record_length(schema: &Schema) -> Option<u32> {
    schema.lrecl_fixed.or_else(|| {
        let max_end = schema_max_end(schema);
        (max_end > 0).then_some(max_end)
    })
}

// Common test configuration
pub mod config {
    /// Default number of test cases for property tests
    pub const DEFAULT_CASES: u32 = 256;

    /// Number of test cases for comprehensive testing (slower)
    pub const COMPREHENSIVE_CASES: u32 = 1024;

    /// Number of test cases for quick testing (CI)
    pub const QUICK_CASES: u32 = 64;

    /// Maximum size for generated copybooks
    pub const MAX_COPYBOOK_SIZE: usize = 1000;

    /// Maximum nesting depth for generated structures
    pub const MAX_NESTING_DEPTH: usize = 5;

    /// Maximum number of fields in a generated copybook
    pub const MAX_FIELDS: usize = 20;

    /// Maximum array size for OCCURS clauses
    pub const MAX_ARRAY_SIZE: usize = 100;

    /// Maximum length for PIC clauses
    pub const MAX_PIC_LENGTH: usize = 18;
}
