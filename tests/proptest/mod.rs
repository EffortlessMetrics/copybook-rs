//! Property testing module for copybook-rs
//!
//! This module contains property-based tests using the `proptest` framework.
//! Property tests verify that certain invariants hold true for a wide range
//! of randomly generated inputs, helping to catch edge cases that traditional
//! unit tests might miss.

pub mod generators;
pub mod roundtrip;
pub mod parsing;
pub mod numeric;
pub mod arrays;
pub mod pic_clauses;
pub mod redefines;

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
