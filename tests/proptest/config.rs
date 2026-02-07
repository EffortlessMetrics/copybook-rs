//! Configuration for property tests
//!
//! This module contains configuration constants for property testing.

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
