//! Core parsing and schema types for COBOL copybooks
//!
//! This crate provides the fundamental types and parsing logic for COBOL copybook
//! processing, including AST construction, layout resolution, and schema validation.
//!

#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// Allow missing inline for public methods in this library - too many methods to inline individually
#![allow(clippy::missing_inline_in_public_items)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::format_push_string)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::match_same_arms)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::collapsible_else_if)]
#![allow(clippy::unused_self)]
#![allow(clippy::doc_markdown)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::assigning_clones)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::manual_midpoint)]
#![allow(clippy::redundant_closure_for_method_calls)]
#![allow(clippy::single_match_else)]
#![allow(clippy::ignored_unit_patterns)]

//! ## Key Features
//!
//! ### Enhanced COBOL Support
//! - **Level-88 Condition Values**: Full support for condition name definitions with VALUE clauses
//! - **Binary Width Syntax**: Support for explicit `BINARY(n)` width specifications
//! - **Comprehensive Numeric Types**: Full parsing of zoned, packed, and binary field definitions
//! - **Recursion Limits**: Parser protection against deeply nested or malformed copybooks
//! - **Error Context**: Detailed error reporting with field paths and source locations
//!
//! ### Schema Generation
//! - **Field Hierarchy**: Complete representation of COBOL data structures with Level-88 conditions
//! - **Layout Resolution**: Accurate byte offset and size calculations
//! - **Structural Validation**: Comprehensive checks for ODO positioning, Level-88 placement, REDEFINES compatibility
//!
//! ### Parser Improvements
//! - **Robustness**: Enhanced handling of edge cases and malformed input
//! - **Performance**: Efficient parsing with minimal memory allocation
//! - **Standards Compliance**: Adherence to COBOL copybook syntax standards
//! - **Safety**: Zero unsafe code with comprehensive validation
//!
//! ## Usage Example
//!
//! ```rust
//! use copybook_core::{parse_copybook, parse_copybook_with_options, ParseOptions, FieldKind};
//!
//! // Parse a copybook with Level-88 condition values
//! let copybook_text = r#"
//!        01  CUSTOMER-RECORD.
//!            05  CUSTOMER-STATUS     PIC X(1).
//!                88  STATUS-ACTIVE   VALUE 'A'.
//!                88  STATUS-INACTIVE VALUE 'I'.
//!                88  STATUS-PENDING  VALUE 'P'.
//!            05  CUSTOMER-ID         PIC 9(6).
//!            05  ORDER-COUNT         PIC 9(3).
//!            05  ORDERS OCCURS 1 TO 100 TIMES
//!                    DEPENDING ON ORDER-COUNT.
//!                10  ORDER-ID        PIC 9(8).
//!                88  RUSH-ORDER      VALUE 99999999.
//! "#;
//!
//! // Parse with default options and handle any errors gracefully
//! let schema_default = match parse_copybook(copybook_text) {
//!     Ok(schema) => schema,
//!     Err(error) => {
//!         eprintln!("Failed to parse copybook: {error}");
//!         return;
//!     }
//! };
//! println!(
//!     "Default parsing produced {} top-level fields",
//!     schema_default.fields.len()
//! );
//!
//! // Parse with custom options for enhanced validation
//! let options = ParseOptions {
//!     allow_inline_comments: true,
//!     strict: true,
//!     ..Default::default()
//! };
//! let schema_strict = match parse_copybook_with_options(copybook_text, &options) {
//!     Ok(schema) => schema,
//!     Err(error) => {
//!         eprintln!("Strict parsing failed: {error}");
//!         return;
//!     }
//! };
//!
//! // Examine the parsed schema including Level-88 conditions
//! for field in &schema_strict.fields {
//!     match &field.kind {
//!         FieldKind::Condition { values } => {
//!             println!("Level-88 condition '{}' with values: {:?}", field.name, values);
//!         }
//!         FieldKind::Group => {
//!             println!("Group field: {}", field.name);
//!         }
//!         _ => {
//!             println!(
//!                 "Data field: {} (offset: {}, size: {})",
//!                 field.name, field.offset, field.len
//!             );
//!         }
//!     }
//! }
//! ```

#[cfg(feature = "audit")]
pub mod audit;
pub mod error;
pub mod error_reporter;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod pic;
pub mod schema;
pub mod utils;

pub use error::{Error, ErrorCode, ErrorContext, Result};
pub use error_reporter::{ErrorMode, ErrorReport, ErrorReporter, ErrorSeverity, ErrorSummary};
pub use parser::ParseOptions;
pub use schema::{Field, FieldKind, Occurs, Schema, TailODO};
pub use utils::{OptionExt, SliceExt, VecExt, safe_ops};

#[cfg(feature = "audit")]
pub use audit::*;

// Performance-optimized audit stubs when audit feature is disabled
#[cfg(not(feature = "audit"))]
pub mod audit {
    //! No-op audit stubs for performance-critical builds

    use serde::{Deserialize, Serialize};

    /// Lightweight audit context stub (zero-cost when audit disabled)
    #[derive(Debug, Clone, Default, Serialize, Deserialize)]
    pub struct AuditContext {
        // Zero-sized for maximum performance when audit is disabled
    }

    impl AuditContext {
        #[inline]
        pub fn new() -> Self {
            Self::default()
        }
        #[inline]
        pub fn new_lightweight() -> Self {
            Self::default()
        }
        #[inline]
        #[must_use]
        pub fn with_operation_id(self, _id: impl Into<String>) -> Self {
            self // No-op for zero-cost optimization
        }
        #[inline]
        #[must_use]
        pub fn with_user(self, _user: impl Into<String>) -> Self {
            self
        }
        #[inline]
        #[must_use]
        pub fn with_security_classification(self, _classification: SecurityClassification) -> Self {
            self
        }
        #[inline]
        #[must_use]
        pub fn with_compliance_profile(self, _profile: ComplianceProfile) -> Self {
            self
        }
        #[inline]
        #[must_use]
        pub fn with_metadata(self, _key: impl Into<String>, _value: impl Into<String>) -> Self {
            self
        }
        #[inline]
        #[must_use]
        pub fn create_lightweight_child_context(&self, _id: impl Into<String>) -> Self {
            // Return clone for no-op performance - avoid any allocations
            self.clone()
        }
    }

    /// Stub compliance profile enum
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    pub enum ComplianceProfile {
        SOX,
        HIPAA,
        GDPR,
        PCIQDSS,
    }

    /// Stub security classification enum
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
    pub enum SecurityClassification {
        Public,
        Internal,
        Confidential,
        MaterialTransaction,
        PHI,
    }

    pub mod context {
        pub use super::*;
    }
}

/// Parse a COBOL copybook into a structured schema
///
/// # Errors
/// Returns an error if the copybook contains syntax errors or unsupported features.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_copybook(text: &str) -> Result<Schema> {
    parser::parse(text)
}

/// Parse a COBOL copybook with specific options
///
/// # Errors
/// Returns an error if the copybook contains syntax errors or unsupported features.
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn parse_copybook_with_options(text: &str, options: &ParseOptions) -> Result<Schema> {
    parser::parse_with_options(text, options)
}
