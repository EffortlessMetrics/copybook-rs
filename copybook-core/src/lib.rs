//! Core parsing and schema types for COBOL copybooks
//!
//! This crate provides the fundamental types and parsing logic for COBOL copybook
//! processing, including AST construction, layout resolution, and schema validation.
//!
//! ## Key Features
//!
//! ### Enhanced COBOL Support
//! - **Binary Width Syntax**: Support for explicit `BINARY(n)` width specifications
//! - **Comprehensive Numeric Types**: Full parsing of zoned, packed, and binary field definitions
//! - **Recursion Limits**: Parser protection against deeply nested or malformed copybooks
//! - **Error Context**: Detailed error reporting with field paths and source locations
//!
//! ### Schema Generation
//! - **Field Hierarchy**: Complete representation of COBOL data structures
//! - **Layout Resolution**: Accurate byte offset and size calculations
//! - **Validation**: Comprehensive checks for ODO positioning, field compatibility
//!
//! ### Parser Improvements
//! - **Robustness**: Enhanced handling of edge cases and malformed input
//! - **Performance**: Efficient parsing with minimal memory allocation
//! - **Standards Compliance**: Adherence to COBOL copybook syntax standards

pub mod error;
pub mod error_reporter;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod pic;
pub mod schema;

pub use error::{Error, ErrorCode, ErrorContext, Result};
pub use error_reporter::{ErrorMode, ErrorReport, ErrorReporter, ErrorSeverity, ErrorSummary};
pub use parser::ParseOptions;
pub use schema::{Field, FieldKind, Occurs, Schema, TailODO, WorkloadType};

/// Parse a COBOL copybook into a structured schema
///
/// # Errors
///
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse_copybook(text: &str) -> Result<Schema> {
    parser::parse(text)
}

/// Parse a COBOL copybook with specific options
///
/// # Errors
///
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse_copybook_with_options(text: &str, options: &ParseOptions) -> Result<Schema> {
    parser::parse_with_options(text, options)
}
