//! Core parsing and schema types for COBOL copybooks
//!
//! This crate provides the fundamental types and parsing logic for COBOL copybook
//! processing, including AST construction, layout resolution, and schema validation.

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
pub use schema::{Field, FieldKind, Occurs, Schema, TailODO};

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
