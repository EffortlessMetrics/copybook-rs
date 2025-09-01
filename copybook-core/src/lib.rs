//! Core parsing and schema types for COBOL copybooks
//!
//! This crate provides the fundamental types and parsing logic for COBOL copybook
//! processing, including AST construction, layout resolution, and schema validation.

pub mod error;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod pic;
pub mod schema;

pub use error::{Error, ErrorCode, Result};
pub use schema::{Field, FieldKind, Occurs, Schema, TailODO};

/// Parse a COBOL copybook into a structured schema
/// 
/// # Errors
/// 
/// Returns an error if the copybook contains syntax errors or unsupported features
pub fn parse_copybook(text: &str) -> Result<Schema> {
    parser::parse(text)
}
