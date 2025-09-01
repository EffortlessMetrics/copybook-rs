//! Core parsing and schema types for COBOL copybooks
//!
//! This crate provides the fundamental types and parsing logic for COBOL copybook
//! processing, including AST construction, layout resolution, and schema validation.

pub mod error;
pub mod schema;
pub mod parser;
pub mod layout;

pub use error::{Error, Result};
pub use schema::{Schema, Field, FieldKind, Occurs};

/// Parse a COBOL copybook into a structured schema
pub fn parse_copybook(text: &str) -> Result<Schema> {
    parser::parse(text)
}