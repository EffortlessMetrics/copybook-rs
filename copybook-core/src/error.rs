//! Error types and taxonomy for copybook-rs
//!
//! This module defines a comprehensive error taxonomy with stable error codes
//! for all failure modes in the copybook processing system.

use std::fmt;
use thiserror::Error;

/// Result type alias for copybook operations
pub type Result<T> = std::result::Result<T, Error>;

/// Main error type for copybook operations
#[derive(Error, Debug, Clone)]
pub struct Error {
    /// Stable error code for programmatic handling
    pub code: ErrorCode,
    /// Human-readable error message
    pub message: String,
    /// Optional context information
    pub context: Option<ErrorContext>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.code, self.message)?;
        if let Some(ref ctx) = self.context {
            write!(f, " ({ctx})")?;
        }
        Ok(())
    }
}

/// Stable error codes for programmatic error handling
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)] // These are stable external error codes
pub enum ErrorCode {
    // Parse Errors (CBKP*)
    /// CBKP001: General copybook syntax error
    CBKP001_SYNTAX,
    /// CBKP011: Unsupported COBOL clause or feature
    CBKP011_UNSUPPORTED_CLAUSE,
    /// CBKP021: ODO array not at tail position
    CBKP021_ODO_NOT_TAIL,
    /// CBKP051: Unsupported edited PIC clause
    CBKP051_UNSUPPORTED_EDITED_PIC,

    // Schema Errors (CBKS*)
    /// CBKS121: ODO counter field not found
    CBKS121_COUNTER_NOT_FOUND,
    /// CBKS141: Record size exceeds maximum limit
    CBKS141_RECORD_TOO_LARGE,
    /// CBKS301: ODO count clipped to maximum
    CBKS301_ODO_CLIPPED,
    /// CBKS302: ODO count raised to minimum
    CBKS302_ODO_RAISED,

    // Record Errors (CBKR*)
    /// CBKR211: RDW reserved bytes are non-zero
    CBKR211_RDW_RESERVED_NONZERO,
    /// CBKR221: RDW length underflow
    CBKR221_RDW_UNDERFLOW,

    // Character Conversion Errors (CBKC*)
    /// CBKC201: JSON write error
    CBKC201_JSON_WRITE_ERROR,
    /// CBKC301: Invalid EBCDIC byte encountered
    CBKC301_INVALID_EBCDIC_BYTE,

    // Data Decode Errors (CBKD*)
    /// CBKD101: Invalid field type for operation
    CBKD101_INVALID_FIELD_TYPE,
    /// CBKD301: Record too short for field
    CBKD301_RECORD_TOO_SHORT,
    /// CBKD401: Invalid packed decimal nibble
    CBKD401_COMP3_INVALID_NIBBLE,
    /// CBKD411: Invalid zoned decimal sign
    CBKD411_ZONED_BAD_SIGN,
    /// CBKD412: Zoned field is blank (BLANK WHEN ZERO)
    CBKD412_ZONED_BLANK_IS_ZERO,

    // Encode Errors (CBKE*)
    /// CBKE501: JSON type doesn't match field type
    CBKE501_JSON_TYPE_MISMATCH,
    /// CBKE521: Array length out of bounds
    CBKE521_ARRAY_LEN_OOB,

    // File/Format Errors (CBKF*)
    /// CBKF104: RDW appears to be ASCII-corrupted
    CBKF104_RDW_SUSPECT_ASCII,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let code_str = match self {
            ErrorCode::CBKP001_SYNTAX => "CBKP001_SYNTAX",
            ErrorCode::CBKP011_UNSUPPORTED_CLAUSE => "CBKP011_UNSUPPORTED_CLAUSE",
            ErrorCode::CBKP021_ODO_NOT_TAIL => "CBKP021_ODO_NOT_TAIL",
            ErrorCode::CBKP051_UNSUPPORTED_EDITED_PIC => "CBKP051_UNSUPPORTED_EDITED_PIC",
            ErrorCode::CBKS121_COUNTER_NOT_FOUND => "CBKS121_COUNTER_NOT_FOUND",
            ErrorCode::CBKS141_RECORD_TOO_LARGE => "CBKS141_RECORD_TOO_LARGE",
            ErrorCode::CBKS301_ODO_CLIPPED => "CBKS301_ODO_CLIPPED",
            ErrorCode::CBKS302_ODO_RAISED => "CBKS302_ODO_RAISED",
            ErrorCode::CBKR211_RDW_RESERVED_NONZERO => "CBKR211_RDW_RESERVED_NONZERO",
            ErrorCode::CBKR221_RDW_UNDERFLOW => "CBKR221_RDW_UNDERFLOW",
            ErrorCode::CBKC201_JSON_WRITE_ERROR => "CBKC201_JSON_WRITE_ERROR",
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE => "CBKC301_INVALID_EBCDIC_BYTE",
            ErrorCode::CBKD101_INVALID_FIELD_TYPE => "CBKD101_INVALID_FIELD_TYPE",
            ErrorCode::CBKD301_RECORD_TOO_SHORT => "CBKD301_RECORD_TOO_SHORT",
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE => "CBKD401_COMP3_INVALID_NIBBLE",
            ErrorCode::CBKD411_ZONED_BAD_SIGN => "CBKD411_ZONED_BAD_SIGN",
            ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO => "CBKD412_ZONED_BLANK_IS_ZERO",
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH => "CBKE501_JSON_TYPE_MISMATCH",
            ErrorCode::CBKE521_ARRAY_LEN_OOB => "CBKE521_ARRAY_LEN_OOB",
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII => "CBKF104_RDW_SUSPECT_ASCII",
        };
        write!(f, "{code_str}")
    }
}

/// Context information for errors
#[derive(Debug, Clone)]
pub struct ErrorContext {
    /// Record number (1-based) where error occurred
    pub record_index: Option<u64>,
    /// Field path where error occurred
    pub field_path: Option<String>,
    /// Byte offset where error occurred
    pub byte_offset: Option<u64>,
    /// Line number in copybook (for parse errors)
    pub line_number: Option<u32>,
    /// Additional context-specific information
    pub details: Option<String>,
}

impl fmt::Display for ErrorContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut parts = Vec::new();

        if let Some(record) = self.record_index {
            parts.push(format!("record {record}"));
        }
        if let Some(ref path) = self.field_path {
            parts.push(format!("field {path}"));
        }
        if let Some(offset) = self.byte_offset {
            parts.push(format!("offset {offset}"));
        }
        if let Some(line) = self.line_number {
            parts.push(format!("line {line}"));
        }
        if let Some(ref details) = self.details {
            parts.push(details.clone());
        }

        write!(f, "{}", parts.join(", "))
    }
}

impl Error {
    /// Create a new error with the given code and message
    pub fn new(code: ErrorCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            context: None,
        }
    }

    /// Add context information to the error
    #[must_use]
    pub fn with_context(mut self, context: ErrorContext) -> Self {
        self.context = Some(context);
        self
    }

    /// Add record context to the error
    #[must_use]
    pub fn with_record(mut self, record_index: u64) -> Self {
        let context = self.context.get_or_insert(ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: None,
        });
        context.record_index = Some(record_index);
        self
    }

    /// Add field path context to the error
    #[must_use]
    pub fn with_field(mut self, field_path: impl Into<String>) -> Self {
        let context = self.context.get_or_insert(ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: None,
        });
        context.field_path = Some(field_path.into());
        self
    }

    /// Add byte offset context to the error
    #[must_use]
    pub fn with_offset(mut self, byte_offset: u64) -> Self {
        let context = self.context.get_or_insert(ErrorContext {
            record_index: None,
            field_path: None,
            byte_offset: None,
            line_number: None,
            details: None,
        });
        context.byte_offset = Some(byte_offset);
        self
    }
}

/// Convenience macros for creating errors
#[macro_export]
macro_rules! error {
    ($code:expr, $msg:expr) => {
        $crate::error::Error::new($code, $msg)
    };
    ($code:expr, $fmt:expr, $($arg:tt)*) => {
        $crate::error::Error::new($code, format!($fmt, $($arg)*))
    };
}
