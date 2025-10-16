//! Error types and taxonomy for copybook-rs
//!
//! This module defines a comprehensive error taxonomy with stable error codes
//! for all failure modes in the copybook processing system.

use std::fmt;
use thiserror::Error;

/// Result type alias for copybook operations
pub type Result<T> = std::result::Result<T, Error>;

/// Main error type for copybook operations
#[derive(Error, Debug, Clone, PartialEq)]
pub struct Error {
    /// Stable error code for programmatic handling
    pub code: ErrorCode,
    /// Human-readable error message
    pub message: String,
    /// Optional context information
    pub context: Option<ErrorContext>,
}

impl fmt::Display for Error {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.code, self.message)?;
        if let Some(ref ctx) = self.context {
            write!(f, " ({ctx})")?;
        }
        Ok(())
    }
}

impl Error {
    /// Return the CBK* family prefix associated with this error.
    #[inline]
    #[must_use]
    pub const fn family_prefix(&self) -> &'static str {
        self.code.family_prefix()
    }
}

/// Stable error codes for programmatic error handling
///
/// The copybook-rs error taxonomy uses a structured approach with stable error codes
/// that enable programmatic error handling across all components. Each code follows
/// the pattern `CBK[Category][Number]_[Description]` where:
///
/// - **CBKP**: Parse errors during copybook analysis
/// - **CBKS**: Schema validation and ODO processing
/// - **CBKR**: Record format and RDW processing
/// - **CBKC**: Character conversion and encoding
/// - **CBKD**: Data decoding and field validation  
/// - **CBKE**: Encoding and JSON serialization  
/// - **CBKF**: File format and structure validation  
/// - **CBKI**: Iterator and infrastructure state validation (e.g., fixed-format without LRECL -> `CBKI001_INVALID_STATE`)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)] // These are stable external error codes
pub enum ErrorCode {
    // =============================================================================
    // Parse Errors (CBKP*) - Copybook syntax and COBOL clause processing
    // =============================================================================
    /// CBKP001: General copybook syntax error during parsing
    CBKP001_SYNTAX,
    /// CBKP011: Unsupported COBOL clause or feature encountered
    CBKP011_UNSUPPORTED_CLAUSE,
    /// CBKP021: ODO (OCCURS DEPENDING ON) array not at tail position
    CBKP021_ODO_NOT_TAIL,
    /// CBKP051: Unsupported edited PIC clause pattern
    CBKP051_UNSUPPORTED_EDITED_PIC,

    // =============================================================================
    // Schema Errors (CBKS*) - Schema validation and ODO processing
    // =============================================================================
    /// CBKS121: ODO counter field not found in schema
    CBKS121_COUNTER_NOT_FOUND,
    /// CBKS141: Record size exceeds maximum allowable limit
    CBKS141_RECORD_TOO_LARGE,
    /// CBKS301: ODO count clipped to maximum allowed value (warning)
    CBKS301_ODO_CLIPPED,
    /// CBKS302: ODO count raised to minimum required value (warning)
    CBKS302_ODO_RAISED,

    // =============================================================================
    // Record Errors (CBKR*) - Record format and RDW processing
    // =============================================================================
    /// CBKR211: RDW reserved bytes contain non-zero values
    CBKR211_RDW_RESERVED_NONZERO,

    // =============================================================================
    // Character Conversion Errors (CBKC*) - EBCDIC/ASCII conversion
    // =============================================================================
    /// CBKC201: JSON serialization write error
    CBKC201_JSON_WRITE_ERROR,
    /// CBKC301: Invalid EBCDIC byte encountered during conversion
    CBKC301_INVALID_EBCDIC_BYTE,

    // =============================================================================
    // Data Decode Errors (CBKD*) - Field validation and numeric processing
    // =============================================================================
    /// CBKD101: Invalid field type for requested operation
    CBKD101_INVALID_FIELD_TYPE,
    /// CBKD301: Record data too short for field requirements
    CBKD301_RECORD_TOO_SHORT,
    /// CBKD401: Invalid packed decimal nibble value
    CBKD401_COMP3_INVALID_NIBBLE,
    /// CBKD410: Zoned decimal value exceeded numeric capacity
    CBKD410_ZONED_OVERFLOW,
    /// CBKD411: Invalid zoned decimal sign zone
    CBKD411_ZONED_BAD_SIGN,
    /// CBKD412: Zoned field contains all spaces (BLANK WHEN ZERO processing)
    CBKD412_ZONED_BLANK_IS_ZERO,
    /// CBKD413: Invalid zoned decimal encoding format detected
    CBKD413_ZONED_INVALID_ENCODING,
    /// CBKD414: Mixed ASCII/EBCDIC encoding within single zoned field
    CBKD414_ZONED_MIXED_ENCODING,
    /// CBKD415: Zoned encoding detection failed or remains ambiguous
    CBKD415_ZONED_ENCODING_AMBIGUOUS,

    // =============================================================================
    // Infrastructure Errors (CBKI*) - Iterator and internal state validation
    // =============================================================================
    /// CBKI001: Iterator or decoder encountered an invalid internal state
    CBKI001_INVALID_STATE,

    // =============================================================================
    // Encode Errors (CBKE*) - JSON to binary encoding validation
    // =============================================================================
    /// CBKE501: JSON value type doesn't match expected field type
    CBKE501_JSON_TYPE_MISMATCH,
    /// CBKE505: Decimal scale mismatch during field encoding
    CBKE505_SCALE_MISMATCH,
    /// CBKE510: Numeric value overflow for field capacity
    CBKE510_NUMERIC_OVERFLOW,
    /// CBKE515: String length exceeds field size limit
    CBKE515_STRING_LENGTH_VIOLATION,
    /// CBKE521: Array length exceeds ODO bounds
    CBKE521_ARRAY_LEN_OOB,

    // =============================================================================
    // File/Format Errors (CBKF*) - File structure and format validation
    // =============================================================================
    /// CBKF102: RDW length field references incomplete or oversized payload
    CBKF102_RECORD_LENGTH_INVALID,
    /// CBKF104: RDW appears to be corrupted by ASCII conversion
    CBKF104_RDW_SUSPECT_ASCII,
    /// CBKF221: RDW length field indicates underflow condition
    CBKF221_RDW_UNDERFLOW,

    // =============================================================================
    // Audit Errors (CBKA*) - Performance and compliance audit operations
    // =============================================================================
    /// CBKA001: Performance baseline operation error
    CBKA001_BASELINE_ERROR,
}

impl fmt::Display for ErrorCode {
    #[inline]
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
            ErrorCode::CBKC201_JSON_WRITE_ERROR => "CBKC201_JSON_WRITE_ERROR",
            ErrorCode::CBKC301_INVALID_EBCDIC_BYTE => "CBKC301_INVALID_EBCDIC_BYTE",
            ErrorCode::CBKD101_INVALID_FIELD_TYPE => "CBKD101_INVALID_FIELD_TYPE",
            ErrorCode::CBKD301_RECORD_TOO_SHORT => "CBKD301_RECORD_TOO_SHORT",
            ErrorCode::CBKD401_COMP3_INVALID_NIBBLE => "CBKD401_COMP3_INVALID_NIBBLE",
            ErrorCode::CBKD410_ZONED_OVERFLOW => "CBKD410_ZONED_OVERFLOW",
            ErrorCode::CBKD411_ZONED_BAD_SIGN => "CBKD411_ZONED_BAD_SIGN",
            ErrorCode::CBKD412_ZONED_BLANK_IS_ZERO => "CBKD412_ZONED_BLANK_IS_ZERO",
            ErrorCode::CBKD413_ZONED_INVALID_ENCODING => "CBKD413_ZONED_INVALID_ENCODING",
            ErrorCode::CBKD414_ZONED_MIXED_ENCODING => "CBKD414_ZONED_MIXED_ENCODING",
            ErrorCode::CBKD415_ZONED_ENCODING_AMBIGUOUS => "CBKD415_ZONED_ENCODING_AMBIGUOUS",
            ErrorCode::CBKI001_INVALID_STATE => "CBKI001_INVALID_STATE",
            ErrorCode::CBKE501_JSON_TYPE_MISMATCH => "CBKE501_JSON_TYPE_MISMATCH",
            ErrorCode::CBKE505_SCALE_MISMATCH => "CBKE505_SCALE_MISMATCH",
            ErrorCode::CBKE510_NUMERIC_OVERFLOW => "CBKE510_NUMERIC_OVERFLOW",
            ErrorCode::CBKE515_STRING_LENGTH_VIOLATION => "CBKE515_STRING_LENGTH_VIOLATION",
            ErrorCode::CBKE521_ARRAY_LEN_OOB => "CBKE521_ARRAY_LEN_OOB",
            ErrorCode::CBKF102_RECORD_LENGTH_INVALID => "CBKF102_RECORD_LENGTH_INVALID",
            ErrorCode::CBKF104_RDW_SUSPECT_ASCII => "CBKF104_RDW_SUSPECT_ASCII",
            ErrorCode::CBKF221_RDW_UNDERFLOW => "CBKF221_RDW_UNDERFLOW",
            ErrorCode::CBKA001_BASELINE_ERROR => "CBKA001_BASELINE_ERROR",
        };
        write!(f, "{code_str}")
    }
}

impl ErrorCode {
    /// Return the 4-character family prefix (e.g., `CBKD`) for this error code.
    #[inline]
    #[must_use]
    pub const fn family_prefix(self) -> &'static str {
        match self {
            Self::CBKP001_SYNTAX
            | Self::CBKP011_UNSUPPORTED_CLAUSE
            | Self::CBKP021_ODO_NOT_TAIL
            | Self::CBKP051_UNSUPPORTED_EDITED_PIC => "CBKP",
            Self::CBKS121_COUNTER_NOT_FOUND
            | Self::CBKS141_RECORD_TOO_LARGE
            | Self::CBKS301_ODO_CLIPPED
            | Self::CBKS302_ODO_RAISED => "CBKS",
            Self::CBKR211_RDW_RESERVED_NONZERO => "CBKR",
            Self::CBKC201_JSON_WRITE_ERROR | Self::CBKC301_INVALID_EBCDIC_BYTE => "CBKC",
            Self::CBKD101_INVALID_FIELD_TYPE
            | Self::CBKD301_RECORD_TOO_SHORT
            | Self::CBKD401_COMP3_INVALID_NIBBLE
            | Self::CBKD410_ZONED_OVERFLOW
            | Self::CBKD411_ZONED_BAD_SIGN
            | Self::CBKD412_ZONED_BLANK_IS_ZERO
            | Self::CBKD413_ZONED_INVALID_ENCODING
            | Self::CBKD414_ZONED_MIXED_ENCODING
            | Self::CBKD415_ZONED_ENCODING_AMBIGUOUS => "CBKD",
            Self::CBKI001_INVALID_STATE => "CBKI",
            Self::CBKE501_JSON_TYPE_MISMATCH
            | Self::CBKE505_SCALE_MISMATCH
            | Self::CBKE510_NUMERIC_OVERFLOW
            | Self::CBKE515_STRING_LENGTH_VIOLATION
            | Self::CBKE521_ARRAY_LEN_OOB => "CBKE",
            Self::CBKF102_RECORD_LENGTH_INVALID
            | Self::CBKF104_RDW_SUSPECT_ASCII
            | Self::CBKF221_RDW_UNDERFLOW => "CBKF",
            Self::CBKA001_BASELINE_ERROR => "CBKA",
        }
    }
}

/// Context information for detailed error reporting
///
/// Provides comprehensive location and contextual information for errors,
/// enabling precise error reporting and debugging in enterprise environments.
/// All fields are optional to accommodate different error scenarios.
#[derive(Debug, Clone, PartialEq)]
pub struct ErrorContext {
    /// Record number (1-based) where the error occurred
    ///
    /// Used for data processing errors to identify the specific record
    /// in multi-record files or streams.
    pub record_index: Option<u64>,

    /// Hierarchical field path where the error occurred
    ///
    /// Uses dot notation (e.g., "customer.address.street") to identify
    /// the exact field location within nested structures.
    pub field_path: Option<String>,

    /// Byte offset within the record or file where the error occurred
    ///
    /// Provides precise location information for debugging binary data issues.
    pub byte_offset: Option<u64>,

    /// Line number in the copybook source (for parse errors)
    ///
    /// Used during copybook parsing to identify problematic COBOL syntax.
    pub line_number: Option<u32>,

    /// Additional context-specific information
    ///
    /// Free-form text providing extra details relevant to the specific error.
    pub details: Option<String>,
}

impl fmt::Display for ErrorContext {
    #[inline]
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
    /// Create a new error with the specified code and message
    ///
    /// This is the primary constructor for copybook-rs errors. The error code
    /// should be chosen from the stable `ErrorCode` taxonomy to enable
    /// programmatic error handling.
    ///
    /// # Arguments
    /// * `code` - Stable error code from the copybook-rs taxonomy
    /// * `message` - Human-readable error description
    ///
    /// # Example
    /// ```rust
    /// use copybook_core::{Error, ErrorCode};
    ///
    /// let error = Error::new(
    ///     ErrorCode::CBKD411_ZONED_BAD_SIGN,
    ///     "Invalid sign zone 0xA in zoned decimal field"
    /// );
    /// ```
    #[inline]
    pub fn new(code: ErrorCode, message: impl Into<String>) -> Self {
        Self {
            code,
            message: message.into(),
            context: None,
        }
    }

    /// Add context information to the error
    #[must_use]
    #[inline]
    pub fn with_context(mut self, context: ErrorContext) -> Self {
        self.context = Some(context);
        self
    }

    /// Add record context to the error
    #[must_use]
    #[inline]
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
    #[inline]
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
    #[inline]
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
