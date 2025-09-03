//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides comprehensive encoding/decoding logic for all COBOL data types,
//! including full field-level decoding with support for:
//!

#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::format_push_string)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::match_same_arms)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::collapsible_else_if)]
//! - **Numeric Types**: Zoned decimal (DISPLAY), packed decimal (COMP-3), binary integers (COMP/BINARY)
//! - **Character Conversion**: EBCDIC and ASCII codepage support with proper sign zone handling
//! - **JSON Integration**: Configurable number representation (Lossless strings vs Native JSON numbers)
//! - **Record Framing**: Fixed-length and variable-length (RDW) record processing
//! - **Performance**: Streaming I/O with parallel processing and deterministic output
//!
//! ## Key Features
//!
//! ### Full Numeric Processing
//! - **Zoned Decimals**: EBCDIC sign zones (C=+, D=-) and ASCII sign handling
//! - **Packed Decimals**: Nibble-based BCD with sign in least significant nibble
//! - **Binary Integers**: Big-endian 1-8 byte integers with explicit width support (BINARY(n))
//! - **Precision Preservation**: Lossless decimal handling with configurable JSON representation
//!
//! ### JSON Number Modes
//! - **Lossless Mode**: Decimals as strings to preserve exact precision (default)
//! - **Native Mode**: Use JSON numbers where possible (integers, small decimals)
//!
//! ### Error Handling
//! - **Structured Errors**: CBKD* codes for data decode errors, CBKE* for encode errors
//! - **Context Information**: Field paths, record positions, and data context
//! - **Lenient Processing**: Configurable error limits and warning handling

// Only include working modules for task 9.1
pub mod lib_api;
pub mod options;

// Additional modules required for full decoding implementation
pub mod charset;
pub mod memory;
pub mod numeric;
pub mod record;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};

// Export the core library API functions with comprehensive numeric support
pub use lib_api::{
    RecordIterator, RunSummary, decode_file_to_jsonl, decode_record, encode_jsonl_to_file,
    encode_record, iter_records, iter_records_from_file,
};

// Export numeric processing components
pub use charset::{ebcdic_to_utf8, utf8_to_ebcdic};
pub use numeric::SmallDecimal;

// Export additional modules for benchmarks and full functionality
// DecodeProcessor is not available due to missing dependencies

// RunSummary is now exported from lib_api module
