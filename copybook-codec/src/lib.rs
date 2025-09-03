//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides comprehensive encoding/decoding logic for all COBOL data types,
//! including full field-level decoding with support for:
//!
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

// Core modules
pub mod lib_api;
pub mod options;
// Temporarily comment out problematic modules to focus on core decode_record fix
// pub mod numeric;
// pub mod charset;
// pub mod json;

// Additional modules required for full decoding implementation
pub mod charset;
pub mod memory;
pub mod numeric;

// Components required for decoding functionality
pub mod charset;
pub mod json;
pub mod numeric;
pub mod memory;
pub mod iterator;

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

// Additional numeric functions can be re-enabled as needed
// pub use numeric::{
//     decode_zoned_decimal, decode_packed_decimal, decode_binary_int,
//     encode_zoned_decimal, encode_packed_decimal, encode_binary_int, encode_alphanumeric,
//     encode_zoned_decimal_with_bwz,
// };
// pub use json::{JsonWriter, JsonEncoder};

// Other modules can be re-enabled as needed
/*
pub use memory::{
    DigitBuffer, ScratchBuffers, SequencedRecord, SequenceRing, SequenceRingStats,
    WorkerPool, WorkerPoolStats, StreamingProcessor, StreamingProcessorStats,
};
pub use record::{
    read_record, write_record, FixedRecordReader, FixedRecordWriter,
    RDWRecordReader, RDWRecordWriter, RDWRecord
};
pub use roundtrip::{RoundTripConfig, RoundTripResult, RoundTripTestSuite, create_comprehensive_test_suite};
pub use corruption::{detect_rdw_ascii_corruption, detect_ebcdic_corruption, detect_packed_corruption};
pub use processor::{DecodeProcessor, EncodeProcessor};
pub use odo_redefines::{
    OdoValidationResult, RedefinesContext, validate_odo_counter, validate_odo_tail_position,
    build_redefines_context, validate_redefines_encoding, handle_missing_counter_field,
    create_comprehensive_error_context, validate_odo_decode, validate_odo_encode,
};
*/
