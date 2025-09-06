//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

// Enable essential modules for proper functionality
pub mod lib_api;
pub mod options;
pub mod record;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};

// Export the core library API functions (task 9.1)
pub use lib_api::{
    RecordIterator, RunSummary, decode_file_to_jsonl, decode_record, encode_jsonl_to_file,
    encode_record, iter_records, iter_records_from_file,
};

// Export essential modules (will enable when compilation issues are resolved)

// RunSummary is now exported from lib_api module
