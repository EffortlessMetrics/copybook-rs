//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

#![allow(clippy::missing_inline_in_public_items)]
#![allow(clippy::redundant_else)]
#![allow(clippy::doc_markdown)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::cast_lossless)]
#![allow(clippy::cast_possible_wrap)]
#![allow(clippy::cast_possible_truncation)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::missing_const_for_thread_local)]
#![allow(clippy::unreachable)]
#![allow(clippy::single_char_add_str)]
#![allow(clippy::inline_always)]
#![allow(clippy::unwrap_used)]
#![allow(clippy::explicit_iter_loop)]
#![allow(clippy::cast_precision_loss)]
#![allow(clippy::single_match_else)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::cast_sign_loss)]
#![allow(clippy::match_same_arms)]
#![allow(clippy::comparison_chain)]

// Only include working modules for task 9.1
pub mod charset;
pub mod fidelity;
pub mod lib_api;
pub mod memory;
pub mod numeric;
pub mod options;
pub mod record;
pub mod zoned_overpunch;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};

// Export numeric types for advanced zoned decimal processing
pub use numeric::ZonedEncodingInfo;

// Export the core library API functions (task 9.1)
pub use lib_api::{
    RecordIterator, RunSummary, decode_file_to_jsonl, decode_record, decode_record_with_scratch,
    encode_jsonl_to_file, encode_record, iter_records, iter_records_from_file,
};

// DecodeProcessor not needed - benchmarks use decode_file_to_jsonl instead

// RunSummary is now exported from lib_api module
