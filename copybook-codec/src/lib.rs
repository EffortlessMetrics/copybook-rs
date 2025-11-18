#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
//! Encoding and decoding codecs for COBOL data types.
//!
//! This crate provides the encoding/decoding logic for COBOL record layouts,
//! including character set conversion, numeric codecs, and record framing.

pub mod charset;
pub mod determinism;
pub mod fidelity;
pub mod iterator;
pub mod lib_api;
pub mod memory;
pub mod numeric;
pub mod options;
pub mod record;
pub mod zoned_overpunch;

/// Stable JSONL schema identifier for decoder output.
pub const JSON_SCHEMA_VERSION: &str = "copybook.v1";

pub use iterator::{RecordIterator, iter_records, iter_records_from_file};

pub use lib_api::{
    RunSummary, decode_file_to_jsonl, decode_record, decode_record_with_scratch,
    encode_jsonl_to_file, encode_record,
};

pub use numeric::{SmallDecimal, ZonedEncodingInfo};

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};

pub use determinism::{
    ByteDiff, DeterminismMode, DeterminismResult, check_decode_determinism,
    check_encode_determinism, check_round_trip_determinism,
};
