#![cfg_attr(not(test), deny(clippy::unwrap_used, clippy::expect_used))]
// SPDX-License-Identifier: AGPL-3.0-or-later
#![cfg_attr(
    test,
    allow(
        clippy::expect_used,
        clippy::unwrap_used,
        clippy::panic,
        clippy::duplicated_attributes
    )
)]
//! Encoding and decoding codecs for COBOL data types.
//!
//! This crate provides the encoding/decoding logic for COBOL record layouts,
//! including character set conversion, numeric codecs, and record framing.

/// Character set conversion between EBCDIC codepages and UTF-8.
pub mod charset;
/// Determinism validation for decode, encode, and round-trip operations.
pub mod determinism;
/// Edited PIC (numeric editing) decode and encode support.
pub mod edited_pic;
mod fidelity;
/// Streaming record iterator for file-level decoding.
pub mod iterator;
/// Core library API: record decode/encode and file-level processing.
pub mod lib_api;
/// Re-export of [`copybook_codec_memory`] for scratch buffers and streaming.
pub use copybook_codec_memory as memory;
pub mod numeric;
/// Configuration types: codepage, record format, JSON modes, raw capture.
pub mod options;
/// Record-level binary decode/encode logic.
pub mod record;
/// Zoned decimal overpunch character handling.
pub mod zoned_overpunch;

mod odo_redefines;

/// Stable JSONL schema identifier for decoder output.
pub const JSON_SCHEMA_VERSION: &str = "copybook.v1";

pub use iterator::{RecordIterator, iter_records, iter_records_from_file};

pub use lib_api::{
    RunSummary, decode_file_to_jsonl, decode_record, decode_record_with_scratch,
    encode_jsonl_to_file, encode_record,
};

pub use numeric::{SmallDecimal, ZonedEncodingInfo};

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    UnmappablePolicy, ZonedEncodingFormat,
};

pub use determinism::{
    ByteDiff, DeterminismMode, DeterminismResult, check_decode_determinism,
    check_encode_determinism, check_round_trip_determinism,
};
