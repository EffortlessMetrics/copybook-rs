//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

// Only include working modules for task 9.1
pub mod charset;
pub mod lib_api;
pub mod memory;
pub mod numeric;
pub mod options;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};

// Export the core library API functions (task 9.1)
pub use lib_api::{
    RecordIterator, RunSummary, decode_file_to_jsonl, decode_record, encode_jsonl_to_file,
    encode_record, iter_records, iter_records_from_file,
};

// Export other modules (commented out due to compilation issues)
/*
pub use memory::{
    DigitBuffer, ScratchBuffers, SequencedRecord, SequenceRing, SequenceRingStats,
    WorkerPool, WorkerPoolStats, StreamingProcessor, StreamingProcessorStats,
};
pub use numeric::{
    SmallDecimal, decode_zoned_decimal, decode_packed_decimal, decode_binary_int,
    encode_zoned_decimal, encode_packed_decimal, encode_binary_int, encode_alphanumeric,
    encode_zoned_decimal_with_bwz, get_binary_width_from_digits, validate_explicit_binary_width,
    should_encode_as_blank_when_zero,
};
pub use charset::{ebcdic_to_utf8, utf8_to_ebcdic, get_zoned_sign_table};
pub use record::{
    read_record, write_record, FixedRecordReader, FixedRecordWriter,
    RDWRecordReader, RDWRecordWriter, RDWRecord
};
pub use json::{JsonWriter, JsonEncoder, OrderedJsonWriter};
pub use roundtrip::{RoundTripConfig, RoundTripResult, RoundTripTestSuite, create_comprehensive_test_suite};
pub use corruption::{detect_rdw_ascii_corruption, detect_ebcdic_corruption, detect_packed_corruption};
pub use processor::{DecodeProcessor, EncodeProcessor};
pub use odo_redefines::{
    OdoValidationResult, RedefinesContext, validate_odo_counter, validate_odo_tail_position,
    build_redefines_context, validate_redefines_encoding, handle_missing_counter_field,
    create_comprehensive_error_context, validate_odo_decode, validate_odo_encode,
};
*/

// RunSummary is now exported from lib_api module
