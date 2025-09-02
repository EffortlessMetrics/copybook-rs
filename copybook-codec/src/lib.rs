//! Encoding and decoding codecs for COBOL data types
//!
//! This crate provides the actual encoding/decoding logic for all COBOL data types,
//! character set conversion, and record framing (fixed/RDW).

pub mod lib_api;
pub mod options;
// Temporarily comment out problematic modules to focus on core decode_record fix
// pub mod numeric;
// pub mod charset;
// pub mod json;

pub use options::{
    Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};

// Export the core library API functions (task 9.1)
pub use lib_api::{
    RecordIterator, RunSummary, decode_file_to_jsonl, decode_record, encode_jsonl_to_file,
    encode_record, iter_records, iter_records_from_file,
};

// pub use numeric::{
//     SmallDecimal, decode_zoned_decimal, decode_packed_decimal, decode_binary_int,
//     encode_zoned_decimal, encode_packed_decimal, encode_binary_int, encode_alphanumeric,
//     encode_zoned_decimal_with_bwz,
// };
// pub use charset::{ebcdic_to_utf8, utf8_to_ebcdic};
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
