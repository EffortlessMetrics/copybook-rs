//! Demonstration of the configuration and options API
//! 
//! This example shows how to use the DecodeOptions, EncodeOptions, and RunSummary
//! types to configure and monitor copybook processing operations.

use copybook_codec::{
    DecodeOptions, EncodeOptions, RunSummary,
    Codepage, RecordFormat, JsonNumberMode, RawMode, UnmappablePolicy,
};

fn main() {
    println!("copybook-rs Configuration and Options Demo");
    println!("==========================================");

    // Demonstrate DecodeOptions with builder pattern
    let decode_opts = DecodeOptions::new()
        .with_format(RecordFormat::RDW)
        .with_codepage(Codepage::CP037)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_emit_filler(true)
        .with_emit_meta(true)
        .with_emit_raw(RawMode::Record)
        .with_strict_mode(false)
        .with_max_errors(Some(100))
        .with_unmappable_policy(UnmappablePolicy::Replace)
        .with_threads(4);

    println!("\nDecodeOptions Configuration:");
    println!("  Format: {} ({})", decode_opts.format, decode_opts.format.description());
    println!("  Codepage: {} ({})", decode_opts.codepage, decode_opts.codepage.description());
    println!("  JSON Number Mode: {} ({})", decode_opts.json_number_mode, decode_opts.json_number_mode.description());
    println!("  Emit Filler: {}", decode_opts.emit_filler);
    println!("  Emit Meta: {}", decode_opts.emit_meta);
    println!("  Emit Raw: {}", decode_opts.emit_raw);
    println!("  Strict Mode: {}", decode_opts.strict_mode);
    println!("  Max Errors: {:?}", decode_opts.max_errors);
    println!("  Unmappable Policy: {}", decode_opts.on_decode_unmappable);
    println!("  Threads: {}", decode_opts.threads);

    // Demonstrate EncodeOptions with builder pattern
    let encode_opts = EncodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::CP1140)
        .with_use_raw(true)
        .with_bwz_encode(true)
        .with_strict_mode(true)
        .with_max_errors(Some(50))
        .with_threads(2);

    println!("\nEncodeOptions Configuration:");
    println!("  Format: {} ({})", encode_opts.format, encode_opts.format.description());
    println!("  Codepage: {} ({})", encode_opts.codepage, encode_opts.codepage.description());
    println!("  Use Raw: {}", encode_opts.use_raw);
    println!("  BWZ Encode: {}", encode_opts.bwz_encode);
    println!("  Strict Mode: {}", encode_opts.strict_mode);
    println!("  Max Errors: {:?}", encode_opts.max_errors);
    println!("  Threads: {}", encode_opts.threads);

    // Demonstrate RunSummary
    let mut summary = RunSummary::with_threads(4);
    summary.records_processed = 10000;
    summary.records_with_errors = 25;
    summary.warnings = 150;
    summary.processing_time_ms = 5000;
    summary.bytes_processed = 50 * 1024 * 1024; // 50 MB
    summary.set_schema_fingerprint("sha256:abc123def456...".to_string());
    summary.set_peak_memory_bytes(128 * 1024 * 1024); // 128 MB
    summary.calculate_throughput();

    println!("\nRunSummary Statistics:");
    println!("  Records processed: {}", summary.records_processed);
    println!("  Records with errors: {}", summary.records_with_errors);
    println!("  Total records: {}", summary.total_records());
    println!("  Success rate: {:.1}%", summary.success_rate());
    println!("  Error rate: {:.1}%", summary.error_rate());
    println!("  Warnings: {}", summary.warnings);
    println!("  Processing time: {:.2}s", summary.processing_time_seconds());
    println!("  Bytes processed: {:.2} MB", summary.bytes_processed_mb());
    println!("  Throughput: {:.2} MB/s", summary.throughput_mbps);
    println!("  Threads used: {}", summary.threads_used);
    println!("  Peak memory: {:.2} MB", summary.peak_memory_bytes.unwrap() as f64 / (1024.0 * 1024.0));
    println!("  Schema fingerprint: {}", summary.schema_fingerprint);
    println!("  Has errors: {}", summary.has_errors());
    println!("  Has warnings: {}", summary.has_warnings());
    println!("  Is successful: {}", summary.is_successful());

    // Demonstrate enum utility methods
    println!("\nEnum Utility Methods:");
    println!("  RecordFormat::Fixed.is_fixed(): {}", RecordFormat::Fixed.is_fixed());
    println!("  RecordFormat::RDW.is_variable(): {}", RecordFormat::RDW.is_variable());
    println!("  Codepage::ASCII.is_ascii(): {}", Codepage::ASCII.is_ascii());
    println!("  Codepage::CP037.is_ebcdic(): {}", Codepage::CP037.is_ebcdic());
    println!("  Codepage::CP1140.code_page_number(): {:?}", Codepage::CP1140.code_page_number());
    println!("  JsonNumberMode::Lossless.is_lossless(): {}", JsonNumberMode::Lossless.is_lossless());
    println!("  JsonNumberMode::Native.is_native(): {}", JsonNumberMode::Native.is_native());

    // Demonstrate Display formatting
    println!("\nFormatted Summary:");
    println!("{}", summary);
}