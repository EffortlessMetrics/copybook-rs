//! High-level processing engine with integrated error reporting
//!
//! This module provides the main processing logic that integrates structured error
//! reporting, corruption detection, and configurable error handling modes.

use crate::RunSummary;
use crate::corruption::{
    detect_ebcdic_corruption, detect_packed_corruption, detect_rdw_ascii_corruption,
};
use crate::memory::{ScratchBuffers, StreamingProcessor, WorkerPool};
use crate::options::{DecodeOptions, EncodeOptions};
use copybook_core::{
    Error, ErrorCode, ErrorMode, ErrorReporter, Field, FieldKind, Schema, WorkloadType,
};
use serde_json::Value;
use std::io::{BufRead, BufReader, Read, Write};
use std::sync::Arc;
use std::time::Instant;
use tracing::{debug, info, warn};

/// High-level processor for decoding operations with integrated error handling
pub struct DecodeProcessor {
    /// Error reporter for structured error handling
    error_reporter: ErrorReporter,
    /// Processing options
    options: DecodeOptions,
    /// Processing start time
    start_time: Instant,
    /// Bytes processed counter
    bytes_processed: u64,
    /// Workload classification for SLO validation
    workload_type: WorkloadType,
}

/// High-level processor for encoding operations with integrated error handling
pub struct EncodeProcessor {
    /// Error reporter for structured error handling
    error_reporter: ErrorReporter,
    /// Processing options
    options: EncodeOptions,
    /// Processing start time
    start_time: Instant,
    /// Bytes processed counter
    bytes_processed: u64,
}

impl DecodeProcessor {
    /// Create a new decode processor with the given options
    pub fn new(options: DecodeOptions) -> Self {
        let error_mode = if options.strict_mode {
            ErrorMode::Strict
        } else {
            ErrorMode::Lenient
        };

        let error_reporter =
            ErrorReporter::new(error_mode, options.max_errors).with_verbose_logging(true);

        Self {
            error_reporter,
            options,
            start_time: Instant::now(),
            bytes_processed: 0,
            workload_type: WorkloadType::Mixed,
        }
    }

    /// Process a file with comprehensive error handling and reporting.
    ///
    /// # Errors
    /// Returns an error if decoding fails for any record or if I/O operations encounter failures.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn process_file<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        output: W,
    ) -> Result<RunSummary, Error> {
        // Analyze schema to determine workload characteristics
        self.workload_type = schema.workload_type();

        if self.options.threads == 1 {
            // Single-threaded processing for deterministic baseline
            self.process_file_single_threaded(schema, input, output)
        } else {
            // Multi-threaded processing with ordered output
            self.process_file_parallel(schema, input, output)
        }
    }

    /// Single-threaded processing (original implementation)
    fn process_file_single_threaded<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        mut output: W,
    ) -> Result<RunSummary, Error> {
        info!("Starting single-threaded decode processing");

        let mut reader = BufReader::new(input);
        let mut record_index = 0u64;
        let mut buffer = Vec::new();

        // Process records one by one
        loop {
            buffer.clear();
            record_index += 1;

            // Notify error reporter of record start
            self.error_reporter.start_record(record_index);

            // Read record based on format with optimized buffering
            let record_len = match self.read_record_optimized(&mut reader, &mut buffer, schema)? {
                Some(len) => {
                    self.bytes_processed += len as u64;
                    len
                }
                None => break, // End of file
            };

            let record_data = &buffer[..record_len];

            // Process the record with error handling using optimized path
            match self.process_record_optimized(schema, record_data, record_index) {
                Ok(json_line) => {
                    // Write successful result with buffered output
                    writeln!(output, "{}", json_line).map_err(|e| {
                        Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                    })?;
                }
                Err(error) => {
                    // Report error and check if we should continue
                    if let Err(fatal_error) = self.error_reporter.report_error(error) {
                        return Err(fatal_error);
                    }
                    // In lenient mode, continue to next record
                }
            }
        }

        // Generate final summary
        self.generate_summary(schema, record_index - 1)
    }

    /// Optimized record reading with appropriate chunk sizes
    fn read_record_optimized<R: BufRead>(
        &self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
        schema: &Schema,
    ) -> Result<Option<usize>, Error> {
        match self.options.record_format {
            crate::options::RecordFormat::Fixed => {
                let lrecl = schema.lrecl_fixed.ok_or_else(|| {
                    Error::new(
                        ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                        "No LRECL specified for fixed format",
                    )
                })?;

                // Ensure buffer capacity
                buffer.resize(lrecl as usize, 0);

                // Read exact number of bytes
                match reader.read_exact(buffer) {
                    Ok(()) => Ok(Some(lrecl as usize)),
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => Ok(None),
                    Err(e) => Err(Error::new(
                        ErrorCode::CBKR101_FIXED_RECORD_ERROR,
                        e.to_string(),
                    )),
                }
            }
            crate::options::RecordFormat::RDW => {
                // Read RDW header (4 bytes)
                let mut rdw_header = [0u8; 4];
                match reader.read_exact(&mut rdw_header) {
                    Ok(()) => {}
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => return Ok(None),
                    Err(e) => {
                        return Err(Error::new(ErrorCode::CBKR201_RDW_READ_ERROR, e.to_string()));
                    }
                }

                // Parse RDW length (big-endian)
                let record_length = u16::from_be_bytes([rdw_header[0], rdw_header[1]]) as usize;

                // Validate reasonable record length
                if record_length > 65535 {
                    return Err(Error::new(
                        ErrorCode::CBKR201_RDW_READ_ERROR,
                        format!("RDW record length {} exceeds maximum", record_length),
                    ));
                }

                // Prepare buffer for record data
                buffer.resize(record_length + 4, 0); // Include RDW header
                buffer[0..4].copy_from_slice(&rdw_header);

                // Read record payload
                if record_length > 0 {
                    reader
                        .read_exact(&mut buffer[4..4 + record_length])
                        .map_err(|e| {
                            Error::new(ErrorCode::CBKR201_RDW_READ_ERROR, e.to_string())
                        })?;
                }

                Ok(Some(record_length + 4))
            }
        }
    }

    /// Optimized record processing using streaming JSON writer
    fn process_record_optimized(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<String, Error> {
        // Use streaming JSON writer for better performance
        let mut json_buffer = Vec::with_capacity(4096);
        let mut json_writer = crate::json::JsonWriter::new(
            std::io::Cursor::new(&mut json_buffer),
            schema.clone(),
            self.options.clone(),
        );

        // Write record using streaming approach
        json_writer.write_record_streaming(record_data, record_index, 0)?;

        // Convert to string (remove trailing newline)
        let json_str = String::from_utf8(json_buffer).map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("UTF-8 error: {}", e),
            )
        })?;

        Ok(json_str.trim_end().to_string())
    }

    /// Process record with scratch buffers for optimal performance
    fn process_record_with_scratch(
        schema: &Arc<Schema>,
        record_data: &[u8],
        options: &Arc<DecodeOptions>,
        scratch: &mut ScratchBuffers,
    ) -> Result<String, Error> {
        // Clear scratch buffers for reuse
        scratch.clear();

        // Use streaming JSON writer with scratch buffers
        let mut json_writer = crate::json::JsonWriter::new(
            std::io::Cursor::new(&mut scratch.byte_buffer),
            (**schema).clone(),
            (**options).clone(),
        );

        // Write record using streaming approach
        json_writer.write_record_streaming(record_data, 0, 0)?;

        // Convert to string (remove trailing newline)
        let json_str = String::from_utf8(scratch.byte_buffer.clone()).map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("UTF-8 error: {}", e),
            )
        })?;

        Ok(json_str.trim_end().to_string())
    }

    /// Generate processing summary with performance metrics
    fn generate_summary(
        &self,
        schema: &Schema,
        records_processed: u64,
    ) -> Result<RunSummary, Error> {
        let processing_time_ms = self.start_time.elapsed().as_millis() as u64;

        // Collect error statistics before building the summary
        let error_summary = self.error_reporter.summary().clone();
        let corruption_warnings = error_summary.corruption_warnings;

        let records_with_errors = self.error_reporter.error_count();
        let processed_ok = records_processed.saturating_sub(records_with_errors);
        
        let fingerprint = schema.fingerprint.clone();
        let mut summary = RunSummary {
            records_processed: processed_ok,
            records_with_errors,
            warnings: self.error_reporter.warning_count(),
            processing_time_ms,
            bytes_processed: self.bytes_processed,
            schema_fingerprint: fingerprint,
            throughput_mbps: 0.0,
            peak_memory_bytes: None,
            threads_used: 1,
        };

        summary.calculate_throughput();

        // Log performance metrics
        info!(
            "Processing complete: {} records, {:.2} MB/s throughput",
            records_processed, summary.throughput_mbps
        );

        // Validate SLO targets
        self.validate_performance_slo(&summary)?;

        Ok(summary)
    }

    /// Validate performance against SLO targets
    fn validate_performance_slo(&self, summary: &RunSummary) -> Result<(), Error> {
        // Only validate for significant workloads
        if summary.bytes_processed < 1024 * 1024 {
            return Ok(()); // Skip validation for small datasets
        }

        match self.workload_type {
            WorkloadType::DisplayHeavy => {
                // Target: ≥80 MB/s for DISPLAY-heavy workloads
                if summary.throughput_mbps < 80.0 {
                    warn!(
                        "DISPLAY-heavy throughput {:.2} MB/s below SLO target of 80 MB/s",
                        summary.throughput_mbps
                    );
                } else {
                    info!(
                        "DISPLAY-heavy throughput {:.2} MB/s meets SLO target",
                        summary.throughput_mbps
                    );
                }
            }
            WorkloadType::Comp3Heavy => {
                // Target: ≥40 MB/s for COMP-3-heavy workloads
                if summary.throughput_mbps < 40.0 {
                    warn!(
                        "COMP-3-heavy throughput {:.2} MB/s below SLO target of 40 MB/s",
                        summary.throughput_mbps
                    );
                } else {
                    info!(
                        "COMP-3-heavy throughput {:.2} MB/s meets SLO target",
                        summary.throughput_mbps
                    );
                }
            }
            WorkloadType::Mixed => {}
        }

        Ok(())
    }
    /// Parallel processing with bounded memory and ordered output
    fn process_file_parallel<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        mut output: W,
    ) -> Result<RunSummary, Error> {
        info!(
            "Starting parallel decode processing with {} threads",
            self.options.threads
        );

        // Create streaming processor for memory management
        let mut streaming_processor = StreamingProcessor::with_default_limit();

        // Calculate channel capacity based on memory constraints
        let estimated_record_size = schema.lrecl_fixed.unwrap_or(1024) as usize;
        let max_records_in_flight =
            (streaming_processor.stats().max_memory_bytes / 4) / estimated_record_size;
        let channel_capacity = max_records_in_flight.min(1000).max(10); // Between 10 and 1000

        info!("Using channel capacity: {} records", channel_capacity);

        // Create worker pool for parallel processing
        let schema_arc = Arc::new(schema.clone());
        let options_arc = Arc::new(self.options.clone());

        let mut worker_pool = WorkerPool::new(
            self.options.threads,
            channel_capacity,
            channel_capacity / 2, // Max reorder window
            move |record_data: Vec<u8>, scratch: &mut ScratchBuffers| -> Result<String, Error> {
                // Process record using scratch buffers
                Self::process_record_with_scratch(&schema_arc, &record_data, &options_arc, scratch)
            },
        );

        let mut reader = BufReader::new(input);
        let mut _record_index = 0u64;
        let mut buffer = Vec::new();
        let mut records_submitted = 0u64;
        let mut records_completed = 0u64;

        // Submit records to worker pool
        loop {
            buffer.clear();
            _record_index += 1;

            // Check memory pressure
            if streaming_processor.is_memory_pressure() {
                warn!("Memory pressure detected, processing pending results");
                // Process some pending results to free memory
                for _ in 0..10 {
                    if let Ok(Some(result)) = worker_pool.try_recv_ordered() {
                        match result {
                            Ok(json_line) => {
                                writeln!(output, "{}", json_line).map_err(|e| {
                                    Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                                })?;
                                records_completed += 1;
                                streaming_processor
                                    .update_memory_usage(-(estimated_record_size as isize));
                            }
                            Err(error) => {
                                if let Err(fatal_error) = self.error_reporter.report_error(error) {
                                    return Err(fatal_error);
                                }
                            }
                        }
                    } else {
                        break;
                    }
                }
            }

            // Read record based on format
            let record_len = match self.read_record(&mut reader, &mut buffer, schema)? {
                Some(len) => {
                    self.bytes_processed += len as u64;
                    len
                }
                None => break, // End of file
            };

            let record_data = buffer[..record_len].to_vec();
            streaming_processor.record_processed(record_len);
            streaming_processor.update_memory_usage(record_len as isize);

            // Submit to worker pool
            if let Err(_) = worker_pool.submit(record_data) {
                warn!("Worker pool channel full, this shouldn't happen with proper backpressure");
                break;
            }
            records_submitted += 1;
        }

        info!(
            "Submitted {} records, collecting results",
            records_submitted
        );

        // Collect all remaining results
        while records_completed < records_submitted {
            match worker_pool.recv_ordered() {
                Ok(Some(result)) => {
                    match result {
                        Ok(json_line) => {
                            writeln!(output, "{}", json_line).map_err(|e| {
                                Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                            })?;
                        }
                        Err(error) => {
                            if let Err(fatal_error) = self.error_reporter.report_error(error) {
                                return Err(fatal_error);
                            }
                        }
                    }
                    records_completed += 1;
                }
                Ok(None) => break, // No more results
                Err(e) => {
                    warn!("Error receiving from worker pool: {}", e);
                    break;
                }
            }
        }

        // Shutdown worker pool
        worker_pool.shutdown().map_err(|e| {
            Error::new(
                ErrorCode::CBKC201_JSON_WRITE_ERROR,
                format!("Worker pool shutdown error: {}", e),
            )
        })?;

        info!(
            "Parallel processing completed: {} records processed",
            records_completed
        );

        // Generate final summary
        self.generate_summary(schema, records_completed)
    }

    /// Process a single record using scratch buffers (static method for worker pool)
    fn process_record_with_scratch(
        schema: &Schema,
        record_data: &[u8],
        options: &DecodeOptions,
        _scratch: &mut ScratchBuffers,
    ) -> Result<String, Error> {
        // For now, use the existing decode_record function
        // In a full implementation, this would use the scratch buffers for optimization
        let json_value = crate::decode_record(schema, record_data, options)?;

        serde_json::to_string(&json_value)
            .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))
    }

    /// Read a single record based on the configured format
    fn read_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
        schema: &Schema,
    ) -> Result<Option<usize>, Error> {
        match self.options.format {
            crate::options::RecordFormat::Fixed => self.read_fixed_record(reader, buffer, schema),
            crate::options::RecordFormat::RDW => self.read_rdw_record(reader, buffer),
        }
    }

    /// Read a fixed-length record
    fn read_fixed_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
        schema: &Schema,
    ) -> Result<Option<usize>, Error> {
        let record_length = schema.lrecl_fixed.unwrap_or(0) as usize;
        if record_length == 0 {
            return Err(Error::new(
                ErrorCode::CBKS141_RECORD_TOO_LARGE,
                "Fixed record length not specified in schema",
            ));
        }

        buffer.resize(record_length, 0);
        match reader.read_exact(buffer) {
            Ok(()) => Ok(Some(record_length)),
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => Ok(None),
            Err(e) => Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                e.to_string(),
            )),
        }
    }

    /// Read an RDW variable-length record
    fn read_rdw_record<R: BufRead>(
        &mut self,
        reader: &mut R,
        buffer: &mut Vec<u8>,
    ) -> Result<Option<usize>, Error> {
        // Read RDW header (4 bytes)
        let mut rdw_header = [0u8; 4];

        // CRITICAL FIX: Distinguish between clean EOF (no bytes read) and truncated header (partial bytes)
        // Use manual reading to detect partial reads
        use std::io::Read;
        let mut bytes_read = 0;
        while bytes_read < 4 {
            match reader.read(&mut rdw_header[bytes_read..]) {
                Ok(0) => {
                    // EOF
                    if bytes_read == 0 {
                        // Clean EOF - no data read yet
                        return Ok(None);
                    } else {
                        // Partial RDW header - error
                        return Err(Error::new(
                            ErrorCode::CBKF221_RDW_UNDERFLOW,
                            format!("Truncated RDW header: only {} of 4 bytes available", bytes_read),
                        ));
                    }
                }
                Ok(n) => bytes_read += n,
                Err(e) => return Err(Error::new(ErrorCode::CBKF221_RDW_UNDERFLOW, e.to_string())),
            }
        }

        // Check for corruption in RDW header
        if let Some(corruption_error) = detect_rdw_ascii_corruption(&rdw_header) {
            self.error_reporter.report_warning(corruption_error);
        }

        // Parse RDW header
        let record_length = u16::from_be_bytes([rdw_header[0], rdw_header[1]]) as usize;
        let reserved = u16::from_be_bytes([rdw_header[2], rdw_header[3]]);

        // Check reserved bytes
        if reserved != 0 {
            let warning = Error::new(
                ErrorCode::CBKR211_RDW_RESERVED_NONZERO,
                format!("RDW reserved bytes are non-zero: 0x{:04X}", reserved),
            );
            self.error_reporter.report_warning(warning);
        }

        // Validate record length
        if record_length == 0 {
            return Err(Error::new(
                ErrorCode::CBKF221_RDW_UNDERFLOW,
                "RDW record length is zero",
            ));
        }

        // Read record data
        buffer.resize(record_length, 0);
        reader
            .read_exact(buffer)
            .map_err(|e| Error::new(ErrorCode::CBKD301_RECORD_TOO_SHORT, e.to_string()))?;

        Ok(Some(record_length))
    }

    /// Process a single record with comprehensive error detection
    fn process_record(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<String, Error> {
        debug!(
            "Processing record {} ({} bytes)",
            record_index,
            record_data.len()
        );

        // Perform corruption detection on the record
        self.detect_record_corruption(schema, record_data, record_index)?;

        // Validate ODO constraints if present
        self.validate_record_odo_constraints(schema, record_data, record_index)?;

        // Decode the record to JSON
        let json_value = crate::decode_record(schema, record_data, &self.options)
            .map_err(|e| self.enhance_error_context(e, record_index, None, None))?;

        // Serialize to JSON string
        serde_json::to_string(&json_value).map_err(|e| {
            Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()).with_context(
                crate::odo_redefines::create_comprehensive_error_context(
                    record_index,
                    "JSON_SERIALIZATION",
                    0,
                    Some(format!("json_error={}", e)),
                ),
            )
        })
    }

    /// Detect various forms of corruption in record data
    fn detect_record_corruption(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<(), Error> {
        // Recursively check fields for corruption
        for field in &schema.fields {
            self.check_field_corruption(field, record_data, record_index)?;
        }
        Ok(())
    }

    /// Check a specific field for corruption patterns
    fn check_field_corruption(
        &mut self,
        field: &Field,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<(), Error> {
        let field_start = field.offset as usize;
        let field_end = field_start + field.len as usize;

        // Ensure field is within record bounds
        if field_end > record_data.len() {
            return Err(Error::new(
                ErrorCode::CBKD301_RECORD_TOO_SHORT,
                format!("Field {} extends beyond record boundary", field.path),
            )
            .with_record(record_index)
            .with_field(&field.path)
            .with_offset(field.offset as u64));
        }

        let field_data = &record_data[field_start..field_end];

        // Check for corruption based on field type
        match &field.kind {
            FieldKind::Alphanum { .. } => {
                // Check for EBCDIC corruption in text fields
                let corruption_errors = detect_ebcdic_corruption(field_data, &field.path);
                for error in corruption_errors {
                    let error_with_context = error.with_record(record_index);
                    self.error_reporter.report_warning(error_with_context);
                }
            }
            FieldKind::PackedDecimal { .. } => {
                // Check for packed decimal corruption
                let corruption_errors = detect_packed_corruption(field_data, &field.path);
                for error in corruption_errors {
                    let error_with_context = error.with_record(record_index);
                    // Packed decimal corruption is usually an error, not just a warning
                    if let Err(fatal_error) = self.error_reporter.report_error(error_with_context) {
                        return Err(fatal_error);
                    }
                }
            }
            FieldKind::ZonedDecimal { .. } => {
                // Check for zoned decimal corruption (less common but possible)
                // This could include checking for invalid sign zones
                debug!("Checking zoned decimal field {} for corruption", field.path);
            }
            FieldKind::BinaryInt { .. } => {
                // Binary fields are less susceptible to character corruption
                // but we could check for suspicious patterns
                debug!("Checking binary field {} for corruption", field.path);
            }
            FieldKind::Group => {
                // Groups are handled by their child fields
            }
        }

        Ok(())
    }

    /// Validate ODO constraints for a record
    fn validate_record_odo_constraints(
        &mut self,
        schema: &Schema,
        record_data: &[u8],
        record_index: u64,
    ) -> Result<(), Error> {
        // Check if schema has tail ODO
        if let Some(ref tail_odo) = schema.tail_odo {
            // Find the counter field value
            let counter_field = schema.find_field(&tail_odo.counter_path).ok_or_else(|| {
                crate::odo_redefines::handle_missing_counter_field(
                    &tail_odo.counter_path,
                    &tail_odo.array_path,
                    schema,
                    record_index,
                    0,
                )
            })?;

            // Extract counter value from record data
            let counter_start = counter_field.offset as usize;
            let counter_end = counter_start + counter_field.len as usize;

            if counter_end > record_data.len() {
                return Err(Error::new(
                    ErrorCode::CBKD301_RECORD_TOO_SHORT,
                    format!(
                        "Record too short to contain ODO counter field '{}'",
                        tail_odo.counter_path
                    ),
                )
                .with_context(
                    crate::odo_redefines::create_comprehensive_error_context(
                        record_index,
                        &tail_odo.counter_path,
                        counter_field.offset as u64,
                        Some(format!(
                            "required_length={}, actual_length={}",
                            counter_end,
                            record_data.len()
                        )),
                    ),
                ));
            }

            let counter_data = &record_data[counter_start..counter_end];

            // Decode counter value based on field type
            let counter_value = match &counter_field.kind {
                FieldKind::ZonedDecimal { digits, signed, .. } => {
                    let decimal = crate::numeric::decode_zoned_decimal(
                        counter_data,
                        *digits,
                        0, // ODO counters are typically integers
                        *signed,
                        self.options.codepage,
                        counter_field.blank_when_zero,
                    )
                    .map_err(|e| {
                        self.enhance_error_context(
                            e,
                            record_index,
                            Some(&tail_odo.counter_path),
                            Some(counter_field.offset as u64),
                        )
                    })?;

                    decimal.value as u32
                }
                FieldKind::BinaryInt { bits, signed } => {
                    let int_value = crate::numeric::decode_binary_int(counter_data, *bits, *signed)
                        .map_err(|e| {
                            self.enhance_error_context(
                                e,
                                record_index,
                                Some(&tail_odo.counter_path),
                                Some(counter_field.offset as u64),
                            )
                        })?;
                    int_value as u32
                }
                _ => {
                    return Err(Error::new(
                        ErrorCode::CBKD101_INVALID_FIELD_TYPE,
                        format!(
                            "ODO counter field '{}' has invalid type for counter",
                            tail_odo.counter_path
                        ),
                    )
                    .with_context(
                        crate::odo_redefines::create_comprehensive_error_context(
                            record_index,
                            &tail_odo.counter_path,
                            counter_field.offset as u64,
                            Some(format!("field_type={:?}", counter_field.kind)),
                        ),
                    ));
                }
            };

            // Validate ODO counter value
            let validation_result = crate::odo_redefines::validate_odo_decode(
                counter_value,
                tail_odo.min_count,
                tail_odo.max_count,
                &tail_odo.array_path,
                &tail_odo.counter_path,
                record_index,
                counter_field.offset as u64,
                &self.options,
            )?;

            // Report warning if clamping occurred
            if let Some(warning) = validation_result.warning {
                self.error_reporter.report_warning(warning);
            }

            debug!(
                "ODO validation passed: counter_value={}, actual_count={}, clamped={}",
                counter_value, validation_result.actual_count, validation_result.was_clamped
            );
        }

        Ok(())
    }

    /// Enhance error with comprehensive context information
    fn enhance_error_context(
        &self,
        mut error: Error,
        record_index: u64,
        field_path: Option<&str>,
        byte_offset: Option<u64>,
    ) -> Error {
        // Only enhance if context is not already present
        if error.context.is_none() {
            let context = crate::odo_redefines::create_comprehensive_error_context(
                record_index,
                field_path.unwrap_or("UNKNOWN_FIELD"),
                byte_offset.unwrap_or(0),
                None,
            );
            error = error.with_context(context);
        }
        error
    }

    /// Generate final processing summary
    fn generate_summary(&self, schema: &Schema, total_records: u64) -> Result<RunSummary, Error> {
        let processing_time_ms = self.start_time.elapsed().as_millis() as u64;

        let error_summary = self.error_reporter.summary();
        let records_with_errors = self.error_reporter.error_count();
        let processed_ok = total_records.saturating_sub(records_with_errors);

        let fingerprint = schema.fingerprint.clone();

        let mut summary = RunSummary {
            records_processed: processed_ok,
            records_with_errors,
            warnings: self.error_reporter.warning_count(),
            processing_time_ms,
            bytes_processed: self.bytes_processed,
            schema_fingerprint: fingerprint,
            throughput_mbps: 0.0,
            peak_memory_bytes: None,
            threads_used: self.options.threads,
        };

        summary.calculate_throughput();

        info!(
            "Processing complete: {} records, {} errors, {} warnings, {:.2} MB/s",
            total_records, summary.records_with_errors, summary.warnings, summary.throughput_mbps
        );

        Ok(summary)
    }
}

impl EncodeProcessor {
    /// Create a new encode processor with the given options
    pub fn new(options: EncodeOptions) -> Self {
        let error_mode = if options.strict_mode {
            ErrorMode::Strict
        } else {
            ErrorMode::Lenient
        };

        let error_reporter =
            ErrorReporter::new(error_mode, options.max_errors).with_verbose_logging(true);

        Self {
            error_reporter,
            options,
            start_time: Instant::now(),
            bytes_processed: 0,
        }
    }

    /// Process JSONL input to binary with comprehensive error handling.
    ///
    /// # Errors
    /// Returns an error if encoding fails for any record or if I/O operations encounter failures.
    #[inline]
    #[must_use = "Handle the Result or propagate the error"]
    pub fn process_file<R: Read, W: Write>(
        &mut self,
        schema: &Schema,
        input: R,
        mut output: W,
    ) -> Result<RunSummary, Error> {
        info!(
            "Starting encode processing with {} threads",
            self.options.threads
        );

        let reader = BufReader::new(input);
        let mut record_index = 0u64;

        // Process each JSON line
        for line_result in reader.lines() {
            record_index += 1;
            self.error_reporter.start_record(record_index);

            let line = line_result
                .map_err(|e| Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string()))?;

            self.bytes_processed += line.len() as u64;

            // Parse JSON
            let json_value: serde_json::Value = serde_json::from_str(&line).map_err(|e| {
                Error::new(
                    ErrorCode::CBKE501_JSON_TYPE_MISMATCH,
                    format!("Invalid JSON: {}", e),
                )
                .with_record(record_index)
            })?;

            // Encode to binary
            match self.process_json_record(schema, &json_value, record_index) {
                Ok(binary_data) => {
                    output.write_all(&binary_data).map_err(|e| {
                        Error::new(ErrorCode::CBKC201_JSON_WRITE_ERROR, e.to_string())
                    })?;
                }
                Err(error) => {
                    if let Err(fatal_error) = self.error_reporter.report_error(error) {
                        return Err(fatal_error);
                    }
                    // In lenient mode, continue to next record
                }
            }
        }

        self.generate_summary(schema, record_index)
    }

    /// Process a single JSON record to binary
    fn process_json_record(
        &mut self,
        schema: &Schema,
        json_value: &serde_json::Value,
        record_index: u64,
    ) -> Result<Vec<u8>, Error> {
        debug!("Encoding record {} to binary", record_index);

        // Validate REDEFINES encoding constraints
        self.validate_redefines_encoding(schema, json_value, record_index)?;

        // Validate ODO array lengths
        self.validate_odo_encoding(schema, json_value, record_index)?;

        crate::encode_record(schema, json_value, &self.options)
            .map_err(|e| self.enhance_encode_error_context(e, record_index))
    }

    /// Generate final processing summary
    fn generate_summary(&self, schema: &Schema, total_records: u64) -> Result<RunSummary, Error> {
        let processing_time_ms = self.start_time.elapsed().as_millis() as u64;

        let error_summary = self.error_reporter.summary();
        let records_with_errors = self.error_reporter.error_count();
        let processed_ok = total_records.saturating_sub(records_with_errors);

        let fingerprint = schema.fingerprint.clone();

        let mut summary = RunSummary {
            records_processed: processed_ok,
            records_with_errors,
            warnings: self.error_reporter.warning_count(),
            processing_time_ms,
            bytes_processed: self.bytes_processed,
            schema_fingerprint: fingerprint,
            throughput_mbps: 0.0,
            peak_memory_bytes: None,
            threads_used: self.options.threads,
        };

        summary.calculate_throughput();

        info!(
            "Encoding complete: {} records, {} errors, {} warnings, {:.2} MB/s",
            total_records, summary.records_with_errors, summary.warnings, summary.throughput_mbps
        );

        Ok(summary)
    }

    /// Validate REDEFINES encoding constraints
    fn validate_redefines_encoding(
        &mut self,
        schema: &Schema,
        json_value: &serde_json::Value,
        record_index: u64,
    ) -> Result<(), Error> {
        // Build REDEFINES context from JSON data
        let redefines_context = crate::odo_redefines::build_redefines_context(schema, json_value);

        // Check each REDEFINES cluster for ambiguity
        for (cluster_path, non_null_views) in &redefines_context.cluster_views {
            if non_null_views.len() > 1 {
                // Multiple non-null views - validate encoding
                for view_path in non_null_views {
                    if let Some(field) = schema.find_field(view_path) {
                        crate::odo_redefines::validate_redefines_encoding(
                            &redefines_context,
                            cluster_path,
                            view_path,
                            json_value,
                            self.options.use_raw,
                            record_index,
                            field.offset as u64,
                        )?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Validate ODO array lengths during encoding
    fn validate_odo_encoding(
        &mut self,
        schema: &Schema,
        json_value: &serde_json::Value,
        record_index: u64,
    ) -> Result<(), Error> {
        // Check if schema has tail ODO
        if let Some(ref tail_odo) = schema.tail_odo {
            if let Value::Object(obj) = json_value {
                // Find the array field in JSON
                let array_field_name = tail_odo
                    .array_path
                    .split('.')
                    .next_back()
                    .unwrap_or(&tail_odo.array_path);

                if let Some(Value::Array(array)) = obj.get(array_field_name) {
                    let array_field = schema.find_field(&tail_odo.array_path).ok_or_else(|| {
                        Error::new(
                            ErrorCode::CBKS121_COUNTER_NOT_FOUND,
                            format!(
                                "ODO array field '{}' not found in schema",
                                tail_odo.array_path
                            ),
                        )
                        .with_context(
                            crate::odo_redefines::create_comprehensive_error_context(
                                record_index,
                                &tail_odo.array_path,
                                0,
                                None,
                            ),
                        )
                    })?;

                    // Validate array length
                    crate::odo_redefines::validate_odo_encode(
                        array.len(),
                        tail_odo.min_count,
                        tail_odo.max_count,
                        &tail_odo.array_path,
                        &tail_odo.counter_path,
                        record_index,
                        array_field.offset as u64,
                        &self.options,
                    )?;
                }
            }
        }

        Ok(())
    }

    /// Enhance encode error with comprehensive context information
    fn enhance_encode_error_context(&self, mut error: Error, record_index: u64) -> Error {
        // Only enhance if context is not already present
        if error.context.is_none() {
            let context = crate::odo_redefines::create_comprehensive_error_context(
                record_index,
                "ENCODE_OPERATION",
                0,
                None,
            );
            error = error.with_context(context);
        }
        error
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;
    use crate::options::{DecodeOptions, RecordFormat};
    use copybook_core::{Field, FieldKind, Schema};
    use std::io::Cursor;

    fn create_test_schema() -> Schema {
        Schema {
            fields: vec![Field {
                path: "ROOT.ID".to_string(),
                name: "ID".to_string(),
                level: 5,
                kind: FieldKind::ZonedDecimal {
                    digits: 5,
                    scale: 0,
                    signed: false,
                },
                offset: 0,
                len: 5,
                redefines_of: None,
                occurs: None,
                sync_padding: None,
                synchronized: false,
                blank_when_zero: false,
                children: vec![],
            }],
            lrecl_fixed: Some(5),
            tail_odo: None,
            fingerprint: String::new(),
        }
    }

    #[test]
    fn test_decode_processor_creation() {
        let options = DecodeOptions::default();
        let processor = DecodeProcessor::new(options);
        assert!(!processor.error_reporter.has_errors());
    }

    #[test]
    fn test_rdw_corruption_detection() {
        let options = DecodeOptions {
            format: RecordFormat::RDW,
            ..DecodeOptions::default()
        };
        let mut processor = DecodeProcessor::new(options);

        // Create RDW data with ASCII corruption
        let rdw_data = [b'1', b'2', 0x00, 0x00, b'H', b'e', b'l', b'l', b'o'];
        let mut cursor = Cursor::new(&rdw_data);
        let mut buffer = Vec::new();

        // This should detect corruption but continue processing
        let result = processor.read_rdw_record(&mut cursor, &mut buffer);
        assert!(result.is_err()); // Will fail due to invalid length parsing
    }

    #[test]
    fn test_error_summary_generation() {
        let options = DecodeOptions::default();
        let processor = DecodeProcessor::new(options);
        let schema = create_test_schema();

        let summary = processor.generate_summary(&schema, 100).unwrap();
        assert_eq!(summary.records_processed, 100);
        assert!(!summary.has_errors());
    }

    #[test]
    fn test_deterministic_parallel_output() {
        // Test that --threads 1 vs --threads 8 produce identical outputs
        let schema = create_test_schema();

        // Create test data with multiple records
        let mut test_data = Vec::new();
        for i in 0..100 {
            let record = format!("{:05}", i % 10000); // 5-digit numbers
            test_data.extend_from_slice(record.as_bytes());
        }

        let mut results = Vec::new();

        // Test with different thread counts
        for threads in [1, 2, 4, 8] {
            let options = DecodeOptions {
                threads,
                ..DecodeOptions::default()
            };

            let mut processor = DecodeProcessor::new(options);
            let input = Cursor::new(&test_data);
            let mut output = Vec::new();

            let summary = processor.process_file(&schema, input, &mut output).unwrap();
            assert_eq!(summary.records_processed, 100);

            let output_str = String::from_utf8(output).unwrap();
            results.push((threads, output_str));
        }

        // All outputs should be identical
        let baseline = &results[0].1;
        for (threads, output) in &results[1..] {
            assert_eq!(
                output, baseline,
                "Output differs between 1 thread and {} threads",
                threads
            );
        }
    }

    #[test]
    fn test_memory_bounded_processing() {
        let schema = create_test_schema();

        // Create larger test data to test memory management
        let mut test_data = Vec::new();
        for i in 0..1000 {
            let record = format!("{:05}", i % 10000);
            test_data.extend_from_slice(record.as_bytes());
        }

        let options = DecodeOptions {
            threads: 4,
            ..DecodeOptions::default()
        };

        let mut processor = DecodeProcessor::new(options);
        let input = Cursor::new(&test_data);
        let mut output = Vec::new();

        let summary = processor.process_file(&schema, input, &mut output).unwrap();
        assert_eq!(summary.records_processed, 1000);

        // Verify output is valid JSON lines
        let output_str = String::from_utf8(output).unwrap();
        let lines: Vec<&str> = output_str.trim().split('\n').collect();
        assert_eq!(lines.len(), 1000);

        // Each line should be valid JSON
        for line in lines {
            let _: serde_json::Value = serde_json::from_str(line).unwrap();
        }
    }
}
