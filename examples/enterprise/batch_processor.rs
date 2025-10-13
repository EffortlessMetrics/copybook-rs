// Enterprise batch processing example
// High-performance, production-ready COBOL data processing with comprehensive error handling

use copybook_codec::{
    decode_file_to_jsonl, Codepage, DecodeOptions, JsonNumberMode, RecordFormat, UnmappablePolicy,
};
use copybook_core::parse_copybook;
use std::path::Path;
use std::sync::Arc;
use std::time::Instant;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize enterprise logging
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_target(false)
        .init();

    let processor = BatchProcessor::new()?;
    processor.process_batch().await
}

struct BatchProcessor {
    schema: Arc<copybook_core::Schema>,
    options: DecodeOptions,
}

impl BatchProcessor {
    fn new() -> Result<Self, Box<dyn std::error::Error>> {
        // Enterprise COBOL copybook for high-volume transaction processing
        let copybook = r#"
           01 TRANSACTION-RECORD.
              05 TRANSACTION-HEADER.
                 10 TXN-ID            PIC X(16).
                 10 TXN-TYPE          PIC X(4).
                 10 TXN-DATE          PIC 9(8).
                 10 TXN-TIME          PIC 9(6).
                 10 BATCH-ID          PIC X(10).
              05 ACCOUNT-INFO.
                 10 ACCOUNT-NUMBER    PIC 9(12).
                 10 CUSTOMER-ID       PIC 9(10).
                 10 BRANCH-CODE       PIC X(6).
              05 TRANSACTION-DETAILS.
                 10 AMOUNT            PIC S9(13)V99 COMP-3.
                 10 CURRENCY-CODE     PIC X(3).
                 10 REFERENCE-NUMBER  PIC X(20).
                 10 DESCRIPTION       PIC X(50).
              05 AUDIT-TRAIL.
                 10 PROCESSING-DATE   PIC 9(8).
                 10 PROCESSOR-ID      PIC X(8).
                 10 VALIDATION-CODE   PIC X(4).
                 10 ERROR-FLAGS       PIC X(8).
        "#;

        tracing::info!("🏗️  Initializing enterprise batch processor");
        let schema = Arc::new(parse_copybook(copybook)?);

        // Enterprise-grade processing options
        let options = DecodeOptions::new()
            .with_format(RecordFormat::Fixed)
            .with_codepage(Codepage::CP037) // Mainframe EBCDIC
            .with_json_number_mode(JsonNumberMode::Lossless)
            .with_emit_meta(true) // Include metadata for audit
            .with_strict_mode(false) // Handle malformed data gracefully
            .with_max_errors(Some(1000)) // Limit error collection
            .with_unmappable_policy(UnmappablePolicy::Substitute)
            .with_threads(std::thread::available_parallelism()?.get().min(8)); // Optimal threading

        tracing::info!(
            "✅ Schema loaded: {} fields, {} byte records",
            schema.fields.len(),
            schema.lrecl_fixed.unwrap_or(0)
        );

        Ok(Self { schema, options })
    }

    async fn process_batch(&self) -> Result<(), Box<dyn std::error::Error>> {
        let batch_files = self.discover_batch_files().await?;

        if batch_files.is_empty() {
            tracing::warn!("⚠️  No batch files found for processing");
            return Ok(());
        }

        tracing::info!("📁 Discovered {} files for batch processing", batch_files.len());

        let mut total_records = 0u64;
        let mut total_errors = 0u64;
        let batch_start = Instant::now();

        for (file_path, output_path) in batch_files {
            match self.process_single_file(&file_path, &output_path).await {
                Ok(stats) => {
                    total_records += stats.records_processed;
                    total_errors += stats.error_count;
                    tracing::info!(
                        "✅ {}: {} records, {} errors, {:?}",
                        file_path.display(),
                        stats.records_processed,
                        stats.error_count,
                        stats.processing_time
                    );
                }
                Err(e) => {
                    tracing::error!("❌ Failed to process {}: {}", file_path.display(), e);
                    total_errors += 1;
                }
            }
        }

        let batch_duration = batch_start.elapsed();
        let throughput = if batch_duration.as_secs() > 0 {
            total_records / batch_duration.as_secs()
        } else {
            total_records
        };

        tracing::info!(
            "🎯 Batch processing complete: {} records, {} errors, {} records/sec",
            total_records,
            total_errors,
            throughput
        );

        // Enterprise alerting for error rates
        let error_rate = if total_records > 0 {
            (total_errors as f64 / total_records as f64) * 100.0
        } else {
            0.0
        };

        if error_rate > 5.0 {
            tracing::error!("🚨 High error rate detected: {:.2}%", error_rate);
        } else if error_rate > 1.0 {
            tracing::warn!("⚠️  Elevated error rate: {:.2}%", error_rate);
        }

        Ok(())
    }

    async fn discover_batch_files(&self) -> Result<Vec<(std::path::PathBuf, std::path::PathBuf)>, Box<dyn std::error::Error>> {
        // In production, this would scan a designated batch input directory
        // For demo, we'll create a sample file

        let input_dir = Path::new("batch_input");
        let output_dir = Path::new("batch_output");

        // Create directories if they don't exist
        tokio::fs::create_dir_all(input_dir).await?;
        tokio::fs::create_dir_all(output_dir).await?;

        // Create a sample batch file for demonstration
        let sample_file = input_dir.join("transactions_20241201.dat");
        self.create_sample_batch_file(&sample_file).await?;

        let mut files = Vec::new();
        let mut entries = tokio::fs::read_dir(input_dir).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();
            if path.extension().map_or(false, |ext| ext == "dat") {
                if let Some(stem) = path.file_stem() {
                    let output_name = format!("{}.jsonl", stem.to_string_lossy());
                    let output_path = output_dir.join(output_name);
                    files.push((path, output_path));
                } else {
                    tracing::warn!(
                        "Skipping batch input without valid filename stem: {}",
                        path.display()
                    );
                }
            }
        }

        Ok(files)
    }

    async fn process_single_file(
        &self,
        input_path: &Path,
        output_path: &Path,
    ) -> Result<ProcessingStats, Box<dyn std::error::Error>> {
        let start_time = Instant::now();

        let summary = decode_file_to_jsonl(
            &self.schema,
            input_path,
            output_path,
            &self.options,
        )?;

        Ok(ProcessingStats {
            records_processed: summary.records_processed,
            error_count: summary.errors.len() as u64,
            processing_time: start_time.elapsed(),
        })
    }

    async fn create_sample_batch_file(&self, file_path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        // Create sample transaction data
        // In production, this data would come from mainframe systems

        let sample_data = vec![
            // Transaction record with packed decimal amounts
            b"TXN001234567890 PAYM202412011430001BATCH001  123456789012345678901001   USD12345678901234567890SALARY PAYMENT                                   20241201PROC001 VAL001  FLAGS001".to_vec(),
        ];

        let mut file_data = Vec::new();
        for record in &sample_data {
            file_data.extend_from_slice(record);
            // Pad to fixed record length
            while file_data.len() % 200 != 0 {
                file_data.push(b' ');
            }
        }

        tokio::fs::write(file_path, file_data).await?;
        tracing::debug!("📝 Created sample batch file: {}", file_path.display());

        Ok(())
    }
}

#[derive(Debug)]
struct ProcessingStats {
    records_processed: u64,
    error_count: u64,
    processing_time: std::time::Duration,
}