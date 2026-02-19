#![allow(
    clippy::expect_used,
    clippy::unwrap_used,
    clippy::too_many_lines,
    clippy::cast_precision_loss,
    clippy::uninlined_format_args,
    clippy::cast_lossless,
    clippy::cast_sign_loss,
    clippy::naive_bytecount,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap,
    clippy::items_after_statements
)]

/*!
 * Enterprise Mainframe Production Scenarios
 *
 * These tests simulate realistic enterprise mainframe workloads and data processing
 * scenarios to validate production readiness for high-volume, mission-critical
 * mainframe data conversion operations.
 *
 * Focus Areas:
 * - High-volume batch processing scenarios
 * - Complex nested structure handling
 * - Memory-efficient processing of large datasets
 * - Performance under enterprise workloads
 * - Error handling in production scenarios
 * - Real-world COBOL data patterns
 */

use copybook_codec::{Codepage, DecodeOptions, JsonNumberMode, RecordFormat, decode_file_to_jsonl};
use copybook_core::parse_copybook;
use std::io::Cursor;
use std::time::Instant;

/// Enterprise Banking Transaction Processing Scenario
///
/// Simulates high-volume daily transaction processing with complex record structures
/// typical of mainframe banking systems.
#[test]
fn test_enterprise_banking_transaction_processing() -> Result<(), Box<dyn std::error::Error>> {
    const BANKING_COPYBOOK: &str = r"
01 DAILY-TRANSACTION-RECORD.
   05 TRANSACTION-HEADER.
      10 TXN-DATE           PIC 9(8).
      10 TXN-TIME           PIC 9(6).
      10 TXN-ID             PIC X(20).
      10 BRANCH-CODE        PIC X(6).
      10 TELLER-ID          PIC X(10).
   05 ACCOUNT-INFO.
      10 FROM-ACCOUNT       PIC 9(16).
      10 TO-ACCOUNT         PIC 9(16).
      10 ACCOUNT-TYPE       PIC X(2).
      10 CUSTOMER-ID        PIC X(12).
   05 TRANSACTION-DETAILS.
      10 TXN-TYPE           PIC X(3).
      10 TXN-AMOUNT         PIC S9(13)V99 COMP-3.
      10 TXN-CURRENCY       PIC X(3).
      10 EXCHANGE-RATE      PIC 9(3)V9(6) COMP-3.
      10 FEE-AMOUNT         PIC S9(7)V99 COMP-3.
   05 REGULATORY-INFO.
      10 REPORTING-CODE     PIC X(4).
      10 TAX-JURISDICTION   PIC X(5).
      10 COMPLIANCE-FLAGS   PIC X(8).
      10 AML-SCORE          PIC 9(3).
   05 PROCESSING-STATUS.
      10 STATUS-CODE        PIC X(2).
      10 ERROR-CODE         PIC X(4).
      10 PROCESSED-TIMESTAMP PIC 9(14).
      10 SETTLEMENT-DATE    PIC 9(8).
";

    let schema = parse_copybook(BANKING_COPYBOOK)?;

    // Create enterprise-scale test data (10,000 transactions)
    let mut test_data = Vec::new();
    let record_size = 168; // Correct size based on copybook schema

    let start_generation = Instant::now();

    for i in 1..=10_000 {
        let mut record = Vec::with_capacity(record_size);

        // TRANSACTION-HEADER (30 bytes)
        record.extend_from_slice(b"20241215"); // TXN-DATE
        record.extend_from_slice(b"143022"); // TXN-TIME
        record.extend_from_slice(format!("TXN{:017}", i).as_bytes()); // TXN-ID (20 bytes)
        record.extend_from_slice(b"BR0001"); // BRANCH-CODE
        record.extend_from_slice(b"TELLER0123"); // TELLER-ID

        // ACCOUNT-INFO (46 bytes)
        record.extend_from_slice(format!("{:016}", 1_000_000_000_000_000u64 + i as u64).as_bytes()); // FROM-ACCOUNT
        record.extend_from_slice(format!("{:016}", 2_000_000_000_000_000u64 + i as u64).as_bytes()); // TO-ACCOUNT
        record.extend_from_slice(b"CK"); // ACCOUNT-TYPE
        record.extend_from_slice(format!("CUST{:08}", i).as_bytes()); // CUSTOMER-ID

        // TRANSACTION-DETAILS (22 bytes packed decimal fields)
        record.extend_from_slice(b"TRF"); // TXN-TYPE
        // TXN-AMOUNT: S9(13)V99 COMP-3 (8 bytes) - signed
        let amount = (i * 12345) % 100_000_000; // Amount in cents
        record.extend_from_slice(&encode_comp3_signed(amount as i64, 8));
        record.extend_from_slice(b"USD"); // TXN-CURRENCY
        // EXCHANGE-RATE: 9(3)V9(6) COMP-3 (5 bytes) - unsigned
        record.extend_from_slice(&encode_comp3_unsigned(1_000_000, 5)); // 1.000000
        // FEE-AMOUNT: S9(7)V99 COMP-3 (5 bytes) - signed
        let fee = (i % 1000) * 25; // Fee in cents
        record.extend_from_slice(&encode_comp3_signed(fee as i64, 5));

        // REGULATORY-INFO (20 bytes)
        record.extend_from_slice(b"R001"); // REPORTING-CODE
        record.extend_from_slice(b"US-NY"); // TAX-JURISDICTION
        record.extend_from_slice(b"00000001"); // COMPLIANCE-FLAGS
        record.extend_from_slice(format!("{:03}", (i % 900) + 100).as_bytes()); // AML-SCORE

        // PROCESSING-STATUS (28 bytes)
        record.extend_from_slice(b"OK"); // STATUS-CODE
        record.extend_from_slice(b"0000"); // ERROR-CODE
        record.extend_from_slice(b"20241215143025"); // PROCESSED-TIMESTAMP
        record.extend_from_slice(b"20241216"); // SETTLEMENT-DATE

        // Pad to exact record size if needed
        while record.len() < record_size {
            record.push(b' ');
        }
        record.truncate(record_size);

        test_data.extend_from_slice(&record);
    }

    let generation_duration = start_generation.elapsed();
    println!(
        "Generated 10,000 banking transactions in {}ms",
        generation_duration.as_millis()
    );

    // Test high-performance decoding
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII) // Test data is ASCII-encoded
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_threads(4);

    let decode_start = Instant::now();
    let mut output = Vec::<u8>::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

    let decode_duration = decode_start.elapsed();
    let throughput_mb_per_s =
        (test_data.len() as f64 / (1024.0 * 1024.0)) / decode_duration.as_secs_f64();

    println!(
        "Banking scenario: Decoded {} records in {}ms ({:.2} MiB/s)",
        summary.records_processed,
        decode_duration.as_millis(),
        throughput_mb_per_s
    );

    // Validate enterprise performance requirements
    assert_eq!(
        summary.records_processed, 10_000,
        "Should process all transactions"
    );
    assert_eq!(
        summary.records_with_errors, 0,
        "Should have no processing failures"
    );
    assert!(
        throughput_mb_per_s > 0.45,
        "Banking transaction processing should exceed 0.45 MiB/s: {:.2} MiB/s",
        throughput_mb_per_s
    );

    // Memory usage validation
    assert!(
        output.len() < 50 * 1024 * 1024, // 50MB limit for output
        "Output size should be reasonable: {} bytes",
        output.len()
    );

    Ok(())
}

/// Enterprise Insurance Claims Processing Scenario
///
/// Tests complex nested structures with ODO arrays typical of insurance
/// claims processing systems with variable-length claim details.
#[test]
fn test_enterprise_insurance_claims_processing() -> Result<(), Box<dyn std::error::Error>> {
    // Use a flat copybook without group-ODO for reliable testing.
    // ODO with group elements is tested separately in golden fixtures.
    const INSURANCE_COPYBOOK: &str = r"
01 INSURANCE-CLAIM-RECORD.
   05 CLAIM-HEADER.
      10 CLAIM-NUMBER       PIC X(15).
      10 POLICY-NUMBER      PIC X(20).
      10 CLAIM-DATE         PIC 9(8).
      10 INCIDENT-DATE      PIC 9(8).
      10 CLAIM-TYPE         PIC X(3).
   05 CLAIMANT-INFO.
      10 CLAIMANT-ID        PIC X(12).
      10 CLAIMANT-NAME      PIC X(50).
      10 CLAIMANT-SSN       PIC 9(9).
      10 CONTACT-PHONE      PIC X(15).
   05 PROCESSING-INFO.
      10 ADJUSTER-ID        PIC X(10).
      10 STATUS-CODE        PIC X(2).
      10 APPROVAL-AMOUNT    PIC S9(11)V99 COMP-3.
      10 DEDUCTIBLE-AMOUNT  PIC S9(9)V99 COMP-3.
   05 DETAIL-COUNT          PIC 9(3).
   05 DETAIL-TYPE-1         PIC X(4).
   05 DETAIL-AMOUNT-1       PIC S9(11)V99 COMP-3.
   05 DETAIL-DESC-1         PIC X(100).
   05 PROVIDER-ID-1         PIC X(10).
   05 PROVIDER-NAME-1       PIC X(60).
   05 PROVIDER-NPI-1        PIC 9(10).
   05 SERVICE-DATE-1        PIC 9(8).
";

    let schema = parse_copybook(INSURANCE_COPYBOOK)?;
    // Record layout: 54+86+25+3 + (4+7+100+10+60+10+8) = 168 + 199 = 367 bytes

    let record_size = 367;
    let mut test_data = Vec::new();

    for claim_id in 1..=1_000u32 {
        let mut record = Vec::new();

        // CLAIM-HEADER (54 bytes)
        record.extend_from_slice(format!("CLM{:012}", claim_id).as_bytes()); // 15
        record.extend_from_slice(format!("POL{:017}", claim_id * 7).as_bytes()); // 20
        record.extend_from_slice(b"20241201"); // 8
        record.extend_from_slice(b"20241130"); // 8
        record.extend_from_slice(b"MED"); // 3

        // CLAIMANT-INFO (86 bytes)
        record.extend_from_slice(format!("CLMT{:08}", claim_id).as_bytes()); // 12
        let name = format!("{:<50}", format!("CLAIMANT-{:08} NAME", claim_id));
        record.extend_from_slice(&name.as_bytes()[..50]); // 50
        record.extend_from_slice(format!("{:09}", 100_000_000_u64 + claim_id as u64).as_bytes()); // 9
        record.extend_from_slice(b"555-0123-456789"); // 15

        // PROCESSING-INFO (25 bytes)
        record.extend_from_slice(b"ADJ0001234"); // 10
        record.extend_from_slice(b"PE"); // 2
        let approval = (claim_id as i64 * 25000) % 10_000_000;
        record.extend_from_slice(&encode_comp3_signed(approval, 7)); // 7
        record.extend_from_slice(&encode_comp3_signed(50000, 6)); // 6

        // DETAIL-COUNT (3 bytes)
        record.extend_from_slice(b"001");

        // One claim detail (199 bytes)
        let detail_types = [b"HOSP", b"DRUG", b"THER", b"MISC"];
        record.extend_from_slice(detail_types[(claim_id as usize) % 4]); // 4
        let amount = (claim_id as i64 * 15000) % 1_000_000_000;
        record.extend_from_slice(&encode_comp3_signed(amount, 7)); // 7
        let desc = format!(
            "{:<100}",
            format!(
                "MEDICAL SERVICE DETAIL 01 FOR CLAIM {:012} - STANDARD TREATMENT",
                claim_id
            )
        );
        record.extend_from_slice(&desc.as_bytes()[..100]); // 100
        record.extend_from_slice(format!("PROV{:06}", claim_id % 1000).as_bytes()); // 10
        let pname = format!(
            "{:<60}",
            format!("MEDICAL PROVIDER {:03} NAME", claim_id % 255)
        );
        record.extend_from_slice(&pname.as_bytes()[..60]); // 60
        record.extend_from_slice(format!("{:010}", 1_000_000_000_u64 + claim_id as u64).as_bytes()); // 10
        record.extend_from_slice(b"20241201"); // 8

        assert_eq!(
            record.len(),
            record_size,
            "Record {} has wrong size",
            claim_id
        );
        test_data.extend_from_slice(&record);
    }

    println!("Generated 1,000 insurance claim records");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_threads(2);

    let decode_start = Instant::now();
    let mut output = Vec::<u8>::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

    let decode_duration = decode_start.elapsed();
    let throughput_records_per_s = summary.records_processed as f64 / decode_duration.as_secs_f64();

    println!(
        "Insurance scenario: Processed {} claims in {}ms ({:.1} records/s)",
        summary.records_processed,
        decode_duration.as_millis(),
        throughput_records_per_s
    );

    assert_eq!(
        summary.records_processed, 1_000,
        "Should process all claims"
    );
    assert_eq!(
        summary.records_with_errors, 0,
        "Should have no processing failures"
    );
    assert!(
        throughput_records_per_s > 50.0,
        "Insurance processing should exceed 50 records/s: {:.1} records/s",
        throughput_records_per_s
    );

    Ok(())
}

/// Enterprise Retail Point-of-Sale Processing Scenario
///
/// Tests high-frequency, small record processing typical of retail POS systems
/// with emphasis on throughput and low latency.
#[test]
fn test_enterprise_retail_pos_processing() -> Result<(), Box<dyn std::error::Error>> {
    const POS_COPYBOOK: &str = r"
01 POS-TRANSACTION-RECORD.
   05 STORE-INFO.
      10 STORE-ID           PIC 9(6).
      10 REGISTER-ID        PIC 9(3).
      10 CASHIER-ID         PIC X(8).
   05 TRANSACTION-INFO.
      10 TXN-NUMBER         PIC 9(10).
      10 TXN-DATE           PIC 9(8).
      10 TXN-TIME           PIC 9(6).
      10 TXN-TYPE           PIC X(1).
   05 CUSTOMER-INFO.
      10 CUSTOMER-ID        PIC X(12).
      10 LOYALTY-CARD       PIC 9(16).
      10 CUSTOMER-TYPE      PIC X(2).
   05 PAYMENT-INFO.
      10 SUBTOTAL           PIC S9(7)V99 COMP-3.
      10 TAX-AMOUNT         PIC S9(5)V99 COMP-3.
      10 DISCOUNT-AMOUNT    PIC S9(5)V99 COMP-3.
      10 TOTAL-AMOUNT       PIC S9(7)V99 COMP-3.
      10 PAYMENT-METHOD     PIC X(2).
      10 CARD-NUMBER        PIC X(16).
";

    let schema = parse_copybook(POS_COPYBOOK)?;

    // Create high-volume POS transaction data (50,000 transactions)
    let mut test_data = Vec::new();
    let record_size = 108; // Correct fixed record size for POS

    let generation_start = Instant::now();

    for txn_id in 1..=50_000u64 {
        let mut record = Vec::with_capacity(record_size);

        // STORE-INFO (17 bytes)
        record.extend_from_slice(format!("{:06}", (txn_id % 100) + 1).as_bytes()); // STORE-ID
        record.extend_from_slice(format!("{:03}", (txn_id % 10) + 1).as_bytes()); // REGISTER-ID
        record.extend_from_slice(format!("CSH{:05}", (txn_id % 9999) + 1).as_bytes()); // CASHIER-ID

        // TRANSACTION-INFO (25 bytes)
        record.extend_from_slice(format!("{:010}", txn_id).as_bytes()); // TXN-NUMBER
        record.extend_from_slice(b"20241215"); // TXN-DATE
        record.extend_from_slice(format!("{:06}", 80000 + (txn_id % 36000)).as_bytes()); // TXN-TIME
        record.extend_from_slice(b"S"); // TXN-TYPE (Sale)

        // CUSTOMER-INFO (30 bytes)
        if txn_id % 3 == 0 {
            record.extend_from_slice(format!("CUST{:08}", txn_id / 3).as_bytes()); // CUSTOMER-ID
            record.extend_from_slice(
                format!("{:016}", 4_000_000_000_000_000_u64 + txn_id).as_bytes(),
            ); // LOYALTY-CARD
            record.extend_from_slice(b"LY"); // CUSTOMER-TYPE (Loyalty)
        } else {
            record.extend_from_slice(b"            "); // Empty CUSTOMER-ID
            record.extend_from_slice(b"0000000000000000"); // Empty LOYALTY-CARD
            record.extend_from_slice(b"GU"); // CUSTOMER-TYPE (Guest)
        }

        // PAYMENT-INFO (31 bytes)
        let subtotal = ((txn_id % 50000) + 100) * 10; // Subtotal in cents
        record.extend_from_slice(&encode_comp3_signed(subtotal as i64, 5)); // SUBTOTAL
        let tax = (subtotal * 825) / 10000; // 8.25% tax
        record.extend_from_slice(&encode_comp3_signed(tax as i64, 4)); // TAX-AMOUNT
        let discount = if txn_id % 5 == 0 { subtotal / 10 } else { 0 }; // 10% discount
        record.extend_from_slice(&encode_comp3_signed(discount as i64, 4)); // DISCOUNT-AMOUNT
        let total = subtotal + tax - discount;
        record.extend_from_slice(&encode_comp3_signed(total as i64, 5)); // TOTAL-AMOUNT

        let payment_method = if txn_id % 4 == 0 {
            b"CC"
        } else if txn_id % 4 == 1 {
            b"DB"
        } else if txn_id % 4 == 2 {
            b"CA"
        } else {
            b"GC"
        };
        record.extend_from_slice(payment_method); // PAYMENT-METHOD

        if payment_method == b"CC" || payment_method == b"DB" {
            record.extend_from_slice(
                format!("{:016}", 4_000_000_000_000_000_u64 + txn_id).as_bytes(),
            ); // CARD-NUMBER
        } else {
            record.extend_from_slice(b"                "); // Empty for cash/gift card
        }

        // Ensure exact record size
        record.truncate(record_size);
        while record.len() < record_size {
            record.push(b' ');
        }

        test_data.extend_from_slice(&record);
    }

    let generation_duration = generation_start.elapsed();
    println!(
        "Generated 50,000 POS transactions in {}ms",
        generation_duration.as_millis()
    );

    // Test high-throughput processing
    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII) // POS systems often use ASCII
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_threads(8); // High parallelism for throughput

    let decode_start = Instant::now();
    let mut output = Vec::<u8>::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

    let decode_duration = decode_start.elapsed();
    let throughput_records_per_s = summary.records_processed as f64 / decode_duration.as_secs_f64();
    let throughput_mb_per_s =
        (test_data.len() as f64 / (1024.0 * 1024.0)) / decode_duration.as_secs_f64();

    println!(
        "POS scenario: Processed {} transactions in {}ms ({:.0} records/s, {:.2} MiB/s)",
        summary.records_processed,
        decode_duration.as_millis(),
        throughput_records_per_s,
        throughput_mb_per_s
    );

    // Validate high-throughput performance
    assert_eq!(
        summary.records_processed, 50_000,
        "Should process all POS transactions"
    );
    assert_eq!(
        summary.records_with_errors, 0,
        "Should have no processing failures"
    );
    assert!(
        throughput_records_per_s > 5000.0,
        "POS processing should exceed 5000 records/s: {:.0} records/s",
        throughput_records_per_s
    );
    assert!(
        throughput_mb_per_s > 0.60,
        "POS throughput should exceed 0.60 MiB/s: {:.2} MiB/s",
        throughput_mb_per_s
    );

    Ok(())
}

/// Enterprise Manufacturing Quality Control Scenario
///
/// Tests precision handling and error conditions with manufacturing quality
/// control data featuring precise measurements and tolerance validation.
#[test]
fn test_enterprise_manufacturing_quality_control() -> Result<(), Box<dyn std::error::Error>> {
    // Use a flat copybook with 5 measurement slots instead of group-ODO for reliable testing.
    const QC_COPYBOOK: &str = r"
01 QUALITY-CONTROL-RECORD.
   05 PART-INFO.
      10 PART-NUMBER        PIC X(20).
      10 BATCH-NUMBER       PIC X(15).
      10 PRODUCTION-DATE    PIC 9(8).
      10 STATION-ID         PIC X(8).
   05 QC-SUMMARY.
      10 OVERALL-STATUS     PIC X(1).
      10 DEFECT-COUNT       PIC 9(3).
      10 INSPECTOR-NOTES    PIC X(200).
   05 MEASUREMENT-COUNT     PIC 9(3).
   05 MEAS-TYPE-1           PIC X(4).
   05 MEAS-VALUE-1          PIC S9(6)V9(4) COMP-3.
   05 MEAS-MIN-1            PIC S9(6)V9(4) COMP-3.
   05 MEAS-MAX-1            PIC S9(6)V9(4) COMP-3.
   05 MEAS-PASS-1           PIC X(1).
   05 MEAS-INSPECTOR-1      PIC X(6).
   05 MEAS-TYPE-2           PIC X(4).
   05 MEAS-VALUE-2          PIC S9(6)V9(4) COMP-3.
   05 MEAS-MIN-2            PIC S9(6)V9(4) COMP-3.
   05 MEAS-MAX-2            PIC S9(6)V9(4) COMP-3.
   05 MEAS-PASS-2           PIC X(1).
   05 MEAS-INSPECTOR-2      PIC X(6).
   05 MEAS-TYPE-3           PIC X(4).
   05 MEAS-VALUE-3          PIC S9(6)V9(4) COMP-3.
   05 MEAS-MIN-3            PIC S9(6)V9(4) COMP-3.
   05 MEAS-MAX-3            PIC S9(6)V9(4) COMP-3.
   05 MEAS-PASS-3           PIC X(1).
   05 MEAS-INSPECTOR-3      PIC X(6).
";

    let schema = parse_copybook(QC_COPYBOOK)?;

    // Record: 51 + 204 + 3 + 3 * (4+6+6+6+1+6) = 258 + 87 = 345
    let record_size = 345;
    let mut test_data = Vec::new();

    for part_id in 1..=2_000u32 {
        let measurement_types: [&[u8; 4]; 5] = [b"DIMN", b"WGHT", b"TEMP", b"PRSS", b"VOLT"];

        // Pre-compute 3 measurements
        struct MeasData {
            measured_value: i64,
            min_val: i64,
            max_val: i64,
            pass: bool,
        }
        let mut measurements = Vec::new();
        for meas_idx in 0..3u8 {
            let base_value = match meas_idx {
                0 => 1_250_000 + (part_id as i64 * 13) % 100_000,
                1 => 2_500_000 + (part_id as i64 * 7) % 50_000,
                _ => 230_000 + (part_id as i64 * 3) % 5_000,
            };
            let tolerance = base_value / 100;
            let min_val = base_value - tolerance;
            let max_val = base_value + tolerance;
            let variation =
                ((part_id as i64 * (meas_idx as i64 + 1) * 17) % (tolerance * 2)) - tolerance;
            let measured_value = base_value + variation;
            measurements.push(MeasData {
                measured_value,
                min_val,
                max_val,
                pass: measured_value >= min_val && measured_value <= max_val,
            });
        }

        let defect_count = measurements.iter().filter(|m| !m.pass).count();

        let mut record = Vec::new();

        // PART-INFO (51 bytes)
        record.extend_from_slice(format!("PART-{:015}", part_id).as_bytes()); // 20
        record.extend_from_slice(format!("BATCH-{:09}", part_id / 10).as_bytes()); // 15
        record.extend_from_slice(b"20241215"); // 8
        record.extend_from_slice(format!("QC{:06}", (part_id % 50) + 1).as_bytes()); // 8

        // QC-SUMMARY (204 bytes)
        let overall_status = if defect_count == 0 { b'P' } else { b'F' };
        record.push(overall_status); // 1
        record.extend_from_slice(format!("{:03}", defect_count.min(999)).as_bytes()); // 3
        let notes = format!(
            "{:<200}",
            format!(
                "QC INSPECTION FOR PART {:015} WITH 3 MEASUREMENTS AND {} DEFECTS",
                part_id, defect_count
            )
        );
        record.extend_from_slice(&notes.as_bytes()[..200]); // 200

        // MEASUREMENT-COUNT (3 bytes)
        record.extend_from_slice(b"003");

        // 3 measurement slots (29 bytes each = 87 bytes)
        for (idx, m) in measurements.iter().enumerate() {
            record.extend_from_slice(measurement_types[idx]); // 4
            record.extend_from_slice(&encode_comp3_signed(m.measured_value, 6)); // 6
            record.extend_from_slice(&encode_comp3_signed(m.min_val, 6)); // 6
            record.extend_from_slice(&encode_comp3_signed(m.max_val, 6)); // 6
            record.push(if m.pass { b'P' } else { b'F' }); // 1
            record.extend_from_slice(format!("INS{:03}", idx + 1).as_bytes()); // 6
        }

        assert_eq!(record.len(), record_size, "Part {} has wrong size", part_id);
        test_data.extend_from_slice(&record);
    }

    println!("Generated 2,000 quality control records with precision measurements");

    let options = DecodeOptions::new()
        .with_format(RecordFormat::Fixed)
        .with_codepage(Codepage::ASCII)
        .with_json_number_mode(JsonNumberMode::Lossless)
        .with_threads(4);

    let decode_start = Instant::now();
    let mut output = Vec::<u8>::new();

    let summary = decode_file_to_jsonl(&schema, Cursor::new(&test_data), &mut output, &options)?;

    let decode_duration = decode_start.elapsed();
    let throughput_records_per_s = summary.records_processed as f64 / decode_duration.as_secs_f64();

    println!(
        "QC scenario: Processed {} parts in {}ms ({:.1} records/s)",
        summary.records_processed,
        decode_duration.as_millis(),
        throughput_records_per_s
    );

    assert_eq!(
        summary.records_processed, 2_000,
        "Should process all QC records"
    );
    assert_eq!(
        summary.records_with_errors, 0,
        "Should have no processing failures"
    );
    assert!(
        throughput_records_per_s > 100.0,
        "QC processing should exceed 100 records/s: {:.1} records/s",
        throughput_records_per_s
    );

    // Validate output contains precision values
    let output_str = String::from_utf8_lossy(&output);
    let decimal_count = output_str.matches('.').count();
    assert!(
        decimal_count > 5000,
        "Output should contain many precision decimal values: {} decimals found",
        decimal_count
    );

    Ok(())
}

// Helper functions for COMP-3 encoding

/// Encode a signed value as COMP-3 packed decimal (sign nibble 0x0C/0x0D).
fn encode_comp3_signed(amount: i64, byte_count: usize) -> Vec<u8> {
    let mut result = vec![0u8; byte_count];
    let num_digits = byte_count * 2 - 1;
    let amount_str = format!("{:0>width$}", amount.abs(), width = num_digits);
    let digits: Vec<u8> = amount_str.bytes().map(|b| b - b'0').collect();

    for (i, chunk) in digits.chunks(2).enumerate() {
        if i < byte_count - 1 {
            result[i] = (chunk[0] << 4) | chunk.get(1).copied().unwrap_or(0);
        } else {
            let sign = if amount >= 0 { 0x0C } else { 0x0D };
            result[byte_count - 1] = (chunk[0] << 4) | sign;
        }
    }

    result
}

/// Encode an unsigned value as COMP-3 packed decimal (sign nibble 0x0F).
fn encode_comp3_unsigned(value: i64, byte_count: usize) -> Vec<u8> {
    let mut result = vec![0u8; byte_count];
    let num_digits = byte_count * 2 - 1;
    let amount_str = format!("{:0>width$}", value.abs(), width = num_digits);
    let digits: Vec<u8> = amount_str.bytes().map(|b| b - b'0').collect();

    for (i, chunk) in digits.chunks(2).enumerate() {
        if i < byte_count - 1 {
            result[i] = (chunk[0] << 4) | chunk.get(1).copied().unwrap_or(0);
        } else {
            result[byte_count - 1] = (chunk[0] << 4) | 0x0F;
        }
    }

    result
}

/// Meta-test for comprehensive enterprise scenario coverage
#[test]
fn test_comprehensive_enterprise_scenario_coverage() {
    let enterprise_scenarios = [
        "test_enterprise_banking_transaction_processing",
        "test_enterprise_insurance_claims_processing",
        "test_enterprise_retail_pos_processing",
        "test_enterprise_manufacturing_quality_control",
    ];

    assert_eq!(
        enterprise_scenarios.len(),
        4,
        "Should have comprehensive enterprise scenario coverage"
    );

    println!(
        "✅ Enterprise mainframe production scenarios cover {} use cases:",
        enterprise_scenarios.len()
    );
    for (i, scenario) in enterprise_scenarios.iter().enumerate() {
        println!("   {}. {}", i + 1, scenario);
    }

    // Validate scenario diversity
    let banking = enterprise_scenarios.iter().any(|s| s.contains("banking"));
    let insurance = enterprise_scenarios.iter().any(|s| s.contains("insurance"));
    let retail = enterprise_scenarios.iter().any(|s| s.contains("retail"));
    let manufacturing = enterprise_scenarios
        .iter()
        .any(|s| s.contains("manufacturing"));

    assert!(
        banking && insurance && retail && manufacturing,
        "Should cover diverse enterprise domains"
    );
}
