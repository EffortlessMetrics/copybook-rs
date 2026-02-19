// SPDX-License-Identifier: AGPL-3.0-or-later
//! Enterprise COBOL Data Processing Test Fixtures
//!
//! Realistic COBOL processing scenarios for audit system testing including
//! high-performance throughput validation and compliance data scenarios.

use std::collections::HashMap;

/// Generate high-volume DISPLAY field test data for performance validation
/// Targets 4.1+ GiB/s throughput for enterprise scenarios
pub fn generate_display_performance_data(record_count: usize) -> Vec<u8> {
    let mut data = Vec::new();

    // Enterprise customer record: 850 bytes fixed-length
    // Optimized for DISPLAY field processing performance testing
    let record_template = format!(
        "{:<50}{:<100}{:<100}{:<100}{:<100}{:<50}{:<50}{:<100}{:<100}{:<50}",
        "CUSTOMER", // Customer ID (50 bytes)
        "ENTERPRISE CUSTOMER NAME WITH SUFFICIENT PADDING FOR REALISTIC MAINFRAME PROCESSING SCENARIO TEST", // Name (100 bytes)
        "123 MAIN STREET SUITE 456 COMPREHENSIVE ADDRESS LINE FOR ENTERPRISE TESTING MAINFRAME PATTERNS", // Address 1 (100 bytes)
        "ADDITIONAL ADDRESS INFORMATION FOR COMPREHENSIVE COBOL PROCESSING VALIDATION ENTERPRISE TESTING", // Address 2 (100 bytes)
        "DETAILED CUSTOMER DESCRIPTION FOR COMPREHENSIVE COBOL DISPLAY PROCESSING PERFORMANCE VALIDATION", // Description (100 bytes)
        "PHONE_NUMBER_FIELD_FOR_ENTERPRISE_MAINFRAME_TEST", // Phone (50 bytes)
        "EMAIL@ENTERPRISE.COBOL.PROCESSING.VALIDATION.COM", // Email (50 bytes)
        "EXTENSIVE_NOTES_FIELD_FOR_COMPREHENSIVE_COBOL_PROCESSING_PERFORMANCE_VALIDATION_ENTERPRISE_TEST", // Notes (100 bytes)
        "ACCOUNT_STATUS_INFORMATION_FOR_COMPREHENSIVE_ENTERPRISE_MAINFRAME_COBOL_PROCESSING_VALIDATION", // Status (100 bytes)
        "CATEGORY_CLASSIFICATION_FOR_MAINFRAME_PROCESSING" // Category (50 bytes)
    );

    assert_eq!(record_template.len(), 850, "Record template must be exactly 850 bytes");

    for i in 0..record_count {
        // Vary the data slightly to avoid compression optimization
        let mut record = record_template.clone();
        let customer_id = format!("CUST{:010}", i);
        record.replace_range(0..customer_id.len(), &customer_id);

        data.extend_from_slice(record.as_bytes());
    }

    data
}

/// Generate high-volume COMP-3 packed decimal test data for performance validation
/// Targets 560+ MiB/s throughput for enterprise scenarios
pub fn generate_comp3_performance_data(record_count: usize) -> Vec<u8> {
    let mut data = Vec::new();

    for i in 0..record_count {
        let mut record = Vec::with_capacity(320); // 320 bytes per record

        // Account ID: PIC 9(12) COMP-3 (7 bytes)
        let account_id = 100000000000u64 + (i as u64);
        record.extend_from_slice(&pack_comp3_12_digits(account_id));

        // Transaction Amount: PIC S9(13)V99 COMP-3 (8 bytes)
        let amount = ((i * 12345) % 999999999999999) as i64;
        record.extend_from_slice(&pack_comp3_15_digits_signed(amount));

        // Balance: PIC S9(13)V99 COMP-3 (8 bytes)
        let balance = ((i * 54321 + 1000000) % 999999999999999) as i64;
        record.extend_from_slice(&pack_comp3_15_digits_signed(balance));

        // Interest Rate: PIC S9(3)V9999 COMP-3 (4 bytes)
        let rate = ((i * 7) % 9999999) as i32;
        record.extend_from_slice(&pack_comp3_7_digits_signed(rate));

        // Processing Date: PIC 9(8) COMP-3 (5 bytes)
        let date = 20240925u32 + ((i % 365) as u32);
        record.extend_from_slice(&pack_comp3_8_digits(date));

        // Fill remaining space with additional COMP-3 fields for realistic processing
        for j in 0..45 { // 45 * 6 = 270 more bytes
            let value = ((i * j * 123) % 999999) as u32;
            record.extend_from_slice(&pack_comp3_6_digits(value));
        }

        // Pad to exactly 320 bytes if needed
        while record.len() < 320 {
            record.push(0x0C); // COMP-3 positive sign
        }

        record.truncate(320);
        data.extend_from_slice(&record);
    }

    data
}

/// Generate enterprise healthcare HIPAA test data for compliance validation
pub fn generate_hipaa_healthcare_data() -> Vec<u8> {
    let mut data = Vec::new();

    // Healthcare record with PHI fields: 400 bytes fixed-length
    for i in 0..1000 {
        let mut record = Vec::with_capacity(400);

        // Patient ID: 12 digits COMP-3 (7 bytes)
        let patient_id = 100000000000u64 + (i as u64);
        record.extend_from_slice(&pack_comp3_12_digits(patient_id));

        // SSN Last Four: PIC 9(4) COMP (2 bytes binary)
        let ssn_last_four = ((i % 9999) as u16).to_be_bytes();
        record.extend_from_slice(&ssn_last_four);

        // Medical Record Number: DISPLAY 20 bytes
        let mrn = format!("MRN{:017}", i);
        record.extend_from_slice(mrn.as_bytes());

        // Patient Name: DISPLAY 50 bytes (PHI)
        let name = format!("PATIENT NAME {:037}", i);
        record.extend_from_slice(name.as_bytes());

        // Date of Birth: PIC 9(8) COMP-3 (5 bytes) (PHI)
        let dob = 19800101u32 + ((i % 15000) as u32); // Spread over ~40 years
        record.extend_from_slice(&pack_comp3_8_digits(dob));

        // Diagnosis Code: DISPLAY 10 bytes
        let diagnosis = format!("DX{:08}", i % 100000);
        record.extend_from_slice(diagnosis.as_bytes());

        // Treatment Amount: PIC S9(9)V99 COMP-3 (6 bytes)
        let amount = ((i * 125) % 99999999999) as i64;
        record.extend_from_slice(&pack_comp3_11_digits_signed(amount));

        // Insurance ID: DISPLAY 30 bytes
        let insurance = format!("INS{:027}", i % 1000000);
        record.extend_from_slice(insurance.as_bytes());

        // Provider Code: DISPLAY 15 bytes
        let provider = format!("PROV{:011}", i % 10000);
        record.extend_from_slice(provider.as_bytes());

        // Consent Status: PIC X(1)
        let consent = if i % 5 == 0 { b"N" } else { b"Y" };
        record.extend_from_slice(consent);

        // PHI Category: PIC X(2)
        let phi_category = match i % 4 {
            0 => b"DM", // Demographic
            1 => b"MH", // Medical History
            2 => b"BI", // Billing Info
            _ => b"TX", // Treatment
        };
        record.extend_from_slice(phi_category);

        // Encryption Status: PIC X(1)
        let encryption = if i % 10 == 0 { b"N" } else { b"Y" };
        record.extend_from_slice(encryption);

        // Fill remaining space to reach 400 bytes
        while record.len() < 400 {
            record.push(b' '); // DISPLAY padding
        }

        record.truncate(400);
        data.extend_from_slice(&record);
    }

    data
}

/// Generate SOX financial transaction data for compliance validation
pub fn generate_sox_financial_data() -> Vec<u8> {
    let mut data = Vec::new();

    // Financial transaction record: 300 bytes fixed-length
    for i in 0..5000 {
        let mut record = Vec::with_capacity(300);

        // Transaction ID: 12 digits COMP-3 (7 bytes)
        let txn_id = 100000000000u64 + (i as u64);
        record.extend_from_slice(&pack_comp3_12_digits(txn_id));

        // Account Number: 16 digits COMP-3 (9 bytes)
        let account = 1000000000000000u64 + ((i * 7) as u64);
        record.extend_from_slice(&pack_comp3_16_digits(account));

        // Transaction Amount: PIC S9(13)V99 COMP-3 (8 bytes)
        let amount = ((i * 12345) % 999999999999999) as i64;
        record.extend_from_slice(&pack_comp3_15_digits_signed(amount));

        // Processing Date: PIC 9(8) COMP-3 (5 bytes)
        let date = 20240101u32 + ((i % 365) as u32);
        record.extend_from_slice(&pack_comp3_8_digits(date));

        // Transaction Type: PIC X(3)
        let txn_type = match i % 3 {
            0 => b"DBT",
            1 => b"CRT",
            _ => b"TRF",
        };
        record.extend_from_slice(txn_type);

        // Authorizer ID: DISPLAY 8 bytes
        let authorizer = format!("AUTH{:04}", i % 1000);
        record.extend_from_slice(authorizer.as_bytes());

        // Audit Trail Reference: DISPLAY 32 bytes
        let audit_ref = format!("AUDIT{:027}", i);
        record.extend_from_slice(audit_ref.as_bytes());

        // Material Amount Flag: PIC X(1)
        let material = if amount.abs() > 10000000 { b"Y" } else { b"N" };
        record.extend_from_slice(material);

        // SOX Control Code: DISPLAY 10 bytes
        let sox_control = format!("SOX{:07}", i % 100);
        record.extend_from_slice(sox_control.as_bytes());

        // Fill remaining space to reach 300 bytes
        while record.len() < 300 {
            record.push(b' ');
        }

        record.truncate(300);
        data.extend_from_slice(&record);
    }

    data
}

/// Generate mixed enterprise workload for comprehensive testing
pub fn generate_mixed_enterprise_workload() -> HashMap<String, Vec<u8>> {
    let mut workloads = HashMap::new();

    // High-volume DISPLAY processing (4.1+ GiB/s target)
    workloads.insert(
        "display_heavy_processing".to_string(),
        generate_display_performance_data(100000) // ~85 MB of test data
    );

    // High-volume COMP-3 processing (560+ MiB/s target)
    workloads.insert(
        "comp3_heavy_processing".to_string(),
        generate_comp3_performance_data(50000) // ~16 MB of test data
    );

    // HIPAA compliance validation
    workloads.insert(
        "hipaa_phi_processing".to_string(),
        generate_hipaa_healthcare_data() // ~400 KB of test data
    );

    // SOX financial compliance
    workloads.insert(
        "sox_financial_processing".to_string(),
        generate_sox_financial_data() // ~1.5 MB of test data
    );

    workloads
}

// Helper functions for COMP-3 packed decimal encoding

fn pack_comp3_6_digits(value: u32) -> [u8; 4] {
    let digits = format!("{:06}", value % 1000000);
    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        0x0C, // Positive sign
    ]
}

fn pack_comp3_7_digits_signed(value: i32) -> [u8; 4] {
    let abs_value = value.abs();
    let digits = format!("{:07}", abs_value % 10000000);
    let sign = if value >= 0 { 0x0C } else { 0x0D };

    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | sign,
    ]
}

fn pack_comp3_8_digits(value: u32) -> [u8; 5] {
    let digits = format!("{:08}", value % 100000000);
    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | (digits.as_bytes()[7] - b'0'),
        0x0C, // Positive sign
    ]
}

fn pack_comp3_11_digits_signed(value: i64) -> [u8; 6] {
    let abs_value = value.abs();
    let digits = format!("{:011}", abs_value % 100000000000);
    let sign = if value >= 0 { 0x0C } else { 0x0D };

    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | (digits.as_bytes()[7] - b'0'),
        ((digits.as_bytes()[8] - b'0') << 4) | (digits.as_bytes()[9] - b'0'),
        ((digits.as_bytes()[10] - b'0') << 4) | sign,
    ]
}

fn pack_comp3_12_digits(value: u64) -> [u8; 7] {
    let digits = format!("{:012}", value % 1000000000000);
    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | (digits.as_bytes()[7] - b'0'),
        ((digits.as_bytes()[8] - b'0') << 4) | (digits.as_bytes()[9] - b'0'),
        ((digits.as_bytes()[10] - b'0') << 4) | (digits.as_bytes()[11] - b'0'),
        0x0C, // Positive sign
    ]
}

fn pack_comp3_15_digits_signed(value: i64) -> [u8; 8] {
    let abs_value = value.abs();
    let digits = format!("{:015}", abs_value % 1000000000000000);
    let sign = if value >= 0 { 0x0C } else { 0x0D };

    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | (digits.as_bytes()[7] - b'0'),
        ((digits.as_bytes()[8] - b'0') << 4) | (digits.as_bytes()[9] - b'0'),
        ((digits.as_bytes()[10] - b'0') << 4) | (digits.as_bytes()[11] - b'0'),
        ((digits.as_bytes()[12] - b'0') << 4) | (digits.as_bytes()[13] - b'0'),
        ((digits.as_bytes()[14] - b'0') << 4) | sign,
    ]
}

fn pack_comp3_16_digits(value: u64) -> [u8; 9] {
    let digits = format!("{:016}", value % 10000000000000000);
    [
        ((digits.as_bytes()[0] - b'0') << 4) | (digits.as_bytes()[1] - b'0'),
        ((digits.as_bytes()[2] - b'0') << 4) | (digits.as_bytes()[3] - b'0'),
        ((digits.as_bytes()[4] - b'0') << 4) | (digits.as_bytes()[5] - b'0'),
        ((digits.as_bytes()[6] - b'0') << 4) | (digits.as_bytes()[7] - b'0'),
        ((digits.as_bytes()[8] - b'0') << 4) | (digits.as_bytes()[9] - b'0'),
        ((digits.as_bytes()[10] - b'0') << 4) | (digits.as_bytes()[11] - b'0'),
        ((digits.as_bytes()[12] - b'0') << 4) | (digits.as_bytes()[13] - b'0'),
        ((digits.as_bytes()[14] - b'0') << 4) | (digits.as_bytes()[15] - b'0'),
        0x0C, // Positive sign
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_performance_data_generation() {
        let data = generate_display_performance_data(100);
        assert_eq!(data.len(), 100 * 850); // 100 records * 850 bytes each

        // Verify first record starts with customer ID
        let first_record = String::from_utf8_lossy(&data[0..50]);
        assert!(first_record.starts_with("CUST0000000000"));
    }

    #[test]
    fn test_comp3_performance_data_generation() {
        let data = generate_comp3_performance_data(100);
        assert_eq!(data.len(), 100 * 320); // 100 records * 320 bytes each
    }

    #[test]
    fn test_hipaa_healthcare_data() {
        let data = generate_hipaa_healthcare_data();
        assert_eq!(data.len(), 1000 * 400); // 1000 records * 400 bytes each
    }

    #[test]
    fn test_sox_financial_data() {
        let data = generate_sox_financial_data();
        assert_eq!(data.len(), 5000 * 300); // 5000 records * 300 bytes each
    }

    #[test]
    fn test_mixed_enterprise_workload() {
        let workloads = generate_mixed_enterprise_workload();
        assert!(workloads.contains_key("display_heavy_processing"));
        assert!(workloads.contains_key("comp3_heavy_processing"));
        assert!(workloads.contains_key("hipaa_phi_processing"));
        assert!(workloads.contains_key("sox_financial_processing"));

        // Verify data sizes are reasonable for performance testing
        let display_data = workloads.get("display_heavy_processing").unwrap();
        assert!(display_data.len() > 50_000_000); // > 50 MB for throughput testing
    }

    #[test]
    fn test_comp3_packing() {
        let packed = pack_comp3_6_digits(123456);
        assert_eq!(packed.len(), 4);
        assert_eq!(packed[3] & 0x0F, 0x0C); // Positive sign

        let packed_signed = pack_comp3_7_digits_signed(-1234567);
        assert_eq!(packed_signed[3] & 0x0F, 0x0D); // Negative sign
    }
}