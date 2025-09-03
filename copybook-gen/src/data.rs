//! Synthetic data generation

use crate::GeneratorConfig;
use copybook_core::{Schema, Field, FieldKind, Occurs};
use rand::rngs::StdRng;
use rand::{Rng, SeedableRng};

/// Data generation strategy
#[derive(Debug, Clone, Copy)]
pub enum DataStrategy {
    /// Normal valid data
    Normal,
    /// Edge cases (min/max values, boundary conditions)
    EdgeCases,
    /// Invalid data for negative testing
    Invalid,
    /// Performance test data (optimized patterns)
    Performance,
}

/// Generate synthetic binary data for a schema
#[must_use] pub fn generate_synthetic_data(schema: &Schema, config: &GeneratorConfig) -> Vec<Vec<u8>> {
    generate_data_with_strategy(schema, config, DataStrategy::Normal)
}

/// Generate data with specific strategy
#[must_use] pub fn generate_data_with_strategy(
    schema: &Schema, 
    config: &GeneratorConfig, 
    strategy: DataStrategy
) -> Vec<Vec<u8>> {
    let mut rng = StdRng::seed_from_u64(config.seed);
    let mut records = Vec::new();

    for record_idx in 0..config.record_count {
        let record = match strategy {
            DataStrategy::Normal => generate_normal_record(schema, &mut rng, record_idx),
            DataStrategy::EdgeCases => generate_edge_case_record(schema, &mut rng, record_idx),
            DataStrategy::Invalid => generate_invalid_record(schema, &mut rng, record_idx),
            DataStrategy::Performance => generate_performance_record(schema, &mut rng, record_idx),
        };
        records.push(record);
    }

    records
}

fn generate_normal_record(schema: &Schema, rng: &mut StdRng, record_idx: usize) -> Vec<u8> {
    let record_len = schema.lrecl_fixed.unwrap_or(1000) as usize;
    let mut record = vec![0x40; record_len]; // EBCDIC spaces
    
    // Fill fields with appropriate data
    for field in &schema.fields {
        if field.redefines_of.is_some() {
            continue; // Skip REDEFINES fields - data comes from base field
        }
        
        fill_field_data(&mut record, field, rng, record_idx, false, false);
    }
    
    record
}

fn generate_edge_case_record(schema: &Schema, rng: &mut StdRng, record_idx: usize) -> Vec<u8> {
    let record_len = schema.lrecl_fixed.unwrap_or(1000) as usize;
    let mut record = vec![0x40; record_len]; // EBCDIC spaces
    
    for field in &schema.fields {
        if field.redefines_of.is_some() {
            continue;
        }
        
        fill_field_data(&mut record, field, rng, record_idx, true, false);
    }
    
    record
}

fn generate_invalid_record(schema: &Schema, rng: &mut StdRng, record_idx: usize) -> Vec<u8> {
    let record_len = schema.lrecl_fixed.unwrap_or(1000) as usize;
    let mut record = vec![0x40; record_len]; // EBCDIC spaces
    
    for field in &schema.fields {
        if field.redefines_of.is_some() {
            continue;
        }
        
        fill_field_data(&mut record, field, rng, record_idx, false, true);
    }
    
    record
}

fn generate_performance_record(schema: &Schema, _rng: &mut StdRng, record_idx: usize) -> Vec<u8> {
    let record_len = schema.lrecl_fixed.unwrap_or(1000) as usize;
    let mut record = vec![0x40; record_len]; // EBCDIC spaces
    
    // Use predictable patterns for performance testing
    for field in &schema.fields {
        if field.redefines_of.is_some() {
            continue;
        }
        
        fill_performance_field_data(&mut record, field, record_idx);
    }
    
    record
}

fn fill_field_data(
    record: &mut [u8], 
    field: &Field, 
    rng: &mut StdRng, 
    record_idx: usize,
    edge_cases: bool,
    invalid: bool
) {
    let start = field.offset as usize;
    let end = start + field.len as usize;
    
    if end > record.len() {
        return; // Field extends beyond record
    }
    
    match &field.kind {
        FieldKind::Alphanum { len } => {
            fill_alphanum_field(&mut record[start..end], *len, rng, edge_cases, invalid);
        }
        FieldKind::ZonedDecimal { digits, scale, signed } => {
            fill_zoned_field(&mut record[start..end], *digits, *scale, *signed, rng, edge_cases, invalid);
        }
        FieldKind::PackedDecimal { digits, scale, signed } => {
            fill_packed_field(&mut record[start..end], *digits, *scale, *signed, rng, edge_cases, invalid);
        }
        FieldKind::BinaryInt { bits, signed } => {
            fill_binary_field(&mut record[start..end], *bits, *signed, rng, edge_cases, invalid);
        }
        FieldKind::Group => {
            // Groups are filled by their child fields
        }
    }
    
    // Handle OCCURS
    if let Some(occurs) = &field.occurs {
        match occurs {
            Occurs::Fixed { count } => {
                // Fixed arrays are handled by the schema layout
            }
            Occurs::ODO { min, max, counter_path } => {
                // For ODO, we need to set the counter field
                if let Some(counter_field) = find_field_by_path(field, counter_path) {
                    let actual_count = if edge_cases {
                        if rng.gen_bool(0.5) { *min } else { *max }
                    } else {
                        rng.gen_range(*min..=*max)
                    };
                    
                    // Set counter field value
                    set_counter_field_value(record, counter_field, actual_count);
                }
            }
        }
    }
}

fn fill_alphanum_field(data: &mut [u8], _len: u32, rng: &mut StdRng, edge_cases: bool, invalid: bool) {
    if invalid && rng.gen_bool(0.3) {
        // Invalid: use control characters or invalid EBCDIC
        for byte in data.iter_mut() {
            *byte = rng.gen_range(0x00..=0x1F);
        }
        return;
    }
    
    if edge_cases && rng.gen_bool(0.3) {
        // Edge case: all spaces (already initialized)
        return;
    }
    
    // Generate random text
    let chars = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ";
    for byte in data.iter_mut() {
        if rng.gen_bool(0.8) {
            // Convert ASCII to EBCDIC approximation
            let ascii_char = chars[rng.gen_range(0..chars.len())];
            *byte = ascii_to_ebcdic_approx(ascii_char);
        }
        // else leave as space (0x40)
    }
}

fn fill_zoned_field(
    data: &mut [u8], 
    digits: u16, 
    _scale: i16, 
    signed: bool, 
    rng: &mut StdRng, 
    edge_cases: bool, 
    invalid: bool
) {
    if invalid && rng.gen_bool(0.3) {
        // Invalid: bad zone nibbles
        for byte in data.iter_mut() {
            *byte = rng.gen_range(0x00..=0x9F); // Invalid zones
        }
        return;
    }
    
    if edge_cases && rng.gen_bool(0.2) {
        // BLANK WHEN ZERO case - all spaces
        for byte in data.iter_mut() {
            *byte = 0x40; // EBCDIC space
        }
        return;
    }
    
    // Generate a valid number
    let is_negative = signed && rng.gen_bool(0.3);
    let max_value = 10_u64.pow(u32::from(digits)) - 1;
    
    let value = if edge_cases && rng.gen_bool(0.3) {
        if rng.gen_bool(0.5) { 0 } else { max_value }
    } else {
        rng.gen_range(0..=max_value)
    };
    
    // Format as zoned decimal
    let value_str = format!("{:0width$}", value, width = digits as usize);
    
    for (i, digit_char) in value_str.chars().enumerate() {
        if i >= data.len() {
            break;
        }
        
        let digit = digit_char.to_digit(10).unwrap_or(0) as u8;
        
        if i == value_str.len() - 1 && signed {
            // Last digit carries sign
            data[i] = if is_negative {
                0xD0 | digit // Negative sign zone
            } else {
                0xF0 | digit // Positive sign zone
            };
        } else {
            data[i] = 0xF0 | digit; // Normal digit
        }
    }
}

fn fill_packed_field(
    data: &mut [u8], 
    digits: u16, 
    _scale: i16, 
    signed: bool, 
    rng: &mut StdRng, 
    edge_cases: bool, 
    invalid: bool
) {
    if invalid && rng.gen_bool(0.3) {
        // Invalid: bad nibbles
        for byte in data.iter_mut() {
            *byte = rng.gen_range(0xAA..=0xFF); // Invalid nibbles
        }
        return;
    }
    
    let is_negative = signed && rng.gen_bool(0.3);
    let max_value = 10_u64.pow(u32::from(digits)) - 1;
    
    let value = if edge_cases && rng.gen_bool(0.3) {
        if rng.gen_bool(0.5) { 0 } else { max_value }
    } else {
        rng.gen_range(0..=max_value)
    };
    
    // Pack the decimal
    let value_str = format!("{:0width$}", value, width = digits as usize);
    let mut nibbles = Vec::new();
    
    for digit_char in value_str.chars() {
        nibbles.push(digit_char.to_digit(10).unwrap_or(0) as u8);
    }
    
    // Add sign nibble
    let sign_nibble = if is_negative { 0xD } else { 0xC };
    nibbles.push(sign_nibble);
    
    // Pack nibbles into bytes
    for (i, chunk) in nibbles.chunks(2).enumerate() {
        if i >= data.len() {
            break;
        }
        
        let high = chunk[0];
        let low = chunk.get(1).copied().unwrap_or(0);
        data[i] = (high << 4) | low;
    }
}

fn fill_binary_field(
    data: &mut [u8], 
    bits: u16, 
    signed: bool, 
    rng: &mut StdRng, 
    edge_cases: bool, 
    invalid: bool
) {
    if invalid {
        // For binary, invalid data is just random bytes
        for byte in data.iter_mut() {
            *byte = rng.r#gen();
        }
        return;
    }
    
    let byte_len = data.len();
    let max_value = if signed {
        (1u64 << (bits - 1)) - 1
    } else {
        (1u64 << bits) - 1
    };
    
    let value = if edge_cases && rng.gen_bool(0.3) {
        if rng.gen_bool(0.5) { 0 } else { max_value }
    } else {
        rng.gen_range(0..=max_value)
    };
    
    let is_negative = signed && rng.gen_bool(0.3) && value > 0;
    let final_value = if is_negative {
        // Two's complement
        (!value).wrapping_add(1)
    } else {
        value
    };
    
    // Store as big-endian
    let bytes = final_value.to_be_bytes();
    let start_idx = 8 - byte_len;
    
    for (i, &byte) in bytes[start_idx..].iter().enumerate() {
        if i < data.len() {
            data[i] = byte;
        }
    }
}

fn fill_performance_field_data(record: &mut [u8], field: &Field, record_idx: usize) {
    let start = field.offset as usize;
    let end = start + field.len as usize;
    
    if end > record.len() {
        return;
    }
    
    match &field.kind {
        FieldKind::Alphanum { .. } => {
            // Predictable text pattern
            let pattern = format!("REC{record_idx:06}");
            let pattern_bytes = pattern.as_bytes();
            
            for (i, byte) in record[start..end].iter_mut().enumerate() {
                if i < pattern_bytes.len() {
                    *byte = ascii_to_ebcdic_approx(pattern_bytes[i]);
                } else {
                    *byte = 0x40; // EBCDIC space
                }
            }
        }
        FieldKind::ZonedDecimal { digits, .. } => {
            // Predictable numeric pattern
            let value = (record_idx % (10_usize.pow(u32::from(*digits)))) as u64;
            let value_str = format!("{:0width$}", value, width = *digits as usize);
            
            for (i, digit_char) in value_str.chars().enumerate() {
                if i >= (end - start) {
                    break;
                }
                let digit = digit_char.to_digit(10).unwrap_or(0) as u8;
                record[start + i] = 0xF0 | digit;
            }
        }
        FieldKind::PackedDecimal { digits, .. } => {
            // Predictable packed pattern
            let value = (record_idx % (10_usize.pow(u32::from(*digits)))) as u64;
            let value_str = format!("{:0width$}", value, width = *digits as usize);
            let mut nibbles = Vec::new();
            
            for digit_char in value_str.chars() {
                nibbles.push(digit_char.to_digit(10).unwrap_or(0) as u8);
            }
            nibbles.push(0xC); // Positive sign
            
            for (i, chunk) in nibbles.chunks(2).enumerate() {
                if start + i >= end {
                    break;
                }
                let high = chunk[0];
                let low = chunk.get(1).copied().unwrap_or(0);
                record[start + i] = (high << 4) | low;
            }
        }
        FieldKind::BinaryInt { bits, .. } => {
            // Predictable binary pattern
            let max_value = (1u64 << bits) - 1;
            let value = (record_idx as u64) % max_value;
            let bytes = value.to_be_bytes();
            let byte_len = end - start;
            let start_idx = 8 - byte_len;
            
            for (i, &byte) in bytes[start_idx..].iter().enumerate() {
                if i < byte_len {
                    record[start + i] = byte;
                }
            }
        }
        FieldKind::Group => {
            // Groups handled by child fields
        }
    }
}

fn ascii_to_ebcdic_approx(ascii: u8) -> u8 {
    // Simplified ASCII to EBCDIC conversion for common characters
    match ascii {
        b' ' => 0x40,
        b'0'..=b'9' => 0xF0 + (ascii - b'0'),
        b'A'..=b'Z' => 0xC1 + (ascii - b'A'),
        b'a'..=b'z' => 0x81 + (ascii - b'a'),
        _ => 0x40, // Default to space
    }
}

fn find_field_by_path<'a>(_field: &'a Field, _path: &'a str) -> Option<&'a Field> {
    // This is a simplified implementation
    // In practice, we'd need to search the schema
    None
}

fn set_counter_field_value(record: &mut [u8], field: &Field, value: u32) {
    // Set the counter field value based on its type
    let start = field.offset as usize;
    let end = start + field.len as usize;
    
    if end > record.len() {
        return;
    }
    
    match &field.kind {
        FieldKind::BinaryInt { .. } => {
            let bytes = u64::from(value).to_be_bytes();
            let byte_len = end - start;
            let start_idx = 8 - byte_len;
            
            for (i, &byte) in bytes[start_idx..].iter().enumerate() {
                if i < byte_len {
                    record[start + i] = byte;
                }
            }
        }
        FieldKind::ZonedDecimal { digits, .. } => {
            let value_str = format!("{:0width$}", value, width = *digits as usize);
            for (i, digit_char) in value_str.chars().enumerate() {
                if i >= (end - start) {
                    break;
                }
                let digit = digit_char.to_digit(10).unwrap_or(0) as u8;
                record[start + i] = 0xF0 | digit;
            }
        }
        FieldKind::PackedDecimal { digits, .. } => {
            let value_str = format!("{:0width$}", value, width = *digits as usize);
            let mut nibbles = Vec::new();
            
            for digit_char in value_str.chars() {
                nibbles.push(digit_char.to_digit(10).unwrap_or(0) as u8);
            }
            nibbles.push(0xC); // Positive sign
            
            for (i, chunk) in nibbles.chunks(2).enumerate() {
                if start + i >= end {
                    break;
                }
                let high = chunk[0];
                let low = chunk.get(1).copied().unwrap_or(0);
                record[start + i] = (high << 4) | low;
            }
        }
        _ => {
            // Other field types not typically used as counters
        }
    }
}

/// Generate test datasets for specific scenarios
#[must_use] pub fn generate_test_datasets(_config: &GeneratorConfig) -> Vec<(String, Vec<Vec<u8>>)> {
    
    
    // This would be implemented with actual schemas once they're available
    // For now, return empty datasets
    
    Vec::new()
}

/// Generate corruption scenarios for negative testing
#[must_use] pub fn generate_corrupted_data(clean_data: &[u8], corruption_type: CorruptionType) -> Vec<u8> {
    let mut corrupted = clean_data.to_vec();
    
    match corruption_type {
        CorruptionType::BitFlip => {
            // Flip random bits
            if !corrupted.is_empty() {
                let idx = corrupted.len() / 2;
                corrupted[idx] ^= 0x01;
            }
        }
        CorruptionType::Truncation => {
            // Truncate data
            if corrupted.len() > 10 {
                corrupted.truncate(corrupted.len() - 5);
            }
        }
        CorruptionType::Padding => {
            // Add extra padding
            corrupted.extend_from_slice(&[0x00; 10]);
        }
        CorruptionType::AsciiTransfer => {
            // Simulate ASCII transfer corruption
            for byte in &mut corrupted {
                if *byte >= 0x80 {
                    *byte = b'?'; // ASCII replacement
                }
            }
        }
    }
    
    corrupted
}

/// Types of data corruption for testing
#[derive(Debug, Clone, Copy)]
pub enum CorruptionType {
    BitFlip,
    Truncation,
    Padding,
    AsciiTransfer,
}
