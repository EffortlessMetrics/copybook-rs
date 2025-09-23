# Zoned Decimal Encoding Domain Schema

## Overview

This document defines the domain types and logic for handling zoned decimal encoding format detection and preservation in copybook-rs. The implementation ensures byte-perfect round-trip fidelity while maintaining enterprise-grade performance and reliability.

## Core Domain Types

### ZonedEncodingFormat Enum

```rust
/// Zoned decimal encoding format specification
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, clap::ValueEnum)]
pub enum ZonedEncodingFormat {
    /// ASCII digit zones (0x30-0x39) - common in ASCII mainframe exports
    #[value(name = "ascii")]
    Ascii,
    /// EBCDIC digit zones (0xF0-0xF9) - native mainframe COBOL format
    #[value(name = "ebcdic")]
    Ebcdic,
    /// Automatic detection based on input data patterns
    #[value(name = "auto")]
    Auto,
}

impl ZonedEncodingFormat {
    /// Check if this format uses ASCII encoding
    #[must_use]
    pub const fn is_ascii(self) -> bool {
        matches!(self, Self::Ascii)
    }

    /// Check if this format uses EBCDIC encoding
    #[must_use]
    pub const fn is_ebcdic(self) -> bool {
        matches!(self, Self::Ebcdic)
    }

    /// Check if this format requires automatic detection
    #[must_use]
    pub const fn is_auto(self) -> bool {
        matches!(self, Self::Auto)
    }

    /// Get the expected zone nibble value for this encoding
    #[must_use]
    pub const fn expected_zone_nibble(self) -> Option<u8> {
        match self {
            Self::Ascii => Some(0x3),
            Self::Ebcdic => Some(0xF),
            Self::Auto => None,
        }
    }

    /// Get a human-readable description of the encoding format
    #[must_use]
    pub const fn description(self) -> &'static str {
        match self {
            Self::Ascii => "ASCII digit zones (0x30-0x39)",
            Self::Ebcdic => "EBCDIC digit zones (0xF0-0xF9)",
            Self::Auto => "Automatic detection from input data",
        }
    }

    /// Detect encoding format from a zone nibble
    #[must_use]
    pub const fn from_zone_nibble(zone: u8) -> Option<Self> {
        match zone {
            0x3 => Some(Self::Ascii),
            0xF => Some(Self::Ebcdic),
            _ => None,
        }
    }
}

impl fmt::Display for ZonedEncodingFormat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascii => write!(f, "ascii"),
            Self::Ebcdic => write!(f, "ebcdic"),
            Self::Auto => write!(f, "auto"),
        }
    }
}

impl Default for ZonedEncodingFormat {
    fn default() -> Self {
        Self::Ebcdic // Maintain backward compatibility
    }
}
```

### ZonedEncodingDetection Result

```rust
/// Result of zoned decimal encoding detection
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZonedEncodingDetection {
    /// Detected encoding format
    pub format: ZonedEncodingFormat,
    /// Confidence level of detection (0.0 = uncertain, 1.0 = certain)
    pub confidence: f32,
    /// Number of consistent zone nibbles found
    pub consistent_zones: usize,
    /// Number of inconsistent zone nibbles (mixed encoding)
    pub inconsistent_zones: usize,
    /// Whether mixed encoding was detected within the field
    pub has_mixed_encoding: bool,
}

impl ZonedEncodingDetection {
    /// Create a new detection result
    pub fn new(
        format: ZonedEncodingFormat,
        confidence: f32,
        consistent_zones: usize,
        inconsistent_zones: usize,
    ) -> Self {
        Self {
            format,
            confidence,
            consistent_zones,
            inconsistent_zones,
            has_mixed_encoding: inconsistent_zones > 0,
        }
    }

    /// Check if detection is reliable (confidence >= 0.8)
    #[must_use]
    pub fn is_reliable(&self) -> bool {
        self.confidence >= 0.8
    }

    /// Check if mixed encoding was detected
    #[must_use]
    pub fn has_mixed_encoding(&self) -> bool {
        self.has_mixed_encoding
    }

    /// Get a description of the detection result
    pub fn description(&self) -> String {
        if self.has_mixed_encoding {
            format!(
                "Mixed encoding detected: {} consistent zones, {} inconsistent zones",
                self.consistent_zones, self.inconsistent_zones
            )
        } else {
            format!(
                "{} encoding detected with {:.1}% confidence ({} zones)",
                self.format,
                self.confidence * 100.0,
                self.consistent_zones
            )
        }
    }
}
```

## Encoding Detection Algorithm

### Core Detection Logic

```rust
/// Detect zoned decimal encoding format from field data
pub fn detect_zoned_encoding(
    data: &[u8],
    signed: bool,
    preferred_format: Option<ZonedEncodingFormat>,
) -> Result<ZonedEncodingDetection> {
    if data.is_empty() {
        return Ok(ZonedEncodingDetection::new(
            preferred_format.unwrap_or_default(),
            0.0,
            0,
            0,
        ));
    }

    let mut ascii_zones = 0;
    let mut ebcdic_zones = 0;
    let mut invalid_zones = 0;

    // Analyze zone nibbles excluding the last byte if signed
    let analyze_len = if signed {
        data.len().saturating_sub(1)
    } else {
        data.len()
    };

    for &byte in &data[..analyze_len] {
        let zone = (byte >> 4) & 0x0F;
        match zone {
            0x3 => ascii_zones += 1,
            0xF => ebcdic_zones += 1,
            _ => invalid_zones += 1,
        }
    }

    // Handle signed field - last byte zone indicates sign, not encoding
    if signed && !data.is_empty() {
        let last_byte = data[data.len() - 1];
        let last_zone = (last_byte >> 4) & 0x0F;

        // For signed fields, use the sign table to validate the zone
        // but don't count it towards encoding detection
        let sign_table = get_zoned_sign_table(Codepage::CP037); // Use EBCDIC table as reference
        let (has_sign, _) = sign_table[last_zone as usize];

        if !has_sign {
            // If it's not a valid sign zone, it might be a regular digit zone
            match last_zone {
                0x3 => ascii_zones += 1,
                0xF => ebcdic_zones += 1,
                _ => invalid_zones += 1,
            }
        }
    }

    // Determine format based on zone analysis
    let total_zones = ascii_zones + ebcdic_zones;
    let inconsistent_zones = ascii_zones.min(ebcdic_zones) + invalid_zones;

    if total_zones == 0 {
        // No identifiable zones - use preferred format or default
        let format = preferred_format.unwrap_or_default();
        return Ok(ZonedEncodingDetection::new(format, 0.0, 0, inconsistent_zones));
    }

    let (detected_format, consistent_zones) = if ascii_zones > ebcdic_zones {
        (ZonedEncodingFormat::Ascii, ascii_zones)
    } else if ebcdic_zones > ascii_zones {
        (ZonedEncodingFormat::Ebcdic, ebcdic_zones)
    } else {
        // Tie - use preferred format or default
        let format = preferred_format.unwrap_or_default();
        let consistent = if format.is_ascii() { ascii_zones } else { ebcdic_zones };
        (format, consistent)
    };

    // Calculate confidence based on consistency
    let confidence = if total_zones > 0 {
        consistent_zones as f32 / (consistent_zones + inconsistent_zones) as f32
    } else {
        0.0
    };

    Ok(ZonedEncodingDetection::new(
        detected_format,
        confidence,
        consistent_zones,
        inconsistent_zones,
    ))
}
```

## JSON Metadata Schema

### Field-Level Encoding Metadata

When `preserve_zoned_encoding` is enabled, encoding format information is stored in JSON metadata:

```json
{
  "field_name": "12345",
  "_field_name_encoding": "ascii",
  "_meta": {
    "field_name": {
      "zoned_encoding": "ascii",
      "detection_confidence": 1.0,
      "has_mixed_encoding": false
    }
  }
}
```

### Record-Level Encoding Metadata

For fields with mixed or uncertain encoding:

```json
{
  "_meta": {
    "_zoned_encoding_default": "ebcdic",
    "_zoned_encoding_detection": {
      "field1": {"format": "ascii", "confidence": 1.0},
      "field2": {"format": "ebcdic", "confidence": 0.9},
      "field3": {"format": "auto", "confidence": 0.5, "mixed": true}
    }
  }
}
```

## Error Handling Domain

### New CBKD Error Codes

```rust
// In copybook-core/src/error.rs
pub enum ErrorCode {
    // ... existing codes ...

    /// CBKD413: Invalid zoned decimal encoding format
    CBKD413_ZONED_INVALID_ENCODING,

    /// CBKD414: Mixed ASCII/EBCDIC encoding in single field
    CBKD414_ZONED_MIXED_ENCODING,

    /// CBKD415: Unable to detect zoned decimal encoding format
    CBKD415_ZONED_ENCODING_DETECTION_FAILED,
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // ... existing cases ...
            Self::CBKD413_ZONED_INVALID_ENCODING => write!(f, "CBKD413_ZONED_INVALID_ENCODING"),
            Self::CBKD414_ZONED_MIXED_ENCODING => write!(f, "CBKD414_ZONED_MIXED_ENCODING"),
            Self::CBKD415_ZONED_ENCODING_DETECTION_FAILED => write!(f, "CBKD415_ZONED_ENCODING_DETECTION_FAILED"),
        }
    }
}
```

### Error Handling Patterns

```rust
/// Handle encoding detection results with appropriate error/warning generation
pub fn handle_encoding_detection(
    detection: &ZonedEncodingDetection,
    field_path: &str,
    strict_mode: bool,
) -> Result<ZonedEncodingFormat> {
    if detection.has_mixed_encoding() {
        if strict_mode {
            return Err(Error::new(
                ErrorCode::CBKD414_ZONED_MIXED_ENCODING,
                format!(
                    "Mixed encoding detected in field '{}': {} inconsistent zones",
                    field_path, detection.inconsistent_zones
                ),
            ));
        } else {
            warn!(
                "CBKD414_ZONED_MIXED_ENCODING: Mixed encoding in field '{}', using {} (confidence: {:.1}%)",
                field_path, detection.format, detection.confidence * 100.0
            );
            crate::lib_api::increment_warning_counter();
        }
    }

    if !detection.is_reliable() && strict_mode {
        return Err(Error::new(
            ErrorCode::CBKD415_ZONED_ENCODING_DETECTION_FAILED,
            format!(
                "Low confidence encoding detection in field '{}': {:.1}% confidence",
                field_path, detection.confidence * 100.0
            ),
        ));
    }

    Ok(detection.format)
}
```

## Integration Points

### DecodeOptions Extensions

```rust
impl DecodeOptions {
    /// Enable zoned decimal encoding preservation
    #[must_use]
    pub fn with_preserve_zoned_encoding(mut self, preserve: bool) -> Self {
        self.preserve_zoned_encoding = preserve;
        self
    }

    /// Set preferred zoned encoding format for auto-detection fallback
    #[must_use]
    pub fn with_preferred_zoned_encoding(mut self, format: Option<ZonedEncodingFormat>) -> Self {
        self.preferred_zoned_encoding = format;
        self
    }
}
```

### EncodeOptions Extensions

```rust
impl EncodeOptions {
    /// Override zoned decimal encoding format
    #[must_use]
    pub fn with_zoned_encoding_override(mut self, format: Option<ZonedEncodingFormat>) -> Self {
        self.zoned_encoding_override = format;
        self
    }
}
```

## Performance Considerations

### Optimized Detection Path

```rust
/// Fast path for common encoding detection scenarios
#[inline]
pub fn detect_zoned_encoding_fast(data: &[u8]) -> Option<ZonedEncodingFormat> {
    if data.is_empty() {
        return None;
    }

    // Fast path: check first few bytes for consistent zone pattern
    let sample_size = data.len().min(4);
    let mut ascii_count = 0;
    let mut ebcdic_count = 0;

    for &byte in &data[..sample_size] {
        match (byte >> 4) & 0x0F {
            0x3 => ascii_count += 1,
            0xF => ebcdic_count += 1,
            _ => return None, // Mixed or invalid - use full detection
        }
    }

    match (ascii_count, ebcdic_count) {
        (n, 0) if n == sample_size => Some(ZonedEncodingFormat::Ascii),
        (0, n) if n == sample_size => Some(ZonedEncodingFormat::Ebcdic),
        _ => None, // Use full detection algorithm
    }
}
```

### Memory-Efficient Metadata

```rust
/// Compact encoding metadata for memory efficiency
#[derive(Debug, Clone)]
pub struct CompactEncodingMetadata {
    /// Bitfield: bit 0 = has_ascii, bit 1 = has_ebcdic, bit 2 = is_mixed
    flags: u8,
    /// Confidence level (0-255, scaled from 0.0-1.0)
    confidence: u8,
}

impl CompactEncodingMetadata {
    #[must_use]
    pub fn new(detection: &ZonedEncodingDetection) -> Self {
        let mut flags = 0;
        if detection.format.is_ascii() {
            flags |= 0x01;
        }
        if detection.format.is_ebcdic() {
            flags |= 0x02;
        }
        if detection.has_mixed_encoding {
            flags |= 0x04;
        }

        Self {
            flags,
            confidence: (detection.confidence * 255.0) as u8,
        }
    }

    #[must_use]
    pub fn to_detection(&self) -> ZonedEncodingDetection {
        let format = if self.flags & 0x01 != 0 {
            ZonedEncodingFormat::Ascii
        } else {
            ZonedEncodingFormat::Ebcdic
        };

        ZonedEncodingDetection {
            format,
            confidence: f32::from(self.confidence) / 255.0,
            consistent_zones: 0, // Not preserved in compact form
            inconsistent_zones: 0, // Not preserved in compact form
            has_mixed_encoding: self.flags & 0x04 != 0,
        }
    }
}
```

## Testing Strategy

### Property-Based Test Generators

```rust
#[cfg(test)]
mod prop_tests {
    use super::*;
    use proptest::prelude::*;

    /// Generate valid zoned decimal data with known encoding
    pub fn zoned_decimal_with_encoding(
        digits: u16,
        encoding: ZonedEncodingFormat,
    ) -> impl Strategy<Value = Vec<u8>> {
        let zone = encoding.expected_zone_nibble().unwrap_or(0xF);
        prop::collection::vec(0..=9u8, digits as usize)
            .prop_map(move |digits| {
                digits.into_iter()
                    .map(|digit| (zone << 4) | digit)
                    .collect()
            })
    }

    proptest! {
        #[test]
        fn round_trip_preserves_encoding(
            digits in zoned_decimal_with_encoding(1..=18, ZonedEncodingFormat::Ascii)
        ) {
            let detection = detect_zoned_encoding(&digits, false, None).unwrap();
            prop_assert_eq!(detection.format, ZonedEncodingFormat::Ascii);
            prop_assert!(detection.is_reliable());
        }
    }
}
```

This domain schema provides a comprehensive foundation for implementing zoned decimal encoding detection and preservation while maintaining copybook-rs's enterprise-grade performance and reliability standards.