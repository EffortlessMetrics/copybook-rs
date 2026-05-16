// SPDX-License-Identifier: AGPL-3.0-or-later
//! Floating-point codecs for COBOL COMP-1 and COMP-2 fields.

use crate::options::FloatFormat;
use copybook_core::{Error, ErrorCode, Result};
use std::convert::TryFrom;

// =============================================================================
// Floating-Point Codecs (COMP-1 / COMP-2)
// =============================================================================

const IBM_HEX_EXPONENT_BIAS: i32 = 64;
const IBM_HEX_FRACTION_MIN: f64 = 1.0 / 16.0;
const IBM_HEX_SINGLE_FRACTION_BITS: u32 = 24;
const IBM_HEX_DOUBLE_FRACTION_BITS: u32 = 56;

#[inline]
fn validate_float_buffer_len(data: &[u8], required: usize, usage: &str) -> Result<()> {
    if data.len() < required {
        return Err(Error::new(
            ErrorCode::CBKD301_RECORD_TOO_SHORT,
            format!("{usage} requires {required} bytes, got {}", data.len()),
        ));
    }
    Ok(())
}

#[inline]
fn validate_float_encode_buffer_len(buffer: &[u8], required: usize, usage: &str) -> Result<()> {
    if buffer.len() < required {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            format!(
                "{usage} requires {required} bytes, buffer has {}",
                buffer.len()
            ),
        ));
    }
    Ok(())
}

#[inline]
#[allow(clippy::cast_precision_loss)]
fn decode_ibm_hex_to_f64(sign: bool, exponent_raw: u8, fraction_bits: u64, bits: u32) -> f64 {
    if exponent_raw == 0 && fraction_bits == 0 {
        return if sign { -0.0 } else { 0.0 };
    }

    let exponent = i32::from(exponent_raw) - IBM_HEX_EXPONENT_BIAS;
    let divisor = 2_f64.powi(i32::try_from(bits).unwrap_or(0));
    let fraction = (fraction_bits as f64) / divisor;
    let magnitude = fraction * 16_f64.powi(exponent);
    if sign { -magnitude } else { magnitude }
}

#[inline]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
fn encode_f64_to_ibm_hex_parts(value: f64, bits: u32) -> Result<(u8, u64)> {
    if !value.is_finite() {
        return Err(Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "IBM hex float encoding requires finite values",
        ));
    }

    if value == 0.0 {
        return Ok((0, 0));
    }

    let mut exponent = IBM_HEX_EXPONENT_BIAS;
    let mut fraction = value.abs();

    while fraction < IBM_HEX_FRACTION_MIN {
        fraction *= 16.0;
        exponent -= 1;
        if exponent <= 0 {
            return Ok((0, 0));
        }
    }

    while fraction >= 1.0 {
        fraction /= 16.0;
        exponent += 1;
        if exponent >= 128 {
            return Err(Error::new(
                ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                "IBM hex float exponent overflow",
            ));
        }
    }

    let scale = 2_f64.powi(i32::try_from(bits).unwrap_or(0));
    let mut fraction_bits = (fraction * scale).round() as u64;
    let full_scale = 1_u64 << bits;
    if fraction_bits >= full_scale {
        // Carry from rounding: re-normalize to 0x1... and bump exponent.
        fraction_bits = 1_u64 << (bits - 4);
        exponent += 1;
        if exponent >= 128 {
            return Err(Error::new(
                ErrorCode::CBKE510_NUMERIC_OVERFLOW,
                "IBM hex float exponent overflow",
            ));
        }
    }

    Ok((u8::try_from(exponent).unwrap_or(0), fraction_bits))
}

/// Decode a COMP-1 field in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single_ieee_be(data: &[u8]) -> Result<f32> {
    validate_float_buffer_len(data, 4, "COMP-1")?;
    let bytes: [u8; 4] = [data[0], data[1], data[2], data[3]];
    Ok(f32::from_be_bytes(bytes))
}

/// Decode a COMP-2 field in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double_ieee_be(data: &[u8]) -> Result<f64> {
    validate_float_buffer_len(data, 8, "COMP-2")?;
    let bytes: [u8; 8] = [
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
    ];
    Ok(f64::from_be_bytes(bytes))
}

/// Decode a COMP-1 field in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single_ibm_hex(data: &[u8]) -> Result<f32> {
    validate_float_buffer_len(data, 4, "COMP-1")?;
    let word = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
    let sign = (word & 0x8000_0000) != 0;
    let exponent_raw = ((word >> 24) & 0x7F) as u8;
    let fraction_bits = u64::from(word & 0x00FF_FFFF);
    let value = decode_ibm_hex_to_f64(
        sign,
        exponent_raw,
        fraction_bits,
        IBM_HEX_SINGLE_FRACTION_BITS,
    );
    #[allow(clippy::cast_possible_truncation)]
    {
        Ok(value as f32)
    }
}

/// Decode a COMP-2 field in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double_ibm_hex(data: &[u8]) -> Result<f64> {
    validate_float_buffer_len(data, 8, "COMP-2")?;
    let word = u64::from_be_bytes([
        data[0], data[1], data[2], data[3], data[4], data[5], data[6], data[7],
    ]);
    let sign = (word & 0x8000_0000_0000_0000) != 0;
    let exponent_raw = ((word >> 56) & 0x7F) as u8;
    let fraction_bits = word & 0x00FF_FFFF_FFFF_FFFF;
    Ok(decode_ibm_hex_to_f64(
        sign,
        exponent_raw,
        fraction_bits,
        IBM_HEX_DOUBLE_FRACTION_BITS,
    ))
}

/// Decode a COMP-1 float with explicit format selection.
///
/// # Errors
/// Returns format-specific decode errors.
#[inline]
pub fn decode_float_single_with_format(data: &[u8], format: FloatFormat) -> Result<f32> {
    match format {
        FloatFormat::IeeeBigEndian => decode_float_single_ieee_be(data),
        FloatFormat::IbmHex => decode_float_single_ibm_hex(data),
    }
}

/// Decode a COMP-2 float with explicit format selection.
///
/// # Errors
/// Returns format-specific decode errors.
#[inline]
pub fn decode_float_double_with_format(data: &[u8], format: FloatFormat) -> Result<f64> {
    match format {
        FloatFormat::IeeeBigEndian => decode_float_double_ieee_be(data),
        FloatFormat::IbmHex => decode_float_double_ibm_hex(data),
    }
}

/// Decode a COMP-1 float with default IEEE-754 big-endian interpretation.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 4 bytes.
#[inline]
pub fn decode_float_single(data: &[u8]) -> Result<f32> {
    decode_float_single_ieee_be(data)
}

/// Decode a COMP-2 float with default IEEE-754 big-endian interpretation.
///
/// # Errors
/// Returns `CBKD301_RECORD_TOO_SHORT` if the data slice has fewer than 8 bytes.
#[inline]
pub fn decode_float_double(data: &[u8]) -> Result<f64> {
    decode_float_double_ieee_be(data)
}

/// Encode a COMP-1 value in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 4 bytes.
#[inline]
pub fn encode_float_single_ieee_be(value: f32, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 4, "COMP-1")?;
    let bytes = value.to_be_bytes();
    buffer[..4].copy_from_slice(&bytes);
    Ok(())
}

/// Encode a COMP-2 value in IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 8 bytes.
#[inline]
pub fn encode_float_double_ieee_be(value: f64, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 8, "COMP-2")?;
    let bytes = value.to_be_bytes();
    buffer[..8].copy_from_slice(&bytes);
    Ok(())
}

/// Encode a COMP-1 value in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns:
/// - `CBKE510_NUMERIC_OVERFLOW` if the buffer is too small
/// - `CBKE510_NUMERIC_OVERFLOW` for non-finite values or exponent overflow
#[inline]
pub fn encode_float_single_ibm_hex(value: f32, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 4, "COMP-1")?;
    let sign = value.is_sign_negative();
    let (exponent_raw, fraction_bits) =
        encode_f64_to_ibm_hex_parts(f64::from(value), IBM_HEX_SINGLE_FRACTION_BITS)?;
    let sign_bit = if sign { 0x8000_0000 } else { 0 };
    let fraction_low = u32::try_from(fraction_bits & 0x00FF_FFFF).map_err(|_| {
        Error::new(
            ErrorCode::CBKE510_NUMERIC_OVERFLOW,
            "IBM hex fraction overflow for COMP-1",
        )
    })?;
    let word = sign_bit | (u32::from(exponent_raw) << 24) | fraction_low;
    buffer[..4].copy_from_slice(&word.to_be_bytes());
    Ok(())
}

/// Encode a COMP-2 value in IBM hexadecimal floating-point format.
///
/// # Errors
/// Returns:
/// - `CBKE510_NUMERIC_OVERFLOW` if the buffer is too small
/// - `CBKE510_NUMERIC_OVERFLOW` for non-finite values or exponent overflow
#[inline]
pub fn encode_float_double_ibm_hex(value: f64, buffer: &mut [u8]) -> Result<()> {
    validate_float_encode_buffer_len(buffer, 8, "COMP-2")?;
    let sign = value.is_sign_negative();
    let (exponent_raw, fraction_bits) =
        encode_f64_to_ibm_hex_parts(value, IBM_HEX_DOUBLE_FRACTION_BITS)?;
    let sign_bit = if sign { 0x8000_0000_0000_0000 } else { 0 };
    let word = sign_bit | (u64::from(exponent_raw) << 56) | (fraction_bits & 0x00FF_FFFF_FFFF_FFFF);
    buffer[..8].copy_from_slice(&word.to_be_bytes());
    Ok(())
}

/// Encode a COMP-1 float with explicit format selection.
///
/// # Errors
/// Returns format-specific encode errors.
#[inline]
pub fn encode_float_single_with_format(
    value: f32,
    buffer: &mut [u8],
    format: FloatFormat,
) -> Result<()> {
    match format {
        FloatFormat::IeeeBigEndian => encode_float_single_ieee_be(value, buffer),
        FloatFormat::IbmHex => encode_float_single_ibm_hex(value, buffer),
    }
}

/// Encode a COMP-2 float with explicit format selection.
///
/// # Errors
/// Returns format-specific encode errors.
#[inline]
pub fn encode_float_double_with_format(
    value: f64,
    buffer: &mut [u8],
    format: FloatFormat,
) -> Result<()> {
    match format {
        FloatFormat::IeeeBigEndian => encode_float_double_ieee_be(value, buffer),
        FloatFormat::IbmHex => encode_float_double_ibm_hex(value, buffer),
    }
}

/// Encode a COMP-1 float with default IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 4 bytes.
#[inline]
pub fn encode_float_single(value: f32, buffer: &mut [u8]) -> Result<()> {
    encode_float_single_ieee_be(value, buffer)
}

/// Encode a COMP-2 float with default IEEE-754 big-endian format.
///
/// # Errors
/// Returns `CBKE510_NUMERIC_OVERFLOW` if the buffer has fewer than 8 bytes.
#[inline]
pub fn encode_float_double(value: f64, buffer: &mut [u8]) -> Result<()> {
    encode_float_double_ieee_be(value, buffer)
}
