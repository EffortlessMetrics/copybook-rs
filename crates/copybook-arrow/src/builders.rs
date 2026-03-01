// SPDX-License-Identifier: AGPL-3.0-or-later
//! Column accumulator implementations for building Arrow arrays from raw COBOL data.
//!
//! Each accumulator type wraps an Arrow builder and knows how to extract a typed
//! value from a raw byte slice using the appropriate codec function.

use arrow::array::{
    ArrayRef, Decimal128Builder, Float32Builder, Float64Builder, PrimitiveBuilder, StringBuilder,
};
use arrow::datatypes::{Int16Type, Int32Type, Int64Type, UInt16Type, UInt32Type, UInt64Type};
use std::sync::Arc;

use copybook_codec::Codepage;
use copybook_codec::FloatFormat;
use copybook_codec::UnmappablePolicy;
use copybook_codec::charset::ebcdic_to_utf8;
use copybook_codec::numeric::{
    decode_binary_int, decode_float_double_with_format, decode_float_single_with_format,
    decode_packed_decimal, decode_zoned_decimal,
};
use copybook_core::schema::FieldKind;

use crate::options::ArrowOptions;
use crate::{ArrowError, Result};

/// Trait for accumulating values into Arrow arrays.
pub(crate) trait ColumnAccumulator: Send {
    /// Append a value from raw binary data for this field.
    fn append_value(&mut self, data: &[u8]) -> Result<()>;

    /// Append a null value.
    fn append_null(&mut self);

    /// Finish building and return the Arrow array.
    fn finish(&mut self) -> ArrayRef;

    /// Number of values accumulated.
    #[allow(dead_code)]
    fn len(&self) -> usize;
}

/// Create the right accumulator for a given `FieldKind`.
pub(crate) fn create_accumulator(
    kind: &FieldKind,
    field_len: u32,
    options: &ArrowOptions,
) -> Result<Box<dyn ColumnAccumulator>> {
    match kind {
        FieldKind::Alphanum { .. } => {
            Ok(Box::new(Utf8Accumulator::new(options.codepage, field_len)))
        }

        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            ..
        } => Ok(Box::new(ZonedDecimalAccumulator::new(
            *digits,
            *scale,
            *signed,
            options.codepage,
        ))),

        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => Ok(Box::new(PackedDecimalAccumulator::new(
            *digits, *scale, *signed,
        ))),

        FieldKind::BinaryInt { bits, signed } => make_int_accumulator(*bits, *signed),

        FieldKind::FloatSingle => Ok(Box::new(Float32Accumulator::new(options.float_format))),
        FieldKind::FloatDouble => Ok(Box::new(Float64Accumulator::new(options.float_format))),

        FieldKind::EditedNumeric { .. } => {
            // For now, edited numeric fields are stored as Utf8 strings
            Ok(Box::new(Utf8Accumulator::new(options.codepage, field_len)))
        }

        _ => Err(ArrowError::ColumnBuild(format!(
            "No accumulator for field kind: {kind:?}"
        ))),
    }
}

// ---------------------------------------------------------------------------
// Utf8Accumulator
// ---------------------------------------------------------------------------

struct Utf8Accumulator {
    builder: StringBuilder,
    codepage: Codepage,
    field_len: u32,
    count: usize,
}

impl Utf8Accumulator {
    fn new(codepage: Codepage, field_len: u32) -> Self {
        Self {
            builder: StringBuilder::new(),
            codepage,
            field_len,
            count: 0,
        }
    }
}

impl ColumnAccumulator for Utf8Accumulator {
    fn append_value(&mut self, data: &[u8]) -> Result<()> {
        let len = self.field_len as usize;
        let slice = if data.len() >= len {
            &data[..len]
        } else {
            data
        };
        let s = ebcdic_to_utf8(slice, self.codepage, UnmappablePolicy::Replace)
            .map_err(|e| ArrowError::Codec(e.to_string()))?;
        // Trim trailing spaces (COBOL convention)
        let trimmed = s.trim_end();
        self.builder.append_value(trimmed);
        self.count += 1;
        Ok(())
    }

    fn append_null(&mut self) {
        self.builder.append_null();
        self.count += 1;
    }

    fn finish(&mut self) -> ArrayRef {
        Arc::new(self.builder.finish())
    }

    fn len(&self) -> usize {
        self.count
    }
}

// ---------------------------------------------------------------------------
// ZonedDecimalAccumulator
// ---------------------------------------------------------------------------

struct ZonedDecimalAccumulator {
    builder: Decimal128Builder,
    digits: u16,
    scale: i16,
    signed: bool,
    codepage: Codepage,
    count: usize,
}

impl ZonedDecimalAccumulator {
    fn new(digits: u16, scale: i16, signed: bool, codepage: Codepage) -> Self {
        let precision = u8::try_from(digits).unwrap_or(u8::MAX);
        let arrow_scale = i8::try_from(scale).unwrap_or(0);
        Self {
            builder: Decimal128Builder::new()
                .with_precision_and_scale(precision, arrow_scale)
                .unwrap_or_else(|_| Decimal128Builder::new()),
            digits,
            scale,
            signed,
            codepage,
            count: 0,
        }
    }
}

impl ColumnAccumulator for ZonedDecimalAccumulator {
    fn append_value(&mut self, data: &[u8]) -> Result<()> {
        let len = usize::from(self.digits);
        let slice = if data.len() >= len {
            &data[..len]
        } else {
            data
        };
        let sd = decode_zoned_decimal(
            slice,
            self.digits,
            self.scale,
            self.signed,
            self.codepage,
            false,
        )
        .map_err(|e| ArrowError::Codec(e.to_string()))?;
        let i128_val = small_decimal_to_i128(&sd);
        self.builder.append_value(i128_val);
        self.count += 1;
        Ok(())
    }

    fn append_null(&mut self) {
        self.builder.append_null();
        self.count += 1;
    }

    fn finish(&mut self) -> ArrayRef {
        Arc::new(self.builder.finish())
    }

    fn len(&self) -> usize {
        self.count
    }
}

// ---------------------------------------------------------------------------
// PackedDecimalAccumulator
// ---------------------------------------------------------------------------

struct PackedDecimalAccumulator {
    builder: Decimal128Builder,
    digits: u16,
    scale: i16,
    signed: bool,
    count: usize,
}

impl PackedDecimalAccumulator {
    fn new(digits: u16, scale: i16, signed: bool) -> Self {
        let precision = u8::try_from(digits).unwrap_or(u8::MAX);
        let arrow_scale = i8::try_from(scale).unwrap_or(0);
        Self {
            builder: Decimal128Builder::new()
                .with_precision_and_scale(precision, arrow_scale)
                .unwrap_or_else(|_| Decimal128Builder::new()),
            digits,
            scale,
            signed,
            count: 0,
        }
    }
}

impl ColumnAccumulator for PackedDecimalAccumulator {
    fn append_value(&mut self, data: &[u8]) -> Result<()> {
        let expected_bytes = usize::from((self.digits + 1).div_ceil(2));
        let slice = if data.len() >= expected_bytes {
            &data[..expected_bytes]
        } else {
            data
        };
        let sd = decode_packed_decimal(slice, self.digits, self.scale, self.signed)
            .map_err(|e| ArrowError::Codec(e.to_string()))?;
        let i128_val = small_decimal_to_i128(&sd);
        self.builder.append_value(i128_val);
        self.count += 1;
        Ok(())
    }

    fn append_null(&mut self) {
        self.builder.append_null();
        self.count += 1;
    }

    fn finish(&mut self) -> ArrayRef {
        Arc::new(self.builder.finish())
    }

    fn len(&self) -> usize {
        self.count
    }
}

// ---------------------------------------------------------------------------
// Integer accumulators
// ---------------------------------------------------------------------------

fn make_int_accumulator(bits: u16, signed: bool) -> Result<Box<dyn ColumnAccumulator>> {
    match (bits, signed) {
        (16, true) => Ok(Box::new(IntAccumulator::<Int16Type>::new(16, true))),
        (16, false) => Ok(Box::new(IntAccumulator::<UInt16Type>::new(16, false))),
        (32, true) => Ok(Box::new(IntAccumulator::<Int32Type>::new(32, true))),
        (32, false) => Ok(Box::new(IntAccumulator::<UInt32Type>::new(32, false))),
        (64, true) => Ok(Box::new(IntAccumulator::<Int64Type>::new(64, true))),
        (64, false) => Ok(Box::new(IntAccumulator::<UInt64Type>::new(64, false))),
        _ => Err(ArrowError::ColumnBuild(format!(
            "Unsupported binary int width: {bits}"
        ))),
    }
}

struct IntAccumulator<T: arrow::datatypes::ArrowPrimitiveType> {
    builder: PrimitiveBuilder<T>,
    bits: u16,
    signed: bool,
    count: usize,
}

impl<T: arrow::datatypes::ArrowPrimitiveType> IntAccumulator<T> {
    fn new(bits: u16, signed: bool) -> Self {
        Self {
            builder: PrimitiveBuilder::<T>::new(),
            bits,
            signed,
            count: 0,
        }
    }
}

macro_rules! impl_int_accumulator {
    ($arrow_ty:ty, $native_ty:ty) => {
        impl ColumnAccumulator for IntAccumulator<$arrow_ty> {
            #[allow(
                clippy::cast_possible_truncation,
                clippy::cast_sign_loss,
                clippy::cast_possible_wrap
            )]
            fn append_value(&mut self, data: &[u8]) -> Result<()> {
                let byte_len = usize::from(self.bits / 8);
                let slice = if data.len() >= byte_len {
                    &data[..byte_len]
                } else {
                    data
                };
                let val = decode_binary_int(slice, self.bits, self.signed)
                    .map_err(|e| ArrowError::Codec(e.to_string()))?;
                self.builder.append_value(val as $native_ty);
                self.count += 1;
                Ok(())
            }

            fn append_null(&mut self) {
                self.builder.append_null();
                self.count += 1;
            }

            fn finish(&mut self) -> ArrayRef {
                Arc::new(self.builder.finish())
            }

            fn len(&self) -> usize {
                self.count
            }
        }
    };
}

impl_int_accumulator!(Int16Type, i16);
impl_int_accumulator!(UInt16Type, u16);
impl_int_accumulator!(Int32Type, i32);
impl_int_accumulator!(UInt32Type, u32);
impl_int_accumulator!(Int64Type, i64);
impl_int_accumulator!(UInt64Type, u64);

// ---------------------------------------------------------------------------
// Float accumulators
// ---------------------------------------------------------------------------

struct Float32Accumulator {
    builder: Float32Builder,
    float_format: FloatFormat,
    count: usize,
}

impl Float32Accumulator {
    fn new(float_format: FloatFormat) -> Self {
        Self {
            builder: Float32Builder::new(),
            float_format,
            count: 0,
        }
    }
}

impl ColumnAccumulator for Float32Accumulator {
    fn append_value(&mut self, data: &[u8]) -> Result<()> {
        if data.len() < 4 {
            self.builder.append_null();
        } else {
            let val = decode_float_single_with_format(data, self.float_format)
                .map_err(|e| ArrowError::Codec(e.to_string()))?;
            if val.is_nan() || val.is_infinite() {
                self.builder.append_null();
            } else {
                self.builder.append_value(val);
            }
        }
        self.count += 1;
        Ok(())
    }

    fn append_null(&mut self) {
        self.builder.append_null();
        self.count += 1;
    }

    fn finish(&mut self) -> ArrayRef {
        Arc::new(self.builder.finish())
    }

    fn len(&self) -> usize {
        self.count
    }
}

struct Float64Accumulator {
    builder: Float64Builder,
    float_format: FloatFormat,
    count: usize,
}

impl Float64Accumulator {
    fn new(float_format: FloatFormat) -> Self {
        Self {
            builder: Float64Builder::new(),
            float_format,
            count: 0,
        }
    }
}

impl ColumnAccumulator for Float64Accumulator {
    fn append_value(&mut self, data: &[u8]) -> Result<()> {
        if data.len() < 8 {
            self.builder.append_null();
        } else {
            let val = decode_float_double_with_format(data, self.float_format)
                .map_err(|e| ArrowError::Codec(e.to_string()))?;
            if val.is_nan() || val.is_infinite() {
                self.builder.append_null();
            } else {
                self.builder.append_value(val);
            }
        }
        self.count += 1;
        Ok(())
    }

    fn append_null(&mut self) {
        self.builder.append_null();
        self.count += 1;
    }

    fn finish(&mut self) -> ArrayRef {
        Arc::new(self.builder.finish())
    }

    fn len(&self) -> usize {
        self.count
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Convert a [`SmallDecimal`](copybook_codec::SmallDecimal) to an `i128` value
/// suitable for Arrow `Decimal128`.
///
/// `SmallDecimal` stores `value` (unscaled `i64`) and `negative` flag.
/// Arrow `Decimal128` stores the unscaled integer as `i128`.
fn small_decimal_to_i128(sd: &copybook_codec::SmallDecimal) -> i128 {
    let abs = i128::from(sd.value);
    if sd.negative { -abs } else { abs }
}
