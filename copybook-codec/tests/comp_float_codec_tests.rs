//! Tests for COMP-1 and COMP-2 floating-point codec functions.
//!
//! Validates encode/decode of IEEE 754 single and double precision floats,
//! including NaN/Infinity handling and roundtrip consistency.

#[allow(clippy::unwrap_used)]
#[allow(clippy::expect_used)]
mod comp_float_codec {
    use copybook_core::feature_flags::{Feature, FeatureFlags};
    use copybook_codec::FloatFormat;
    use copybook_codec::numeric::{
        decode_float_double, decode_float_double_ibm_hex, decode_float_double_with_format,
        decode_float_single, decode_float_single_ibm_hex, decode_float_single_with_format,
        encode_float_double, encode_float_double_ibm_hex, encode_float_double_with_format,
        encode_float_single, encode_float_single_ibm_hex, encode_float_single_with_format,
    };
    use copybook_codec::{
        Codepage, DecodeOptions, EncodeOptions, JsonNumberMode, RawMode, RecordFormat,
        UnmappablePolicy, ZonedEncodingFormat,
    };

    fn enable_comp_flags_for_codec_tests() {
        let mut flags = FeatureFlags::default();
        flags.enable(Feature::Comp1);
        flags.enable(Feature::Comp2);
        copybook_core::feature_flags::FeatureFlags::set_global(flags);
    }

    // =========================================================================
    // decode_float_single tests
    // =========================================================================

    #[test]
    fn test_decode_float_single_positive() {
        // IEEE 754 binary32 for 1.0: 0x3F800000
        let data: [u8; 4] = [0x3F, 0x80, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!((result - 1.0_f32).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_single_negative() {
        // IEEE 754 binary32 for -1.0: 0xBF800000
        let data: [u8; 4] = [0xBF, 0x80, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!((result - (-1.0_f32)).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_single_zero() {
        // IEEE 754 binary32 for 0.0: 0x00000000
        let data: [u8; 4] = [0x00, 0x00, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!((result - 0.0_f32).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_single_pi() {
        // IEEE 754 binary32 for ~3.14159265: 0x40490FDB
        let data: [u8; 4] = [0x40, 0x49, 0x0F, 0xDB];
        let result = decode_float_single(&data).unwrap();
        assert!((result - std::f32::consts::PI).abs() < 1e-6);
    }

    #[test]
    fn test_decode_float_single_nan() {
        // IEEE 754 NaN: exponent all 1s, mantissa non-zero
        let data: [u8; 4] = [0x7F, 0xC0, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!(result.is_nan());
    }

    #[test]
    fn test_decode_float_single_positive_infinity() {
        // IEEE 754 +Infinity: 0x7F800000
        let data: [u8; 4] = [0x7F, 0x80, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!(result.is_infinite());
        assert!(result.is_sign_positive());
    }

    #[test]
    fn test_decode_float_single_negative_infinity() {
        // IEEE 754 -Infinity: 0xFF800000
        let data: [u8; 4] = [0xFF, 0x80, 0x00, 0x00];
        let result = decode_float_single(&data).unwrap();
        assert!(result.is_infinite());
        assert!(result.is_sign_negative());
    }

    #[test]
    fn test_decode_float_single_too_short() {
        let data: [u8; 3] = [0x3F, 0x80, 0x00];
        let result = decode_float_single(&data);
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_float_single_ibm_hex_positive() {
        // IBM HFP short for +1.0
        let data: [u8; 4] = [0x41, 0x10, 0x00, 0x00];
        let result = decode_float_single_ibm_hex(&data).unwrap();
        assert!((result - 1.0_f32).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_single_ibm_hex_negative() {
        // IBM HFP short for -1.0
        let data: [u8; 4] = [0xC1, 0x10, 0x00, 0x00];
        let result = decode_float_single_ibm_hex(&data).unwrap();
        assert!((result - (-1.0_f32)).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_single_ibm_hex_fraction() {
        // IBM HFP short for +0.15625
        let data: [u8; 4] = [0x40, 0x28, 0x00, 0x00];
        let result = decode_float_single_ibm_hex(&data).unwrap();
        assert!((result - 0.15625_f32).abs() < f32::EPSILON);
    }

    // =========================================================================
    // decode_float_double tests
    // =========================================================================

    #[test]
    fn test_decode_float_double_positive() {
        // IEEE 754 binary64 for 1.0: 0x3FF0000000000000
        let data: [u8; 8] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data).unwrap();
        assert!((result - 1.0_f64).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decode_float_double_negative() {
        // IEEE 754 binary64 for -1.0: 0xBFF0000000000000
        let data: [u8; 8] = [0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data).unwrap();
        assert!((result - (-1.0_f64)).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decode_float_double_zero() {
        let data: [u8; 8] = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data).unwrap();
        assert!((result - 0.0_f64).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decode_float_double_pi() {
        // IEEE 754 binary64 for PI: 0x400921FB54442D18
        let data: [u8; 8] = [0x40, 0x09, 0x21, 0xFB, 0x54, 0x44, 0x2D, 0x18];
        let result = decode_float_double(&data).unwrap();
        assert!((result - std::f64::consts::PI).abs() < 1e-15);
    }

    #[test]
    fn test_decode_float_double_nan() {
        let data: [u8; 8] = [0x7F, 0xF8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data).unwrap();
        assert!(result.is_nan());
    }

    #[test]
    fn test_decode_float_double_positive_infinity() {
        let data: [u8; 8] = [0x7F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data).unwrap();
        assert!(result.is_infinite());
        assert!(result.is_sign_positive());
    }

    #[test]
    fn test_decode_float_double_too_short() {
        let data: [u8; 7] = [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double(&data);
        assert!(result.is_err());
    }

    #[test]
    fn test_decode_float_double_ibm_hex_positive() {
        // IBM HFP long for +1.0
        let data: [u8; 8] = [0x41, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double_ibm_hex(&data).unwrap();
        assert!((result - 1.0_f64).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decode_float_double_ibm_hex_fraction() {
        // IBM HFP long for +0.15625
        let data: [u8; 8] = [0x40, 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00];
        let result = decode_float_double_ibm_hex(&data).unwrap();
        assert!((result - 0.15625_f64).abs() < f64::EPSILON);
    }

    // =========================================================================
    // encode_float_single tests
    // =========================================================================

    #[test]
    fn test_encode_float_single_positive() {
        let mut buf = [0u8; 4];
        encode_float_single(1.0_f32, &mut buf).unwrap();
        assert_eq!(buf, [0x3F, 0x80, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_single_negative() {
        let mut buf = [0u8; 4];
        encode_float_single(-1.0_f32, &mut buf).unwrap();
        assert_eq!(buf, [0xBF, 0x80, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_single_zero() {
        let mut buf = [0u8; 4];
        encode_float_single(0.0_f32, &mut buf).unwrap();
        assert_eq!(buf, [0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_single_buffer_too_small() {
        let mut buf = [0u8; 3];
        let result = encode_float_single(1.0_f32, &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_encode_float_single_ibm_hex_positive() {
        let mut buf = [0u8; 4];
        encode_float_single_ibm_hex(1.0_f32, &mut buf).unwrap();
        assert_eq!(buf, [0x41, 0x10, 0x00, 0x00]);
    }

    // =========================================================================
    // encode_float_double tests
    // =========================================================================

    #[test]
    fn test_encode_float_double_positive() {
        let mut buf = [0u8; 8];
        encode_float_double(1.0_f64, &mut buf).unwrap();
        assert_eq!(buf, [0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_double_negative() {
        let mut buf = [0u8; 8];
        encode_float_double(-1.0_f64, &mut buf).unwrap();
        assert_eq!(buf, [0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_double_zero() {
        let mut buf = [0u8; 8];
        encode_float_double(0.0_f64, &mut buf).unwrap();
        assert_eq!(buf, [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_encode_float_double_buffer_too_small() {
        let mut buf = [0u8; 7];
        let result = encode_float_double(1.0_f64, &mut buf);
        assert!(result.is_err());
    }

    #[test]
    fn test_encode_float_double_ibm_hex_positive() {
        let mut buf = [0u8; 8];
        encode_float_double_ibm_hex(1.0_f64, &mut buf).unwrap();
        assert_eq!(buf, [0x41, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
    }

    #[test]
    fn test_ibm_hex_encode_rejects_non_finite() {
        let mut f32_buf = [0u8; 4];
        let mut f64_buf = [0u8; 8];
        assert!(encode_float_single_ibm_hex(f32::NAN, &mut f32_buf).is_err());
        assert!(encode_float_double_ibm_hex(f64::INFINITY, &mut f64_buf).is_err());
    }

    // =========================================================================
    // Roundtrip tests
    // =========================================================================

    #[test]
    fn test_roundtrip_float_single() {
        let values: &[f32] = &[
            0.0,
            1.0,
            -1.0,
            std::f32::consts::PI,
            f32::MAX,
            f32::MIN,
            f32::MIN_POSITIVE,
            1.23e-10,
            9.99e30,
        ];
        for &v in values {
            let mut buf = [0u8; 4];
            encode_float_single(v, &mut buf).unwrap();
            let decoded = decode_float_single(&buf).unwrap();
            assert!(
                (decoded - v).abs() < f32::EPSILON || (decoded == 0.0 && v == 0.0),
                "Roundtrip failed for f32 value {}: decoded as {}",
                v,
                decoded
            );
        }
    }

    #[test]
    fn test_roundtrip_float_double() {
        let values: &[f64] = &[
            0.0,
            1.0,
            -1.0,
            std::f64::consts::PI,
            f64::MAX,
            f64::MIN,
            f64::MIN_POSITIVE,
            1.23e-100,
            9.99e200,
        ];
        for &v in values {
            let mut buf = [0u8; 8];
            encode_float_double(v, &mut buf).unwrap();
            let decoded = decode_float_double(&buf).unwrap();
            assert!(
                (decoded - v).abs() < f64::EPSILON || (decoded == 0.0 && v == 0.0),
                "Roundtrip failed for f64 value {}: decoded as {}",
                v,
                decoded
            );
        }
    }

    #[test]
    fn test_roundtrip_float_single_nan() {
        let mut buf = [0u8; 4];
        encode_float_single(f32::NAN, &mut buf).unwrap();
        let decoded = decode_float_single(&buf).unwrap();
        assert!(decoded.is_nan());
    }

    #[test]
    fn test_roundtrip_float_double_nan() {
        let mut buf = [0u8; 8];
        encode_float_double(f64::NAN, &mut buf).unwrap();
        let decoded = decode_float_double(&buf).unwrap();
        assert!(decoded.is_nan());
    }

    #[test]
    fn test_roundtrip_float_single_infinity() {
        let mut buf = [0u8; 4];
        encode_float_single(f32::INFINITY, &mut buf).unwrap();
        let decoded = decode_float_single(&buf).unwrap();
        assert!(decoded.is_infinite() && decoded.is_sign_positive());

        encode_float_single(f32::NEG_INFINITY, &mut buf).unwrap();
        let decoded = decode_float_single(&buf).unwrap();
        assert!(decoded.is_infinite() && decoded.is_sign_negative());
    }

    #[test]
    fn test_roundtrip_float_double_infinity() {
        let mut buf = [0u8; 8];
        encode_float_double(f64::INFINITY, &mut buf).unwrap();
        let decoded = decode_float_double(&buf).unwrap();
        assert!(decoded.is_infinite() && decoded.is_sign_positive());

        encode_float_double(f64::NEG_INFINITY, &mut buf).unwrap();
        let decoded = decode_float_double(&buf).unwrap();
        assert!(decoded.is_infinite() && decoded.is_sign_negative());
    }

    // =========================================================================
    // Extra data in buffer (valid: only first N bytes are used)
    // =========================================================================

    #[test]
    fn test_decode_float_single_extra_data() {
        // 8 bytes provided, only first 4 used
        let data: [u8; 8] = [0x3F, 0x80, 0x00, 0x00, 0xFF, 0xFF, 0xFF, 0xFF];
        let result = decode_float_single(&data).unwrap();
        assert!((result - 1.0_f32).abs() < f32::EPSILON);
    }

    #[test]
    fn test_decode_float_double_extra_data() {
        // 16 bytes provided, only first 8 used
        let mut data = [0xFFu8; 16];
        data[..8].copy_from_slice(&[0x3F, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]);
        let result = decode_float_double(&data).unwrap();
        assert!((result - 1.0_f64).abs() < f64::EPSILON);
    }

    #[test]
    fn test_dispatch_decode_float_single_by_format() {
        let ieee = [0x3F, 0x80, 0x00, 0x00];
        let ibm = [0x41, 0x10, 0x00, 0x00];

        let ieee_val = decode_float_single_with_format(&ieee, FloatFormat::IeeeBigEndian).unwrap();
        let ibm_val = decode_float_single_with_format(&ibm, FloatFormat::IbmHex).unwrap();
        assert!((ieee_val - 1.0_f32).abs() < f32::EPSILON);
        assert!((ibm_val - 1.0_f32).abs() < f32::EPSILON);
    }

    #[test]
    fn test_dispatch_encode_float_single_by_format() {
        let mut ieee = [0u8; 4];
        let mut ibm = [0u8; 4];

        encode_float_single_with_format(1.0_f32, &mut ieee, FloatFormat::IeeeBigEndian).unwrap();
        encode_float_single_with_format(1.0_f32, &mut ibm, FloatFormat::IbmHex).unwrap();

        assert_eq!(ieee, [0x3F, 0x80, 0x00, 0x00]);
        assert_eq!(ibm, [0x41, 0x10, 0x00, 0x00]);
    }

    #[test]
    fn test_dispatch_roundtrip_float_double_by_format() {
        let value = 0.15625_f64;
        let mut ieee = [0u8; 8];
        let mut ibm = [0u8; 8];

        encode_float_double_with_format(value, &mut ieee, FloatFormat::IeeeBigEndian).unwrap();
        encode_float_double_with_format(value, &mut ibm, FloatFormat::IbmHex).unwrap();

        let ieee_decoded =
            decode_float_double_with_format(&ieee, FloatFormat::IeeeBigEndian).unwrap();
        let ibm_decoded = decode_float_double_with_format(&ibm, FloatFormat::IbmHex).unwrap();

        assert!((ieee_decoded - value).abs() < f64::EPSILON);
        assert!((ibm_decoded - value).abs() < f64::EPSILON);
    }

    #[test]
    fn test_decode_record_uses_ibm_float_option() {
        enable_comp_flags_for_codec_tests();
        let schema = copybook_core::parse_copybook("01 REC.\n 05 RATE COMP-1.").unwrap();
        let options = DecodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            json_number_mode: JsonNumberMode::Native,
            emit_filler: false,
            emit_meta: false,
            emit_raw: RawMode::Off,
            strict_mode: false,
            max_errors: None,
            on_decode_unmappable: UnmappablePolicy::Error,
            threads: 1,
            preserve_zoned_encoding: false,
            preferred_zoned_encoding: ZonedEncodingFormat::Auto,
            float_format: FloatFormat::IbmHex,
        };

        let value =
            copybook_codec::decode_record(&schema, &[0x41, 0x10, 0x00, 0x00], &options).unwrap();
        assert_eq!(value["RATE"], serde_json::json!(1.0));
    }

    #[test]
    fn test_encode_record_uses_ibm_float_option() {
        enable_comp_flags_for_codec_tests();
        let schema = copybook_core::parse_copybook("01 REC.\n 05 RATE COMP-1.").unwrap();
        let options = EncodeOptions {
            format: RecordFormat::Fixed,
            codepage: Codepage::CP037,
            preferred_zoned_encoding: ZonedEncodingFormat::Auto,
            use_raw: false,
            bwz_encode: false,
            strict_mode: false,
            max_errors: None,
            threads: 1,
            coerce_numbers: false,
            on_encode_unmappable: UnmappablePolicy::Error,
            json_number_mode: JsonNumberMode::Native,
            zoned_encoding_override: None,
            float_format: FloatFormat::IbmHex,
        };

        let json = serde_json::json!({ "RATE": 1.0 });
        let encoded = copybook_codec::encode_record(&schema, &json, &options).unwrap();
        assert_eq!(encoded, vec![0x41, 0x10, 0x00, 0x00]);
    }
}
