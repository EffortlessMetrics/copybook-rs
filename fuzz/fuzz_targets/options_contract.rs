#![no_main]
// SPDX-License-Identifier: AGPL-3.0-or-later

use copybook_options::{
    DecodeOptions, EncodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat,
    ZonedEncodingFormat,
};
use libfuzzer_sys::fuzz_target;
use serde_json::Value;

fuzz_target!(|data: &[u8]| {
    for &byte in data {
        let _ = ZonedEncodingFormat::detect_from_byte(byte);
    }

    let record_format = if data.first().is_some_and(|b| (b & 1) == 1) {
        RecordFormat::RDW
    } else {
        RecordFormat::Fixed
    };

    let json_mode = if data.get(1).is_some_and(|b| (b & 1) == 1) {
        JsonNumberMode::Native
    } else {
        JsonNumberMode::Lossless
    };

    let raw_mode = match data.get(2).copied().unwrap_or_default() & 0b11 {
        0 => RawMode::Off,
        1 => RawMode::Record,
        2 => RawMode::Field,
        _ => RawMode::RecordRDW,
    };

    let zoned_pref = match data.get(3).copied().unwrap_or_default() % 3 {
        0 => ZonedEncodingFormat::Auto,
        1 => ZonedEncodingFormat::Ascii,
        _ => ZonedEncodingFormat::Ebcdic,
    };

    let float_format = if data.get(4).is_some_and(|b| (b & 1) == 1) {
        FloatFormat::IbmHex
    } else {
        FloatFormat::IeeeBigEndian
    };

    let decode = DecodeOptions::new()
        .with_format(record_format)
        .with_json_number_mode(json_mode)
        .with_emit_raw(raw_mode)
        .with_preferred_zoned_encoding(zoned_pref)
        .with_float_format(float_format);

    let encode = EncodeOptions::new()
        .with_format(record_format)
        .with_json_number_mode(json_mode)
        .with_preferred_zoned_encoding(zoned_pref)
        .with_zoned_encoding_override(Some(zoned_pref))
        .with_float_format(float_format);

    if let Ok(blob) = serde_json::to_vec(&decode) {
        let _ = serde_json::from_slice::<DecodeOptions>(&blob);
    }

    if let Ok(blob) = serde_json::to_vec(&encode) {
        let _ = serde_json::from_slice::<EncodeOptions>(&blob);
    }

    if let Ok(value) = serde_json::from_slice::<Value>(data) {
        if let Ok(decoded) = serde_json::from_value::<DecodeOptions>(value.clone()) {
            let _ = decoded.format.description();
            let _ = decoded.json_number_mode.description();
            let _ = decoded.emit_raw.to_string();
            let _ = serde_json::to_vec(&decoded);
        }

        if let Ok(encoded) = serde_json::from_value::<EncodeOptions>(value) {
            let _ = encoded.format.description();
            let _ = encoded.json_number_mode.description();
            let _ = encoded.preferred_zoned_encoding.description();
            let _ = serde_json::to_vec(&encoded);
        }
    }
});
