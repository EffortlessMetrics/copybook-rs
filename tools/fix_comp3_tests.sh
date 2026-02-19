#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later

FILE="copybook-codec/tests/comp3_property_tests.rs"

# Replace EncodeOptions struct literals
sed -i 's/let enc = EncodeOptions {$/let enc = EncodeOptions::new()/' "$FILE"
sed -i '/let enc = EncodeOptions::new()/,/};$/{
    s/format: RecordFormat::Fixed,/.with_format(RecordFormat::Fixed)/
    s/codepage: Codepage::CP037,/.with_codepage(Codepage::CP037)/
    s/use_raw: false,/.with_use_raw(false)/
    s/bwz_encode: false,/.with_bwz_encode(false)/
    s/strict_mode: true,/.with_strict_mode(true)/
    s/max_errors: None,/.with_max_errors(None)/
    s/threads: 1,/.with_threads(1)/
    s/coerce_numbers: false,/.with_coerce_numbers(false)/
    s/};$/.with_zoned_encoding_override(None);/
}' "$FILE"

# Replace DecodeOptions struct literals
sed -i 's/let dec = DecodeOptions {$/let dec = DecodeOptions::new()/' "$FILE"
sed -i '/let dec = DecodeOptions::new()/,/};$/{
    s/format: RecordFormat::Fixed,/.with_format(RecordFormat::Fixed)/
    s/codepage: Codepage::CP037,/.with_codepage(Codepage::CP037)/
    s/json_number_mode: copybook_codec::JsonNumberMode::Lossless,/.with_json_number_mode(copybook_codec::JsonNumberMode::Lossless)/
    s/emit_filler: false,/.with_emit_filler(false)/
    s/emit_meta: false,/.with_emit_meta(false)/
    s/emit_raw: copybook_codec::RawMode::Off,/.with_emit_raw(copybook_codec::RawMode::Off)/
    s/strict_mode: true,/.with_strict_mode(true)/
    s/max_errors: None,/.with_max_errors(None)/
    s/on_decode_unmappable: copybook_codec::UnmappablePolicy::Error,/.with_unmappable_policy(copybook_codec::UnmappablePolicy::Error)/
    s/threads: 1,/.with_threads(1)/
    s/};$/.with_preserve_zoned_encoding(false).with_preferred_zoned_encoding(copybook_codec::ZonedEncodingFormat::Auto);/
}' "$FILE"

echo "Fixed comp3_property_tests.rs"