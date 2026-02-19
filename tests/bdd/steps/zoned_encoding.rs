// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::ZonedEncodingFormat;
use cucumber::given;

use crate::world::CopybookWorld;

#[given(expr = "zoned encoding {string}")]
async fn given_zoned_encoding(world: &mut CopybookWorld, encoding: String) {
    let format = match encoding.to_lowercase().as_str() {
        "auto" => ZonedEncodingFormat::Auto,
        "ascii" => ZonedEncodingFormat::Ascii,
        "ebcdic" => ZonedEncodingFormat::Ebcdic,
        _ => panic!("Unknown zoned encoding format: {}", encoding),
    };

    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_preferred_zoned_encoding(format));
    }
}

#[given(expr = "zoned encoding override {string}")]
async fn given_zoned_encoding_override(world: &mut CopybookWorld, encoding: String) {
    let format = match encoding.to_lowercase().as_str() {
        "auto" => Some(ZonedEncodingFormat::Auto),
        "ascii" => Some(ZonedEncodingFormat::Ascii),
        "ebcdic" => Some(ZonedEncodingFormat::Ebcdic),
        "none" => None,
        _ => panic!("Unknown zoned encoding format: {}", encoding),
    };

    world.ensure_encode_options();
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_zoned_encoding_override(format));
    }
}

#[given(expr = "preserve zoned encoding enabled")]
async fn given_preserve_zoned_encoding(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_preserve_zoned_encoding(true));
    }
}

#[given(expr = "preferred zoned encoding {string}")]
async fn given_preferred_zoned_encoding(world: &mut CopybookWorld, encoding: String) {
    let format = match encoding.to_lowercase().as_str() {
        "auto" => ZonedEncodingFormat::Auto,
        "ascii" => ZonedEncodingFormat::Ascii,
        "ebcdic" => ZonedEncodingFormat::Ebcdic,
        _ => panic!("Unknown zoned encoding format: {}", encoding),
    };

    world.ensure_encode_options();
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_zoned_encoding_format(format));
    }
}
