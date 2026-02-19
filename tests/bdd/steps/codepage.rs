// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::Codepage;
use cucumber::given;

use crate::world::CopybookWorld;

#[given(expr = "codepage {string}")]
async fn given_codepage(world: &mut CopybookWorld, codepage_str: String) {
    let codepage = match codepage_str.to_uppercase().as_str() {
        "CP037" | "EBCDIC" => Codepage::CP037,
        "CP273" => Codepage::CP273,
        "CP500" => Codepage::CP500,
        "CP1047" => Codepage::CP1047,
        "CP1140" => Codepage::CP1140,
        "ASCII" => Codepage::ASCII,
        _ => panic!("Unknown codepage: {}", codepage_str),
    };

    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_codepage(codepage));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_codepage(codepage));
    }
}
