use copybook_codec::RecordFormat;
use cucumber::{given, then};

use crate::world::CopybookWorld;

#[given(expr = "RDW record format")]
async fn given_rdw_record_format(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_format(RecordFormat::RDW));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_format(RecordFormat::RDW));
    }
}

#[then(expr = "the encoded output should start with RDW header")]
async fn then_encoded_output_starts_rdw(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(output.len() >= 4, "Encoded output too short for RDW header");
    let rdw_len = u16::from_be_bytes([output[0], output[1]]) as usize;
    assert!(rdw_len > 0, "RDW length field should be > 0");
}
