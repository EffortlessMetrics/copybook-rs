// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::RecordFormat;
use copybook_rdw::{
    RdwHeader, rdw_read_len, rdw_slice_body, rdw_try_peek_len, rdw_validate_and_finish,
};
use cucumber::{given, then};
use std::io::{BufRead, Cursor};

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

#[then(expr = "the encoded output should round-trip through the RDW microcrate")]
async fn then_encoded_output_roundtrips_through_rdw_microcrate(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set");
    assert!(output.len() >= 4, "Encoded output too short for RDW header");

    let mut cursor = Cursor::new(output.as_slice());
    assert!(
        rdw_try_peek_len(&mut cursor)
            .expect("RDW peek should succeed")
            .is_some(),
        "RDW microcrate should detect header bytes"
    );

    let length = rdw_read_len(&mut cursor).expect("RDW length read should succeed");
    cursor.consume(2);
    let payload = rdw_slice_body(&mut cursor, length).expect("RDW payload slice should succeed");
    let payload = rdw_validate_and_finish(payload);

    let header = RdwHeader::from_bytes(
        output[0..4]
            .try_into()
            .expect("Encoded output should include 4-byte RDW header"),
    );
    assert_eq!(
        usize::from(header.length()),
        payload.len(),
        "RDW header length should match payload length"
    );
}
