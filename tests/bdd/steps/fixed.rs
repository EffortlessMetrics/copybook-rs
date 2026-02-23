// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::RecordFormat;
use copybook_fixed::{FixedRecordReader, FixedRecordWriter};
use cucumber::{given, then};
use std::io::Cursor;

use crate::world::CopybookWorld;

#[given(expr = "fixed record format")]
async fn given_fixed_record_format(world: &mut CopybookWorld) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_format(RecordFormat::Fixed));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_format(RecordFormat::Fixed));
    }
}

#[then(
    regex = r"^the encoded output should round-trip through the fixed microcrate with LRECL (\d+)$"
)]
async fn then_encoded_output_roundtrips_fixed_microcrate(world: &mut CopybookWorld, lrecl: u32) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set")
        .clone();

    let mut reader = FixedRecordReader::new(Cursor::new(output.clone()), Some(lrecl))
        .expect("fixed microcrate reader should build");
    let mut records = Vec::new();
    while let Some(record) = reader
        .read_record()
        .expect("fixed microcrate read should work")
    {
        assert_eq!(
            record.len(),
            lrecl as usize,
            "record length should match LRECL"
        );
        records.push(record);
    }
    assert!(
        !records.is_empty(),
        "fixed microcrate should read at least one record"
    );

    let mut rebuilt = Vec::new();
    let mut writer = FixedRecordWriter::new(&mut rebuilt, Some(lrecl))
        .expect("fixed microcrate writer should build");
    for record in &records {
        writer
            .write_record(record)
            .expect("fixed microcrate writer should round-trip record");
    }
    writer.flush().expect("flush should succeed");

    assert_eq!(
        rebuilt, output,
        "fixed microcrate round-trip should preserve encoded bytes"
    );
}

#[then(regex = r"^the binary input should be readable by the fixed microcrate with LRECL (\d+)$")]
async fn then_binary_input_readable_by_fixed_microcrate(world: &mut CopybookWorld, lrecl: u32) {
    let binary = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();
    let mut reader =
        FixedRecordReader::new(Cursor::new(binary), Some(lrecl)).expect("reader should build");

    let first = reader.read_record().expect("read should succeed");
    assert!(first.is_some(), "expected at least one fixed record");
}
