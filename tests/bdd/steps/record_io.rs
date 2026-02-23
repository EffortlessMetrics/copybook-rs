// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_codec::RecordFormat;
use copybook_record_io::{read_record, write_record};
use cucumber::then;
use std::io::Cursor;

use crate::world::CopybookWorld;

#[then(
    regex = r"^the encoded output should round-trip through the record I/O microcrate in fixed mode with LRECL (\d+)$"
)]
async fn then_encoded_output_roundtrips_record_io_fixed(world: &mut CopybookWorld, lrecl: u32) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set")
        .clone();

    let mut cursor = Cursor::new(output.clone());
    let payload = read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
        .expect("record-io fixed read should succeed")
        .expect("one fixed record expected");

    let mut rebuilt = Vec::new();
    write_record(&mut rebuilt, &payload, RecordFormat::Fixed)
        .expect("record-io fixed write should succeed");
    assert_eq!(
        rebuilt, output,
        "fixed record-io round-trip should be stable"
    );

    assert!(
        read_record(&mut cursor, RecordFormat::Fixed, Some(lrecl))
            .expect("fixed EOF read should succeed")
            .is_none(),
        "fixed stream should contain exactly one record for this scenario"
    );
}

#[then(
    regex = r"^the encoded output should round-trip through the record I/O microcrate in RDW mode$"
)]
async fn then_encoded_output_roundtrips_record_io_rdw(world: &mut CopybookWorld) {
    let output = world
        .encoded_output
        .as_ref()
        .expect("Encoded output not set")
        .clone();

    let mut cursor = Cursor::new(output.clone());
    let payload = read_record(&mut cursor, RecordFormat::RDW, None)
        .expect("record-io rdw read should succeed")
        .expect("one rdw record expected");

    let mut rebuilt = Vec::new();
    write_record(&mut rebuilt, &payload, RecordFormat::RDW)
        .expect("record-io rdw write should succeed");
    assert_eq!(rebuilt, output, "rdw record-io round-trip should be stable");

    assert!(
        read_record(&mut cursor, RecordFormat::RDW, None)
            .expect("rdw EOF read should succeed")
            .is_none(),
        "rdw stream should contain exactly one record for this scenario"
    );
}
