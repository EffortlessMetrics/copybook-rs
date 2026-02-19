use copybook_codec::decode_file_to_jsonl;
use cucumber::{given, then, when};
use std::io::Cursor;

use crate::helpers::*;
use crate::world::CopybookWorld;

#[given(expr = "thread count {int}")]
async fn given_thread_count(world: &mut CopybookWorld, count: usize) {
    world.ensure_decode_options();
    world.ensure_encode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_threads(count));
    }
    if let Some(options) = world.encode_options.take() {
        world.encode_options = Some(options.with_threads(count));
    }
}

#[given(expr = "multi-record binary data with {int} records")]
async fn given_multi_record_data(world: &mut CopybookWorld, count: usize) {
    if !world.ensure_schema_and_return() {
        return;
    }
    let schema = world.schema();
    let record_len = schema.lrecl_fixed.unwrap_or(10) as usize;
    let mut data = Vec::with_capacity(record_len * count);
    for i in 0..count {
        let mut record = vec![b' '; record_len];
        // Put a record identifier in first bytes
        let id = format!("{:0>width$}", i + 1, width = record_len.min(10));
        let id_bytes = id.as_bytes();
        let copy_len = id_bytes.len().min(record_len);
        record[..copy_len].copy_from_slice(&id_bytes[..copy_len]);
        data.extend_from_slice(&record);
    }
    world.binary_data = Some(data);
    world.record_count = Some(count);
}

#[when(expr = "the data is decoded with {int} thread(s)")]
async fn when_decoded_with_threads(world: &mut CopybookWorld, threads: usize) {
    if !world.ensure_schema_and_return() {
        return;
    }
    world.ensure_decode_options();
    if let Some(options) = world.decode_options.take() {
        world.decode_options = Some(options.with_threads(threads));
    }

    let binary_data = world
        .binary_data
        .as_ref()
        .expect("Binary data not set")
        .clone();
    let mut output = Vec::new();

    match decode_file_to_jsonl(
        world.schema(),
        Cursor::new(&binary_data),
        &mut output,
        world
            .decode_options
            .as_ref()
            .expect("Decode options not set"),
    ) {
        Ok(_summary) => {
            world.decoded_output = Some(String::from_utf8(output).unwrap());
        }
        Err(e) => {
            world.error = Some(e);
        }
    }
}

#[when(expr = "the output is saved as baseline")]
async fn when_output_saved_as_baseline(world: &mut CopybookWorld) {
    world.baseline_output = world.decoded_output.clone();
}

#[then(expr = "the multi-threaded output should match baseline")]
async fn then_multi_threaded_matches_baseline(world: &mut CopybookWorld) {
    let baseline = world
        .baseline_output
        .as_ref()
        .expect("Baseline output not set");
    let current = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");

    // Compare record sets (order may differ with threads)
    let mut baseline_records: Vec<&str> =
        baseline.lines().filter(|l| !l.trim().is_empty()).collect();
    let mut current_records: Vec<&str> = current.lines().filter(|l| !l.trim().is_empty()).collect();
    baseline_records.sort_unstable();
    current_records.sort_unstable();

    assert_eq!(
        baseline_records.len(),
        current_records.len(),
        "Record count mismatch: baseline={}, current={}",
        baseline_records.len(),
        current_records.len()
    );

    for (b, c) in baseline_records.iter().zip(current_records.iter()) {
        assert_eq!(
            b, c,
            "Record mismatch between baseline and multi-threaded output"
        );
    }
}

#[then(expr = "{int} records should be decoded")]
async fn then_n_records_decoded(world: &mut CopybookWorld, expected: usize) {
    let output = world
        .decoded_output
        .as_ref()
        .expect("Decoded output not set");
    let count = output.lines().filter(|l| !l.trim().is_empty()).count();
    assert_eq!(
        count, expected,
        "Expected {} records decoded, got {}",
        expected, count
    );
}
