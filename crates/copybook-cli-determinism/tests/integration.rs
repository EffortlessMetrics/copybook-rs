// SPDX-License-Identifier: AGPL-3.0-or-later
#![allow(clippy::expect_used)]

use copybook_cli_determinism::{
    CommonDeterminismArgs, DecodeDeterminismArgs, DeterminismCommand, DeterminismModeCommand,
    DeterminismVerdict, EncodeDeterminismArgs, OutputFormat, RoundTripDeterminismArgs, run,
};
use copybook_codec::{Codepage, JsonNumberMode, RecordFormat};
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;

const SIMPLE_COPYBOOK: &str = r"
       01 RECORD.
          05 FIELD PIC X(5).
";

const SIMPLE_DATA_CP037: [u8; 5] = [0xC1, 0xC2, 0xC3, 0xC4, 0xC5];

fn write_simple_fixture(tmp_dir: &tempfile::TempDir) -> (PathBuf, PathBuf, PathBuf) {
    let copybook_path = tmp_dir.path().join("schema.cpy");
    let data_path = tmp_dir.path().join("input.bin");
    let json_path = tmp_dir.path().join("input.jsonl");

    fs::write(&copybook_path, SIMPLE_COPYBOOK).expect("write copybook");
    fs::write(&data_path, SIMPLE_DATA_CP037).expect("write data");
    fs::write(&json_path, "{\"FIELD\":\"ABCDE\"}\n").expect("write json");

    (copybook_path, data_path, json_path)
}

fn common_args(copybook: PathBuf) -> CommonDeterminismArgs {
    CommonDeterminismArgs {
        copybook,
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: 100,
    }
}

#[test]
fn decode_determinism_command_reports_success_human_output() {
    let tmp = tempdir().expect("temp dir");
    let (copybook, data, _) = write_simple_fixture(&tmp);

    let command = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: common_args(copybook),
            data,
        }),
    };

    let result = run(&command).expect("determinism decode run");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert!(result.output.contains("DETERMINISTIC"));
}

#[test]
fn decode_determinism_command_supports_json_output() {
    let tmp = tempdir().expect("temp dir");
    let (copybook, data, _) = write_simple_fixture(&tmp);
    let mut common = common_args(copybook);
    common.output = OutputFormat::Json;

    let command = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs { common, data }),
    };

    let result = run(&command).expect("determinism decode run");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);

    let value: serde_json::Value = serde_json::from_str(&result.output).expect("json output");
    assert_eq!(value["is_deterministic"], serde_json::Value::Bool(true));
    assert!(value["round1_hash"].as_str().is_some());
    assert_eq!(value["round1_hash"], value["round2_hash"]);
}

#[test]
fn encode_determinism_command_is_deterministic() {
    let tmp = tempdir().expect("temp dir");
    let (copybook, _, json) = write_simple_fixture(&tmp);

    let command = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(EncodeDeterminismArgs {
            common: common_args(copybook),
            json,
        }),
    };

    let result = run(&command).expect("determinism encode run");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert!(result.output.contains("Round 1 hash"));
}

#[test]
fn round_trip_determinism_command_is_deterministic() {
    let tmp = tempdir().expect("temp dir");
    let (copybook, data, _) = write_simple_fixture(&tmp);

    let command = DeterminismCommand {
        mode: DeterminismModeCommand::RoundTrip(RoundTripDeterminismArgs {
            common: common_args(copybook),
            data,
        }),
    };

    let result = run(&command).expect("determinism round-trip run");
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
    assert!(result.output.contains("Byte differences"));
}

#[test]
fn run_return_code_map_is_stable() {
    let tmp = tempdir().expect("temp dir");
    let (copybook, data, _) = write_simple_fixture(&tmp);

    let command = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(DecodeDeterminismArgs {
            common: common_args(copybook),
            data,
        }),
    };

    let result = run(&command).expect("determinism decode should return a run");
    assert_eq!(result.exit_code(), 0);
    assert_eq!(result.verdict, DeterminismVerdict::Deterministic);
}
