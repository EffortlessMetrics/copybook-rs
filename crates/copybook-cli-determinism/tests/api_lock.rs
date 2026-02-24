// SPDX-License-Identifier: AGPL-3.0-or-later
use copybook_cli_determinism::{
    CommonDeterminismArgs, DecodeDeterminismArgs, DeterminismCommand, DeterminismModeCommand,
    DeterminismRun, DeterminismVerdict, EncodeDeterminismArgs, OutputFormat, build_decode_options,
    build_encode_options, render_human_result, render_json_result,
};
use copybook_codec::{Codepage, JsonNumberMode, RecordFormat};
use copybook_determinism::{DeterminismMode, DeterminismResult};
use std::path::PathBuf;

#[allow(clippy::unwrap_used)]
#[test]
fn command_facade_surface_is_stable() {
    let common = CommonDeterminismArgs {
        copybook: PathBuf::from("schema.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: 16,
    };

    let decode = DecodeDeterminismArgs {
        common: common.clone(),
        data: PathBuf::from("input.bin"),
    };
    let encode = EncodeDeterminismArgs {
        common,
        json: PathBuf::from("input.jsonl"),
    };

    let _decode_command = DeterminismCommand {
        mode: DeterminismModeCommand::Decode(decode),
    };
    let _encode_command = DeterminismCommand {
        mode: DeterminismModeCommand::Encode(encode),
    };

    assert_eq!(OutputFormat::Human, OutputFormat::Human);
    assert_eq!(OutputFormat::Json, OutputFormat::Json);
    assert_eq!(DeterminismVerdict::Deterministic.exit_code(), 0);
    assert_eq!(DeterminismVerdict::NonDeterministic.exit_code(), 2);

    let _decode_opts = build_decode_options(&CommonDeterminismArgs {
        copybook: PathBuf::from("schema.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: 16,
    });

    let _encode_opts = build_encode_options(&CommonDeterminismArgs {
        copybook: PathBuf::from("schema.cpy"),
        format: RecordFormat::Fixed,
        codepage: Codepage::CP037,
        json_number: JsonNumberMode::Lossless,
        emit_meta: false,
        output: OutputFormat::Human,
        max_diffs: 16,
    });

    let result = DeterminismResult {
        mode: DeterminismMode::DecodeOnly,
        round1_hash: "a".repeat(64),
        round2_hash: "a".repeat(64),
        is_deterministic: true,
        byte_differences: None,
    };
    let json = render_json_result(&result).unwrap();
    assert!(json.contains("\"is_deterministic\": true"));

    let human = render_human_result(&result, 16);
    assert!(human.contains("Determinism mode"));
    assert!(human.contains("Byte differences"));
    let _run = DeterminismRun {
        verdict: DeterminismVerdict::Deterministic,
        output: String::new(),
    };
}
