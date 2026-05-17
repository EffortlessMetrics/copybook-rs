// SPDX-License-Identifier: AGPL-3.0-or-later
//! Decode command implementation

use crate::exit_codes::ExitCode;
use crate::subcode;
use crate::utils::{
    ParseOptionsConfig, determine_exit_code, effective_error_options, parse_projected_schema,
    process_input_to_output, write_processing_summary,
};
use crate::{ExitDiagnostics, Stage, emit_exit_diagnostics_stage};
use copybook_codec::{
    Codepage, DecodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use std::path::{Path, PathBuf};
use tracing::{Level, info};

#[allow(clippy::struct_excessive_bools)]
pub struct DecodeArgs<'a> {
    pub copybook: &'a PathBuf,
    pub input: &'a PathBuf,
    pub output: &'a PathBuf,
    pub format: RecordFormat,
    pub codepage: Codepage,
    pub json_number: JsonNumberMode,
    pub strict: bool,
    pub max_errors: Option<u64>,
    pub fail_fast: bool,
    pub emit_filler: bool,
    pub emit_meta: bool,
    pub emit_raw: RawMode,
    pub on_decode_unmappable: UnmappablePolicy,
    pub threads: usize,
    pub strict_comments: bool,
    pub preserve_zoned_encoding: bool,
    pub preferred_zoned_encoding: copybook_codec::ZonedEncodingFormat,
    pub float_format: FloatFormat,
    pub strict_policy: bool,
    pub dialect: copybook_core::dialect::Dialect,
    pub select: &'a [String],
}

#[allow(clippy::too_many_lines)]
pub fn run(args: &DecodeArgs) -> anyhow::Result<ExitCode> {
    info!("Decoding data file: {:?}", args.input);

    if args.strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    if args.preferred_zoned_encoding != copybook_codec::ZonedEncodingFormat::Auto
        && !args.preserve_zoned_encoding
    {
        let preferred = args.preferred_zoned_encoding;
        let subcode = Some(subcode::POLICY_PREFERRED_WITHOUT_PRESERVE);
        let op_path = Some(args.input.as_path());
        if args.strict_policy {
            let diagnostics = ExitDiagnostics::new(
                ExitCode::Encode,
                "--preferred-zoned-encoding requires --preserve-zoned-encoding in strict mode",
                "decode",
                "", // op_stage will be overridden by emit_exit_diagnostics_stage
                Level::ERROR,
                ExitCode::Encode.as_i32(),
            )
            .with_path(op_path)
            .with_subcode(subcode);
            emit_exit_diagnostics_stage(&diagnostics, Stage::Execute);
            return Ok(ExitCode::Encode);
        }

        let diagnostics = ExitDiagnostics::new(
            ExitCode::Encode,
            "compat: prefer --preserve-zoned-encoding when using --preferred-zoned-encoding (future strict mode will fail)",
            "decode",
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::WARN,
            ExitCode::Ok.as_i32(),
        )
        .with_path(op_path)
        .with_subcode(subcode);
        emit_exit_diagnostics_stage(&diagnostics, Stage::Execute);
        tracing::warn!(
            preferred = ?preferred,
            preserve_zoned_encoding = args.preserve_zoned_encoding,
            "preferred zoned encoding requested without preservation; continuing in compatibility mode"
        );
    }

    let working_schema = parse_projected_schema(
        args.copybook,
        &ParseOptionsConfig {
            strict: args.strict,
            strict_comments: args.strict_comments,
            codepage: &args.codepage.to_string(),
            emit_filler: args.emit_filler,
            dialect: args.dialect,
        },
        args.select,
    )?;

    let (effective_strict_mode, effective_max_errors) =
        effective_error_options(args.strict, args.max_errors, args.fail_fast);

    let options = DecodeOptions::new()
        .with_format(args.format)
        .with_codepage(args.codepage)
        .with_json_number_mode(args.json_number)
        .with_emit_filler(args.emit_filler)
        .with_emit_meta(args.emit_meta)
        .with_emit_raw(args.emit_raw)
        .with_strict_mode(effective_strict_mode)
        .with_max_errors(effective_max_errors)
        .with_unmappable_policy(args.on_decode_unmappable)
        .with_threads(args.threads)
        .with_preserve_zoned_encoding(args.preserve_zoned_encoding)
        .with_preferred_zoned_encoding(args.preferred_zoned_encoding)
        .with_float_format(args.float_format);

    let write_to_stdout = args.output.as_path() == Path::new("-");
    let summary = process_input_to_output(args.input, args.output, |input_file, output_writer| {
        copybook_codec::decode_file_to_jsonl(&working_schema, input_file, output_writer, &options)
            .map_err(Into::into)
    })?;

    if !write_to_stdout {
        write_processing_summary("Decode", &summary, true)?;
    }

    info!("Decode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code =
        determine_exit_code(summary.has_warnings(), summary.has_errors(), ExitCode::Data);
    Ok(exit_code)
}
