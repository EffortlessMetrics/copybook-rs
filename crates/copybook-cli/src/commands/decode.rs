// SPDX-License-Identifier: AGPL-3.0-or-later
//! Decode command implementation

use crate::exit_codes::ExitCode;
use crate::subcode;
use crate::utils::{
    ParseOptionsConfig, SummaryIssueCountStyle, append_processing_summary, append_record_failures,
    determine_exit_code, effective_error_policy, log_strict_comments, parse_projected_schema,
    run_with_output,
};
use crate::{
    ExitDiagnostics, Stage, emit_exit_diagnostics_stage, write_stderr_all, write_stdout_all,
};
use copybook_codec::{
    Codepage, DecodeOptions, FloatFormat, JsonNumberMode, RawMode, RecordFormat, UnmappablePolicy,
};
use std::path::PathBuf;
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

    log_strict_comments(args.strict_comments);

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

    let codepage = args.codepage.to_string();
    let working_schema = parse_projected_schema(
        args.copybook,
        &ParseOptionsConfig {
            strict: args.strict,
            strict_comments: args.strict_comments,
            codepage: &codepage,
            emit_filler: args.emit_filler,
            dialect: args.dialect,
        },
        args.select,
    )?;

    let error_policy = effective_error_policy(args.strict, args.fail_fast, args.max_errors);

    let options = DecodeOptions::new()
        .with_format(args.format)
        .with_codepage(args.codepage)
        .with_json_number_mode(args.json_number)
        .with_emit_filler(args.emit_filler)
        .with_emit_meta(args.emit_meta)
        .with_emit_raw(args.emit_raw)
        .with_strict_mode(error_policy.strict_mode)
        .with_max_errors(error_policy.max_errors)
        .with_unmappable_policy(args.on_decode_unmappable)
        .with_threads(args.threads)
        .with_preserve_zoned_encoding(args.preserve_zoned_encoding)
        .with_preferred_zoned_encoding(args.preferred_zoned_encoding)
        .with_float_format(args.float_format);

    let (summary, write_to_stdout) =
        run_with_output(args.input, args.output, |input_file, output_writer| {
            Ok(copybook_codec::decode_file_to_jsonl(
                &working_schema,
                input_file,
                output_writer,
                &options,
            )?)
        })?;

    // Print comprehensive summary (only when not writing to stdout)
    if !write_to_stdout {
        let mut summary_output = String::new();
        append_processing_summary(
            &mut summary_output,
            "Decode",
            &summary,
            SummaryIssueCountStyle {
                show_zero_counts: true,
                repeat_nonzero_counts: true,
            },
        )?;

        write_stdout_all(summary_output.as_bytes())?;
    }

    // Failure detail goes to stderr so it survives `-o -` and shell redirection.
    let mut failure_output = String::new();
    append_record_failures(&mut failure_output, &summary)?;
    if !failure_output.is_empty() {
        write_stderr_all(failure_output.as_bytes())?;
    }

    info!("Decode completed successfully");

    // Return appropriate exit code based on normative specification
    let exit_code =
        determine_exit_code(summary.has_warnings(), summary.has_errors(), ExitCode::Data);
    Ok(exit_code)
}
