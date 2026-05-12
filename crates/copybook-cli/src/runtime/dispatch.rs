// SPDX-License-Identifier: AGPL-3.0-or-later
//! Command dispatch: translate parsed CLI variants into command module calls.

use crate::exit_codes::ExitCode;
use crate::{Commands, effective_dialect};
use anyhow::anyhow;
use std::convert::TryFrom;

pub(crate) struct CommandExecution {
    pub(crate) status: anyhow::Result<ExitCode>,
    pub(crate) operation: &'static str,
}

pub(crate) fn execute_command(command: Commands, strict_policy: bool) -> CommandExecution {
    let (status, operation) = match command {
        Commands::Parse {
            copybook,
            output,
            strict,
            strict_comments,
            dialect,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::parse::run(
                    &copybook,
                    output,
                    strict,
                    strict_comments,
                    effective_dialect,
                ),
                "parse",
            )
        }
        Commands::Inspect {
            copybook,
            codepage,
            strict,
            strict_comments,
            dialect,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::inspect::run(
                    &copybook,
                    codepage,
                    strict,
                    strict_comments,
                    effective_dialect,
                ),
                "inspect",
            )
        }
        Commands::Decode {
            copybook,
            input,
            output,
            format,
            codepage,
            json_number,
            strict,
            max_errors,
            fail_fast,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable,
            threads,
            strict_comments,
            preserve_zoned_encoding,
            preferred_zoned_encoding: preferred_zoned_encoding_cli,
            float_format,
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::decode::run(&crate::commands::decode::DecodeArgs {
                    copybook: &copybook,
                    input: &input,
                    output: &output,
                    format,
                    codepage,
                    json_number,
                    strict,
                    max_errors,
                    fail_fast,
                    emit_filler,
                    emit_meta,
                    emit_raw,
                    on_decode_unmappable,
                    threads,
                    strict_comments,
                    preserve_zoned_encoding,
                    preferred_zoned_encoding: preferred_zoned_encoding_cli.into(),
                    float_format,
                    strict_policy,
                    dialect: effective_dialect.into(),
                    select: &select,
                }),
                "decode",
            )
        }
        Commands::Encode {
            copybook,
            input,
            output,
            format,
            codepage,
            use_raw,
            bwz_encode,
            strict,
            max_errors,
            fail_fast,
            threads,
            coerce_numbers,
            strict_comments,
            zoned_encoding_override,
            float_format,
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
            (
                crate::commands::encode::run(
                    &copybook,
                    &input,
                    &output,
                    &crate::commands::encode::EncodeCliOptions {
                        format,
                        codepage,
                        use_raw,
                        bwz_encode,
                        strict,
                        max_errors,
                        fail_fast,
                        threads,
                        coerce_numbers,
                        strict_comments,
                        zoned_encoding_override,
                        float_format,
                        dialect: effective_dialect.into(),
                        select: &select,
                    },
                ),
                "encode",
            )
        }
        #[cfg(feature = "audit")]
        Commands::Audit { audit_command } => {
            let status = match tokio::runtime::Runtime::new() {
                Ok(runtime) => runtime
                    .block_on(crate::commands::audit::run(audit_command))
                    .map_err(|err| anyhow!(err)),
                Err(err) => Err(anyhow!(err)),
            };
            (status, "audit")
        }
        Commands::Verify {
            copybook,
            input,
            report,
            format,
            codepage,
            strict,
            max_errors,
            sample,
            strict_comments,
            dialect,
            select,
        } => {
            let effective_dialect = effective_dialect(dialect);
            let status = normalize_max_errors(max_errors).and_then(|normalized_max_errors| {
                let opts = crate::commands::verify::VerifyOptions {
                    format,
                    codepage,
                    strict,
                    max_errors: normalized_max_errors,
                    sample: sample.unwrap_or(5),
                    strict_comments,
                    dialect: effective_dialect.into(),
                    select: &select,
                };
                crate::commands::verify::run(&copybook, &input, report, &opts)
            });
            (status, "verify")
        }
        Commands::Support { args } => (crate::commands::support::run(&args), "support"),
        Commands::Determinism { command } => {
            (crate::commands::determinism::run(&command), "determinism")
        }
    };

    CommandExecution { status, operation }
}

fn normalize_max_errors(max_errors: Option<u64>) -> anyhow::Result<u32> {
    let value = max_errors.unwrap_or(10);
    u32::try_from(value).map_err(|_| {
        anyhow!(
            "--max-errors must be between 0 and {} (received {value})",
            u32::MAX
        )
    })
}
