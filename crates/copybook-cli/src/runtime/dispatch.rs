use std::convert::TryFrom;

use super::{Commands, ExitCode, effective_dialect};

pub(super) struct CommandStatus {
    pub(super) result: anyhow::Result<ExitCode>,
    pub(super) op: &'static str,
}

pub(super) fn execute(command: Commands, strict_policy: bool) -> CommandStatus {
    match command {
        Commands::Parse { .. } | Commands::Inspect { .. } => execute_schema_command(command),
        Commands::Decode { .. } | Commands::Encode { .. } => {
            execute_codec_command(command, strict_policy)
        }
        #[cfg(feature = "audit")]
        Commands::Audit { .. } => execute_support_command(command),
        Commands::Verify { .. } | Commands::Support { .. } | Commands::Determinism { .. } => {
            execute_support_command(command)
        }
    }
}

fn execute_schema_command(command: Commands) -> CommandStatus {
    match command {
        Commands::Parse {
            copybook,
            output,
            strict,
            strict_comments,
            dialect,
        } => {
            let effective_dialect = effective_dialect(dialect);
            command_status(
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
            command_status(
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
        _ => command_routing_error("schema"),
    }
}

fn execute_codec_command(command: Commands, strict_policy: bool) -> CommandStatus {
    match command {
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
            command_status(
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
            command_status(
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
        _ => command_routing_error("codec"),
    }
}

fn execute_support_command(command: Commands) -> CommandStatus {
    match command {
        #[cfg(feature = "audit")]
        Commands::Audit { audit_command } => {
            let result = tokio::runtime::Runtime::new().map_or_else(
                |err| Err(anyhow::Error::from(err)),
                |runtime| {
                    runtime
                        .block_on(crate::commands::audit::run(audit_command))
                        .map_err(|err| anyhow::anyhow!(err))
                },
            );
            command_status(result, "audit")
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
            let result = (|| {
                let normalized_max_errors = normalize_max_errors(max_errors)?;
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
            })();
            command_status(result, "verify")
        }
        Commands::Support { args } => {
            command_status(crate::commands::support::run(&args), "support")
        }
        Commands::Determinism { command } => {
            command_status(crate::commands::determinism::run(&command), "determinism")
        }
        _ => command_routing_error("support"),
    }
}

fn command_status(result: anyhow::Result<ExitCode>, op: &'static str) -> CommandStatus {
    CommandStatus { result, op }
}

fn command_routing_error(route: &'static str) -> CommandStatus {
    command_status(
        Err(anyhow::anyhow!(
            "command routed to {route} dispatcher unexpectedly"
        )),
        "dispatch",
    )
}

fn normalize_max_errors(max_errors: Option<u64>) -> anyhow::Result<u32> {
    let value = max_errors.unwrap_or(10);
    u32::try_from(value).map_err(|_| {
        anyhow::anyhow!(
            "--max-errors must be between 0 and {} (received {value})",
            u32::MAX
        )
    })
}
