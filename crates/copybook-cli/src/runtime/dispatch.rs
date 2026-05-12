use super::*;

pub(super) struct CommandStatus {
    pub(super) result: anyhow::Result<ExitCode>,
    pub(super) op: &'static str,
}

pub(super) fn execute(command: Commands, strict_policy: bool) -> CommandStatus {
    let (result, op) = match command {
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
            let result = tokio::runtime::Runtime::new().map_or_else(
                |err| Err(anyhow::Error::from(err)),
                |runtime| {
                    runtime
                        .block_on(crate::commands::audit::run(audit_command))
                        .map_err(|err| anyhow::anyhow!(err))
                },
            );
            (result, "audit")
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
            let value = max_errors.unwrap_or(10);
            let normalized_max_errors = match u32::try_from(value) {
                Ok(value) => value,
                Err(_) => {
                    return CommandStatus {
                        result: Err(anyhow::anyhow!(
                            "--max-errors must be between 0 and {} (received {value})",
                            u32::MAX
                        )),
                        op: "verify",
                    };
                }
            };

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
            (
                crate::commands::verify::run(&copybook, &input, report, &opts),
                "verify",
            )
        }
        Commands::Support { args } => (crate::commands::support::run(&args), "support"),
        Commands::Determinism { command } => {
            (crate::commands::determinism::run(&command), "determinism")
        }
    };

    CommandStatus { result, op }
}
