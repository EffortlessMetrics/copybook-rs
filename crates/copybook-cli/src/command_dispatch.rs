// SPDX-License-Identifier: AGPL-3.0-or-later
//! Subcommand routing for the copybook CLI.
//!
//! Keeping this match out of `main` makes process lifecycle, diagnostics, and
//! feature setup independent from per-command argument adaptation.

use crate::cli_config::effective_dialect;
use crate::exit_codes::ExitCode;
use crate::utils::effective_error_policy;
use crate::{Commands, commands};
use anyhow::anyhow;
use copybook::core::FeatureFlags;
use std::convert::TryFrom;

type CommandOutcome = (anyhow::Result<ExitCode>, &'static str);

/// Render a `--profile` load/resolution failure as structured diagnostics.
///
/// Profile problems are invalid run configuration (validation failure,
/// exit 3), never an internal error: the operator must edit the flag or
/// the profile so they agree.
fn profile_failure(
    op: &'static str,
    error: &crate::profile_inputs::ProfileInputError,
) -> CommandOutcome {
    let message = match error {
        crate::profile_inputs::ProfileInputError::Conflict(_) => {
            format!("{error} (edit the profile or the corresponding command flag so they agree)")
        }
        _ => error.to_string(),
    };
    let diagnostics = crate::ExitDiagnostics::new(
        ExitCode::Encode,
        &message,
        op,
        "", // op_stage will be overridden by emit_exit_diagnostics_stage
        tracing::Level::ERROR,
        ExitCode::Encode.as_i32(),
    )
    .with_subcode(Some(error.subcode()));
    crate::emit_exit_diagnostics_stage(&diagnostics, crate::Stage::Execute);
    (Ok(ExitCode::Encode), op)
}

pub(crate) fn run_command(
    command: Commands,
    strict_policy: bool,
    feature_flags: &FeatureFlags,
    verbose: bool,
) -> CommandOutcome {
    match command {
        command @ Commands::Parse { .. } => run_parse_command(command, feature_flags),
        command @ Commands::Inspect { .. } => run_inspect_command(command, feature_flags),
        command @ Commands::Decode { .. } => {
            run_decode_command(command, strict_policy, feature_flags)
        }
        command @ Commands::Encode { .. } => run_encode_command(command, feature_flags),
        #[cfg(feature = "audit")]
        command @ Commands::Audit { .. } => run_audit_command(command, feature_flags),
        command @ Commands::Verify { .. } => run_verify_command(command, feature_flags),
        Commands::Support { args } => (commands::support::run(&args, feature_flags), "support"),
        Commands::Determinism { command } => (
            commands::determinism::run(&command, feature_flags),
            "determinism",
        ),
        Commands::Doctor {
            copybook,
            input,
            format,
            codepage,
            sample,
            json,
            strict_comments,
            dialect,
            emit_profile,
        } => (
            commands::doctor::run(
                &copybook,
                input,
                format,
                codepage,
                sample,
                json,
                strict_comments,
                crate::cli_config::effective_dialect(dialect),
                verbose,
                emit_profile,
                dialect,
            ),
            "doctor",
        ),
        Commands::Explain {
            code,
            format,
            copybook,
            input,
            record,
            record_format,
            codepage,
            strict,
            strict_comments,
            dialect,
        } => (
            commands::explain::run(&commands::explain::ExplainArgs {
                code,
                format,
                copybook,
                input,
                record,
                record_format,
                codepage,
                strict,
                strict_comments,
                dialect: crate::cli_config::effective_dialect(dialect).into(),
                feature_flags,
            }),
            "explain",
        ),
        Commands::Compat {
            base,
            head,
            record_format,
            codepage,
            dialect,
            fail_on,
            format,
        } => (
            commands::compat::run(
                &base,
                &head,
                &record_format.to_string(),
                &codepage.to_string(),
                dialect,
                fail_on,
                format,
            ),
            "compat",
        ),
    }
}

fn dispatch_mismatch(operation: &'static str) -> CommandOutcome {
    (
        Err(anyhow!(
            "internal command dispatch mismatch for {operation}"
        )),
        operation,
    )
}

fn run_parse_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Parse {
        copybook,
        output,
        strict,
        strict_comments,
        dialect,
    } = command
    else {
        return dispatch_mismatch("parse");
    };

    let effective_dialect = effective_dialect(dialect);
    (
        commands::parse::run(
            &copybook,
            output,
            strict,
            strict_comments,
            effective_dialect,
            feature_flags,
        ),
        "parse",
    )
}

fn run_inspect_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Inspect {
        copybook,
        codepage,
        strict,
        strict_comments,
        dialect,
    } = command
    else {
        return dispatch_mismatch("inspect");
    };

    let effective_dialect = effective_dialect(dialect);
    (
        commands::inspect::run(
            &copybook,
            codepage,
            strict,
            strict_comments,
            effective_dialect,
            feature_flags,
        ),
        "inspect",
    )
}

fn run_decode_command(
    command: Commands,
    strict_policy: bool,
    feature_flags: &FeatureFlags,
) -> CommandOutcome {
    let Commands::Decode {
        copybook,
        input,
        output,
        profile,
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
    } = command
    else {
        return dispatch_mismatch("decode");
    };

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("decode", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("decode", &error),
    };
    let decode_only = match crate::profile_inputs::resolve_decode(
        json_number,
        on_decode_unmappable,
        loaded.as_ref(),
    ) {
        Ok(decode_only) => decode_only,
        Err(error) => return profile_failure("decode", &error),
    };
    // Same inputs as `decode::run`'s own error-policy computation, so the
    // direct (profile-less) policy agrees with the run by construction.
    let strict_mode = effective_error_policy(strict, fail_fast, common.max_errors).strict_mode;
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict_mode)
    {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "decode",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    (
        commands::decode::run(&commands::decode::DecodeArgs {
            copybook: &copybook,
            input: &input,
            output: &output,
            format: common.format,
            codepage: common.codepage,
            json_number: decode_only.json_number,
            strict,
            max_errors: common.max_errors,
            fail_fast,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable: decode_only.unmappable,
            threads,
            strict_comments,
            preserve_zoned_encoding,
            preferred_zoned_encoding: preferred_zoned_encoding_cli.into(),
            float_format,
            strict_policy,
            execution_policy,
            dialect: common.dialect,
            select: &select,
            feature_flags,
        }),
        "decode",
    )
}

fn run_encode_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Encode {
        copybook,
        input,
        output,
        profile,
        format,
        codepage,
        use_raw,
        bwz_encode,
        strict,
        max_errors,
        fail_fast,
        no_fail_fast,
        threads,
        coerce_numbers,
        strict_comments,
        zoned_encoding_override,
        float_format,
        dialect,
        select,
    } = command
    else {
        return dispatch_mismatch("encode");
    };

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("encode", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("encode", &error),
    };
    // Same inputs as `encode::run`'s own error-policy computation, so the
    // direct (profile-less) policy agrees with the run by construction.
    let strict_mode =
        effective_error_policy(strict, fail_fast || !no_fail_fast, common.max_errors).strict_mode;
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict_mode)
    {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "encode",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    (
        commands::encode::run(
            &copybook,
            &input,
            &output,
            &commands::encode::EncodeCliOptions {
                format: common.format,
                codepage: common.codepage,
                use_raw,
                bwz_encode,
                strict,
                max_errors: common.max_errors,
                execution_policy,
                // Stopping on the first failure is the default, so only `--no-fail-fast`
                // changes anything. clap rejects the two together, so this cannot be
                // ambiguous.
                fail_fast: fail_fast || !no_fail_fast,
                threads,
                coerce_numbers,
                strict_comments,
                zoned_encoding_override,
                float_format,
                dialect: common.dialect,
                select: &select,
            },
            feature_flags,
        ),
        "encode",
    )
}

#[cfg(feature = "audit")]
fn run_audit_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Audit { audit_command } = command else {
        return dispatch_mismatch("audit");
    };

    let runtime = match tokio::runtime::Runtime::new() {
        Ok(runtime) => runtime,
        Err(err) => return (Err(anyhow!(err)), "audit"),
    };
    (
        runtime
            .block_on(commands::audit::run(audit_command, feature_flags))
            .map_err(|err| anyhow!(err)),
        "audit",
    )
}

fn run_verify_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Verify {
        copybook,
        input,
        report,
        profile,
        format,
        codepage,
        strict,
        max_errors,
        sample,
        strict_comments,
        dialect,
        select,
    } = command
    else {
        return dispatch_mismatch("verify");
    };

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("verify", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("verify", &error),
    };
    // Verify's strict mode is the flag itself, matching `verify::run`.
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict) {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "verify",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    // Verify keeps its historical default budget of 10 when neither a flag
    // nor a profile sets one.
    let value = common.max_errors.unwrap_or(10);
    let Ok(normalized_max_errors) = u32::try_from(value) else {
        return (
            Err(anyhow!(
                "--max-errors must be between 0 and {} (received {value})",
                u32::MAX
            )),
            "verify",
        );
    };

    let opts = commands::verify::VerifyOptions {
        format: common.format,
        codepage: common.codepage,
        strict,
        max_errors: normalized_max_errors,
        sample: sample.unwrap_or(5),
        strict_comments,
        execution_policy,
        dialect: common.dialect,
        select: &select,
    };
    (
        commands::verify::run(&copybook, &input, report, &opts, feature_flags),
        "verify",
    )
}
