// SPDX-License-Identifier: AGPL-3.0-or-later
//! Subcommand routing for the copybook CLI.
//!
//! Keeping this match out of `main` makes process lifecycle, diagnostics, and
//! feature setup independent from per-command argument adaptation.

use crate::cli_config::effective_dialect;
use crate::exit_codes::ExitCode;
use crate::{Commands, commands};
use anyhow::anyhow;
use std::convert::TryFrom;

type CommandOutcome = (anyhow::Result<ExitCode>, &'static str);

pub(crate) fn run_command(command: Commands, strict_policy: bool) -> CommandOutcome {
    match command {
        command @ Commands::Parse { .. } => run_parse_command(command),
        command @ Commands::Inspect { .. } => run_inspect_command(command),
        command @ Commands::Decode { .. } => run_decode_command(command, strict_policy),
        command @ Commands::Encode { .. } => run_encode_command(command),
        #[cfg(feature = "audit")]
        command @ Commands::Audit { .. } => run_audit_command(command),
        command @ Commands::Verify { .. } => run_verify_command(command),
        Commands::Support { args } => (commands::support::run(&args), "support"),
        Commands::Determinism { command } => (commands::determinism::run(&command), "determinism"),
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

fn run_parse_command(command: Commands) -> CommandOutcome {
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
        ),
        "parse",
    )
}

fn run_inspect_command(command: Commands) -> CommandOutcome {
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
        ),
        "inspect",
    )
}

fn run_decode_command(command: Commands, strict_policy: bool) -> CommandOutcome {
    let Commands::Decode {
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
    } = command
    else {
        return dispatch_mismatch("decode");
    };

    let effective_dialect = effective_dialect(dialect);
    (
        commands::decode::run(&commands::decode::DecodeArgs {
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

fn run_encode_command(command: Commands) -> CommandOutcome {
    let Commands::Encode {
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

    let effective_dialect = effective_dialect(dialect);
    (
        commands::encode::run(
            &copybook,
            &input,
            &output,
            &commands::encode::EncodeCliOptions {
                format,
                codepage,
                use_raw,
                bwz_encode,
                strict,
                max_errors,
                // Stopping on the first failure is the default, so only `--no-fail-fast`
                // changes anything. clap rejects the two together, so this cannot be
                // ambiguous.
                fail_fast: fail_fast || !no_fail_fast,
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
fn run_audit_command(command: Commands) -> CommandOutcome {
    let Commands::Audit { audit_command } = command else {
        return dispatch_mismatch("audit");
    };

    let runtime = match tokio::runtime::Runtime::new() {
        Ok(runtime) => runtime,
        Err(err) => return (Err(anyhow!(err)), "audit"),
    };
    (
        runtime
            .block_on(commands::audit::run(audit_command))
            .map_err(|err| anyhow!(err)),
        "audit",
    )
}

fn run_verify_command(command: Commands) -> CommandOutcome {
    let Commands::Verify {
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
    } = command
    else {
        return dispatch_mismatch("verify");
    };

    let effective_dialect = effective_dialect(dialect);
    let value = max_errors.unwrap_or(10);
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
        commands::verify::run(&copybook, &input, report, &opts),
        "verify",
    )
}
