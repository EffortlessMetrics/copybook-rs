// SPDX-License-Identifier: AGPL-3.0-or-later
//! CLI parsing and parse-error diagnostics.

use crate::exit_codes::ExitCode;
use crate::{Cli, ExitDiagnostics, Stage, emit_exit_diagnostics_stage};
use clap::Parser;
use clap::error::ErrorKind as ClapErrorKind;
use tracing::Level;

pub(crate) enum ParseOutcome {
    Run(Box<Cli>),
    Exit(ExitCode),
}

pub(crate) fn parse_cli() -> ParseOutcome {
    match Cli::try_parse() {
        Ok(cli) => ParseOutcome::Run(Box::new(cli)),
        Err(err) => {
            let kind = err.kind();
            let _ = err.print();
            if matches!(
                kind,
                ClapErrorKind::DisplayHelp | ClapErrorKind::DisplayVersion
            ) {
                let op = if matches!(kind, ClapErrorKind::DisplayVersion) {
                    "version"
                } else {
                    "help"
                };
                let diagnostics = ExitDiagnostics::new(
                    ExitCode::Ok,
                    "completed",
                    op,
                    "", // op_stage will be overridden by emit_exit_diagnostics_stage
                    Level::INFO,
                    0,
                );
                emit_exit_diagnostics_stage(&diagnostics, Stage::Finalize);
                return ParseOutcome::Exit(ExitCode::Ok);
            }
            let exit_code = ExitCode::Encode;
            let message = err.to_string();
            let diagnostics = ExitDiagnostics::new(
                exit_code,
                &message,
                "cli_parse",
                "", // op_stage will be overridden by emit_exit_diagnostics_stage
                Level::ERROR,
                exit_code.as_i32(),
            )
            .with_error(Some(&err));
            emit_exit_diagnostics_stage(&diagnostics, Stage::Parse);
            ParseOutcome::Exit(exit_code)
        }
    }
}
