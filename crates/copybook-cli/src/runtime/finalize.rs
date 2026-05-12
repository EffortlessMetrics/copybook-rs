// SPDX-License-Identifier: AGPL-3.0-or-later
//! Exit diagnostics and status finalization.

use super::dispatch::CommandExecution;
use crate::exit_codes::ExitCode;
use crate::{ExitDiagnostics, Stage, emit_exit_diagnostics_stage};
use tracing::Level;

pub(crate) fn finalize_execution(execution: CommandExecution) -> anyhow::Result<ExitCode> {
    let status = execution.status?;
    emit_command_diagnostics(status, execution.operation);
    Ok(status)
}

fn emit_command_diagnostics(status: ExitCode, operation: &'static str) {
    let diagnostics = if status == ExitCode::Ok {
        ExitDiagnostics::new(
            ExitCode::Ok,
            "completed",
            operation,
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::INFO,
            0,
        )
    } else {
        ExitDiagnostics::new(
            status,
            "command completed with non-zero exit code",
            operation,
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::ERROR,
            status.as_i32(),
        )
    };

    let stage = if status == ExitCode::Ok {
        Stage::Finalize
    } else {
        Stage::Execute
    };
    emit_exit_diagnostics_stage(&diagnostics, stage);
}
