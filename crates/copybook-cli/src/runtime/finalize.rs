use super::{ExitCode, ExitDiagnostics, Level, Stage, dispatch, emit_exit_diagnostics_stage};

pub(super) fn emit_command_exit(
    command_status: dispatch::CommandStatus,
) -> anyhow::Result<ExitCode> {
    let status = command_status.result?;

    let diagnostics = if status == ExitCode::Ok {
        ExitDiagnostics::new(
            ExitCode::Ok,
            "completed",
            command_status.op,
            "", // op_stage will be overridden by emit_exit_diagnostics_stage
            Level::INFO,
            0,
        )
    } else {
        ExitDiagnostics::new(
            status,
            "command completed with non-zero exit code",
            command_status.op,
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

    Ok(status)
}
