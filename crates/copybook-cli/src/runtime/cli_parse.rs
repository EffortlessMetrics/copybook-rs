use super::*;

pub(super) fn parse_cli() -> Result<Cli, ExitCode> {
    match Cli::try_parse() {
        Ok(cli) => Ok(cli),
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
                return Err(ExitCode::Ok);
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
            Err(exit_code)
        }
    }
}
