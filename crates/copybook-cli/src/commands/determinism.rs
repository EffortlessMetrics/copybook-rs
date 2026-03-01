// SPDX-License-Identifier: AGPL-3.0-or-later
//! Determinism validation command facade.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use anyhow::Context;

pub use copybook_cli_determinism::{DeterminismCommand, DeterminismVerdict, run as run_command};

/// Determinism validation for encode/decode operations.
pub fn run(cmd: &DeterminismCommand) -> anyhow::Result<ExitCode> {
    let result = run_command(cmd).context("Determinism command execution failed")?;
    write_stdout_all(result.output.as_bytes())?;

    let exit_code = match result.verdict {
        DeterminismVerdict::Deterministic => ExitCode::Ok,
        DeterminismVerdict::NonDeterministic => ExitCode::Data,
    };
    Ok(exit_code)
}
