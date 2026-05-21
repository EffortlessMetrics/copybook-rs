// SPDX-License-Identifier: AGPL-3.0-or-later
//! Single-responsibility runtime orchestration for the CLI entrypoint.

mod dispatch;
mod finalize;
mod parse;
mod telemetry;

use crate::exit_codes::ExitCode;

pub(crate) fn run() -> anyhow::Result<ExitCode> {
    trigger_test_panic_if_requested();

    let cli = match parse::parse_cli() {
        parse::ParseOutcome::Run(cli) => *cli,
        parse::ParseOutcome::Exit(exit_code) => return Ok(exit_code),
    };

    #[cfg(feature = "metrics")]
    let metrics_opts = cli.metrics.clone();

    #[cfg(feature = "metrics")]
    let metrics_server = crate::metrics_start_if_requested(&metrics_opts)?;

    #[cfg(feature = "metrics")]
    if metrics_server.is_some() {
        crate::describe_metrics_once();
    }

    #[cfg(feature = "metrics")]
    let _metrics_guard = crate::metrics_grace_guard(&metrics_opts);

    let startup = telemetry::configure_runtime(cli)?;
    let (command, strict_policy) = match startup {
        telemetry::RuntimeStartup::Execute {
            command,
            strict_policy,
        } => (command, strict_policy),
        telemetry::RuntimeStartup::Exit(exit_code) => return Ok(exit_code),
    };
    let execution = dispatch::execute_command(command, strict_policy);

    #[cfg(feature = "metrics")]
    if let (Err(err), Some((handle, _))) = (&execution.status, &metrics_server) {
        let records_processed = crate::metrics_records_total(handle);
        crate::bump_error_if_pre_run(err, records_processed);
    }

    finalize::finalize_execution(execution)
}

fn trigger_test_panic_if_requested() {
    assert!(
        !std::env::var("COPYBOOK_TEST_PANIC").is_ok_and(|v| v == "1"),
        "COPYBOOK_TEST_PANIC triggered"
    );
}
