//! Runtime orchestration for the CLI binary.
//!
//! The top-level runner coordinates single-purpose submodules for parsing,
//! startup configuration, command dispatch, and final diagnostics.

mod cli_parse;
mod dispatch;
mod finalize;
mod startup_trace;

use super::*;

struct RuntimeContext {
    command: Commands,
    strict_policy: bool,
    verbose: bool,
}

pub(crate) fn run() -> anyhow::Result<ExitCode> {
    assert_test_panic_requested();

    let cli = match cli_parse::parse_cli() {
        Ok(cli) => cli,
        Err(exit_code) => return Ok(exit_code),
    };

    #[cfg(feature = "metrics")]
    let metrics_opts = cli.metrics.clone();

    #[cfg(feature = "metrics")]
    let metrics_server = metrics_start_if_requested(&metrics_opts)?;

    #[cfg(feature = "metrics")]
    if metrics_server.is_some() {
        describe_metrics_once();
    }

    #[cfg(feature = "metrics")]
    let _metrics_guard = metrics_grace_guard(&metrics_opts);

    let Some(context) = prepare_runtime_context(cli)? else {
        return Ok(ExitCode::Ok);
    };
    startup_trace::initialize(context.verbose, context.strict_policy);

    let command_status = dispatch::execute(context.command, context.strict_policy);

    #[cfg(feature = "metrics")]
    if let (Err(err), Some((handle, _))) = (&command_status.result, &metrics_server) {
        let records_processed = metrics_records_total(handle);
        bump_error_if_pre_run(err, records_processed);
    }

    finalize::emit_command_exit(command_status)
}

fn prepare_runtime_context(cli: Cli) -> anyhow::Result<Option<RuntimeContext>> {
    let feature_flags = initialize_feature_flags(&cli.feature_flags)?;
    copybook_core::feature_flags::FeatureFlags::set_global(feature_flags.clone());

    if cli.feature_flags.list_features {
        list_all_features(&feature_flags);
        return Ok(None);
    }

    let strict_policy = effective_strict_policy(&cli);
    let verbose = cli.verbose || feature_flags.is_enabled(Feature::VerboseLogging);
    Ok(Some(RuntimeContext {
        command: cli.command,
        strict_policy,
        verbose,
    }))
}

fn assert_test_panic_requested() {
    assert!(
        !std::env::var("COPYBOOK_TEST_PANIC").is_ok_and(|value| value == "1"),
        "COPYBOOK_TEST_PANIC triggered"
    );
}
