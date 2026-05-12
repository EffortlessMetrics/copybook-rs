// SPDX-License-Identifier: AGPL-3.0-or-later
//! Feature-flag, strict-policy, and tracing startup concerns.

use crate::{BrokenPipeSafeStderr, Cli, Commands, effective_strict_policy, invocation_id};
use copybook_core::Feature;
use tracing_subscriber::EnvFilter;

pub(crate) enum RuntimeStartup {
    Execute {
        command: Commands,
        strict_policy: bool,
    },
    Exit(crate::exit_codes::ExitCode),
}

pub(crate) fn configure_runtime(cli: Cli) -> anyhow::Result<RuntimeStartup> {
    let feature_flags = crate::initialize_feature_flags(&cli.feature_flags)?;

    copybook_core::feature_flags::FeatureFlags::set_global(feature_flags.clone());

    if cli.feature_flags.list_features {
        crate::list_all_features(&feature_flags);
        return Ok(RuntimeStartup::Exit(crate::exit_codes::ExitCode::Ok));
    }

    let strict_policy = effective_strict_policy(&cli);
    let verbose = cli.verbose || feature_flags.is_enabled(Feature::VerboseLogging);
    initialize_tracing(verbose);
    log_startup(strict_policy);

    Ok(RuntimeStartup::Execute {
        command: cli.command,
        strict_policy,
    })
}

fn initialize_tracing(verbose: bool) {
    let default_directive = if verbose { "debug" } else { "warn" };
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive));
    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_ansi(false)
        .with_writer(|| BrokenPipeSafeStderr(std::io::stderr()))
        .init();
}

fn log_startup(strict_policy: bool) {
    let help_requested =
        std::env::args_os().any(|arg| arg == "--help" || arg == "-h" || arg == "-?" || arg == "/?");
    let version_requested = std::env::args_os().any(|arg| arg == "--version" || arg == "-V");
    if !(help_requested || version_requested) {
        tracing::info!(
            invocation_id = %invocation_id(),
            version = env!("CARGO_PKG_VERSION"),
            commit = option_env!("GIT_SHA").unwrap_or("unknown"),
            os = std::env::consts::OS,
            arch = std::env::consts::ARCH,
            strict_policy,
            "copybook-cli start"
        );
    }
}
