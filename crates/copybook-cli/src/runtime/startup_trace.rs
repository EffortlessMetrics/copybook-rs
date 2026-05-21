use super::{BrokenPipeSafeStderr, EnvFilter, invocation_id};

pub(super) fn initialize(verbose: bool, strict_policy: bool) {
    let default_directive = if verbose { "debug" } else { "warn" };
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new(default_directive));
    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_ansi(false)
        .with_writer(|| BrokenPipeSafeStderr(std::io::stderr()))
        .init();

    if !metadata_only_invocation_requested() {
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

fn metadata_only_invocation_requested() -> bool {
    let help_requested =
        std::env::args_os().any(|arg| arg == "--help" || arg == "-h" || arg == "-?" || arg == "/?");
    let version_requested = std::env::args_os().any(|arg| arg == "--version" || arg == "-V");
    help_requested || version_requested
}
