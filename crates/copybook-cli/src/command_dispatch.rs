// SPDX-License-Identifier: AGPL-3.0-or-later
//! Subcommand routing for the copybook CLI.
//!
//! Keeping this match out of `main` makes process lifecycle, diagnostics, and
//! feature setup independent from per-command argument adaptation.

use crate::cli_config::effective_dialect;
use crate::exit_codes::ExitCode;
use crate::utils::effective_error_policy;
use crate::{Commands, commands};
use anyhow::anyhow;
use copybook::core::FeatureFlags;
use std::convert::TryFrom;

type CommandOutcome = (anyhow::Result<ExitCode>, &'static str);

/// Render a `--profile` load/resolution failure as structured diagnostics.
///
/// Profile problems are invalid run configuration (validation failure,
/// exit 3), never an internal error: the operator must edit the flag or
/// the profile so they agree.
fn profile_failure(
    op: &'static str,
    error: &crate::profile_inputs::ProfileInputError,
) -> CommandOutcome {
    let message = match error {
        crate::profile_inputs::ProfileInputError::Conflict(_) => {
            format!("{error} (edit the profile or the corresponding command flag so they agree)")
        }
        _ => error.to_string(),
    };
    let diagnostics = crate::ExitDiagnostics::new(
        ExitCode::Encode,
        &message,
        op,
        "", // op_stage will be overridden by emit_exit_diagnostics_stage
        tracing::Level::ERROR,
        ExitCode::Encode.as_i32(),
    )
    .with_subcode(Some(error.subcode()));
    crate::emit_exit_diagnostics_stage(&diagnostics, crate::Stage::Execute);
    (Ok(ExitCode::Encode), op)
}

pub(crate) fn run_command(
    command: Commands,
    strict_policy: bool,
    feature_flags: &FeatureFlags,
    verbose: bool,
) -> CommandOutcome {
    match command {
        command @ Commands::Parse { .. } => run_parse_command(command, feature_flags),
        command @ Commands::Inspect { .. } => run_inspect_command(command, feature_flags),
        command @ Commands::Decode { .. } => {
            run_decode_command(command, strict_policy, feature_flags)
        }
        command @ Commands::Encode { .. } => run_encode_command(command, feature_flags),
        #[cfg(feature = "audit")]
        command @ Commands::Audit { .. } => run_audit_command(command, feature_flags),
        command @ Commands::Verify { .. } => run_verify_command(command, feature_flags),
        Commands::Support { args } => (commands::support::run(&args, feature_flags), "support"),
        Commands::Determinism { command } => run_determinism_command(&command, feature_flags),
        command @ Commands::Doctor { .. } => run_doctor_command(command, verbose),
        command @ Commands::Explain { .. } => run_explain_command(command, feature_flags),
        Commands::Compat {
            base,
            head,
            record_format,
            codepage,
            dialect,
            fail_on,
            format,
        } => (
            commands::compat::run(
                &base,
                &head,
                &record_format.to_string(),
                &codepage.to_string(),
                dialect,
                fail_on,
                format,
            ),
            "compat",
        ),
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

fn run_doctor_command(command: Commands, verbose: bool) -> CommandOutcome {
    let Commands::Doctor {
        copybook,
        input,
        format,
        codepage,
        sample,
        json,
        strict_comments,
        dialect,
        emit_profile,
    } = command
    else {
        return dispatch_mismatch("doctor");
    };

    let resolved = match crate::cli_config::effective_dialect(dialect) {
        Ok(dialect) => dialect,
        Err(error) => return dialect_env_failure("doctor", &error),
    };
    (
        commands::doctor::run(
            &copybook,
            input,
            format,
            codepage,
            sample,
            json,
            strict_comments,
            resolved,
            verbose,
            emit_profile,
            dialect,
        ),
        "doctor",
    )
}

fn run_explain_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Explain {
        code,
        format,
        copybook,
        input,
        record,
        record_format,
        codepage,
        strict,
        strict_comments,
        dialect,
    } = command
    else {
        return dispatch_mismatch("explain");
    };

    let resolved = match crate::cli_config::effective_dialect(dialect) {
        Ok(dialect) => dialect,
        Err(error) => return dialect_env_failure("explain", &error),
    };
    (
        commands::explain::run(&commands::explain::ExplainArgs {
            code,
            format,
            copybook,
            input,
            record,
            record_format,
            codepage,
            strict,
            strict_comments,
            dialect: copybook::core::dialect::Dialect::from(resolved),
            feature_flags,
        }),
        "explain",
    )
}

fn run_parse_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
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

    let effective_dialect = match effective_dialect(dialect) {
        Ok(dialect) => dialect,
        Err(error) => return dialect_env_failure("parse", &error),
    };
    (
        commands::parse::run(
            &copybook,
            output,
            strict,
            strict_comments,
            effective_dialect,
            feature_flags,
        ),
        "parse",
    )
}

fn run_inspect_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Inspect {
        copybook,
        format,
        codepage,
        strict,
        strict_comments,
        dialect,
        profile,
        emit_manifest,
        overwrite_manifest,
        manifest,
        payload_byte,
        field,
        output,
    } = command
    else {
        return dispatch_mismatch("inspect");
    };

    // `--manifest` alone also enters query validation: without a selector the
    // contradiction fails closed instead of silently running the layout
    // report that ignores the manifest.
    if payload_byte.is_some() || field.is_some() || manifest.is_some() {
        return run_inspect_query_command(
            copybook.as_ref(),
            profile.as_ref(),
            manifest.as_ref(),
            payload_byte,
            field,
            output,
            format,
            codepage,
            strict,
            strict_comments,
            dialect,
            emit_manifest.as_ref(),
            feature_flags,
        );
    }

    // `--output` renders queries only: the layout report and manifest
    // emission have no machine rendering, so a non-default value without a
    // selector is a contradiction, never a silent default.
    if output != commands::inspect::InspectQueryFormat::Human {
        return query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "--output json needs an ownership query: pass --payload-byte <N> or --field <PATH>",
        );
    }

    let Some(copybook) = copybook else {
        return query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "inspect needs COPYBOOK; ownership queries take --manifest instead",
        );
    };

    if let Some(manifest_path) = emit_manifest {
        return run_inspect_emit_manifest(
            &copybook,
            codepage,
            strict,
            strict_comments,
            dialect,
            profile.as_ref(),
            &manifest_path,
            overwrite_manifest,
            feature_flags,
        );
    }

    let effective_dialect = match effective_dialect(dialect) {
        Ok(dialect) => dialect,
        Err(error) => return dialect_env_failure("inspect", &error),
    };
    (
        commands::inspect::run(
            &copybook,
            codepage.unwrap_or(copybook::codec::Codepage::CP037),
            strict,
            strict_comments,
            effective_dialect,
            feature_flags,
        ),
        "inspect",
    )
}

/// Ownership query mode: answer one static field/byte question.
///
/// Exactly one selector (`--payload-byte` or `--field`) and exactly one
/// input (`--manifest` or `COPYBOOK`) is required; every other combination
/// is a contradiction, never a silent default. Manifest-backed queries read
/// no copybook and no record data; source-backed queries build the manifest
/// through the same constructor emission uses, so both inputs agree. Query
/// refusals (unknown or ambiguous paths) render structured diagnostics with
/// an `Encode` (validation) exit. `--emit-manifest` stays a separate run so
/// the layout report never mixes into a query answer.
#[allow(clippy::too_many_arguments)]
fn run_inspect_query_command(
    copybook: Option<&std::path::PathBuf>,
    profile: Option<&std::path::PathBuf>,
    manifest_path: Option<&std::path::PathBuf>,
    payload_byte: Option<u32>,
    field: Option<String>,
    output: commands::inspect::InspectQueryFormat,
    record_format: Option<copybook::codec::RecordFormat>,
    codepage: Option<copybook::codec::Codepage>,
    strict: bool,
    strict_comments: bool,
    dialect: Option<crate::DialectPreference>,
    emit_manifest: Option<&std::path::PathBuf>,
    feature_flags: &FeatureFlags,
) -> CommandOutcome {
    let selector = match select_ownership_query(payload_byte, field) {
        Ok(selector) => selector,
        Err(outcome) => return outcome,
    };
    if emit_manifest.is_some() {
        return query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "--emit-manifest and ownership queries are separate runs: emit first, then query the file",
        );
    }
    let manifest = match (manifest_path, copybook) {
        (Some(_), Some(_)) => {
            return query_failure(
                "inspect",
                crate::subcode::QUERY_CONTRADICTION,
                "--manifest reads no copybook: pass exactly one query input",
            );
        }
        (None, None) => {
            return query_failure(
                "inspect",
                crate::subcode::QUERY_CONTRADICTION,
                "inspect query needs --manifest or COPYBOOK",
            );
        }
        (Some(path), None) => {
            if profile.is_some() {
                return query_failure(
                    "inspect",
                    crate::subcode::QUERY_CONTRADICTION,
                    "--profile contradicts --manifest: the manifest already binds its reviewed inputs",
                );
            }
            match load_query_manifest(path) {
                Ok(manifest) => manifest,
                Err(error) => return error,
            }
        }
        (None, Some(path)) => {
            if path.as_os_str() == "-" {
                return query_failure(
                    "inspect",
                    crate::subcode::QUERY_CONTRADICTION,
                    "inspect query needs a copybook file; stdin has no stable source identity",
                );
            }
            let source = QuerySource {
                copybook: path,
                profile,
                record_format,
                codepage,
                strict,
                strict_comments,
                dialect,
            };
            match build_query_manifest(&source, feature_flags) {
                Ok(manifest) => manifest,
                Err(outcome) => return outcome,
            }
        }
    };
    let report = match commands::inspect::answer_query(&manifest, selector) {
        Ok(report) => report,
        Err(commands::inspect::QueryRefusal::UnknownField { query }) => {
            return query_failure(
                "inspect",
                crate::subcode::QUERY_UNANSWERABLE,
                &format!(
                    "unknown field path '{query}': no field, alias, or condition matches \
                     (short names must name exactly one entry)"
                ),
            );
        }
        Err(commands::inspect::QueryRefusal::AmbiguousField { query, candidates }) => {
            return query_failure(
                "inspect",
                crate::subcode::QUERY_UNANSWERABLE,
                &format!(
                    "ambiguous field path '{query}': {} (qualify the full dotted path)",
                    candidates.join(", ")
                ),
            );
        }
    };
    let output = match output {
        commands::inspect::InspectQueryFormat::Human => {
            commands::inspect::render_human_report(&report)
        }
        commands::inspect::InspectQueryFormat::Json => {
            match commands::inspect::render_json_report(&report) {
                Ok(json) => json,
                Err(error) => return (Err(error), "inspect"),
            }
        }
    };
    match crate::write_stdout_all(output.as_bytes()) {
        Ok(()) => (Ok(crate::ExitCode::Ok), "inspect"),
        Err(error) => (Err(anyhow!(error)), "inspect"),
    }
}

/// Exactly one ownership selector: `--payload-byte` or `--field`, never
/// both, never neither.
fn select_ownership_query(
    payload_byte: Option<u32>,
    field: Option<String>,
) -> Result<commands::inspect::OwnershipSelector, CommandOutcome> {
    use commands::inspect::OwnershipSelector;
    match (payload_byte, field) {
        (Some(_), Some(_)) => Err(query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "inspect query needs exactly one of --payload-byte <N> or --field <PATH>",
        )),
        (Some(byte), None) => Ok(OwnershipSelector::PayloadByte(byte)),
        (None, Some(path)) => Ok(OwnershipSelector::FieldPath(path)),
        (None, None) => Err(query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "inspect query needs --payload-byte <N> or --field <PATH>",
        )),
    }
}

/// Source-backed query inputs: a copybook resolved through the same
/// layers as emission, so source queries answer what manifests contain.
struct QuerySource<'a> {
    copybook: &'a std::path::PathBuf,
    profile: Option<&'a std::path::PathBuf>,
    record_format: Option<copybook::codec::RecordFormat>,
    codepage: Option<copybook::codec::Codepage>,
    strict: bool,
    strict_comments: bool,
    dialect: Option<crate::DialectPreference>,
}

/// Build the manifest for a source-backed ownership query.
fn build_query_manifest(
    source: &QuerySource<'_>,
    feature_flags: &FeatureFlags,
) -> Result<copybook::codec::resolved_manifest::ResolvedManifest, CommandOutcome> {
    if source.copybook.as_os_str() == "-" {
        return Err(query_failure(
            "inspect",
            crate::subcode::QUERY_CONTRADICTION,
            "inspect query needs a copybook file; stdin has no stable source identity",
        ));
    }
    let loaded = match crate::profile_inputs::load_profile(
        source.profile.map(std::path::PathBuf::as_path),
    ) {
        Ok(loaded) => loaded,
        Err(error) => return Err(profile_failure("inspect", &error)),
    };
    let common = match crate::profile_inputs::resolve_common(
        source.record_format,
        source.codepage,
        source.dialect,
        None,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return Err(profile_failure("inspect", &error)),
    };
    match commands::inspect::build_manifest(
        source.copybook,
        &common,
        loaded.as_ref(),
        source.strict,
        source.strict_comments,
        feature_flags,
    ) {
        Ok((manifest, _)) => Ok(manifest),
        Err(error) => Err((Err(error), "inspect")),
    }
}

/// Load a manifest document for a manifest-backed ownership query, mapping
/// unreadable files and invalid documents to their guardrail subcodes.
fn load_query_manifest(
    path: &std::path::PathBuf,
) -> Result<copybook::codec::resolved_manifest::ResolvedManifest, CommandOutcome> {
    // Refuse oversize documents before allocating: `from_json` enforces the
    // same bound after the read, so this pre-check only moves the rejection
    // ahead of the allocation. A missing file still falls through to the
    // unreadable refusal below.
    if path.metadata().is_ok_and(|meta| {
        meta.len() > copybook::codec::resolved_manifest::MAX_MANIFEST_BYTES as u64
    }) {
        return Err(query_failure(
            "inspect",
            crate::subcode::MANIFEST_INVALID,
            &format!(
                "manifest {} exceeds the {}-byte manifest bound",
                path.display(),
                copybook::codec::resolved_manifest::MAX_MANIFEST_BYTES
            ),
        ));
    }
    let Ok(bytes) = std::fs::read(path) else {
        return Err(query_failure(
            "inspect",
            crate::subcode::MANIFEST_UNREADABLE,
            &format!("cannot read manifest {}", path.display()),
        ));
    };
    match copybook::codec::resolved_manifest::ResolvedManifest::from_json(&bytes) {
        Ok(manifest) => Ok(manifest),
        Err(error) => Err(query_failure(
            "inspect",
            crate::subcode::MANIFEST_INVALID,
            &format!("cannot parse manifest {}: {error}", path.display()),
        )),
    }
}

/// A refused inspect ownership query. Dispatch renders this as structured
/// diagnostics with an `Encode` (validation) exit, never as a silent
/// default or an internal error.
fn query_failure(op: &'static str, subcode: u16, message: &str) -> CommandOutcome {
    let diagnostics = crate::ExitDiagnostics::new(
        crate::ExitCode::Encode,
        message,
        op,
        "", // op_stage will be overridden by emit_exit_diagnostics_stage
        tracing::Level::ERROR,
        crate::ExitCode::Encode.as_i32(),
    )
    .with_subcode(Some(subcode));
    crate::emit_exit_diagnostics_stage(&diagnostics, crate::Stage::Execute);
    (Ok(crate::ExitCode::Encode), op)
}

/// An environment input naming no known value. Dispatch renders this as
/// structured diagnostics with an `Encode` (validation) exit, never as a
/// silent default or an internal error.
fn dialect_env_failure(
    op: &'static str,
    error: &crate::cli_config::DialectEnvError,
) -> CommandOutcome {
    let message = error.to_string();
    let diagnostics = crate::ExitDiagnostics::new(
        crate::ExitCode::Encode,
        &message,
        op,
        "", // op_stage will be overridden by emit_exit_diagnostics_stage
        tracing::Level::ERROR,
        crate::ExitCode::Encode.as_i32(),
    )
    .with_subcode(Some(crate::subcode::ENV_INVALID));
    crate::emit_exit_diagnostics_stage(&diagnostics, crate::Stage::Execute);
    (Ok(crate::ExitCode::Encode), op)
}

/// A refused `--emit-manifest` target: an existing file without
/// `--overwrite-manifest`, or a non-file target. Dispatch renders this as
/// structured diagnostics with an `Encode` (validation) exit, never as an
/// internal error.
fn manifest_target_failure(op: &'static str, message: &str) -> CommandOutcome {
    let diagnostics = crate::ExitDiagnostics::new(
        crate::ExitCode::Encode,
        message,
        op,
        "", // op_stage will be overridden by emit_exit_diagnostics_stage
        tracing::Level::ERROR,
        crate::ExitCode::Encode.as_i32(),
    )
    .with_subcode(Some(crate::subcode::MANIFEST_TARGET_REFUSED));
    crate::emit_exit_diagnostics_stage(&diagnostics, crate::Stage::Execute);
    (Ok(crate::ExitCode::Encode), op)
}

/// Inspect with manifest emission: resolve the reviewed inputs with provenance
/// and emit the layout report plus the manifest file.
///
/// Generation never replaces an existing file unless `overwrite` was passed,
/// and never writes a partial manifest: the target guards run before any
/// resolution work, and failures leave no file behind.
#[allow(clippy::too_many_arguments)]
fn run_inspect_emit_manifest(
    copybook: &std::path::PathBuf,
    codepage: Option<copybook::codec::Codepage>,
    strict: bool,
    strict_comments: bool,
    dialect: Option<crate::DialectPreference>,
    profile: Option<&std::path::PathBuf>,
    manifest_path: &std::path::PathBuf,
    overwrite: bool,
    feature_flags: &copybook::core::FeatureFlags,
) -> CommandOutcome {
    if copybook.as_os_str() == "-" {
        return profile_failure(
            "inspect",
            &crate::profile_inputs::ProfileInputError::Missing {
                message:
                    "--emit-manifest requires a copybook file; stdin has no stable source identity"
                        .to_string(),
            },
        );
    }
    if manifest_path.as_os_str() == "-" {
        return manifest_target_failure(
            "inspect",
            "--emit-manifest requires a file path; '-' would mix the manifest with the layout report on stdout",
        );
    }
    // symlink_metadata (not exists) so a dangling symlink target also refuses
    // cleanly here; anything appearing afterwards still cannot be replaced
    // because publication itself is no-clobber.
    if !overwrite && std::fs::symlink_metadata(manifest_path).is_ok() {
        return manifest_target_failure(
            "inspect",
            &format!(
                "refusing to overwrite existing manifest {}; pass --overwrite-manifest to replace it",
                manifest_path.display()
            ),
        );
    }
    if profile.is_none() {
        return profile_failure(
            "inspect",
            &crate::profile_inputs::ProfileInputError::Missing {
                message: "--emit-manifest requires --profile".to_string(),
            },
        );
    }
    let loaded = match crate::profile_inputs::load_profile(profile.map(std::path::PathBuf::as_path))
    {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("inspect", &error),
    };
    let common =
        match crate::profile_inputs::resolve_common(None, codepage, dialect, None, loaded.as_ref())
        {
            Ok(common) => common,
            Err(error) => return profile_failure("inspect", &error),
        };
    (
        commands::inspect::run_with_manifest(
            copybook,
            &common,
            loaded.as_ref(),
            strict,
            strict_comments,
            feature_flags,
            &commands::inspect::ManifestEmission {
                manifest_path,
                overwrite,
            },
        ),
        "inspect",
    )
}

/// Resolve the profile for a determinism comparison and run it.
///
/// The comparison consumes the same resolution as the operating commands
/// (explicit flag, then profile, then ambient environment, then product
/// default), so repeated decode/encode/round-trip runs prove exactly what
/// `decode`/`encode` would run. Determinism carries no `--dialect` or
/// `--max-errors` flags: dialect falls back through profile, environment,
/// and default, and the single-record comparison performs no multi-record
/// error budgeting.
fn run_determinism_command(
    command: &commands::determinism::DeterminismCommand,
    feature_flags: &FeatureFlags,
) -> CommandOutcome {
    let common = commands::determinism::common_args(command);
    let loaded = match crate::profile_inputs::load_profile(common.profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("determinism", &error),
    };
    let resolved = match crate::profile_inputs::resolve_common(
        common.format,
        common.codepage,
        None, // No --dialect flag: profile, ambient environment, then default.
        None, // No --max-errors flag: a single-record comparison budgets nothing.
        loaded.as_ref(),
    ) {
        Ok(resolved) => resolved,
        Err(error) => return profile_failure("determinism", &error),
    };
    let decode_only =
        match crate::profile_inputs::resolve_decode(common.json_number, None, loaded.as_ref()) {
            Ok(decode_only) => decode_only,
            Err(error) => return profile_failure("determinism", &error),
        };
    let encode_only = match crate::profile_inputs::resolve_encode(loaded.as_ref()) {
        Ok(encode_only) => encode_only,
        Err(error) => return profile_failure("determinism", &error),
    };
    let profile_path = common
        .profile
        .as_ref()
        .map_or("<profile>".to_string(), |path| path.display().to_string());
    // Same inputs as the operating commands' own error-policy computation:
    // determinism carries no --strict/--fail-fast/--max-errors flags, so
    // the direct strict mode is unconditionally false and only a bound
    // profile can supply reviewed framing and record policy.
    let strict_mode = effective_error_policy(false, false, None).strict_mode;
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict_mode)
    {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "determinism",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile_path.clone(),
                    message: error.to_string(),
                },
            );
        }
    };
    let fingerprint = match &loaded {
        Some(profile) => match profile.fingerprint() {
            Ok(fingerprint) => Some(fingerprint),
            Err(error) => {
                return profile_failure(
                    "determinism",
                    &crate::profile_inputs::ProfileInputError::Invalid {
                        path: profile_path,
                        message: error.to_string(),
                    },
                );
            }
        },
        None => None,
    };
    let inputs = commands::determinism::DeterminismInputs {
        format: resolved.format,
        codepage: resolved.codepage,
        dialect: resolved.dialect,
        json_number: decode_only.json_number,
        decode_unmappable: decode_only.unmappable,
        encode_unmappable: encode_only.unmappable,
        execution_policy,
        profile_fingerprint: fingerprint,
    };
    (
        commands::determinism::run(command, &inputs, feature_flags),
        "determinism",
    )
}

fn run_decode_command(
    command: Commands,
    strict_policy: bool,
    feature_flags: &FeatureFlags,
) -> CommandOutcome {
    let Commands::Decode {
        copybook,
        input,
        output,
        profile,
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

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("decode", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("decode", &error),
    };
    let decode_only = match crate::profile_inputs::resolve_decode(
        json_number,
        on_decode_unmappable,
        loaded.as_ref(),
    ) {
        Ok(decode_only) => decode_only,
        Err(error) => return profile_failure("decode", &error),
    };
    // Same inputs as `decode::run`'s own error-policy computation, so the
    // direct (profile-less) policy agrees with the run by construction.
    let strict_mode = effective_error_policy(strict, fail_fast, common.max_errors).strict_mode;
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict_mode)
    {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "decode",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    (
        commands::decode::run(&commands::decode::DecodeArgs {
            copybook: &copybook,
            input: &input,
            output: &output,
            format: common.format,
            codepage: common.codepage,
            json_number: decode_only.json_number,
            strict,
            max_errors: common.max_errors,
            fail_fast,
            emit_filler,
            emit_meta,
            emit_raw,
            on_decode_unmappable: decode_only.unmappable,
            threads,
            strict_comments,
            preserve_zoned_encoding,
            preferred_zoned_encoding: preferred_zoned_encoding_cli.into(),
            float_format,
            strict_policy,
            execution_policy,
            dialect: common.dialect,
            select: &select,
            feature_flags,
        }),
        "decode",
    )
}

fn run_encode_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Encode {
        copybook,
        input,
        output,
        profile,
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

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("encode", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("encode", &error),
    };
    let encode_only = match crate::profile_inputs::resolve_encode(loaded.as_ref()) {
        Ok(encode_only) => encode_only,
        Err(error) => return profile_failure("encode", &error),
    };
    // Same inputs as `encode::run`'s own error-policy computation, so the
    // direct (profile-less) policy agrees with the run by construction.
    let strict_mode =
        effective_error_policy(strict, fail_fast || !no_fail_fast, common.max_errors).strict_mode;
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict_mode)
    {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "encode",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    (
        commands::encode::run(
            &copybook,
            &input,
            &output,
            &commands::encode::EncodeCliOptions {
                format: common.format,
                codepage: common.codepage,
                use_raw,
                bwz_encode,
                strict,
                max_errors: common.max_errors,
                execution_policy,
                // Stopping on the first failure is the default, so only `--no-fail-fast`
                // changes anything. clap rejects the two together, so this cannot be
                // ambiguous.
                fail_fast: fail_fast || !no_fail_fast,
                threads,
                coerce_numbers,
                strict_comments,
                zoned_encoding_override,
                float_format,
                dialect: common.dialect,
                select: &select,
                on_encode_unmappable: encode_only.unmappable,
            },
            feature_flags,
        ),
        "encode",
    )
}

#[cfg(feature = "audit")]
fn run_audit_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Audit { audit_command } = command else {
        return dispatch_mismatch("audit");
    };

    let runtime = match tokio::runtime::Runtime::new() {
        Ok(runtime) => runtime,
        Err(err) => return (Err(anyhow!(err)), "audit"),
    };
    (
        runtime
            .block_on(commands::audit::run(audit_command, feature_flags))
            .map_err(|err| anyhow!(err)),
        "audit",
    )
}

fn run_verify_command(command: Commands, feature_flags: &FeatureFlags) -> CommandOutcome {
    let Commands::Verify {
        copybook,
        input,
        report,
        profile,
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

    let loaded = match crate::profile_inputs::load_profile(profile.as_deref()) {
        Ok(loaded) => loaded,
        Err(error) => return profile_failure("verify", &error),
    };
    let common = match crate::profile_inputs::resolve_common(
        format,
        codepage,
        dialect,
        max_errors,
        loaded.as_ref(),
    ) {
        Ok(common) => common,
        Err(error) => return profile_failure("verify", &error),
    };
    // Verify's strict mode is the flag itself, matching `verify::run`.
    let execution_policy = match crate::profile_inputs::resolve_policy(loaded.as_ref(), strict) {
        Ok(policy) => policy,
        Err(error) => {
            return profile_failure(
                "verify",
                &crate::profile_inputs::ProfileInputError::Invalid {
                    path: profile
                        .as_ref()
                        .map_or("<profile>".to_string(), |path| path.display().to_string()),
                    message: error.to_string(),
                },
            );
        }
    };
    // Verify keeps its historical default budget of 10 when neither a flag
    // nor a profile sets one.
    let value = common.max_errors.unwrap_or(10);
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
        format: common.format,
        codepage: common.codepage,
        strict,
        max_errors: normalized_max_errors,
        sample: sample.unwrap_or(5),
        strict_comments,
        execution_policy,
        dialect: common.dialect,
        select: &select,
    };
    (
        commands::verify::run(&copybook, &input, report, &opts, feature_flags),
        "verify",
    )
}
