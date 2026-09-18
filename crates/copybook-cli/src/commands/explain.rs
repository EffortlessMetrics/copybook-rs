// SPDX-License-Identifier: AGPL-3.0-or-later
//! Explain command implementation.
//!
//! Renders operator knowledge for one stable error identity: what it means,
//! which diagnostic context it carries, and how to fix it. Content comes
//! from the generated [`copybook::error::explain`] table (source:
//! `docs/reference/ERROR_CODES.md`), so explanations can never drift from
//! the documented taxonomy.

use crate::exit_codes::ExitCode;
use crate::write_stdout_all;
use copybook::error::explain::explanation_for;
use std::fmt::Write as _;

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
pub enum ExplainFormat {
    Text,
    Json,
}

/// Explain one stable error identity.
///
/// `code` accepts the full identity (`CBKE501_JSON_TYPE_MISMATCH`) or the
/// short code (`CBKE501`), case-insensitively. Unknown identities exit 3
/// (validation failure) with the closest guidance the CLI can offer.
pub fn run(code: &str, format: ExplainFormat) -> anyhow::Result<ExitCode> {
    let Some(entry) = explanation_for(code) else {
        let mut stderr = String::new();
        let _ = writeln!(
            stderr,
            "error: unknown error identity `{code}`; expected a stable CBK* code such as CBKE501_JSON_TYPE_MISMATCH"
        );
        crate::write_stderr_all(stderr.as_bytes())?;
        return Ok(ExitCode::Encode);
    };
    let family = entry.code.get(..4).unwrap_or(entry.code);
    match format {
        ExplainFormat::Text => {
            let mut out = String::new();
            let _ = writeln!(out, "{} (family {family}, {})", entry.code, entry.severity);
            let _ = writeln!(out);
            let _ = writeln!(out, "  What: {}", entry.description);
            let _ = writeln!(out, "  Context: {}", entry.context);
            let _ = writeln!(out, "  Fix: {}", entry.resolution);
            write_stdout_all(out.as_bytes())?;
        }
        ExplainFormat::Json => {
            let value = serde_json::json!({
                "code": entry.code,
                "family": family,
                "severity": entry.severity,
                "description": entry.description,
                "context": entry.context,
                "resolution": entry.resolution,
            });
            let mut rendered = serde_json::to_string_pretty(&value)
                .unwrap_or_else(|_| "{\"error\":\"json render failed\"}".to_string());
            rendered.push('\n');
            write_stdout_all(rendered.as_bytes())?;
        }
    }
    Ok(ExitCode::Ok)
}
