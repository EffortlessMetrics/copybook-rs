// SPDX-License-Identifier: AGPL-3.0-or-later
//! Advisory diagnostic domain for `support --advise` (#954, Slice A).
//!
//! Owner decision (recorded per #954 true-owner rule):
//!
//! ```text
//! analysis owner crate/module .... copybook-support-matrix::advise
//! result type owner .............. same module (AdviseResult)
//! public versus crate-private API  public types; construction is explicit,
//!                                  no globals, no I/O
//! schema/stability class ......... beta (ADVISE_STABILITY_CLASS); JSON schema
//!                                  versioned by ADVISE_RESULT_SCHEMA_VERSION
//! CLI orchestration boundary ..... CLI parses options, runs analysis via
//!                                  Slice B/C, renders; no domain logic in
//!                                  CLI handlers
//! renderer ownership ............. deterministic JSON here; human rendering
//!                                  in the CLI (Slice C) over the same value
//! existing types reused .......... FeatureId, SupportStatus, find_feature
//! new types justified ............ Verdict/ScenarioAssessment/AdviseResult
//!                                  carry ledger scenario IDs, layer impact,
//!                                  redaction and bounds state that the matrix
//!                                  feature table cannot express
//! ```
//!
//! No new crate: the matrix crate already owns the support-status vocabulary
//! and serves the `support` command. This module adds only owned data plus
//! pure derivation; it performs no I/O, reads no record payload, and never
//! shells out. Uncertainty stays local: anything heuristic is
//! [`AssessmentStatus::Unknown`], never rendered as certainty.

use serde::{Deserialize, Serialize};

/// Version of the advisory result JSON contract.
///
/// Beta for 0.7: automation must match this exact string, never parse prose.
pub const ADVISE_RESULT_SCHEMA_VERSION: &str = "0.7.0-beta.1";

/// Stability class of the advisory contract for 0.7.
pub const ADVISE_STABILITY_CLASS: &str = "beta";

/// Maximum scenario assessments carried in one result; excess is dropped and
/// counted in [`TruncationMeta`].
pub const MAX_SCENARIOS: usize = 64;

/// Maximum evidence references per scenario assessment.
pub const MAX_EVIDENCE_REFS: usize = 8;

/// Maximum characters kept in any single suggestion/next-action string.
pub const MAX_SUGGESTION_CHARS: usize = 512;

/// Closed overall verdict. Derivation from scenario results is fixed by
/// [`derive_verdict`] and documented there; it is never free-form prose.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum Verdict {
    /// Evaluated path is fully supported.
    Supported,
    /// Supported subject to the stated limits.
    SupportedWithLimits,
    /// Evaluated path is beta (usable, may change).
    Beta,
    /// Some applicability is partial or unknown.
    PartialUnknown,
    /// Deliberately rejected/unsupported scenario.
    Rejected,
    /// The copybook itself is invalid or analysis failed on user input.
    InvalidInput,
    /// The tool failed independent of the input.
    ToolFailure,
}

/// Derive the overall verdict from scenario outcomes under fixed rules.
///
/// Precedence (first match wins): any tool failure dominates everything;
/// then invalid input; then deliberate rejection; then partial/unknown;
/// then beta; then limited; otherwise supported. An empty assessment set is
/// [`Verdict::PartialUnknown`]: nothing was evaluated, so nothing is proven.
#[must_use]
#[inline]
pub fn derive_verdict(statuses: &[AssessmentStatus]) -> Verdict {
    if statuses.is_empty() {
        return Verdict::PartialUnknown;
    }
    if statuses.contains(&AssessmentStatus::ToolFailure) {
        return Verdict::ToolFailure;
    }
    if statuses.contains(&AssessmentStatus::Invalid) {
        return Verdict::InvalidInput;
    }
    if statuses.contains(&AssessmentStatus::Rejected) {
        return Verdict::Rejected;
    }
    if statuses.contains(&AssessmentStatus::Unknown) {
        return Verdict::PartialUnknown;
    }
    if statuses.contains(&AssessmentStatus::Beta) {
        return Verdict::Beta;
    }
    if statuses.contains(&AssessmentStatus::Limited) {
        return Verdict::SupportedWithLimits;
    }
    Verdict::Supported
}

/// Per-scenario outcome. `Unknown` is the honest default for heuristic or
/// incomplete checks; it must never be rendered as certainty.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum AssessmentStatus {
    /// Scenario holds on the evaluated path.
    Supported,
    /// Scenario holds within stated limits.
    Limited,
    /// Scenario is beta on the evaluated path.
    Beta,
    /// Applicability is partial or could not be determined.
    Unknown,
    /// Scenario is deliberately rejected/unsupported.
    Rejected,
    /// Input invalid for this scenario.
    Invalid,
    /// Tool failure while evaluating this scenario.
    ToolFailure,
    /// Scenario does not apply to the evaluated input.
    NotApplicable,
}

/// Processing layer affected by a scenario outcome.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
#[non_exhaustive]
pub enum AffectedLayer {
    /// Copybook parsing.
    Parse,
    /// Field layout resolution.
    Layout,
    /// Record decoding.
    Decode,
    /// Record encoding.
    Encode,
    /// Round-trip fidelity.
    RoundTrip,
    /// Command-line surface.
    Cli,
}

/// One scenario evaluation: the #951 scenario ID plus its outcome on the
/// evaluated path. Every string is bounded at construction (see `MAX_*`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScenarioAssessment {
    /// Scenario identifier from the scenario ledger (e.g. `struct.odo.tail_fixed`).
    pub scenario_id: String,
    /// Outcome on the evaluated path.
    pub status: AssessmentStatus,
    /// Stability class of the scenario (`stable`, `beta`, ...).
    pub stability_class: String,
    /// Processing layers this outcome affects.
    pub affected_layers: Vec<AffectedLayer>,
    /// Applicable record formats (`fixed`, `rdw`, `vb` subset).
    pub record_formats: Vec<String>,
    /// Applicable codepages (`all` or an explicit subset).
    pub codepages: Vec<String>,
    /// Stable error or rejection identity, when the outcome names one.
    pub error_identity: Option<String>,
    /// Bounded evidence references (test paths, registry links, doc refs).
    pub evidence_refs: Vec<String>,
    /// Why a non-supported outcome matters, or remediation.
    pub limitation_or_remediation: String,
    /// Next real command/options to run; must name implemented behavior only.
    pub next_action: String,
}

impl ScenarioAssessment {
    /// Build an assessment, bounding every collected string.
    #[must_use]
    #[inline]
    pub fn bounded(
        scenario_id: impl Into<String>,
        status: AssessmentStatus,
        stability_class: impl Into<String>,
    ) -> Self {
        Self {
            scenario_id: bound_chars(scenario_id.into(), MAX_SUGGESTION_CHARS),
            status,
            stability_class: bound_chars(stability_class.into(), MAX_SUGGESTION_CHARS),
            affected_layers: Vec::new(),
            record_formats: Vec::new(),
            codepages: Vec::new(),
            error_identity: None,
            evidence_refs: Vec::new(),
            limitation_or_remediation: String::new(),
            next_action: String::new(),
        }
    }

    /// Push an evidence reference unless the per-scenario bound is reached.
    /// Returns `false` when the reference was dropped.
    #[inline]
    pub fn push_evidence(&mut self, reference: impl Into<String>) -> bool {
        if self.evidence_refs.len() >= MAX_EVIDENCE_REFS {
            return false;
        }
        self.evidence_refs
            .push(bound_chars(reference.into(), MAX_SUGGESTION_CHARS));
        true
    }

    /// Set the next action, bounded. It must name implemented behavior only;
    /// inventing flags or recovery paths is a contract violation.
    #[inline]
    pub fn set_next_action(&mut self, action: impl Into<String>) {
        self.next_action = bound_chars(action.into(), MAX_SUGGESTION_CHARS);
    }
}

/// Effective evaluated options carried for reproducibility.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EffectiveOptions {
    /// Requested record format (`fixed`, `rdw`, `vb`).
    pub format: String,
    /// Requested codepage.
    pub codepage: String,
    /// Requested dialect lever.
    pub dialect: String,
}

impl EffectiveOptions {
    /// Build options, bounding each value.
    #[must_use]
    #[inline]
    pub fn bounded(
        format: impl Into<String>,
        codepage: impl Into<String>,
        dialect: impl Into<String>,
    ) -> Self {
        Self {
            format: bound_chars(format.into(), 32),
            codepage: bound_chars(codepage.into(), 32),
            dialect: bound_chars(dialect.into(), 32),
        }
    }
}

/// Counts preserved whenever output is bounded.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct TruncationMeta {
    /// Scenarios considered by the analysis.
    pub scenarios_considered: usize,
    /// Scenarios carried in this result.
    pub scenarios_reported: usize,
    /// Evidence references dropped by the per-scenario bound.
    pub evidence_dropped: usize,
}

/// Redaction posture. Advisory analysis needs no record payload and takes
/// none; source excerpts are disabled by default. The type makes that
/// posture explicit and checkable instead of incidental.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RedactionState {
    /// Record payload is never collected.
    pub record_payload_excluded: bool,
    /// Source excerpts are emitted only when explicitly enabled under policy.
    pub source_excerpts_enabled: bool,
    /// Paths and environment details are redacted from machine output.
    pub paths_redacted: bool,
}

impl RedactionState {
    /// Default locked-down posture: no payload, no excerpts, paths redacted.
    #[must_use]
    #[inline]
    pub const fn locked_down() -> Self {
        Self {
            record_payload_excluded: true,
            source_excerpts_enabled: false,
            paths_redacted: true,
        }
    }

    /// The result is safe to log when payload is excluded and paths redacted.
    #[must_use]
    #[inline]
    pub const fn is_log_safe(self) -> bool {
        self.record_payload_excluded && self.paths_redacted
    }
}

/// One deterministic advisory result. Human and JSON renderers project this
/// same value; neither adds verdict content of its own.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdviseResult {
    /// Contract version automation must match exactly.
    pub schema_version: String,
    /// Stability class of this contract.
    pub stability_class: String,
    /// Derived overall verdict (see [`derive_verdict`]).
    pub verdict: Verdict,
    /// Canonical schema fingerprint when a copybook was parsed.
    pub copybook_fingerprint: Option<String>,
    /// Byte-level source identity when available.
    pub source_fingerprint: Option<String>,
    /// Effective evaluated options.
    pub effective_options: EffectiveOptions,
    /// Scenario assessments, sorted by `scenario_id` for determinism.
    pub scenarios: Vec<ScenarioAssessment>,
    /// Bounds accounting.
    pub truncation: TruncationMeta,
    /// Redaction posture.
    pub redaction: RedactionState,
    /// Producing tool and version.
    pub tool_version: String,
}

impl AdviseResult {
    /// Build a result; scenarios are sorted by ID and capped at
    /// [`MAX_SCENARIOS`] with counts preserved. The verdict is derived from
    /// the *reported* assessments only when nothing was dropped; a dropped
    /// scenario of unknown severity forces [`Verdict::PartialUnknown`] so a
    /// bound can never manufacture certainty.
    #[must_use]
    #[inline]
    pub fn bounded(
        effective_options: EffectiveOptions,
        mut scenarios: Vec<ScenarioAssessment>,
        evidence_dropped: usize,
        redaction: RedactionState,
        tool_version: impl Into<String>,
    ) -> Self {
        scenarios.sort_by(|left, right| left.scenario_id.cmp(&right.scenario_id));
        let considered = scenarios.len();
        scenarios.truncate(MAX_SCENARIOS);
        let reported = scenarios.len();
        let reported_statuses: Vec<AssessmentStatus> =
            scenarios.iter().map(|item| item.status).collect();
        let mut verdict = derive_verdict(&reported_statuses);
        if reported < considered && verdict == Verdict::Supported {
            verdict = Verdict::PartialUnknown;
        }
        let copybook_fingerprint = None;
        Self {
            schema_version: ADVISE_RESULT_SCHEMA_VERSION.to_string(),
            stability_class: ADVISE_STABILITY_CLASS.to_string(),
            verdict,
            copybook_fingerprint,
            source_fingerprint: None,
            effective_options,
            scenarios,
            truncation: TruncationMeta {
                scenarios_considered: considered,
                scenarios_reported: reported,
                evidence_dropped,
            },
            redaction,
            tool_version: tool_version.into(),
        }
    }

    /// Deterministic JSON projection: same value always yields the same bytes
    /// (declaration-ordered struct fields, sorted scenarios, no maps).
    #[must_use]
    #[inline]
    pub fn to_canonical_json(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| {
            "{\"schema_version\":\"0.7.0-beta.1\",\"verdict\":\"tool-failure\"}".to_string()
        })
    }
}

/// Truncate a string to a character bound without splitting UTF-8.
fn bound_chars(value: String, max_chars: usize) -> String {
    if value.chars().count() <= max_chars {
        return value;
    }
    value.chars().take(max_chars).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn options() -> EffectiveOptions {
        EffectiveOptions::bounded("fixed", "ascii", "normative")
    }

    fn assessment(id: &str, status: AssessmentStatus) -> ScenarioAssessment {
        ScenarioAssessment::bounded(id, status, "stable")
    }

    #[test]
    fn encoding_advise_verdict_empty_is_unknown() {
        assert_eq!(derive_verdict(&[]), Verdict::PartialUnknown);
    }

    #[test]
    fn encoding_advise_verdict_precedence() {
        use AssessmentStatus::{Beta, Invalid, Limited, Rejected, Supported, ToolFailure, Unknown};
        use Verdict::{
            Beta as BetaVerdict, InvalidInput, PartialUnknown, Rejected as RejectedVerdict,
            Supported as SupportedVerdict, SupportedWithLimits, ToolFailure as ToolVerdict,
        };
        assert_eq!(derive_verdict(&[Supported]), SupportedVerdict);
        assert_eq!(derive_verdict(&[Supported, Limited]), SupportedWithLimits);
        assert_eq!(derive_verdict(&[Supported, Limited, Beta]), BetaVerdict);
        assert_eq!(derive_verdict(&[Supported, Beta, Unknown]), PartialUnknown);
        assert_eq!(
            derive_verdict(&[Supported, Unknown, Rejected]),
            RejectedVerdict
        );
        assert_eq!(derive_verdict(&[Rejected, Invalid]), InvalidInput);
        assert_eq!(derive_verdict(&[Invalid, ToolFailure]), ToolVerdict);
    }

    #[test]
    fn encoding_advise_result_sorts_and_bounds() {
        let scenarios = vec![
            assessment("struct.odo.tail_fixed", AssessmentStatus::Supported),
            assessment("struct.level88.single_value", AssessmentStatus::Supported),
        ];
        let result = AdviseResult::bounded(
            options(),
            scenarios,
            0,
            RedactionState::locked_down(),
            "copybook 0.7.0",
        );
        assert_eq!(result.verdict, Verdict::Supported);
        assert_eq!(
            result.scenarios[0].scenario_id,
            "struct.level88.single_value"
        );
        assert_eq!(result.scenarios[1].scenario_id, "struct.odo.tail_fixed");
        assert_eq!(result.truncation.scenarios_considered, 2);
        assert_eq!(result.truncation.scenarios_reported, 2);
        assert_eq!(result.schema_version, ADVISE_RESULT_SCHEMA_VERSION);
    }

    #[test]
    fn encoding_advise_dropped_scenarios_forbid_supported() {
        let scenarios: Vec<ScenarioAssessment> = (0..MAX_SCENARIOS + 4)
            .map(|index| assessment(&format!("row.{index:03}"), AssessmentStatus::Supported))
            .collect();
        let result = AdviseResult::bounded(
            options(),
            scenarios,
            0,
            RedactionState::locked_down(),
            "copybook 0.7.0",
        );
        assert_eq!(result.truncation.scenarios_considered, MAX_SCENARIOS + 4);
        assert_eq!(result.truncation.scenarios_reported, MAX_SCENARIOS);
        assert_eq!(result.verdict, Verdict::PartialUnknown);
    }

    #[test]
    fn encoding_advise_evidence_bound_counts_drops() {
        let mut item = assessment("row", AssessmentStatus::Supported);
        for index in 0..MAX_EVIDENCE_REFS + 3 {
            let kept = item.push_evidence(format!("ref-{index}"));
            assert_eq!(kept, index < MAX_EVIDENCE_REFS);
        }
        assert_eq!(item.evidence_refs.len(), MAX_EVIDENCE_REFS);
    }

    #[test]
    fn encoding_advise_json_is_deterministic() {
        let first = AdviseResult::bounded(
            options(),
            vec![assessment(
                "struct.odo.tail_fixed",
                AssessmentStatus::Limited,
            )],
            0,
            RedactionState::locked_down(),
            "copybook 0.7.0",
        );
        let second = first.clone();
        assert_eq!(first.to_canonical_json(), second.to_canonical_json());
        let parsed: serde_json::Value =
            serde_json::from_str(&first.to_canonical_json()).expect("valid JSON");
        assert_eq!(parsed["schema_version"], ADVISE_RESULT_SCHEMA_VERSION);
        assert_eq!(parsed["verdict"], "supported-with-limits");
        assert_eq!(
            parsed["redaction"]["record_payload_excluded"].as_bool(),
            Some(true)
        );
    }

    #[test]
    fn encoding_advise_redaction_locked_down_is_log_safe() {
        assert!(RedactionState::locked_down().is_log_safe());
    }
}
