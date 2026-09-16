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
    /// scenario of unknown severity downgrades any non-negative verdict
    /// ([`Verdict::Supported`], [`Verdict::SupportedWithLimits`], or
    /// [`Verdict::Beta`]) to [`Verdict::PartialUnknown`] so a bound can
    /// never manufacture certainty. Already-negative verdicts are preserved.
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
        if reported < considered
            && matches!(
                verdict,
                Verdict::Supported | Verdict::SupportedWithLimits | Verdict::Beta
            )
        {
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

// ---------------------------------------------------------------------------
// Slice B: analysis (constructs plus options become assessments)
// ---------------------------------------------------------------------------

/// Closed inventory of parsed copybook constructs the analysis understands.
/// The CLI (Slice C) builds these from the parsed schema; anything else
/// arrives as [`ConstructKind::Unmapped`] and stays [`AssessmentStatus::Unknown`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum ConstructKind {
    /// `OCCURS ... DEPENDING ON` at the record tail.
    OccursDepending,
    /// `OCCURS ... DEPENDING ON` not at the record tail.
    NonTailOdo,
    /// Nested `OCCURS DEPENDING ON`.
    NestedOdo,
    /// Level-66 `RENAMES`.
    Renames,
    /// `REDEFINES` clause.
    Redefines,
    /// Level-88 condition names.
    Level88,
    /// Edited numeric `PICTURE`.
    EditedPic,
    /// `COMP-1` / `COMP-2` floating point.
    Comp1Comp2,
    /// `SIGN LEADING/TRAILING SEPARATE`.
    SignSeparate,
    /// Construct with no analysis mapping; never rendered as certainty.
    Unmapped,
}

/// One parsed construct handed to the analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdviseConstruct {
    /// What was found.
    pub kind: ConstructKind,
    /// Bounded detail (field name, clause text, never record data).
    pub detail: String,
    /// Source line when known.
    pub line: Option<u32>,
}

impl AdviseConstruct {
    /// Build a construct with bounded detail.
    #[inline]
    #[must_use]
    pub fn bounded(kind: ConstructKind, detail: impl Into<String>, line: Option<u32>) -> Self {
        Self {
            kind,
            detail: bound_chars(detail.into(), MAX_SUGGESTION_CHARS),
            line,
        }
    }
}

/// Caller-provided analysis input. Malformed input is reported as
/// [`Verdict::InvalidInput`], never fatal: analysis always returns a value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdviseInput {
    /// Parsed constructs to evaluate.
    pub constructs: Vec<AdviseConstruct>,
    /// Caller-reported parse failure, when the copybook did not parse.
    pub parse_error: Option<String>,
    /// Effective options under evaluation.
    pub options: EffectiveOptions,
    /// Canonical schema fingerprint when available.
    pub copybook_fingerprint: Option<String>,
    /// Byte-level source identity when available.
    pub source_fingerprint: Option<String>,
    /// Producing tool and version.
    pub tool_version: String,
}

impl AdviseInput {
    /// Build input with bounded strings.
    ///
    /// Caller precondition: `parse_error` and each construct `detail` must
    /// carry schema text only (field names, clause spellings, diagnostic
    /// identities) — never filesystem paths, record payload, or user
    /// identifiers. Analysis copies caller text verbatim into evidence and
    /// next actions, so the emitted [`RedactionState::paths_redacted`] claim
    /// holds only when callers meet this precondition.
    #[inline]
    #[must_use]
    pub fn bounded(
        constructs: Vec<AdviseConstruct>,
        parse_error: Option<String>,
        options: EffectiveOptions,
        tool_version: impl Into<String>,
    ) -> Self {
        Self {
            constructs,
            parse_error: parse_error.map(|value| bound_chars(value, MAX_SUGGESTION_CHARS)),
            options,
            copybook_fingerprint: None,
            source_fingerprint: None,
            tool_version: tool_version.into(),
        }
    }
}

/// Map a matrix status onto an assessment outcome.
#[inline]
#[must_use]
pub fn assessment_for_status(status: crate::SupportStatus) -> AssessmentStatus {
    match status {
        crate::SupportStatus::Supported => AssessmentStatus::Supported,
        crate::SupportStatus::Partial => AssessmentStatus::Limited,
        crate::SupportStatus::Planned => AssessmentStatus::Unknown,
        crate::SupportStatus::NotPlanned => AssessmentStatus::Rejected,
    }
}

/// Analyze parsed constructs under the requested options.
///
/// Never fails on user input: an empty construct set yields
/// [`Verdict::PartialUnknown`], a caller-reported parse failure yields one
/// `Invalid` assessment, and unmapped constructs yield `Unknown` with
/// guidance. Scenario IDs are ledger IDs where a row exists, otherwise
/// `matrix:<feature-id>`.
#[inline]
#[must_use]
pub fn analyze(input: &AdviseInput) -> AdviseResult {
    let mut assessments = Vec::new();
    let mut evidence_dropped = 0usize;
    if let Some(parse_error) = input.parse_error.as_deref() {
        let mut item = ScenarioAssessment::bounded("unparsed", AssessmentStatus::Invalid, "stable");
        item.set_next_action(format!(
            "Fix the reported parse error, then re-run: {parse_error}"
        ));
        assessments.push(item);
    }
    for construct in &input.constructs {
        assessments.push(assess_construct(
            construct,
            &input.options,
            &mut evidence_dropped,
        ));
    }
    let mut result = AdviseResult::bounded(
        input.options.clone(),
        assessments,
        evidence_dropped,
        RedactionState::locked_down(),
        input.tool_version.clone(),
    );
    result
        .copybook_fingerprint
        .clone_from(&input.copybook_fingerprint);
    result
        .source_fingerprint
        .clone_from(&input.source_fingerprint);
    result
}

/// Build one scenario assessment for a parsed construct.
fn assess_construct(
    construct: &AdviseConstruct,
    options: &EffectiveOptions,
    evidence_dropped: &mut usize,
) -> ScenarioAssessment {
    let (scenario_id, feature_id, fixed, error_identity, note) = construct_plan(&construct.kind);
    let mut item = match fixed {
        Some(status) => ScenarioAssessment::bounded(scenario_id, status, "stable"),
        None if feature_id == "none" => {
            ScenarioAssessment::bounded(scenario_id, AssessmentStatus::Unknown, "stable")
        }
        None => match crate::find_feature(feature_id) {
            Some(feature) => ScenarioAssessment::bounded(
                scenario_id,
                assessment_for_status(feature.status),
                stability_for(feature.status),
            ),
            None => ScenarioAssessment::bounded(scenario_id, AssessmentStatus::Unknown, "stable"),
        },
    };
    item.record_formats = vec![options.format.clone()];
    item.codepages = vec![options.codepage.clone()];
    item.error_identity = error_identity.map(str::to_string);
    if !note.is_empty() {
        item.limitation_or_remediation = bound_chars(note.to_string(), MAX_SUGGESTION_CHARS);
    }
    if !item.push_evidence(format!("construct:{}", construct.detail)) {
        *evidence_dropped += 1;
    }
    item.set_next_action(next_action_for(&construct.kind, scenario_id));
    item
}

/// (scenario ID, matrix feature, fixed outcome, error identity, note).
/// Fixed outcomes cover distinctions the matrix feature table cannot
/// express: tail vs non-tail ODO, and REDEFINES, which is unconditional
/// core behavior with ledger rows but no matrix feature. Each is pinned
/// by advise tests, which ledger rows may anchor as cli evidence.
fn construct_plan(
    kind: &ConstructKind,
) -> (
    &'static str,
    &'static str,
    Option<AssessmentStatus>,
    Option<&'static str>,
    &'static str,
) {
    match kind {
        ConstructKind::OccursDepending => (
            "struct.odo.tail_fixed",
            "occurs-depending",
            Some(AssessmentStatus::Supported),
            None,
            "Tail ODO is supported; non-tail and over-REDEFINES variants are rejected.",
        ),
        ConstructKind::NonTailOdo => (
            "struct.odo.not_tail",
            "occurs-depending",
            Some(AssessmentStatus::Rejected),
            Some("CBKP021_ODO_NOT_TAIL"),
            "Only tail ODO is supported; move the OCCURS to the record tail.",
        ),
        ConstructKind::NestedOdo => (
            "struct.odo.nested",
            "nested-odo",
            None,
            Some("CBKP022_NESTED_ODO"),
            "O1-O4 nesting is supported; O5/O6 shapes are rejected.",
        ),
        ConstructKind::Renames => (
            "struct.renames.r1_r3",
            "level-66-renames",
            None,
            None,
            "Same-scope and THRU renames hold; cross-OCCURS and over-REDEFINES are limited.",
        ),
        ConstructKind::Redefines => (
            "struct.redefines.scalar",
            "none",
            Some(AssessmentStatus::Supported),
            None,
            "Scalar and group REDEFINES hold; encode ambiguity and nested cases are limited.",
        ),
        ConstructKind::Level88 => (
            "struct.level88.single_value",
            "level-88",
            None,
            None,
            "Condition names are metadata; a mismatch is a failed condition, never a decode error.",
        ),
        ConstructKind::EditedPic => ("matrix:edited-pic", "edited-pic", None, None, ""),
        ConstructKind::Comp1Comp2 => ("matrix:comp-1-comp-2", "comp-1-comp-2", None, None, ""),
        ConstructKind::SignSeparate => ("matrix:sign-separate", "sign-separate", None, None, ""),
        ConstructKind::Unmapped => ("unmapped", "none", None, None, ""),
    }
}

/// Stability label for a matrix status.
#[inline]
fn stability_for(status: crate::SupportStatus) -> &'static str {
    match status {
        crate::SupportStatus::Supported | crate::SupportStatus::Partial => "stable",
        crate::SupportStatus::Planned | crate::SupportStatus::NotPlanned => "beta",
    }
}

/// Next real action for a construct kind. Names implemented behavior only.
fn next_action_for(kind: &ConstructKind, scenario_id: &str) -> String {
    match kind {
        ConstructKind::Unmapped => {
            "Run `copybook support --check <feature>` for the construct, or narrow the copybook to inventoried clauses.".to_string()
        }
        _ => format!("Scenario evidence: {scenario_id}; re-run with --format and --codepage for the target path."),
    }
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
    fn encoding_advise_dropped_scenarios_downgrade_limited_and_beta() {
        for status in [AssessmentStatus::Limited, AssessmentStatus::Beta] {
            let scenarios: Vec<ScenarioAssessment> = (0..MAX_SCENARIOS + 2)
                .map(|index| {
                    let item_status = if index == 0 {
                        status
                    } else {
                        AssessmentStatus::Supported
                    };
                    assessment(&format!("row.{index:03}"), item_status)
                })
                .collect();
            let result = AdviseResult::bounded(
                options(),
                scenarios,
                0,
                RedactionState::locked_down(),
                "copybook 0.7.0",
            );
            assert_eq!(
                result.verdict,
                Verdict::PartialUnknown,
                "dropped scenarios must not leave a {status:?} certainty"
            );
        }
    }

    #[test]
    fn encoding_advise_dropped_scenarios_preserve_rejection() {
        let scenarios: Vec<ScenarioAssessment> = (0..MAX_SCENARIOS + 2)
            .map(|index| {
                let item_status = if index == 0 {
                    AssessmentStatus::Rejected
                } else {
                    AssessmentStatus::Supported
                };
                assessment(&format!("row.{index:03}"), item_status)
            })
            .collect();
        let result = AdviseResult::bounded(
            options(),
            scenarios,
            0,
            RedactionState::locked_down(),
            "copybook 0.7.0",
        );
        assert_eq!(result.verdict, Verdict::Rejected);
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

    fn input_with(constructs: Vec<AdviseConstruct>, parse_error: Option<String>) -> AdviseInput {
        AdviseInput::bounded(constructs, parse_error, options(), "copybook 0.7.0")
    }

    fn construct(kind: ConstructKind) -> AdviseConstruct {
        AdviseConstruct::bounded(kind, "FIELD-A", Some(3))
    }

    #[test]
    fn encoding_advise_analysis_maps_constructs() {
        let input = input_with(
            vec![
                construct(ConstructKind::OccursDepending),
                construct(ConstructKind::NonTailOdo),
                construct(ConstructKind::NestedOdo),
                construct(ConstructKind::Renames),
                construct(ConstructKind::Redefines),
                construct(ConstructKind::Level88),
                construct(ConstructKind::Unmapped),
            ],
            None,
        );
        let result = analyze(&input);
        let by_id: std::collections::BTreeMap<&str, AssessmentStatus> = result
            .scenarios
            .iter()
            .map(|item| (item.scenario_id.as_str(), item.status))
            .collect();
        assert_eq!(by_id["struct.odo.tail_fixed"], AssessmentStatus::Supported);
        assert_eq!(by_id["struct.odo.not_tail"], AssessmentStatus::Rejected);
        assert_eq!(by_id["struct.odo.nested"], AssessmentStatus::Limited);
        assert_eq!(by_id["struct.renames.r1_r3"], AssessmentStatus::Limited);
        assert_eq!(
            by_id["struct.redefines.scalar"],
            AssessmentStatus::Supported
        );
        assert_eq!(
            by_id["struct.level88.single_value"],
            AssessmentStatus::Supported
        );
        assert_eq!(by_id["unmapped"], AssessmentStatus::Unknown);
        assert_eq!(result.verdict, Verdict::Rejected);
        for item in &result.scenarios {
            assert_eq!(item.record_formats, vec!["fixed".to_string()]);
            assert_eq!(item.codepages, vec!["ascii".to_string()]);
        }
        let nested = result
            .scenarios
            .iter()
            .find(|item| item.scenario_id == "struct.odo.nested")
            .expect("nested assessment");
        assert_eq!(nested.error_identity.as_deref(), Some("CBKP022_NESTED_ODO"));
    }

    #[test]
    fn encoding_advise_analysis_parse_error_is_invalid_not_fatal() {
        let input = input_with(vec![], Some("unexpected token at line 2".to_string()));
        let result = analyze(&input);
        assert_eq!(result.verdict, Verdict::InvalidInput);
        assert_eq!(result.scenarios.len(), 1);
        assert_eq!(result.scenarios[0].scenario_id, "unparsed");
        assert_eq!(result.scenarios[0].status, AssessmentStatus::Invalid);
    }

    #[test]
    fn encoding_advise_analysis_empty_is_unknown() {
        let result = analyze(&input_with(vec![], None));
        assert_eq!(result.verdict, Verdict::PartialUnknown);
        assert!(result.scenarios.is_empty());
    }
}
