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
//! schema/stability class ......... stable since 0.7.1
//!                                  (ADVISE_STABILITY_CLASS; beta in 0.7.0);
//!                                  JSON schema versioned by
//!                                  ADVISE_RESULT_SCHEMA_VERSION
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
//! [`crate::advise::AssessmentStatus::Unknown`], never rendered as certainty.

use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::ledger_projection::{LedgerProjection, projection_for};

/// Version of the advisory result JSON contract.
///
/// Stable since 0.7.1 (beta as `0.7.0-beta.1` in 0.7.0): automation must
/// match this exact string, never parse prose.
pub const ADVISE_RESULT_SCHEMA_VERSION: &str = "1.0";

/// Stability class of the advisory contract.
pub const ADVISE_STABILITY_CLASS: &str = "stable";

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
    /// the *complete* evaluated set before truncation, so a presentation
    /// bound can never discard a higher-precedence failure (#977). A
    /// dropped scenario downgrades a non-negative complete-set verdict
    /// ([`Verdict::Supported`], [`Verdict::SupportedWithLimits`], or
    /// [`Verdict::Beta`]) to [`Verdict::PartialUnknown`] so a bound can
    /// never manufacture certainty. Complete-set negative verdicts are
    /// preserved, including when every negative assessment sorts beyond
    /// the cap.
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
        let complete_statuses: Vec<AssessmentStatus> =
            scenarios.iter().map(|item| item.status).collect();
        let complete_verdict = derive_verdict(&complete_statuses);
        scenarios.truncate(MAX_SCENARIOS);
        let reported = scenarios.len();
        let mut verdict = complete_verdict;
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
            format!(
                "{{\"schema_version\":\"{ADVISE_RESULT_SCHEMA_VERSION}\",\"verdict\":\"tool-failure\"}}"
            )
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
    /// Plain alphanumeric field (`PIC X`, #980 first family).
    Alphanumeric,
    /// Unsigned display numeric without scale (`PIC 9`, #980 first family).
    /// Signed, scaled, and `SIGN SEPARATE` zoned fields keep their existing
    /// mappings; the ledger row covers unsigned display only.
    DisplayNumeric,
    /// Binary integer (`COMP`, #980 second family). Big-endian per
    /// mainframe convention; see the ledger row for the covered widths.
    BinaryInt,
    /// Packed decimal (`COMP-3`, #980 second family), signed, scaled, and
    /// unsigned with mainframe sign-nibble conventions.
    PackedDecimal,
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
    /// Typed identity for `parse_error` (e.g. `CBKP021_ODO_NOT_TAIL`),
    /// carried separately so machine readers never parse prose (#979).
    /// `None` preserves the historical prose-only projection.
    pub parse_error_identity: Option<String>,
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
            parse_error_identity: None,
            options,
            copybook_fingerprint: None,
            source_fingerprint: None,
            tool_version: tool_version.into(),
        }
    }

    /// Attach the typed identity for `parse_error` without disturbing
    /// existing `bounded` callers (#979). The identity must be a stable
    /// code (never prose, paths, or payload); analysis copies it verbatim
    /// into the `unparsed` assessment's `error_identity`.
    #[inline]
    #[must_use]
    pub fn with_parse_error_identity(mut self, identity: impl Into<String>) -> Self {
        self.parse_error_identity = Some(bound_chars(identity.into(), MAX_SUGGESTION_CHARS));
        self
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
        item.error_identity.clone_from(&input.parse_error_identity);
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

/// Assessment resolved against the ledger projection (#978): the requested
/// format selects the applicable row, and row evidence is never relabelled
/// onto another path.
struct ResolvedAssessment {
    scenario_id: &'static str,
    status: AssessmentStatus,
    stability: &'static str,
    layers: &'static [AffectedLayer],
    evidence: &'static [&'static str],
    error_identity: Option<&'static str>,
    note: Cow<'static, str>,
}

/// Build one scenario assessment for a parsed construct under the requested
/// options. Construct location (`construct:<detail>`) is preserved separately
/// from evidence authority (ledger `path::test` refs): a field name is never
/// emitted as proof.
fn assess_construct(
    construct: &AdviseConstruct,
    options: &EffectiveOptions,
    evidence_dropped: &mut usize,
) -> ScenarioAssessment {
    let resolved = resolve_assessment(&construct.kind, options);
    let mut item =
        ScenarioAssessment::bounded(resolved.scenario_id, resolved.status, resolved.stability);
    item.record_formats = vec![options.format.clone()];
    item.codepages = vec![options.codepage.clone()];
    item.affected_layers = resolved.layers.to_vec();
    item.error_identity = resolved.error_identity.map(str::to_string);
    if !resolved.note.is_empty() {
        item.limitation_or_remediation =
            bound_chars(resolved.note.into_owned(), MAX_SUGGESTION_CHARS);
    }
    for reference in resolved.evidence {
        if !item.push_evidence((*reference).to_string()) {
            *evidence_dropped += 1;
        }
    }
    if !item.push_evidence(format!("construct:{}", construct.detail)) {
        *evidence_dropped += 1;
    }
    item.set_next_action(next_action_for(
        &construct.kind,
        resolved.scenario_id,
        options,
    ));
    item
}

/// Resolve a construct plus requested options against the ledger projection.
/// A positive claim resolves only to a row evidencing the requested format;
/// formats without a row stay [`AssessmentStatus::Unknown`] with an explicit
/// unevaluated-format limitation. Rejections are format-independent by
/// design (the tail rule and nesting bans hold on every path), so rejected
/// rows resolve regardless of format. A missing projection row fails
/// conservatively to `Unknown` under the intended row ID, never to a
/// neighboring row's evidence.
fn resolve_assessment(kind: &ConstructKind, options: &EffectiveOptions) -> ResolvedAssessment {
    match kind {
        ConstructKind::OccursDepending => match options.format.as_str() {
            "fixed" => row_assessment(
                "struct.odo.tail_fixed",
                None,
                Some("Tail ODO is supported; non-tail and over-REDEFINES variants are rejected."),
            ),
            "rdw" => row_assessment(
                "struct.odo.tail_rdw_variable",
                None,
                Some(
                    "Tail ODO on RDW is supported via variable-length records; non-tail and over-REDEFINES variants are rejected.",
                ),
            ),
            _ => ResolvedAssessment {
                scenario_id: "matrix:occurs-depending",
                status: AssessmentStatus::Unknown,
                stability: "stable",
                layers: &[],
                evidence: &[],
                error_identity: None,
                note: Cow::Owned(format!(
                    "No ledger evidence for tail ODO under record format `{}`; fixed and RDW paths are evidenced separately, and codepage and dialect dimensions are not separately evaluated for this construct.",
                    options.format
                )),
            },
        },
        ConstructKind::NonTailOdo => {
            row_assessment("struct.odo.not_tail", Some("CBKP021_ODO_NOT_TAIL"), None)
        }
        ConstructKind::NestedOdo => {
            row_assessment("struct.odo.nested", Some("CBKP022_NESTED_ODO"), None)
        }
        ConstructKind::Alphanumeric => row_for_format(
            "struct.field.alphanumeric",
            "matrix:alphanumeric",
            "alphanumeric fields",
            options,
        ),
        ConstructKind::DisplayNumeric => row_for_format(
            "struct.field.display_numeric",
            "matrix:display-numeric",
            "unsigned display numerics",
            options,
        ),
        ConstructKind::BinaryInt => row_for_format(
            "struct.field.binary_int",
            "matrix:binary-int",
            "COMP binary integers",
            options,
        ),
        ConstructKind::PackedDecimal => row_for_format(
            "struct.field.packed_decimal",
            "matrix:packed-decimal",
            "COMP-3 packed decimals",
            options,
        ),
        ConstructKind::Renames
        | ConstructKind::Redefines
        | ConstructKind::Level88
        | ConstructKind::EditedPic
        | ConstructKind::Comp1Comp2
        | ConstructKind::SignSeparate
        | ConstructKind::Unmapped => {
            let (scenario_id, feature_id, fixed, error_identity, note) = construct_plan(kind);
            let (status, stability) = match fixed {
                Some(status) => (status, "stable"),
                None if feature_id == "none" => (AssessmentStatus::Unknown, "stable"),
                None => match crate::find_feature(feature_id) {
                    Some(feature) => (
                        assessment_for_status(feature.status),
                        stability_for(feature.status),
                    ),
                    None => (AssessmentStatus::Unknown, "stable"),
                },
            };
            ResolvedAssessment {
                scenario_id,
                status,
                stability,
                layers: &[],
                evidence: &[],
                error_identity,
                note: Cow::Borrowed(note),
            }
        }
    }
}

/// Resolve one ledger row for the requested format (#980). The row applies
/// only when the format is inside the row's applicability; any other format
/// stays [`AssessmentStatus::Unknown`] under an explicit `matrix:` ID with
/// an unevaluated-format limitation, never relabelled row evidence.
fn row_for_format(
    id: &'static str,
    fallback_id: &'static str,
    family: &'static str,
    options: &EffectiveOptions,
) -> ResolvedAssessment {
    let applicable =
        projection_for(id).is_some_and(|row| row.formats.contains(&options.format.as_str()));
    if applicable {
        return row_assessment(id, None, None);
    }
    ResolvedAssessment {
        scenario_id: fallback_id,
        status: AssessmentStatus::Unknown,
        stability: "stable",
        layers: &[],
        evidence: &[],
        error_identity: None,
        note: Cow::Owned(format!(
            "No ledger evidence for {family} under record format `{}`; codepage and dialect dimensions are not separately evaluated for this construct.",
            options.format
        )),
    }
}

/// Resolve one ledger row by ID, carrying its outcome, stability, layers,
/// and evidence. `note_override` replaces the row limitations text when the
/// caller needs path-specific prose; otherwise the row speaks for itself.
fn row_assessment(
    id: &'static str,
    error_identity: Option<&'static str>,
    note_override: Option<&'static str>,
) -> ResolvedAssessment {
    match projection_for(id) {
        Some(row) => {
            let row: &LedgerProjection = row;
            ResolvedAssessment {
                scenario_id: row.id,
                status: row.status,
                stability: row.stability,
                layers: row.layers,
                evidence: row.evidence,
                error_identity,
                note: Cow::Borrowed(note_override.unwrap_or(row.limitations)),
            }
        }
        None => ResolvedAssessment {
            scenario_id: id,
            status: AssessmentStatus::Unknown,
            stability: "stable",
            layers: &[],
            evidence: &[],
            error_identity,
            note: Cow::Owned(format!(
                "Scenario authority has no row for `{id}`; failing conservatively instead of substituting a neighboring row."
            )),
        },
    }
}

/// (scenario ID, matrix feature, fixed outcome, error identity, note).
/// ODO kinds resolve in [`resolve_assessment`] against the ledger projection
/// (format-selected rows, scenario authority over the coarse matrix); the
/// remaining arms cover distinctions the matrix feature table cannot express,
/// such as REDEFINES, which is unconditional core behavior with ledger rows
/// but no matrix feature.
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
        // ODO and ordinary-field kinds resolve in `resolve_assessment`
        // against the ledger projection and never reach this table; they
        // share the conservative unmapped row if that ever changes.
        ConstructKind::OccursDepending
        | ConstructKind::NonTailOdo
        | ConstructKind::NestedOdo
        | ConstructKind::Alphanumeric
        | ConstructKind::DisplayNumeric
        | ConstructKind::BinaryInt
        | ConstructKind::PackedDecimal
        | ConstructKind::Unmapped => ("unmapped", "none", None, None, ""),
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

/// Next real action for a construct kind under the requested options (#979).
/// Names implemented behavior only: `--record-format` selects framing,
/// `--codepage` selects the codepage, and the values echoed are the
/// evaluated ones, so the command is runnable as written against the real
/// CLI parser (placeholders stay explicit where a value is unknown).
fn next_action_for(kind: &ConstructKind, scenario_id: &str, options: &EffectiveOptions) -> String {
    match kind {
        ConstructKind::Unmapped => {
            "Run `copybook support --check <feature>` for the construct, or narrow the copybook to inventoried clauses.".to_string()
        }
        _ => format!(
            "Scenario evidence: {scenario_id}; re-run with --record-format {} --codepage {} for the target path.",
            options.format, options.codepage
        ),
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

    fn sixty_four_supported_plus(id: &str, status: AssessmentStatus) -> Vec<ScenarioAssessment> {
        let mut scenarios: Vec<ScenarioAssessment> = (0..MAX_SCENARIOS)
            .map(|index| assessment(&format!("a{index:03}"), AssessmentStatus::Supported))
            .collect();
        scenarios.push(assessment(id, status));
        scenarios
    }

    fn bounded_of(scenarios: Vec<ScenarioAssessment>) -> AdviseResult {
        AdviseResult::bounded(
            options(),
            scenarios,
            0,
            RedactionState::locked_down(),
            "copybook 0.7.0",
        )
    }

    #[test]
    fn encoding_advise_omitted_tool_failure_dominates_retained_rejection() {
        // #977 counterexample: 63 supported + 1 retained rejection sort
        // before an omitted tool failure; complete-set precedence is
        // tool failure, not rejection.
        let mut scenarios = sixty_four_supported_plus("z999", AssessmentStatus::ToolFailure);
        scenarios[0] = assessment("a000", AssessmentStatus::Rejected);
        let result = bounded_of(scenarios);
        assert_eq!(result.truncation.scenarios_considered, MAX_SCENARIOS + 1);
        assert_eq!(result.truncation.scenarios_reported, MAX_SCENARIOS);
        assert_eq!(result.verdict, Verdict::ToolFailure);
    }

    #[test]
    fn encoding_advise_omitted_invalid_dominates_retained_rejection() {
        let mut scenarios = sixty_four_supported_plus("z999", AssessmentStatus::Invalid);
        scenarios[0] = assessment("a000", AssessmentStatus::Rejected);
        let result = bounded_of(scenarios);
        assert_eq!(result.verdict, Verdict::InvalidInput);
    }

    #[test]
    fn encoding_advise_omitted_negative_counts_with_supported_retained_set() {
        // An omitted negative with an otherwise supported retained set must
        // surface the negative, not the dropped-detail unknown.
        for (status, expected) in [
            (AssessmentStatus::Rejected, Verdict::Rejected),
            (AssessmentStatus::Invalid, Verdict::InvalidInput),
            (AssessmentStatus::ToolFailure, Verdict::ToolFailure),
            (AssessmentStatus::Unknown, Verdict::PartialUnknown),
        ] {
            let result = bounded_of(sixty_four_supported_plus("z999", status));
            assert_eq!(
                result.verdict, expected,
                "omitted {status:?} must set the complete-set verdict"
            );
            assert_eq!(result.truncation.scenarios_considered, MAX_SCENARIOS + 1);
            assert_eq!(result.truncation.scenarios_reported, MAX_SCENARIOS);
        }
    }

    #[test]
    fn encoding_advise_complete_set_verdict_ignores_input_order() {
        // Reordered input with an omitted beta and a retained rejection
        // still yields the complete-set verdict.
        let mut scenarios = sixty_four_supported_plus("z999", AssessmentStatus::Beta);
        scenarios[0] = assessment("a000", AssessmentStatus::Rejected);
        scenarios.reverse();
        let result = bounded_of(scenarios);
        assert_eq!(result.verdict, Verdict::Rejected);
        assert_eq!(
            result
                .scenarios
                .first()
                .map(|item| item.scenario_id.as_str()),
            Some("a000")
        );
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
        // #978: the ledger rejects nested ODO by design (the supported O1-O4
        // subset has no row), so scenario authority overrides the coarse
        // matrix Partial that previously rendered as Limited.
        assert_eq!(by_id["struct.odo.nested"], AssessmentStatus::Rejected);
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

    fn input_with_options(
        constructs: Vec<AdviseConstruct>,
        parse_error: Option<String>,
        format: &str,
        codepage: &str,
    ) -> AdviseInput {
        AdviseInput::bounded(
            constructs,
            parse_error,
            EffectiveOptions::bounded(format, codepage, "normative"),
            "copybook 0.7.0",
        )
    }

    fn single_scenario(format: &str, kind: ConstructKind) -> ScenarioAssessment {
        let input = input_with_options(
            vec![AdviseConstruct::bounded(kind, "DATA", None)],
            None,
            format,
            "ascii",
        );
        let result = analyze(&input);
        assert_eq!(result.scenarios.len(), 1);
        assert_eq!(result.truncation.scenarios_considered, 1);
        assert_eq!(result.truncation.scenarios_reported, 1);
        result.scenarios.into_iter().next().expect("one scenario")
    }

    #[test]
    fn encoding_advise_tail_odo_resolves_per_format() {
        // #978 acceptance: the same tail-ODO construct resolves to the
        // applicable ledger row per format; fixed-only evidence is never
        // relabelled as RDW/VB evidence.
        let fixed = single_scenario("fixed", ConstructKind::OccursDepending);
        assert_eq!(fixed.scenario_id, "struct.odo.tail_fixed");
        assert_eq!(fixed.status, AssessmentStatus::Supported);
        assert_eq!(fixed.record_formats, vec!["fixed".to_string()]);

        let rdw = single_scenario("rdw", ConstructKind::OccursDepending);
        assert_eq!(rdw.scenario_id, "struct.odo.tail_rdw_variable");
        assert_eq!(rdw.status, AssessmentStatus::Supported);
        assert_eq!(rdw.record_formats, vec!["rdw".to_string()]);

        let vb = single_scenario("vb", ConstructKind::OccursDepending);
        assert_eq!(vb.scenario_id, "matrix:occurs-depending");
        assert_eq!(vb.status, AssessmentStatus::Unknown);
        assert_eq!(vb.record_formats, vec!["vb".to_string()]);
        assert!(
            vb.limitation_or_remediation.contains("vb"),
            "VB must name the unevaluated format, got: {}",
            vb.limitation_or_remediation
        );
        assert!(
            vb.evidence_refs
                .iter()
                .all(|reference| reference.starts_with("construct:")),
            "unevaluated path carries location only, never row evidence"
        );
    }

    #[test]
    fn encoding_advise_tail_odo_verdict_follows_format() {
        for (format, expected) in [
            ("fixed", Verdict::Supported),
            ("rdw", Verdict::Supported),
            ("vb", Verdict::PartialUnknown),
        ] {
            let input = input_with_options(
                vec![AdviseConstruct::bounded(
                    ConstructKind::OccursDepending,
                    "DATA",
                    None,
                )],
                None,
                format,
                "ascii",
            );
            assert_eq!(analyze(&input).verdict, expected, "format {format}");
        }
    }

    #[test]
    fn encoding_advise_resolved_rows_carry_layers_and_ledger_evidence() {
        // #978: applicable layers and real evidence refs within bounds;
        // construct location stays separate from evidence authority.
        let fixed = single_scenario("fixed", ConstructKind::OccursDepending);
        assert_eq!(
            fixed.affected_layers,
            vec![
                AffectedLayer::Parse,
                AffectedLayer::Layout,
                AffectedLayer::Decode,
                AffectedLayer::Encode,
            ]
        );
        assert!(
            fixed
                .evidence_refs
                .contains(&"crates/copybook-core/tests/odo_tail_validation.rs::odo_tail_ok_with_children_but_no_sibling_after".to_string())
        );
        assert!(
            fixed
                .evidence_refs
                .iter()
                .any(|reference| reference.starts_with("construct:"))
        );
        assert!(fixed.evidence_refs.len() <= MAX_EVIDENCE_REFS);
        assert_eq!(fixed.codepages, vec!["ascii".to_string()]);

        let nested = single_scenario("fixed", ConstructKind::NestedOdo);
        assert_eq!(nested.status, AssessmentStatus::Rejected);
        assert_eq!(nested.affected_layers, vec![AffectedLayer::Parse]);
        assert!(
            nested
                .evidence_refs
                .contains(&"crates/copybook-core/tests/nested_odo_negative_tests.rs::test_o5_nested_odo_basic_rejection".to_string())
        );
        assert_eq!(nested.error_identity.as_deref(), Some("CBKP022_NESTED_ODO"));
        assert!(
            nested.limitation_or_remediation.contains("O1-O4"),
            "nested note must carry the ledger caveat, got: {}",
            nested.limitation_or_remediation
        );
    }

    #[test]
    fn encoding_advise_projection_covers_resolved_rows() {
        // The rows assessment resolves by ID must exist in the compiled
        // projection; a missing row would fail conservatively to Unknown.
        for id in [
            "struct.odo.tail_fixed",
            "struct.odo.tail_rdw_variable",
            "struct.odo.not_tail",
            "struct.odo.nested",
            "struct.field.alphanumeric",
            "struct.field.display_numeric",
            "struct.field.binary_int",
            "struct.field.packed_decimal",
        ] {
            assert!(
                crate::ledger_projection::projection_for(id).is_some(),
                "projection must carry row {id}"
            );
        }
    }

    #[test]
    fn encoding_advise_ordinary_fields_resolve_with_evidence() {
        // #980 first family: ordinary fields resolve to applicable rows
        // with real layers and evidence, and state the copybook-only limit.
        let alpha = single_scenario("fixed", ConstructKind::Alphanumeric);
        assert_eq!(alpha.scenario_id, "struct.field.alphanumeric");
        assert_eq!(alpha.status, AssessmentStatus::Supported);
        assert_eq!(
            alpha.affected_layers,
            vec![
                AffectedLayer::Parse,
                AffectedLayer::Layout,
                AffectedLayer::Decode,
                AffectedLayer::Encode,
                AffectedLayer::RoundTrip,
            ]
        );
        assert!(
            alpha.evidence_refs.contains(
                &"crates/copybook-core/tests/parser_comprehensive.rs::test_pic_alphanumeric"
                    .to_string()
            )
        );
        assert!(
            alpha
                .limitation_or_remediation
                .contains("never validates unseen record payloads"),
            "copybook-only limit must be explicit, got: {}",
            alpha.limitation_or_remediation
        );

        let numeric = single_scenario("rdw", ConstructKind::DisplayNumeric);
        assert_eq!(numeric.scenario_id, "struct.field.display_numeric");
        assert_eq!(numeric.status, AssessmentStatus::Supported);
        assert_eq!(numeric.record_formats, vec!["rdw".to_string()]);
        assert!(
            numeric
                .evidence_refs
                .contains(&"crates/copybook-codec/tests/codec_roundtrip_exhaustive.rs::roundtrip_ascii_zoned_unsigned".to_string())
        );
    }

    #[test]
    fn encoding_advise_numeric_fields_resolve_with_evidence() {
        // #980 second family: COMP binary and COMP-3 packed resolve to
        // applicable rows with real layers and evidence.
        let binary = single_scenario("fixed", ConstructKind::BinaryInt);
        assert_eq!(binary.scenario_id, "struct.field.binary_int");
        assert_eq!(binary.status, AssessmentStatus::Supported);
        assert_eq!(
            binary.affected_layers,
            vec![
                AffectedLayer::Parse,
                AffectedLayer::Layout,
                AffectedLayer::Decode,
                AffectedLayer::Encode,
                AffectedLayer::RoundTrip,
            ]
        );
        assert!(
            binary.evidence_refs.contains(
                &"crates/copybook-codec/tests/comp_binary_deep.rs::test_comp_16bit_signed_zero"
                    .to_string()
            )
        );
        assert!(
            binary
                .limitation_or_remediation
                .contains("Big-endian byte order"),
            "binary row must state byte order, got: {}",
            binary.limitation_or_remediation
        );

        let packed = single_scenario("rdw", ConstructKind::PackedDecimal);
        assert_eq!(packed.scenario_id, "struct.field.packed_decimal");
        assert_eq!(packed.status, AssessmentStatus::Supported);
        assert_eq!(packed.record_formats, vec!["rdw".to_string()]);
        assert!(
            packed
                .evidence_refs
                .contains(&"crates/copybook-codec/tests/binary_roundtrip_fidelity_tests.rs::test_comp3_packed_decimal_roundtrip_accuracy".to_string())
        );
        assert!(
            packed
                .limitation_or_remediation
                .contains("never validates unseen record payloads"),
            "copybook-only limit must be explicit, got: {}",
            packed.limitation_or_remediation
        );
    }

    #[test]
    fn encoding_advise_numeric_fields_stay_unknown_without_row() {
        // #980: VB has no binary/packed rows; both stay unknown under
        // explicit matrix IDs instead of borrowing fixed evidence.
        for (kind, id) in [
            (ConstructKind::BinaryInt, "matrix:binary-int"),
            (ConstructKind::PackedDecimal, "matrix:packed-decimal"),
        ] {
            let item = single_scenario("vb", kind);
            assert_eq!(item.scenario_id, id);
            assert_eq!(item.status, AssessmentStatus::Unknown);
        }
    }

    #[test]
    fn encoding_advise_ordinary_fields_stay_unknown_without_row() {
        // #980: VB has no ordinary-field row; the assessment stays unknown
        // under an explicit matrix ID instead of borrowing fixed evidence.
        let alpha = single_scenario("vb", ConstructKind::Alphanumeric);
        assert_eq!(alpha.scenario_id, "matrix:alphanumeric");
        assert_eq!(alpha.status, AssessmentStatus::Unknown);
        assert!(
            alpha
                .evidence_refs
                .iter()
                .all(|reference| reference.starts_with("construct:")),
            "unevaluated path carries location only, got: {:?}",
            alpha.evidence_refs
        );
        let numeric = single_scenario("vb", ConstructKind::DisplayNumeric);
        assert_eq!(numeric.scenario_id, "matrix:display-numeric");
        assert_eq!(numeric.status, AssessmentStatus::Unknown);
    }

    #[test]
    fn encoding_advise_analysis_parse_error_is_invalid_not_fatal() {
        let input = input_with(vec![], Some("unexpected token at line 2".to_string()));
        let result = analyze(&input);
        assert_eq!(result.verdict, Verdict::InvalidInput);
        assert_eq!(result.scenarios.len(), 1);
        assert_eq!(result.scenarios[0].scenario_id, "unparsed");
        assert_eq!(result.scenarios[0].status, AssessmentStatus::Invalid);
        assert_eq!(result.scenarios[0].error_identity, None);
    }

    #[test]
    fn encoding_advise_parse_error_identity_rides_beside_prose() {
        // #979: the typed identity occupies the machine field while prose
        // stays free to change; nothing derives the code from the text.
        let input = input_with(
            vec![],
            Some("human prose that never names a code".to_string()),
        )
        .with_parse_error_identity("CBKP021_ODO_NOT_TAIL");
        let result = analyze(&input);
        assert_eq!(result.verdict, Verdict::InvalidInput);
        assert_eq!(
            result.scenarios[0].error_identity.as_deref(),
            Some("CBKP021_ODO_NOT_TAIL")
        );
        assert!(
            !result.scenarios[0]
                .next_action
                .contains("CBKP021_ODO_NOT_TAIL"),
            "identity must not leak into prose in this projection"
        );
    }

    #[test]
    fn encoding_advise_next_action_names_real_flags_with_values() {
        // #979: remediation references `--record-format` (framing) and
        // `--codepage` with the evaluated values, never bare `--format`.
        let item = single_scenario("rdw", ConstructKind::OccursDepending);
        assert!(
            item.next_action.contains("--record-format rdw"),
            "got: {}",
            item.next_action
        );
        assert!(
            item.next_action.contains("--codepage ascii"),
            "got: {}",
            item.next_action
        );
        assert!(
            !item.next_action.contains("--format "),
            "framing flag must be --record-format, got: {}",
            item.next_action
        );
    }

    #[test]
    fn encoding_advise_analysis_empty_is_unknown() {
        let result = analyze(&input_with(vec![], None));
        assert_eq!(result.verdict, Verdict::PartialUnknown);
        assert!(result.scenarios.is_empty());
    }
}
