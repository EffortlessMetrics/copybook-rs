// SPDX-License-Identifier: AGPL-3.0-or-later
//! Static field-to-byte and byte-to-field ownership over a resolved manifest.
//!
//! A query answers one question against payload-relative schema offsets:
//! which field owns payload byte N, or which byte range backs a field path.
//! Both directions run over the same [`ResolvedManifest`], whether it was
//! loaded from a manifest document or generated in memory from a
//! copybook/profile pair, so the two inputs agree byte for byte.
//!
//! The engine never reads record data: occurrence indices are layout
//! arithmetic (uniform repetition stride), ODO presence beyond the minimum
//! is reported as possible rather than present, and VB block structure is
//! out of scope for the static query. Anything the static layout cannot
//! decide is a typed state or error, never a guess.

#![allow(clippy::missing_inline_in_public_items)]

use crate::resolved_manifest::{ManifestField, ManifestOccurs, ResolvedManifest};
use serde::{Deserialize, Serialize};

/// Cap on retained ownership matches: the manifest bounds `fields`, so a
/// query walk is bounded, but the report states truncation explicitly
/// instead of growing without limit.
pub const MAX_OWNERSHIP_MATCHES: usize = 256;

/// Prefix the parser assigns renamed `FILLER` fields.
const FILLER_PREFIX: &str = "_filler_";

/// Coordinate system a byte query addresses. Static queries operate on
/// payload-relative schema offsets only; physical file/block/record
/// coordinates require a selected record and belong to a record-specific
/// query, which must never be confused with this one.
const COORDINATE_SYSTEM: &str = "payload-relative";

/// Query side of one ownership comparison.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum OwnershipQuery {
    /// Which field owns this payload-relative byte.
    PayloadByte {
        /// Payload-relative byte offset.
        byte: u32,
    },
    /// Which byte range backs this field path.
    FieldPath {
        /// Requested path (full dotted path, or a unique short name).
        path: String,
    },
}

/// Answered state of an ownership query.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OwnershipState {
    /// At least one layout entry owns the byte or answers the path.
    Owned,
    /// The byte lies inside the record extent but no field covers it.
    Gap,
    /// The byte lies past the record extent.
    OutOfRange,
}

/// Role of one match inside the ownership answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum MatchRole {
    /// Narrowest covering entry: the storage owner callers act on.
    Primary,
    /// Enclosing group container.
    Container,
    /// REDEFINES view over the same storage.
    View,
    /// RENAMES alias over the range.
    Alias,
}

/// Presence of a statically computed occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OccurrencePresence {
    /// Below the ODO minimum (or any fixed occurrence): layout-guaranteed.
    Guaranteed,
    /// At or past the ODO minimum: needs the record counter to confirm.
    Possible,
}

/// One occurrence index inside a repeating ancestor.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OccurrenceIndex {
    /// Dotted path of the repeating field.
    pub path: String,
    /// Zero-based occurrence index within the static span.
    pub index: u32,
    /// Static presence of this occurrence.
    pub presence: OccurrencePresence,
}

/// Repetition facts for a match on a repeating field.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RepetitionInfo {
    /// Repetition kind: `fixed` or `odo`.
    pub kind: String,
    /// Fixed count, or the ODO maximum.
    pub count: u32,
    /// Minimum repetitions.
    pub min_count: u32,
    /// ODO counter path, when the manifest records one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counter_path: Option<String>,
}

/// Numeric representation facts joined from the manifest numeric details.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipNumeric {
    /// Total digits.
    pub digits: u32,
    /// Decimal scale.
    pub scale: i32,
    /// Whether the field is signed.
    pub signed: bool,
    /// Canonical encoding tag.
    pub encoding: String,
}

/// One layout entry participating in the answer.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipMatch {
    /// Dotted field path (or RENAMES alias path).
    pub path: String,
    /// Role inside this answer.
    pub role: MatchRole,
    /// Payload-relative start byte.
    pub offset: u32,
    /// Payload-relative covered length (effective span for tables).
    pub len: u32,
    /// Payload-relative end byte (`offset + len`, saturating).
    pub end: u32,
    /// Manifest kind tag.
    pub kind: String,
    /// True for parser-renamed `FILLER` padding entries.
    pub filler: bool,
    /// Occurrence indices from the outermost repeating ancestor inward.
    pub occurrences: Vec<OccurrenceIndex>,
    /// Repetition facts when the match itself repeats.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repetition: Option<RepetitionInfo>,
    /// Ultimate REDEFINES storage owner, when this entry redefines one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub redefines: Option<String>,
    /// Numeric representation, when the manifest records one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub numeric: Option<OwnershipNumeric>,
    /// ODO minimum/maximum/counter, when the manifest records them.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub odo: Option<OdoInfo>,
    /// Level-88 condition paths declared directly under this entry.
    pub conditions: Vec<String>,
    /// RENAMES member paths for alias matches.
    pub members: Vec<String>,
}

/// ODO bounds joined from the manifest ODO details.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OdoInfo {
    /// Minimum occurrence count.
    pub min_count: u32,
    /// Maximum occurrence count.
    pub max_count: u32,
    /// Counter field path.
    pub counter_path: String,
}

/// Typed deterministic ownership answer shared by human and JSON renderers.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OwnershipReport {
    /// Asked query, echoed with its coordinate system.
    pub query: OwnershipQuery,
    /// Coordinate system of the query and every range in this report.
    pub coordinate_system: String,
    /// Manifest fingerprint (`sha256-v1:…`).
    pub manifest_fingerprint: String,
    /// Bound profile fingerprint, when the manifest records one.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile_fingerprint: Option<String>,
    /// Resolved layout fingerprint.
    pub schema_fingerprint: String,
    /// Maximum static record extent in bytes.
    pub record_len: u32,
    /// Answered state.
    pub state: OwnershipState,
    /// Matches in stable order (primary first, then views, containers,
    /// aliases). Truncated at [`MAX_OWNERSHIP_MATCHES`] with `truncated`.
    pub matches: Vec<OwnershipMatch>,
    /// True when matches were truncated at the cap.
    pub truncated: bool,
}

/// A query the static layout refuses to answer by guessing.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum OwnershipError {
    /// No field, alias, or condition answers to this path.
    #[error("unknown field path: {query}")]
    UnknownField {
        /// Requested path as given.
        query: String,
    },
    /// A short name matches several entries; the caller must qualify it.
    #[error("ambiguous field path '{query}': {candidates:?}")]
    AmbiguousField {
        /// Requested path as given.
        query: String,
        /// Fully-qualified candidates in manifest order.
        candidates: Vec<String>,
    },
}

/// Answer which field owns payload-relative byte `byte`.
///
/// Bytes past the manifest record extent report [`OwnershipState::OutOfRange`];
/// in-extent bytes no field covers report [`OwnershipState::Gap`]. Covered
/// bytes report every overlapping entry with roles, never a silent pick:
/// exactly one match carries [`MatchRole::Primary`]. Byte queries are
/// total over the manifest: every input maps to an explicit state.
#[must_use]
pub fn query_byte_owner(manifest: &ResolvedManifest, byte: u32) -> OwnershipReport {
    let state = if byte >= manifest.record_len {
        OwnershipState::OutOfRange
    } else {
        OwnershipState::Owned
    };
    let mut matches = if state == OwnershipState::Owned {
        collect_byte_matches(manifest, byte)
    } else {
        Vec::new()
    };
    if matches.is_empty() && state == OwnershipState::Owned {
        return report(
            manifest,
            OwnershipQuery::PayloadByte { byte },
            OwnershipState::Gap,
            Vec::new(),
        );
    }
    sort_matches(&mut matches);
    mark_primary(&mut matches);
    let truncated = truncate_matches(&mut matches);
    let mut answered = report(
        manifest,
        OwnershipQuery::PayloadByte { byte },
        state,
        matches,
    );
    answered.truncated = truncated;
    answered
}

/// Answer which byte range backs field path `path`.
///
/// Matching is case-insensitive, following projection: a full dotted path
/// matches exactly; a short name matches only when it names exactly one
/// field, alias, or condition, and fails closed otherwise.
///
/// # Errors
///
/// Returns [`OwnershipError::UnknownField`] when nothing matches, or
/// [`OwnershipError::AmbiguousField`] when a short name matches several
/// entries.
#[must_use = "Handle the Result or propagate the error"]
pub fn query_field_range(
    manifest: &ResolvedManifest,
    path: &str,
) -> Result<OwnershipReport, OwnershipError> {
    let query = OwnershipQuery::FieldPath {
        path: path.to_owned(),
    };
    if let Some(field) = exact_field(manifest, path) {
        let field_match = field_match(manifest, field);
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![field_match],
        ));
    }
    if let Some(alias_match) = exact_alias(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![alias_match],
        ));
    }
    if let Some(condition_match) = exact_condition(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![condition_match],
        ));
    }
    let mut candidates = short_name_candidates(manifest, path);
    candidates.sort();
    candidates.dedup();
    match candidates.len() {
        0 => Err(OwnershipError::UnknownField {
            query: path.to_owned(),
        }),
        1 => {
            let found = candidates[0].clone();
            query_field_range(manifest, &found)
        }
        _ => Err(OwnershipError::AmbiguousField {
            query: path.to_owned(),
            candidates,
        }),
    }
}

/// Effective covered span of one manifest field: single-occurrence length
/// for scalars, stride times repetition bound for tables.
fn effective_span(field: &ManifestField) -> (u32, u32) {
    // `count` is the fixed repetition count or the ODO maximum: the static
    // span bound in both cases. Minimums govern presence, not width.
    let repetitions = field
        .occurs
        .as_ref()
        .map_or(1, |occurs| occurs.count.max(1));
    let len = field.len.saturating_mul(repetitions);
    (field.offset, field.offset.saturating_add(len))
}

/// True for zero-storage manifest kinds that can never own a byte.
fn is_non_storage(kind: &str) -> bool {
    matches!(kind, "condition" | "renames")
}

/// Collect every layout entry whose effective span covers `byte`.
///
/// Top-level entries seed a recursive descent: a repeating entry contributes
/// itself with its occurrence index plus its descendants projected into that
/// occurrence, so a byte in occurrence 2 names the true within-occurrence
/// child rather than only the table. RENAMES aliases match as flat spans.
fn collect_byte_matches(manifest: &ResolvedManifest, byte: u32) -> Vec<OwnershipMatch> {
    let mut matches = Vec::new();
    for field in &manifest.fields {
        if parent_path(&field.path)
            .is_some_and(|parent| manifest.fields.iter().any(|other| other.path == parent))
        {
            continue;
        }
        descend(manifest, field, field.offset, byte, &[], &mut matches);
    }
    for renames in &manifest.renames {
        let end = renames.offset.saturating_add(renames.length);
        if byte < renames.offset || byte >= end {
            continue;
        }
        matches.push(OwnershipMatch {
            path: renames.path.clone(),
            role: MatchRole::Alias,
            offset: renames.offset,
            len: renames.length,
            end,
            kind: "renames".to_owned(),
            filler: false,
            occurrences: Vec::new(),
            repetition: None,
            redefines: None,
            numeric: None,
            odo: None,
            conditions: Vec::new(),
            members: renames.members.clone(),
        });
    }
    matches
}

/// Recursive ownership descent.
///
/// `base` is the entry's static start byte (its manifest offset for a
/// first-occurrence entry, shifted for projected descendants).
/// `inherited` carries occurrence indices from repeating ancestors.
fn descend(
    manifest: &ResolvedManifest,
    field: &ManifestField,
    base: u32,
    byte: u32,
    inherited: &[OccurrenceIndex],
    matches: &mut Vec<OwnershipMatch>,
) {
    if is_non_storage(&field.kind) || field.len == 0 {
        return;
    }
    let repetitions = field
        .occurs
        .as_ref()
        .map_or(1, |occurs| occurs.count.max(1));
    let span = field.len.saturating_mul(repetitions);
    let end = base.saturating_add(span);
    if byte < base || byte >= end {
        return;
    }
    if repetitions > 1 {
        let stride = field.len.max(1);
        let minimum = field.occurs.as_ref().map_or(1, occurs_min_owned);
        let index = byte.saturating_sub(base) / stride;
        if index >= repetitions {
            return;
        }
        let mut entered = inherited.to_vec();
        entered.push(OccurrenceIndex {
            path: field.path.clone(),
            index,
            presence: if index < minimum {
                OccurrencePresence::Guaranteed
            } else {
                OccurrencePresence::Possible
            },
        });
        let occurrence_base = base.saturating_add(index.saturating_mul(stride));
        matches.push(field_match_at(manifest, field, base, end, entered.clone()));
        for child in direct_children(manifest, &field.path) {
            let child_base =
                occurrence_base.saturating_add(child.offset.saturating_sub(field.offset));
            descend(manifest, child, child_base, byte, &entered, matches);
        }
        return;
    }
    matches.push(field_match_at(
        manifest,
        field,
        base,
        end,
        inherited.to_vec(),
    ));
    for child in direct_children(manifest, &field.path) {
        descend(manifest, child, child.offset, byte, inherited, matches);
    }
}

/// Manifest entries whose parent is exactly `path`, in manifest order.
fn direct_children<'a>(manifest: &'a ResolvedManifest, path: &str) -> Vec<&'a ManifestField> {
    manifest
        .fields
        .iter()
        .filter(|field| parent_path(&field.path).as_deref() == Some(path))
        .collect()
}

/// Build one match for a manifest field at its working base byte,
/// resolving roles, occurrences, and joined details.
fn field_match_at(
    manifest: &ResolvedManifest,
    field: &ManifestField,
    base: u32,
    end: u32,
    occurrences: Vec<OccurrenceIndex>,
) -> OwnershipMatch {
    OwnershipMatch {
        path: field.path.clone(),
        role: MatchRole::Primary,
        offset: base,
        len: end.saturating_sub(base),
        end,
        kind: field.kind.clone(),
        filler: is_filler(&field.path),
        occurrences,
        repetition: field.occurs.as_ref().map(|occurs| RepetitionInfo {
            kind: occurs_kind(occurs),
            count: occurs_count(occurs),
            min_count: occurs_min(occurs),
            counter_path: occurs.counter_path.clone(),
        }),
        redefines: field.redefines.clone(),
        numeric: numeric_for(manifest, &field.path),
        odo: odo_for(manifest, &field.path),
        conditions: conditions_under(manifest, &field.path),
        members: Vec::new(),
    }
}

/// Build one match for a field-path query: the entry's own static range
/// with no occurrence context.
fn field_match(manifest: &ResolvedManifest, field: &ManifestField) -> OwnershipMatch {
    let (start, end) = effective_span(field);
    field_match_at(manifest, field, start, end, Vec::new())
}

/// Repetition minimum for stride arithmetic: fixed tables repeat their
/// count exactly, ODO tables repeat at least their minimum.
fn occurs_min_owned(occurs: &ManifestOccurs) -> u32 {
    if occurs.kind == "fixed" {
        occurs.count.max(1)
    } else {
        occurs.min_count
    }
}

/// Stable match order: narrowest span first, deeper entries before their
/// containers on ties, then manifest order. Depth-before-container keeps a
/// leaf primary over its group when spans coincide (e.g. single-child
/// groups, REDEFINES twins); path comparison only breaks full ties.
fn sort_matches(matches: &mut [OwnershipMatch]) {
    matches.sort_by(|left, right| span_key(left).cmp(&span_key(right)));
}

/// Dotted depth of a path: more segments means a more specific entry.
fn path_depth(path: &str) -> usize {
    path.split('.').count()
}

/// Assign roles after sorting: the primary is the narrowest covering entry
/// that is itself storage (never a REDEFINES view), so the answer names one
/// storage owner plus views. Groups and strict range-containers of the
/// primary become containers; remaining overlapping entries become views.
/// Aliases keep the role they were built with.
fn mark_primary(matches: &mut [OwnershipMatch]) {
    let primary_index = matches
        .iter()
        .position(|candidate| candidate.role != MatchRole::Alias)
        .and_then(|first| {
            matches
                .iter()
                .enumerate()
                .filter(|(_, candidate)| {
                    candidate.role != MatchRole::Alias && candidate.redefines.is_none()
                })
                .min_by(|left, right| {
                    span_key(left.1)
                        .cmp(&span_key(right.1))
                        .then_with(|| left.0.cmp(&right.0))
                })
                .map(|(index, _)| index)
                .or(Some(first))
        });
    let Some(primary_index) = primary_index else {
        return;
    };
    let (primary_offset, primary_end) = {
        let primary = &matches[primary_index];
        (primary.offset, primary.end)
    };
    for (index, candidate) in matches.iter_mut().enumerate() {
        if candidate.role == MatchRole::Alias {
            continue;
        }
        if index == primary_index {
            candidate.role = MatchRole::Primary;
        } else if candidate.redefines.is_some() {
            candidate.role = MatchRole::View;
        } else if candidate.kind == "group"
            || strictly_contains(candidate, primary_offset, primary_end)
        {
            candidate.role = MatchRole::Container;
        } else {
            candidate.role = MatchRole::View;
        }
    }
}

/// Sort key shared by ordering and primary selection: narrowest span,
/// deepest entry, lowest offset, then path.
fn span_key(candidate: &OwnershipMatch) -> (u32, std::cmp::Reverse<usize>, u32, &str) {
    (
        candidate.len,
        std::cmp::Reverse(path_depth(&candidate.path)),
        candidate.offset,
        candidate.path.as_str(),
    )
}

/// True when the candidate strictly encloses the primary range.
fn strictly_contains(candidate: &OwnershipMatch, offset: u32, end: u32) -> bool {
    candidate.offset <= offset
        && candidate.end >= end
        && (candidate.offset < offset || candidate.end > end)
}

/// Cap retained matches, reporting truncation.
fn truncate_matches(matches: &mut Vec<OwnershipMatch>) -> bool {
    if matches.len() > MAX_OWNERSHIP_MATCHES {
        matches.truncate(MAX_OWNERSHIP_MATCHES);
        return true;
    }
    false
}

/// Assemble the report with manifest identities. Machine output carries
/// fingerprints, never local paths.
fn report(
    manifest: &ResolvedManifest,
    query: OwnershipQuery,
    state: OwnershipState,
    matches: Vec<OwnershipMatch>,
) -> OwnershipReport {
    OwnershipReport {
        query,
        coordinate_system: COORDINATE_SYSTEM.to_owned(),
        manifest_fingerprint: manifest.fingerprint(),
        profile_fingerprint: manifest
            .inputs
            .profile
            .as_ref()
            .map(|profile| profile.fingerprint.clone()),
        schema_fingerprint: manifest.schema_fingerprint.clone(),
        record_len: manifest.record_len,
        state,
        matches,
        truncated: false,
    }
}

/// Exact full-path storage-field lookup, case-insensitive per projection.
/// Level-88 conditions and level-66 RENAMES entries live in `fields` but
/// own no storage; they fall through to their dedicated matchers so a path
/// query reports their relationships instead of a zero-width range.
fn exact_field<'a>(manifest: &'a ResolvedManifest, path: &str) -> Option<&'a ManifestField> {
    manifest
        .fields
        .iter()
        .find(|field| field.path.eq_ignore_ascii_case(path) && !is_non_storage(&field.kind))
}

/// Exact RENAMES alias lookup with its static range and members.
fn exact_alias(manifest: &ResolvedManifest, path: &str) -> Option<OwnershipMatch> {
    manifest
        .renames
        .iter()
        .find(|alias| alias.path.eq_ignore_ascii_case(path))
        .map(|alias| {
            let end = alias.offset.saturating_add(alias.length);
            OwnershipMatch {
                path: alias.path.clone(),
                role: MatchRole::Primary,
                offset: alias.offset,
                len: alias.length,
                end,
                kind: "renames".to_owned(),
                filler: false,
                occurrences: Vec::new(),
                repetition: None,
                redefines: None,
                numeric: None,
                odo: None,
                conditions: Vec::new(),
                members: alias.members.clone(),
            }
        })
}

/// Exact Level-88 condition lookup: conditions own no storage, so the
/// answer names the parent and sibling conditions truthfully.
fn exact_condition(manifest: &ResolvedManifest, path: &str) -> Option<OwnershipMatch> {
    let condition = manifest
        .fields
        .iter()
        .find(|field| field.kind == "condition" && field.path.eq_ignore_ascii_case(path))?;
    let parent = parent_path(&condition.path).unwrap_or_default();
    let siblings = manifest
        .fields
        .iter()
        .filter(|field| {
            field.kind == "condition"
                && parent_path(&field.path).as_deref() == parent_path(&condition.path).as_deref()
                && !field.path.eq_ignore_ascii_case(&condition.path)
        })
        .map(|field| field.path.clone())
        .collect();
    Some(OwnershipMatch {
        path: condition.path.clone(),
        role: MatchRole::Primary,
        offset: condition.offset,
        len: 0,
        end: condition.offset,
        kind: "condition".to_owned(),
        filler: false,
        occurrences: Vec::new(),
        repetition: None,
        redefines: None,
        numeric: None,
        odo: None,
        conditions: siblings,
        members: vec![parent],
    })
}

/// Short-name candidates across fields, aliases, and conditions.
fn short_name_candidates(manifest: &ResolvedManifest, path: &str) -> Vec<String> {
    let mut candidates = Vec::new();
    for field in &manifest.fields {
        if short_name(&field.path).eq_ignore_ascii_case(path) {
            candidates.push(field.path.clone());
        }
    }
    for alias in &manifest.renames {
        if short_name(&alias.path).eq_ignore_ascii_case(path)
            && !candidates.iter().any(|found| found == &alias.path)
        {
            candidates.push(alias.path.clone());
        }
    }
    candidates
}

/// Final dotted segment of a path.
fn short_name(path: &str) -> &str {
    path.rsplit('.').next().unwrap_or(path)
}

/// Parent dotted prefix, if any.
fn parent_path(path: &str) -> Option<String> {
    path.rfind('.').map(|dot| path[..dot].to_owned())
}

/// True for parser-renamed `FILLER` padding entries.
fn is_filler(path: &str) -> bool {
    path.split('.')
        .next_back()
        .is_some_and(|leaf| leaf.starts_with(FILLER_PREFIX) || leaf.eq_ignore_ascii_case("FILLER"))
}

/// Level-88 condition paths declared directly under `path`.
fn conditions_under(manifest: &ResolvedManifest, path: &str) -> Vec<String> {
    manifest
        .fields
        .iter()
        .filter(|field| {
            field.kind == "condition" && parent_path(&field.path).as_deref() == Some(path)
        })
        .map(|field| field.path.clone())
        .collect()
}

/// Numeric representation joined by exact path.
fn numeric_for(manifest: &ResolvedManifest, path: &str) -> Option<OwnershipNumeric> {
    manifest
        .numeric_details
        .iter()
        .find(|detail| detail.path == path)
        .map(|detail| OwnershipNumeric {
            digits: detail.digits,
            scale: detail.scale,
            signed: detail.signed,
            encoding: detail.encoding.clone(),
        })
}

/// ODO bounds joined by exact path.
fn odo_for(manifest: &ResolvedManifest, path: &str) -> Option<OdoInfo> {
    manifest
        .odo_details
        .iter()
        .find(|detail| detail.path == path)
        .map(|detail| OdoInfo {
            min_count: detail.min_count,
            max_count: detail.max_count,
            counter_path: detail.counter_path.clone(),
        })
}

/// Repetition accessors over the wire `occurs` union.
fn occurs_kind(occurs: &ManifestOccurs) -> String {
    occurs.kind.clone()
}

/// Repetition count: fixed count, or the ODO maximum.
fn occurs_count(occurs: &ManifestOccurs) -> u32 {
    occurs.count
}

/// Repetition minimum.
fn occurs_min(occurs: &ManifestOccurs) -> u32 {
    occurs.min_count
}

#[cfg(test)]
#[allow(clippy::expect_used)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::options::resolve::{OptionSource, Resolved};
    use crate::resolved_manifest::{GenerateInputs, MANIFEST_SOURCE_SPANS, ManifestTool};
    use copybook_core::dialect::Dialect;
    use copybook_core::layout::resolve_layout;
    use copybook_core::source_bundle::SourceBundle;
    use copybook_core::{Schema, parse_copybook};

    const LAYOUT_COPYBOOK: &str = concat!(
        "       01 REC.\n",
        "           05 NAME PIC X(10).\n",
        "           05 AMOUNT PIC 9(5).\n",
    );

    fn manifest_for(copybook: &str) -> ResolvedManifest {
        let bundle = SourceBundle::single("REC", copybook.as_bytes()).expect("bundle builds");
        let mut schema: Schema = parse_copybook(copybook).expect("copybook parses");
        resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
        ResolvedManifest::generate(GenerateInputs {
            bundle: &bundle,
            profile: None,
            tool: ManifestTool {
                name: "copybook-test".to_owned(),
                version: "0.0.0".to_owned(),
            },
            encoding: Resolved {
                value: "cp037".to_owned(),
                source: OptionSource::Default,
            },
            dialect: Resolved {
                value: Dialect::Normative,
                source: OptionSource::Default,
            },
            framing: Resolved {
                value: "fixed".to_owned(),
                source: OptionSource::Default,
            },
            record_bound: None,
            schema: &schema,
        })
        .expect("manifest generates")
    }

    fn primary(report: &OwnershipReport) -> &OwnershipMatch {
        report
            .matches
            .iter()
            .find(|item| item.role == MatchRole::Primary)
            .expect("one primary match")
    }

    #[test]
    fn byte_query_names_leaf_owner_and_containers() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let report = query_byte_owner(&manifest, 2);
        assert_eq!(report.state, OwnershipState::Owned);
        assert_eq!(report.coordinate_system, "payload-relative");
        assert!(!report.manifest_fingerprint.is_empty());
        assert!(report.profile_fingerprint.is_none());
        assert_eq!(report.record_len, 15);

        let owner = primary(&report);
        assert_eq!(owner.path, "REC.NAME");
        assert_eq!((owner.offset, owner.len, owner.end), (0, 10, 10));
        assert_eq!(owner.kind, "alphanum");
        assert!(!owner.filler);

        let containers: Vec<&str> = report
            .matches
            .iter()
            .filter(|item| item.role == MatchRole::Container)
            .map(|item| item.path.as_str())
            .collect();
        assert_eq!(containers, vec!["REC"]);
    }

    #[test]
    fn byte_query_at_record_extent_is_out_of_range() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let report = query_byte_owner(&manifest, 15);
        assert_eq!(report.state, OwnershipState::OutOfRange);
        assert!(report.matches.is_empty());
    }

    #[test]
    fn byte_query_in_filler_reports_padding() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 NAME PIC X(4).\n",
            "           05 FILLER PIC X(2).\n",
            "           05 AMOUNT PIC 9(2).\n",
        ));
        let report = query_byte_owner(&manifest, 5);
        assert_eq!(report.state, OwnershipState::Owned);
        let owner = primary(&report);
        assert!(owner.filler, "got {}", owner.path);
        assert_eq!((owner.offset, owner.len), (4, 2));
    }

    #[test]
    fn byte_query_reports_redefines_owner_and_views() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 PRIMARY PIC X(6).\n",
            "           05 SECONDARY REDEFINES PRIMARY PIC 9(6).\n",
        ));
        let report = query_byte_owner(&manifest, 2);
        let owner = primary(&report);
        assert_eq!(owner.path, "REC.PRIMARY");
        let views: Vec<&str> = report
            .matches
            .iter()
            .filter(|item| item.role == MatchRole::View)
            .map(|item| item.path.as_str())
            .collect();
        assert_eq!(views, vec!["REC.SECONDARY"]);
        let view = report
            .matches
            .iter()
            .find(|item| item.path == "REC.SECONDARY")
            .expect("view present");
        assert_eq!(view.redefines.as_deref(), Some("REC.PRIMARY"));
    }

    #[test]
    fn byte_query_in_fixed_occurs_names_true_child_and_index() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 ROW OCCURS 3 TIMES.\n",
            "               10 CELL-A PIC X(4).\n",
            "               10 CELL-B PIC 9(2).\n",
            "           05 TAIL PIC X.\n",
        ));
        // Byte 8 sits in occurrence 1, within-occurrence offset 2: CELL-A.
        let report = query_byte_owner(&manifest, 8);
        let owner = primary(&report);
        assert_eq!(owner.path, "REC.ROW.CELL-A");
        assert_eq!((owner.offset, owner.len), (6, 4));
        assert_eq!(
            owner.occurrences,
            vec![OccurrenceIndex {
                path: "REC.ROW".to_owned(),
                index: 1,
                presence: OccurrencePresence::Guaranteed,
            }]
        );
        // The repeating table itself is also reported with its index.
        let table = report
            .matches
            .iter()
            .find(|item| item.path == "REC.ROW")
            .expect("table present");
        assert_eq!(
            table.occurrences,
            vec![OccurrenceIndex {
                path: "REC.ROW".to_owned(),
                index: 1,
                presence: OccurrencePresence::Guaranteed,
            }]
        );
    }

    #[test]
    fn byte_query_in_odo_reports_possible_presence() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 HOWMANY PIC 9(2).\n",
            "           05 CELLS OCCURS 1 TO 3 TIMES DEPENDING ON HOWMANY PIC X(4).\n",
        ));
        // Occurrence 2 is at/past the minimum: statically possible.
        let report = query_byte_owner(&manifest, 10);
        let owner = primary(&report);
        assert_eq!(owner.path, "REC.CELLS");
        assert_eq!(
            owner.occurrences,
            vec![OccurrenceIndex {
                path: "REC.CELLS".to_owned(),
                index: 2,
                presence: OccurrencePresence::Possible,
            }]
        );
        assert!(owner.odo.is_some());
    }

    #[test]
    fn byte_query_never_returns_conditions() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 STATUS PIC X.\n",
            "               88 ACTIVE VALUE 'A'.\n",
        ));
        let report = query_byte_owner(&manifest, 0);
        assert!(
            report.matches.iter().all(|item| item.kind != "condition"),
            "conditions own no storage"
        );
        assert_eq!(primary(&report).path, "REC.STATUS");
    }

    #[test]
    fn byte_query_reports_renames_alias() {
        let manifest = manifest_for(concat!(
            "       01  REC.\n",
            "           05  NAME  PIC X(10).\n",
            "           05  AMOUNT  PIC 9(5).\n",
            "           66  NAME-AND-AMOUNT  RENAMES NAME THRU AMOUNT.\n",
        ));
        let report = query_byte_owner(&manifest, 12);
        let alias = report
            .matches
            .iter()
            .find(|item| item.role == MatchRole::Alias)
            .expect("alias present");
        assert_eq!(alias.path, "REC.NAME-AND-AMOUNT");
        assert!(!alias.members.is_empty());
    }

    #[test]
    fn field_query_returns_static_range_and_details() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let report = query_field_range(&manifest, "REC.AMOUNT").expect("path answers");
        assert_eq!(report.state, OwnershipState::Owned);
        let owner = primary(&report);
        assert_eq!((owner.offset, owner.len, owner.end), (10, 5, 15));
        assert!(owner.numeric.is_some());
    }

    #[test]
    fn field_query_matches_case_insensitively() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let report = query_field_range(&manifest, "rec.amount").expect("lowercase answers");
        assert_eq!(primary(&report).path, "REC.AMOUNT");
    }

    #[test]
    fn field_query_resolves_unique_short_name() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let report = query_field_range(&manifest, "AMOUNT").expect("short name answers");
        assert_eq!(primary(&report).path, "REC.AMOUNT");
    }

    #[test]
    fn field_query_fails_closed_on_ambiguity() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 LEFT.\n",
            "               10 CODE PIC X(2).\n",
            "           05 RIGHT.\n",
            "               10 CODE PIC X(2).\n",
        ));
        let error = query_field_range(&manifest, "CODE").expect_err("ambiguous fails");
        assert!(
            matches!(
                &error,
                OwnershipError::AmbiguousField { query, candidates }
                    if query == "CODE"
                        && candidates == &["REC.LEFT.CODE", "REC.RIGHT.CODE"]
            ),
            "wrong error: {error:?}"
        );
    }

    #[test]
    fn field_query_rejects_unknown_path() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        let error = query_field_range(&manifest, "REC.NOPE").expect_err("unknown fails");
        assert!(matches!(error, OwnershipError::UnknownField { .. }));
    }

    #[test]
    fn field_query_on_condition_names_parent_and_siblings() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 STATUS PIC X.\n",
            "               88 ACTIVE VALUE 'A'.\n",
            "               88 IDLE VALUE 'I'.\n",
        ));
        let report = query_field_range(&manifest, "REC.STATUS.ACTIVE").expect("88 answers");
        let owner = primary(&report);
        assert_eq!(owner.kind, "condition");
        assert_eq!(owner.len, 0);
        assert!(owner.members.contains(&"REC.STATUS".to_owned()));
        assert!(owner.conditions.contains(&"REC.STATUS.IDLE".to_owned()));
    }

    #[test]
    fn manifest_source_spans_state_is_preserved() {
        let manifest = manifest_for(LAYOUT_COPYBOOK);
        assert_eq!(manifest.source_spans, MANIFEST_SOURCE_SPANS);
    }
}
