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
    /// The path resolves in the schema but has no extent in this record:
    /// an ODO table (or its descendant) with zero actual occurrences.
    /// Record-specific queries only; static queries never report it.
    Absent,
}

/// Actual ODO presence for one table in a selected record, read off the
/// decoded record (array lengths), never re-derived from counters.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OdoCount {
    /// Fully-qualified ODO table path.
    pub table_path: String,
    /// Occurrences present in this record.
    pub actual: u32,
}

/// Record-specific context for an ownership query: the payload length and
/// the actual ODO presence the selected record decodes to.
///
/// Static queries answer over repetition bounds (ODO tails report
/// [`OccurrencePresence::Possible`]); record queries clamp ODO tables to
/// their actual counts, so later occurrences vanish and the table span
/// shrinks to what this record holds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordPresence {
    /// Actual ODO counts by table path.
    pub odo_counts: Vec<OdoCount>,
    /// Selected record payload length in bytes; overrides the manifest
    /// maximum as the query extent.
    pub record_len: u32,
}

impl RecordPresence {
    /// Actual occurrences for an ODO table path, when the context carries
    /// one.
    #[must_use]
    pub fn actual_for(&self, table_path: &str) -> Option<u32> {
        self.odo_counts
            .iter()
            .find(|count| count.table_path == table_path)
            .map(|count| count.actual)
    }

    /// Build record presence from a decoded record value: every manifest
    /// ODO table resolves to the length of the array the decoder produced
    /// for it, which is exactly what decode reports for this record.
    ///
    /// # Errors
    ///
    /// Returns [`OwnershipError::NestedOdoTable`] when an ODO table sits
    /// under a repeating ancestor (one flat count cannot name per-occurrence
    /// arrays), or [`OwnershipError::MissingOdoCount`] when the decoded
    /// value holds no array for a table.
    #[must_use = "Handle the Result or propagate the error"]
    #[inline]
    pub fn from_decoded(
        manifest: &ResolvedManifest,
        decoded: &serde_json::Value,
        record_len: u32,
    ) -> Result<Self, OwnershipError> {
        let mut odo_counts = Vec::new();
        for table in manifest.fields.iter().filter(|field| {
            field
                .occurs
                .as_ref()
                .is_some_and(|occurs| occurs.kind == "odo")
        }) {
            if has_repeating_ancestor(manifest, &table.path) {
                return Err(OwnershipError::NestedOdoTable {
                    table: table.path.clone(),
                });
            }
            let array = match walk_decoded(decoded, &table.path) {
                DecodedWalk::Found(value) => value.as_array(),
                DecodedWalk::Missing => None,
                DecodedWalk::NestedArray => {
                    return Err(OwnershipError::NestedOdoTable {
                        table: table.path.clone(),
                    });
                }
            };
            let Some(array) = array else {
                return Err(OwnershipError::MissingOdoCount {
                    table: table.path.clone(),
                });
            };
            let actual = u32::try_from(array.len()).unwrap_or(u32::MAX);
            odo_counts.push(OdoCount {
                table_path: table.path.clone(),
                actual,
            });
        }
        Ok(Self {
            odo_counts,
            record_len,
        })
    }
}

/// Identity of the selected record behind a record-specific answer.
///
/// The answer echoes which record it interpreted (1-based, matching the
/// `record_index` decode envelopes report) and the actual ODO counts it
/// clamped to, so machine output stays self-describing without local
/// paths. The payload length itself rides on
/// [`OwnershipReport::record_len`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordContext {
    /// 1-based selected record number within the input.
    pub index: u64,
    /// Actual ODO counts the answer clamped to.
    pub odo_counts: Vec<OdoCount>,
}

/// Outcome of walking a dotted path through a decoded record value.
enum DecodedWalk<'a> {
    /// The path resolves to a value.
    Found(&'a serde_json::Value),
    /// A segment is missing or not an object.
    Missing,
    /// An intermediate segment is an array: per-occurrence values a flat
    /// lookup cannot name.
    NestedArray,
}

/// Walk a dotted path through decoded output: full path first, then the
/// root-stripped short form, mirroring the decoder-side lookup.
fn walk_decoded<'a>(decoded: &'a serde_json::Value, path: &str) -> DecodedWalk<'a> {
    let root = decoded
        .as_object()
        .and_then(|object| object.get("fields"))
        .unwrap_or(decoded);
    walk_decoded_exact(root, path)
        .or_else(|| {
            path.split_once('.')
                .and_then(|(_, rest)| walk_decoded_exact(root, rest))
        })
        .unwrap_or(DecodedWalk::Missing)
}

/// Walk one dotted path without fallbacks.
fn walk_decoded_exact<'a>(root: &'a serde_json::Value, path: &str) -> Option<DecodedWalk<'a>> {
    let mut current = root;
    let mut segments = path.split('.').peekable();
    while let Some(segment) = segments.next() {
        let next = current.as_object().and_then(|object| object.get(segment))?;
        current = next;
        if current.is_array() && segments.peek().is_some() {
            return Some(DecodedWalk::NestedArray);
        }
    }
    Some(DecodedWalk::Found(current))
}

/// True when any strict ancestor of `path` repeats (fixed or ODO, any
/// bound): the decoder nests one array per ancestor occurrence, so no
/// single count can describe the table for a record.
fn has_repeating_ancestor(manifest: &ResolvedManifest, path: &str) -> bool {
    let mut ancestor = parent_path(path);
    while let Some(candidate) = ancestor {
        if manifest
            .fields
            .iter()
            .any(|field| field.path == candidate && field.occurs.is_some())
        {
            return true;
        }
        ancestor = parent_path(&candidate);
    }
    false
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
    /// Query extent in bytes: the manifest maximum for static answers, the
    /// selected record's payload length for record-specific answers.
    pub record_len: u32,
    /// Selected record identity, present only for record-specific answers.
    /// Static answers omit it, so their machine output is unchanged.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub record: Option<RecordContext>,
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
    /// A record-specific query names an ODO table the record context
    /// carries no usable count for. Counts come from the decoded record,
    /// never from guessing.
    #[error("no record count for ODO table: {table}")]
    MissingOdoCount {
        /// Fully-qualified ODO table path.
        table: String,
    },
    /// A record-specific query names an ODO table under a repeating
    /// ancestor: each ancestor occurrence holds its own array, so one flat
    /// count cannot describe it without per-occurrence selection.
    #[error("nested ODO table needs per-occurrence selection: {table}")]
    NestedOdoTable {
        /// Fully-qualified ODO table path.
        table: String,
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
    query_byte_owner_inner(manifest, byte, None)
}

/// Answer which field owns payload-relative byte `byte` in a selected
/// record: ODO tables clamp to their actual counts, so absent occurrences
/// vanish and the query extent is the record length, not the manifest
/// maximum.
///
/// # Errors
///
/// Returns [`OwnershipError::MissingOdoCount`] when the record context
/// carries no count for a manifest ODO table.
#[must_use = "Handle the Result or propagate the error"]
#[inline]
pub fn query_byte_owner_in_record(
    manifest: &ResolvedManifest,
    byte: u32,
    record: &RecordPresence,
) -> Result<OwnershipReport, OwnershipError> {
    require_odo_counts(manifest, record)?;
    Ok(query_byte_owner_inner(manifest, byte, Some(record)))
}

fn query_byte_owner_inner(
    manifest: &ResolvedManifest,
    byte: u32,
    record: Option<&RecordPresence>,
) -> OwnershipReport {
    let extent = record.map_or(manifest.record_len, |presence| presence.record_len);
    let state = if byte >= extent {
        OwnershipState::OutOfRange
    } else {
        OwnershipState::Owned
    };
    let mut matches = if state == OwnershipState::Owned {
        collect_byte_matches(manifest, byte, record)
    } else {
        Vec::new()
    };
    if matches.is_empty() && state == OwnershipState::Owned {
        return report(
            manifest,
            OwnershipQuery::PayloadByte { byte },
            OwnershipState::Gap,
            Vec::new(),
            record,
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
        record,
    );
    answered.truncated = truncated;
    answered
}

/// Fail closed when a record context cannot name every manifest ODO
/// table: answering without a count would silently fall back to static
/// bounds, and a nested table's per-occurrence arrays need selection no
/// flat count can name.
fn require_odo_counts(
    manifest: &ResolvedManifest,
    record: &RecordPresence,
) -> Result<(), OwnershipError> {
    for table in manifest.fields.iter().filter(|field| {
        field
            .occurs
            .as_ref()
            .is_some_and(|occurs| occurs.kind == "odo")
    }) {
        if has_repeating_ancestor(manifest, &table.path) {
            return Err(OwnershipError::NestedOdoTable {
                table: table.path.clone(),
            });
        }
        if record.actual_for(&table.path).is_none() {
            return Err(OwnershipError::MissingOdoCount {
                table: table.path.clone(),
            });
        }
    }
    Ok(())
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
#[inline]
#[must_use = "Handle the Result or propagate the error"]
pub fn query_field_range(
    manifest: &ResolvedManifest,
    path: &str,
) -> Result<OwnershipReport, OwnershipError> {
    let query = OwnershipQuery::FieldPath {
        path: path.to_owned(),
    };
    if let Some(field) = exact_field(manifest, path) {
        let field_match = field_match(manifest, field, None);
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![field_match],
            None,
        ));
    }
    if let Some(alias_match) = exact_alias(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![alias_match],
            None,
        ));
    }
    if let Some(condition_match) = exact_condition(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![condition_match],
            None,
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

/// Answer which byte range backs field path `path` in a selected record:
/// ODO tables shrink to their actual counts, and a path whose schema
/// entry has no extent in this record reports [`OwnershipState::Absent`]
/// instead of a range. Alias and condition answers stay schema-level:
/// they name relationships, and storage presence is answered by byte
/// queries.
///
/// # Errors
///
/// Returns [`OwnershipError::UnknownField`], [`OwnershipError::AmbiguousField`],
/// [`OwnershipError::MissingOdoCount`], or [`OwnershipError::NestedOdoTable`]
/// without guessing.
#[must_use = "Handle the Result or propagate the error"]
#[inline]
pub fn query_field_range_in_record(
    manifest: &ResolvedManifest,
    path: &str,
    record: &RecordPresence,
) -> Result<OwnershipReport, OwnershipError> {
    require_odo_counts(manifest, record)?;
    let query = OwnershipQuery::FieldPath {
        path: path.to_owned(),
    };
    if let Some(field) = exact_field(manifest, path) {
        if odo_absent_in_record(manifest, field, record) {
            return Ok(report(
                manifest,
                query,
                OwnershipState::Absent,
                Vec::new(),
                Some(record),
            ));
        }
        let field_match = field_match(manifest, field, Some(record));
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![field_match],
            Some(record),
        ));
    }
    if let Some(alias_match) = exact_alias(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![alias_match],
            Some(record),
        ));
    }
    if let Some(condition_match) = exact_condition(manifest, path) {
        return Ok(report(
            manifest,
            query,
            OwnershipState::Owned,
            vec![condition_match],
            Some(record),
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
            query_field_range_in_record(manifest, &found, record)
        }
        _ => Err(OwnershipError::AmbiguousField {
            query: path.to_owned(),
            candidates,
        }),
    }
}

/// True when a schema entry has no extent in this record: it is an ODO
/// table with zero actual occurrences, or descends from one.
fn odo_absent_in_record(
    manifest: &ResolvedManifest,
    field: &ManifestField,
    record: &RecordPresence,
) -> bool {
    let mut current: Option<&ManifestField> = Some(field);
    while let Some(entry) = current {
        if entry
            .occurs
            .as_ref()
            .is_some_and(|occurs| occurs.kind == "odo")
            && record
                .actual_for(&entry.path)
                .is_some_and(|actual| actual == 0)
        {
            return true;
        }
        current = parent_path(&entry.path).and_then(|parent| {
            manifest
                .fields
                .iter()
                .find(|candidate| candidate.path == parent)
        });
    }
    false
}

/// Effective covered span of one manifest field: single-occurrence length
/// for scalars, stride times repetition bound for tables. A record context
/// clamps ODO tables to their actual counts, shrinking the span to what
/// the selected record holds.
fn effective_span(field: &ManifestField, record: Option<&RecordPresence>) -> (u32, u32) {
    let repetitions = repetitions_for(field, record);
    let len = field.len.saturating_mul(repetitions);
    (field.offset, field.offset.saturating_add(len))
}

/// Repetition bound for span and descent arithmetic: the static bound,
/// except ODO tables under a record context resolve to their actual
/// counts (clamped to the static bound, never beyond it). Missing counts
/// fall back to the static bound; [`require_odo_counts`] keeps that
/// fallback unreachable for record queries by failing closed first.
fn repetitions_for(field: &ManifestField, record: Option<&RecordPresence>) -> u32 {
    let bound = field
        .occurs
        .as_ref()
        .map_or(1, |occurs| occurs.count.max(1));
    match (record, field.occurs.as_ref()) {
        (Some(presence), Some(occurs)) if occurs.kind == "odo" => {
            match presence.actual_for(&field.path) {
                Some(actual) => bound.min(actual),
                None => bound,
            }
        }
        _ => bound,
    }
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
fn collect_byte_matches(
    manifest: &ResolvedManifest,
    byte: u32,
    record: Option<&RecordPresence>,
) -> Vec<OwnershipMatch> {
    let mut matches = Vec::new();
    for field in &manifest.fields {
        if parent_path(&field.path)
            .is_some_and(|parent| manifest.fields.iter().any(|other| other.path == parent))
        {
            continue;
        }
        descend(
            manifest,
            field,
            field.offset,
            byte,
            &[],
            record,
            &mut matches,
        );
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
    record: Option<&RecordPresence>,
    matches: &mut Vec<OwnershipMatch>,
) {
    if is_non_storage(&field.kind) || field.len == 0 {
        return;
    }
    let repetitions = repetitions_for(field, record);
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
            presence: match record {
                // A record context resolves every surviving occurrence:
                // only actual occurrences are projected, so each one is
                // guaranteed for this record.
                Some(_) => OccurrencePresence::Guaranteed,
                None => {
                    if index < minimum {
                        OccurrencePresence::Guaranteed
                    } else {
                        OccurrencePresence::Possible
                    }
                }
            },
        });
        let occurrence_base = base.saturating_add(index.saturating_mul(stride));
        matches.push(field_match_at(manifest, field, base, end, entered.clone()));
        for child in direct_children(manifest, &field.path) {
            let child_base =
                occurrence_base.saturating_add(child.offset.saturating_sub(field.offset));
            descend(manifest, child, child_base, byte, &entered, record, matches);
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
    // Nonrepeating entries still shift under a projected ancestor: children
    // stay relative to the working base, exactly as in the repeating branch.
    for child in direct_children(manifest, &field.path) {
        let child_base = base.saturating_add(child.offset.saturating_sub(field.offset));
        descend(
            manifest, child, child_base, byte, inherited, record, matches,
        );
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

/// The REDEFINES owner this entry views, if any: its own clause, else the
/// nearest redefining ancestor's. Children of a redefining group are views
/// of that storage, so they must never pass the storage filter in primary
/// selection even though they carry no clause themselves.
fn redefines_view(manifest: &ResolvedManifest, field: &ManifestField) -> Option<String> {
    if field.redefines.is_some() {
        return field.redefines.clone();
    }
    let mut ancestor = parent_path(&field.path);
    while let Some(path) = ancestor {
        if let Some(entry) = manifest
            .fields
            .iter()
            .find(|candidate| candidate.path == path)
            && entry.redefines.is_some()
        {
            return entry.redefines.clone();
        }
        ancestor = parent_path(&path);
    }
    None
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
        redefines: redefines_view(manifest, field),
        numeric: numeric_for(manifest, &field.path),
        odo: odo_for(manifest, &field.path),
        conditions: conditions_under(manifest, &field.path),
        members: Vec::new(),
    }
}

/// Build one match for a field-path query: the entry's own range with no
/// occurrence context (static bound, or the record-resolved span).
fn field_match(
    manifest: &ResolvedManifest,
    field: &ManifestField,
    record: Option<&RecordPresence>,
) -> OwnershipMatch {
    let (start, end) = effective_span(field, record);
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
/// fingerprints, never local paths. A record context overrides the
/// extent with the selected record length.
fn report(
    manifest: &ResolvedManifest,
    query: OwnershipQuery,
    state: OwnershipState,
    matches: Vec<OwnershipMatch>,
    record: Option<&RecordPresence>,
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
        record_len: record.map_or(manifest.record_len, |presence| presence.record_len),
        record: None,
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
    fn byte_query_in_later_occurrence_projects_through_plain_groups() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 TBL OCCURS 3 TIMES.\n",
            "               10 GRP.\n",
            "                   15 ITEM PIC X(2).\n",
            "           05 TAIL PIC X.\n",
        ));
        // Byte 3 sits in occurrence 1 of TBL, inside GRP.ITEM: the plain
        // group must carry the projected base to its children, not reset
        // them to first-occurrence offsets.
        let report = query_byte_owner(&manifest, 3);
        let owner = primary(&report);
        assert_eq!(owner.path, "REC.TBL.GRP.ITEM");
        assert_eq!((owner.offset, owner.len), (2, 2));
        assert_eq!(
            owner.occurrences,
            vec![OccurrenceIndex {
                path: "REC.TBL".to_owned(),
                index: 1,
                presence: OccurrencePresence::Guaranteed,
            }]
        );
    }

    #[test]
    fn byte_query_prefers_storage_over_redefines_group_children() {
        let manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 STORAGE.\n",
            "               10 A PIC X(4).\n",
            "           05 VIEW-GRP REDEFINES STORAGE.\n",
            "               10 B1 PIC X(1).\n",
            "               10 B2 PIC X(3).\n",
        ));
        // Byte 0 is covered by A and by the narrower B1. B1 is part of a
        // redefining view, so storage A stays primary and B1 renders a view.
        let report = query_byte_owner(&manifest, 0);
        assert_eq!(primary(&report).path, "REC.STORAGE.A");
        let child = report
            .matches
            .iter()
            .find(|item| item.path == "REC.VIEW-GRP.B1")
            .expect("redefining child present");
        assert_eq!(child.role, MatchRole::View);
        assert_eq!(child.redefines.as_deref(), Some("REC.STORAGE"));
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

    const ODO_GROUP_COPYBOOK: &str = concat!(
        "       01 REC.\n",
        "           05 HOWMANY PIC 9(2).\n",
        "           05 TAIL PIC X.\n",
        "           05 TBL OCCURS 1 TO 2 TIMES DEPENDING ON HOWMANY.\n",
        "               10 CELL PIC X(4).\n",
    );

    fn record_presence(counts: &[(&str, u32)], record_len: u32) -> RecordPresence {
        RecordPresence {
            odo_counts: counts
                .iter()
                .map(|(table, actual)| OdoCount {
                    table_path: (*table).to_owned(),
                    actual: *actual,
                })
                .collect(),
            record_len,
        }
    }

    /// Decode record bytes with the fixture encoding so `from_decoded`
    /// tests prove the true decoded shape, not a hand-built guess.
    fn decode_odo_record(copybook: &str, bytes: &[u8]) -> (ResolvedManifest, serde_json::Value) {
        use crate::{Codepage, DecodeOptions, RecordFormat, decode_record};

        let manifest = manifest_for(copybook);
        let mut schema: Schema = parse_copybook(copybook).expect("copybook parses");
        resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
        let options = DecodeOptions::new()
            .with_codepage(Codepage::CP037)
            .with_format(RecordFormat::Fixed);
        let decoded = decode_record(&schema, bytes, &options).expect("record decodes");
        (manifest, decoded)
    }

    #[test]
    fn record_query_resolves_present_odo_occurrence() {
        let manifest = manifest_for(ODO_GROUP_COPYBOOK);
        // HOWMANY=2: TBL holds occurrences 0..2 (bytes 3..11).
        let record = record_presence(&[("REC.TBL", 2)], 11);
        let report = query_byte_owner_in_record(&manifest, 7, &record).expect("byte answers");
        assert_eq!(report.record_len, 11);
        let owner = primary(&report);
        assert_eq!(owner.path, "REC.TBL.CELL");
        assert_eq!((owner.offset, owner.len), (7, 4));
        assert_eq!(
            owner.occurrences,
            vec![OccurrenceIndex {
                path: "REC.TBL".to_owned(),
                index: 1,
                presence: OccurrencePresence::Guaranteed,
            }]
        );
    }

    #[test]
    fn record_query_absent_odo_extent_falls_back_to_group() {
        let manifest = manifest_for(ODO_GROUP_COPYBOOK);
        // HOWMANY=1: byte 8 sits where occurrence 1 would start, but only
        // occurrence 0 exists; TBL contributes no match there while the
        // record group still owns the byte.
        let record = record_presence(&[("REC.TBL", 1)], 11);
        let report = query_byte_owner_in_record(&manifest, 8, &record).expect("byte answers");
        assert_eq!(report.state, OwnershipState::Owned);
        assert_eq!(primary(&report).path, "REC");
        assert!(
            report
                .matches
                .iter()
                .all(|item| item.path != "REC.TBL.CELL"),
            "absent occurrence names no leaf"
        );
    }

    #[test]
    fn record_query_zero_odo_field_reports_absent() {
        let manifest = manifest_for(ODO_GROUP_COPYBOOK);
        let record = record_presence(&[("REC.TBL", 0)], 3);
        let report =
            query_field_range_in_record(&manifest, "REC.TBL.CELL", &record).expect("path answers");
        assert_eq!(report.state, OwnershipState::Absent);
        assert!(report.matches.is_empty());
        let table =
            query_field_range_in_record(&manifest, "REC.TBL", &record).expect("table answers");
        assert_eq!(table.state, OwnershipState::Absent);
    }

    #[test]
    fn record_query_missing_count_fails_closed() {
        let manifest = manifest_for(ODO_GROUP_COPYBOOK);
        let record = record_presence(&[], 11);
        let error = query_byte_owner_in_record(&manifest, 2, &record).expect_err("count missing");
        assert!(
            matches!(error, OwnershipError::MissingOdoCount { .. }),
            "got {error:?}"
        );
    }

    #[test]
    fn record_query_nested_odo_fails_closed() {
        // The parser refuses nested ODO, so this hostile shape is built by
        // hand: an ODO table under a fixed table holds one array per outer
        // occurrence, which no flat count can name.
        let mut manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 OUTER OCCURS 2 TIMES.\n",
            "               10 INNER PIC X(2).\n",
            "           05 HOWMANY PIC 9(2).\n",
        ));
        let inner = manifest
            .fields
            .iter_mut()
            .find(|field| field.path == "REC.OUTER.INNER")
            .expect("inner present");
        inner.occurs = Some(ManifestOccurs {
            kind: "odo".to_owned(),
            count: 2,
            min_count: 1,
            counter_path: Some("REC.HOWMANY".to_owned()),
        });
        let record = record_presence(&[("REC.OUTER.INNER", 1)], 8);
        let error = query_byte_owner_in_record(&manifest, 4, &record).expect_err("nested refused");
        assert!(
            matches!(error, OwnershipError::NestedOdoTable { .. }),
            "got {error:?}"
        );
    }

    #[test]
    fn record_presence_from_decoded_refuses_nested_arrays() {
        let mut manifest = manifest_for(concat!(
            "       01 REC.\n",
            "           05 OUTER OCCURS 2 TIMES.\n",
            "               10 INNER PIC X(2).\n",
            "           05 HOWMANY PIC 9(2).\n",
        ));
        let inner = manifest
            .fields
            .iter_mut()
            .find(|field| field.path == "REC.OUTER.INNER")
            .expect("inner present");
        inner.occurs = Some(ManifestOccurs {
            kind: "odo".to_owned(),
            count: 2,
            min_count: 1,
            counter_path: Some("REC.HOWMANY".to_owned()),
        });
        let decoded = serde_json::json!({
            "REC": {
                "OUTER": [{"INNER": ["a"]}, {"INNER": ["b"]}],
                "HOWMANY": 1,
            }
        });
        let error =
            RecordPresence::from_decoded(&manifest, &decoded, 8).expect_err("nested refused");
        assert!(
            matches!(error, OwnershipError::NestedOdoTable { .. }),
            "got {error:?}"
        );
    }

    #[test]
    fn record_presence_from_decoded_reads_array_lengths() {
        // EBCDIC: HOWMANY "02", TAIL "Z", two CELLs.
        let bytes: Vec<u8> = [
            &[0xF0, 0xF2][..],
            &[0xE9][..],
            &[0x81, 0x81, 0x81, 0x81][..],
            &[0x82, 0x82, 0x82, 0x82][..],
        ]
        .concat();
        let (manifest, decoded) = decode_odo_record(ODO_GROUP_COPYBOOK, &bytes);
        let record = RecordPresence::from_decoded(
            &manifest,
            &decoded,
            u32::try_from(bytes.len()).expect("len fits"),
        )
        .expect("counts resolve");
        assert_eq!(
            record.odo_counts,
            vec![OdoCount {
                table_path: "REC.TBL".to_owned(),
                actual: 2,
            }]
        );
        let report = query_byte_owner_in_record(&manifest, 7, &record).expect("byte answers");
        assert_eq!(primary(&report).path, "REC.TBL.CELL");
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
