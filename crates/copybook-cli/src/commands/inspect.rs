// SPDX-License-Identifier: AGPL-3.0-or-later
//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::profile_inputs::ResolvedCommonInputs;
use crate::utils::{
    InputRole, atomic_write, atomic_write_new, print_identity_hint, read_input_or_stdin,
};
use crate::write_stdout_all;
use anyhow::Context as _;
use copybook::codec::Codepage;
use copybook::codec::options::profile::InterpretationProfile;
use copybook::codec::options::resolve::Resolved;
use copybook::codec::ownership::{
    MatchRole, OwnershipError, OwnershipReport, OwnershipState, RecordContext, RecordPresence,
    query_byte_owner, query_byte_owner_in_record, query_field_range, query_field_range_in_record,
};
use copybook::codec::resolved_manifest::{GenerateInputs, ManifestTool, ResolvedManifest};
use copybook::core::source_bundle::SourceBundle;
use copybook::core::{
    FeatureFlags, Field, FieldKind, Occurs, ParseOptions, Schema, parse_copybook_with_feature_flags,
};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use tracing::info;

/// Minimum widths for the layout table columns.
const MIN_PATH_WIDTH: usize = 32;
const MIN_TYPE_WIDTH: usize = 12;
const OFFSET_WIDTH: usize = 8;
const LENGTH_WIDTH: usize = 8;

/// Header label for the optional trailing clause column.
const DETAILS_HEADER: &str = "Details";

pub fn run(
    copybook: &PathBuf,
    codepage: Codepage,
    strict: bool,
    strict_comments: bool,
    dialect: crate::DialectPreference,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<ExitCode> {
    info!("Inspecting copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_input_or_stdin(InputRole::Copybook, copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict_comments,
        strict,
        codepage: codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: dialect.into(),
    };
    // #656 Phase D: CLI-resolved flags passed explicitly; no global state.
    let schema = parse_schema(&copybook_text, &options, feature_flags)?;

    let rows: Vec<Row> = schema
        .all_fields()
        .into_iter()
        .map(Row::from_field)
        .collect();
    let output = render_layout(codepage, schema.lrecl_fixed, &rows);

    write_stdout_all(output.as_bytes())?;

    info!("Inspect completed successfully");
    Ok(ExitCode::Ok)
}

/// Inspect with manifest emission: the layout report goes to stdout and a
/// Query output rendering.
#[derive(clap::ValueEnum, Debug, Clone, Copy, PartialEq, Eq)]
pub enum InspectQueryFormat {
    /// Human-readable ownership answer.
    Human,
    /// Machine-readable ownership answer (fingerprints, no local paths).
    Json,
}

/// Ownership-query inputs for `inspect`: framing, document, selector, and
/// rendering.
///
/// A flattened group rather than inline variant fields, so the top-level
/// command builder stays small: the full CLI tree already sits close to the
/// Windows main-thread stack bound in debug builds, and inline fields were
/// enough to overflow it there.
#[derive(clap::Args, Debug)]
pub struct InspectQueryArgs {
    /// Record format (explicit, no auto-detection). Supplies framing for
    /// source-backed ownership queries unless --profile does; the legacy
    /// layout report and --emit-manifest need no flag.
    #[arg(long)]
    pub format: Option<copybook::codec::RecordFormat>,
    /// Answer an ownership query from a pre-generated manifest document.
    /// Reads no copybook and no record data; conflicts with COPYBOOK
    /// and --profile, which the manifest already binds.
    #[arg(long, value_name = "FILE")]
    pub manifest: Option<PathBuf>,
    /// Payload-relative byte to own. Exactly one of --payload-byte
    /// and --field starts query mode.
    #[arg(long, value_name = "N")]
    pub payload_byte: Option<u32>,
    /// Field path to locate (full dotted path or a unique short name,
    /// case-insensitive). Exactly one of --payload-byte and --field
    /// starts query mode.
    #[arg(long, value_name = "PATH")]
    pub field: Option<String>,
    /// Query rendering: human or json (default: human).
    #[arg(long, value_enum, default_value = "human")]
    pub output: InspectQueryFormat,
    /// Record data file answering a record-specific query. Reads no record
    /// data without `--record`; conflicts with `--manifest`, which binds no
    /// decode schema. Record queries answer inside one decoded record:
    /// ODO tables clamp to that record's actual counts.
    #[arg(long, value_name = "FILE")]
    pub input: Option<PathBuf>,
    /// 1-based record number within `--input` to answer inside, matching
    /// the `record_index` decode envelopes report. Requires `--input` and
    /// a query selector; without record selection queries answer over
    /// static repetition bounds.
    #[arg(long, value_name = "N")]
    pub record: Option<u64>,
}

/// Build the resolved manifest for a copybook without writing it anywhere.
///
/// Emission and source-backed queries share this constructor, so a query
/// over `COPYBOOK` answers exactly what the emitted manifest contains.
/// Returns the manifest with the schema it was generated from for callers
/// that also render the legacy layout table.
pub fn build_manifest(
    copybook: &Path,
    common: &ResolvedCommonInputs,
    profile: Option<&InterpretationProfile>,
    strict: bool,
    strict_comments: bool,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<(ResolvedManifest, Schema)> {
    // The parse path already requires valid UTF-8 source, so the text bytes
    // are exactly the file bytes the bundle fingerprints.
    let copybook_text = read_input_or_stdin(InputRole::Copybook, copybook)?;
    let options = ParseOptions {
        strict_comments,
        strict,
        codepage: common.codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: common.dialect,
    };
    let schema = parse_schema(&copybook_text, &options, feature_flags)?;

    let logical_id = copybook
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("copybook");
    let bundle = SourceBundle::single(logical_id, copybook_text.as_bytes())
        .map_err(|error| anyhow::anyhow!("cannot build source bundle for {logical_id}: {error}"))?;
    ResolvedManifest::generate(GenerateInputs {
        bundle: &bundle,
        profile,
        tool: ManifestTool {
            name: "copybook".to_owned(),
            version: env!("CARGO_PKG_VERSION").to_owned(),
        },
        encoding: Resolved {
            value: common.codepage.to_string(),
            source: common.codepage_source,
        },
        dialect: Resolved {
            value: common.dialect,
            source: common.dialect_source,
        },
        framing: Resolved {
            value: common.format.to_string(),
            source: common.format_source,
        },
        record_bound: common.record_bound.clone(),
        schema: &schema,
    })
    .map_err(|error| anyhow::anyhow!("cannot generate resolved manifest: {error}"))
    .map(|manifest| (manifest, schema))
}

/// Where a resolved manifest is written when `inspect --emit-manifest` runs.
pub struct ManifestEmission<'a> {
    /// Destination document path for the emitted manifest.
    pub manifest_path: &'a PathBuf,
    /// Replace the destination when it already exists.
    pub overwrite: bool,
}

/// Emit the resolved-schema manifest binding the reviewed inputs.
///
/// The manifest records the exact [`ResolvedCommonInputs`] the run resolved,
/// so the file reproduces the run's interpretation without re-resolution.
///
/// Publication is atomic and, unless overwrite is set, no-clobber: a target
/// that appears after dispatch's pre-check still cannot be replaced.
pub fn run_with_manifest(
    copybook: &PathBuf,
    common: &ResolvedCommonInputs,
    profile: Option<&InterpretationProfile>,
    strict: bool,
    strict_comments: bool,
    feature_flags: &FeatureFlags,
    emission: &ManifestEmission<'_>,
) -> anyhow::Result<ExitCode> {
    info!("Inspecting copybook with manifest emission: {copybook:?}");

    let (manifest, schema) = build_manifest(
        copybook,
        common,
        profile,
        strict,
        strict_comments,
        feature_flags,
    )?;
    let json = manifest
        .to_json()
        .map_err(|error| anyhow::anyhow!("cannot serialize resolved manifest: {error}"))?;
    let write = |writer: &mut dyn std::io::Write| writer.write_all(&json);
    if emission.overwrite {
        atomic_write(emission.manifest_path, write)
    } else {
        atomic_write_new(emission.manifest_path, write)
    }
    .map_err(|error| {
        anyhow::anyhow!(
            "cannot write manifest {}: {error}",
            emission.manifest_path.display()
        )
    })?;

    let rows: Vec<Row> = schema
        .all_fields()
        .into_iter()
        .map(Row::from_field)
        .collect();
    let output = render_layout(common.codepage, schema.lrecl_fixed, &rows);
    write_stdout_all(output.as_bytes())?;

    info!("Inspect with manifest emission completed successfully");
    Ok(ExitCode::Ok)
}

/// A query the dispatcher refuses without guessing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QueryRefusal {
    /// No field, alias, or condition matches the requested path.
    UnknownField {
        /// Requested path as given.
        query: String,
    },
    /// A short name matches several entries; the caller must qualify it.
    AmbiguousField {
        /// Requested path as given.
        query: String,
        /// Fully-qualified candidates in manifest order.
        candidates: Vec<String>,
    },
    /// A record-specific query names an ODO table the selected record
    /// carries no usable count for. Counts come from the decoded record,
    /// never from guessing.
    MissingRecordCount {
        /// Fully-qualified ODO table path.
        table: String,
    },
    /// A record-specific query names an ODO table under a repeating
    /// ancestor: each ancestor occurrence holds its own array, so one flat
    /// count cannot describe it without per-occurrence selection.
    NestedOdoTable {
        /// Fully-qualified ODO table path.
        table: String,
    },
}

/// One validated ownership selector. Dispatch guarantees exactly one side
/// before calling [`answer_query`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OwnershipSelector {
    /// Own a payload-relative byte.
    PayloadByte(u32),
    /// Locate a field path.
    FieldPath(String),
}

/// Answer one ownership query against an already-resolved manifest.
///
/// Returns the typed report for the dispatcher to render, or the refusal
/// for structured diagnostics: unknown and ambiguous paths are refused
/// rather than guessed. Answered states — including `gap` and
/// `out_of_range` — render with an explicit state.
pub fn answer_query(
    manifest: &ResolvedManifest,
    selector: OwnershipSelector,
) -> Result<OwnershipReport, QueryRefusal> {
    let report = match selector {
        OwnershipSelector::PayloadByte(byte) => Ok(query_byte_owner(manifest, byte)),
        OwnershipSelector::FieldPath(path) => query_field_range(manifest, &path),
    };
    report.map_err(map_ownership_error)
}

/// Answer one ownership query inside a selected record: ODO tables clamp
/// to the record's actual counts, so later occurrences vanish and the
/// query extent is the record length, not the manifest maximum.
///
/// Static selectors behave as in [`answer_query`]; record-only refusals
/// ([`QueryRefusal::MissingRecordCount`], [`QueryRefusal::NestedOdoTable`])
/// fail closed instead of falling back to static bounds. The answer echoes
/// the 1-based `record_index` it interpreted with the counts it clamped
/// to, so machine output stays self-describing.
pub fn answer_query_in_record(
    manifest: &ResolvedManifest,
    selector: OwnershipSelector,
    record: &RecordPresence,
    record_index: u64,
) -> Result<OwnershipReport, QueryRefusal> {
    let report = match selector {
        OwnershipSelector::PayloadByte(byte) => query_byte_owner_in_record(manifest, byte, record),
        OwnershipSelector::FieldPath(path) => query_field_range_in_record(manifest, &path, record),
    };
    report
        .map(|mut answered| {
            answered.record = Some(RecordContext {
                index: record_index,
                odo_counts: record.odo_counts.clone(),
            });
            answered
        })
        .map_err(map_ownership_error)
}

/// One decoded record selected for a record-specific query.
pub struct SelectedRecord {
    /// 1-based record number within the input, as answered.
    pub index: u64,
    /// Actual ODO presence the selected record decodes to.
    pub presence: RecordPresence,
}

/// Why a record selection cannot be answered.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordSelectionFailure {
    /// The input file cannot be opened or streamed.
    Unreadable {
        /// What went wrong, naming the input.
        detail: String,
    },
    /// The input holds fewer records than requested.
    NoSuchRecord {
        /// Requested 1-based record number.
        index: u64,
        /// Records the input holds.
        available: u64,
    },
    /// The selected record cannot be decoded.
    Undecodable {
        /// Requested 1-based record number.
        index: u64,
        /// Decoder's refusal.
        detail: String,
    },
    /// The decoded record carries no usable ODO count.
    Refused(QueryRefusal),
}

/// Select the `--record`th record from `--input` and read its ODO presence
/// off the decoded value: every manifest ODO table resolves to the length
/// of the array the decoder produced for it.
///
/// Records frame exactly as decode frames them (fixed strides by the
/// schema length, RDW by headers); the index is 1-based to match the
/// `record_index` decode envelopes report. Selection fails closed:
/// unreadable inputs, short inputs, undecodable records, and unusable
/// counts never fall back to static bounds.
pub fn select_record(
    manifest: &ResolvedManifest,
    schema: &Schema,
    format: copybook::codec::RecordFormat,
    codepage: Codepage,
    input: &Path,
    index: u64,
) -> Result<SelectedRecord, RecordSelectionFailure> {
    use RecordSelectionFailure as Failure;
    let file = std::fs::File::open(input).map_err(|error| Failure::Unreadable {
        detail: format!("cannot read input {}: {error}", input.display()),
    })?;
    let options = copybook::codec::DecodeOptions::new()
        .with_format(format)
        .with_codepage(codepage);
    let mut iterator =
        copybook::codec::RecordIterator::new(file, schema, &options).map_err(|error| {
            Failure::Unreadable {
                detail: format!("cannot frame {} as {format}: {error}", input.display()),
            }
        })?;
    let mut available: u64 = 0;
    loop {
        match iterator
            .read_raw_record()
            .map_err(|error| Failure::Unreadable {
                detail: format!("cannot read input {}: {error}", input.display()),
            })? {
            None => {
                return Err(Failure::NoSuchRecord { index, available });
            }
            Some(payload) => {
                available = available.saturating_add(1);
                if available < index {
                    continue;
                }
                let record_len =
                    u32::try_from(payload.len()).map_err(|_| Failure::Undecodable {
                        index,
                        detail: format!(
                            "record payload ({} bytes) exceeds the query extent",
                            payload.len()
                        ),
                    })?;
                let decoded = copybook::codec::decode_record(schema, &payload, &options).map_err(
                    |error| Failure::Undecodable {
                        index,
                        detail: error.to_string(),
                    },
                )?;
                let presence = RecordPresence::from_decoded(manifest, &decoded, record_len)
                    .map_err(|error| Failure::Refused(map_ownership_error(error)))?;
                return Ok(SelectedRecord { index, presence });
            }
        }
    }
}

/// Map every ownership refusal to its dispatcher refusal without guessing:
/// static paths refuse unknown and ambiguous names, record paths
/// additionally refuse unusable ODO counts.
fn map_ownership_error(error: OwnershipError) -> QueryRefusal {
    match error {
        OwnershipError::UnknownField { query } => QueryRefusal::UnknownField { query },
        OwnershipError::AmbiguousField { query, candidates } => {
            QueryRefusal::AmbiguousField { query, candidates }
        }
        OwnershipError::MissingOdoCount { table } => QueryRefusal::MissingRecordCount { table },
        OwnershipError::NestedOdoTable { table } => QueryRefusal::NestedOdoTable { table },
    }
}

/// Render one ownership answer as JSON.
///
/// # Errors
///
/// Returns an error when serialization fails.
pub fn render_json_report(report: &OwnershipReport) -> anyhow::Result<String> {
    serde_json::to_string_pretty(report).context("cannot serialize ownership answer to JSON")
}

/// Render one ownership answer for humans.
#[must_use]
pub fn render_human_report(report: &OwnershipReport) -> String {
    let mut output = String::new();
    let query_line = match &report.query {
        copybook::codec::ownership::OwnershipQuery::PayloadByte { byte } => {
            format!("payload byte {byte}")
        }
        copybook::codec::ownership::OwnershipQuery::FieldPath { path } => {
            format!("field path {path}")
        }
    };
    let _ = writeln!(
        &mut output,
        "Ownership query: {query_line} ({})",
        report.coordinate_system
    );
    let _ = writeln!(&mut output, "Manifest: {}", report.manifest_fingerprint);
    match &report.profile_fingerprint {
        Some(fingerprint) => {
            let _ = writeln!(&mut output, "Profile: sha256:{fingerprint}");
        }
        None => {
            let _ = writeln!(&mut output, "Profile: direct (no reviewed profile)");
        }
    }
    let _ = writeln!(
        &mut output,
        "Layout: {}  Record extent: {} bytes",
        report.schema_fingerprint, report.record_len
    );
    if let Some(selected) = &report.record {
        let mut line = format!("Record: #{} ({} bytes", selected.index, report.record_len);
        for count in &selected.odo_counts {
            let _ = write!(line, "; {}={}", count.table_path, count.actual);
        }
        line.push(')');
        let _ = writeln!(&mut output, "{line}");
    }
    let state_line = match report.state {
        OwnershipState::Owned => "owned".to_owned(),
        OwnershipState::Gap => "gap (byte inside the record extent, no field covers it)".to_owned(),
        OwnershipState::OutOfRange => format!(
            "out of range (record extent is {} bytes)",
            report.record_len
        ),
        OwnershipState::Absent => {
            "absent (no extent in this record: an ODO table has zero occurrences)".to_owned()
        }
    };
    let _ = writeln!(&mut output, "State: {state_line}");
    for item in &report.matches {
        let _ = writeln!(
            &mut output,
            "{}  {}  {}..{} (len {})  {}{}",
            role_label(item.role),
            item.path,
            item.offset,
            item.end,
            item.len,
            item.kind,
            match_details(item)
        );
    }
    if report.truncated {
        let _ = writeln!(
            &mut output,
            "Note: matches truncated at {MAX} entries",
            MAX = copybook::codec::ownership::MAX_OWNERSHIP_MATCHES
        );
    }
    output
}

/// One-line role label for the human table.
fn role_label(role: MatchRole) -> &'static str {
    match role {
        MatchRole::Primary => "PRIMARY  ",
        MatchRole::Container => "CONTAINER",
        MatchRole::View => "VIEW     ",
        MatchRole::Alias => "ALIAS    ",
    }
}

/// Trailing human details for one match.
fn match_details(item: &copybook::codec::ownership::OwnershipMatch) -> String {
    use copybook::codec::ownership::OccurrencePresence;
    let mut details = String::new();
    if item.filler {
        details.push_str("  padding");
    }
    for occurrence in &item.occurrences {
        let presence = match occurrence.presence {
            OccurrencePresence::Guaranteed => "guaranteed",
            OccurrencePresence::Possible => "possible",
        };
        let _ = write!(
            &mut details,
            "  occ {}[{}] ({presence})",
            occurrence.path, occurrence.index
        );
    }
    if let Some(repetition) = &item.repetition {
        let _ = write!(
            &mut details,
            "  repeats {}x{}",
            repetition.kind, repetition.count
        );
        if let Some(counter) = &repetition.counter_path {
            let _ = write!(&mut details, " counter {counter}");
        }
    }
    if let Some(owner) = &item.redefines {
        let _ = write!(&mut details, "  redefines {owner}");
    }
    if let Some(numeric) = &item.numeric {
        let _ = write!(
            &mut details,
            "  digits={} scale={} signed={} encoding={}",
            numeric.digits, numeric.scale, numeric.signed, numeric.encoding
        );
    }
    if let Some(odo) = &item.odo {
        let _ = write!(
            &mut details,
            "  odo {}..{} counter {}",
            odo.min_count, odo.max_count, odo.counter_path
        );
    }
    if !item.conditions.is_empty() {
        let _ = write!(&mut details, "  conditions={}", item.conditions.join(","));
    }
    if !item.members.is_empty() {
        let _ = write!(&mut details, "  members={}", item.members.join(","));
    }
    details
}

/// Parse one copybook text with explicit options, hinting the identity fix.
fn parse_schema(
    copybook_text: &str,
    options: &ParseOptions,
    feature_flags: &FeatureFlags,
) -> anyhow::Result<Schema> {
    parse_copybook_with_feature_flags(copybook_text, options, feature_flags).map_err(|error| {
        // A broken copybook ends here, so the next step goes out
        // with the error: the identity explanation names the rule
        // and the fix.
        print_identity_hint(&error.code().to_string());
        anyhow::Error::from(error)
    })
}

/// One rendered layout row.
struct Row {
    path: String,
    offset: u32,
    len: u32,
    type_str: String,
    details: String,
}

impl Row {
    fn from_field(field: &Field) -> Self {
        Self {
            path: field.path.clone(),
            offset: field.offset,
            len: field.len,
            type_str: render_type(&field.kind),
            details: render_details(field),
        }
    }
}

/// Render the full layout report for a parsed schema.
fn render_layout(codepage: Codepage, lrecl_fixed: Option<u32>, rows: &[Row]) -> String {
    let columns = Columns {
        path_width: column_width(rows.iter().map(|row| row.path.len()), MIN_PATH_WIDTH),
        type_width: column_width(rows.iter().map(|row| row.type_str.len()), MIN_TYPE_WIDTH),
        has_details: rows.iter().any(|row| !row.details.is_empty()),
    };

    let mut output = String::new();
    output.push_str("Copybook Layout\n");
    output.push_str("===============\n");
    // `--codepage` parsing is case-insensitive, so the upper-case spelling is
    // both the familiar one and a valid value to paste back into the flag.
    writeln!(
        output,
        "Codepage: {} ({})",
        codepage.as_str().to_ascii_uppercase(),
        codepage.description()
    )
    .ok();
    match lrecl_fixed {
        Some(lrecl) => writeln!(output, "Fixed LRECL: {lrecl} bytes").ok(),
        // A schema with a trailing ODO array has no single fixed record length.
        None => writeln!(
            output,
            "Fixed LRECL: variable (record length depends on OCCURS DEPENDING ON)"
        )
        .ok(),
    };
    writeln!(output, "Fields: {}", rows.len()).ok();
    output.push('\n');

    output.push_str(&columns.format_row("Field Path", "Offset", "Length", "Type", DETAILS_HEADER));
    writeln!(output, "{:-<width$}", "", width = columns.rule_width()).ok();

    for row in rows {
        output.push_str(&columns.format_row(
            &row.path,
            &row.offset.to_string(),
            &row.len.to_string(),
            &row.type_str,
            &row.details,
        ));
    }

    output
}

/// Column layout shared by the header and every data row.
#[derive(Clone, Copy)]
struct Columns {
    path_width: usize,
    type_width: usize,
    has_details: bool,
}

impl Columns {
    /// Format a single table line, trimming trailing padding so rows stay diff-friendly.
    fn format_row(
        self,
        path: &str,
        offset: &str,
        len: &str,
        type_str: &str,
        details: &str,
    ) -> String {
        let Self {
            path_width,
            type_width,
            has_details,
        } = self;
        let mut line = format!(
            "{path:<path_width$} {offset:<OFFSET_WIDTH$} {len:<LENGTH_WIDTH$} {type_str:<type_width$}"
        );
        if has_details {
            line.push(' ');
            line.push_str(details);
        }
        while line.ends_with(' ') {
            line.pop();
        }
        line.push('\n');
        line
    }

    /// Width of the header underline: the header row before trailing trim.
    fn rule_width(self) -> usize {
        let details = if self.has_details {
            1 + DETAILS_HEADER.len()
        } else {
            0
        };
        self.path_width + 1 + OFFSET_WIDTH + 1 + LENGTH_WIDTH + 1 + self.type_width + details
    }
}

/// Pick a column width that fits the widest value without collapsing below a floor.
fn column_width(widths: impl Iterator<Item = usize>, minimum: usize) -> usize {
    widths.max().unwrap_or(minimum).max(minimum)
}

/// Render a COBOL-shaped type description that matches the source PIC clause.
fn render_type(kind: &FieldKind) -> String {
    match kind {
        FieldKind::Alphanum { len } => format!("PIC X({len})"),
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
            sign_separate,
        } => {
            let mut rendered = format!("PIC {}", numeric_picture(*digits, *scale, *signed));
            if let Some(info) = sign_separate {
                write!(rendered, " SIGN {} SEPARATE", sign_placement(info)).ok();
            }
            rendered
        }
        FieldKind::BinaryInt { bits, signed } => {
            let signedness = if *signed { "signed" } else { "unsigned" };
            format!("COMP ({bits}-bit {signedness})")
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => format!("PIC {} COMP-3", numeric_picture(*digits, *scale, *signed)),
        FieldKind::Group => "GROUP".to_string(),
        FieldKind::Condition { values } => format!("88 VALUE {}", values.join(", ")),
        FieldKind::Renames {
            from_field,
            thru_field,
        } => format!("66 RENAMES {from_field} THRU {thru_field}"),
        FieldKind::EditedNumeric { pic_string, .. } => format!("PIC {pic_string} (EDITED)"),
        FieldKind::FloatSingle => "COMP-1".to_string(),
        FieldKind::FloatDouble => "COMP-2".to_string(),
    }
}

/// Reconstruct the PIC digit/scale notation from the stored total digit count.
///
/// `digits` counts every digit position, so the integer part is
/// `digits - scale`. A negative scale is COBOL `P` positional scaling.
fn numeric_picture(digits: u16, scale: i16, signed: bool) -> String {
    let sign = if signed { "S" } else { "" };
    match scale.cmp(&0) {
        std::cmp::Ordering::Greater => {
            let fraction = u16::try_from(scale).unwrap_or(digits);
            let integer = digits.saturating_sub(fraction);
            if integer == 0 {
                format!("{sign}V9({fraction})")
            } else {
                format!("{sign}9({integer})V9({fraction})")
            }
        }
        // PIC 9(n)P(m): the P positions scale the value away from the decimal point.
        std::cmp::Ordering::Less => {
            let shift = scale.unsigned_abs();
            format!("{sign}9({digits})P({shift})")
        }
        std::cmp::Ordering::Equal => format!("{sign}9({digits})"),
    }
}

/// Render clause-level facts that do not belong in the PIC column.
fn render_details(field: &Field) -> String {
    let mut parts: Vec<String> = Vec::new();

    match &field.occurs {
        Some(Occurs::Fixed { count }) => parts.push(format!("OCCURS {count}")),
        Some(Occurs::ODO {
            min,
            max,
            counter_path,
        }) => parts.push(format!("OCCURS {min} TO {max} DEPENDING ON {counter_path}")),
        None => {}
    }

    if let Some(redefines_of) = &field.redefines_of {
        parts.push(format!("REDEFINES {redefines_of}"));
    }

    if field.synchronized {
        match field.sync_padding {
            Some(padding) if padding > 0 => {
                parts.push(format!("SYNCHRONIZED (+{padding} pad)"));
            }
            _ => parts.push("SYNCHRONIZED".to_string()),
        }
    }

    if field.blank_when_zero {
        parts.push("BLANK WHEN ZERO".to_string());
    }

    if let Some(renames) = &field.resolved_renames {
        parts.push(format!(
            "covers {} field(s) at {}..{}",
            renames.members.len(),
            renames.offset,
            renames.offset + renames.length
        ));
    }

    parts.join(", ")
}

/// Human-readable SIGN SEPARATE placement.
fn sign_placement(info: &copybook::core::SignSeparateInfo) -> &'static str {
    match info.placement {
        copybook::core::SignPlacement::Leading => "LEADING",
        copybook::core::SignPlacement::Trailing => "TRAILING",
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;

    #[test]
    fn numeric_picture_splits_integer_and_fraction_digits() {
        // PIC S9(7)V99 is stored as 9 total digits with scale 2.
        assert_eq!(numeric_picture(9, 2, true), "S9(7)V9(2)");
        assert_eq!(numeric_picture(6, 0, false), "9(6)");
        assert_eq!(numeric_picture(2, 2, false), "V9(2)");
        assert_eq!(numeric_picture(3, -3, false), "9(3)P(3)");
    }

    #[test]
    fn binary_types_name_their_width_and_signedness() {
        assert_eq!(
            render_type(&FieldKind::BinaryInt {
                bits: 32,
                signed: false
            }),
            "COMP (32-bit unsigned)"
        );
        assert_eq!(
            render_type(&FieldKind::BinaryInt {
                bits: 16,
                signed: true
            }),
            "COMP (16-bit signed)"
        );
    }

    #[test]
    fn condition_values_render_without_debug_punctuation() {
        assert_eq!(
            render_type(&FieldKind::Condition {
                values: vec!["'A'".to_string(), "'B'".to_string()],
            }),
            "88 VALUE 'A', 'B'"
        );
    }

    #[test]
    fn header_reports_variable_length_when_lrecl_is_unknown() {
        let rendered = render_layout(Codepage::CP037, None, &[]);
        assert!(
            rendered.contains("Fixed LRECL: variable"),
            "expected variable-length note, got:\n{rendered}"
        );
        assert!(!rendered.contains("None"), "leaked Debug formatting");
    }

    #[test]
    fn header_reports_byte_count_for_fixed_records() {
        let rendered = render_layout(Codepage::CP037, Some(31), &[]);
        assert!(rendered.contains("Fixed LRECL: 31 bytes"));
        assert!(!rendered.contains("Some("), "leaked Debug formatting");
    }

    #[test]
    fn rows_stay_aligned_when_a_path_exceeds_the_minimum_width() {
        let rows = vec![
            Row {
                path: "A".repeat(50),
                offset: 0,
                len: 4,
                type_str: "PIC X(4)".to_string(),
                details: String::new(),
            },
            Row {
                path: "SHORT".to_string(),
                offset: 4,
                len: 2,
                type_str: "COMP (16-bit signed)".to_string(),
                details: "SYNCHRONIZED".to_string(),
            },
        ];
        let rendered = render_layout(Codepage::CP037, Some(6), &rows);
        let lines: Vec<&str> = rendered.lines().collect();
        let long_row = lines
            .iter()
            .find(|line| line.starts_with("AAAA"))
            .expect("long row present");
        let short_row = lines
            .iter()
            .find(|line| line.starts_with("SHORT"))
            .expect("short row present");
        let long_offset_col = long_row.find(" 0 ").expect("offset column");
        let short_offset_col = short_row.find(" 4 ").expect("offset column");
        assert_eq!(long_offset_col, short_offset_col);
    }
}
