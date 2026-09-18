// SPDX-License-Identifier: AGPL-3.0-or-later
//! Resolved-schema compatibility: did the record layout change?
//!
//! Scenario assessments say whether tooling still understands a copybook;
//! this module says whether downstream readers still see the same record.
//! It compares two layout-resolved [`Schema`](crate::schema::Schema) values
//! field by field (by hierarchical path) and reports added, removed, and
//! changed data fields plus record-length drift. Anything unknown stays
//! unknown: fields whose extent cannot be established are compared by
//! identity alone.
//!
//! A removed field, a moved, resized, or retyped field, or a record-length
//! change breaks byte-level readers. An added overlapping view (a new
//! `REDEFINES` branch, which shifts no offset and grows no record) does
//! not. [`SchemaDiff::is_breaking`](crate::schema_diff::SchemaDiff::is_breaking)
//! encodes exactly that.

use crate::schema::{Field, FieldKind, Schema};
use std::collections::BTreeMap;

/// How one field moved between base and head.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FieldChangeKind {
    /// Present in head only. Safe on its own: additions that shift offsets
    /// or grow the record surface separately as [`FieldChangeKind::Changed`]
    /// on the fields they move, or as a record-length change.
    Added,
    /// Present in base only. Byte-level readers lose data.
    Removed,
    /// Same path, different extent or representation.
    Changed,
}

/// One field-level layout change.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldChange {
    /// Hierarchical field path.
    pub path: String,
    /// How the field moved.
    pub kind: FieldChangeKind,
    /// Base layout (`offset`, `len`, representation), when present.
    pub base: Option<FieldLayout>,
    /// Head layout (`offset`, `len`, representation), when present.
    pub head: Option<FieldLayout>,
}

/// Record-relative extent and representation of one data field.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FieldLayout {
    /// Byte offset within the record.
    pub offset: u32,
    /// Field length in bytes.
    pub len: u32,
    /// Canonical representation signature (kind plus array shape).
    pub signature: String,
}

/// Layout difference between two resolved schemas.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SchemaDiff {
    /// Per-field changes in path order.
    pub changes: Vec<FieldChange>,
    /// Base record length, when established.
    pub base_lrecl: Option<u32>,
    /// Head record length, when established.
    pub head_lrecl: Option<u32>,
}

impl SchemaDiff {
    /// Whether the change breaks byte-level readers: any removed or
    /// changed field, or record-length drift while both sides establish
    /// a length.
    #[must_use]
    pub fn is_breaking(&self) -> bool {
        if self.lrecl_changed() {
            return true;
        }
        self.changes.iter().any(|change| {
            matches!(
                change.kind,
                FieldChangeKind::Removed | FieldChangeKind::Changed
            )
        })
    }

    /// Whether both sides establish a record length and they differ.
    #[must_use]
    pub fn lrecl_changed(&self) -> bool {
        match (self.base_lrecl, self.head_lrecl) {
            (Some(base), Some(head)) => base != head,
            _ => false,
        }
    }
}

/// Compare two layout-resolved schemas by field path.
///
/// Both schemas must have passed through layout resolution (offsets
/// assigned); unresolved schemas compare by whatever extents they carry.
#[must_use]
pub fn diff_schemas(base: &Schema, head: &Schema) -> SchemaDiff {
    let base_fields = index_fields(&base.fields);
    let head_fields = index_fields(&head.fields);
    let mut changes = Vec::new();
    for (path, base_layout) in &base_fields {
        match head_fields.get(path) {
            None => changes.push(FieldChange {
                path: path.clone(),
                kind: FieldChangeKind::Removed,
                base: Some(base_layout.clone()),
                head: None,
            }),
            Some(head_layout) if head_layout != base_layout => changes.push(FieldChange {
                path: path.clone(),
                kind: FieldChangeKind::Changed,
                base: Some(base_layout.clone()),
                head: Some(head_layout.clone()),
            }),
            Some(_) => {}
        }
    }
    for (path, head_layout) in &head_fields {
        if !base_fields.contains_key(path) {
            changes.push(FieldChange {
                path: path.clone(),
                kind: FieldChangeKind::Added,
                base: None,
                head: Some(head_layout.clone()),
            });
        }
    }
    SchemaDiff {
        changes,
        base_lrecl: base.lrecl_fixed,
        head_lrecl: head.lrecl_fixed,
    }
}

/// Index data fields by path. Groups only name structure: their extent is
/// implied by their children, so comparing them would double-report every
/// nested move. Level-88 conditions and `RENAMES` aliases carry no record
/// bytes and are likewise out of scope.
fn index_fields(fields: &[Field]) -> BTreeMap<String, FieldLayout> {
    let mut index = BTreeMap::new();
    append_fields(fields, &mut index);
    index
}

fn append_fields(fields: &[Field], index: &mut BTreeMap<String, FieldLayout>) {
    for field in fields {
        match field.kind {
            FieldKind::Group | FieldKind::Condition { .. } | FieldKind::Renames { .. } => {}
            _ => {
                index.insert(
                    field.path.clone(),
                    FieldLayout {
                        offset: field.offset,
                        len: field.len,
                        signature: format!("{:?}|{:?}", field.kind, field.occurs),
                    },
                );
            }
        }
        append_fields(&field.children, index);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::resolve_layout;
    use crate::{Dialect, parse_copybook};

    fn resolved(source: &str) -> Schema {
        let mut schema = parse_copybook(source).expect("copybook parses");
        resolve_layout(&mut schema, Dialect::Normative).expect("layout resolves");
        schema
    }

    const BASE: &str =
        "       01  REC.\n           05  FLD PIC X(10).\n           05  AMT PIC S9(7)V99 COMP-3.\n";

    #[test]
    fn cobol_identical_schemas_have_no_changes() {
        let diff = diff_schemas(&resolved(BASE), &resolved(BASE));
        assert!(diff.changes.is_empty());
        assert!(!diff.is_breaking());
    }

    #[test]
    fn cobol_removed_field_is_breaking() {
        let head = "       01  REC.\n           05  FLD PIC X(10).\n";
        let diff = diff_schemas(&resolved(BASE), &resolved(head));
        assert!(diff.is_breaking());
        let removed: Vec<_> = diff
            .changes
            .iter()
            .filter(|change| change.kind == FieldChangeKind::Removed)
            .collect();
        assert_eq!(removed.len(), 1);
        assert_eq!(removed[0].path, "REC.AMT");
        assert!(removed[0].head.is_none());
    }

    #[test]
    fn cobol_resized_field_is_breaking_change() {
        let head = "       01  REC.\n           05  FLD PIC X(12).\n           05  AMT PIC S9(7)V99 COMP-3.\n";
        let diff = diff_schemas(&resolved(BASE), &resolved(head));
        assert!(diff.is_breaking());
        assert!(diff.lrecl_changed());
        let changed: Vec<_> = diff
            .changes
            .iter()
            .filter(|change| change.kind == FieldChangeKind::Changed)
            .collect();
        assert!(changed.iter().any(|change| change.path == "REC.FLD"));
        // The resized head pushes every later field to a new offset.
        assert!(changed.iter().any(|change| change.path == "REC.AMT"));
    }

    #[test]
    fn cobol_retyped_field_is_breaking_change() {
        let head =
            "       01  REC.\n           05  FLD PIC X(10).\n           05  AMT PIC 9(9) COMP.\n";
        let diff = diff_schemas(&resolved(BASE), &resolved(head));
        assert!(diff.is_breaking());
        let changed: Vec<_> = diff
            .changes
            .iter()
            .filter(|change| change.kind == FieldChangeKind::Changed)
            .collect();
        assert_eq!(changed.len(), 1);
        assert_eq!(changed[0].path, "REC.AMT");
    }

    #[test]
    fn cobol_overlapping_redefines_view_is_safe() {
        let head = "       01  REC.\n           05  FLD PIC X(10).\n           05  AMT PIC S9(7)V99 COMP-3.\n           05  AMT-X REDEFINES AMT PIC X(5).\n";
        let diff = diff_schemas(&resolved(BASE), &resolved(head));
        assert!(!diff.lrecl_changed());
        let added: Vec<_> = diff
            .changes
            .iter()
            .filter(|change| change.kind == FieldChangeKind::Added)
            .collect();
        assert_eq!(added.len(), 1);
        assert_eq!(added[0].path, "REC.AMT-X");
        assert!(!diff.is_breaking());
    }

    #[test]
    fn cobol_appended_field_grows_record_and_breaks() {
        let head = "       01  REC.\n           05  FLD PIC X(10).\n           05  AMT PIC S9(7)V99 COMP-3.\n           05  EXTRA PIC X(2).\n";
        let diff = diff_schemas(&resolved(BASE), &resolved(head));
        assert!(diff.lrecl_changed());
        assert!(diff.is_breaking());
    }
}
