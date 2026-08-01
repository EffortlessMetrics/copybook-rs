// SPDX-License-Identifier: AGPL-3.0-or-later
//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use crate::write_stdout_all;
use copybook_codec::Codepage;
use copybook_core::{Field, FieldKind, Occurs, ParseOptions, parse_copybook_with_options};
use std::fmt::Write as _;
use std::path::PathBuf;
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
) -> anyhow::Result<ExitCode> {
    info!("Inspecting copybook: {:?}", copybook);

    if strict_comments {
        info!("Inline comments (*>) disabled (COBOL-85 compatibility)");
    }

    // Read copybook file or stdin
    let copybook_text = read_file_or_stdin(copybook)?;

    // Parse copybook with options
    let options = ParseOptions {
        strict_comments,
        strict,
        codepage: codepage.to_string(),
        emit_filler: false,
        allow_inline_comments: !strict_comments,
        dialect: dialect.into(),
    };
    let schema = parse_copybook_with_options(&copybook_text, &options)?;

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
fn sign_placement(info: &copybook_core::SignSeparateInfo) -> &'static str {
    match info.placement {
        copybook_core::SignPlacement::Leading => "LEADING",
        copybook_core::SignPlacement::Trailing => "TRAILING",
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
