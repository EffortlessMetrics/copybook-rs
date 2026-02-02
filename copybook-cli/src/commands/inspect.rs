//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use crate::write_stdout_all;
use copybook_codec::Codepage;
use copybook_core::{Field, FieldKind, Occurs, ParseOptions, parse_copybook_with_options};
use std::fmt::Write as _;
use std::path::PathBuf;
use tracing::info;

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

    // Display human-readable layout
    let mut output = String::new();
    output.push_str("Copybook Layout\n");
    output.push_str("===============\n");
    writeln!(output, "Codepage: {codepage:?}").ok();
    writeln!(output, "Fixed LRECL: {:?}", schema.lrecl_fixed).ok();
    output.push('\n');
    writeln!(
        output,
        "{:<40} {:<8} {:<8} {:<12} {:<20}",
        "Field Structure", "Offset", "Length", "Type", "Details"
    )
    .ok();
    writeln!(output, "{:-<88}", "").ok();

    for field in &schema.fields {
        print_field_tree(&mut output, field, "", true, true);
    }

    write_stdout_all(output.as_bytes())?;

    info!("Inspect completed successfully");
    Ok(ExitCode::Ok)
}

fn print_field_tree(
    output: &mut String,
    field: &Field,
    prefix: &str,
    is_last: bool,
    is_root: bool,
) {
    let type_str = match &field.kind {
        FieldKind::Alphanum { len } => format!("X({len})"),
        FieldKind::ZonedDecimal {
            digits,
            scale,
            signed,
        } => {
            if *signed {
                format!("S9({digits})V9({scale})")
            } else {
                format!("9({digits})V9({scale})")
            }
        }
        FieldKind::BinaryInt { bits, signed } => {
            format!("COMP-{} ({}bit)", if *signed { "S" } else { "" }, bits)
        }
        FieldKind::PackedDecimal {
            digits,
            scale,
            signed,
        } => {
            if *signed {
                format!("S9({digits})V9({scale}) COMP-3")
            } else {
                format!("9({digits})V9({scale}) COMP-3")
            }
        }
        FieldKind::Group => "GROUP".to_string(),
        FieldKind::Condition { values } => {
            format!("LEVEL-88: {values:?}")
        }
        FieldKind::Renames {
            from_field,
            thru_field,
        } => {
            format!("RENAMES {from_field} THRU {thru_field}")
        }
        FieldKind::EditedNumeric { pic_string, .. } => {
            format!("EDITED PIC {pic_string}")
        }
    };

    let details = if let Some(ref occurs) = field.occurs {
        match occurs {
            Occurs::Fixed { count } => format!("OCCURS {count}"),
            Occurs::ODO {
                min,
                max,
                counter_path,
            } => {
                format!("ODO {min}-{max} ({counter_path})")
            }
        }
    } else {
        String::new()
    };

    let connector = if is_root {
        ""
    } else if is_last {
        "└── "
    } else {
        "├── "
    };

    let tree_name = format!("{}{}{}", prefix, connector, field.name);

    writeln!(
        output,
        "{:<40} {:<8} {:<8} {:<12} {:<20}",
        tree_name, field.offset, field.len, type_str, details
    )
    .ok();

    let child_count = field.children.len();
    for (i, child) in field.children.iter().enumerate() {
        let extension = if is_root {
            ""
        } else if is_last {
            "    "
        } else {
            "│   "
        };

        let child_prefix = format!("{}{}", prefix, extension);
        print_field_tree(output, child, &child_prefix, i == child_count - 1, false);
    }
}
