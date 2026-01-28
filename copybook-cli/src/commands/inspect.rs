//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use crate::write_stdout_all;
use copybook_codec::Codepage;
use copybook_core::schema::{Field, FieldKind, Occurs};
use copybook_core::{ParseOptions, parse_copybook_with_options};
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
        "Field", "Offset", "Length", "Type", "Details"
    )
    .ok();
    writeln!(output, "{:-<88}", "").ok();

    print_tree(&schema.fields, &mut output, "", true)
        .map_err(|e| anyhow::anyhow!("Formatting error: {}", e))?;

    write_stdout_all(output.as_bytes())?;

    info!("Inspect completed successfully");
    Ok(ExitCode::Ok)
}

fn format_field_info(field: &Field) -> (String, String) {
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

    (type_str, details)
}

fn print_tree(
    fields: &[Field],
    output: &mut String,
    prefix: &str,
    is_root: bool,
) -> std::fmt::Result {
    for (i, field) in fields.iter().enumerate() {
        let is_last = i == fields.len() - 1;

        // Tree drawing logic
        let connector = if is_root {
            ""
        } else if is_last {
            "└── "
        } else {
            "├── "
        };

        // Combine prefix and connector
        let tree_prefix = format!("{prefix}{connector}");
        let display_name = format!("{tree_prefix}{}", field.name);

        let (type_str, details) = format_field_info(field);

        writeln!(
            output,
            "{:<40} {:<8} {:<8} {:<12} {:<20}",
            display_name, field.offset, field.len, type_str, details
        )?;

        // Prepare next prefix for children
        let child_prefix_segment = if is_root {
            ""
        } else if is_last {
            "    " // 4 spaces matching "└── " length
        } else {
            "│   " // bar and 3 spaces matching "├── " length
        };

        let next_prefix = format!("{prefix}{child_prefix_segment}");

        print_tree(&field.children, output, &next_prefix, false)?;
    }
    Ok(())
}
