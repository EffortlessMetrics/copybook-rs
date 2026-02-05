//! Inspect command implementation

use crate::exit_codes::ExitCode;
use crate::utils::read_file_or_stdin;
use crate::write_stdout_all;
use copybook_codec::Codepage;
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
        "{:<50} {:<8} {:<8} {:<25} {:<30}",
        "Field", "Offset", "Length", "Type", "Details"
    )
    .ok();
    writeln!(output, "{:-<121}", "").ok();

    format_tree(&mut output, &schema.fields, "");

    write_stdout_all(output.as_bytes())?;

    info!("Inspect completed successfully");
    Ok(ExitCode::Ok)
}

fn format_tree(output: &mut String, fields: &[copybook_core::Field], prefix: &str) {
    for (i, field) in fields.iter().enumerate() {
        let is_last = i == fields.len() - 1;
        let connector = if is_last { "└─ " } else { "├─ " };

        let type_str = match &field.kind {
            copybook_core::FieldKind::Alphanum { len } => format!("X({len})"),
            copybook_core::FieldKind::ZonedDecimal {
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
            copybook_core::FieldKind::BinaryInt { bits, signed } => {
                format!("COMP-{} ({}bit)", if *signed { "S" } else { "" }, bits)
            }
            copybook_core::FieldKind::PackedDecimal {
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
            copybook_core::FieldKind::Group => "GROUP".to_string(),
            copybook_core::FieldKind::Condition { values } => {
                format!("LEVEL-88: {values:?}")
            }
            copybook_core::FieldKind::Renames {
                from_field,
                thru_field,
            } => {
                format!("RENAMES {from_field} THRU {thru_field}")
            }
            copybook_core::FieldKind::EditedNumeric { pic_string, .. } => {
                format!("EDITED PIC {pic_string}")
            }
        };

        let details = if let Some(ref occurs) = field.occurs {
            match occurs {
                copybook_core::Occurs::Fixed { count } => format!("OCCURS {count}"),
                copybook_core::Occurs::ODO {
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

        let tree_node = format!("{}{}{}", prefix, connector, field.name);

        writeln!(
            output,
            "{:<50} {:<8} {:<8} {:<25} {:<30}",
            tree_node, field.offset, field.len, type_str, details
        )
        .ok();

        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });
        if !field.children.is_empty() {
            format_tree(output, &field.children, &child_prefix);
        }
    }
}
